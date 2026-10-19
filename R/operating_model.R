## operating_model.R
## Age-structured operating model, assessment emulation and HCRs.
## run_sops_mse() simulates Nrepl replicates of a projection of 'years' years
## under one HCR and returns the performance metrics and the simulated matrices.
## regla: 0 = SPRFMO baseline (HCR0), 1 = constant catch (HCR1),
##        2 = SPRFMO alternative (HCR2), 3 = linear SOPS HCR (HCR3).
## Catch and biomass are in thousand tonnes (kt), as in the JJM assessment.
## The function reads the assessment output from the global object 'data'.

run_sops_mse <- function(Qslope, BminH, Nrepl = 1, years = 50,
                         regla = 3,
                         hara_par = 0.6,
                         CBA_const = 1500, # constant TAC of HCR1 (kt)
                         CBA_max = 2000, # maximum TAC of HCR2 (kt)
                         CBA_min = 200, # TAC of HCR2 below 5% of B0 (kt)
                         Blim_low = 0.1, # lower biomass threshold of HCR0 and HCR2 (fraction of SSBmsy)
                         Blim_high = 0.8, # upper biomass threshold of HCR0 and HCR2 (fraction of SSBmsy)
                         sigmaR = 0.64, # recruitment log-sd
                         sigmaY = 0.1, # implementation error log-sd
                         sesgoY = 0.0, # implementation bias (log scale)
                         sigmaEst = 0.20, # assessment error log-sd
                         sesgoEst = -0.15, # assessment bias (log scale)
                         banda = 0.15, # TAC stabilizer band of HCR0 and HCR2
                         rho = 0.77, # recruitment autocorrelation
                         mR = 1.0, # recruitment scaling (1 = no scaling)
                         h = 0.8, # steepness
                         dt = 0.875, # fraction of the year at spawning
                         tar = 0.4, # target SSB/B0 for reference points
                         scenario_id = "base",
                         seed_offset = 0) { # 0 reproduces the published results

  ## Catch at fishing mortality fm (Baranov equation)
  baranov_catch <- function(fm, n, s, m, W) {
    mortF <- fm * s
    mortZ <- mortF + m
    sum(n * mortF / mortZ * (1 - exp(-mortZ)) * W)
  }

  ## Fishing mortality that produces a given catch, with F capped at 5
  solve_F <- function(tac, Np, Sel) {
    if (tac < 0.01) {
      return(0)
    } # near-zero catch

    C_max <- baranov_catch(5, Np, Sel, M, W)
    if (tac >= C_max) {
      return(5)
    } # catch not attainable: F = 5

    F_upper <- 0.5
    while (baranov_catch(F_upper, Np, Sel, M, W) < tac) F_upper <- F_upper * 2

    res <- tryCatch(
      optimize(
        f        = function(fm) (baranov_catch(fm, Np, Sel, M, W) - tac)^2,
        interval = c(0, F_upper),
        tol      = 1e-8
      ),
      error = function(e) list(minimum = 0.3)
    )
    res$minimum
  }

  ## Assessment output
  catch_last <- tail(data$catch, 1) # observed total catch in the last assessment year
  N <- data$N
  W <- data$wt_a_pop
  Fmat <- data$TotF
  M <- data$M[1]
  O <- data$mature_a

  ## Dimensions
  n_hist <- nrow(Fmat)
  n_ages <- ncol(Fmat)
  edades <- seq_len(n_ages)

  ## Derived quantities
  Rmed <- mean(N[, 1]) # mean historical recruitment
  matZ <- Fmat + M
  SSB <- (N * exp(-dt * matZ)) %*% (W * O) # historical spawning biomass

  ## Historical selectivity
  MatSel <- matrix(NA, n_hist, max(edades)) # historical selectivity (years x ages)

  for (j in 1:n_hist) {
    MatSel[j, ] <- Fmat[j, ] / max(Fmat[j, ]) # selectivity-at-age
  }

  Sel_base <- colSums(MatSel / n_hist) / max(colSums(MatSel / n_hist)) # mean historical selectivity

  ## Time-varying reference points of HCR0 and HCR2
  Fmsy <- rep(0, years)
  SSBmsy <- rep(0, years)
  MSY <- rep(0, years)


  ## Baseline reference points (mean historical selectivity)
  out <- por_recluta_r(Tmax = n_ages, Sel = Sel_base, Madage = O, Wage = W, tar = tar, h = h, M = M, dt = dt)
  B0 <- out$SPR0 * Rmed # unfished spawning biomass
  SSBmsy_base <- out$BPRtar * Rmed
  Fmsy_base <- out$Ftar
  MSY_base <- out$YPRtar * Rmed

  ## Initial values of the time-varying reference points (HCR0 and HCR2)
  SSBmsy[1] <- SSBmsy_base
  Fmsy[1] <- Fmsy_base
  MSY[1] <- MSY_base

  ## Output matrices (replicates x years)
  matB <- matrix(0, Nrepl, years)
  matBest <- matrix(0, Nrepl, years)
  matY <- matrix(0, Nrepl, years)
  matF <- matrix(0, Nrepl, years)
  matTAC <- matrix(0, Nrepl, years)

  ## Rows of the historical selectivity used for resampling (last 15 years)
  sel_resample_rows <- (n_hist - 14):n_hist

  for (i in 1:Nrepl) {
    ## Seed fixed per replicate, so that all HCRs face the same stochastic sequences
    set.seed(i + seed_offset)

    ## State vectors of this replicate
    Bio <- numeric(years)
    Capt <- numeric(years)
    CBA <- numeric(years)
    Reclu <- numeric(years)
    Best <- numeric(years)
    Fy <- numeric(years)
    Sel <- matrix(NA, years, n_ages) # projected selectivity (years x ages)

    Np <- N[n_hist, ]
    mortF <- Fmat[n_hist, ]
    mortZ <- mortF + M


    ## Year 1: last assessment year
    Capt[1] <- catch_last
    CBA[1] <- catch_last
    Reclu[1] <- N[n_hist, 1]
    Best[1] <- SSB[n_hist]
    Bio[1] <- Best[1]
    Fy[1] <- max(Fmat[n_hist, ])

    Sel[1, ] <- mortF / max(mortF)
    eps <- rnorm(1, 0, sigmaR) # initial recruitment deviation

    for (t in 2:years) {
      ## Population projection, with a plus group at the oldest age
      Np_plus <- Np[n_ages] * exp(-mortZ[n_ages]) # survivors of the plus group
      Np[2:n_ages] <- Np[1:(n_ages - 1)] * exp(-mortZ[1:(n_ages - 1)]) # ageing of ages 1 to A-1
      Np[n_ages] <- Np[n_ages] + Np_plus

      ## Recruitment: Beverton-Holt on the true SSB of the previous year, with AR(1) lognormal deviations
      eps <- rho * eps + rnorm(1, 0, sigmaR) * sqrt(1 - rho^2)
      Reclu[t] <- 4 * h * Rmed * Bio[t - 1] / (B0 * (1 - h) + Bio[t - 1] * (5 * h - 1)) *
        exp(eps - 0.5 * sigmaR^2) # lognormal bias correction

      Reclu[t] <- Reclu[t] * mR
      Np[1] <- Reclu[t]

      ## Selectivity resampled from the last 15 historical years
      Sel[t, ] <- MatSel[sample(sel_resample_rows, 1), ]

      ## Reference points recalculated with the current selectivity (HCR0 and HCR2)
      if (regla == 0 | regla == 2) {
        out <- por_recluta_r(Tmax = n_ages, Sel = Sel[t, ], Madage = O, Wage = W, tar = tar, h = h, M = M, dt = dt)
        SSBmsy[t] <- out$BPRtar * Rmed
        Fmsy[t] <- out$Ftar
        MSY[t] <- out$YPRtar * Rmed
      }


      if (regla == 0) { # HCR0: SPRFMO baseline

        ## Note: if Best/B0 >= 0.05 and Best/SSBmsy <= 0.1, the TAC stays at 0.

        if (Best[t - 1] / B0 < 0.05) {
          CBA[t] <- 0
        } else if (Best[t - 1] / SSBmsy[t] > Blim_low && Best[t - 1] / SSBmsy[t] < Blim_high) {
          CBA[t] <- min(Catch(Np, Sel[t, ], M, W, Fmsy[t], Fy[t - 1])) # lower of C(Fmsy) and C(F previous year)
        } else if (Best[t - 1] / SSBmsy[t] > Blim_high && Best[t - 1] / SSBmsy[t] < 1.0) {
          CBA[t] <- min(Catch(Np, Sel[t, ], M, W, Fmsy[t], Fy[t - 1])) # lower of C(Fmsy) and C(F previous year)
          CBA[t] <- Estabiliza(CBA[t - 1], CBA[t], banda)
        } else if (Best[t - 1] / SSBmsy[t] > 1.0) {
          CBA[t] <- Catch(Np, Sel[t, ], M, W, Fmsy[t], Fy[t - 1])[1] # C(Fmsy)
          CBA[t] <- Estabiliza(CBA[t - 1], CBA[t], banda)
        }
      }


      if (regla == 1) { # HCR1: constant catch
        CBA[t] <- CBA_const
      }


      if (regla == 2) { # HCR2: SPRFMO alternative

        if (Best[t - 1] / B0 < 0.05) {
          CBA[t] <- CBA_min
        } else if (Best[t - 1] / SSBmsy[t] > Blim_low && Best[t - 1] / SSBmsy[t] < Blim_high) {
          CBA[t] <- CBA_max * Best[t - 1] / SSBmsy[t]
        } else if (Best[t - 1] / SSBmsy[t] > Blim_high) {
          CBA[t] <- CBA_max
        }

        CBA[t] <- Estabiliza(CBA[t - 1], CBA[t], banda)
      } # 15% stabilizer


      if (regla == 3) { # HCR3: linear SOPS rule, TAC = Qslope * (Best - BminH)


        Bpow <- 1 # exponent of the rule (1 = linear)

        if (Best[t - 1] > 0.1) {
          CBA[t] <- Qslope * (Best[t - 1] - BminH)^Bpow
        } else {
          CBA[t] <- 0.0001
        }
        CBA[t] <- max(0, CBA[t]) # no negative TAC
      }


      ## Implementation error: realized catch target
      Capt_target <- CBA[t] * exp(rnorm(1, sesgoY - sigmaY^2 / 2, sigmaY))

      ## Fishing mortality that produces the target catch
      Fy[t] <- solve_F(tac = Capt_target, Np = Np, Sel = Sel[t, ])
      mortF <- Fy[t] * Sel[t, ]
      mortZ <- mortF + M

      ## Realized catch (Baranov equation)
      Capt[t] <- baranov_catch(Fy[t], Np, Sel[t, ], M, W)

      ## Spawning biomass at spawning time, and its assessment estimate
      Bio[t] <- sum(Np * O * W * exp(-dt * mortZ))

      Best[t] <- Bio[t] * exp(rnorm(1, sesgoEst - 0.5 * sigmaEst^2, sigmaEst))
    }

    matB[i, ] <- Bio
    matBest[i, ] <- Best
    matY[i, ] <- Capt # realized catch, used for the performance metrics
    matF[i, ] <- Fy
    matTAC[i, ] <- CBA # TAC
  }

  ## Performance metrics, over years 2 to 'years' (year 1 is the last assessment year)
  B_eval <- matB[, 2:years, drop = FALSE]
  Y_eval <- matY[, 2:years, drop = FALSE]
  F_eval <- matF[, 2:years, drop = FALSE]

  mean_catch <- mean(Y_eval)
  catch_cv <- sd(Y_eval) / mean_catch
  aav_catch <- mean(abs(diff(t(Y_eval)))) / mean_catch
  prob_below_SSBmsy <- mean(B_eval < SSBmsy_base)
  prob_above_Fmsy <- mean(F_eval > Fmsy_base)
  prob_above_msy <- mean(Y_eval > MSY_base)
  ## Carruthers et al. (2016) criteria
  prob_below_125Fmsy <- mean(F_eval < 1.25 * Fmsy_base)
  prob_above_half_SSBmsy <- mean(B_eval > 0.5 * SSBmsy_base)

  ## HARA utility, E[sum(C_t^0.6)]
  utility_per_rep <- rowSums(Y_eval^hara_par) # sum of C_t^0.6 per replicate
  mean_utility <- mean(utility_per_rep) # expected utility
  sd_utility <- sd(utility_per_rep) # variability across replicates

  return(list(
    Qslope = Qslope,
    BminH = BminH,
    mean_utility = mean_utility,
    sd_utility = sd_utility,
    mean_catch = mean_catch,
    catch_cv = catch_cv,
    catch_avv = aav_catch,
    prob_below_SSBmsy = prob_below_SSBmsy,
    prob_above_half_SSBmsy = prob_above_half_SSBmsy,
    prob_above_Fmsy = prob_above_Fmsy,
    prob_below_125Fmsy = prob_below_125Fmsy,
    prob_above_msy = prob_above_msy,
    matB = matB,
    matBest = matBest,
    matY = matY,
    matF = matF,
    matTAC = matTAC,
    B0 = B0,
    SSBmsy_base = SSBmsy_base,
    Fmsy_base = Fmsy_base,
    MSY_base = MSY_base,
    scenario_id = scenario_id
  ))
}
