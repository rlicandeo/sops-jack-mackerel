## helpers.R
## Helper functions used by run_sops_mse().

## Yield at two fishing mortalities: Fmsy (first value) and Fy (second value)

Catch <- function(N, S, M, W, Fmsy, Fy) {
  mortF <- Fmsy * S
  mortZ <- mortF + M
  Y1 <- sum(N * mortF * W / mortZ * (1 - exp(-mortZ)))

  mortF <- Fy * S
  mortZ <- mortF + M
  Y2 <- sum(N * mortF * W / mortZ * (1 - exp(-mortZ)))

  Yield <- c(Y1, Y2)

  return(Yield)
}


## TAC stabilizer: limits the change from the previous TAC (C1) to +/- banda
Estabiliza <- function(C1, C2, banda) {
  if (C1 <= 0) {
    return(C2)
  } # no previous TAC to stabilize against
  if (C2 / C1 > 1 + banda) {
    C2 <- C1 * (1 + banda)
  } # upper limit
  if (C2 / C1 < 1 - banda) {
    C2 <- C1 * (1 - banda)
  } # lower limit

  return(C2)
}


## Equilibrium (per-recruit with Beverton-Holt) reference points over a range of F.
## Ftar, BPRtar, YPRtar: F, spawning biomass and yield at the target depletion
## (equilibrium spawning biomass / B0 = tar). FRMS, BRMS, YRMS: maximum yield.
por_recluta_r <- function(Tmax, Sel, Madage, Wage, tar, h, M, dt) {
  N <- seq(1, Tmax)
  N0 <- N

  Fcr <- seq(0.001, 5 * M, 0.01)
  B <- Fcr
  BE <- Fcr
  Y <- Fcr
  R <- Fcr

  ## Unfished spawning biomass per recruit
  for (i in 2:Tmax) {
    N0[i] <- N0[i - 1] * exp(-M)
  }
  N0[i] <- N0[i] / (1 - exp(-M))
  B0 <- sum(N0 * Madage * Wage * exp(-dt * M))
  alfa <- 4 * h / (5 * h - 1)
  beta <- (1 - h) / (5 * h - 1) * B0
  aux <- 0

  for (j in 1:length(Fcr)) {
    F <- Fcr[j] * Sel
    Z <- F + M


    for (i in 2:Tmax) {
      N[i] <- N[i - 1] * exp(-Z[i - 1])
    }
    N[i] <- N[i] / (1 - exp(-Z[i]))

    C <- N * F / Z * (1 - exp(-Z))

    B[j] <- alfa * sum(N * Madage * Wage * exp(-dt * Z)) - beta
    R[j] <- alfa * B[j] / (beta + B[j])
    Y[j] <- R[j] * sum(C * Wage)
    BE[j] <- sum(N * Sel * Wage * exp(-0.5 * Z))


    if (B[j] / B[1] > 0.99 * tar) {
      Ftar <- Fcr[j]
      BPRtar <- B[j]
      YPRtar <- Y[j]
      BEtar <- BE[j]
    }

    if (Y[j] > aux) {
      BRMS <- B[j]
      YRMS <- Y[j]
      FRMS <- Fcr[j]
    }

    aux <- Y[j]
  }


  outputs <- list(
    Fcr = Fcr, B = B, Y = Y, Ftar = Ftar, BPRtar = BPRtar, YPRtar = YPRtar,
    FRMS = FRMS, BRMS = BRMS, YRMS = YRMS, BEtar = BEtar,
    SPR0 = B0 # unfished spawning biomass per recruit
  )

  return(outputs)
}
