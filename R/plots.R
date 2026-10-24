## plots.R
## Figure functions of the paper.
##   plot_policy_space       Figure 1 (policy space of HCR3)
##   plot_trajectories       Figure 2 (projected SSB, catch and F)
##   plot_hcr_F              Figure 3 (realized F versus SSB under the selected HCR)
##   plot_tradeoff           Figure 4 (Pareto frontier)
##   plot_utility            Appendix Figures A1 and A2 (HARA utility)
##   plot_SSBmsy_carruthers  Appendix Figure A3 (Carruthers et al. 2016 criteria)
##   plot_refined_grid       Appendix Figure A4 (refined grid)
## plot_tradeoff, plot_utility and plot_SSBmsy_carruthers save their figure (and,
## with save_grid = TRUE, a .txt grid) in the working directory.


plot_policy_space <- function(results_all, scenario = "M0",
                              max_dep = 0.40, max_ovf = 0.50, max_aav = 0.30,
                              chosen = c(Qslope = 0.15, BminH = 1000),
                              file = NULL, width = 14, height = 17) {
  d <- results_all %>%
    filter(scenario_id == scenario, Qslope > 0) %>%
    mutate(
      fail_dep = prob_below_SSBmsy > max_dep,
      fail_ovf = prob_above_Fmsy >= max_ovf,
      fail_aav = catch_avv >= max_aav,
      fail_any = fail_dep | fail_ovf | fail_aav
    )
  hara <- unlist(d[which.max(d$mean_utility), c("Qslope", "BminH")]) # HARA-optimal HCR

  pts <- data.frame(
    BminH = c(hara["BminH"], chosen["BminH"]),
    Qslope = c(hara["Qslope"], chosen["Qslope"]),
    what = c("HARA-optimal", "Constrained-optimal")
  )

  base_theme <- theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 13, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 12),
      legend.key.height = unit(0.7, "cm"),
      legend.key.width = unit(0.4, "cm"),
      legend.position = "inside",
      legend.position.inside = c(0.985, 0.02),
      legend.justification.inside = c(1, 0),
      legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
      legend.margin = margin(4, 6, 4, 6),
      panel.grid = element_blank()
    )

  panel <- function(var, fail, title, legend, breaks, option, direction,
                    contour_col = "black") {
    p <- ggplot(d, aes(x = BminH, y = Qslope, z = .data[[var]])) +
      geom_tile(aes(fill = .data[[var]]), color = "white", linewidth = 0.3) +
      scale_fill_viridis_c(name = legend, option = option, direction = direction) +
      geom_contour(breaks = breaks, color = contour_col, linewidth = 0.5) +
      geom_text_contour(
        breaks = breaks, stroke = 0.25, size = 4.8,
        color = "black", skip = 0,
        label.placer = label_placer_fraction(frac = 0.3)
      ) +
      scale_x_continuous(breaks = seq(0, 20000, 2000), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0.1, 1, 0.1), expand = c(0, 0)) +
      labs(x = "Bmin (kt)", y = "Qslope", title = title) +
      base_theme
    if (!is.null(fail)) {
      p <- p + geom_point(
        data = d[d[[fail]], ], aes(x = BminH, y = Qslope),
        inherit.aes = FALSE, shape = 4, size = 1.8,
        stroke = 0.7, color = "grey55"
      )
    }
    p + geom_point(
      data = pts, aes(x = BminH, y = Qslope, shape = what),
      inherit.aes = FALSE, size = 3.5, stroke = 1,
      fill = "white", color = "black"
    ) +
      scale_shape_manual(
        name = NULL,
        values = c("HARA-optimal" = 23, "Constrained-optimal" = 21)
      ) +
      guides(shape = "none")
  }

  pa <- panel(
    "prob_below_SSBmsy", "fail_dep", "(a) Depletion risk",
    "P(SSB < SSBmsy)", c(0.1, 0.3, 0.4, 0.5, 0.6), "inferno", -1
  )
  pb <- panel(
    "prob_above_Fmsy", "fail_ovf", "(b) Overfishing risk",
    "P(F > Fmsy)", c(0.1, 0.25, 0.5, 0.75), "inferno", -1
  )
  pc <- panel(
    "catch_avv", "fail_aav", "(c) Catch variability",
    "AAV", c(0.20, 0.25, 0.3, 0.4), "inferno", -1
  )
  pd <- panel("prob_above_msy", NULL, "(d) Overharvest risk",
    "P(Catch > MSY)", c(0.2, 0.3, 0.4, 0.5), "inferno", -1,
    contour_col = "gray80"
  )
  pe <- panel(
    "mean_catch", "fail_any", "(e) Mean catch",
    "Catch (kt/yr)", c(500, 1000, 1250, 1500, 2000), "plasma", 1
  ) +
    geom_tile(
      data = d[!d$fail_any, ], aes(x = BminH, y = Qslope),
      inherit.aes = FALSE, width = 1000, height = 0.05,
      fill = NA, color = "black", linewidth = 0.8
    )

  ## symbol key inside panel (e), top-right (data coordinates)
  kx <- 5600
  ky <- c(0.98, 0.915, 0.85, 0.785)
  pe <- pe +
    annotate("rect",
      xmin = 5000, xmax = 20450, ymin = 0.74, ymax = 1.022,
      fill = "white", alpha = 0.85, color = NA
    ) +
    annotate("point", x = kx, y = ky[1], shape = 4, size = 2.5, stroke = 1, color = "grey40") +
    annotate("point", x = kx, y = ky[2], shape = 22, size = 4, stroke = 1.1, fill = NA) +
    annotate("point", x = kx, y = ky[3], shape = 21, size = 3.5, stroke = 1, fill = "white") +
    annotate("point", x = kx, y = ky[4], shape = 23, size = 3.5, stroke = 1, fill = "white") +
    annotate("text",
      x = kx + 600, y = ky, hjust = 0, size = 4.3,
      label = c(
        "Fails a constraint (a to c) or any constraint (e)",
        "Acceptable HCR",
        "Constrained-optimal HCR (0.15, 1,000 kt)",
        sprintf("HARA-optimal HCR (%.2f, %s)", hara["Qslope"], format(hara["BminH"], big.mark = ","))
      )
    )

  fig <- pa + pb + pc + pd + pe +
    plot_layout(design = "AABB
CCDD
#EE#")

  if (!is.null(file)) ggsave(device = ragg::agg_png, file, fig, width = width, height = height, dpi = 300, bg = "white")
  invisible(fig)
}


plot_trajectories <- function(results_hcr,
                              SSBmsy, MSY, Fmsy, # baseline reference points (outputs/reference_points.csv)
                              ylim_B = c(0, 36000), ylim_Y = c(0, 5000), ylim_F = c(0, 1.25),
                              file = NULL, width = 16, height = 11) {
  hcr_lab <- c(
    "HCR0: SPRFMO baseline", "HCR1: Constant catch",
    "HCR2: SPRFMO alternative", "HCR3: SOPS (0.15, 1,000 kt)"
  )
  hcr_col <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
  names(hcr_col) <- hcr_lab
  years <- ncol(results_hcr[[1]]$matB)

  ## example replicates: 10th, 50th, 90th percentile of mean SSB under HCR3
  mB3 <- rowMeans(results_hcr[[4]]$matB)
  ex <- sapply(c(0.10, 0.50, 0.90), function(q) which.min(abs(mB3 - quantile(mB3, q))))
  ex_lab <- c("Low", "Medium", "High")

  band <- function(mat_name) {
    do.call(rbind, lapply(1:4, function(i) {
      m <- results_hcr[[i]][[mat_name]]
      data.frame(
        HCR = hcr_lab[i], Year = 1:years,
        lo = apply(m, 2, quantile, 0.05), hi = apply(m, 2, quantile, 0.95),
        med = apply(m, 2, median)
      )
    }))
  }
  lines_df <- function(mat_name) {
    do.call(rbind, lapply(1:4, function(i) {
      m <- results_hcr[[i]][[mat_name]]
      do.call(rbind, lapply(1:3, function(k) {
        data.frame(
          HCR = hcr_lab[i], Year = 1:years, value = m[ex[k], ],
          Replicate = ex_lab[k]
        )
      }))
    }))
  }

  row_plot <- function(mat_name, ylab, ref, ylim, top, bottom) {
    b <- band(mat_name)
    l <- lines_df(mat_name)
    b$HCR <- factor(b$HCR, hcr_lab)
    l$HCR <- factor(l$HCR, hcr_lab)
    l$Replicate <- factor(l$Replicate, ex_lab)
    ggplot() +
      geom_ribbon(data = b, aes(Year, ymin = lo, ymax = hi, fill = HCR), alpha = 0.3) +
      geom_hline(yintercept = ref, linetype = "dashed", color = "grey30", linewidth = 0.5) +
      geom_line(data = l, aes(Year, value, linetype = Replicate), linewidth = 0.4, color = "grey25") +
      geom_line(data = b, aes(Year, med, color = HCR), linewidth = 1.6, alpha = 0.55) + # median
      facet_wrap(~HCR, nrow = 1) +
      scale_fill_manual(values = hcr_col, guide = "none") +
      scale_color_manual(values = hcr_col, guide = "none") +
      scale_linetype_manual(
        values = c(Low = "dotted", Medium = "solid", High = "dashed"),
        name = "Example replicate (mean SSB under HCR3)"
      ) +
      scale_x_continuous(breaks = seq(0, 50, 10), expand = c(0, 0)) +
      coord_cartesian(ylim = ylim) +
      labs(x = if (bottom) "Projection year" else NULL, y = ylab) +
      theme_bw() +
      theme(
        strip.text = if (top) element_text(size = 13, face = "bold") else element_blank(),
        strip.background = element_rect(fill = "grey92"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = if (bottom) element_text(size = 12) else element_blank(),
        axis.ticks.x = if (bottom) element_line() else element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.5, "cm")
      )
  }

  fig <- row_plot("matB", "SSB (kt)", SSBmsy, ylim_B, top = TRUE, bottom = FALSE) /
    row_plot("matY", "Catch (kt)", MSY, ylim_Y, top = FALSE, bottom = FALSE) /
    row_plot("matF", "F (year⁻¹)", Fmsy, ylim_F, top = FALSE, bottom = TRUE) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  if (!is.null(file)) ggsave(device = ragg::agg_png, file, fig, width = width, height = height, dpi = 300, bg = "white")
  invisible(list(fig = fig, replicates = ex))
}


## Appendix Figure A4: mean catch across the refined grid, acceptable cells outlined.
plot_refined_grid <- function(A, file = NULL, width = 9, height = 7) {
  A$acc <- is_acceptable(A)
  orig <- expand.grid(Qslope = seq(0.05, 0.25, 0.05), BminH = seq(0, 3000, 1000))
  p <- ggplot(A, aes(BminH, Qslope)) +
    geom_tile(aes(fill = mean_catch), color = "white", linewidth = 0.2) +
    geom_tile(data = A[A$acc, ], fill = NA, color = "black", linewidth = 0.5, width = 250, height = 0.01) +
    geom_point(data = orig, size = 1.2) +
    geom_point(data = data.frame(BminH = 1000, Qslope = 0.15), shape = 21, size = 4, stroke = 1, fill = "white") +
    scale_fill_viridis_c(name = "Catch (kt/yr)", option = "plasma") +
    scale_x_continuous(breaks = seq(0, 3000, 500), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0.05, 0.25, 0.05), expand = c(0, 0)) +
    labs(x = "Bmin (kt)", y = "Qslope") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12), axis.title = element_text(size = 13, face = "bold"),
      legend.title = element_text(size = 11, face = "bold"), panel.grid = element_blank()
    )
  if (!is.null(file)) ggsave(device = ragg::agg_png, file, p, width = width, height = height, dpi = 300, bg = "white")
  invisible(p)
}


plot_hcr_F <- function(matB, matF,
                       Qslope, BminH, Bpow,
                       SSBmsy_base = NA,
                       Fmsy_base = NA,
                       Blim = 30000,
                       save_path = NULL) {
  # SSB sequence for HCR line
  Best_plot <- seq(0.01, Blim, length.out = 500)

  # HCR catch -> F
  CBA_plot <- ifelse(Best_plot <= BminH, 0, Qslope * (Best_plot - BminH)^Bpow)
  Ut_hcr <- CBA_plot / Best_plot
  Ftt_hcr <- -log(1 - pmin(Ut_hcr, 0.99))

  # Density colours
  dens_colors <- densCols(matB, matF,
    colramp = colorRampPalette(c("blue", "green", "yellow", "red"))
  )

  # Axis ticks
  xticks <- pretty(c(0, Blim), n = 10)
  yticks <- pretty(c(0, max(matF, na.rm = TRUE)), n = 10)

  # Open device if saving
  if (!is.null(save_path)) {
    png(save_path, width = 12, height = 10, units = "in", res = 300)
  }

  # Base plot
  par(mar = c(5, 5, 2, 2)) # margins: bottom, left, top, right
  plot(matB, matF,
    col = dens_colors, pch = 20, cex = 1.2,
    xlim = c(0, Blim),
    ylim = c(0, max(matF, na.rm = TRUE)),
    xlab = "SSB (kt)", ylab = "Fishing mortality (F)",
    xaxt = "n", yaxt = "n",
    cex.lab = 1.6,
    mgp = c(3.5, 1, 0)
  )

  axis(1, at = xticks, labels = sprintf("%.0f", xticks), cex.axis = 1.3)
  axis(2, at = yticks, labels = sprintf("%.2f", yticks), las = 1, cex.axis = 1.3)

  # HCR line
  lines(Best_plot, Ftt_hcr, lty = 2, col = "black", lwd = 3)

  # BminH reference
  abline(v = BminH, col = "gray", lty = 3, lwd = 2)

  # Optional reference points
  if (!is.na(SSBmsy_base)) abline(v = SSBmsy_base, col = "darkgreen", lty = 3, lwd = 2)
  if (!is.na(Fmsy_base)) abline(h = Fmsy_base, col = "red", lty = 3, lwd = 2)

  # Build legend dynamically
  leg_labels <- list(
    list(label = "Simulated points", col = "black", pch = 20, lty = NA, lwd = NA),
    list(label = "HCR (F)", col = "black", pch = NA, lty = 2, lwd = 3),
    list(label = "B[minH]", col = "gray", pch = NA, lty = 3, lwd = 2)
  )
  if (!is.na(SSBmsy_base)) {
    leg_labels <- c(leg_labels, list(list(label = "SSB[MSY]", col = "darkgreen", pch = NA, lty = 3, lwd = 2)))
  }
  if (!is.na(Fmsy_base)) {
    leg_labels <- c(leg_labels, list(list(label = "F[MSY]", col = "red", pch = NA, lty = 3, lwd = 2)))
  }

  legend("topleft",
    legend = lapply(leg_labels, function(x) {
      if (grepl("\\[|\\^", x$label)) {
        parse(text = x$label)[[1]]
      } else {
        x$label
      }
    }),
    col = sapply(leg_labels, `[[`, "col"),
    pch = sapply(leg_labels, `[[`, "pch"),
    lty = sapply(leg_labels, `[[`, "lty"),
    lwd = sapply(leg_labels, `[[`, "lwd"),
    pt.cex = 1.4, bty = "n",
    cex = 1.4
  )

  if (!is.null(save_path)) {
    dev.off()
    message("Plot saved to: ", save_path)
  }
}


plot_tradeoff <- function(results_all,
                          filter_scenario = NULL,
                          hcr_metrics = NULL,
                          show_traditional = TRUE,
                          show_recommended = TRUE,
                          recommended_BminH = 1000,
                          recommended_Qslope = 0.15,
                          save_plot = TRUE,
                          max_prob_below_SSBmsy = 0.50,
                          max_prob_above_Fmsy = 0.50,
                          max_catch_avv = 0.30,
                          min_mean_catch = 100,
                          aav_breaks = c(0.15, 0.20, 0.25, 0.30),
                          vline_prob_SSBmsy = NULL, # e.g. 0.40; NULL = no line
                          return_data = FALSE) { # TRUE = return list(plot, data)

  scenario_tag <- if (!is.null(filter_scenario)) paste(filter_scenario, collapse = "_") else "all"

  # --- Filter ---
  results_plot <- results_all %>% filter(Qslope > 0)

  if (!is.null(filter_scenario)) {
    results_plot <- results_plot %>% filter(scenario_id %in% filter_scenario)
  }

  # --- Acceptable grid points ---
  safe_hcrs <- results_plot %>%
    filter(
      prob_below_SSBmsy <= max_prob_below_SSBmsy,
      prob_above_Fmsy < max_prob_above_Fmsy,
      catch_avv < max_catch_avv,
      mean_catch >= min_mean_catch
    )

  # --- Pareto frontier per scenario ---
  pareto_front <- safe_hcrs %>%
    group_by(scenario_id) %>%
    arrange(prob_below_SSBmsy) %>%
    mutate(
      max_catch_so_far = cummax(mean_catch),
      pareto_optimal = mean_catch == cummax(mean_catch)
    ) %>%
    filter(pareto_optimal) %>%
    ungroup()

  # --- Traditional HCRs ---
  if (show_traditional && !is.null(hcr_metrics)) {
    trad_hcrs <- hcr_metrics %>%
      mutate(HCR = paste0("HCR", regla))
  }

  # --- Recommended point ---
  recommended <- results_plot %>%
    filter(BminH == recommended_BminH) %>%
    mutate(diff = abs(Qslope - recommended_Qslope)) %>%
    arrange(diff) %>%
    slice(1)

  # --- axis limits ---
  y_max <- max(
    c(
      safe_hcrs$mean_catch,
      if (!is.null(trad_hcrs)) trad_hcrs$mean_catch else 0
    ),
    na.rm = TRUE
  ) * 1.05

  x_max <- max(
    c(
      safe_hcrs$prob_below_SSBmsy,
      if (!is.null(trad_hcrs)) trad_hcrs$prob_below_SSBmsy else 0
    ),
    na.rm = TRUE
  ) * 1.05

  # --- Plot ---
  p <- ggplot(safe_hcrs, aes(x = prob_below_SSBmsy, y = mean_catch)) +
    geom_point(aes(color = catch_avv), size = 8, alpha = 0.5, shape = 16) +
    geom_path(
      data = pareto_front,
      aes(x = prob_below_SSBmsy, y = mean_catch, group = scenario_id),
      color = "black", linetype = "dashed", linewidth = 1, alpha = 0.7
    ) +
    scale_color_viridis_c(
      name = "AAV", option = "magma", direction = -1,
      limits = c(min(aav_breaks), max_catch_avv),
      breaks = aav_breaks,
      oob = scales::squish
    ) +
    facet_wrap(~scenario_id) +
    scale_x_continuous(
      breaks = seq(0, x_max, by = 0.05),
      limits = c(0, x_max)
    ) +
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = seq(0, ceiling(y_max / 100) * 100, by = 100)
    ) +
    labs(x = "P(SSB < SSBmsy) - Depletion Risk", y = "Mean Catch (kt/yr)") +
    theme_minimal(base_size = 14) +
    theme(
      axis.title.x         = element_text(size = 16, face = "bold", margin = margin(t = 10)),
      axis.title.y         = element_text(size = 16, face = "bold", margin = margin(r = 10)),
      axis.text.x          = element_text(size = 13, angle = 45, hjust = 1),
      axis.text.y          = element_text(size = 13),
      legend.title         = element_text(size = 14, face = "bold"),
      legend.text          = element_text(size = 14),
      legend.position      = c(0.95, 0.05),
      legend.justification = c("right", "bottom"),
      legend.background    = element_rect(fill = alpha("white", 0.6)),
      legend.key.height    = unit(1., "cm"),
      legend.key.width     = unit(0.6, "cm"),
      panel.grid.major     = element_line(color = "grey90", linewidth = 0.05)
    )

  # --- optional vertical threshold line ---
  if (!is.null(vline_prob_SSBmsy)) {
    p <- p +
      geom_vline(
        xintercept = vline_prob_SSBmsy,
        linetype = "dashed", color = "gray", linewidth = 0.8
      )
  }

  # --- traditional HCRs ---
  if (show_traditional && !is.null(trad_hcrs)) {
    p <- p +
      geom_point(
        data = trad_hcrs,
        aes(x = prob_below_SSBmsy, y = mean_catch),
        size = 4, color = "red", shape = 15, stroke = 2, alpha = 0.7
      ) +
      geom_text(
        data = mutate(trad_hcrs, lab_vjust = ifelse(HCR == "HCR2", 1.8, -0.8)),
        aes(x = prob_below_SSBmsy, y = mean_catch, label = HCR, vjust = lab_vjust),
        hjust = 0.5, color = "black", fontface = "bold", size = 4
      ) # HCR2 label below its point, so that it does not overlap HCR0
  }

  # --- recommended point ---
  if (show_recommended && nrow(recommended) > 0) {
    p <- p +
      geom_point(
        data = recommended,
        aes(x = prob_below_SSBmsy, y = mean_catch),
        size = 4, color = "red", shape = 17, stroke = 2
      )
  }

  if (save_plot) {
    n_sc <- length(unique(safe_hcrs$scenario_id))
    ggsave(
      device = ragg::agg_png, paste0("SOPS_tradeoff_pareto_", scenario_tag, ".png"),
      width = 9, height = 6 * ceiling(n_sc / 3), dpi = 300
    )
  }

  # --- return data or just plot ---
  if (return_data) {
    return(list(
      plot         = p,
      safe_hcrs    = safe_hcrs,
      pareto_front = pareto_front,
      recommended  = recommended,
      trad_hcrs    = if (exists("trad_hcrs")) trad_hcrs else NULL
    ))
  }

  p
}


plot_utility <- function(results_all, show_safe = TRUE, save_grid = TRUE,
                         save_plot = TRUE, filter_scenario = NULL,
                         show_utility_values = FALSE, utility_value_size = 3,
                         max_prob_below_SSBmsy = 0.40,
                         max_prob_above_Fmsy = 0.50,
                         max_catch_avv = 0.30,
                         contour_breaks = NULL) {
  scenario_tag <- if (!is.null(filter_scenario)) paste(filter_scenario, collapse = "_") else "all"

  results_plot <- results_all %>% filter(Qslope > 0)

  # Filter first, then save the grid
  if (!is.null(filter_scenario)) {
    results_plot <- results_plot %>% filter(scenario_id %in% filter_scenario)
  }

  if (save_grid) {
    results_plot %>%
      dplyr::select(scenario_id, BminH, Qslope, mean_utility) %>%
      mutate(mean_utility = round(mean_utility, 1)) %>%
      write.table(paste0("SOPS_utility_grid_", scenario_tag, ".txt"),
        sep = "\t", row.names = FALSE, quote = FALSE
      )
  }

  # Auto-compute contour breaks from data range if not supplied
  if (is.null(contour_breaks)) {
    u_range <- range(results_plot$mean_utility, na.rm = TRUE)
    contour_breaks <- pretty(u_range, n = 6)
  }

  p <- ggplot(results_plot, aes(x = BminH, y = Qslope, z = mean_utility)) +
    geom_tile(aes(fill = mean_utility), color = "white", linewidth = 0.5) +
    scale_fill_viridis_c(name = "HARA Utility\n(C^0.6)", option = "plasma") +
    geom_contour(breaks = contour_breaks, color = "black", linewidth = 0.6) +
    geom_text_contour(
      breaks = contour_breaks,
      stroke = 0.2, size = 4.5, color = "black", skip = 0,
      label.placer = label_placer_fraction(frac = 0.3)
    ) +
    facet_wrap(~scenario_id) +
    labs(x = "Bmin (kt)", y = "Qslope") +
    scale_x_continuous(breaks = unique(results_plot$BminH), expand = c(0, 0)) +
    scale_y_continuous(breaks = unique(results_plot$Qslope), expand = c(0, 0)) +
    theme_minimal() +
    theme(
      axis.text.x          = element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y          = element_text(size = 16),
      axis.title.x         = element_text(size = 18, face = "bold"),
      axis.title.y         = element_text(size = 18, face = "bold"),
      legend.title         = element_text(size = 14, face = "bold"),
      legend.text          = element_text(size = 14),
      legend.position      = c(0.95, 0.05),
      legend.justification = c("right", "bottom"),
      legend.background    = element_rect(fill = alpha("white", 0.6)),
      legend.key.height    = unit(1., "cm"),
      legend.key.width     = unit(0.6, "cm")
    )

  if (show_utility_values) {
    p <- p + geom_text(aes(label = round(mean_utility, 0)),
      color = "white", size = utility_value_size, fontface = "bold"
    )
  }

  if (show_safe) {
    safe <- results_plot %>%
      filter(
        prob_below_SSBmsy <= max_prob_below_SSBmsy,
        prob_above_Fmsy <= max_prob_above_Fmsy,
        catch_avv <= max_catch_avv
      )
    p <- p + geom_text(
      data = safe, aes(label = "o"),
      color = "gray30", size = 6, fontface = "bold", alpha = 0.5
    )
  }

  if (save_plot) {
    n_sc <- length(unique(results_plot$scenario_id))
    ggsave(
      device = ragg::agg_png, paste0("SOPS_utility_contour_", scenario_tag, ".png"),
      width = 8,
      height = 6 * ceiling(n_sc / 3),
      dpi = 300
    )
  }
  p
}


plot_SSBmsy_carruthers <- function(results_all, show_safe = TRUE, save_grid = TRUE,
                                   save_plot = TRUE, filter_scenario = NULL,
                                   show_values = FALSE, value_size = 3,
                                   min_prob_above_half_SSBmsy = 0.60,
                                   min_prob_below_125Fmsy = 0.60,
                                   max_catch_avv = 0.30,
                                   min_mean_catch = 1000) {
  scenario_tag <- if (!is.null(filter_scenario)) paste(filter_scenario, collapse = "_") else "all"

  results_plot <- results_all %>% filter(Qslope > 0)

  if (!is.null(filter_scenario)) {
    results_plot <- results_plot %>% filter(scenario_id %in% filter_scenario)
  }

  if (save_grid) {
    results_plot %>%
      dplyr::select(scenario_id, BminH, Qslope, prob_above_half_SSBmsy) %>%
      mutate(prob_above_half_SSBmsy = round(prob_above_half_SSBmsy, 4)) %>%
      write.table(paste0("SOPS_above_half_SSBmsy_grid_", scenario_tag, ".txt"),
        sep = "\t", row.names = FALSE, quote = FALSE
      )
  }

  p <- ggplot(results_plot, aes(x = BminH, y = Qslope, z = prob_above_half_SSBmsy)) +
    geom_tile(aes(fill = prob_above_half_SSBmsy), color = "white", linewidth = 0.5) +
    scale_fill_viridis_c(name = "P(SSB > 0.5·SSBmsy)", option = "inferno", direction = 1) +
    geom_contour(breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9), color = "black", linewidth = 0.6) +
    geom_text_contour(
      breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      stroke = 0.2, size = 4.5, color = "black", skip = 0,
      label.placer = label_placer_fraction(frac = 0.2)
    ) +
    facet_wrap(~scenario_id) +
    labs(x = "Bmin (kt)", y = "Qslope") +
    scale_x_continuous(breaks = unique(results_plot$BminH), expand = c(0, 0)) +
    scale_y_continuous(breaks = unique(results_plot$Qslope), expand = c(0, 0)) +
    theme_minimal() +
    theme(
      axis.text.x          = element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y          = element_text(size = 16),
      axis.title.x         = element_text(size = 18, face = "bold"),
      axis.title.y         = element_text(size = 18, face = "bold"),
      legend.title         = element_text(size = 14, face = "bold"),
      legend.text          = element_text(size = 14),
      legend.position      = c(0.95, 0.05),
      legend.justification = c("right", "bottom"),
      legend.background    = element_rect(fill = alpha("white", 0.6)),
      legend.key.height    = unit(1., "cm"),
      legend.key.width     = unit(0.6, "cm")
    )

  if (show_values) {
    p <- p + geom_text(aes(label = round(prob_above_half_SSBmsy, 2)),
      color = "white", size = value_size, fontface = "bold"
    )
  }

  if (show_safe) {
    safe <- results_plot %>%
      filter(
        prob_above_half_SSBmsy >= min_prob_above_half_SSBmsy,
        prob_below_125Fmsy >= min_prob_below_125Fmsy,
        catch_avv < max_catch_avv,
        mean_catch >= min_mean_catch
      )
    p <- p + geom_text(
      data = safe, aes(label = "o"),
      color = "gray30", size = 6, fontface = "bold", alpha = 0.5
    )
  }

  if (save_plot) {
    n_sc <- length(unique(results_plot$scenario_id))
    ggsave(
      device = ragg::agg_png, paste0("SOPS_above_half_SSBmsy_contour_", scenario_tag, ".png"),
      width = 8,
      height = 6 * ceiling(n_sc / 3),
      dpi = 300
    )
  }
  p
}
