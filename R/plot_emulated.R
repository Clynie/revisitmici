plot_emulated <- function(sim_data, em_data, calib_data, calib_sim, calib_em,
                          a_type, time, calib_era, cred_list, ep_list) {
  #' Plot and write out emulator results.
  #' @param sim_data Simulation data for plots.
  #' @param em_data Emulation data.
  #' @param calib_data Calibration data ranges.
  #' @param calib_sim Calibration index for simulator ensemble.
  #' @param calib_em Calibration index for emulator ensemble.
  #' @param a_type Analysis type: MICI or NoMICI.
  #' @param time Year of projections (deprecated: only 2100 in data).
  #' @param calib_era Which era(s) to use for calibration (deprecated:
  #' only threeEras is tested).
  #' @param cred_list List of credibility thresholds for data file.
  #' @param ep_list List of exceedance probability thresholds for data file.

  # ___________________________________________________________________________
  # MAIN PLOTTING AND WRITING OF EMULATOR RESULTS
  # Extended Data Figures 4, 3 and 2c
  # Write results to output text file and data file
  # Main Figure 1
  # ___________________________________________________________________________
  stopifnot(a_type %in% c("MICI", "NoMICI"))
  print(paste("PlotEmulated(): Plotting and writing",
               a_type, "emulator results"))

  # ___________________________________________________________________________
  # EXTENDED DATA FIGURE 4
  # 3D PLIO vs LIG vs RCP85_2100 (MICI and NoMICI versions)
  # ___________________________________________________________________________
  if ("RCP85_2100" %in% e$rcps_to_predict) {

    # Colours
    pal <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues")[2:9])
    breaks_SLE <- seq(
      from = min(e$var_breaks[["RCP85_2100"]]),
      to = max(e$var_breaks[["RCP85_2100"]]),
      by = 10 * (e$var_breaks[["RCP85_2100"]][2] -
                   e$var_breaks[["RCP85_2100"]][1])
    )
    colrng_SLE <- pal(length(breaks_SLE) - 1)
    col_em <- colrng_SLE[cut(em_data[, "RCP85_2100"], breaks = breaks_SLE)]
    col_sim_mici <- colrng_SLE[cut(sim_data[
      calib_sim[["present"]] == TRUE,
      "RCP85_2100"
    ],
    breaks = breaks_SLE
    )]
    col_sim_nomici <- colrng_SLE[cut(sim_data[
      sim_data$VCLIF < 0.1 &
        calib_sim[["present"]] == TRUE,
      "RCP85_2100"
    ],
    breaks = breaks_SLE
    )]

    # Plot name
    if (a_type == "MICI") a_type_pn <- "ED_Fig4a"
    if (a_type == "NoMICI") a_type_pn <- "ED_Fig4b"

    # OPEN PLOT FILE
    tiff(
      filename = sprintf("%s/%s_%s.tiff", e$outpath, a_type_pn, a_type),
      width = 7, height = 4.8, units = "in", res = 300
    )

    op <- par(no.readonly = TRUE)
    par(
      mar = c(3, 3, 1, 1), oma = c(0, 0, 0, 0), tcl = -0.3,
      mgp = c(0, 0.5, 0)
    )

    # Text size
    size_axis_fig4 <- 0.85
    size_lab_fig4 <- 0.85

    # Plot: PLIOCENE vs LIG
    plot(em_data[, "LIG"], em_data[, "PLIO"],
      type = "n",
      xlim = range(e$var_breaks[["LIG"]]), ylim = range(e$var_breaks[["PLIO"]]),
      xaxs = "i", yaxs = "i", xlab = "", ylab = "", axes = FALSE
    )

    # Y axis
    ticks_y <- seq(min(e$var_breaks[["PLIO"]]), max(e$var_breaks[["PLIO"]]))
    axis(
      side = 2, pos = min(e$var_breaks[["LIG"]]), at = ticks_y,
      labels = ticks_y, cex.axis = size_axis_fig4,
      cex.lab = size_lab_fig4
    )
    mtext(paste("Sea level equivalent for", e$var_labels[["PLIO"]]),
      side = 2,
      line = 2, cex = size_lab_fig4
    )

    # X axis
    ticks_x <- seq(min(e$var_breaks[["LIG"]]), max(e$var_breaks[["LIG"]]))
    axis(
      side = 1, pos = min(e$var_breaks[["PLIO"]]), at = ticks_x,
      labels = ticks_x, cex.axis = size_axis_fig4,
      cex.lab = size_lab_fig4
    )
    mtext(paste("Sea level equivalent for", e$var_labels[["LIG"]]),
      side = 1,
      line = 2, cex = size_lab_fig4
    )

    # Reconstruction data box
    rect(calib_data[["LIG"]][1] - calib_data[["LIG"]][2],
      calib_data[["PLIO"]][1] - calib_data[["PLIO"]][2],
      calib_data[["LIG"]][1] + calib_data[["LIG"]][2],
      calib_data[["PLIO"]][1] + calib_data[["PLIO"]][2],
      col = rgb(0.9, 0.9, 0.9, 0.4), border = NA
    )

    # Final data box including discrepancy
    rect(calib_data[["LIG"]][1] -
      sqrt(calib_data[["LIG"]][2] ** 2 + e$discrep[["LIG"]] ** 2),
    calib_data[["PLIO"]][1] -
      sqrt(calib_data[["PLIO"]][2] ** 2 + e$discrep[["PLIO"]] ** 2),
    calib_data[["LIG"]][1] +
      sqrt(calib_data[["LIG"]][2] ** 2 + e$discrep[["LIG"]] ** 2),
    calib_data[["PLIO"]][1] +
      sqrt(calib_data[["PLIO"]][2] ** 2 + e$discrep[["PLIO"]] ** 2),
    lwd = 0.7, lty = 5, col = NA, border = "black"
    )

    # Emulator
    points(em_data[, "LIG"], em_data[, "PLIO"],
      pch = 21, cex = 0.3, lwd = 0.5, bg = col_em, col = col_em
    )

    # Overlay those that pass present constraints as solid
    points(em_data[calib_em[["present"]] == TRUE, "LIG"],
      em_data[calib_em[["present"]] == TRUE, "PLIO"],
      pch = 21, cex = 0.6, lwd = 0.5,
      bg = col_em[calib_em[["present"]] == TRUE],
      col = col_em[calib_em[["present"]] == TRUE]
    )

    # Simulator
    if (a_type == "NoMICI") {

      # Plot VCLIF = 0 ensemble members
      points(sim_data[sim_data$VCLIF < 0.1, "LIG"],
        sim_data[sim_data$VCLIF < 0.1, "PLIO"],
        pch = 21, cex = 1.1, lwd = 0.8, bg = NULL, col = grey(0.4)
      )
      # Fill those that pass present data constraints
      points(sim_data[sim_data$VCLIF < 0.1 &
        calib_sim[["present"]] == TRUE, "LIG"],
      sim_data[sim_data$VCLIF < 0.1 &
        calib_sim[["present"]] == TRUE, "PLIO"],
      pch = 21, cex = 1.1, lwd = 0.8, bg = col_sim_nomici,
      col = grey(0.4)
      )
    } else {
      # Plot all ensemble members
      points(sim_data[, "LIG"], sim_data[, "PLIO"],
        pch = 21, cex = 1.4, lwd = 0.8,
        bg = NULL, col = grey(0.4)
      )
      # Fill those that pass present data constraints
      points(sim_data[calib_sim[["present"]] == TRUE, "LIG"],
        sim_data[calib_sim[["present"]] == TRUE, "PLIO"],
        pch = 21, cex = 1.4, lwd = 0.8,
        bg = col_sim_mici, col = grey(0.4)
      )
    }

    # Legend
    text(8.8, 9.1, "SLE for RCP8.5", pos = 4, cex = 0.8)
    text(8.8, 8.6, "at 2100 (cm)", pos = 4, cex = 0.8)
    add_scale(breaks_SLE, sprintf("%.1f", breaks_SLE), colrng_SLE,
      xlim = c(9, 9.5), ylim = c(4, 8), cex = 0.7
    )

    if (a_type == "MICI") sublabel <- "a"
    if (a_type == "NoMICI") sublabel <- "b"
    text(-0.8, 13.5, sublabel, font = 2, pos = 4)

    dev.off()
  } # if RCP8.5 emulated
  # End ED Figure 4
  # ___________________________________________________________________________

  # Loop through RCPs
  for (myvar in e$rcps_to_predict) {

    # UNCALIBRATED AND CALIBRATED EMULATOR ENSEMBLES
    # Uncalibrated is plotted next
    # Calibrated is just for output text file later
    em_prior <- em_data[, myvar]
    em_post <- em_data[calib_em[[calib_era]], myvar]

    # _______________________________________________________________________
    # EXTENDED DATA FIGURE 3
    # RCP8.5 at 2100 vs each calibration variable
    # _______________________________________________________________________

    tiff(
      filename = sprintf("%s/ED_Fig3.tiff", e$outpath),
      width = 5, height = 9.7, units = "in", res = 300
    )

    op <- par(no.readonly = TRUE)
    par(
      mfrow = c(3, 1), mar = c(5, 1.8, 0, 0), oma = c(0, 2, 1, 0.5),
      tcl = -0.3, mgp = c(0, 0.5, 0)
    )

    # Text size
    size_axis_fig3 <- 1
    size_lab_fig3 <- 1

    # Loop through calibration variables for plots
    for (vv in c("PLIO", "LIG", e$var_present)) {
      if (a_type == "MICI" && myvar == "RCP85_2100") {
        plot(sim_data[sim_data$BIAS == 0, vv],
          sim_data[sim_data$BIAS == 0, myvar],
          xlab = "", ylab = "",
          xlim = range(e$var_breaks[[vv]]), xaxs = "i",
          ylim = range(e$var_breaks[[myvar]]), yaxs = "i",
          type = "n", axes = FALSE
        )

        # Y axis
        ticks_y <- seq(min(e$var_breaks[[myvar]]), max(e$var_breaks[[myvar]]),
          by = 50
        )
        axis(
          side = 2, pos = min(e$var_breaks[[vv]]), at = ticks_y,
          labels = ticks_y, cex.axis = size_axis_fig3,
          cex.lab = size_lab_fig3
        )
        mtext(paste("SLE for", e$var_labels[[myvar]]),
          side = 2, line = 2.5,
          cex = size_lab_fig3
        )

        # X axis
        ticks_x <- seq(min(e$var_breaks[[vv]]), max(e$var_breaks[[vv]]))
        axis(
          side = 1, pos = min(e$var_breaks[[myvar]]), at = ticks_x,
          labels = ticks_x,
          cex.axis = size_axis_fig3, cex.lab = size_lab_fig3
        )
        mtext(paste("Sea level equivalent for", e$var_labels[[vv]]),
          side = 1, line = 2.5, cex = size_lab_fig3
        )
        Hmisc::minor.tick(ny = 5)

        # Shaded areas for riginal data ranges (no discrepancy)
        # Palaeo is +- 1 's.d.', present is +- 3 s.d.
        if (vv %in% c("LIG", "PLIO")) {
          polyx <- calib_data[[vv]][1] +
            c(-1, 1, 1, -1) * calib_data[[vv]][2]
        } else {
          polyx <- calib_data[[vv]][1] +
            c(-3, 3, 3, -3) * calib_data[[vv]][2]
        }
        polygon(polyx,
          c(
            rep(min(e$var_breaks[[myvar]]), 2),
            rep(max(e$var_breaks[[myvar]]), 2)
          ),
          col = grey(0.9), border = grey(0.9), density = NULL
        )

        # Dashed lines for total range including discrepancy
        if (vv %in% c("LIG", "PLIO")) {
          abline(
            v = calib_data[[vv]][1] + c(-1, 1) *
              sqrt(calib_data[[vv]][2] ** 2 + e$discrep[[vv]] ** 2),
            lty = 5
          )
        } else {
          abline(
            v = calib_data[[vv]][1] + c(-3, 3) *
              sqrt(calib_data[[vv]][2] ** 2 + e$discrep[["present"]] ** 2),
            lty = 5
          )
        }

        # EMULATED ENSEMBLE
        points(em_data[, vv], em_prior,
          pch = 21, cex = 0.5, lwd = 0.5,
          bg = "darkgray", col = "black"
        )

        # SIMULATED ENSEMBLE
        points(sim_data[sim_data$BIAS == 0, vv],
          sim_data[sim_data$BIAS == 0, myvar],
          pch = 21,
          cex = 0.9, lwd = 1.2, col = "blue", bg = NULL
        )
        points(sim_data[sim_data$BIAS == 1, vv],
          sim_data[sim_data$BIAS == 1, myvar],
          pch = 21,
          cex = 0.9, lwd = 1.2, col = "red", bg = NULL
        )

        if (vv == "PLIO") sublabel <- "a"
        if (vv == "LIG") sublabel <- "b"
        if (vv == "RCP45_pres") sublabel <- "c"
        text(min(e$var_breaks[[vv]]) + 0.1, max(e$var_breaks[[myvar]]) - 10,
          sublabel,
          font = 2, pos = 4
        )

        # Overlay x axis again due to grey shaded box partially hiding it
        axis(
          side = 1, pos = min(e$var_breaks[[myvar]]), at = ticks_x,
          labels = ticks_x, cex.axis = size_axis_fig3,
          cex.lab = size_lab_fig3
        )
      } # if data = RCP8.5 2100
    } # calibration variable loop

    dev.off()

    par(op)

    # _______________________________________________________________________
    # EXTENDED DATA FIGURE 2c
    # RCP8.5 at 2100 vs Pliocene lower bound
    # _______________________________________________________________________

    # We are still inside RCP loop so select RCP8.5
    if (a_type == "MICI" && myvar == "RCP85_2100") {
      tiff(
        filename = sprintf("%s/ED_Fig1c_Em.tiff", e$outpath),
        width = 7.2, height = 3.5, units = "in", res = 300
      )
      # Edit par after tiff changes it
      par(pin = c(7.2, 3.5))
      par(plt = c(1, 1, 1, 1))

      op <- par(no.readonly = TRUE)
      par(
        mar = c(1.3, 1.3, 0.3, 0.5), oma = c(1, 1.2, 0.1, 0),
        tcl = -0.3, mgp = c(0, 0.5, 0)
      )

      plot_sens_pliocene(
        dataset = em_data, calib_data = calib_data, calib_em = calib_em,
        source = "Emulated", var_future = myvar
        )

      dev.off()
      par(op)
    }

    # _______________________________________________________________________
    # PRINT RESULTS TO FILES (for each RCP)
    # _______________________________________________________________________

    # Output to main text file
    cat(
      file = e$outfile_text,
      sprintf("\n========================================\n", myvar),
      append = TRUE
    )
    cat(file = e$outfile_text, sprintf("%s\n", myvar), append = TRUE)
    cat(
      file = e$outfile_text,
      sprintf("========================================\n", myvar),
      append = TRUE
    )

    # Write palaeo-calibrated simulator info first
    if (a_type == "MICI") {
      sim_post <- sim_data[calib_sim[["palaeo"]], myvar]
      cat(
        file = e$outfile_text,
        sprintf(
          "\nSimulator mean \u00B1 s.d. (bias on+off) to compare:
          (%.0f \u00B1 %.0f) cm\n",
          mean(sim_post), sd(sim_post)
        ), append = TRUE
      )
    }

    # Get RCP name
    myrcp <- strsplit(myvar, "_")[[1]][1]

    # Write emulator results: N passing, mode, median, mean
    cat(
      file = e$outfile_text,
      sprintf(
        "\nCALIBRATED EMULATOR ESTIMATES (N = %i)\n\n",
        length(em_post)
      ), append = TRUE
    )
    cat(
      file = e$outfile_text,
      sprintf(
        "Mode: %.0f cm\n",
        density(em_post)$x[which.max(density(em_post)$y)]
      ),
      append = TRUE
    )
    cat(file = e$outfile_text, sprintf(
      "Median: %.0f cm\n",
      quantile(em_post, 0.50)
    ), append = TRUE)
    cat(
      file = e$outfile_text, sprintf("Mean: %.0f cm\n", mean(em_post)),
      append = TRUE
    )

    # Write credibility intervals
    cat(file = e$outfile_text, sprintf(
      "68%% range: [%.0f, %.0f] cm\n",
      quantile(em_post, 0.16),
      quantile(em_post, 0.84)
    ), append = TRUE)
    cat(file = e$outfile_text, sprintf(
      "90%% range: [%.0f, %.0f] cm\n",
      quantile(em_post, 0.05),
      quantile(em_post, 0.95)
    ), append = TRUE)

    # Empirical cdf for exceedance probabilities
    em_ecdf_post <- ecdf(em_post)

    # Write EPs in ep_list
    ep_post <- rep(NA, length(ep_list))
    for (tt in 1:length(ep_list)) {
      thresh <- ep_list[tt]
      ep_post[tt] <- 100 *
        (1 - em_ecdf_post(em_post[which.min(abs(em_post - thresh))]))
      cat(
        file = e$outfile_text,
        sprintf(
          "Probability of exceeding %i cm = %.0f%%\n",
          thresh, ep_post[tt]
        ), append = TRUE
      )
    }

    # _______________________________________________________________________
    # Write main results to comma-separated data file too
    # _______________________________________________________________________

    # Mode, quantiles in cred_list, EPs in ep_list
    cat(file = e$outfile_data, sprintf(
      "%s, %.1f, %s, %s\n", myvar,
      density(em_post)$x[which.max(density(em_post)$y)],
      paste(round(quantile(em_post, prob = cred_list), digits = 1),
        collapse = ", "
      ),
      paste(round(ep_post, digits = 1), collapse = ", ")
    ), append = TRUE)
  } # End of first e$rcps_to_predict loop

  # ____________________________________________________________________________
  # FIGURE 1
  # MAIN PDF PLOTS (MICI and NoMICI)
  # ____________________________________________________________________________

  # Text scaling
  size_axis_main <- 0.5
  size_lab_main <- 0.5

  # Axis set up
  if (a_type == "MICI") ydens <- 0.035
  if (a_type == "NoMICI") ydens <- 0.08
  xmax <- ifelse(time == 2100, 250,
    max(e$var_breaks[[paste("RCP85", time, sep = "_")]])
  )
  xmin <- ifelse(time == 2100, -50,
    min(e$var_breaks[[paste("RCP85", time, sep = "_")]])
  )
  xtext <- -52

  # Plot name
  if (a_type == "MICI") a_type_pn <- "Main_Fig1a"
  if (a_type == "NoMICI") a_type_pn <- "Main_Fig1b"
  pdf(
    file = sprintf("%s/%s_%s.pdf", e$outpath, a_type_pn, a_type),
    width = 3.5, height = 3.5
  )

  op <- par(no.readonly = TRUE)
  par(
    mar = c(2, 1.8, 0.6, 0.5), oma = c(0, 0, 0, 0), tcl = -0.3,
    mgp = c(0, 0.5, 0)
  )
  plot(1:3, 1:3,
    type = "n", main = "", xlim = c(xmin, xmax), ylim = c(0, ydens),
    xlab = "", ylab = "", xaxs = "i", yaxs = "i", axes = FALSE
  )

  # Y axis
  if (a_type == "MICI") ticks_y <- seq(0, ydens, by = 0.005)
  if (a_type == "NoMICI") ticks_y <- seq(0, ydens, by = 0.01)
  axis(
    side = 2, pos = xmin, at = ticks_y, labels = ticks_y,
    cex.axis = size_axis_main, cex.lab = size_lab_main
  )
  mtext("Density", side = 2, line = 1.1, cex = size_lab_main)

  # X axis
  ticks_x <- seq(xmin, xmax, by = 50)
  axis(
    side = 1, pos = 0, at = ticks_x, labels = ticks_x,
    cex.axis = size_axis_main, cex.lab = size_lab_main
  )
  mtext("Sea level equivalent (cm)", side = 1, line = 1.1, cex = size_lab_main)
  Hmisc::minor.tick(nx = 5, ny = 5)

  if (a_type == "MICI") sublabel <- "a"
  if (a_type == "NoMICI") sublabel <- "b"
  text(xmax - 30, 0.95 * ydens, sublabel, font = 2, cex = 0.7, pos = 4)

  # Loop through RCPs again
  for (myvar in e$rcps_to_predict) {

    # Get uncalibrated and calibrated emulator ensembles
    em_prior <- em_data[, myvar]
    em_post <- em_data[calib_em[[calib_era]], myvar]

    # RCP name
    myrcp <- strsplit(myvar, "_")[[1]][1]

    # Plot density estimates
    lines(density(em_prior), lwd = 1.5, col = e$cols_rcp_dark[[myrcp]], lty = 3)
    lines(density(em_post), lwd = 1.5, col = e$cols_rcp_dark[[myrcp]])

    # Legend
    text(xtext, 0.955 * ydens,
      pos = 4, substr(e$var_labels[[myvar]], 1, 6),
      col = e$cols_rcp_dark[[myrcp]], cex = size_lab_main
    )

    # Box and whisker
    lines(quantile(em_post, prob = c(0.05, 0.95)), rep(0.955 * ydens, 2),
      lwd = 1, lend = 2, col = e$cols_rcp_dark[[myrcp]]
    )
    lines(rep(quantile(em_post, 0.05), 2), c(0.945, 0.965) * ydens,
      lwd = 1, lend = 2, col = e$cols_rcp_dark[[myrcp]]
    )
    lines(rep(quantile(em_post, 0.95), 2), c(0.945, 0.965) * ydens,
      lwd = 1, lend = 2, col = e$cols_rcp_dark[[myrcp]]
    )
    rect(quantile(em_post, 0.25), 0.965 * ydens,
      quantile(em_post, 0.75), 0.945 * ydens,
      col = "white", border = e$cols_rcp_dark[[myrcp]]
    )
    lines(rep(quantile(em_post, 0.5), 2), c(0.945, 0.965) * ydens,
      lwd = 1.5, lend = 2, col = e$cols_rcp_dark[[myrcp]]
    )

    # Mode
    points(density(em_post)$x[which.max(density(em_post)$y)], 0.985 * ydens,
      pch = "*", cex = 0.9, col = e$cols_rcp_dark[[myrcp]]
    )

    # Reduce y height for next legend/box and whisker line
    if (a_type == "MICI") ydens <- ydens - 0.0023
    if (a_type == "NoMICI") ydens <- ydens - 0.0046

    # Add DP16 histogram for RCP8.5 and rescale to max height of pdf
    if (a_type == "MICI" && myvar == "RCP85_2100") {

      dp16_hist <- hist(sim_data[calib_sim[["palaeo"]], myvar],
        breaks = e$var_breaks[[myvar]], plot = FALSE
      )
      dp16_hist$density <- dp16_hist$density *
        (max(density(em_post)$y) / max(dp16_hist$density))
      plot(dp16_hist,
        border = rgb(1, 0, 0, 0.1), col = rgb(1, 0, 0, 0.1),
        freq = FALSE, add = TRUE
      )

      # DP16 mean +- 2 s.d.: bar, mean, +- 2 s.d.
      dp16_mean <- mean(sim_data[calib_sim[["palaeo"]], myvar])
      dp16_sd <- sd(sim_data[calib_sim[["palaeo"]], myvar])
      lines(dp16_mean + c(-2, 2) * dp16_sd, rep(0.915 * ydens, 2),
        lwd = 1,
        lend = 2, col = e$cols_rcp_light[[myrcp]]
      )
      lines(rep(dp16_mean, 2), c(0.905, 0.925) * ydens,
        lwd = 1,
        lend = 2, col = e$cols_rcp_light[[myrcp]]
      )
      lines(rep(dp16_mean + 2 * dp16_sd, 2), c(0.905, 0.925) * ydens,
        lwd = 1,
        lend = 2, col = e$cols_rcp_light[[myrcp]]
      )
      lines(rep(dp16_mean - 2 * dp16_sd, 2), c(0.905, 0.925) * ydens,
        lwd = 1,
        lend = 2, col = e$cols_rcp_light[[myrcp]]
      )
      text(xtext, 0.95 * ydens,
        pos = 4,
        "RCP8.5 from DeConto & Pollard (2016): mean \u00B1 2 s.d.",
        col = rgb(1, 0, 0, 0.7), cex = size_lab_main
      )
    } # MICI and RCP8.5 at 2100

    # Add Ritz after 4.5
    if (a_type == "NoMICI" && myvar == "RCP45_2100") {

      lines(e$ritz_d, lwd = 1.2, col = "cadetblue3")

      text(xtext, 0.95 * ydens, pos = 4, "Ritz et al.\nA1B",
            col = "cadetblue3", cex = size_lab_main)

      # Whiskers
      lines (c(e$ritz_q[["q0.05"]], e$ritz_q[["q0.95"]]),
             rep(0.955 * ydens, 2),
             lwd = 2, lend = 2, col = "cadetblue3")
      lines(rep(e$ritz_q[["q0.05"]], 2), c(0.945, 0.965) * ydens,
            lwd = 1, lend = 2, col = "cadetblue3")
      lines(rep(e$ritz_q[["q0.95"]], 2), c(0.945, 0.965) * ydens,
            lwd = 1, lend = 2, col = "cadetblue3")

      # Interquartile range
      rect(e$ritz_q[["q0.25"]], 0.97 * ydens,
           e$ritz_q[["q0.75"]], 0.94 * ydens, col = "white",
           border = "cadetblue3")
      # Median
      lines(rep(e$ritz_q[["q0.5"]], 2), c(0.94, 0.97) * ydens,
            lwd = 1.5, lend = 2, col = "cadetblue3")

      # Mode
      points(e$ritz_m, 0.985 * ydens, pch = "*",
             cex = 0.9, col = "cadetblue3")

      ydens <- ydens - 0.0046

    }
  } # Second e$rcps_to_predict loop

  # Close Figure 1 pdf
  dev.off()
  par(op)
}
