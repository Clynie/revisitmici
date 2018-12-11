plot_simulated <- function(sim_data, calib_sim, pliocene_range, chebyshev) {
  #' Plot DeConto and Pollard (2016) simulator data.
  #'
  #' @param sim_data Simulation dataset.
  #' @param calib_sim Calibration index for simulator ensemble.
  #' @param pliocene_range Name of Pliocene range (used for plot details).
  #' @param chebyshev k-values for Chebyshev inequality intervals to output
  #' to text file.

  # ____________________________________________________________________________
  # Plot DP16 simulator data
  # ____________________________________________________________________________
  print("PlotSimulated(): Plotting and writing simulator data")

  cat(file = e$outfile_text, "\nENSEMBLE DATA\n", append = TRUE)

  # Loop through all chosen variables
  for (myvar in c(e$var_palaeo, e$var_present, e$rcps_to_predict)) {

    # tag for colours
    myrcp <- strsplit(myvar, "_")[[1]][1]

    # _______________________________________________________
    # EXTENDED DATA FIGURE 2
    # HISTOGRAMS
    # _______________________________________________________

    if (myvar %in% e$rcps_to_predict) {

      # Open plot (RCP8.5 only)
      if (myvar == "RCP85_2100") {

      if (pliocene_range == "low") plioplotname <- "ED_Fig2ab"
      if (pliocene_range == "high") plioplotname <- "ED_Fig2cd"

      tiff(
        filename = sprintf(
          "%s/%s_Plio%s.tiff", e$outpath, plioplotname,
          pliocene_range
        ), width = 7.2, height = 3.5,
        units = "in", res = 300
      )
      layout(t(1:2), TRUE)

      # fix par after tiff changes it
      par(pin = c(7.2, 3.5))
      par(plt = c(1, 1, 1, 1))

      op <- par(no.readonly = TRUE)
      par(
        mar = c(1.3, 1.3, 0.3, 0.5), oma = c(1, 1.2, 0.1, 0), tcl = -0.3,
        mgp = c(0, 0.5, 0), xpd = TRUE
      )

      size_axis_fig1 <- 0.6
      size_lab_fig1 <- 0.6

      }

      # Separate results for bias on and off as in DP16
      for (dobias in c(0, 1)) {

        if (dobias == 0) bias_name <- "off"
        if (dobias == 1) bias_name <- "on"

        # UNCALIBRATED AND CALIBRATED ENSEMBLES
        sim_prior <- sim_data[sim_data$BIAS == dobias, myvar]
        sim_post <- sim_data[sim_data$BIAS == dobias &
          calib_sim[["palaeo"]], myvar]

        # OUTPUT CALIBRATED TO TEXT FILES
        cat(file = e$outfile_text, "\n=====================\n", append = TRUE)
        cat(
          file = e$outfile_text, sprintf("%s: BIAS %s\n", myvar, bias_name),
          append = TRUE
        )

        cat(file = e$outfile_text, sprintf(
          "Mean \u00B1 1 s.d.: %.0f \u00B1 %.0f cm\n",
          mean(sim_post), sd(sim_post)
        ), append = TRUE)

        for (cheby in names(chebyshev)) {
          cat(file = e$outfile_text, sprintf(
            "Chebyshev >= %s range = [%.0f, %.0f] cm\n", cheby,
            (mean(sim_post) - unlist(chebyshev[[cheby]]) * sd(sim_post)),
            (mean(sim_post) + unlist(chebyshev[[cheby]]) * sd(sim_post))
          ), append = TRUE)
        }

        # PLOT EXTENDED DATA FIGURE 2: RCP8.5 at 2100 ensemble histograms
        if (myvar == "RCP85_2100") {

          # Axis ranges
          xmax <- max(e$var_breaks[[myvar]])
          ymax <- ifelse(myvar == "RCP26_2100" && dobias == FALSE, 14, 10)

          # Set up plot axes
          sim_hist <- hist(sim_prior,
            xlab = "", ylab = "",
            breaks = e$var_breaks[[myvar]], col = NULL, border = NA,
            cex.axis = size_axis_fig1, cex.lab = size_lab_fig1,
            main = NULL, ylim = c(0, ymax), axes = FALSE
          )
          ticks_y <- 0:10
          axis(
            side = 2, pos = min(e$var_breaks[[myvar]]), at = ticks_y,
            labels = ticks_y, cex.axis = size_axis_fig1, cex.lab = size_lab_fig1
          )
          if (bias_name == "off") {
            mtext("Frequency",
              side = 2, line = 1.2,
              cex = size_lab_fig1
            )
          }

          ticks_x <- seq(
            from = min(e$var_breaks[[myvar]]),
            to = max(e$var_breaks[[myvar]]), by = 50
          )
          axis(
            side = 1, pos = 0, at = ticks_x, labels = ticks_x,
            cex.axis = size_axis_fig1, cex.lab = size_lab_fig1
          )
          mtext(paste("Sea level equivalent for", e$var_labels[[myvar]]),
            side = 1,
            line = 1.1, cex = size_lab_fig1
          )

          # Plot mean +- 1 s.d. first
          text(mean(sim_post), ymax - 1.6, sprintf(
            "Mean \u00B1 1 s.d. range = [%.0f, %.0f] cm",
            mean(sim_post) - sd(sim_post), mean(sim_post) + sd(sim_post)
          ), col = e$cols_rcp_dark[[myrcp]], cex = size_lab_fig1)
          rect(mean(sim_post) - sd(sim_post), 0, mean(sim_post) + sd(sim_post),
            ymax - 2,
            col = e$cols_rcp_light[[myrcp]], border = NA
          )
          lines(rep(mean(sim_post) + sd(sim_post), 2), c(0, ymax - 2),
            col = e$cols_rcp_dark[[myrcp]], lwd = 0.7
          )
          lines(rep(mean(sim_post) - sd(sim_post), 2), c(0, ymax - 2),
            col = e$cols_rcp_dark[[myrcp]], lwd = 0.7
          )
          lines(c(mean(sim_post), mean(sim_post)), c(0, ymax - 2),
            col = e$cols_rcp_dark[[myrcp]], lwd = 2
          )

          # Plot histograms over the top
          hist(sim_prior, breaks = e$var_breaks[[myvar]],
               col = grey(0.9), add = TRUE)
          hist(sim_post,
            breaks = e$var_breaks[[myvar]], col = e$cols_rcp_dark[[myrcp]],
            add = TRUE
          )

          # Plot Chebyshev range along the top
          text(mean(sim_post), ymax - 0.5, sprintf(
            "68%% or greater probability range = [%.0f, %.0f] cm",
            mean(sim_post) - chebyshev[["68%"]] * sd(sim_post),
            mean(sim_post) + chebyshev[["68%"]] * sd(sim_post)
          ), cex = size_lab_fig1)
          lines(
            x = c(mean(sim_post) - chebyshev[["68%"]] * sd(sim_post),
                  mean(sim_post) + chebyshev[["68%"]] * sd(sim_post)),
            y = rep(ymax - 1, 2), lwd = 1.2
          )
          lines(
            x = rep(mean(sim_post) - chebyshev[["68%"]] * sd(sim_post), 2),
            y = ymax - 1 + c(0.1, -0.1), lwd = 1.2
          )
          lines(
            x = rep(mean(sim_post) + chebyshev[["68%"]] * sd(sim_post), 2),
            y = ymax - 1 + c(0.1, -0.1), lwd = 1.2
          )

          # Add legend
          if (pliocene_range == "low") xmaxleg <- xmax - 120
          if (pliocene_range == "high") xmaxleg <- xmax - 103
          text(xmaxleg - 3, 7,
            pos = 4,
            paste("Ocean bias correction", bias_name), cex = size_lab_fig1
          )
          text(xmaxleg + 12, 6.2,
            pos = 4, "Full ensemble",
            cex = size_lab_fig1
          )
          text(xmaxleg + 12, 5.1,
            pos = 4,
            sprintf("Calibrated with\nPliocene (%s)", pliocene_range),
            cex = size_lab_fig1
          )
          text(xmaxleg + 12, 4.5,
            pos = 4,
            sprintf("and Last Interglacial", pliocene_range),
            cex = size_lab_fig1
          )
          rect(xmaxleg + 8, 6.1, xmaxleg + 18, 6.3, col = grey(0.9))
          rect(xmaxleg + 8, 5.3, xmaxleg + 18, 5.5,
               col = e$cols_rcp_dark[[myrcp]])

          if (pliocene_range == "low") {
            if (bias_name == "off") sublabel <- "a"
            if (bias_name == "on") sublabel <- "b"
          }
          if (pliocene_range == "high") {
            if (bias_name == "off") sublabel <- "c"
            if (bias_name == "on") sublabel <- "d"
          }
          text(0.95 * max(e$var_breaks[[myvar]]), 9.5, sublabel,
            font = 2, pos = 4,
            cex = size_lab_fig1
          )
        } # RCP8.5 2100
      } # select bias on/off

      if (myvar == "RCP85_2100") {
      dev.off()
      par(op)
      }
    } # use only future variables
  } # loop all variables
}
