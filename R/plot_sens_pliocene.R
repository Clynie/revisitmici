plot_sens_pliocene <- function(dataset, calib_data, calib_em = NULL, source,
                               var_future, dobias = NA) {
  #' Plot sensitivity of results to Pliocene minimum.
  #'
  #' @param dataset Dataset to use: simulated or emulated.
  #' @param calib_data Calibration data ranges.
  #' @param calib_em (Optional) Calibration index for emulated ensemble.
  #' @param source Data type: "Simulated" or "Emulated".
  #' @param var_future (Deprecated: RCP8.5 at 2100).
  #' @param dobias Bias on or off (simulated only) or both (used for emulated).

  # ____________________________________________________________________________
  # PLOT SENSITIVITY OF RESULTS TO PLIOCENE LOWER BOUND
  # Extended Data Figure 2
  # ____________________________________________________________________________
  print(paste(
    "PlotSensPliocene(): Plotting sensitivity of",
    source, "data to Pliocene min"
  ))

  # Pass args to relevant names
  if (source == "Simulated") {
    sim_data <- dataset
  }
  if (source == "Emulated") {
    if (!is.na(dobias)) {
      warning("Cannot choose bias value for emulated ensemble:
              must be missing or NA", immediate = TRUE)
      return
    }
    if (is.null(calib_em)) {
      warning("Must provide calibration index for emulated data",
              immediate = TRUE)
      return
    }
    em_data <- dataset
  }

  # Convert to human readable names
  if (is.na(dobias)) {
    bias_name <- "both"
  } else {
    if (dobias == 0) bias_name <- "off"
    if (dobias == 1) bias_name <- "on"
  }
  print(paste("Bias:", bias_name))

  # RCP name for colours
  myrcp <- strsplit(var_future, "_")[[1]][1]

  # X values to plot; max poss for ensemble data is 11.5
  pliocene_minima <- seq(0, 11.5, by = 0.1)

  # Text size
  size_axis_fig2 <- 0.6
  size_lab_fig2 <- 0.6

  # Plot mean and sd (simulated) or quantiles (emulated)
  if (source == "Simulated") {
    sim_plio_mean <- rep(NA, length(pliocene_minima))
    sim_plio_sd <- rep(NA, length(pliocene_minima))
  }
  if (source == "Emulated") {
    em_plio_mode <- rep(NA, length(pliocene_minima))
    quant_list <- c(0.05, 0.25, 0.5, 0.75, 0.95)
    em_plio_quantiles <- list()
    for (quant in quant_list) {
      em_plio_quantiles[[as.character(quant)]] <-
        rep(NA, length(pliocene_minima))
    }
  }
  # Bias on/off and LIG calibration for simulator ensemble (emulated is below)
  if (source == "Simulated") {
    if (is.na(dobias)) {
      sim_lig_post <- sim_data[sim_data$LIG >
        (calib_data[["LIG"]][1] - calib_data[["LIG"]][2]) &
        sim_data$LIG <
          (calib_data[["LIG"]][1] + calib_data[["LIG"]][2]), ]
    } else {
      sim_lig_post <- sim_data[sim_data$BIAS == dobias &
        sim_data$LIG > (calib_data[["LIG"]][1] - calib_data[["LIG"]][2]) &
        sim_data$LIG < (calib_data[["LIG"]][1] + calib_data[["LIG"]][2]), ]
    }
  }

  # Loop through Pliocene minimum values
  # Calculate all the mean/s.d. or quantile/mode values
  for (pm in 1:length(pliocene_minima)) {
    plio_min <- pliocene_minima[pm]

    # Calibrate simulated with Pliocene (LIG was above)
    # Max is written as central value plus width
    if (source == "Simulated") {
      sim_plio_post <-
        sim_lig_post[sim_lig_post$PLIO > plio_min &
                       sim_lig_post$PLIO <
                       (calib_data[["PLIO"]][1] + calib_data[["PLIO"]][2]),
                     var_future]
    }
    # Calibrate emulated with Pliocene and LIG
    if (source == "Emulated") {

      # Bit convoluted: take mean of current min and fixed max (defined as mean + 'error')
      # 'Error' down from mean is difference between this and current min; use for lower bound
      plio_obs <- mean(c(
        plio_min, calib_data[["PLIO"]][1] + calib_data[["PLIO"]][2]
      ))
      plio_err <- plio_obs - plio_min
      tot_err_down <- sqrt((plio_err ** 2 + e$discrep[["PLIO"]] ** 2 +  em_data[, "PLIO_sd"] ** 2))

      # Error up i.e. for upper bound is usual mean + usual total error
      tot_err_up <- sqrt((calib_data[["PLIO"]][2] ** 2 + e$discrep[["PLIO"]] ** 2 +  em_data[, "PLIO_sd"] ** 2))

      # Indices
      calib_plio_down <- em_data[, "PLIO"] > plio_obs - tot_err_down
      calib_plio_up <- em_data[, "PLIO"] < calib_data[["PLIO"]][1] + tot_err_up

      # Calibration of emulated with both LIG and current Pliocene range is here
      em_plio_post <- em_data[calib_em[["LIG"]] & calib_plio_up & calib_plio_down, var_future]

    }

    # If found data OK then calculate mean + sd / quantiles + mode
    if (source == "Simulated" && length(sim_plio_post) > 1) {
      sim_plio_mean[pm] <- mean(sim_plio_post, na.rm = TRUE)
      sim_plio_sd[pm] <- sd(sim_plio_post, na.rm = TRUE)
    }

    if (source == "Emulated" && length(em_plio_post) > 1) {
      for (quant in quant_list) {
        em_plio_quantiles[[as.character(quant)]][pm] <-
          quantile(em_plio_post, quant, na.rm = TRUE)
      }
      # Mode
      em_plio_mode[pm] <-
        density(em_plio_post)$x[which.max(density(em_plio_post)$y)]
    }
  } # for each x-value

  # ____________________________________________________________________________
  # EXTENDED DATA FIGURE 1a, b
  # RCP8.5 at 2100 mean and s.d. vs PLIO minimum
  # ____________________________________________________________________________
  if (source == "Simulated") {
    plot(pliocene_minima[!is.na(sim_plio_mean)],
      sim_plio_mean[!is.na(sim_plio_mean)],
      type = "l", col = e$cols_rcp_dark[[myrcp]], lwd = 2, xlab = "",
      ylim = range(e$var_breaks[[var_future]]), yaxs = "i",
      xlim = range(pliocene_minima), xaxs = "i", ylab = "", main = "",
      cex.axis = size_axis_fig2, cex.lab = size_lab_fig2
    )
    if (bias_name == "off") {
      mtext(paste(
        "Mean and std dev SLE for",
        e$var_labels[[var_future]]
      ),
      side = 2, line = 1.2, cex = size_lab_fig2
      )
    }
    mtext("Lower bound Pliocene (m SLE)",
      side = 1, line = 1.1,
      cex = size_lab_fig2
    )
    Hmisc::minor.tick(nx = 4, ny = 5)

    polygon(c(
      pliocene_minima[!is.na(sim_plio_mean)],
      rev(pliocene_minima[!is.na(sim_plio_mean)])
    ),
    c(
      sim_plio_mean[!is.na(sim_plio_mean)] +
        sim_plio_sd[!is.na(sim_plio_mean)],
      rev(sim_plio_mean[!is.na(sim_plio_mean)] -
        sim_plio_sd[!is.na(sim_plio_mean)])
    ),
    col = e$cols_rcp_light[[myrcp]], border = NA
    )
    lines(pliocene_minima[!is.na(sim_plio_mean)],
      sim_plio_mean[!is.na(sim_plio_mean)] + 2 *
        sim_plio_sd[!is.na(sim_plio_mean)],
      lty = 3, col = e$cols_rcp_dark[[myrcp]]
    )
    lines(pliocene_minima[!is.na(sim_plio_mean)],
      sim_plio_mean[!is.na(sim_plio_mean)] - 2 *
        sim_plio_sd[!is.na(sim_plio_mean)],
      lty = 3, col = e$cols_rcp_dark[[myrcp]]
    )
    abline(v = 5, lty = 5, col = "darkgrey")
    abline(v = 10, lty = 5, col = "darkgrey")
    if (bias_name == "off") sublabel <- "a"
    if (bias_name == "on") sublabel <- "b"
    text(0, 0.95 * max(e$var_breaks[[var_future]]), sublabel,
      font = 2,
      cex = 0.7, pos = 4
    )
  }

  # ____________________________________________________________________________
  # EXTENDED DATA FIGURE 1c
  # RCP8.5 at 2100 quantiles and mode vs PLIO minimum
  # ____________________________________________________________________________

  if (source == "Emulated") {

    plot(pliocene_minima, em_plio_quantiles[["0.5"]],
      type = "l", col = e$cols_rcp_dark[[myrcp]], lwd = 2,
      xlab = "'", ylab = "", main = "",
      ylim = range(e$var_breaks[[var_future]]), yaxs = "i",
      xlim = range(pliocene_minima), xaxs = "i",
      cex.axis = size_axis_fig2, cex.lab = size_lab_fig2
    )
    mtext(paste("Mode and quantiles SLE for", e$var_labels[[var_future]]),
      side = 2, line = 1.2, cex = size_lab_fig2
    )
    mtext("Lower bound Pliocene (m SLE)",
      side = 1, line = 1.1,
      cex = size_lab_fig2
    )
    Hmisc::minor.tick(nx = 4, ny = 5)

    polygon(c(pliocene_minima, rev(pliocene_minima)),
      c(em_plio_quantiles[["0.95"]], rev(em_plio_quantiles[["0.05"]])),
      col = e$cols_rcp_light[[myrcp]], border = e$cols_rcp_dark[[myrcp]]
    )
    polygon(c(pliocene_minima, rev(pliocene_minima)),
      c(em_plio_quantiles[["0.25"]], rev(em_plio_quantiles[["0.75"]])),
      col = e$cols_rcp_light[[myrcp]], border = e$cols_rcp_dark[[myrcp]]
    )
    points(pliocene_minima, em_plio_mode, pch = "*", cex = 0.8)
    abline(v = 5.0, lty = 5, col = "darkgrey")
    abline(v = 10.0, lty = 5, col = "darkgrey")
    text(0, 0.95 * max(e$var_breaks[[var_future]]), "c",
      font = 2,
      cex = 0.7, pos = 4
    )

  }
}
