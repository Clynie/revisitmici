plot_params <-
  function(var_plot, param_list, sim_inputs, sim_outputs, em_data) {
  #' Plot RCP8.5 at 2100 versus 3 parameters.
  #'
  #' @param var_plot Variable to plot (deprecated: only uses RCP8.5 at 2100).
  #' @param param_list List of parameters (deprecated: VCLIF, CREVLIQ, OCFAC).
  #' @param sim_inputs Simulator parameter values.
  #' @param sim_outputs Simulator variable values.
  #' @param em_data Emulator parameter and variable values.

  # ____________________________________________________________________________
  # EXTENDED DATA FIGURE 7: RCP8.5 at 2100 vs 3 parameters
  # ____________________________________________________________________________
  print("PlotParams(): Plotting RCP8.5 at 2100 vs parameters")

  tiff(
    filename = sprintf("%s/ED_Fig7.tiff", e$outpath), width = 5,
    height = 9.7, units = "in", res = 300
  )

  op <- par(no.readonly = TRUE)
  par(
    mfrow = c(3, 1), mar = c(5, 1.8, 0, 0), oma = c(0, 2, 1, 0.7),
    tcl = -0.3, mgp = c(0, 0.5, 0)
  )

  # Text size
  size_axis_fig7 <- 1
  size_lab_fig7 <- 1

  # Plot ensemble design: outputs vs inputs
  # (in reverse order so VCLIF first subfigure)
  for (param in param_list[3:1]) {
    ticks_y <- seq(min(e$var_breaks[[var_plot]]),
                   max(e$var_breaks[[var_plot]]), by = 50)
    if (param %in% c("VCLIF", "OCFAC")) {
      ticks_x <- seq(-1, max(sim_inputs[, param]) + 1)
    } else {
      ticks_x <- seq(-10, max(sim_inputs[, param]) + 10, by = 10)
    }

    plot(sim_inputs[, param], sim_outputs,
      type = "n",
      xlim = range(ticks_x), ylim = range(ticks_y), xaxs = "i",
      yaxs = "i", xlab = "", ylab = "", axes = FALSE
    )

    # Y axis
    axis(
      side = 2, pos = min(ticks_x), at = ticks_y, labels = ticks_y,
      cex.axis = size_axis_fig7, cex.lab = size_lab_fig7
    )
    mtext(paste("SLE for", e$var_labels[[var_plot]]),
      side = 2, line = 2.5,
      cex = size_lab_fig7 * 0.8
    )

    # X axis
    axis(
      side = 1, pos = min(ticks_y), at = ticks_x, labels = ticks_x,
      cex.axis = size_axis_fig7, cex.lab = size_lab_fig7
    )
    mtext(param, side = 1, line = 2.5, cex = size_lab_fig7 * 0.8)

    errbars <- sapply(1:dim(em_data)[1], function(ss) {
      lines(rep(em_data[ss, param], 2),
        c(
          em_data[ss, paste(var_plot, "upper", sep = "_")],
          em_data[ss, paste(var_plot, "lower", sep = "_")]
        ),
        lwd = 0.1, col = rgb(0, 0, 0, 0.2)
      )
    })
    points(em_data[, param], em_data[, var_plot],
      pch = 21, cex = 0.7, col = "black", bg = "darkgray"
    )

    # No bias distinction colors for palaeo
    if (var_plot %in% e$var_palaeo) {
      points(sim_inputs[, param], sim_outputs,
        pch = 21, col = "darkmagenta",
        bg = NULL, cex = 1.4, lwd = 1.5
      )
    }
    else {
      points(sim_inputs[sim_inputs$BIAS == 0, param],
        sim_outputs[sim_inputs$BIAS == 0],
        pch = 21, col = "blue",
        bg = NULL, cex = 1.4, lwd = 1.5
      )
      points(sim_inputs[sim_inputs$BIAS == 1, param],
        sim_outputs[sim_inputs$BIAS == 1],
        pch = 21, col = "red",
        bg = NULL, cex = 1.4, lwd = 1.5
      )
    }

    if (param == "VCLIF") sublabel <- "a"
    if (param == "CREVLIQ") sublabel <- "b"
    if (param == "OCFAC") sublabel <- "c"
    text(min(ticks_x), 0.95 * max(ticks_y), sublabel,
      font = 2, pos = 4,
      cex = size_lab_fig7
    )
  } # parameters

  par(op)
  dev.off()
}
