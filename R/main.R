# ___________________________________________________________________
# MAIN STEERING FILE
# 1. Line defining environment for variables used across functions
# 2. Short add_scale() function for ED Figure 4 legend
# 3. Main function main(): set up and run analysis, calling functions in
#    other files

#' Environment for useful variables.
e <- new.env(parent = emptyenv())
# Now can assign variables called e$varname to use anywhere

add_scale <- function(legbreaks, leglabels, col, xlim, ylim, cex = par("cex")) {
  #' Colour scale.
  #'
  #' Add colour scale to a figure. Adapted from code by Jonty Rougier.
  #'
  #' @param legbreaks Set of legend breaks.
  #' @param leglabels Set of legend labels.
  #' @param col Set of colours.
  #' @param xlim X min, max for placement.
  #' @param ylim Y min, max for placement.
  #' @param cex Text size (optional).

  n <- length(legbreaks)
  stopifnot(length(col) == n - 1)
  dy <- diff(ylim) / (n - 1)
  ybot <- seq(from = ylim[1], by = dy, length.out = n - 1)

  # Palette levels and box
  rect(rep(xlim[1], n - 1), ybot, rep(xlim[2], n - 1), ybot + dy,
    col = col, border = "grey",
    xpd = NA, lwd = 0, ljoin = 1
  )
  rect(xlim[1], ylim[1], xlim[2], ylim[2],
    col = NA, border = par("fg"),
    xpd = NA, lwd = 0.4, ljoin = 1
  )

  labcex <- cex

  # labels (auto)
  if (is.null(leglabels)) {
    db <- diff(range(legbreaks)) / (n - 1)
    pp <- pretty(legbreaks)
    pp <- pp[pp >= legbreaks[1] + db / 2 & pp <= legbreaks[n] - db / 2]
    xx <- rep(xlim[2] + 0.002, length(pp))
    yy <- ylim[1] + diff(ylim) * (pp - legbreaks[1]) / (diff(range(legbreaks)))
  } else {
    # (prescribed)
    pp <- leglabels
    xx <- rep(xlim[2] + 0.002, length(pp))
    yy <- seq(from = ylim[1], by = dy, length.out = n)
    if (length(leglabels) == length(legbreaks) - 2) labcex <- 0.8 * cex
  }

  # text and ticks
  text(xx, yy, pp, cex = labcex, xpd = NA, pos = 4)
  sapply(yy - 0.00001, function(ytick) {
    lines(c(xlim[2], xlim[2] + 0.007), c(ytick, ytick), lwd = 0.4)
  })
}

main <- function(analysis_type, test = FALSE, calib_era = "threeEras",
                 discrep_PLIO = NULL, discrep_LIG = NULL,
                 discrep_present = NULL, VCLIF_max = 5) {
  #' Run revisitmici analysis.
  #'
  #' This is the main steering function to run the analysis for Edwards et al.
  #' (2019) "Revisiting Antarctic ice loss due to marine ice cliff instability",
  #' Nature, producing figures and numerical output for Antarctic projections
  #' at 2100. Uses lhs package to generate maximin Latin Hypercube design
  #' points. All results for figures and tables can be generated except
  #' timeseries projections (Figures 2, 3) and emulator validation (Figure 6).
  #' Results are given for Figure 4 and ED Figure 5, but the manuscript figures
  #' are not plotted.
  #'
  #' All argument combinations to reproduce results are given in "Examples"
  #' below. N.B. Currently does not check if any calibration arguments are
  #' provided but unused in analysis type: will just ignore them.
  #'
  #' @param analysis_type Analysis type. Options: "MICI", "NoMICI" or "SimHigh".
  #' @param test If TRUE, sets emulator ensemble size to N = 500 instead of
  #' 10,000.
  #' @param calib_era Which eras to calibrate with. Default = "threeEras"
  #' (Pliocene, Last interglacial and 1992-2017). Other options = "palaeo"
  #' (Pliocene and LIG only), "present" (1992-2017 only).
  #' If "palaeo" or "present", then only plot Figure 1 (as ED Figure 1c
  #' assumes all three eras used and other ED figures do not change).
  #' @param discrep_PLIO Pliocene model discrepancy in metres. Default = 5.
  #' Set to 0 or 10 for sensitivity analyses in ED Figure 5. If provided, then
  #' only plot Figure 1.
  #' @param discrep_LIG Last Interglacial model discrepancy in metres.
  #' Default = 2. Set to 0 or 4 for sensitivity analyses in ED Figure 5.
  #' If provided, then only plot Figure 1.
  #' @param discrep_present 1992-2017 model discrepancy in cm. Default = 0.5.
  #' Set to 0 or 1 for sensitivity analyses in ED Figure 5. If provided,
  #' then only plot Figure 1.
  #' @param VCLIF_max Maximum ice wastage parameter value in km per year.
  #' Default = 5 and cannot be larger. Sensitivity analysis in ED Figure 5 uses
  #' 4 km/a.
  #' @return Creates a ./results/ directory with the output of the analysis.
  #' $DATESTAMP is the current date and time in YY-mm-dd--HH-MM-SS format.
  #' output--$DATESTAMP.txt: Human-readable analysis settings and results.
  #' data--$DATESTAMP.txt: CSV of main results (if emulating).
  #' stats--$DATESTAMP.txt: output of emulation from do_emulation.R (if emulating).
  #' Main_Fig*.pdf: Main paper figures in pdf format.
  #' ED_Fig*.tiff: Extended Data figures in TIFF format.
  #' @examples
  #' \dontrun{
  #' # Main projections:
  #' main("MICI") # Figure 1a; ED Figures 1, 2a,b, 3, 4a, 7
  #' main("NoMICI") # Figure 1b; ED Figure 4c
  #' main("SimHigh") # ED Figure 2b,c
  #'
  #' # Sensitivity tests for ED Figure 5:
  #' main("MICI", calib_era = "palaeo")
  #' main("MICI", calib_era = "present")
  #' main("MICI", discrep_PLIO = 10, discrep_LIG = 4, discrep_present = 1)
  #' main("MICI", discrep_PLIO = 0, discrep_LIG = 0, discrep_present = 0)
  #' main("MICI", calib_era = "palaeo", discrep_PLIO = 0, discrep_LIG = 0)
  #' main("MICI", VCLIF_max = 4)
  #' main("NoMICI", calib_era = "palaeo")
  #' main("NoMICI", calib_era = "present")
  #' main("NoMICI", discrep_PLIO = 10, discrep_LIG = 4, discrep_present = 1)
  #' main("NoMICI", discrep_PLIO = 0, discrep_LIG = 0, discrep_present = 0)
  #' main("NoMICI", calib_era = "palaeo", discrep_PLIO = 0, discrep_LIG = 0)
  #' }


  # ____________________________________________________________________________
  # SETUP
  # ____________________________________________________________________________

  # Only plot simulator and emulator ensembles if default calibration settings
  # VCLIF_max does change emulator ensemble so don't include this in list
  if (calib_era == "threeEras" && is.null(discrep_PLIO) &&
    is.null(discrep_LIG) && is.null(discrep_present)) e$plot_ensembles <- TRUE
  else e$plot_ensembles <- FALSE

  # Default discrepancy values
  if (is.null(discrep_PLIO)) discrep_PLIO <- 5
  if (is.null(discrep_LIG)) discrep_LIG <- 2
  if (is.null(discrep_present)) discrep_present <- 0.5

  e$date_time <- format(Sys.time(), "%Y-%m-%d--%H-%M-%S")
  print(e$date_time)

  set.seed(2017)

  # ____________________________________________________________________________
  # More settings (mainly legacy i.e. do not / cannot change)
  # ____________________________________________________________________________

  # Pliocene data range: default = "low" for projections; alternative = "high"
  # If "high" then only plots simulation data, i.e. no emulation
  if (analysis_type %in% c("MICI", "NoMICI")) pliocene_range <- "low"
  if (analysis_type == "SimHigh") pliocene_range <- "high"

  # Ensemble size and design
  nbig <- ifelse(test, 500, 10000)
  expt_design_name <- NA
  a_type <- analysis_type
  if (a_type == "MICI") expt_design_name <- "UnifLHSContBias"
  if (a_type == "NoMICI") expt_design_name <- "NoCliffContBias"

  # Model discrepancies for calibration
  e$discrep <- list()
  e$discrep[["PLIO"]] <- discrep_PLIO
  e$discrep[["LIG"]] <- discrep_LIG
  e$discrep[["present"]] <- discrep_present

  # Checks
  stopifnot(analysis_type %in% c("MICI", "NoMICI", "SimHigh"))
  stopifnot(calib_era %in% c("threeEras", "palaeo", "present"))
  if (discrep_PLIO < 0 || discrep_LIG < 0 || discrep_present < 0) {
    stop("Discrepancy values must be positive.")
  }
  if (VCLIF_max > 5) stop("VCLIF_max cannot be larger than 5.")

  # Should be OK as set here
  stopifnot(is.na(pliocene_range) || pliocene_range %in% c("low", "high"))

  # WHICH PROJECTIONS TO MAKE (legacy: only have 2100 data; always do all RCPs)
  # Times; future variables to plot/emulate (RCPs x times)
  years_to_predict <- 2100
  rcp_list <- c("RCP26", "RCP45", "RCP85")
  e$rcps_to_predict <- paste(rcp_list,
    rep(years_to_predict, each = length(rcp_list)),
    sep = "_"
  )

  # Chebyshev inequality for output text file
  chebyshev <- list()
  chebyshev[["68%"]] <- sqrt(1 / (1 - 0.68))
  chebyshev[["90%"]] <- sqrt(1 / (1 - 0.9))

  # Credibility intervals for data file (hard coded for output file)
  cred_list <- c(0.05, 0.16, 0.50, 0.84, 0.95)

  # Emulated exceedance probabilities in cm for output text and data files
  ep_list <- c(30, 50, 100)

  # ____________________________________________________________________________
  # READ DP16 AND RITZ DATA
  # ____________________________________________________________________________

  dp16_file <- system.file("extdata", "DeConto_and_Pollard_2016.csv",
    package = "revisitmici", mustWork = TRUE
  )
  sim_data <- as.data.frame(
    read.csv(dp16_file,
      row.names = 1, stringsAsFactors = FALSE,
      colClasses = "numeric"
    )
  )
  ncol <- dim(sim_data)[2]

  # Convert present and future sea level contribution from m to cm
  sim_data[7:ncol] <- sim_data[7:ncol] * 100

  # Column names to use later
  param_list <- names(sim_data)[1:4] # parameters
  e$var_palaeo <- names(sim_data)[5:6] # PLIO, LIG
  e$var_present <- names(sim_data)[7] # present day (using 'RCP4.5' run)
  # (The remaining 8:ncol columns are the future)

  # All variables to plot/emulate i.e. add past eras
  # Always need LIG, PLIO and present day, for calibration plots later
  vars_to_emulate <- c("LIG", "PLIO", e$var_present, e$rcps_to_predict)

  # Load Ritz data for main Figure 1b
  if (a_type == "NoMICI") {

    # Density estimate stripped from R data file in Nature S.I.
    e$ritz_d <- revisitmici::ritz_dens

    # Quantiles extracted from R data file in Nature S.I.
    e$ritz_q <- list()
    e$ritz_q[["q0.05"]] <- 1.97
    e$ritz_q[["q0.25"]] <- 5.45
    e$ritz_q[["q0.5"]] <- 11.9
    e$ritz_q[["q0.75"]] <- 17.9
    e$ritz_q[["q0.95"]] <- 29.6

    # Mode recalculated with narrower bandwidth (cm)
    e$ritz_m <- 4.79
  }

  # ____________________________________________________________________________
  # OBSERVATIONAL CONSTRAINTS
  # ____________________________________________________________________________

  # Palaeodata: treating bounds as mean +/- 1 std dev throughout
  if (pliocene_range == "low") pliocene_data <- c(5, 15)
  if (pliocene_range == "high") pliocene_data <- c(10, 20)
  LIG <- c(3.5, 7.4)

  # Names match columns of input csv
  calib_data <- list()
  calib_data[["PLIO"]] <- c(
    mean(pliocene_data),
    pliocene_data[2] - mean(pliocene_data)
  )
  calib_data[["LIG"]] <- c(mean(LIG), LIG[2] - mean(LIG))

  # IMBIE-2 data: 2,720 ± 1,390 Gt ice lost from 1992-2017
  calib_data[[e$var_present]] <- c(2720, 1390) / (360 * 10)

  # ____________________________________________________________________________
  # OUTPUT FILES
  # ____________________________________________________________________________

  # Text files stamped by date and time
  e$outpath <- "./results"
  if (!dir.exists(e$outpath)) {
    print(paste("Creating directory for output:", e$outpath))
    dir.create(e$outpath)
  }
  e$outfile_text <- sprintf("%s/output--%s.txt", e$outpath, e$date_time)
  e$outfile_stats <- sprintf("%s/stats--%s.txt", e$outpath, e$date_time)
  e$outfile_data <- sprintf("%s/data--%s.txt", e$outpath, e$date_time)

  # Start output text
  cat(file = e$outfile_text, "OUTPUT\n\n")
  cat(
    file = e$outfile_text, sprintf("Analysis type: %s\n", a_type),
    append = TRUE
  )
  cat(
    file = e$outfile_text,
    sprintf("PLIO data range: %.1f %.1f\n", pliocene_data[1], pliocene_data[2]),
    append = TRUE
  )
  cat(
    file = e$outfile_text,
    sprintf("LIG data range: %.1f %.1f\n", LIG[1], LIG[2]),
    append = TRUE
  )

  if (a_type %in% c("MICI", "NoMICI")) {
    cat(file = e$outfile_text, sprintf(
      "1992-2017 range (\u00B13 sigma): %.1f %.1f\n",
      calib_data[[e$var_present]][1] - 3 * calib_data[[e$var_present]][2],
      calib_data[[e$var_present]][1] + 3 * calib_data[[e$var_present]][2]
    ), append = TRUE)

    # Output emulation-relevant choices
    if (length(e$discrep) > 0) {
      cat(
        file = e$outfile_text,
        sprintf("PLIO model discrep: %.1f\n", e$discrep[["PLIO"]]),
        append = TRUE
      )
      cat(
        file = e$outfile_text,
        sprintf("LIG model discrep: %.1f\n", e$discrep[["LIG"]]),
        append = TRUE
      )
      cat(file = e$outfile_text, sprintf(
        "Present day model discrep: %.1f\n",
        e$discrep[["present"]]
      ), append = TRUE)
    }
  }

  # ____________________________________________________________________________
  # PLOTTING SET-UP
  # ____________________________________________________________________________

  # Default colours for projections
  e$cols_rcp_light <- list()
  e$cols_rcp_light[["RCP26"]] <- rgb(0, 0, 0, 0.2)
  e$cols_rcp_light[["RCP45"]] <- rgb(0, 0, 1, 0.2)
  e$cols_rcp_light[["RCP85"]] <- rgb(1, 0, 0, 0.3)
  e$cols_rcp_dark <- list()
  e$cols_rcp_dark[["RCP26"]] <- grey(0.2)
  e$cols_rcp_dark[["RCP45"]] <- "darkblue"
  e$cols_rcp_dark[["RCP85"]] <- "darkred"

  # Default axis ranges and divisions
  e$var_breaks <- list()
  e$var_breaks[["PLIO"]] <- seq(2, 14, by = 0.5)
  e$var_breaks[["LIG"]] <- seq(-1, 11, by = 0.5)
  e$var_breaks[[e$var_present]] <- seq(-5, 10, by = 1)
  for (myrcp in e$rcps_to_predict) {
    e$var_breaks[[myrcp]] <- seq(-50, 250, by = 5)
  }

  # Default axis labels
  e$var_labels <- list()
  e$var_labels[["LIG"]] <- "Last Interglacial (m)"
  e$var_labels[["PLIO"]] <- "Pliocene (m)"
  e$var_labels[[e$var_present]] <- "1992-2017 (cm)"
  for (tt in years_to_predict) {
    e$var_labels[[paste("RCP26", tt, sep = "_")]] <-
      paste("RCP2.6 at", tt, "(cm)")
    e$var_labels[[paste("RCP45", tt, sep = "_")]] <-
      paste("RCP4.5 at", tt, "(cm)")
    e$var_labels[[paste("RCP85", tt, sep = "_")]] <-
      paste("RCP8.5 at", tt, "(cm)")
  }

  # ____________________________________________________________________________
  # IMPLAUSIBILITY THRESHOLDS
  # ____________________________________________________________________________

  # ENSEMBLE IMPLAUSIBILITY: use original DP16 method i.e. no model error
  # (and no emulator of course)
  implausibility_sim <- list()
  implausibility_sim[["PLIO"]] <-
    abs((sim_data$PLIO - calib_data[["PLIO"]][1]) / (calib_data[["PLIO"]][2]))
  implausibility_sim[["LIG"]] <-
    abs((sim_data$LIG - calib_data[["LIG"]][1]) / (calib_data[["LIG"]][2]))
  implausibility_sim[["present"]] <-
    abs((sim_data[, e$var_present] - calib_data[[e$var_present]][1]) /
      (calib_data[[e$var_present]][2]))

  # Data within ± 1 'sigma' for palaeo and ± 3 sigma for satellite
  # Single and combinations of eras used for calibration
  calib_sim <- list()
  calib_sim[["PLIO"]] <- implausibility_sim[["PLIO"]] < 1
  calib_sim[["LIG"]] <- implausibility_sim[["LIG"]] < 1
  calib_sim[["present"]] <- implausibility_sim[["present"]] < 3
  calib_sim[["palaeo"]] <- calib_sim[["PLIO"]] & calib_sim[["LIG"]]
  calib_sim[["threeEras"]] <- calib_sim[["PLIO"]] & calib_sim[["LIG"]] &
    calib_sim[["present"]]

  # ____________________________________________________________________________
  # PLOT RAW ENSEMBLE DATA
  # ____________________________________________________________________________

  # SIMULATOR ENSEMBLE PLOTS: Extended Data Figure 2
  # Exclude NoMICI just to avoid confusing duplication/overwriting of plots
  if (a_type %in% c("MICI", "SimHigh") && e$plot_ensembles) plot_simulated(
      sim_data = sim_data, calib_sim = calib_sim,
      pliocene_range = pliocene_range, chebyshev = chebyshev
    )

  # If using high Pliocene range we are done (i.e. plot simulation data only)
  if (pliocene_range == "high") {
    return(print("No emulation because pliocene_range = high"))
  }

  # ____________________________________________________________________________
  # START EMULATION PART
  # ____________________________________________________________________________

  # Check got expt design OK
  stopifnot(expt_design_name %in% c("UnifLHSContBias", "NoCliffContBias"))

  cat(
    file = e$outfile_text,
    "\n\nENSEMBLE CALIBRATION INFORMATION\n\n", append = TRUE
  )

  # Write out more choices
  cat(
    file = e$outfile_text,
    sprintf("Ensemble Design: %s\n", expt_design_name),
    append = TRUE
  )

  # Data file header
  cat(file = e$outfile_data, sprintf(
    "RCP_year, mode, %s, %s\n",
    paste("q", cred_list, sep = "", collapse = ", "),
    paste("ep", ep_list, sep = "", collapse = ", ")
  ))

  # Sensitivity of calibrated RCP8.5 at 2100 mean & s.d. to Pliocene min bound
  if (a_type == "MICI" && "RCP85_2100" %in% e$rcps_to_predict &&
    e$plot_ensembles) {
    tiff(
      filename = sprintf("%s/ED_Fig1ab_Sim.tiff", e$outpath), width = 7.2,
      height = 3.5, units = "in", res = 300
    )
    layout(t(1:2), TRUE)

    # fix up par after tiff ruins it
    par(pin = c(7.2, 3.5))
    par(plt = c(1, 1, 1, 1))

    op <- par(no.readonly = TRUE)
    par(
      mar = c(1.3, 1.3, 0.3, 0.5), oma = c(1, 1.2, 0.1, 0), tcl = 0.3,
      mgp = c(0, 0.2, 0)
    )

    # Variable to plot is "RCP85_2100" by default
    plot_sens_pliocene(
      dataset = sim_data, calib_data = calib_data,
      source = "Simulated", dobias = FALSE
    )
    plot_sens_pliocene(
      dataset = sim_data, calib_data = calib_data,
      source = "Simulated", dobias = TRUE
    )
    dev.off()
    par(op)
  }

  # __________________________________________________________________________
  # ENSEMBLE DESIGNS
  # __________________________________________________________________________

  # Create structure for LHS designs
  expt_design_params <- NULL
  expt_design_row <- sim_data[1, param_list[1:4]]
  for (pp in 1:nbig) expt_design_params <- rbind(
      expt_design_params, expt_design_row
    )

  # UnifLHSContBias
  if (expt_design_name == "UnifLHSContBias") {
    make_lhs <- lhs::maximinLHS(nbig, 4)

    # Flll and transform to original ranges
    expt_design_params[, "OCFAC"] <- make_lhs[, 1] * 10
    expt_design_params[, "CREVLIQ"] <- make_lhs[, 2] * 150
    expt_design_params[, "VCLIF"] <- make_lhs[, 3] * VCLIF_max
    expt_design_params[, "BIAS"] <- make_lhs[, 4]
  }

  # NoCliffContBias: VCLIF = 0 (cliff failure off)
  if (expt_design_name == "NoCliffContBias") {

    # Smaller Latin Hypercube
    make_lhs <- lhs::maximinLHS(nbig, 3)

    # As above but VCLIF = 0
    expt_design_params[, "OCFAC"] <- make_lhs[, 1] * 10
    expt_design_params[, "CREVLIQ"] <- make_lhs[, 2] * 150
    expt_design_params[, "VCLIF"] <- rep(0, nbig)
    expt_design_params[, "BIAS"] <- make_lhs[, 3]
  }

  # __________________________________________________________________________
  # EMULATION
  # __________________________________________________________________________

  # Matrix: 4 cols (mean, sd, upper, lower) for each era; nbig rows
  resultscols <- matrix(rep(NA, 4 * length(vars_to_emulate) *
    dim(expt_design_params)[1]),
  ncol = 4 * length(vars_to_emulate)
  )

  # Stick expt_design_params and results columns together
  em_data <- cbind(expt_design_params, resultscols)

  # Column index starts after expt_design_params
  cc <- dim(expt_design_params)[2] + 1

  for (myvar in vars_to_emulate) {
    if (myvar %in% c("LIG", "PLIO")) {

      # No bias parameter for palaeo
      sim_inputs <- sim_data[sim_data$BIAS == 0, param_list[1:3]]
      sim_outputs <- sim_data[sim_data$BIAS == 0, myvar]
      expt_design_par <- expt_design_params[, param_list[1:3]]
    } else {
      sim_inputs <- sim_data[, param_list]
      sim_outputs <- sim_data[, myvar]
      expt_design_par <- expt_design_params
    }

    # EMULATOR MEAN AND COVARIANCE FUNCTIONS
    # Model building and validation analysis not included (see Methods)
    covtype <- "exp"
    if (myvar == "LIG") {
      trend <- "~ OCFAC + CREVLIQ + VCLIF + CREVLIQ:VCLIF"
      covtype <- "powexp"
    }
    if (myvar == "PLIO") trend <- "~ OCFAC + CREVLIQ + VCLIF + CREVLIQ:VCLIF"
    if (myvar == e$var_present) {
      trend <- "~ OCFAC + CREVLIQ + VCLIF + BIAS + OCFAC:VCLIF + OCFAC:BIAS +
        CREVLIQ:VCLIF + VCLIF:BIAS + OCFAC:VCLIF:BIAS"
      covtype <- "matern3_2"
    }
    if (strsplit(myvar, "_")[[1]][1] == "RCP26") trend <- "~ OCFAC + CREVLIQ +
      VCLIF + BIAS + OCFAC:VCLIF + OCFAC:BIAS + CREVLIQ:VCLIF + VCLIF:BIAS +
      OCFAC:VCLIF:BIAS"
    if (strsplit(myvar, "_")[[1]][1] == "RCP45") trend <- "~ OCFAC + CREVLIQ +
      VCLIF + BIAS + OCFAC:VCLIF + OCFAC:BIAS + CREVLIQ:VCLIF"
    if (strsplit(myvar, "_")[[1]][1] == "RCP85") trend <- "~ OCFAC + CREVLIQ +
      VCLIF + BIAS + OCFAC:VCLIF + OCFAC:BIAS + CREVLIQ:VCLIF"

    # PREDICT SIMULATOR OUTPUT AT DESIGN POINTS FOR THIS ERA
    # EmulateEra function is in functions.R
    em_pred <- emulate_era(
      sim_inputs, sim_outputs, myvar, trend, covtype,
      expt_design_par
    )

    # ADD EMULATOR RESULTS FOR THIS ERA TO MAIN MATRIX OF RESULTS
    em_data[, cc] <- em_pred$mean
    em_data[, cc + 1] <- em_pred$sd
    em_data[, cc + 2] <- em_pred$lower
    em_data[, cc + 3] <- em_pred$upper
    names(em_data)[cc:(cc + 3)] <- c(
      myvar, paste(myvar, "sd", sep = "_"),
      paste(myvar, "lower", sep = "_"), paste(myvar, "upper", sep = "_")
    )
    cc <- cc + 4 # ready for next era

    # Plot variable as a function of parameters: Extended Data Figure 7
    # Default var = "RCP85_2100" so need to set this if changing conditional
    if (a_type == "MICI" && myvar == "RCP85_2100" && e$plot_ensembles) {
      plot_params(
        sim_inputs = sim_inputs, sim_outputs = sim_outputs,
        em_data = em_data
      )
    }
  } # list of variables

  # __________________________________________________________________________
  # CALIBRATION INDICES FOR EMULATOR ENSEMBLE
  # (includes model error and emulator errors in acceptance range)
  # __________________________________________________________________________

  calib_em <- list()

  # Calibration index for each era = TRUE where implausibility < 1 for
  # each palaeo and < 3 for satellite
  calib_em[["PLIO"]] <- sqrt((em_data[, "PLIO"] -
    calib_data[["PLIO"]][1])**2
    / (calib_data[["PLIO"]][2]**2 + e$discrep[["PLIO"]]**2 +
      em_data[, "PLIO_sd"]**2)) < 1
  calib_em[["LIG"]] <- sqrt((em_data[, "LIG"] - calib_data[["LIG"]][1])**2
    / (calib_data[["LIG"]][2]**2 + e$discrep[["LIG"]]**2 +
      em_data[, "LIG_sd"]**2)) < 1
  calib_em[["present"]] <- sqrt((em_data[, e$var_present] -
    calib_data[[e$var_present]][1])**2
    / (calib_data[[e$var_present]][2]**2 + e$discrep[["present"]]**2 +
      em_data[, paste(e$var_present, "sd", sep = "_")]**2)) < 3
  calib_em[["palaeo"]] <- calib_em[["PLIO"]] & calib_em[["LIG"]]
  calib_em[["threeEras"]] <- calib_em[["PLIO"]] & calib_em[["LIG"]] &
    calib_em[["present"]]

  # Output ranges to files
  cat(
    file = e$outfile_text, sprintf("Calibration type: %s\n", calib_era),
    append = TRUE
  )
  cat(
    file = e$outfile_text,
    "\nCalibration ranges (not including emulator variance):\n",
    append = TRUE
  )
  if (calib_era %in% c("threeEras", "palaeo")) {
    cat(file = e$outfile_text, sprintf(
      "PLIO calibration: %.1f %.1f\n",
      calib_data[["PLIO"]][1] -
        sqrt(calib_data[["PLIO"]][2]**2 + e$discrep[["PLIO"]]**2),
      calib_data[["PLIO"]][1] +
        sqrt(calib_data[["PLIO"]][2]**2 + e$discrep[["PLIO"]]**2)
    ), append = TRUE)
    cat(file = e$outfile_text, sprintf(
      "LIG calibration: %.1f %.1f\n",
      calib_data[["LIG"]][1] -
        sqrt(calib_data[["LIG"]][2]**2 + e$discrep[["LIG"]]**2),
      calib_data[["LIG"]][1] +
        sqrt(calib_data[["LIG"]][2]**2 + e$discrep[["LIG"]]**2)
    ), append = TRUE)
  }
  if (calib_era %in% c("threeEras", "present")) {
    cat(file = e$outfile_text, sprintf(
      "1992-2017 calibration: %.1f %.1f\n",
      calib_data[[e$var_present]][1] -
        3 * sqrt(calib_data[[e$var_present]][2]**2 + e$discrep[["present"]]**2),
      calib_data[[e$var_present]][1] +
        3 * sqrt(calib_data[[e$var_present]][2]**2 + e$discrep[["present"]]**2)
    ), append = TRUE)
  }
  # __________________________________________________________________________
  # PLOTS AND OUTPUT TEXT FILES
  # __________________________________________________________________________

  cat(file = e$outfile_text, "\n\nEMULATOR RESULTS\n", append = TRUE)

  # Only 2100 is provided in data file, but keep this structure in case adapt
  # to use more data later
  for (tt in years_to_predict) plot_emulated(
      em_data = em_data, calib_data = calib_data, calib_em = calib_em,
      sim_data = sim_data, calib_sim = calib_sim,
      a_type = a_type, time = tt,
      calib_era = calib_era, cred_list = cred_list,
      ep_list = ep_list
    )
} # end of main
