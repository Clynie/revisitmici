#___________________________________________________________________
# MAIN STEERING FILE
# 1. Environment for variables used across functions
# 2. Short add_scale function for ED Figure 4 legend
# 3. main(analysis_type): set up and run analysis, calling functions in other files
#' Environment for useful variables.
#'

e <- new.env(parent = emptyenv())
# Now can assign variables called e$varname

add_scale <- function(legbreaks, leglabels, col, xlim, ylim, cex = par("cex")) {
  #' Add colour scale to figure.
  #'
  #' @param legbreaks Legend breaks.
  #' @param leglabels Legend labels.
  #' @param col Colours.
  #' @param xlim X range.
  #' @param ylim Y range.
  #' @param cex Text size (optional).
  #' add_scale(breaks_SLE, sprintf("%.1f", breaks_SLE), colrng_SLE,
  #' xlim = c(9, 9.5), ylim = c(4, 8), cex = 0.7)
  # ____________________________________________________________________________
  # Add colour scale to figure (only used for ED Figure 4)
  # Adapted from code by Jonty Rougier.
  # ____________________________________________________________________________

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

main <- function(analysis_type) {
  #' Main steering function.
  #'
  #' This is a test paragraph.
  #'
  #' These are the details.
  #'
  #' @param analysis_type Analysis type: MICI, NoMICI or SimHigh.

  # ____________________________________________________________________________
  # RUN ANALYSIS: main("MICI"); main("NoMICI"); main("SimHigh")
  #
  # ANALYSIS SETTINGS:
  #
  # MODEL DISCREPANCY VALUES
  e$discrep <- list()

  # Pliocene model discrepancy in m: default = 5
  e$discrep[["PLIO"]] <- 5
  #
  # Last Interglacial model discrepancy in m: default = 2
  e$discrep[["LIG"]] <- 2
  #
  # 1992-2017 model discrepancy in cm: default = 0.5
  e$discrep[["present"]] <- 0.5
  #
  # Maximum ice wastage parameter value in km/a: default = 5; cannot be larger
  VCLIF_max <- 5
  #
  # End of analysis options
  # ____________________________________________________________________________

  # ____________________________________________________________________________
  # SETUP
  # ____________________________________________________________________________

  e$date_time <- format(Sys.time(), "%Y-%m-%d--%H-%M-%S")
  print(e$date_time)

  set.seed(2017)

  # ____________________________________________________________________________
  # More settings (mainly legacy i.e. do not / cannot change)
  # ____________________________________________________________________________

  # Pliocene data range: default = "low"; alternative = "high"
  # If "high" then only plots simulation data, i.e. no emulation
  if (analysis_type %in% c("MICI", "NoMICI")) pliocene_range <- "low"
  if (analysis_type == "SimHigh") pliocene_range <- "high"

  # Ensemble size and design
  nbig <- 100
  a_type <- analysis_type
  expt_design_name <- NA
  if (a_type == "MICI") expt_design_name <- "UnifLHSContBias"
  if (a_type == "NoMICI") expt_design_name <- "NoCliffContBias"

  # Legacy code: calibrate with all three eras
  calib_era <- "threeEras"  # threeEras, present, palaeo

  # Checks
  stopifnot(analysis_type %in% c("MICI", "NoMICI", "SimHigh"))
  stopifnot(is.na(pliocene_range) || pliocene_range %in% c("low", "high"))
  stopifnot(VCLIF_max <= 5)

  # WHICH PROJECTIONS TO MAKE (legacy)
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
                           package = "revisitmici", mustWork = TRUE)
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

    # This RData file is in the SI of Ritz et al. (2015) Nature
    if (!exists("ritz_dens")) {

      print("Downloading Ritz data file from Nature...")

      nature_file <- "nature16147-s3.zip"
      nature_rdata_file <- "untitled folder/2015-03-03882B-Ritz_et_al_Nature_projections.gz"
      rdata_file <- "Ritz_et_al_Nature_projections.RData"

      # Download from Nature
      download.file(sprintf("https://media.nature.com/original/nature-assets/nature/journal/v528/n7580/extref/%s", nature_file),
                    destfile = sprintf("data/%s", nature_file))

      print("Unzipping file and moving RData to data/")
      unzip(sprintf("data/%s", nature_file), exdir = "data")
      system("gunzip 'data/untitled folder/2015-03-03882B-Ritz_et_al_Nature_projections.gz'")
      system(sprintf("mv 'data/untitled folder/2015-03-03882B-Ritz_et_al_Nature_projections' data/%s", rdata_file))

      print("Creating stripped down RData file for 2100")
      load(sprintf("data/%s", rdata_file))

      # Get original posterior density estimate to plot
      ritz_dens <- densdata[["2100"]]
      ritz_mode <- 4.79

      # Get quantiles (these are from posterior empirical cdf)
      ritz_q <- c(0.05, 0.25, 0.50, 0.75, 0.95)
      ritz_quant <- list()
      for (rr in ritz_q) {
        ritz_quant[[paste("q", rr, sep = "")]] <-
          min( ecdfdata[[ "2100" ]]$x[ ecdfdata[[ "2100" ]]$y >= rr ] )
      }
      save(ritz_dens, ritz_quant, ritz_mode,
           file = "data/Ritz_et_al_Nature_projections_2100.RData" )
      print("Tidying up.")
      system(sprintf("rm -r 'data/untitled folder' data/__MACOSX data/%s data/%s", nature_file, rdata_file))

    }

    # Make available to other functions
    e$ritz_d <- ritz_dens
    e$ritz_q <- ritz_quant
    e$ritz_m <- ritz_mode

    # Contains original posterior density estimate to plot
   # e$ritz_dens <- densdata[["2100"]]

    # Get quantiles (these are from posterior empirical cdf)
    #ritz_q <- c(0.05, 0.25, 0.50, 0.75, 0.95)
    #e$ritz_quant <- list()
    #for (rr in ritz_q) {
    #  e$ritz_quant[[paste("q", rr, sep = "")]] <-
    #    min( ecdfdata[[ "2100" ]]$x[ ecdfdata[[ "2100" ]]$y >= rr ] )
    #}

    # But mode is recalculated from original data with new bandwidth
    # (too hard/slow to reproduce calculation here)
    #e$ritz_quant[["mode"]] <- 4.79

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
  calib_data[["PLIO"]] <- c(mean(pliocene_data),
                            pliocene_data[2] - mean(pliocene_data))
  calib_data[["LIG"]] <- c(mean(LIG), LIG[2] - mean(LIG))

  # IMBIE-2 data: 2,720 ± 1,390 Gt ice lost from 1992-2017
  calib_data[[e$var_present]] <- c(2720, 1390) / (360 * 10)

  # ____________________________________________________________________________
  # OUTPUT FILES
  # ____________________________________________________________________________

  # Text files stamped by date and time
  e$outpath <- "./results"
  e$outfile_text <- sprintf("%s/output--%s.txt", e$outpath, e$date_time)
  e$outfile_stats <- sprintf("%s/stats--%s.txt", e$outpath, e$date_time)
  e$outfile_data <- sprintf("%s/data--%s.txt", e$outpath, e$date_time)

  # Start output text
  cat(file = e$outfile_text, "OUTPUT\n\n")
  cat(file = e$outfile_text, sprintf("Analysis type: %s\n", a_type),
      append = TRUE)
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

  # ENSEMBLE PLOTS
  # Exclude NoMICI just to avoid confusing duplication/overwriting of plots
  if (a_type %in% c("MICI", "SimHigh")) plot_simulated(
    sim_data = sim_data, calib_sim = calib_sim, pliocene_range = pliocene_range,
    chebyshev = chebyshev)

  # If using high Pliocene range we are done (i.e. plot simulation data only)
  if (pliocene_range == "high") {
    return(print("No emulation because pliocene_range = high"))
  }

  # ____________________________________________________________________________
  # START EMULATION PART
  # ____________________________________________________________________________

    # Check got expt design OK
    stopifnot(expt_design_name %in% c("UnifLHSContBias", "NoCliffContBias"))

    cat(file = e$outfile_text,
        "\n\nENSEMBLE CALIBRATION INFORMATION\n\n", append = TRUE)

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
    if (a_type == "MICI" && "RCP85_2100" %in% e$rcps_to_predict) {
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
        mar = c(1.3, 1.3, 0.3, 0.5), oma = c(1, 1.2, 0.1, 0), tcl = -0.3,
        mgp = c(0, 0.5, 0)
      )

      plot_sens_pliocene(
        dataset = sim_data, calib_data = calib_data,
        source = "Simulated", var_future = "RCP85_2100",
        dobias = FALSE
      )
      plot_sens_pliocene(
        dataset = sim_data, calib_data = calib_data,
        source = "Simulated", var_future = "RCP85_2100",
        dobias = TRUE
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

      # Plot variable as a function of parameters
      if (a_type == "MICI" && myvar == "RCP85_2100") {
        plot_params(
          var_plot = myvar, param_list = param_list,
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
      calib_data[["PLIO"]][1]) ** 2
      / (calib_data[["PLIO"]][2] ** 2 + e$discrep[["PLIO"]] ** 2 +
        em_data[, "PLIO_sd"] ** 2)) < 1
    calib_em[["LIG"]] <- sqrt((em_data[, "LIG"] - calib_data[["LIG"]][1]) ** 2
      / (calib_data[["LIG"]][2] ** 2 + e$discrep[["LIG"]] ** 2 +
        em_data[, "LIG_sd"] ** 2)) < 1
    calib_em[["present"]] <- sqrt((em_data[, e$var_present] -
      calib_data[[e$var_present]][1]) ** 2
      / (calib_data[[e$var_present]][2] ** 2 + e$discrep[["present"]] ** 2 +
        em_data[, paste(e$var_present, "sd", sep = "_")] ** 2)) < 3
    calib_em[["threeEras"]] <- calib_em[["PLIO"]] & calib_em[["LIG"]] &
      calib_em[["present"]]

    # Output ranges to files
    cat(file = e$outfile_text,
        "\nCalibration ranges (not including emulator variance):",
        append = TRUE)
    cat(file = e$outfile_text, sprintf(
      "\nPLIO calibration: %.1f %.1f\n",
      calib_data[["PLIO"]][1] - sqrt(calib_data[["PLIO"]][2] ** 2 +
        e$discrep[["PLIO"]] ** 2),
      calib_data[["PLIO"]][1] + sqrt(calib_data[["PLIO"]][2] ** 2 +
        e$discrep[["PLIO"]] ** 2)
    ), append = TRUE)
    cat(file = e$outfile_text, sprintf(
      "LIG calibration: %.1f %.1f\n",
      calib_data[["LIG"]][1] - sqrt(calib_data[["LIG"]][2] ** 2 +
        e$discrep[["LIG"]] ** 2),
      calib_data[["LIG"]][1] + sqrt(calib_data[["LIG"]][2] ** 2 +
        e$discrep[["LIG"]] ** 2)
    ), append = TRUE)
    cat(file = e$outfile_text, sprintf(
      "1992-2017 calibration (without emulator variance): %.1f %.1f\n",
      calib_data[[e$var_present]][1] - 3 *
        sqrt(calib_data[[e$var_present]][2] ** 2 +
               e$discrep[[e$var_present]] ** 2),
      calib_data[[e$var_present]][1] + 3 *
        sqrt(calib_data[[e$var_present]][2] ** 2 +
               e$discrep[[e$var_present]] ** 2)
    ), append = TRUE)


    # __________________________________________________________________________
    # PLOTS AND OUTPUT TEXT FILES
    # __________________________________________________________________________

    cat(file = e$outfile_text, "\n\nEMULATOR RESULTS\n", append = TRUE)

    for (tt in years_to_predict) plot_emulated(
      sim_data = sim_data, em_data = em_data,
      calib_data = calib_data, calib_sim = calib_sim,
      calib_em = calib_em, a_type = a_type, time = tt,
      calib_era = calib_era, cred_list = cred_list,
      ep_list = ep_list
    )

} # end of main
