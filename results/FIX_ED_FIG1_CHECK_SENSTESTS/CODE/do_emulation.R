emulate_era <- function(sim_inputs, sim_outputs, myvar, trend, covtype,
                        expt_design_par) {
  #' Gaussian process emulation.
  #'
  #' Calibrate GP model with DeConto and Pollard simulator data and predict values at large set of new
  #' parameter values. Uses DiceKriging package.
  #'
  #' @param sim_inputs Simulator ensemble inputs, i.e. parameter values.
  #' @param sim_outputs Simulator ensemble outputs, i.e. variable values.
  #' @param myvar Variable to emulate.
  #' @param trend Gaussian Process mean function.
  #' @param covtype Gaussian Process covariance function.
  #' @param expt_design_par Parameter values at which to make predictions.

  # ____________________________________________________________________________
  # FUNCTION THAT RUNS THE EMULATION
  #
  # Fits emulator to ensemble data
  # Predicts values at parameter values in experimental design
  # ____________________________________________________________________________

  # Write all output to stats file
  sink(file = e$outfile_stats, append = TRUE)

  # Fit emulator: Gaussian Process (kriging) model with a
  # given mean and covariance
  print("", quote = FALSE)
  print("==============================================", quote = FALSE)
  print(paste("Emulating", myvar), quote = FALSE)
  print("Mean function:", quote = FALSE)
  print(trend, quote = FALSE)
  print(paste("Covariance:", covtype), quote = FALSE)
  print("", quote = FALSE)

  trend_formula <- as.formula(trend)

  make_gp <- DiceKriging::km(
    formula = trend_formula, sim_inputs, sim_outputs, covtype = covtype
  )
  show(make_gp)

  # Use to predict for a given experimental design
  print("", quote = FALSE)
  print("==============================================", quote = FALSE)
  print(paste("Predicting", myvar), quote = FALSE)
  print("", quote = FALSE)
  ens_pred <- DiceKriging::predict(make_gp, expt_design_par,
    type = "UK", checkNames = TRUE
  )

  sink()

  return(ens_pred)
}
