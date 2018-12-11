# Legacy code: used to get RData file from SI of Ritz et al. (2015) Nature
# and strip out 2100 values for data/ file in this package
# Saved here for reproducibility
# New RData file is now called data/ritz_2100.RData
#if (!exists("ritz_dens")) {

#  print("Downloading Ritz data file from Nature...")

#  nature_file <- "nature16147-s3.zip"
#  nature_rdata_file <- "untitled folder/2015-03-03882B-Ritz_et_al_Nature_projections.gz"
#  rdata_file <- "Ritz_et_al_Nature_projections.RData"

  # Download from Nature
#  download.file(sprintf("https://media.nature.com/original/nature-assets/nature/journal/v528/n7580/extref/%s", nature_file),
#                destfile = sprintf("data/%s", nature_file))

#  print("Unzipping file and moving RData to data/")
#  unzip(sprintf("data/%s", nature_file), exdir = "data")
#  system("gunzip 'data/untitled folder/2015-03-03882B-Ritz_et_al_Nature_projections.gz'")
#  system(sprintf("mv 'data/untitled folder/2015-03-03882B-Ritz_et_al_Nature_projections' data/%s", rdata_file))

#  print("Creating stripped down RData file for 2100")
#  load(sprintf("data/%s", rdata_file))

  # Get original posterior density estimate to plot
#  ritz_dens <- densdata[["2100"]]
#  ritz_mode <- 4.79

  # Get quantiles (these are from posterior empirical cdf)
#  ritz_q <- c(0.05, 0.25, 0.50, 0.75, 0.95)
#  ritz_quant <- list()
#  for (rr in ritz_q) {
#    ritz_quant[[paste("q", rr, sep = "")]] <-
#      min( ecdfdata[[ "2100" ]]$x[ ecdfdata[[ "2100" ]]$y >= rr ] )
#  }
#  save(ritz_dens, ritz_quant, ritz_mode,
#       file = "data/Ritz_et_al_Nature_projections_2100.RData" )
#  print("Tidying up.")
#  system(sprintf("rm -r 'data/untitled folder' data/__MACOSX data/%s data/%s", nature_file, rdata_file))

#}
