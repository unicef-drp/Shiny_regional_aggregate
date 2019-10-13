#----------------------------------------------------------------------
# loadlibrariesandcodes.R
# Jin Rou New, 2012-2014
#----------------------------------------------------------------------
LoadLibrariesAndCodes <- function(
  run.on.server = FALSE, ##<< Logical value indicating whether or not to run on server.
  package.dir = NULL, ##<< Path to folder with ``R'' folder.
  do.install = FALSE ##<< Logical value indicating whether or not to install R packages.
) {
  # install and load libraries
  pkgs <- c("MASS", "nlme", "rjags", "R2jags",
            "MCMCpack", "stringr", "mvtnorm", "RColorBrewer", 
            "abind", "plyr", "reshape2", "ggplot2", "gridExtra", "msm","stringdist") 
  if (run.on.server)
    pkgs <- c(pkgs, "foreach", "doMC")
  if (do.install)
    install.packages(pkgs, repos = "http://cran.r-project.org")
  # the splines and grid packages are part of the R distribution, no installation needed
  pkgs <- c(pkgs, "splines", "grid")
  lapply(pkgs, library, character.only = TRUE)
  
  # read in R functions
  rcodes <- c("applyrules", "standardisecountrynames",
              "cleandatafromcmeinfo", "cleandata", 
              "runmcmc", "readdata", "getjagsdata", "getjagsdatasubfunctions", 
              "getvalidationdata", "getq5estimates", 
              "getjagsparameters", "getjagsinits", "writemodel", 
              "readmcmcoutput", "checkconvergence", "constructoutput",
              "plotresults", "plotdataandestimates", "plotcomparison", 
              "plotpriorsandposteriors", "plotpostsubfunctions",
              "generateandplotsourcetypepis", "plotcidotsandsegments", 
              "plotmodelparameters",
              "plotmoreresults", "getbiasadjusteddata",
              "summariseglobalrun", "combinecountryspecificruns",
              "getvalidationresults", "getvalidationresultswithppd", 
              "getvalidationupto2000results", "plotvalidationmeasures",
              "getsplines", "adjustments", "misc", "outputresults",
              "getgammaparameters", "getresiduals", "plotresiduals",
              "getimr", "deriveimrestimatesfromu5mr", "plotresultsforimr",
              "deriveq4fromu5mrandimr",
              "constructoutputforPRK", "combinefinalresults", 
              "outputaggregates", "outputaggregatestocsv", "summariseresults",
              "calculatearrforotherperiods",  "calculateposteriorprobabilities",
              "plotsforratesandarr",
              "getloessestimates",
              "getadjustmentfreetrajectories",
              "extractmodelmethodologysheet")
  invisible(sapply(file.path(package.dir, "R", paste0(rcodes, ".R")), source))
}
