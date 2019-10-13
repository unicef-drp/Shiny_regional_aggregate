#----------------------------------------------------------------------
# 7plots.R
# Leontine Alkema & Jin Rou New, 2012-2015
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# 1. User settings
#----------------------------------------------------------------------
rm(list = ls()) # Clear workspace
run.on.server <- F # Indicate if run is on the server
workdir <- "C:/Users/lhug/Dropbox/UN IGME data/2019 Round Estimation/Code" # Give work directory file path if not running things on server

# Define working directory
if (run.on.server) {
  package.dir <- workdir <- getwd()
} else {
  package.dir <- workdir
}
setwd(workdir)
#----------------------------------------------------------------------
# 2. Load libraries and codes
#----------------------------------------------------------------------
source(file.path(package.dir, "R/loadlibrariesandcodes.R"))
# NOTE: if you run this for the first time, use do.install = TRUE to install all packages needed 
# LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir, do.install = TRUE)

LoadLibrariesAndCodes(run.on.server = run.on.server, package.dir = package.dir)
#----------------------------------------------------------------------
# 3. Get all required inputs
#----------------------------------------------------------------------
file.dir.output <- file.path(paste("Aggregate results (final)")) ##<< File directory where results are located and to save final (combined) median estimates + UIs to
fig.dir <- "fig"
year.lastestimatepublished <- 2018.5
regiontype.select <- "MDGRegion" ##<< Region classification to use
high.mortality.only <- FALSE ##<< Get plot for high-mortality (U5MR 1990 >= 40 per 1,000 live births) countries only?
isos.to.exclude <- c("PRK", "LIE") ##<< 3-character ISO country codes of countries to exclude in plot
# End of user input
#----------------------------------------------------------------------
# Read in estimates
CIs.readin <- read.csv(file.path(file.dir.output, "Rates & Deaths_Country Summary.csv"), 
                       header = T, stringsAsFactors = F)
CIs.region.readin <- read.csv(file.path(file.dir.output, 
                                        paste0("Rates & Deaths_", regiontype.select, ".csv")), 
                              header = T, stringsAsFactors = F)
CIsRoD.readin <- read.csv(file.path(file.dir.output, "Rates of Decline_Country Summary.csv"), 
                          header = T, stringsAsFactors = F)
CIsRoD.region.readin <- read.csv(file.path(file.dir.output, 
                                           paste0("Rates of Decline_", regiontype.select, ".csv")), 
                                 header = T, stringsAsFactors = F)
# Format estimates
est.country.L <- list(name.c = CIs.readin$CountryName[CIs.readin$X == "Median"],
                      iso.c = CIs.readin$ISO3Code[CIs.readin$X == "Median"],
                      region.c = CIs.readin[CIs.readin$X == "Median", 
                                            colnames(CIs.readin) == paste0(regiontype.select, "1")],
                      U5MR.1990 = CIs.readin$U5MR.1990[CIs.readin$X == "Median"],
                      U5MR.1995 = CIs.readin$U5MR.1995[CIs.readin$X == "Median"],
                      U5MR.2000 = CIs.readin$U5MR.2000[CIs.readin$X == "Median"],
                      U5MR.2005 = CIs.readin$U5MR.2005[CIs.readin$X == "Median"],
                      U5MR.lastyear = CIs.readin[CIs.readin$X == "Median",
                                                 paste0("U5MR", floor(year.lastestimatepublished))])
names(est.country.L)[names(est.country.L) == "U5MR.lastyear"] <- paste0("U5MR", floor(year.lastestimatepublished))
select.c <- !is.element(CIsRoD.readin$ISO3Code, isos.to.exclude)
if (high.mortality.only)
  select.c <- select.c & is.element(CIsRoD.readin$ISO3Code, est.country.L$iso.c[est.country.L$U5MR.1990 >= 40])
RoD.country.L <- list(name.c = CIsRoD.readin$CountryName,
                      iso.c = CIsRoD.readin$ISO3Code,
                      region.c = CIsRoD.readin[, colnames(CIsRoD.readin) == paste0(regiontype.select, "1")],
                      CIs.ARR.cq = cbind(CIsRoD.readin[
                        , paste0("ARR.1990.", floor(year.lastestimatepublished), ".lower.bound")], 
                        CIsRoD.readin[
                          , paste0("ARR.1990.", floor(year.lastestimatepublished), ".median")], 
                        CIsRoD.readin[
                          , paste0("ARR.1990.", floor(year.lastestimatepublished), ".upper.bound")]), 
                      CIs.changeinARR.cq = cbind(CIsRoD.readin$Change.in.ARR.lower.bound, 
                                                 CIsRoD.readin$Change.in.ARR.median, 
                                                 CIsRoD.readin$Change.in.ARR.upper.bound),
                      select.c = select.c,
                      year.current = floor(year.lastestimatepublished))
sum(RoD.country.L$select.c) # 194; 103
# region + world
CIs.region.readin <- CIs.region.readin[is.element(CIs.region.readin$Region, c("World", est.country.L$region.c)) &
                                         is.element(CIs.region.readin$Year, 1990:(floor(year.lastestimatepublished))), ]
name.r <- unique(CIs.region.readin$Region)
year.t <- unique(CIs.region.readin$Year)+0.5
est.region.L <- list(name.r = name.r, year.t = year.t)
est.region.L$CIs.U5MR.rqt <- est.region.L$CIs.U5deaths.rqt <- array(NA, dim = c(length(name.r), 3, length(year.t)))
for (r in 1:length(name.r)) {
  est.region.L$CIs.U5MR.rqt[r, 1, ] <- CIs.region.readin$U5MR.lower.bound[CIs.region.readin$Region == name.r[r]]
  est.region.L$CIs.U5MR.rqt[r, 2, ] <- CIs.region.readin$U5MR.median[CIs.region.readin$Region == name.r[r]]
  est.region.L$CIs.U5MR.rqt[r, 3, ] <- CIs.region.readin$U5MR.upper.bound[CIs.region.readin$Region == name.r[r]]
  est.region.L$CIs.U5deaths.rqt[r, 1, ] <- CIs.region.readin$Under.five.deaths.lower.bound[CIs.region.readin$Region == name.r[r]]
  est.region.L$CIs.U5deaths.rqt[r, 2, ] <- CIs.region.readin$Under.five.deaths.median[CIs.region.readin$Region == name.r[r]]
  est.region.L$CIs.U5deaths.rqt[r, 3, ] <- CIs.region.readin$Under.five.deaths.upper.bound[CIs.region.readin$Region == name.r[r]]
}
RoD.region.L <- list(name.r = CIsRoD.region.readin$Region,
                     CIs.ARR.rq = cbind(CIsRoD.region.readin[
                       , paste0("ARR.1990.", floor(year.lastestimatepublished), ".lower.bound")], 
                       CIsRoD.region.readin[
                         , paste0("ARR.1990.", floor(year.lastestimatepublished), ".median")], 
                       CIsRoD.region.readin[
                         , paste0("ARR.1990.", floor(year.lastestimatepublished), ".upper.bound")]),
                     CIs.changeinARR.rq = cbind(CIsRoD.region.readin$Change.in.ARR.lower.bound, 
                                                CIsRoD.region.readin$Change.in.ARR.median, 
                                                CIsRoD.region.readin$Change.in.ARR.upper.bound),
                     year.current = floor(year.lastestimatepublished))
#----------------------------------------------------------------------
# 3. Do plots
#----------------------------------------------------------------------
# RoD.country.L$select.c = is.element(CIsRoD.readin$ISO3Code, 
#                       c("ARM", "AZE", "GEO", "KAZ", "KGZ", "MDA", "TJK", "TKM", "UZB", "MKD"))
# ARR categories (Italian flag plot)
PlotARRCategories(RoD.L = RoD.country.L,
                  RoD.region.L = RoD.region.L,
                  fig.dir = fig.dir, 
                  name.pdf = paste0("ARR categories", 
                                    ifelse(high.mortality.only, " (High mortality)", ""), ".pdf"))
# Change in ARR categories (white green plot)
PlotChangeInARRCategories(RoD.L = RoD.country.L,
                          RoD.region.L = RoD.region.L,
                          fig.dir = fig.dir,
                          year1 = 1990,
                          year2 = 2000,
                          year4 = floor(year.lastestimatepublished),
                          name.pdf = paste0("Change in ARR categories", 
                                            ifelse(high.mortality.only, " (High mortality)", ""), ".pdf"))
# # number of countries with acceleration
# select <- CIsRoD.readin$Change.in.ARR.lower.bound > 0 & CIsRoD.readin$Change.in.ARR.upper.bound > 0 &
#   !is.element(CIsRoD.readin$ISO3Code, isos.to.exclude) 
# if (high.mortality.only)
#   select <- select & is.element(CIsRoD.readin$ISO3Code, est.country.L$iso.c[est.country.L$U5MR.1990 >= 40])
# sum(select)
# table(CIsRoD.readin[select, paste0(regiontype.select, "1)])
# CIsRoD.readin$CountryName[select]
#----------------------------------------------------------------------
# Get model methodology sheet



ExtractModelMethodologySheet(
  database.U5MR = "input/data_U5MR_20190722.csv",
  database.IMR = "input/data_IMR_20190718.csv",
  estimation.info.file.U5MR = "input/country.estimate.info.csv",
  estimation.info.file.IMR = "input/country.estimate.info_IMR.csv",
  model.info.file = "input/infoUNinclHIV.csv",
  hiv.file.U5MR = "input/dataUNAIDS_U5MR.csv",
  hiv.file.IMR = "input/dataUNAIDS_IMR.csv",
  adj.file.U5MR = "input/dataPostAdj_U5MR.csv",
  adj.file.IMR = "input/dataPostAdj_IMR.csv",
  output.methodology.sheet.file = "output/Methodology sheet.csv",
  country.order.file = "input/MethodologyCountryOrder.csv" ##<< First two columns of last year's methodology sheet, to preserve order
)
#----------------------------------------------------------------------
# Fin.
