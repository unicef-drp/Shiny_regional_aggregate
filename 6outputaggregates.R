# 6outputaggregates.R
# Jin Rou New, 2013-2015
# David Sharrow, 2019

run.on.server <- FALSE # Indicate if run is on the server
workdir <- here::here()
if (run.on.server) {
  package.dir <- workdir <- getwd()
} else {
  package.dir <- workdir
}

runname.U5MR <- "GR20190311_all"
runname.IMR <- "IMR20190314_all"
runname.NMR <- "NMR_forDeathCalculation"

year.lastestimatepublished <- 2018.5
date <- "2019-08-15"
file.dir.median <- file.path(paste("Aggregate results (median)", date)) ##<< File directory to save median estimates to
country.info <- read.csv(here::here("input", "country.info.CME_adhoc.csv"), as.is = TRUE)


# The following part is just for double-check
#Note: if replace.rates.reg and replace.rates.cat are NULL, Outputaggregates will calculate aggregates in the conventional way, i.e. replacing missing rates with the regional aggregate rate
#Note: if replace.rates.reg and replace.rates.cat are indicated with the desired aggregate and aggregate categories respectrively, Outputaggregates will calculate aggregates by replacing missing rates with the rate from a replacement aggrgate indicated by replace.rates.reg (the code below is set up to use the M49 aggregate as a replacement)

#Note: to calculate aggregates with a replacement aggregate perform the following steps:
# 1. If generating aggregates for the first time this round, generate aggregates in the conventional way to get country deaths and deaths for the aggregate region (i.e. set replace.rates.reg=NULL and replace.rates.cat=NULL)
# 2. Generate aggregates specifiying the replacement aggregate and categories (samples files for the replacement aggregate will have "-replace" in the file names)

# Total (median)
# 1. Generate aggregates in the conventional way (only needed if this is first time running aggregates this round to get the replacement; world results can be silenced since will use regional replacement for those results)
# NMR file needs processing for median (see: Code/output/NMR_forDeathCalculation/processNMRresultsfilefordeathcalc.R)
if(!file.exists(here::here(file.dir.median, "Rates & Deaths_M49Region.csv")))
  {
  OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
                   results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
                   results.NMR.file = file.path("output", runname.NMR, "Results_NMR_2019-08-15.csv"),
                   run.on.server = run.on.server,
                   year4 = year.lastestimatepublished,
                   output.dir = file.dir.median,
                   year.target = year.lastestimatepublished,
                   est.years = seq(1950.5,year.lastestimatepublished,1),
                   test=FALSE,
                   get.world.results = FALSE,
                   round.output = FALSE,
                   regiontypes.select = c("M49"),
                   replace.rates.reg=NULL,
                   replace.rates.cat=NULL)
}

# 2. Generate aggregates specifiying the replacement aggregate and categories
OutputAggregates(results.U5MR.file = file.path("output", runname.U5MR, "Results.csv"),
                 results.IMR.file = file.path("output", runname.IMR, "Results.csv"),
                 results.NMR.file = file.path("output", runname.NMR, "Results_NMR_2019-08-15.csv"),
                 country.info.file = file.path("input", "country.info.CME_adhoc.csv"),
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.median,
                 year.target = year.lastestimatepublished, est.years = seq(1950.5, year.lastestimatepublished, 1),
                 test = FALSE,
                 round.output = FALSE,
                 regiontypes.select = c("Adhoc"),
                 replace.rates.reg="M49Region",
                 replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", 
                                           country.info$M49Region2[country.info$M49Region1=="Americas"]))


