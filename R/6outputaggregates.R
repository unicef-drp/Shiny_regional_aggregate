# 6outputaggregates.R
# Jin Rou New, 2013-2015
# David Sharrow, 2019



#The following part is just for double-check Note: if replace.rates.reg and
#replace.rates.cat are NULL, Outputaggregates will calculate aggregates in the
#conventional way, i.e. replacing missing rates with the regional aggregate rate
#Note: if replace.rates.reg and replace.rates.cat are indicated with the desired
#aggregate and aggregate categories respectrively, Outputaggregates will
#calculate aggregates by replacing missing rates with the rate from a
#replacement aggrgate indicated by replace.rates.reg (the code below is set up
#to use the M49 aggregate as a replacement)

#Note: to calculate aggregates with a replacement aggregate perform the
#following steps: 1. If generating aggregates for the first time this round,
#generate aggregates in the conventional way to get country deaths and deaths
#for the aggregate region (i.e. set replace.rates.reg=NULL and
#replace.rates.cat=NULL) 2. Generate aggregates specifiying the replacement
#aggregate and categories (samples files for the replacement aggregate will have
#"-replace" in the file names)

# Total (median) 1. Generate aggregates in the conventional way (only needed if
# this is first time running aggregates this round to get the replacement; world
# results can be silenced since will use regional replacement for those results)
# NMR file needs processing for median (see:
# Code/output/NMR_forDeathCalculation/processNMRresultsfilefordeathcalc.R)

run.outputaggregates <- function(year.lastestimatepublished){
if(!file.exists(file.path(dir_median_total, "Rates & Deaths_M49Region.csv")))
  {
  OutputAggregates(results.U5MR.file = here::here("output", runname.U5MR, "Results.csv"),
                   results.IMR.file = here::here("output", runname.IMR, "Results.csv"),
                   results.NMR.file = here::here("output", file_name_NMR),
                   run.on.server = FALSE,
                   year4 = year.lastestimatepublished,
                   output.dir = dir_median_total,
                   year.target = year.lastestimatepublished,
                   est.years = seq(1950.5, year.lastestimatepublished,1),
                   test=FALSE,
                   get.world.results = FALSE,
                   round.output = FALSE,
                   regiontypes.select = c("M49"),
                   replace.rates.reg=NULL,
                   replace.rates.cat=NULL)
}

# 2. Generate aggregates specifiying the replacement aggregate and categories
OutputAggregates(results.U5MR.file = here::here("output", runname.U5MR, "Results.csv"),
                 results.IMR.file = here::here("output", runname.IMR, "Results.csv"),
                 results.NMR.file = here::here("output", file_name_NMR),
                 country.info.file = here::here("input", "country.info.CME_adhoc.csv"),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_total,
                 year.target = year.lastestimatepublished, 
                 est.years = seq(1950.5, year.lastestimatepublished, 1),
                 test = FALSE,
                 round.output = FALSE,
                 regiontypes.select = c("Adhoc"),
                 replace.rates.reg="M49Region",
                 replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", 
                                           country.info$M49Region2[country.info$M49Region1=="Americas"]))


}