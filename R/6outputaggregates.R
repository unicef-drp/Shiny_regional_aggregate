# 6outputaggregates.R
# Jin Rou New, 2013-2015
# David Sharrow, 2019


# Aggregates for under-five total  ----------------------------------------


run.outputaggregates <- function(year.lastestimatepublished, reuse.replacement.country = TRUE){
if(!file.exists(file.path(dir_median_total, "Rates & Deaths_M49Region.csv")))
  {
  OutputAggregates(results.U5MR.file = here::here("output", runname.U5MR, "Results.csv"),
                   results.IMR.file  = here::here("output", runname.IMR,  "Results.csv"),
                   results.NMR.file  = here::here("output", file_name_NMR),
                   run.on.server = FALSE,
                   year4 = year.lastestimatepublished,
                   output.dir = dir_median_total,
                   year.target = year.lastestimatepublished,
                   est.years = seq(1950.5, year.lastestimatepublished,1),
                   test=FALSE,
                   get.world.results = FALSE,
                   round.output = FALSE,
                   output.rates.of.decline = FALSE,
                   regiontypes.select = c("M49"),
                   replace.rates.reg = NULL,
                   replace.rates.cat = NULL)
}

# 2. Generate aggregates specifying the replacement aggregate and categories
OutputAggregates(results.U5MR.file = here::here("output", runname.U5MR, "Results.csv"),
                 results.IMR.file  = here::here("output", runname.IMR, "Results.csv"),
                 results.NMR.file  = here::here("output", file_name_NMR),
                 country.info.file = here::here("input", "country.info.CME_adhoc.csv"),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_total,
                 year.target = year.lastestimatepublished, 
                 est.years = seq(1950.5, year.lastestimatepublished, 1),
                 test = FALSE,
                 round.output = FALSE,
                 output.rates.of.decline = FALSE,
                 regiontypes.select = c("Adhoc"),
                 replace.rates.reg = "M49Region",
                 reuse.replacement.country = reuse.replacement.country,
                 
                 replace.rates.cat = replace(
                   country.info$M49Region1,
                   country.info$M49Region1 == "Americas",
                   country.info$M49Region2[country.info$M49Region1 ==
                                             "Americas"]
                 ))


}
