
# For older-children total ------------------------------------------------

run.outputaggregates.5.24 <- function(){
  OutputAggregates.ori(results.U5MR.file = here::here("output", "10q5", "Results.csv"),
                       results.IMR.file  = here::here("output", "5q5",  "Results.csv"),
                       country.info.file = here::here("input", "country.info.CME.5_14_adhoc.csv"),
                       population.file   = here::here("input", "country.info.CME.5_14_adhoc.csv"),
                       output.dir = dir_median_total_5_14,
                       regiontypes.select = c("Adhoc"))
  
  OutputAggregates.ori(results.U5MR.file = here::here("output", "10q15", "Results.csv"),
                       results.IMR.file  = here::here("output", "5q15",  "Results.csv"),
                       country.info.file = here::here("input", "country.info.CME.15_24_adhoc.csv"),
                       population.file   = here::here("input", "country.info.CME.15_24_adhoc.csv"),
                       output.dir = dir_median_total_15_24,
                       regiontypes.select = c("Adhoc"))
}