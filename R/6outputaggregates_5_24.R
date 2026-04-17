
# For older-children total ------------------------------------------------

run.outputaggregates.5.24 <- function(year.lastestimatepublished){
  OutputAggregates.ori(results.U5MR.file = file.path("output", "10q5", "Results.csv"),
                       results.IMR.file  = file.path("output", "5q5",  "Results.csv"),
                       country.info.file = file.path("input", "country.info.CME.5_14_adhoc.csv"),
                       population.file   = file.path("input", "country.info.CME.5_14_adhoc.csv"),
                       output.dir = dir_median_total_5_14,
                       year.target = year.lastestimatepublished,
                       regiontypes.select = "Adhoc")
  
  OutputAggregates.ori(results.U5MR.file = file.path("output", "10q15", "Results.csv"),
                       results.IMR.file  = file.path("output", "5q15",  "Results.csv"),
                       country.info.file = file.path("input", "country.info.CME.15_24_adhoc.csv"),
                       population.file   = file.path("input", "country.info.CME.15_24_adhoc.csv"),
                       output.dir = dir_median_total_15_24,
                       year.target = year.lastestimatepublished,
                       regiontypes.select = c("Adhoc"))
}


# For older-children sex-specific -----------------------------------------

run.outputaggregates.5.24.gender <- function(year.lastestimatepublished){
  
  # Male 5-14 ----
  OutputAggregates.ori(results.U5MR.file = file.path("output", "Sex_forDeathCalculation", "Results_10q5_m.csv"),
                       results.IMR.file  = file.path("output", "Sex_forDeathCalculation", "Results_5q5_m.csv"),
                       country.info.file = file.path("input", "country.info.CME.5_14_adhoc.csv"),
                       population.file   = file.path("input", "country.info.CME.5_14_adhoc.csv"),
                       output.dir = dir_median_male_5_14,
                       year.target = year.lastestimatepublished,
                       regiontypes.select = "Adhoc")
  
  # Male 15-24 ----
  OutputAggregates.ori(results.U5MR.file = file.path("output", "Sex_forDeathCalculation", "Results_10q15_m.csv"),
                       results.IMR.file  = file.path("output", "Sex_forDeathCalculation", "Results_5q15_m.csv"),
                       country.info.file = file.path("input", "country.info.CME.15_24_adhoc.csv"),
                       population.file   = file.path("input", "country.info.CME.15_24_adhoc.csv"),
                       output.dir = dir_median_male_15_24,
                       year.target = year.lastestimatepublished,
                       regiontypes.select = "Adhoc")
  
  # Female 5-14 ----
  OutputAggregates.ori(results.U5MR.file = file.path("output", "Sex_forDeathCalculation", "Results_10q5_f.csv"),
                       results.IMR.file  = file.path("output", "Sex_forDeathCalculation", "Results_5q5_f.csv"),
                       country.info.file = file.path("input", "country.info.CME.5_14_adhoc.csv"),
                       population.file   = file.path("input", "country.info.CME.5_14_adhoc.csv"),
                       output.dir = dir_median_female_5_14,
                       year.target = year.lastestimatepublished,
                       regiontypes.select = "Adhoc")
  
  # Female 15-24 ----
  OutputAggregates.ori(results.U5MR.file = file.path("output", "Sex_forDeathCalculation", "Results_10q15_f.csv"),
                       results.IMR.file  = file.path("output", "Sex_forDeathCalculation", "Results_5q15_f.csv"),
                       country.info.file = file.path("input", "country.info.CME.15_24_adhoc.csv"),
                       population.file   = file.path("input", "country.info.CME.15_24_adhoc.csv"),
                       output.dir = dir_median_female_15_24,
                       year.target = year.lastestimatepublished,
                       regiontypes.select = "Adhoc")
}


# Adjust death for 5-24 age groups ----------------------------------------

adjust.total.death.5.24 <- function(region_name = "AdhocCountries"){
  
  # Adjust 5-14 age group ----
  file.i.f <- read.csv(file.path(dir_median_female_5_14, paste0("/Rates & Deaths_", region_name, ".csv")), as.is=T)
  file.i.m <- read.csv(file.path(dir_median_male_5_14, paste0("/Rates & Deaths_", region_name, ".csv")), as.is=T)
  file.i.t <- read.csv(file.path(dir_median_total_5_14, paste0("/Rates & Deaths_", region_name, ".csv")), as.is=T)
  
  ## Adjust total to be sum of female + male
  file.i.t.adj <- file.i.t
  
  # 10q5 deaths: set total = female + male
  file.i.t.adj$Under.five.deaths.median <- file.i.f$Under.five.deaths.median + file.i.m$Under.five.deaths.median
  
  # 5q5 deaths: set total = female + male
  file.i.t.adj$Infant.deaths.median <- file.i.f$Infant.deaths.median + file.i.m$Infant.deaths.median
  
  # 5q10 deaths: set total = female + male
  file.i.t.adj$Child.deaths.median <- file.i.f$Child.deaths.median + file.i.m$Child.deaths.median

  # Write adjusted total file for 5-14
  write.csv(file.i.t.adj, file=file.path(dir_median_total_5_14, paste0("/Rates & Deaths(ADJUSTED)_", region_name, ".csv")), row.names = F, na="")
  
  
  # Adjust 15-24 age group ----
  file.i.f <- read.csv(file.path(dir_median_female_15_24, paste0("/Rates & Deaths_", region_name, ".csv")), as.is=T)
  file.i.m <- read.csv(file.path(dir_median_male_15_24, paste0("/Rates & Deaths_", region_name, ".csv")), as.is=T)
  file.i.t <- read.csv(file.path(dir_median_total_15_24, paste0("/Rates & Deaths_", region_name, ".csv")), as.is=T)
  
  ## Adjust total to be sum of female + male
  file.i.t.adj <- file.i.t
  
  # 10q15 deaths: set total = female + male
  file.i.t.adj$Under.five.deaths.median <- file.i.f$Under.five.deaths.median + file.i.m$Under.five.deaths.median
  
  # 5q15 deaths: set total = female + male
  file.i.t.adj$Infant.deaths.median <- file.i.f$Infant.deaths.median + file.i.m$Infant.deaths.median

  # 5q20 deaths: set total = female + male
  file.i.t.adj$Child.deaths.median <- file.i.f$Child.deaths.median + file.i.m$Child.deaths.median
  
  # Write adjusted total file for 15-24
  write.csv(file.i.t.adj, file=file.path(dir_median_total_15_24, paste0("/Rates & Deaths(ADJUSTED)_", region_name, ".csv")), row.names = F, na="")
}
