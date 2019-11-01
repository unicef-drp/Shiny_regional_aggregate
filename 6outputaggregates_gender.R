

#----------------------------------------------------------------------
# 7. Sex-specifict death/aggregate estimates
#----------------------------------------------------------------------
# #----------------------------------------------------------------------
# # Note: Requires results for male/female U5MR/IMR & male/female population at age 0/under age
# # Note: if runname directory is Sex_forDeathCalculation, i.e. not GRYYYMMDD_all, need to copy iso.c.rda and year.t.rda from GrYYYMMDD_all folder and paste in Sex_forDeathCalculation or whatever folder contains the sex-specific results
# # Note: Need to run "Dropbox\UN IGME data\2019 Round Estimation\Code\output\Sex_forDeathCalculation\processGendertrajectoriesfordeathcalc.R" and "Dropbox\UN IGME data\2019 Round Estimation\Code\output\Sex_forDeathCalculation\processGenderresultsfilefordeathcalc.R" to get sex-specific input files; trajectories needs to be run on server

# Male --------------------------------------------------------------------

# get country deaths and M49 replacement
file.dir.median.male <- file.path(paste("Aggregate results (median)", date, "(male)")) ##<< File directory to save median estimates to
if(!file.exists(here::here(file.dir.median.male, "Rates & Deaths_M49Region.csv")))
{
  OutputAggregates(results.U5MR.file = "output/Sex_forDeathCalculation/Results_u5mr_m.csv",
                   results.IMR.file = "output/Sex_forDeathCalculation/Results_imr_m.csv",
                   results.NMR.file = NULL,
                   population.file = "input/data_male_CMEpopulation_20150817.csv",
                   run.on.server = run.on.server,
                   year4 = year.lastestimatepublished,
                   output.dir = file.dir.median.male,
                   livebirths.file = "input/data_livebirths_male.csv",
                   year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                   regiontypes.select = c("M49"),
                   test=FALSE,
                   get.world.results = FALSE,
                   round.output = FALSE,
                   replace.rates.reg=NULL,
                   replace.rates.cat=NULL)
}
# get world and regions with M49 replacement
OutputAggregates(results.U5MR.file = "output/Sex_forDeathCalculation/Results_u5mr_m.csv",
                 results.IMR.file = "output/Sex_forDeathCalculation/Results_imr_m.csv",
                 results.NMR.file = NULL,
                 country.info.file = file.path("input", "country.info.CME_adhoc.csv"),
                 population.file = "input/data_male_CMEpopulation_20150817.csv",
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.median.male,
                 livebirths.file = "input/data_livebirths_male.csv",
                 year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("Adhoc"),
                 test=FALSE,
                 get.world.results = TRUE,
                 round.output = FALSE,
                 replace.rates.reg="M49Region",
                 replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", 
                                           country.info$M49Region2[country.info$M49Region1=="Americas"]))



# Female ------------------------------------------------------------------

# # Female (median)
file.dir.median.female <- file.path(paste("Aggregate results (median)", date, "(female)")) ##<< File directory to save median estimates to
if(!file.exists(here::here(file.dir.median.female, "Rates & Deaths_M49Region.csv")))
{
  # get country deaths and M49 replacement
  OutputAggregates(results.U5MR.file = "output/Sex_forDeathCalculation/Results_u5mr_f.csv",
                   results.IMR.file = "output/Sex_forDeathCalculation/Results_imr_f.csv",
                   results.NMR.file = NULL,
                   population.file = "input/data_female_CMEpopulation_20150817.csv",
                   run.on.server = run.on.server,
                   year4 = year.lastestimatepublished,
                   output.dir = file.dir.median.female,
                   livebirths.file = "input/data_livebirths_female.csv",
                   year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                   regiontypes.select = c("M49"),
                   test=FALSE,
                   get.world.results = FALSE,
                   round.output = FALSE,
                   replace.rates.reg=NULL,
                   replace.rates.cat=NULL)
}
# get world and regions with M49 replacement
OutputAggregates(results.U5MR.file = "output/Sex_forDeathCalculation/Results_u5mr_f.csv",
                 results.IMR.file = "output/Sex_forDeathCalculation/Results_imr_f.csv",
                 results.NMR.file = NULL,
                 country.info.file = file.path("input", "country.info.CME_adhoc.csv"),
                 population.file = "input/data_female_CMEpopulation_20150817.csv",
                 run.on.server = run.on.server,
                 year4 = year.lastestimatepublished,
                 output.dir = file.dir.median.female,
                 livebirths.file = "input/data_livebirths_female.csv",
                 year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("Adhoc"),
                 test=FALSE,
                 get.world.results = TRUE,
                 round.output = FALSE,
                 replace.rates.reg="M49Region",
                 replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", country.info$M49Region2[country.info$M49Region1=="Americas"]))
