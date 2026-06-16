# Sex-specific death/aggregate estimates


# Male --------------------------------------------------------------------

run.outputaggregates.gender <- function(year.lastestimatepublished, reuse.replacement.country = TRUE){
meta <- release_metadata()
if(!file.exists(file.path(dir_median_male, "Rates & Deaths_M49Region.csv")))
{
  OutputAggregates(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_u5mr_m.csv"),
                   results.IMR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_imr_m.csv"),
                   results.NMR.file = NULL,
                   country.info.file = file.path(dir_input, "country.info.CME.csv"),
                   population.file = file.path(dir_input, meta$population_file_male),
                   run.on.server = FALSE,
                   year4 = year.lastestimatepublished,
                   output.dir = dir_median_male,
                   livebirths.file = file.path(dir_input, "data_livebirths_male.csv"),
                   data.a0.file = file.path(dir_input, "a0.csv"),
                   year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                   regiontypes.select = c("M49"),
                   test=FALSE,
                   get.world.results = FALSE,
                   round.output = FALSE,
                   output.rates.of.decline = FALSE,
                   replace.rates.reg=NULL,
                   replace.rates.cat=NULL)
}
# get world and regions with M49 replacement
OutputAggregates(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_u5mr_m.csv"),
                 results.IMR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_imr_m.csv"),
                 results.NMR.file = NULL,
                 country.info.file = file.path(dir_input, "country.info.CME_adhoc.csv"),
                 population.file = file.path(dir_input, meta$population_file_male),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_male,
                 livebirths.file = file.path(dir_input, "data_livebirths_male.csv"),
                 data.a0.file = file.path(dir_input, "a0.csv"),
                 year.target = year.lastestimatepublished, 
                 est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("Adhoc"),
                 test=FALSE,
                 get.world.results = TRUE,
                 round.output = FALSE,
                 output.rates.of.decline = FALSE,
                 replace.rates.reg="M49Region",
                 reuse.replacement.country = reuse.replacement.country,
                 replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", 
                                           country.info$M49Region2[country.info$M49Region1=="Americas"]))



# Female ------------------------------------------------------------------

 
if(!file.exists(file.path(dir_median_female, "Rates & Deaths_M49Region.csv")))
{
  # get country deaths and M49 replacement
  OutputAggregates(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_u5mr_f.csv"),
                   results.IMR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_imr_f.csv"),
                   results.NMR.file = NULL,
                   country.info.file = file.path(dir_input, "country.info.CME.csv"),
                   population.file = file.path(dir_input, meta$population_file_female),
                   run.on.server = FALSE,
                   year4 = year.lastestimatepublished,
                   output.dir = dir_median_female,
                   livebirths.file = file.path(dir_input, "data_livebirths_female.csv"),
                   data.a0.file = file.path(dir_input, "a0.csv"),
                   year.target = year.lastestimatepublished, 
                   est.years = seq(1950.5,year.lastestimatepublished,1),
                   regiontypes.select = c("M49"),
                   test=FALSE,
                   get.world.results = FALSE,
                   round.output = FALSE,
                   output.rates.of.decline = FALSE,
                   replace.rates.reg=NULL,
                   replace.rates.cat=NULL)
}
# get world and regions with M49 replacement
OutputAggregates(results.U5MR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_u5mr_f.csv"),
                 results.IMR.file = file.path(dir_output, "Sex_forDeathCalculation", "Results_imr_f.csv"),
                 results.NMR.file = NULL,
                 country.info.file = file.path(dir_input, "country.info.CME_adhoc.csv"),
                 population.file = file.path(dir_input, meta$population_file_female),
                 run.on.server = FALSE,
                 year4 = year.lastestimatepublished,
                 output.dir = dir_median_female,
                 livebirths.file = file.path(dir_input, "data_livebirths_female.csv"),
                 data.a0.file = file.path(dir_input, "a0.csv"),
                 year.target = year.lastestimatepublished, est.years = seq(1950.5,year.lastestimatepublished,1),
                 regiontypes.select = c("Adhoc"),
                 test=FALSE,
                 get.world.results = TRUE,
                 round.output = FALSE,
                 output.rates.of.decline = FALSE,
                 replace.rates.reg="M49Region",
                 reuse.replacement.country = reuse.replacement.country,
                 
                 replace.rates.cat=replace(country.info$M49Region1, country.info$M49Region1=="Americas", country.info$M49Region2[country.info$M49Region1=="Americas"]))
}



# Adjust death for regional aggregates ----
adjust.older.children.total.death <- function(){
  # Read files
  file.i.f <- read.csv(file.path(dir_median_female, "/Rates & Deaths_AdhocCountries.csv"), as.is=T)
  file.i.m <- read.csv(file.path(dir_median_male, "/Rates & Deaths_AdhocCountries.csv"), as.is=T)
  file.i.t <- read.csv(file.path(dir_median_total, "/Rates & Deaths_AdhocCountries.csv"), as.is=T)
  
  ## Adjust total to be sum of female + male
  file.i.t.adj <- file.i.t
  
  # U5 deaths: set total = female + male
  file.i.t.adj$Under.five.deaths.median <- file.i.f$Under.five.deaths.median + file.i.m$Under.five.deaths.median
  
  # Infant deaths: set total = female + male
  file.i.t.adj$Infant.deaths.median <- file.i.f$Infant.deaths.median + file.i.m$Infant.deaths.median
  
  # Child deaths
  file.i.t.adj$Child.deaths.median <- file.i.f$Child.deaths.median + file.i.m$Child.deaths.median
  
  # Write adjusted total file
  write.csv(file.i.t.adj, file=file.path(dir_median_total,"/Rates & Deaths(ADJUSTED)_total_AdhocCountries.csv"), row.names = F, na="")
}


adjust.u5.sex.specific.death <- function(){
  # print(file)
  file.i.f <- read.csv(file.path(dir_median_female, "/Rates & Deaths_AdhocCountries.csv"), as.is=T)
  file.i.m <- read.csv(file.path(dir_median_male, "/Rates & Deaths_AdhocCountries.csv"), as.is=T)
  file.i.t <- read.csv(file.path(dir_median_total, "/Rates & Deaths_AdhocCountries.csv"), as.is=T)

    ## U5 deaths adjustment
  u5.deaths.mf.i <- file.i.m$Under.five.deaths.median+file.i.f$Under.five.deaths.median
  diff.deaths.u5.i <- file.i.t$Under.five.deaths.median-u5.deaths.mf.i
  ## sex-specific deaths don't go all the way back to 1990 for some countries 
  ## get % male
  percent.male.deaths.u5.i <- file.i.m$Under.five.deaths.median/u5.deaths.mf.i
  percent.male.deaths.u5.i[percent.male.deaths.u5.i=="NaN"] <- 0
  ## calculate deaths to add male and female
  to.add.male.u5.i <- round(diff.deaths.u5.i*percent.male.deaths.u5.i)
  to.add.female.u5.i <- diff.deaths.u5.i-to.add.male.u5.i
  ## add to existing file called .adj
  file.i.f.adj <- file.i.f
  file.i.f.adj$Under.five.deaths.median <- file.i.f$Under.five.deaths.median+to.add.female.u5.i
  file.i.m.adj <- file.i.m
  file.i.m.adj$Under.five.deaths.median <- file.i.m$Under.five.deaths.median+to.add.male.u5.i
  
  mf.sum.u5.cols.adj.i <- file.i.f.adj$Under.five.deaths.median+file.i.m.adj$Under.five.deaths.median
  diff.deaths.u5.adj.i <- file.i.t$Under.five.deaths.median-mf.sum.u5.cols.adj.i
  # print("U5 deaths")
  # print(table(diff.deaths.u5.adj.i==0, useNA = "ifany")) # true and NA are ok here -- False would mean difference is still not 0
  if(!all(diff.deaths.u5.adj.i==0, na.rm = TRUE)) message("Not all difference is 0, check `diff.deaths.u5.adj.i==0`")
  
  ## infant deaths adjustment
  u1.deaths.mf.i <- file.i.m$Infant.deaths.median+file.i.f$Infant.deaths.median
  diff.deaths.u1.i <- file.i.t$Infant.deaths.median-u1.deaths.mf.i
  ## sex-specific deaths don't go all the way back to 1990 for some countries 
  ## get % male
  percent.male.deaths.u1.i <- file.i.m$Infant.deaths.median/u1.deaths.mf.i
  percent.male.deaths.u1.i[percent.male.deaths.u1.i=="NaN"] <- 0
  ## calculate deaths to add male and female
  to.add.male.u1.i <- round(diff.deaths.u1.i*percent.male.deaths.u1.i)
  to.add.female.u1.i <- diff.deaths.u1.i-to.add.male.u1.i
  ## add to existing file called .adj
  file.i.f.adj$Infant.deaths.median <- file.i.f$Infant.deaths.median+to.add.female.u1.i
  file.i.m.adj$Infant.deaths.median <- file.i.m$Infant.deaths.median+to.add.male.u1.i
  
  mf.sum.u1.cols.adj.i <- file.i.f.adj$Infant.deaths.median+file.i.m.adj$Infant.deaths.median
  diff.deaths.u1.adj.i <- file.i.t$Infant.deaths.median-mf.sum.u1.cols.adj.i
  # print("u1 deaths")
  # print(table(diff.deaths.u1.adj.i==0, useNA = "ifany"))
  if(!all(diff.deaths.u1.adj.i==0, na.rm = TRUE))message("Not all difference is 0, check `diff.deaths.u1.adj.i==0`")
  
  write.csv(file.i.f.adj, file=file.path(dir_median_female,"/Rates & Deaths(ADJUSTED)_female_AdhocCountries.csv"), row.names = F, na="")
  write.csv(file.i.m.adj, file=file.path(dir_median_male,"/Rates & Deaths(ADJUSTED)_male_AdhocCountries.csv"), row.names = F, na="")
}
