# This is the only script that needs to be updated

# 0. Update the 7 files in the input folder except "country.info.CME_adhoc.csv", which you can just leave there

# 1. Aggregate results folder
# Note
# * All the `csv` files will be generated, but if we wish to show the adjusted values, we can copy the adjusted final files. Only need to copy files in the folder `samples_combined`
# * But the folder structure of the folder `samples` shall be kept, delete all the files inside all sub folders
# * After updating all the files, load global environment in `app.R`, run all the `OutputAggregates` functions (in side the wrapping function like `run.outputaggregates.gender`) in `R/6outputaggregates.R` and `R/6outputaggregates_gender.R` to create all the intermediate files
library("here")
dir_median_total <- here::here("median_results_total")
dir_median_female <- here::here("median_results_female")
dir_median_male <- here::here("median_results_male")

# The name of the final results file:
file_name_total <- "Rates & Deaths_Country Summary.csv"
file_name_female <- "Rates & Deaths(ADJUSTED)_female_Country Summary.csv"
file_name_male <- "Rates & Deaths(ADJUSTED)_male_Country Summary.csv"

# 2. Output folder
# the folder names and file names in output for results.csv
# only need to copy the results.csv files
runname.U5MR <- "GR20200214_all"
runname.IMR <- "IMR20200219_all"
file_name_NMR <- "NMR_forDeathCalculation/Results_NMR_2020-08-20.csv"

# 3. Extra
# Show data from: 
year_started <- 1990 # maybe no need to change, this is starting year to show
# year_end is set to the maximum available year in results.csv, which is the latest year. There is no need to set manually

# The release date in the `About` panel
update_string0 <- "Last updated: September, 2020"