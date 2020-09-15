# This is the only script that needs to be updated
# Please also update all the files in the folders mentioned below

# 0. The 7 files in the input folder except "country.info.CME_adhoc.csv", it doesn't matter.

# 1. Aggregate results folder
library("here")
dir_median_total <- here::here("median_results_total")
dir_median_female <- here::here("median_results_female")
dir_median_male <- here::here("median_results_male")

# The name of the final results file:
file_name_total <- "Rates & Deaths_Country Summary.csv"
file_name_female <- "Rates & Deaths(ADJUSTED)_female_Country Summary (negative replaced).csv"
file_name_male <- "Rates & Deaths(ADJUSTED)_male_Country Summary.csv"

# 2. Output folder
# the folder names in output:
runname.U5MR <- "GR20190311_all"
runname.IMR <- "IMR20190314_all"
runname.NMR <- "NMR_forDeathCalculation"

# 3. Extra
# Show data from: 
year_started <- 1990 
year.lastestimatepublished <- 2019.5  # e.g. 2019.5 for IGME 2020

# year_end is set to the maximum available year in results.csv, no need to set manually

# The release date in the `About` panel
update_string0 <- "Last updated: September, 2019"