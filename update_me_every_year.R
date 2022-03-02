# This is the script that walks through the updating process of the app
library("here")

# 1. Update the "country.info.CME.csv" in the input folder. No need to update
# "country.info.CME_adhoc.csv". Copy-paste from Dropbox\UN IGME data\202x Round
# Estimation\Code\input. Not all the rest files change from year to year, some
# change like "data_livebirths.csv"

# 2. Delete all these three folders
dir_median_total  <- here::here("median_results_total")
dir_median_female <- here::here("median_results_female")
dir_median_male   <- here::here("median_results_male")
dir_median_total_5_14 <- here::here("median_results_total_5_14")
dir_median_total_15_24 <- here::here("median_results_total_15_24")


# 3. Modify and run "update/1.Update median agg and results.csv.R" to copy
# results.csv files, and country summary.csv
# folder names in "output/", no need to change
runname.U5MR   <- "U5MR"
runname.IMR    <- "IMR"
file_name_NMR  <- "NMR/Results.csv"

# The name of the final results file (usually remain unchanged):
file_name_total  <- "Rates & Deaths_Country Summary.csv"
file_name_female <- "Rates & Deaths(ADJUSTED)_female_Country Summary.csv"
file_name_male   <- "Rates & Deaths(ADJUSTED)_male_Country Summary.csv"

# 4. Build M49 region and initiate folder structure, all the files in these
# "median_results" median folders will be created by the script: 
# "update/2.Create M49 regions and initiate app.R"

# 5. Extra
# Show data from (usually remain unchanged): 
year_started <- 1990 # no need to change, this is starting year to show
# year_end is set to be the maximum available year in results.csv, which is the latest year. There is no need to set manually

# The release date in the `About` panel, please revise the date every year: 
update_string0 <- "Last updated: December, 2021"