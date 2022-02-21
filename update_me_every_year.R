# This is the script that walks through the updating process of the app


# 0. Update the files in the input folder except "country.info.CME_adhoc.csv", which you can just leave there
# Copy-paste from Dropbox\UN IGME data\202x Round Estimation\Code\input
# The major files requiring updating is "country.info.CME.csv"

# 1. Reconstruct aggregate results folder (no need to copy-paste any file)
library("here")
# 1.1. Delete all these three folders
dir_median_total  <- here::here("median_results_total")
dir_median_female <- here::here("median_results_female")
dir_median_male   <- here::here("median_results_male")

# 1.2. Modify and run "update/1.Update median agg and results.csv.R"

# The name of the final results file (can remain unchanged):
file_name_total  <- "Rates & Deaths_Country Summary.csv"
file_name_female <- "Rates & Deaths(ADJUSTED)_female_Country Summary.csv"
file_name_male   <- "Rates & Deaths(ADJUSTED)_male_Country Summary.csv"

# 1.3. Build M49 region and initiate folder structure, all the files in these
# median folders will be created by script 
# run "update/2.Create M49 regions and initiate app.R"

# 2. "Results.csv" in the "output" folder 
# results.csv files are already updated earlier in the script "update/1.Update median agg and results.csv.R" folder
# folder names (can remain unchanged):
runname.U5MR   <- "U5MR"
runname.IMR    <- "IMR"
file_name_NMR  <- "NMR/Results.csv"

# 3. Extra
# Show data from (usually remain unchanged): 
year_started <- 1990 # no need to change, this is starting year to show
# year_end is set to be the maximum available year in results.csv, which is the latest year. There is no need to set manually

# The release date in the `About` panel, please revise every year: 
update_string0 <- "Last updated: December, 2021"