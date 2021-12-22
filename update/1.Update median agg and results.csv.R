# Update median aggregates file and results.csv
# Modify this script to copy the correct files 

leading_path <- "C:\\Users\\lyhel" # leading dir to Dropbox
dir_u5    <- file.path(leading_path, "Dropbox/UN IGME Data/2021 Round Estimation/Code")

dir_CC_code <- file.path(leading_path, "Dropbox/UNICEF Work/Country consultation/Code_for_CC")
source(file.path(dir_CC_code, "R/Dropbox_results_directories.R"))
file.copy(results_dir_list_under_5_final$u5mr.t.in.path, here::here("output", runname.U5MR, "Results.csv"), overwrite = TRUE)
file.copy(results_dir_list_under_5_final$imr.t.in.path, here::here("output", runname.IMR, "Results.csv"), overwrite = TRUE)
file.copy(results_dir_list_under_5_final$nmr.t.in.path, here::here("output", file_name_NMR), overwrite = TRUE)

dir_Sex_forDeathCalculation <- file.path(dir_u5, "output", "Sex_forDeathCalculation")
results.fileslist.files(dir_Sex_forDeathCalculation)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_imr_f.csv"), here::here("output/Sex_forDeathCalculation/Results_imr_f.csv"))
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_imr_m.csv"), here::here("output/Sex_forDeathCalculation/Results_imr_m.csv"))
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_u5mr_f.csv"), here::here("output/Sex_forDeathCalculation/Results_u5mr_f.csv"))
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_u5mr_m.csv"), here::here("output/Sex_forDeathCalculation/Results_u5mr_m.csv"))

# Update final aggregates
dir_median_total <- here::here("median_results_total")
dir_median_female <- here::here("median_results_female")
dir_median_male <- here::here("median_results_male")
invisible(lapply(list(dir_median_total, dir_median_female, dir_median_male), dir.create))

# The name of the final results file:
file_name_total <- "Rates & Deaths_Country Summary.csv"
file_name_female <- "Rates & Deaths(ADJUSTED)_female_Country Summary.csv"
file_name_male <- "Rates & Deaths(ADJUSTED)_male_Country Summary.csv"

# directories
# under-five
dir_aggu5.median <- file.path(dir_u5, "Aggregate results (median) 2021-11-08")
# sex-specific
dir_aggu5.median_f <- file.path(dir_u5, "Aggregate results (median) 2021-11-08 (female)")
dir_aggu5.median_m <- file.path(dir_u5, "Aggregate results (median) 2021-11-08 (male)")
file.exists(file.path(dir_aggu5.median, file_name_total))
file.exists(file.path(dir_aggu5.median_f, file_name_female))
file.exists(file.path(dir_aggu5.median_m, file_name_male))

file.copy(file.path(dir_aggu5.median, file_name_total),    to = file.path(dir_median_total, file_name_total))
file.copy(file.path(dir_aggu5.median_f, file_name_female), to = file.path(dir_median_female, file_name_female))
file.copy(file.path(dir_aggu5.median_m, file_name_male),   to = file.path(dir_median_male, file_name_male))


# Creating a testing example ---------------------------------------------

# Run WB Low Income countries and compared to IGME results 
dc <- fread(file.path(dir_u5, "input/country.info.CME.csv"))
fwrite(dc[WBRegion4 == "Low income", .(ISO3Code, OfficialName)], here::here("Upload_ISO_example_WBLIC.csv"))
