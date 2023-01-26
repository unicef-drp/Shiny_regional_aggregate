# Update median aggregates file and results.csv
# Modify this script to copy the correct files from each round's outputs

USERPROFILE <- "C:/Users/lyhel" # leading dir to Dropbox
dir_CC_code <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Country consultation/Code_for_CC")
source(file.path(dir_CC_code, "R/Dropbox_results_directories_2022.R"))

# main directories
dir_IGME_u5 <- file.path(USERPROFILE, "Dropbox/UN IGME Data/2022 Round Estimation/Code")
dir_IGME_5_14 <- file.path(USERPROFILE, "Dropbox/IGME 5-14/2022 Round Estimation")
dir_IGME_15_24 <- file.path(USERPROFILE, "Dropbox/IGME 15-24/2022 Round Estimation")
  
# copy country.info.CME.csv
file.copy(from = file.path(dir_IGME_u5, "input/country.info.CME.csv"), 
          to = here::here("input", "country.info.CME.csv"), overwrite = TRUE)
file.copy(from = file.path(dir_IGME_5_14, "input/country.info.CME.csv"), 
          to = here::here("input", "country.info.CME.5_14.csv"), overwrite = TRUE)
file.copy(from = file.path(dir_IGME_15_24, "input/country.info.CME.csv"), 
          to = here::here("input", "country.info.CME.15_24.csv"), overwrite = TRUE)


# copy results.csv -----------------------------------------------------------
file.copy(from = results_dir_list_under_5_final_2022$u5mr.t.in.path, 
          to = here::here("output", runname.U5MR, "Results.csv"), overwrite = TRUE)
file.copy(results_dir_list_under_5_final_2022$imr.t.in.path, 
          here::here("output", runname.IMR, "Results.csv"), overwrite = TRUE)
file.copy(results_dir_list_under_5_final_2022$nmr.t.in.path, 
          here::here("output", file_name_NMR), overwrite = TRUE)

# sex-specific under-file results.csv
dir_Sex_forDeathCalculation <- file.path(dir_IGME_u5, "output", "Sex_forDeathCalculation")
list.files(dir_Sex_forDeathCalculation)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_imr_f.csv"),  here::here("output/Sex_forDeathCalculation/Results_imr_f.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_imr_m.csv"),  here::here("output/Sex_forDeathCalculation/Results_imr_m.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_u5mr_f.csv"), here::here("output/Sex_forDeathCalculation/Results_u5mr_f.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_u5mr_m.csv"), here::here("output/Sex_forDeathCalculation/Results_u5mr_m.csv"), overwrite = TRUE)

# older children
file.copy(from = results_dir_list_5_24_final_2022$mr5t14.t.in.path, 
          to = here::here("output", "10q5", "Results.csv"), overwrite = TRUE)
file.copy(from = results_dir_list_5_24_final_2022$mr5t9.t.in.path, 
          to = here::here("output", "5q5", "Results.csv"), overwrite = TRUE)
file.copy(from = results_dir_list_5_24_final_2022$mr15t24.t.in.path, 
          to = here::here("output", "10q15", "Results.csv"), overwrite = TRUE)
file.copy(from = results_dir_list_5_24_final_2022$mr15t19.t.in.path, 
          to = here::here("output", "5q15", "Results.csv"), overwrite = TRUE)

# copy country summary  -------------------------------------------------------
# This is to get country median estimates (used for downloaded file)
dir_median_total <- here::here("median_results_total")
dir_median_female <- here::here("median_results_female")
dir_median_male <- here::here("median_results_male")
dir_median_total_5_14 <- here::here("median_results_total_5_14")
dir_median_total_15_24 <- here::here("median_results_total_15_24")
invisible(lapply(list(dir_median_total, 
                      dir_median_female, dir_median_male,
                      dir_median_total_5_14, 
                      dir_median_total_15_24), dir.create))

# The name of the final results file:
file_name_total <- "Rates & Deaths_Country Summary.csv"
file_name_female <- "Rates & Deaths(ADJUSTED)_female_Country Summary.csv"
file_name_male <- "Rates & Deaths(ADJUSTED)_male_Country Summary.csv"

# under-five
dir_aggu5.median <- file.path(dir_IGME_u5, "Aggregate results (median) 2022-11-06")
# sex-specific
dir_aggu5.median_f <- file.path(dir_IGME_u5, "Aggregate results (median) 2022-11-06 (female)")
dir_aggu5.median_m <- file.path(dir_IGME_u5, "Aggregate results (median) 2022-11-06 (male)")
# older children
dir_5_14.median <- file.path(dir_IGME_5_14, "Aggregate results (median) 2022-11-17")
dir_15_24.median <- file.path(dir_IGME_15_24, "Aggregate results (median) 2022-11-17")

# check if all TRUE:
file.exists(file.path(dir_aggu5.median, file_name_total))
file.exists(file.path(dir_aggu5.median_f, file_name_female))
file.exists(file.path(dir_aggu5.median_m, file_name_male))
file.exists(file.path(dir_5_14.median, file_name_total))
file.exists(file.path(dir_15_24.median, file_name_total))

file.copy(file.path(dir_aggu5.median, file_name_total),    to = file.path(dir_median_total, file_name_total), overwrite = TRUE)
file.copy(file.path(dir_aggu5.median_f, file_name_female), to = file.path(dir_median_female, file_name_female), overwrite = TRUE)
file.copy(file.path(dir_aggu5.median_m, file_name_male),   to = file.path(dir_median_male, file_name_male), overwrite = TRUE)
file.copy(file.path(dir_5_14.median, file_name_total),     to = file.path(dir_median_total_5_14, file_name_total), overwrite = TRUE)
file.copy(file.path(dir_15_24.median, file_name_total),    to = file.path(dir_median_total_15_24, file_name_total), overwrite = TRUE)

# Create a testing example ---------------------------------------------
library("data.table")
# Run WB Low Income countries and compared to IGME results 
dc <- fread(file.path(dir_IGME_u5, "input/country.info.CME.csv"))
dcWBLIC <- dc[WBRegion4 == "Low income", .(ISO3Code, OfficialName)]
dcWBLIC[, Region := "Low income countries"]
fwrite(dcWBLIC, here::here("Upload_ISO_example_WBLIC.csv"))
fwrite(dcWBLIC, here::here("www/Upload_ISO3Code_example_single_region.csv"))

dcWBHIC <- dc[WBRegion4 == "High income", .(ISO3Code, OfficialName)]
dcWBHIC[, Region := "High income countries"]
fwrite(dcWBHIC, here::here("Upload_ISO_example_WBHIC.csv"))

dcSP <- dc[SPhumanitarian == "Humanitarian", .(ISO3Code, OfficialName)]
dcSP[, Region := "SP humanitarian countries"]
fwrite(dcSP, here::here("Upload_ISO_example_SPhumanitarian.csv"))

# after finishing running this script, run "update/2.Create M49 regions and
# initiate app.R"

