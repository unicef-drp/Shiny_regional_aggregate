# Update median aggregates file and results.csv

# Delete all the "median_*" folders in `inst/extdata`
extdata_dir <- file.path("inst", "extdata")
median_dirs <- list.dirs(extdata_dir, full.names = TRUE, recursive = FALSE)
median_dirs <- median_dirs[startsWith(basename(median_dirs), "median_")]
if (length(median_dirs) > 0) unlink(median_dirs, recursive = TRUE, force = TRUE)

# Modify this script to copy the correct files from each round's outputs
source("update_me_every_year.R")
release_meta <- release_metadata()
list2env(release_meta, envir = environment())

USERPROFILE <- Sys.getenv("USERPROFILE") # leading dir to Dropbox
source(file.path(USERPROFILE, "Dropbox/UNICEF Work/profile.R"))
dir_CC_code <- file.path(USERPROFILE, "Dropbox/UNICEF Work/Country consultation/Code_for_CC")
source(file.path(dir_CC_code, "R/Dropbox_results_directories_2025.R"))

work_dir_report <- file.path(dir_SP, "IGME report/2025/Code_for_report")
source(file.path(work_dir_report, "Dropbox_aggresults_directories_2025.R"))

source(file.path(USERPROFILE, "Dropbox/UNICEF Work/CME.assistant/R/funcs_read_data.R"))
# main directories

IGME_ROUND <- 2025
work_dir_IGME <- get.workdir(year = IGME_ROUND)
work_dir_5_14 <- file.path(USERPROFILE, paste0("Dropbox/IGME 5-14/", IGME_ROUND ," Round Estimation"))
work_dir_15_24 <- file.path(USERPROFILE, paste0("Dropbox/IGME 15-24/", IGME_ROUND ," Round Estimation"))

invisible(lapply(
  c(
    dir_input,
    dir_output,
    dir_examples,
    dir_www,
    file.path(dir_output, runname.U5MR),
    file.path(dir_output, runname.IMR),
    file.path(dir_output, dirname(file_name_NMR)),
    file.path(dir_output, "Sex_forDeathCalculation"),
    file.path(dir_output, "10q5"),
    file.path(dir_output, "5q5"),
    file.path(dir_output, "10q15"),
    file.path(dir_output, "5q15")
  ),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# copy country.info.CME.csv
file.copy(from = file.path(work_dir_IGME, "input/country.info.CME.csv"), 
          to = file.path(dir_input, "country.info.CME.csv"), overwrite = TRUE)
file.copy(from = file.path(work_dir_5_14, "input/country.info.CME.csv"), 
          to = file.path(dir_input, "country.info.CME.5_14.csv"), overwrite = TRUE)
file.copy(from = file.path(work_dir_15_24, "input/country.info.CME.csv"), 
          to = file.path(dir_input, "country.info.CME.15_24.csv"), overwrite = TRUE)

# stillbirth median-only country input ---------------------------------------
file_stillbirth_country_results <- file.path(dir_stillbirth_aggregate_results, file_name_stillbirth_country_results)
stopifnot(file.exists(file_stillbirth_country_results))
stillbirth_country_medians <- data.table::fread(
  file_stillbirth_country_results,
  select = c("ISO3Code", "CountryName", "Shortind", "Year", "Indicator", "Median")
)
stillbirth_country_medians <- stillbirth_country_medians[Shortind %in% c("SBR", "SB", "LB")]
data.table::fwrite(
  stillbirth_country_medians,
  file.path(dir_input, file_name_stillbirth_country_medians),
  na = ""
)

# population file ------------------------------------------------------------
# only need to copy once 

# copy results.csv -----------------------------------------------------------
file.copy(from = results_dir_list_under_5_final_2025$u5mr.t.in.path, 
          to = file.path(dir_output, runname.U5MR, "Results.csv"), overwrite = TRUE)
file.copy(results_dir_list_under_5_final_2025$imr.t.in.path, 
          file.path(dir_output, runname.IMR, "Results.csv"), overwrite = TRUE)
file.copy(results_dir_list_under_5_final_2025$nmr.t.in.path, 
          file.path(dir_output, file_name_NMR), overwrite = TRUE)

# sex-specific under-five results.csv
dir_Sex_forDeathCalculation <- file.path(work_dir_IGME, "output", "Sex_forDeathCalculation")
list.files(dir_Sex_forDeathCalculation)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_imr_f.csv"),  file.path(dir_output, "Sex_forDeathCalculation", "Results_imr_f.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_imr_m.csv"),  file.path(dir_output, "Sex_forDeathCalculation", "Results_imr_m.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_u5mr_f.csv"), file.path(dir_output, "Sex_forDeathCalculation", "Results_u5mr_f.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation, "Results_u5mr_m.csv"), file.path(dir_output, "Sex_forDeathCalculation", "Results_u5mr_m.csv"), overwrite = TRUE)

# older children
file.copy(from = results_dir_list_5_24_final_2025$mr5t14.t.in.path, 
          to = file.path(dir_output, "10q5", "Results.csv"), overwrite = TRUE)
file.copy(from = results_dir_list_5_24_final_2025$mr5t9.t.in.path, 
          to = file.path(dir_output, "5q5", "Results.csv"), overwrite = TRUE)
file.copy(from = results_dir_list_5_24_final_2025$mr15t24.t.in.path, 
          to = file.path(dir_output, "10q15", "Results.csv"), overwrite = TRUE)
file.copy(from = results_dir_list_5_24_final_2025$mr15t19.t.in.path, 
          to = file.path(dir_output, "5q15", "Results.csv"), overwrite = TRUE)

# sex-specific older children results.csv
dir_Sex_forDeathCalculation_5_14 <- file.path(work_dir_5_14, "output", "Sex_forDeathCalculation")
dir_Sex_forDeathCalculation_15_24 <- file.path(work_dir_15_24, "output", "Sex_forDeathCalculation")

# 5-14 age group
file.copy(file.path(dir_Sex_forDeathCalculation_5_14, "Results_10q5_f.csv"), file.path(dir_output, "Sex_forDeathCalculation", "Results_10q5_f.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation_5_14, "Results_10q5_m.csv"), file.path(dir_output, "Sex_forDeathCalculation", "Results_10q5_m.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation_5_14, "Results_5q5_f.csv"),  file.path(dir_output, "Sex_forDeathCalculation", "Results_5q5_f.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation_5_14, "Results_5q5_m.csv"),  file.path(dir_output, "Sex_forDeathCalculation", "Results_5q5_m.csv"), overwrite = TRUE)

# 15-24 age group
file.copy(file.path(dir_Sex_forDeathCalculation_15_24, "Results_10q15_f.csv"), file.path(dir_output, "Sex_forDeathCalculation", "Results_10q15_f.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation_15_24, "Results_10q15_m.csv"), file.path(dir_output, "Sex_forDeathCalculation", "Results_10q15_m.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation_15_24, "Results_5q15_f.csv"),  file.path(dir_output, "Sex_forDeathCalculation", "Results_5q15_f.csv"), overwrite = TRUE)
file.copy(file.path(dir_Sex_forDeathCalculation_15_24, "Results_5q15_m.csv"),  file.path(dir_output, "Sex_forDeathCalculation", "Results_5q15_m.csv"), overwrite = TRUE)


# copy country summary  -------------------------------------------------------
# This is to get country median estimates (used for downloaded file)
# Directory variables are sourced from update_me_every_year.R (canonical source)
invisible(lapply(list(dir_median_total, 
                      dir_median_female, dir_median_male,
                      dir_median_total_5_14, dir_median_female_5_14, dir_median_male_5_14,
                      dir_median_total_15_24, dir_median_female_15_24, dir_median_male_15_24), dir.create,
                 recursive = TRUE, showWarnings = FALSE))


# check if all TRUE:
stopifnot(file.exists(file.path(dir_aggu5_median, file_name_total)))
stopifnot(file.exists(file.path(dir_aggu5_f_median, file_name_female)))
stopifnot(file.exists(file.path(dir_aggu5_m_median, file_name_male)))
stopifnot(file.exists(file.path(dir_agg10q5_median, file_name_total_5_24)))
stopifnot(file.exists(file.path(dir_agg10q5_f_median, file_name_female_5_24)))
stopifnot(file.exists(file.path(dir_agg10q5_m_median, file_name_male_5_24)))
stopifnot(file.exists(file.path(dir_agg10q15_median, file_name_total_5_24)))
stopifnot(file.exists(file.path(dir_agg10q15_f_median, file_name_female_5_24)))
stopifnot(file.exists(file.path(dir_agg10q15_m_median, file_name_male_5_24)))

# Country Summary will actually be reproduced when running the script 2 .Create
# M49 regions and initiate app.R

file.copy(file.path(dir_aggu5_median, file_name_total),    to = file.path(dir_median_total, file_name_total), overwrite = TRUE)
file.copy(file.path(dir_aggu5_f_median, file_name_female), to = file.path(dir_median_female, file_name_female), overwrite = TRUE)
file.copy(file.path(dir_aggu5_m_median, file_name_male),   to = file.path(dir_median_male, file_name_male), overwrite = TRUE)
file.copy(file.path(dir_agg10q5_median,    file_name_total_5_24),    to = file.path(dir_median_total_5_14,  file_name_total_5_24), overwrite = TRUE)
file.copy(file.path(dir_agg10q5_f_median,  file_name_female_5_24),   to = file.path(dir_median_female_5_14, file_name_female_5_24), overwrite = TRUE)
file.copy(file.path(dir_agg10q5_m_median,  file_name_male_5_24),     to = file.path(dir_median_male_5_14,   file_name_male_5_24), overwrite = TRUE)
file.copy(file.path(dir_agg10q15_median,   file_name_total_5_24),    to = file.path(dir_median_total_15_24, file_name_total_5_24), overwrite = TRUE)
file.copy(file.path(dir_agg10q15_f_median, file_name_female_5_24),   to = file.path(dir_median_female_15_24,file_name_female_5_24), overwrite = TRUE)
file.copy(file.path(dir_agg10q15_m_median, file_name_male_5_24),     to = file.path(dir_median_male_15_24,  file_name_male_5_24), overwrite = TRUE)

# Create a testing example ---------------------------------------------
library("data.table")
# Run WB Low Income countries and compared to IGME results 
dc <- fread(file.path(work_dir_IGME, "input/country.info.CME.csv"))
dcWBLIC <- dc[WBRegion4 == "Low income", .(ISO3Code, OfficialName)]
dcWBLIC[, Region := "Low income countries"]
fwrite(dcWBLIC, file.path(dir_examples, "Upload_ISO_example_WBLIC.csv"))
fwrite(dcWBLIC, file.path(dir_www, "Upload_ISO3Code_example_single_region.csv"))

dcWBHIC <- dc[WBRegion4 == "High income", .(ISO3Code, OfficialName)]
dcWBHIC[, Region := "High income countries"]
fwrite(dcWBHIC, file.path(dir_examples, "Upload_ISO_example_WBHIC.csv"))


dcWB1 <- dc[WBRegion4 != "", .(WBRegion4, ISO3Code, OfficialName)]
dcWB2 <- dc[WBRegion5 != "", .(WBRegion5, ISO3Code, OfficialName)]
setnames(dcWB1, "WBRegion4", "Region")
setnames(dcWB2, "WBRegion5", "Region")
dcWB <- rbind(dcWB1, dcWB2)
fwrite(dcWB, file.path(dir_examples, "Upload_ISO_example_WB.csv"))


# after finishing running this script, run "update/2.Create M49 regions and
# initiate app.R"

