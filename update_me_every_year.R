# This is the script that walks through the updating process of the app


# Annual metadata -------------------------------------------------------------
# The only place to update release date, WPP year, WPP-derived file names, and
# stillbirth input location is R/fct_release_metadata.R.
# `fct` is short for "function"; this file contains release metadata functions.
if (!exists("release_metadata", mode = "function")) {
  if (
    "shinyregionalaggregate" %in% loadedNamespaces() &&
      exists("release_metadata", envir = asNamespace("shinyregionalaggregate"), inherits = FALSE)
  ) {
    release_metadata <- get("release_metadata", envir = asNamespace("shinyregionalaggregate"))
  } else {
    source(file.path("R", "fct_release_metadata.R"))
  }
}
release_meta <- release_metadata()
list2env(release_meta, envir = environment())
update_string0 <- release_meta$update_string

# now, first source this script and then walk through following steps:
# 1. If needed (like in the 2022 round, we add the number of countries, or WPP
# got updated), update a0.csv, livebirths files and male/female population from
# the IGME round input folder. Copy-paste from Dropbox\UN IGME data\202x Round
# Estimation\Code\input. Not all these files change from year to year.
# 

# The "country.info.CME.csv" files are updated by code later. No need to update
# "country.info.CME_adhoc.csv" --- you can delete all the "_adhoc.csv"

# Package data locations are loaded from release_metadata().

# 2. Delete all these folders in `inst/extdata`
# dir_median_total, dir_median_female, dir_median_male
# dir_median_total_5_14, dir_median_female_5_14, dir_median_male_5_14
# dir_median_total_15_24, dir_median_female_15_24, dir_median_male_15_24


# 3. Modify and run "update/1.Update median agg and results.csv.R" to copy
# required results.csv files, and country summary.csv folder names in "output/".
# Result folder names, country summary file names, and stillbirth input names are
# loaded from release_metadata().

# 4. Build M49 region and initiate folder structure, all the files in these
# "median_results" median folders will be created by the script: 
# "update/2.Create M49 regions and initiate app.R"

# 5. Extra
# Show data from year_started, loaded from release_metadata().
# year_end is set to be the maximum available year in results.csv, which is the latest year. There is no need to set manually
