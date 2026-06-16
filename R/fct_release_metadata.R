#' Current packaged release metadata
#' @noRd
release_metadata_value <- function(name, default) {
  get0(name, envir = globalenv(), inherits = FALSE, ifnotfound = default)
}

#' Release metadata defaults maintained each annual update
#' @noRd
release_metadata_defaults <- function() {
  list(
    update_string = "Last updated: March, 2026",
    WPP_Year = 2024L,
    stillbirth_aggregate_results_dir = file.path(
      Sys.getenv("USERPROFILE"),
      "Dropbox",
      "UNICEF Stillbirth",
      "Aggregate results 2026-01-28"
    )
  )
}

#' @noRd
release_metadata <- function() {
  defaults <- release_metadata_defaults()
  wpp_year <- as.integer(release_metadata_value("WPP_Year", defaults$WPP_Year))
  dir_extdata <- file.path("inst", "extdata")

  list(
    update_string = release_metadata_value("update_string0", defaults$update_string),
    WPP_Year = wpp_year,
    population_file_male = paste0("data_male_CMEpopulation.WPP", wpp_year, ".csv"),
    population_file_female = paste0("data_female_CMEpopulation.WPP", wpp_year, ".csv"),
    population_file_male_10q5 = paste0("data_male_CME_WPP", wpp_year, "_10q5.csv"),
    population_file_female_10q5 = paste0("data_female_CME_WPP", wpp_year, "_10q5.csv"),
    population_file_male_10q15 = paste0("data_male_CME_WPP", wpp_year, "_10q15.csv"),
    population_file_female_10q15 = paste0("data_female_CME_WPP", wpp_year, "_10q15.csv"),
    dir_extdata = dir_extdata,
    dir_input = file.path(dir_extdata, "input"),
    dir_output = file.path(dir_extdata, "output"),
    dir_examples = file.path(dir_extdata, "examples"),
    dir_www = file.path("inst", "app", "www"),
    dir_median_total = file.path(dir_extdata, "median_results_total"),
    dir_median_female = file.path(dir_extdata, "median_results_female"),
    dir_median_male = file.path(dir_extdata, "median_results_male"),
    dir_median_total_5_14 = file.path(dir_extdata, "median_results_total_5_14"),
    dir_median_female_5_14 = file.path(dir_extdata, "median_results_female_5_14"),
    dir_median_male_5_14 = file.path(dir_extdata, "median_results_male_5_14"),
    dir_median_total_15_24 = file.path(dir_extdata, "median_results_total_15_24"),
    dir_median_female_15_24 = file.path(dir_extdata, "median_results_female_15_24"),
    dir_median_male_15_24 = file.path(dir_extdata, "median_results_male_15_24"),
    year_started = 1990,
    runname.U5MR = "U5MR",
    runname.IMR = "IMR",
    runname.NMR = "NMR",
    file_name_NMR = file.path("NMR", "Results.csv"),
    file_name_total = "Rates & Deaths_Country Summary.csv",
    file_name_female = "Rates & Deaths(ADJUSTED)_female_Country Summary.csv",
    file_name_male = "Rates & Deaths(ADJUSTED)_male_Country Summary.csv",
    file_name_total_5_24 = "Rates & Deaths(ADJUSTED)_Country Summary.csv",
    file_name_female_5_24 = "Rates & Deaths_Country Summary.csv",
    file_name_male_5_24 = "Rates & Deaths_Country Summary.csv",
    dir_stillbirth_aggregate_results = release_metadata_value(
      "dir_stillbirth_aggregate_results",
      defaults$stillbirth_aggregate_results_dir
    ),
    file_name_stillbirth_country_results = "UNIGME_SBR_CountryResults.csv",
    file_name_stillbirth_country_medians = "stillbirth_country_medians.csv"
  )
}
