#' Current packaged release metadata
#' @noRd
release_metadata_value <- function(name, default) {
  get0(name, envir = globalenv(), inherits = FALSE, ifnotfound = default)
}

#' @noRd
release_metadata <- function() {
  list(
    update_string = release_metadata_value("update_string0", "Last updated: March, 2026"),
    WPP_Year = as.integer(release_metadata_value("WPP_Year", 2024L)),
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
    file_name_male_5_24 = "Rates & Deaths_Country Summary.csv"
  )
}
