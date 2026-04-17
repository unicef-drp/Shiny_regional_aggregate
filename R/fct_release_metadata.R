#' Current packaged release metadata
#' @noRd
release_metadata <- function() {
  list(
    id = "release-2026",
    update_string = "Last updated: March, 2026",
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
