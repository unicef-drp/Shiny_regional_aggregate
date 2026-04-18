#' Rename adhoc output tables into user-facing shape
#' @noRd
change_adhoc_name <- function(dt, adhoc_name = NULL) {
  out <- data.table::copy(dt)
  data.table::setnames(out, gsub("\\.", " ", names(out)))
  data.table::setnames(out, gsub("Under five", "Under-five", names(out)))
  if (!is.null(adhoc_name) && nzchar(adhoc_name)) {
    out[Region == "Adhoc", Region := adhoc_name]
  }
  out[]
}

#' Read aggregate outputs from a runtime workspace
#' @noRd
read_runtime_results <- function(workspace, adhoc_name = NULL) {
  both <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_total", "Rates & Deaths_AdhocCountries.csv")), adhoc_name)
  f <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_female", "Rates & Deaths(ADJUSTED)_female_AdhocCountries.csv")), adhoc_name)
  m <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_male", "Rates & Deaths(ADJUSTED)_male_AdhocCountries.csv")), adhoc_name)
  both[, Sex := "Total"]
  f[, Sex := "Female"]
  m[, Sex := "Male"]

  dt5_14 <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_total_5_14", "Rates & Deaths(ADJUSTED)_AdhocCountries.csv")), adhoc_name)
  dt15_24 <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_total_15_24", "Rates & Deaths(ADJUSTED)_AdhocCountries.csv")), adhoc_name)
  data.table::setnames(dt5_14, gsub(" median", "", names(dt5_14)))
  data.table::setnames(dt15_24, gsub(" median", "", names(dt15_24)))
  dt5_14 <- recode_ind_5_14(dt5_14)
  dt15_24 <- recode_ind_15_24(dt15_24)
  both_5_24 <- calculate.10q10(merge(dt15_24, dt5_14, by = c("Region", "Year"), sort = FALSE))
  both_5_24[, Sex := "Both"]

  f5_14 <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_female_5_14", "Rates & Deaths_AdhocCountries.csv")), adhoc_name)
  f15_24 <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_female_15_24", "Rates & Deaths_AdhocCountries.csv")), adhoc_name)
  data.table::setnames(f5_14, gsub(" median", "", names(f5_14)))
  data.table::setnames(f15_24, gsub(" median", "", names(f15_24)))
  f5_14 <- recode_ind_5_14(f5_14)
  f15_24 <- recode_ind_15_24(f15_24)
  f_5_24 <- calculate.10q10(merge(f15_24, f5_14, by = c("Region", "Year"), sort = FALSE))
  f_5_24[, Sex := "Female"]

  m5_14 <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_male_5_14", "Rates & Deaths_AdhocCountries.csv")), adhoc_name)
  m15_24 <- change_adhoc_name(data.table::fread(runtime_path(workspace, "median_results_male_15_24", "Rates & Deaths_AdhocCountries.csv")), adhoc_name)
  data.table::setnames(m5_14, gsub(" median", "", names(m5_14)))
  data.table::setnames(m15_24, gsub(" median", "", names(m15_24)))
  m5_14 <- recode_ind_5_14(m5_14)
  m15_24 <- recode_ind_15_24(m15_24)
  m_5_24 <- calculate.10q10(merge(m15_24, m5_14, by = c("Region", "Year"), sort = FALSE))
  m_5_24[, Sex := "Male"]

  list(
    both = both,
    f = f,
    m = m,
    both_5_24 = both_5_24,
    f_5_24 = f_5_24,
    m_5_24 = m_5_24
  )
}
