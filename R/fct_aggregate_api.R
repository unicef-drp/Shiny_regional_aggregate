#' Compute regional child mortality aggregates
#'
#' @param region_iso A data.frame or data.table containing at least Region and ISO3Code.
#' @return A data.table with Region, Region_Code when available, Shortind, Sex, Year, and Median.
#' @export
get_CME_aggregate <- function(region_iso) {
  results <- get_CME_aggregate_results(region_iso)
  build_long_download(results, results$region_code_lookup)
}
