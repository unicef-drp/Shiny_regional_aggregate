#' Compute regional child mortality aggregates
#'
#' @param region_iso A data.frame or data.table containing at least Region and ISO3Code.
#' @return A data.table with Region, Region_Code when available, Shortind, Sex, Year, and Median.
#' @export
get_CME_aggregate <- function(region_iso) {
  normalized <- normalize_region_iso_input(region_iso)
  workspace <- create_runtime_workspace()
  on.exit(unlink(workspace, recursive = TRUE, force = TRUE), add = TRUE)

  membership <- write_adhoc_country_info(normalized, workspace)
  run_full_aggregate_pipeline(workspace)
  results <- read_runtime_results(workspace, adhoc_name = NULL)
  build_long_download(results, membership$region_code_lookup)
}
