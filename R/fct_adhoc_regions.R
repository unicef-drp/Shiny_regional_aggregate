#' Apply region membership columns to a country-info table
#' @noRd
apply_region_membership <- function(country_info, membership_wide) {
  out <- data.table::copy(country_info)
  wide <- membership_wide$data

  for (col in membership_wide$region_cols) {
    out[, (col) := ""]
    merge_dt <- wide[, .(ISO3Code, region_value = get(col))]
    out[merge_dt, on = "ISO3Code", (col) := data.table::fifelse(is.na(i.region_value) | i.region_value == "", "", i.region_value)]
  }

  out[]
}

#' Write adhoc country-info files into the runtime workspace
#' @noRd
write_adhoc_country_info <- function(region_iso, workspace) {
  membership <- build_region_membership_wide(region_iso)

  total <- data.table::fread(runtime_path(workspace, "input", "country.info.CME.csv"))
  five14 <- data.table::fread(runtime_path(workspace, "input", "country.info.CME.5_14.csv"))
  fifteen24 <- data.table::fread(runtime_path(workspace, "input", "country.info.CME.15_24.csv"))

  data.table::fwrite(
    apply_region_membership(total, membership),
    runtime_path(workspace, "input", "country.info.CME_adhoc.csv")
  )
  data.table::fwrite(
    apply_region_membership(five14, membership),
    runtime_path(workspace, "input", "country.info.CME.5_14_adhoc.csv")
  )
  data.table::fwrite(
    apply_region_membership(fifteen24, membership),
    runtime_path(workspace, "input", "country.info.CME.15_24_adhoc.csv")
  )

  membership
}
