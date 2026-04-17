#' Convert one aggregate table to the long download format
#' @noRd
table_to_long_download <- function(dt) {
  if (is.null(dt)) {
    return(NULL)
  }

  out <- data.table::copy(dt)
  if (!"Sex" %in% names(out)) {
    out[, Sex := "Total"]
  }
  out[Sex == "Both", Sex := "Total"]

  value_cols <- setdiff(names(out), c("Region", "Year", "Sex"))
  if (length(value_cols) == 0L) {
    return(NULL)
  }

  for (col in value_cols) {
    data.table::set(out, j = col, value = as.double(out[[col]]))
  }

  out <- data.table::melt(
    out,
    id.vars = c("Region", "Year", "Sex"),
    measure.vars = value_cols,
    variable.name = "Shortind",
    value.name = "Median",
    variable.factor = FALSE
  )

  out <- out[!is.na(Median), .(Region, Shortind, Sex, Year, Median)]
  data.table::setorder(out, Region, Shortind, Sex, Year)
  out[]
}

#' Build the combined long-format output
#' @noRd
build_long_download <- function(results, region_code_lookup = NULL) {
  pieces <- Filter(
    Negate(is.null),
    list(
      table_to_long_download(results$both),
      table_to_long_download(results$f),
      table_to_long_download(results$m),
      table_to_long_download(results$both_5_24),
      table_to_long_download(results$f_5_24),
      table_to_long_download(results$m_5_24)
    )
  )

  out <- data.table::rbindlist(pieces, fill = TRUE)
  if (!is.null(region_code_lookup) && nrow(region_code_lookup) > 0L) {
    region_code_lookup <- unique(region_code_lookup[, .(Region, Region_Code)], by = "Region")
    out[region_code_lookup, Region_Code := i.Region_Code, on = "Region"]
    data.table::setcolorder(out, c("Region", "Region_Code", "Shortind", "Sex", "Year", "Median"))
  }

  data.table::setorder(out, Region, Shortind, Sex, Year)
  out[]
}
