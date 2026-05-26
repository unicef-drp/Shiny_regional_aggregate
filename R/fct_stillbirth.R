#' Read packaged stillbirth country median input
#' @noRd
read_stillbirth_country_medians <- function(path = release_path("input", "stillbirth_country_medians.csv")) {
  if (!file.exists(path)) {
    return(NULL)
  }

  dt <- data.table::fread(path)
  required <- c("ISO3Code", "Shortind", "Year", "Median")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0L) {
    stop("Stillbirth country median input is missing columns: ", paste(missing, collapse = ", "))
  }

  dt[, ISO3Code := toupper(trimws(as.character(ISO3Code)))]
  dt[, Shortind := as.character(Shortind)]
  dt[, Year := as.integer(floor(as.numeric(Year)))]
  dt[, Median := as.numeric(Median)]
  dt[Shortind %in% c("SBR", "SB", "LB")]
}

#' Aggregate stillbirth medians for selected regions
#' @noRd
aggregate_stillbirth_medians <- function(region_iso, stillbirth_country, include_world = TRUE) {
  if (is.null(stillbirth_country) || nrow(stillbirth_country) == 0L) {
    return(NULL)
  }

  membership <- data.table::as.data.table(region_iso)
  iso_cols <- names(membership)[grepl("ISO", toupper(names(membership)))]
  if (length(iso_cols) == 0L || !"Region" %in% names(membership)) {
    stop("region_iso must contain Region and ISO3Code columns.")
  }
  data.table::setnames(membership, iso_cols[1], "ISO3Code")
  membership <- unique(membership[, .(
    Region = trimws(as.character(Region)),
    ISO3Code = toupper(trimws(as.character(ISO3Code)))
  )])
  membership <- membership[!is.na(Region) & nzchar(Region) & !is.na(ISO3Code) & nzchar(ISO3Code)]

  country <- data.table::copy(stillbirth_country)
  country[, ISO3Code := toupper(trimws(as.character(ISO3Code)))]
  country[, Year := as.integer(floor(as.numeric(Year)))]
  country[, Median := as.numeric(Median)]
  country <- country[Shortind %in% c("SBR", "SB", "LB")]

  country_wide <- data.table::dcast(
    country,
    ISO3Code + Year ~ Shortind,
    value.var = "Median",
    fun.aggregate = function(x) x[1],
    fill = NA_real_
  )

  if (!"SB" %in% names(country_wide) && all(c("SBR", "LB") %in% names(country_wide))) {
    country_wide[, SB := round(LB * (SBR / 1000) / (1 - SBR / 1000))]
  }
  if (!all(c("SB", "LB") %in% names(country_wide))) {
    stop("Stillbirth country median input must contain SB and LB, or SBR and LB.")
  }

  sum_or_na <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    sum(x, na.rm = TRUE)
  }

  aggregate_membership <- function(member_dt) {
    if (nrow(member_dt) == 0L) {
      return(NULL)
    }

    region_country <- merge(member_dt, country_wide, by = "ISO3Code", allow.cartesian = TRUE)
    if (nrow(region_country) == 0L) {
      return(NULL)
    }

    out <- region_country[, .(
      Stillbirths = sum_or_na(SB),
      live_births = sum_or_na(LB)
    ), by = .(Region, Year)]
    out[, `Stillbirth rate` := data.table::fifelse(
      !is.na(Stillbirths) & !is.na(live_births) & (Stillbirths + live_births) > 0,
      Stillbirths / (Stillbirths + live_births) * 1000,
      NA_real_
    )]
    out[, Sex := "Total"]
    out[, .(Region, Year, Sex, `Stillbirth rate`, Stillbirths)]
  }

  selected <- aggregate_membership(membership)
  if (!isTRUE(include_world)) {
    return(selected)
  }

  world_membership <- unique(country_wide[, .(Region = "World", ISO3Code)])
  data.table::rbindlist(list(selected, aggregate_membership(world_membership)), use.names = TRUE, fill = TRUE)
}

#' Append stillbirth median columns to total aggregate results
#' @noRd
append_stillbirth_results <- function(results, stillbirth_results) {
  if (is.null(stillbirth_results) || nrow(stillbirth_results) == 0L || is.null(results$both)) {
    return(results)
  }

  out <- results
  total <- data.table::copy(out$both)
  if (!"Sex" %in% names(total)) {
    total[, Sex := "Total"]
  }

  stillbirth_cols <- c("Stillbirth rate", "Stillbirths")
  for (col in stillbirth_cols) {
    if (!col %in% names(total)) {
      total[, (col) := NA_real_]
    }
  }

  stillbirth <- data.table::copy(stillbirth_results)
  stillbirth[, Year := as.integer(floor(as.numeric(Year)))]
  stillbirth[, Sex := as.character(Sex)]

  total[
    stillbirth,
    (stillbirth_cols) := list(`i.Stillbirth rate`, i.Stillbirths),
    on = c("Region", "Year", "Sex")
  ]

  out$both <- total
  out
}

#' Add packaged stillbirth medians to aggregate results
#' @noRd
add_stillbirth_medians <- function(results, region_iso, stillbirth_country = read_stillbirth_country_medians()) {
  stillbirth_results <- aggregate_stillbirth_medians(region_iso, stillbirth_country)
  append_stillbirth_results(results, stillbirth_results)
}

#' Under-five rate indicators shown in the main app chart
#' @noRd
under_five_rate_plot_indicators <- function() {
  c("U5MR median", "IMR median", "NMR median")
}

#' Stillbirth rate indicator shown in a separate app chart
#' @noRd
stillbirth_rate_plot_indicators <- function() {
  "Stillbirth rate"
}

#' Under-five count indicators shown in the main app chart
#' @noRd
under_five_count_plot_indicators <- function(columns) {
  setdiff(grep("deaths", columns, value = TRUE), "Stillbirths")
}

#' Stillbirth count indicator shown in a separate app chart
#' @noRd
stillbirth_count_plot_indicators <- function(columns) {
  intersect("Stillbirths", columns)
}

#' Bootstrap column width for stillbirth plots in the app
#' @noRd
stillbirth_plot_column_width <- function() {
  4L
}
