#' Read a region/ISO upload file
#' @noRd
read_region_iso_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  dt <- switch(
    ext,
    csv = data.table::fread(path),
    xlsx = data.table::setDT(readxl::read_excel(path)),
    xls = data.table::setDT(readxl::read_excel(path)),
    stop("Unsupported input file type: ", ext)
  )

  normalize_region_iso_input(dt)
}

#' Normalize region/ISO input
#' @noRd
normalize_region_iso_input <- function(region_iso) {
  dt <- data.table::as.data.table(region_iso)
  valid_iso <- unique(data.table::fread(release_path("input", "country.info.CME.csv"))[["ISO3Code"]])

  iso_cols <- names(dt)[grepl("ISO", toupper(names(dt)))]
  if (length(iso_cols) == 0L) {
    stop("Input must contain an ISO3Code column.")
  }

  data.table::setnames(dt, iso_cols[1], "ISO3Code")

  if (!"Region" %in% names(dt)) {
    stop("Input must contain a Region column.")
  }

  dt[, ISO3Code := toupper(trimws(as.character(ISO3Code)))]
  dt[, Region := trimws(as.character(Region))]

  if ("Region_Code" %in% names(dt)) {
    dt[, Region_Code := trimws(as.character(Region_Code))]
  }

  if ("OfficialName" %in% names(dt)) {
    dt[, OfficialName := trimws(as.character(OfficialName))]
  }

  dt <- dt[!is.na(ISO3Code) & nzchar(ISO3Code) & !is.na(Region) & nzchar(Region)]
  dt <- dt[ISO3Code %in% valid_iso]

  if (nrow(dt) == 0L) {
    stop("Input contains no valid Region/ISO3Code rows.")
  }

  dt[]
}

#' Convert long region membership input into the wide AdhocCountries shape
#' @noRd
build_region_membership_wide <- function(region_iso) {
  dt <- data.table::copy(normalize_region_iso_input(region_iso))
  dt[, region_level := seq_len(.N), by = ISO3Code]

  dt_wide <- data.table::dcast(
    dt,
    ISO3Code ~ region_level,
    value.var = "Region",
    fill = ""
  )

  region_level_cols <- setdiff(names(dt_wide), "ISO3Code")
  new_names <- c(
    "ISO3Code",
    "AdhocCountries",
    if (length(region_level_cols) > 1L) paste0("AdhocCountries", seq.int(2L, length(region_level_cols))) else NULL
  )
  data.table::setnames(dt_wide, names(dt_wide), new_names)

  region_code_lookup <- NULL
  if ("Region_Code" %in% names(dt)) {
    region_code_lookup <- unique(dt[, .(Region, Region_Code)], by = "Region")
  }

  list(
    data = dt_wide,
    region_cols = setdiff(names(dt_wide), "ISO3Code"),
    region_code_lookup = region_code_lookup
  )
}

#' Build an HTML country summary grouped by uploaded regions
#' @noRd
build_grouped_country_summary_html <- function(selected_countries, region_structure, country_lookup) {
  if (is.null(region_structure)) {
    return(NULL)
  }

  dt_wide <- data.table::copy(region_structure$data)
  region_cols <- intersect(region_structure$region_cols, colnames(dt_wide))
  if (length(region_cols) == 0) {
    return(NULL)
  }

  selected_lookup <- data.table::copy(country_lookup)
  if (!is.null(region_structure$normalized) &&
      all(c("ISO3Code", "OfficialName") %in% colnames(region_structure$normalized))) {
    selected_lookup <- unique(rbind(
      selected_lookup,
      region_structure$normalized[, .(ISO3Code, OfficialName)],
      fill = TRUE
    ))
  }

  selected_iso <- unique(selected_lookup[OfficialName %in% selected_countries, ISO3Code])
  dt_wide <- dt_wide[ISO3Code %in% selected_iso]
  if (nrow(dt_wide) == 0) {
    return(NULL)
  }

  dt_wide[country_lookup[, .(ISO3Code, OfficialName)], OfficialName := i.OfficialName, on = "ISO3Code"]
  dt_wide[, country_order := seq_len(.N)]

  dt_long <- data.table::melt(
    dt_wide,
    id.vars = c("ISO3Code", "OfficialName", "country_order"),
    measure.vars = region_cols,
    variable.name = "RegionLevel",
    value.name = "Region",
    variable.factor = FALSE
  )
  dt_long <- dt_long[Region != "" & !is.na(Region)]
  if (nrow(dt_long) == 0) {
    return(NULL)
  }

  dt_long[, DisplayName := ifelse(is.na(OfficialName) | OfficialName == "", ISO3Code, OfficialName)]
  assigned_iso <- unique(dt_long$ISO3Code)
  dt_long <- unique(dt_long[, .(Region, DisplayName, country_order)])

  region_groups <- dt_long[, .(
    Countries = paste(sort(unique(DisplayName)), collapse = ", "),
    group_order = min(country_order)
  ), by = Region][order(group_order)]

  if (nrow(region_groups) <= 1) {
    return(NULL)
  }

  ungrouped_iso <- unique(setdiff(selected_iso, assigned_iso))
  if (length(ungrouped_iso) > 0) {
    ungrouped_lookup <- unique(country_lookup[ISO3Code %in% ungrouped_iso, .(ISO3Code, OfficialName)])
    ungrouped_countries <- sort(unique(ifelse(
      is.na(ungrouped_lookup$OfficialName) | ungrouped_lookup$OfficialName == "",
      ungrouped_lookup$ISO3Code,
      ungrouped_lookup$OfficialName
    )))

    region_groups <- rbind(
      region_groups,
      data.table::data.table(
        Region = "Ungrouped",
        Countries = paste(ungrouped_countries, collapse = ", "),
        group_order = Inf
      ),
      fill = TRUE
    )
  }

  paste(
    sprintf(
      "<strong>%s:</strong> %s",
      htmltools::htmlEscape(region_groups$Region),
      htmltools::htmlEscape(region_groups$Countries)
    ),
    collapse = "<br>"
  )
}

#' Build selected map polygons with one fill color per uploaded region
#' @noRd
build_selected_world_map <- function(
  world_map,
  selected_countries,
  country_lookup,
  region_structure = NULL,
  default_region = "Selected Countries"
) {
  empty_map <- function() {
    out <- world_map[FALSE, ]
    out$Region <- character(0)
    out$fillColor <- character(0)
    out
  }

  selected_countries <- unique(trimws(as.character(selected_countries)))
  selected_countries <- selected_countries[!is.na(selected_countries) & nzchar(selected_countries)]
  if (length(selected_countries) == 0L) {
    return(empty_map())
  }

  lookup <- data.table::as.data.table(country_lookup)
  lookup <- unique(lookup[, .(ISO3Code, OfficialName)])
  selected_lookup <- lookup[OfficialName %in% selected_countries]

  if (!is.null(region_structure) && !is.null(region_structure$normalized)) {
    normalized <- data.table::copy(region_structure$normalized)
    normalized[, upload_order := seq_len(.N)]
    selected_regions <- normalized[ISO3Code %in% selected_lookup$ISO3Code]
    selected_regions <- selected_regions[order(upload_order)]
    selected_regions <- unique(selected_regions[, .(ISO3Code, Region, upload_order)], by = "ISO3Code")

    country_region <- selected_regions[
      lookup,
      on = "ISO3Code",
      nomatch = 0
    ][, .(country = OfficialName, Region, upload_order)]

    missing_uploaded <- setdiff(selected_lookup$OfficialName, country_region$country)
    if (length(missing_uploaded) > 0L) {
      country_region <- data.table::rbindlist(
        list(
          country_region,
          data.table::data.table(
            country = missing_uploaded,
            Region = default_region,
            upload_order = seq.int(nrow(country_region) + 1L, nrow(country_region) + length(missing_uploaded))
          )
        ),
        fill = TRUE
      )
    }
  } else {
    country_region <- selected_lookup[, .(
      country = OfficialName,
      Region = default_region,
      upload_order = seq_len(.N)
    )]
  }

  direct_map_names <- setdiff(selected_countries, lookup$OfficialName)
  direct_map_names <- direct_map_names[direct_map_names %in% world_map$country]
  if (length(direct_map_names) > 0L) {
    country_region <- data.table::rbindlist(
      list(
        country_region,
        data.table::data.table(
          country = direct_map_names,
          Region = default_region,
          upload_order = seq.int(nrow(country_region) + 1L, nrow(country_region) + length(direct_map_names))
        )
      ),
      fill = TRUE
    )
  }

  country_region <- country_region[!is.na(country) & nzchar(country) & !is.na(Region) & nzchar(Region)]
  if (nrow(country_region) == 0L) {
    return(empty_map())
  }

  country_region <- country_region[order(upload_order)]
  country_region <- unique(country_region, by = "country")

  if ("China" %in% country_region$country &&
      "Taiwan" %in% world_map$country &&
      !"Taiwan" %in% country_region$country) {
    china_region <- country_region[country == "China"][1]
    country_region <- data.table::rbindlist(
      list(
        country_region,
        data.table::data.table(
          country = "Taiwan",
          Region = china_region$Region,
          upload_order = china_region$upload_order + 0.1
        )
      ),
      fill = TRUE
    )
  }

  region_colors <- data.table::data.table(
    Region = unique(country_region$Region),
    fillColor = ResolvePlotColors(data.table::uniqueN(country_region$Region))
  )
  country_region[region_colors, fillColor := i.fillColor, on = "Region"]

  selected_map <- world_map[world_map$country %in% country_region$country, ]
  if (nrow(selected_map) == 0L) {
    return(empty_map())
  }

  country_match <- match(selected_map$country, country_region$country)
  selected_map$Region <- country_region$Region[country_match]
  selected_map$fillColor <- country_region$fillColor[country_match]
  selected_map
}
