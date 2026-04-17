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
