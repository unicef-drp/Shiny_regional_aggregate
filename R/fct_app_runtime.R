#' Normalize upload input for the app, allowing files without Region
#' @noRd
normalize_app_region_iso_input <- function(region_iso) {
  dt <- data.table::as.data.table(region_iso)
  valid_iso <- build_app_context()$ISOs

  iso_cols <- names(dt)[grepl("ISO", toupper(names(dt)))]
  if (length(iso_cols) == 0L) {
    stop("Could not find an ISO3Code column. Please ensure your file has a column named 'ISO3Code'.")
  }

  data.table::setnames(dt, iso_cols[1], "ISO3Code")
  dt[, ISO3Code := toupper(trimws(as.character(ISO3Code)))]

  if (!"Region" %in% names(dt)) {
    dt[, Region := "Adhoc"]
  } else {
    dt[, Region := trimws(as.character(Region))]
    dt[is.na(Region) | !nzchar(Region), Region := "Adhoc"]
  }

  if ("Region_Code" %in% names(dt)) {
    dt[, Region_Code := trimws(as.character(Region_Code))]
  }

  if ("OfficialName" %in% names(dt)) {
    dt[, OfficialName := trimws(as.character(OfficialName))]
  }

  dt <- dt[!is.na(ISO3Code) & nzchar(ISO3Code)]
  dt <- dt[ISO3Code %in% valid_iso]

  if (nrow(dt) == 0L) {
    stop("Input contains no valid Region/ISO3Code rows.")
  }

  dt[]
}

#' Read a region/ISO upload file for the app
#' @noRd
read_app_region_iso_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  dt <- switch(
    ext,
    csv = data.table::fread(path),
    xlsx = data.table::setDT(readxl::read_excel(path)),
    xls = data.table::setDT(readxl::read_excel(path)),
    stop("Accepted file types are CSV, XLSX, and XLS. Please upload the file again.")
  )

  normalize_app_region_iso_input(dt)
}

#' Build the picker update after a successful uploaded ISO file
#' @noRd
build_uploaded_country_picker_update <- function(country_lookup, membership, choices) {
  lookup <- data.table::as.data.table(country_lookup)
  membership_iso <- unique(toupper(trimws(as.character(membership$data$ISO3Code))))

  selected <- unique(lookup[ISO3Code %in% membership_iso, OfficialName])
  selected <- selected[!is.na(selected) & nzchar(selected)]
  selected <- choices[choices %in% selected]

  list(
    choices = choices,
    selected = selected
  )
}

#' Compute packaged aggregate outputs used by the app
#' @noRd
get_CME_aggregate_results <- function(region_iso, adhoc_name = NULL) {
  normalized <- normalize_region_iso_input(region_iso)
  workspace <- create_runtime_workspace()
  on.exit(unlink(workspace, recursive = TRUE, force = TRUE), add = TRUE)

  membership <- write_adhoc_country_info(normalized, workspace)
  run_full_aggregate_pipeline(workspace)
  results <- read_runtime_results(workspace, adhoc_name = adhoc_name)
  results <- add_stillbirth_medians(results, normalized)
  results$region_code_lookup <- membership$region_code_lookup
  results
}
