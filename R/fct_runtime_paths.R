#' Packaged release root
#' @noRd
release_root <- function() {
  release_id <- release_metadata()$id

  installed_root <- app_sys("extdata", release_id)
  if (!identical(installed_root, "") && dir.exists(installed_root)) {
    return(installed_root)
  }

  dev_root <- file.path("inst", "extdata", release_id)
  if (dir.exists(dev_root)) {
    return(normalizePath(dev_root, winslash = "/", mustWork = TRUE))
  }

  stop("Packaged release data was not found in inst/extdata.")
}

#' Join paths below the packaged release
#' @noRd
release_path <- function(...) {
  file.path(release_root(), ...)
}

#' Create a run-specific workspace from packaged release assets
#' @noRd
create_runtime_workspace <- function(parent = tempdir()) {
  workspace <- tempfile("cme-aggregate-", tmpdir = parent)

  dirs_to_copy <- c(
    "input",
    "output",
    "median_results_total",
    "median_results_female",
    "median_results_male",
    "median_results_total_5_14",
    "median_results_female_5_14",
    "median_results_male_5_14",
    "median_results_total_15_24",
    "median_results_female_15_24",
    "median_results_male_15_24"
  )

  dir.create(workspace, recursive = TRUE, showWarnings = FALSE)
  for (dir_name in dirs_to_copy) {
    source_dir <- release_path(dir_name)
    dest_dir <- file.path(workspace, dir_name)

    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    contents <- list.files(source_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(contents) == 0L) {
      next
    }

    ok <- file.copy(contents, dest_dir, recursive = TRUE)
    if (!all(ok)) {
      stop("Failed to copy release directory: ", dir_name)
    }
  }

  legacy_script <- app_sys("legacy", "R", "chooseregion.R")
  if (identical(legacy_script, "") || !file.exists(legacy_script)) {
    legacy_script <- file.path("inst", "legacy", "R", "chooseregion.R")
  }
  if (!file.exists(legacy_script)) {
    stop("Failed to locate legacy helper script: chooseregion.R")
  }

  legacy_r_dir <- file.path(workspace, "R")
  dir.create(legacy_r_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(legacy_script, legacy_r_dir, overwrite = TRUE)) {
    stop("Failed to copy legacy helper script: ", legacy_script)
  }

  workspace
}

#' Runtime file helper
#' @noRd
runtime_path <- function(workspace, ...) {
  file.path(workspace, ...)
}
