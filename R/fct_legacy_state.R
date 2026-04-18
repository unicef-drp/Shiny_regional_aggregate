#' Build the legacy variables expected by the existing wrapper files
#' @noRd
build_legacy_state <- function(workspace) {
  meta <- release_metadata()
  c_median_total <- read.country.summary(
    file.path(workspace, "median_results_total", meta$file_name_total),
    year_wanted = meta$year_started:2030
  )
  year_ended <- floor(max(c_median_total$Year))

  list(
    runname.U5MR = meta$runname.U5MR,
    runname.IMR = meta$runname.IMR,
    runname.NMR = meta$runname.NMR,
    file_name_NMR = meta$file_name_NMR,
    file_name_total = meta$file_name_total,
    file_name_female = meta$file_name_female,
    file_name_male = meta$file_name_male,
    file_name_total_5_24 = meta$file_name_total_5_24,
    file_name_female_5_24 = meta$file_name_female_5_24,
    file_name_male_5_24 = meta$file_name_male_5_24,
    year_started = meta$year_started,
    year.lastestimatepublished = year_ended + 0.5,
    dir_median_total = file.path(workspace, "median_results_total"),
    dir_median_female = file.path(workspace, "median_results_female"),
    dir_median_male = file.path(workspace, "median_results_male"),
    dir_median_total_5_14 = file.path(workspace, "median_results_total_5_14"),
    dir_median_female_5_14 = file.path(workspace, "median_results_female_5_14"),
    dir_median_male_5_14 = file.path(workspace, "median_results_male_5_14"),
    dir_median_total_15_24 = file.path(workspace, "median_results_total_15_24"),
    dir_median_female_15_24 = file.path(workspace, "median_results_female_15_24"),
    dir_median_male_15_24 = file.path(workspace, "median_results_male_15_24"),
    country.info = data.table::fread(file.path(workspace, "input", "country.info.CME.csv"))
  )
}

#' Temporarily bind legacy variables in the package namespace
#' @noRd
with_legacy_state <- function(workspace, code) {
  ns <- asNamespace("shinyregionalaggregate")
  state <- build_legacy_state(workspace)
  old <- vector("list", length(state))
  names(old) <- names(state)
  old_exists <- setNames(logical(length(state)), names(state))

  set_binding <- function(name, value) {
    was_locked <- exists(name, envir = ns, inherits = FALSE) && bindingIsLocked(name, ns)
    if (was_locked) {
      unlockBinding(name, ns)
    }
    assign(name, value, envir = ns)
    if (was_locked) {
      lockBinding(name, ns)
    }
  }

  remove_binding <- function(name) {
    if (!exists(name, envir = ns, inherits = FALSE)) {
      return(invisible(NULL))
    }
    was_locked <- bindingIsLocked(name, ns)
    if (was_locked) {
      unlockBinding(name, ns)
    }
    rm(list = name, envir = ns)
    invisible(NULL)
  }

  for (nm in names(state)) {
    if (exists(nm, envir = ns, inherits = FALSE)) {
      old_exists[[nm]] <- TRUE
      old[[nm]] <- get(nm, envir = ns, inherits = FALSE)
    }
    set_binding(nm, state[[nm]])
  }

  on.exit({
    for (nm in names(state)) {
      if (!old_exists[[nm]]) {
        remove_binding(nm)
      } else {
        set_binding(nm, old[[nm]])
      }
    }
  }, add = TRUE)

  eval(substitute(code), envir = parent.frame())
}
