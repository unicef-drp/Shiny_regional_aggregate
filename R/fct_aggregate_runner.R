#' Run the full aggregate pipeline in a seeded runtime workspace
#' @noRd
has_replacement_country_cache <- function(output_dir, replace.rates.reg = "M49Region", nn.exists = TRUE) {
  samples_dir <- file.path(output_dir, "samples_combined")
  required <- file.path(
    samples_dir,
    paste0(
      c("death0.ctj", "death1to4.ctj", "deathu5.ctj"),
      ".",
      replace.rates.reg,
      "-replace.rda"
    )
  )
  if (isTRUE(nn.exists)) {
    required <- c(
      required,
      file.path(samples_dir, paste0("deathnn.ctj.", replace.rates.reg, "-replace.rda"))
    )
  }

  if (!all(file.exists(required)) || !file.exists(file.path(samples_dir, "info.rda"))) {
    return(FALSE)
  }

  validator <- get0("ValidateReplacementCountryCacheBWC", mode = "function", inherits = TRUE)
  if (is.null(validator)) {
    return(FALSE)
  }

  isTRUE(validator(
    output.dir.samplescombined = samples_dir,
    replace.rates.reg = replace.rates.reg,
    nn.exists = nn.exists
  ))
}

#' @noRd
run_full_aggregate_pipeline <- function(workspace) {
  state <- build_legacy_state(workspace)
  old_wd <- setwd(workspace)
  on.exit(setwd(old_wd), add = TRUE)

  with_legacy_state(workspace, {
    run.outputaggregates(
      state$year.lastestimatepublished,
      reuse.replacement.country = has_replacement_country_cache(state$dir_median_total)
    )
    run.outputaggregates.gender(
      state$year.lastestimatepublished,
      reuse.replacement.country =
        has_replacement_country_cache(state$dir_median_male, nn.exists = FALSE) &&
        has_replacement_country_cache(state$dir_median_female, nn.exists = FALSE)
    )
    adjust.u5.sex.specific.death()
    run.outputaggregates.5.24(state$year.lastestimatepublished)
    run.outputaggregates.5.24.gender(state$year.lastestimatepublished)
    adjust.total.death.5.24()
  })

  invisible(workspace)
}
