#' Run the full aggregate pipeline in a seeded runtime workspace
#' @noRd
run_full_aggregate_pipeline <- function(workspace) {
  state <- build_legacy_state(workspace)
  old_wd <- setwd(workspace)
  on.exit(setwd(old_wd), add = TRUE)

  with_legacy_state(workspace, {
    run.outputaggregates(state$year.lastestimatepublished, reuse.replacement.country = TRUE)
    run.outputaggregates.gender(state$year.lastestimatepublished, reuse.replacement.country = TRUE)
    adjust.u5.sex.specific.death()
    run.outputaggregates.5.24(state$year.lastestimatepublished)
    run.outputaggregates.5.24.gender(state$year.lastestimatepublished)
    adjust.total.death.5.24()
  })

  invisible(workspace)
}
