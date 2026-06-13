testthat::test_that("get.world.results true forces world regeneration even when cache exists", {
  get_outputaggregates_object <- function(name) {
    if (exists(name, inherits = TRUE)) {
      return(get(name, inherits = TRUE))
    }
    get(name, envir = asNamespace("shinyregionalaggregate"), inherits = FALSE)
  }

  should_regenerate_world <- get_outputaggregates_object("ShouldRegenerateWorldResultsBWC")

  testthat::expect_true(should_regenerate_world(
    get.world.results = TRUE,
    world.files.exist = TRUE
  ))
  testthat::expect_true(should_regenerate_world(
    get.world.results = TRUE,
    world.files.exist = FALSE
  ))
  testthat::expect_true(should_regenerate_world(
    get.world.results = FALSE,
    world.files.exist = TRUE,
    replacement.needs.rebuild = TRUE
  ))
  testthat::expect_true(should_regenerate_world(
    get.world.results = FALSE,
    world.files.exist = FALSE,
    replacement.needs.rebuild = TRUE
  ))
  testthat::expect_false(should_regenerate_world(
    get.world.results = FALSE,
    world.files.exist = FALSE,
    replacement.needs.rebuild = FALSE
  ))
})
