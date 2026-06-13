testthat::test_that("build_app_context loads packaged app metadata", {
  ctx <- pkg_fn("build_app_context")()

  testthat::expect_true(data.table::is.data.table(ctx$dc))
  testthat::expect_true(length(ctx$countries) > 0)
  testthat::expect_true(length(ctx$ISOs) > 0)
  testthat::expect_true(inherits(ctx$world_map, "SpatialPolygonsDataFrame"))
  testthat::expect_true("update_string" %in% names(ctx))
  testthat::expect_identical(ctx$WPP_Year, 2024L)
})

testthat::test_that("release metadata uses WPP year from annual update globals", {
  old_exists <- exists("WPP_Year", envir = globalenv(), inherits = FALSE)
  old_value <- if (old_exists) get("WPP_Year", envir = globalenv()) else NULL
  withr::defer({
    if (old_exists) {
      assign("WPP_Year", old_value, envir = globalenv())
    } else if (exists("WPP_Year", envir = globalenv(), inherits = FALSE)) {
      rm(WPP_Year, envir = globalenv())
    }
  })

  assign("WPP_Year", 2099L, envir = globalenv())

  testthat::expect_identical(pkg_fn("release_metadata")()$WPP_Year, 2099L)
})

testthat::test_that("build_app_context excludes countries unavailable for app aggregates", {
  ctx <- pkg_fn("build_app_context")(force = TRUE)

  testthat::expect_false("LIE" %in% ctx$ISOs)
  testthat::expect_false("Liechtenstein" %in% ctx$countries)
  testthat::expect_false(any(ctx$dc$ISO3Code == "LIE"))
  testthat::expect_false(any(vapply(
    ctx$input_country_list,
    function(countries) "Liechtenstein" %in% countries,
    logical(1)
  )))
})
