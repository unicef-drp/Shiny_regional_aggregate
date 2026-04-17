testthat::test_that("build_app_context loads packaged app metadata", {
  ctx <- pkg_fn("build_app_context")()

  testthat::expect_true(data.table::is.data.table(ctx$dc))
  testthat::expect_true(length(ctx$countries) > 0)
  testthat::expect_true(length(ctx$ISOs) > 0)
  testthat::expect_true(inherits(ctx$world_map, "SpatialPolygonsDataFrame"))
  testthat::expect_true("update_string" %in% names(ctx))
})
