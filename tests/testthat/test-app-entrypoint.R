testthat::test_that("run_app returns a shiny app object", {
  app <- run_app()
  testthat::expect_s3_class(app, "shiny.appobj")
})
