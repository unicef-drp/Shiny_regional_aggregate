testthat::test_that("app_ui contains the original app structure", {
  html <- htmltools::renderTags(pkg_fn("app_ui")(NULL))$html

  testthat::expect_match(html, "Aggregate Selected Countries")
  testthat::expect_match(html, "Table and Data Download")
  testthat::expect_match(html, "About")
  testthat::expect_match(html, "country_input_select")
  testthat::expect_match(html, "ISO_input")
  testthat::expect_match(html, "click_run")
})
