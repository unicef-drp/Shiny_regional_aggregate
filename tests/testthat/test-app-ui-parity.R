testthat::test_that("app_ui contains the original app structure", {
  html <- htmltools::renderTags(pkg_fn("app_ui")(NULL))$html

  testthat::expect_match(html, "Aggregate Selected Countries")
  testthat::expect_match(html, "Table and Data Download")
  testthat::expect_match(html, "About")
  testthat::expect_match(html, "country_input_select")
  testthat::expect_match(html, "ISO_input")
  testthat::expect_match(html, "click_run")
})

testthat::test_that("upload examples are actual downloads", {
  html <- htmltools::renderTags(pkg_fn("app_ui")(NULL))$html

  testthat::expect_match(
    html,
    'href="www/Upload_ISO3Code_example_single_region.csv"[^>]+download="Upload_ISO3Code_example_single_region.csv"'
  )
  testthat::expect_match(
    html,
    'href="www/Upload_ISO3Code_example_multiple_regions.csv"[^>]+download="Upload_ISO3Code_example_multiple_regions.csv"'
  )
})

testthat::test_that("map output reserves enough space for the world view", {
  html <- as.character(htmltools::renderTags(pkg_fn("app_ui")(NULL))[["html"]])

  testthat::expect_match(html, 'id="mymap"', fixed = TRUE)
  testthat::expect_match(html, "height:500px", fixed = TRUE)
})

testthat::test_that("About panel uses the configured WPP year", {
  html <- as.character(htmltools::renderTags(pkg_fn("app_ui")(NULL))[["html"]])

  testthat::expect_match(html, "World Population Prospects 2024", fixed = TRUE)
  testthat::expect_false(grepl("World Population Prospects 2022", html, fixed = TRUE))
})
