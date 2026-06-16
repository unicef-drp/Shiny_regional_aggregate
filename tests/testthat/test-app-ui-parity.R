testthat::test_that("app_ui contains the original app structure", {
  html <- htmltools::renderTags(pkg_fn("app_ui")(NULL))$html

  testthat::expect_match(html, "Aggregate selected countries")
  testthat::expect_match(html, "Tables and data download")
  testthat::expect_match(html, "About")
  testthat::expect_match(html, "country_input_select")
  testthat::expect_match(html, "ISO_input")
  testthat::expect_match(html, "click_run")
})

testthat::test_that("app_ui renders proofread copy", {
  html <- htmltools::renderTags(pkg_fn("app_ui")(NULL))$html

  testthat::expect_match(html, "This Shiny app produces regional aggregates", fixed = TRUE)
  testthat::expect_match(html, "Please select countries from the drop-down list or upload a file", fixed = TRUE)
  testthat::expect_match(html, "Optional: Upload ISO alpha-3 country codes", fixed = TRUE)
  testthat::expect_match(html, "Column name must contain &quot;ISO&quot;", fixed = TRUE)
  testthat::expect_match(html, "For questions, feedback, or suggestions", fixed = TRUE)

  testthat::expect_false(grepl("ShinyApp", html, fixed = TRUE))
  testthat::expect_false(grepl("Column name shall contain", html, fixed = TRUE))
  testthat::expect_false(grepl("feedback or suggestion", html, fixed = TRUE))
})

testthat::test_that("server-rendered copy uses proofread wording", {
  server_source <- paste(readLines(testthat::test_path("..", "..", "R", "app_server.R")), collapse = "\n")

  testthat::expect_match(server_source, "Download indicator definitions", fixed = TRUE)
  testthat::expect_match(server_source, "Female data", fixed = TRUE)
  testthat::expect_match(server_source, "Male data", fixed = TRUE)
  testthat::expect_match(server_source, "Download all in long format", fixed = TRUE)
  testthat::expect_match(server_source, "This takes about", fixed = TRUE)

  testthat::expect_false(grepl("Data for the female", server_source, fixed = TRUE))
  testthat::expect_false(grepl("Data for the male", server_source, fixed = TRUE))
  testthat::expect_false(grepl("long-format", server_source, fixed = TRUE))
})

testthat::test_that("custom group display-name text box has a fixed size", {
  shiny::testServer(pkg_fn("app_server"), {
    html <- paste(as.character(output$panel_custom_group_name), collapse = "\n")

    testthat::expect_match(html, 'id="adhoc_name"', fixed = TRUE)
    testthat::expect_match(html, "height:38px", fixed = TRUE)
    testthat::expect_match(html, "resize:none", fixed = TRUE)
  })
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
  expected <- paste("World Population Prospects", pkg_fn("release_metadata_defaults")()$WPP_Year)

  testthat::expect_match(html, expected, fixed = TRUE)
  testthat::expect_false(grepl("World Population Prospects 2022", html, fixed = TRUE))
})
