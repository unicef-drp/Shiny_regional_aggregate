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
  testthat::expect_match(html, "mortality rates and deaths for ages 0-24", fixed = TRUE)
  testthat::expect_match(html, "Please select countries from the drop-down list, or upload a file", fixed = TRUE)
  testthat::expect_match(html, "Optional: Upload a file of ISO Alpha-3 country codes", fixed = TRUE)
  testthat::expect_match(html, "Column name must contain &quot;ISO&quot;", fixed = TRUE)
  testthat::expect_match(html, "For any questions, feedback, or suggestions", fixed = TRUE)

  testthat::expect_false(grepl("ShinyApp", html, fixed = TRUE))
  testthat::expect_false(grepl("ISO alpha-3", html, fixed = TRUE))
  testthat::expect_false(grepl("Column name shall contain", html, fixed = TRUE))
  testthat::expect_false(grepl("feedback or suggestion", html, fixed = TRUE))
})

testthat::test_that("server-rendered copy uses proofread wording", {
  server_source <- paste(readLines(testthat::test_path("..", "..", "R", "app_server.R")), collapse = "\n")

  testthat::expect_match(server_source, "Download indicator definitions", fixed = TRUE)
  testthat::expect_match(server_source, "Download table", fixed = TRUE)
  testthat::expect_match(server_source, "Show \\\"World\\\" rates in plots", fixed = TRUE)
  testthat::expect_match(server_source, "uploaded_single_region_name", fixed = TRUE)
  testthat::expect_match(server_source, "Download all in long format", fixed = TRUE)
  testthat::expect_match(server_source, "This takes about", fixed = TRUE)

  testthat::expect_false(grepl("Data for the female", server_source, fixed = TRUE))
  testthat::expect_false(grepl("Data for the male", server_source, fixed = TRUE))
  testthat::expect_false(grepl("Female data", server_source, fixed = TRUE))
  testthat::expect_false(grepl("Male data", server_source, fixed = TRUE))
  testthat::expect_false(grepl("long-format", server_source, fixed = TRUE))
})

testthat::test_that("custom group display-name text box has a fixed size", {
  shiny::testServer(pkg_fn("app_server"), {
    html <- paste(as.character(output$panel_custom_group_name), collapse = "\n")

    testthat::expect_match(html, 'id="adhoc_name"', fixed = TRUE)
    testthat::expect_match(html, "Name of the single custom group aggregate", fixed = TRUE)
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
  testthat::expect_match(
    html,
    'multi-region</a>\\s+-\\s+<a href="www/Country_ISO3Code_reference.csv"[^>]+download="Country_ISO3Code_reference.csv"'
  )
})

testthat::test_that("country code reference download contains app countries", {
  path <- testthat::test_path("..", "..", "inst", "app", "www", "Country_ISO3Code_reference.csv")

  testthat::expect_true(file.exists(path))

  country_codes <- data.table::fread(path)

  testthat::expect_identical(names(country_codes), c("ISO3Code", "OfficialName"))
  testthat::expect_identical(nrow(country_codes), 200L)
  testthat::expect_identical(length(unique(country_codes$ISO3Code)), 200L)
  testthat::expect_false("LIE" %in% country_codes$ISO3Code)
  testthat::expect_true(all(nchar(country_codes$ISO3Code) == 3L))
})

testthat::test_that("map output reserves enough space for the world view", {
  html <- as.character(htmltools::renderTags(pkg_fn("app_ui")(NULL))[["html"]])

  testthat::expect_match(html, 'id="mymap"', fixed = TRUE)
  testthat::expect_match(html, "height:500px", fixed = TRUE)
})

testthat::test_that("About panel uses explanatory-notes copy", {
  html <- as.character(htmltools::renderTags(pkg_fn("app_ui")(NULL))[["html"]])

  testthat::expect_match(html, "World and regional results are calculated by aggregating country-level estimates", fixed = TRUE)
  testthat::expect_match(html, "https://childmortality.org/wp-content/uploads/2025/08/IGME_country_consultation_note_EN_2025.pdf", fixed = TRUE)
  testthat::expect_match(html, "https://childmortality.org/wp-content/uploads/2025/03/UN-IGME_Stillbirth_explanatory_notes_EN_2024.pdf", fixed = TRUE)
  testthat::expect_match(html, "https://childmortality.org/all-cause-mortality/methods", fixed = TRUE)
  testthat::expect_match(html, "Suggested citation for mortality estimates", fixed = TRUE)
  testthat::expect_match(html, "Suggested citation for stillbirth estimates", fixed = TRUE)
  testthat::expect_false(grepl("A birth-week cohort method is used", html, fixed = TRUE))
})

testthat::test_that("About panel renders IGME citation years and note URLs from release metadata", {
  html <- as.character(htmltools::renderTags(pkg_fn("get.about.panel")(
    update_string = "Last updated: test",
    IGME_YEAR = 2030L,
    IGME_SB_YEAR = 2029L,
    IGME_NOTE_URL = "https://example.org/child-mortality-note.pdf",
    IGME_SB_NOTE_URL = "https://example.org/stillbirth-note.pdf"
  ))[["html"]])

  testthat::expect_match(html, "https://example.org/child-mortality-note.pdf", fixed = TRUE)
  testthat::expect_match(html, "https://example.org/stillbirth-note.pdf", fixed = TRUE)
  testthat::expect_match(html, "Levels &amp; Trends in Child Mortality: Report 2029", fixed = TRUE)
  testthat::expect_match(html, "United Nations Children's Fund, New York, 2030.", fixed = TRUE)
  testthat::expect_match(html, "United Nations Children's Fund, New York, 2029.", fixed = TRUE)
  testthat::expect_false(grepl("IGME_country_consultation_note_EN_2029.pdf", html, fixed = TRUE))
  testthat::expect_false(grepl("UN-IGME_Stillbirth_explanatory_notes_EN_2028.pdf", html, fixed = TRUE))
})
