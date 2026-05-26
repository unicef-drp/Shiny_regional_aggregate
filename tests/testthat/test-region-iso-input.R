testthat::test_that("normalize_region_iso_input keeps the required columns from AU.csv", {
  out <- pkg_fn("normalize_region_iso_input")(read_au_input())

  testthat::expect_true(all(c("Region", "ISO3Code") %in% names(out)))
  testthat::expect_true("Region_Code" %in% names(out))
  testthat::expect_true(all(out$ISO3Code != ""))
})

testthat::test_that("build_region_membership_wide converts long input to AdhocCountries columns", {
  raw <- data.table::data.table(
    Region = c("Group A", "Group B", "Group B"),
    ISO3Code = c("AFG", "AFG", "AGO")
  )

  wide <- pkg_fn("build_region_membership_wide")(raw)

  testthat::expect_true(all(c("ISO3Code", "AdhocCountries", "AdhocCountries2") %in% names(wide$data)))
  testthat::expect_identical(wide$data[ISO3Code == "AFG", AdhocCountries][[1]], "Group A")
  testthat::expect_identical(wide$data[ISO3Code == "AFG", AdhocCountries2][[1]], "Group B")
})

testthat::test_that("WB multi-region upload can be summarized by uploaded regions", {
  raw <- pkg_fn("read_region_iso_file")(
    testthat::test_path("..", "..", "inst", "extdata", "examples", "Upload_ISO_example_WB.csv")
  )
  membership <- pkg_fn("build_region_membership_wide")(raw)
  region_structure <- c(membership, list(normalized = raw))
  country_lookup <- unique(raw[, .(ISO3Code, OfficialName)])

  grouped <- pkg_fn("build_grouped_country_summary_html")(
    selected_countries = unique(raw$OfficialName),
    region_structure = region_structure,
    country_lookup = country_lookup
  )

  testthat::expect_match(grouped, "<strong>High income:</strong>", fixed = TRUE)
  testthat::expect_match(grouped, "<strong>Low and middle income:</strong>", fixed = TRUE)
  testthat::expect_match(grouped, "<strong>Low income:</strong>", fixed = TRUE)
  testthat::expect_match(grouped, "Afghanistan", fixed = TRUE)
  testthat::expect_match(grouped, "<br>", fixed = TRUE)
})

testthat::test_that("grouped country summary can resolve selected names from the uploaded file", {
  raw <- data.table::data.table(
    Region = c("Group A", "Group B"),
    ISO3Code = c("AFG", "AGO"),
    OfficialName = c("Uploaded Afghanistan", "Uploaded Angola")
  )
  membership <- pkg_fn("build_region_membership_wide")(raw)
  region_structure <- c(membership, list(normalized = raw))
  country_lookup <- data.table::data.table(
    ISO3Code = c("AFG", "AGO"),
    OfficialName = c("App Afghanistan", "App Angola")
  )

  grouped <- pkg_fn("build_grouped_country_summary_html")(
    selected_countries = raw$OfficialName,
    region_structure = region_structure,
    country_lookup = country_lookup
  )

  testthat::expect_match(grouped, "<strong>Group A:</strong> App Afghanistan", fixed = TRUE)
  testthat::expect_match(grouped, "<strong>Group B:</strong> App Angola", fixed = TRUE)
  testthat::expect_false(grepl("Ungrouped", grouped, fixed = TRUE))
})
