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
