testthat::test_that("aggregate results can be expanded into app download payloads", {
  region_iso <- read_au_input()
  results <- pkg_fn("get_CME_aggregate_results")(region_iso)

  testthat::expect_true(is.list(results))
  testthat::expect_true(all(c("both", "f", "m", "both_5_24", "f_5_24", "m_5_24") %in% names(results)))
})
