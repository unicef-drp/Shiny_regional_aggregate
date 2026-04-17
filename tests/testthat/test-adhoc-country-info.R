testthat::test_that("write_adhoc_country_info creates all three adhoc CSV files", {
  workspace <- pkg_fn("create_runtime_workspace")()
  on.exit(unlink(workspace, recursive = TRUE, force = TRUE), add = TRUE)

  region_iso <- pkg_fn("normalize_region_iso_input")(read_au_input())
  pkg_fn("write_adhoc_country_info")(region_iso, workspace)

  testthat::expect_true(file.exists(file.path(workspace, "input", "country.info.CME_adhoc.csv")))
  testthat::expect_true(file.exists(file.path(workspace, "input", "country.info.CME.5_14_adhoc.csv")))
  testthat::expect_true(file.exists(file.path(workspace, "input", "country.info.CME.15_24_adhoc.csv")))
})
