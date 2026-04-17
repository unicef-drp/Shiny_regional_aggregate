old_wd <- setwd(testthat::test_path("..", ".."))
on.exit(setwd(old_wd), add = TRUE)
pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)

testthat::test_that("write_adhoc_country_info creates all three adhoc CSV files", {
  workspace <- create_runtime_workspace()
  on.exit(unlink(workspace, recursive = TRUE, force = TRUE), add = TRUE)

  region_iso <- normalize_region_iso_input(data.table::fread("AU.csv"))
  write_adhoc_country_info(region_iso, workspace)

  testthat::expect_true(file.exists(file.path(workspace, "input", "country.info.CME_adhoc.csv")))
  testthat::expect_true(file.exists(file.path(workspace, "input", "country.info.CME.5_14_adhoc.csv")))
  testthat::expect_true(file.exists(file.path(workspace, "input", "country.info.CME.15_24_adhoc.csv")))
})
