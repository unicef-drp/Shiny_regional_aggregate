old_wd <- setwd(testthat::test_path("..", ".."))
on.exit(setwd(old_wd), add = TRUE)
pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)

testthat::test_that("table_to_long_download standardizes total sex labels", {
  dt <- data.table::data.table(
    Region = "Group A",
    Year = 1990,
    Sex = "Both",
    `Under-five Mortality Rate` = 10
  )

  out <- table_to_long_download(dt)

  testthat::expect_identical(names(out), c("Region", "Shortind", "Sex", "Year", "Median"))
  testthat::expect_identical(out$Sex[[1]], "Total")
  testthat::expect_identical(out$Shortind[[1]], "Under-five Mortality Rate")
})

testthat::test_that("build_long_download joins region codes when provided", {
  results <- list(
    both = data.table::data.table(Region = "Group A", Year = 1990, Sex = "Both", `Under-five Mortality Rate` = 10),
    f = data.table::data.table(Region = "Group A", Year = 1990, Sex = "Female", `Under-five Mortality Rate` = 9),
    m = data.table::data.table(Region = "Group A", Year = 1990, Sex = "Male", `Under-five Mortality Rate` = 11),
    both_5_24 = NULL,
    f_5_24 = NULL,
    m_5_24 = NULL
  )

  out <- build_long_download(
    results = results,
    region_code_lookup = data.table::data.table(Region = "Group A", Region_Code = "GA")
  )

  testthat::expect_identical(names(out), c("Region", "Region_Code", "Shortind", "Sex", "Year", "Median"))
  testthat::expect_identical(unique(out$Region_Code), "GA")
})
