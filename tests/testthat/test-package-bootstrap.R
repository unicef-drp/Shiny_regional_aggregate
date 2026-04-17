old_wd <- setwd(testthat::test_path("..", ".."))
on.exit(setwd(old_wd), add = TRUE)

testthat::test_that("package skeleton files exist", {
  testthat::expect_true(file.exists("DESCRIPTION"))
  testthat::expect_true(file.exists("R/run_app.R"))
  testthat::expect_true(file.exists("R/app_ui.R"))
  testthat::expect_true(file.exists("R/app_server.R"))
  testthat::expect_true(file.exists("tests/testthat.R"))
})

testthat::test_that("DESCRIPTION declares the package name", {
  desc <- read.dcf("DESCRIPTION")
  testthat::expect_identical(unname(desc[1, "Package"]), "shinyregionalaggregate")
})
