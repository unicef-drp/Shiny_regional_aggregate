old_wd <- setwd(testthat::test_path("..", ".."))
on.exit(setwd(old_wd), add = TRUE)
pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)

testthat::test_that("release data root contains required directories", {
  root <- release_root()
  testthat::expect_true(dir.exists(file.path(root, "input")))
  testthat::expect_true(dir.exists(file.path(root, "output")))
  testthat::expect_true(dir.exists(file.path(root, "median_results_total")))
})

testthat::test_that("runtime workspace is seeded from packaged data", {
  workspace <- create_runtime_workspace()
  on.exit(unlink(workspace, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::expect_true(dir.exists(file.path(workspace, "input")))
  testthat::expect_true(file.exists(file.path(workspace, "input", "country.info.CME.csv")))
  testthat::expect_true(dir.exists(file.path(workspace, "median_results_total")))
})

testthat::test_that("repository R files no longer use here::here", {
  r_files <- list.files(c("R", "update"), pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
  r_files <- c(r_files, "app.R", "update_me_every_year.R")
  hits <- vapply(
    r_files,
    function(path) any(grepl("here::here\\(", readLines(path, warn = FALSE), fixed = FALSE)),
    logical(1)
  )

  testthat::expect_false(any(hits), info = paste(r_files[hits], collapse = "\n"))
})
