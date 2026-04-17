repo_root <- testthat::test_path("..", "..")

testthat::test_that("release data root contains required directories", {
  root <- pkg_fn("release_root")()
  testthat::expect_true(dir.exists(file.path(root, "input")))
  testthat::expect_true(dir.exists(file.path(root, "output")))
  testthat::expect_true(dir.exists(file.path(root, "median_results_total")))
})

testthat::test_that("runtime workspace is seeded from packaged data", {
  workspace <- pkg_fn("create_runtime_workspace")()
  on.exit(unlink(workspace, recursive = TRUE, force = TRUE), add = TRUE)

  testthat::expect_true(dir.exists(file.path(workspace, "input")))
  testthat::expect_true(file.exists(file.path(workspace, "input", "country.info.CME.csv")))
  testthat::expect_true(dir.exists(file.path(workspace, "median_results_total")))
  testthat::expect_true(file.exists(file.path(workspace, "R", "chooseregion.R")))
})

testthat::test_that("repository R files no longer use here::here", {
  r_files <- list.files(
    c(file.path(repo_root, "R"), file.path(repo_root, "update")),
    pattern = "\\.[Rr]$",
    full.names = TRUE,
    recursive = TRUE
  )
  r_files <- c(r_files, file.path(repo_root, "app.R"), file.path(repo_root, "update_me_every_year.R"))
  r_files <- r_files[file.exists(r_files)]
  hits <- vapply(
    r_files,
    function(path) any(grepl("here::here\\(", readLines(path, warn = FALSE), fixed = FALSE)),
    logical(1)
  )

  testthat::expect_false(any(hits), info = paste(r_files[hits], collapse = "\n"))
})
