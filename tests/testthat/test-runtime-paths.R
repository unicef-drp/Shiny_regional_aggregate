repo_root <- testthat::test_path("..", "..")

testthat::test_that("release data root contains required directories", {
  root <- pkg_fn("release_root")()
  testthat::expect_false(grepl("release-2026", root, fixed = TRUE))
  testthat::expect_true(dir.exists(file.path(root, "input")))
  testthat::expect_true(dir.exists(file.path(root, "output")))
  testthat::expect_true(dir.exists(file.path(root, "median_results_total")))
})

testthat::test_that("packaged AU example lives under the flattened extdata tree", {
  root <- pkg_fn("release_root")()
  testthat::expect_true(file.exists(file.path(root, "examples", "AU.csv")))
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

testthat::test_that("WPP input file names are derived from release metadata", {
  r_files <- c(
    file.path(repo_root, "R", "6outputaggregates_gender.R"),
    file.path(repo_root, "update", "2.Create M49 regions and initiate files for app.R")
  )
  hits <- vapply(
    r_files,
    function(path) any(grepl("WPP20[0-9]{2}", readLines(path, warn = FALSE))),
    logical(1)
  )

  testthat::expect_false(any(hits), info = paste(r_files[hits], collapse = "\n"))
})

testthat::test_that("annual update scripts explicitly load release metadata", {
  update_files <- file.path(
    repo_root,
    "update",
    c("1.Update all input data.R", "2.Create M49 regions and initiate files for app.R")
  )

  for (path in update_files) {
    lines <- readLines(path, warn = FALSE)

    testthat::expect_true(
      any(grepl("release_meta <- release_metadata\\(\\)", lines)),
      info = path
    )
    testthat::expect_true(
      any(grepl("list2env\\(release_meta, envir = environment\\(\\)\\)", lines)),
      info = path
    )
  }
})
