repo_root <- testthat::test_path("..", "..")
testthat::skip_if_not(
  file.exists(file.path(repo_root, "DESCRIPTION")),
  message = "Repository scaffold checks only run against the source tree."
)

testthat::test_that("package skeleton files exist", {
  testthat::expect_true(file.exists(file.path(repo_root, "DESCRIPTION")))
  testthat::expect_true(file.exists(file.path(repo_root, "R", "run_app.R")))
  testthat::expect_true(file.exists(file.path(repo_root, "R", "app_ui.R")))
  testthat::expect_true(file.exists(file.path(repo_root, "R", "app_server.R")))
  testthat::expect_true(file.exists(file.path(repo_root, "tests", "testthat.R")))
})

testthat::test_that("DESCRIPTION declares the package name", {
  desc <- read.dcf(file.path(repo_root, "DESCRIPTION"))
  testthat::expect_identical(unname(desc[1, "Package"]), "shinyregionalaggregate")
})
