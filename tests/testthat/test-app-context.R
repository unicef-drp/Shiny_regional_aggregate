testthat::test_that("build_app_context loads packaged app metadata", {
  ctx <- pkg_fn("build_app_context")()

  testthat::expect_true(data.table::is.data.table(ctx$dc))
  testthat::expect_true(length(ctx$countries) > 0)
  testthat::expect_true(length(ctx$ISOs) > 0)
  testthat::expect_true(inherits(ctx$world_map, "SpatialPolygonsDataFrame"))
  testthat::expect_true("update_string" %in% names(ctx))
  testthat::expect_identical(ctx$WPP_Year, pkg_fn("release_metadata_defaults")()$WPP_Year)
})

testthat::test_that("release metadata uses WPP year from annual update globals", {
  old_exists <- exists("WPP_Year", envir = globalenv(), inherits = FALSE)
  old_value <- if (old_exists) get("WPP_Year", envir = globalenv()) else NULL
  withr::defer({
    if (old_exists) {
      assign("WPP_Year", old_value, envir = globalenv())
    } else if (exists("WPP_Year", envir = globalenv(), inherits = FALSE)) {
      rm(WPP_Year, envir = globalenv())
    }
  })

  assign("WPP_Year", 2099L, envir = globalenv())

  meta <- pkg_fn("release_metadata")()

  testthat::expect_identical(meta$WPP_Year, 2099L)
  testthat::expect_identical(meta$population_file_male, "data_male_CMEpopulation.WPP2099.csv")
  testthat::expect_identical(meta$population_file_female, "data_female_CMEpopulation.WPP2099.csv")
  testthat::expect_identical(meta$population_file_male_10q5, "data_male_CME_WPP2099_10q5.csv")
  testthat::expect_identical(meta$population_file_female_10q5, "data_female_CME_WPP2099_10q5.csv")
  testthat::expect_identical(meta$population_file_male_10q15, "data_male_CME_WPP2099_10q15.csv")
  testthat::expect_identical(meta$population_file_female_10q15, "data_female_CME_WPP2099_10q15.csv")
})

testthat::test_that("annual update script gets WPP year from release metadata", {
  repo_root <- testthat::test_path("..", "..")
  update_script <- file.path(repo_root, "update_me_every_year.R")
  update_lines <- readLines(update_script, warn = FALSE)
  literal_metadata_assignments <- c(
    "^\\s*WPP_Year\\s*<-\\s*[0-9]+L?\\s*$",
    "^\\s*year_started\\s*<-\\s*[0-9]+L?\\b",
    "^\\s*runname\\.[A-Za-z0-9_]+\\s*<-\\s*\"",
    "^\\s*file_name_[A-Za-z0-9_]+\\s*<-\\s*\"",
    "^\\s*dir_stillbirth_aggregate_results\\s*<-\\s*file\\.path\\(",
    "^\\s*file_name_stillbirth_[A-Za-z0-9_]+\\s*<-\\s*\""
  )

  testthat::expect_true(any(grepl("R/fct_release_metadata.R", update_lines, fixed = TRUE)))
  testthat::expect_true(any(grepl("only place to update", update_lines, ignore.case = TRUE)))
  testthat::expect_false(any(vapply(
    literal_metadata_assignments,
    function(pattern) any(grepl(pattern, update_lines)),
    logical(1)
  )))

  env <- new.env(parent = globalenv())
  sys.source(update_script, envir = env)
  meta <- pkg_fn("release_metadata")()
  legacy_names <- c(
    "WPP_Year",
    "year_started",
    "runname.U5MR",
    "runname.IMR",
    "runname.NMR",
    "file_name_NMR",
    "file_name_total",
    "file_name_female",
    "file_name_male",
    "file_name_total_5_24",
    "file_name_female_5_24",
    "file_name_male_5_24",
    "dir_extdata",
    "dir_input",
    "dir_output",
    "dir_examples",
    "dir_www",
    "dir_median_total",
    "dir_median_female",
    "dir_median_male",
    "dir_median_total_5_14",
    "dir_median_female_5_14",
    "dir_median_male_5_14",
    "dir_median_total_15_24",
    "dir_median_female_15_24",
    "dir_median_male_15_24",
    "dir_stillbirth_aggregate_results",
    "file_name_stillbirth_country_results",
    "file_name_stillbirth_country_medians"
  )

  testthat::expect_identical(env$WPP_Year, meta$WPP_Year)
  testthat::expect_identical(env$update_string0, meta$update_string)
  for (name in legacy_names) {
    testthat::expect_identical(env[[name]], meta[[name]], info = name)
  }
})

testthat::test_that("build_app_context excludes countries unavailable for app aggregates", {
  ctx <- pkg_fn("build_app_context")(force = TRUE)

  testthat::expect_false("LIE" %in% ctx$ISOs)
  testthat::expect_false("Liechtenstein" %in% ctx$countries)
  testthat::expect_false(any(ctx$dc$ISO3Code == "LIE"))
  testthat::expect_false(any(vapply(
    ctx$input_country_list,
    function(countries) "Liechtenstein" %in% countries,
    logical(1)
  )))
})
