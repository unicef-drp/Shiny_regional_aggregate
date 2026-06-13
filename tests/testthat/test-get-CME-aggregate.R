testthat::test_that("get_CME_aggregate matches the AU baseline fixture", {
  input <- read_au_input()
  expected <- read_au_fixture()

  out <- get_CME_aggregate(input)

  u5_sexes <- unique(out[
    Shortind %in% c("Under-five Mortality Rate", "Infant Mortality Rate", "Neonatal Mortality Rate"),
    Sex
  ])
  testthat::expect_setequal(u5_sexes, c("Total", "Female", "Male"))
  testthat::expect_identical(names(out), names(expected))
  testthat::expect_equal(out, expected)
})

testthat::test_that("all-country aggregate matches world and keeps world totals invariant", {
  release_path <- pkg_fn("release_path")
  country_info <- data.table::fread(release_path("input", "country.info.CME.csv"))
  all_countries <- pkg_fn("filter_app_country_info")(country_info)[
    ,
    .(Region = "All Countries", Region_Code = "ALL", ISO3Code)
  ]
  one_country <- data.table::data.table(
    Region = "One Country",
    ISO3Code = all_countries$ISO3Code[[1]]
  )

  all_country_out <- get_CME_aggregate(all_countries)
  one_country_out <- get_CME_aggregate(one_country)
  compare_cols <- c("Shortind", "Sex", "Year", "Median")
  comparable_rows <- function(dt, region) {
    dt[
      Region == region,
      ..compare_cols
    ][order(Shortind, Sex, Year)]
  }
  expect_aggregate_rows_equal <- function(actual, expected) {
    testthat::expect_equal(actual[, .(Shortind, Sex, Year)], expected[, .(Shortind, Sex, Year)])

    delta <- abs(actual$Median - expected$Median)
    count_rows <- grepl("Deaths|Stillbirths", actual$Shortind)
    max_delta <- function(x) {
      if (length(x) == 0L || all(is.na(x))) {
        return(0)
      }
      max(x, na.rm = TRUE)
    }

    # Count medians are integer-rounded in separate regional and world paths.
    count_relative_delta <- delta[count_rows] / pmax(abs(expected$Median[count_rows]), 1)
    testthat::expect_lte(max_delta(count_relative_delta), 1e-5)
    testthat::expect_lte(max_delta(delta[!count_rows]), 1e-8)
  }

  all_country_rows <- comparable_rows(all_country_out, "All Countries")
  all_country_world_rows <- comparable_rows(all_country_out, "World")
  one_country_world_rows <- comparable_rows(one_country_out, "World")

  expect_aggregate_rows_equal(all_country_rows, all_country_world_rows)
  testthat::expect_equal(all_country_world_rows, one_country_world_rows, tolerance = 1e-6)
})
