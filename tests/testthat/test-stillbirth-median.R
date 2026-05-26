testthat::test_that("aggregate_stillbirth_medians combines selected countries using SB and LB medians", {
  region_iso <- data.table::data.table(
    Region = c("Group A", "Group A", "Group B"),
    ISO3Code = c("AAA", "BBB", "BBB")
  )
  stillbirth_country <- data.table::data.table(
    ISO3Code = rep(c("AAA", "BBB"), each = 6),
    Shortind = rep(c("SBR", "SB", "LB"), each = 2, times = 2),
    Year = rep(c(2000, 2001), times = 6),
    Median = c(10, 20, 5, 10, 500, 490, 20, 30, 20, 30, 980, 970)
  )

  out <- pkg_fn("aggregate_stillbirth_medians")(
    region_iso = region_iso,
    stillbirth_country = stillbirth_country,
    include_world = FALSE
  )

  row_2000 <- out[Region == "Group A" & Year == 2000]
  testthat::expect_equal(row_2000$Stillbirths, 25)
  testthat::expect_equal(row_2000$`Stillbirth rate`, 25 / (500 + 980 + 25) * 1000)
  testthat::expect_setequal(unique(out$Sex), "Total")
})

testthat::test_that("append_stillbirth_results adds stillbirth columns only to total results", {
  results <- list(
    both = data.table::data.table(
      Region = c("Group A", "Group A"),
      Year = c(2000, 2001),
      Sex = "Total",
      `Under-five Mortality Rate` = c(50, 49)
    ),
    f = data.table::data.table(Region = "Group A", Year = 2000, Sex = "Female", `Under-five Mortality Rate` = 48),
    m = data.table::data.table(Region = "Group A", Year = 2000, Sex = "Male", `Under-five Mortality Rate` = 52),
    both_5_24 = NULL,
    f_5_24 = NULL,
    m_5_24 = NULL
  )
  stillbirth_results <- data.table::data.table(
    Region = "Group A",
    Year = 2000,
    Sex = "Total",
    `Stillbirth rate` = 15,
    Stillbirths = 100
  )

  out <- pkg_fn("append_stillbirth_results")(results, stillbirth_results)

  testthat::expect_true(all(c("Stillbirth rate", "Stillbirths") %in% names(out$both)))
  testthat::expect_equal(out$both[Year == 2000]$`Stillbirth rate`, 15)
  testthat::expect_true(is.na(out$both[Year == 2001]$`Stillbirth rate`))
  testthat::expect_false("Stillbirth rate" %in% names(out$f))
  testthat::expect_false("Stillbirths" %in% names(out$m))
})

testthat::test_that("build_long_download includes appended stillbirth medians", {
  results <- list(
    both = data.table::data.table(
      Region = "Group A",
      Year = 2000,
      Sex = "Total",
      `Stillbirth rate` = 15,
      Stillbirths = 100
    ),
    f = NULL,
    m = NULL,
    both_5_24 = NULL,
    f_5_24 = NULL,
    m_5_24 = NULL
  )

  out <- pkg_fn("build_long_download")(results)

  testthat::expect_setequal(out$Shortind, c("Stillbirth rate", "Stillbirths"))
  testthat::expect_setequal(out$Median, c(15, 100))
})

testthat::test_that("stillbirth chart indicators are separated from under-five chart indicators", {
  columns <- c(
    "Under-five Deaths",
    "Infant Deaths",
    "Neonatal Deaths",
    "Stillbirths",
    "Stillbirth rate"
  )

  testthat::expect_false("Stillbirth rate" %in% pkg_fn("under_five_rate_plot_indicators")())
  testthat::expect_identical(pkg_fn("stillbirth_rate_plot_indicators")(), "Stillbirth rate")
  testthat::expect_false("Stillbirths" %in% pkg_fn("under_five_count_plot_indicators")(columns))
  testthat::expect_identical(pkg_fn("stillbirth_count_plot_indicators")(columns), "Stillbirths")
  testthat::expect_identical(pkg_fn("stillbirth_plot_column_width")(), 4L)
})
