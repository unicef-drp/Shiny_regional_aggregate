testthat::test_that("clean.table title-cases Stillbirth Rate for display", {
  dt <- data.table::data.table(
    Region = "Group A",
    Year = 2024,
    Sex = "Total",
    `Stillbirth rate` = 12.3
  )

  out <- pkg_fn("clean.table")(dt)

  testthat::expect_true("Stillbirth Rate" %in% names(out))
  testthat::expect_false("Stillbirth rate" %in% names(out))
})

testthat::test_that("clean_older_children_table groups mortality rates before deaths", {
  dt <- data.table::data.table(
    Region = "Group A",
    Year = 2024,
    Sex = "Total",
    `Deaths age 5 to 24` = 240,
    `Mortality rate age 15-24` = 5.4,
    `Deaths age 10 to 14` = 60,
    `Mortality rate age 5-14` = 2.4,
    `Deaths age 20 to 24` = 90,
    `Mortality rate age 5-24` = 7.4,
    `Deaths age 5 to 9` = 50,
    `Mortality rate age 10-19` = 4.4,
    `Deaths age 15 to 24` = 130,
    `Mortality rate age 15-19` = 3.4,
    `Deaths age 10 to 19` = 110,
    `Mortality rate age 10-14` = 1.4,
    `Deaths age 15 to 19` = 40,
    `Mortality rate age 20-24` = 2.4,
    `Deaths age 5 to 14` = 100,
    `Mortality rate age 5-9` = 1.1
  )

  out <- pkg_fn("clean_older_children_table")(dt)

  testthat::expect_identical(
    names(out),
    c(
      "Region",
      "Year",
      "Sex",
      "Mortality rate age 5-9",
      "Mortality rate age 10-14",
      "Mortality rate age 5-14",
      "Mortality rate age 15-19",
      "Mortality rate age 20-24",
      "Mortality rate age 15-24",
      "Mortality rate age 10-19",
      "Mortality rate age 5-24",
      "Deaths age 5 to 9",
      "Deaths age 10 to 14",
      "Deaths age 5 to 14",
      "Deaths age 15 to 19",
      "Deaths age 20 to 24",
      "Deaths age 15 to 24",
      "Deaths age 10 to 19",
      "Deaths age 5 to 24"
    )
  )
})
