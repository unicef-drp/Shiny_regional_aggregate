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
