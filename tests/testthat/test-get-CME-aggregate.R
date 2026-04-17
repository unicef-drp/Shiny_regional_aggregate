testthat::test_that("get_CME_aggregate matches the AU baseline fixture", {
  input <- read_au_input()
  expected <- read_au_fixture()

  out <- get_CME_aggregate(input)

  testthat::expect_identical(names(out), names(expected))
  testthat::expect_equal(out, expected)
})
