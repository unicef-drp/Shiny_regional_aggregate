read_au_input <- function() {
  data.table::fread(testthat::test_path("fixtures", "au_input.csv"))
}

read_au_fixture <- function() {
  data.table::fread(testthat::test_path("fixtures", "au_expected_long.csv"))
}
