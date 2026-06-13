write_test_replacement_cache <- function(output_dir, lie_value = NA_real_, include_neonatal = FALSE) {
  samples_dir <- file.path(output_dir, "samples_combined")
  dir.create(samples_dir, recursive = TRUE, showWarnings = FALSE)

  iso <- c("AFG", "LIE")
  info <- list(iso.c = iso)
  save(info, file = file.path(samples_dir, "info.rda"))

  values <- c(10, lie_value)
  replacement_array <- array(values, dim = c(2, 1, 1), dimnames = list(iso, "2024", NULL))
  for (object_name in c("death0.ctj", "death1to4.ctj", "deathu5.ctj")) {
    assign(object_name, replacement_array)
    save(list = object_name, file = file.path(samples_dir, paste0(object_name, ".M49Region-replace.rda")))
  }
  if (isTRUE(include_neonatal)) {
    deathnn.ctj <- replacement_array
    save(deathnn.ctj, file = file.path(samples_dir, "deathnn.ctj.M49Region-replace.rda"))
  }
}

testthat::test_that("sex-specific replacement cache does not require neonatal cache", {
  output_dir <- tempfile("aggregate-cache-")
  write_test_replacement_cache(output_dir, include_neonatal = FALSE)

  has_cache <- pkg_fn("has_replacement_country_cache")

  testthat::expect_true(has_cache(output_dir, nn.exists = FALSE))
  testthat::expect_false(has_cache(output_dir, nn.exists = TRUE))

  write_test_replacement_cache(output_dir, include_neonatal = TRUE)

  testthat::expect_true(has_cache(output_dir, nn.exists = TRUE))
})

testthat::test_that("replacement cache is rejected when excluded countries have deaths", {
  output_dir <- tempfile("aggregate-cache-")
  write_test_replacement_cache(output_dir, lie_value = 1.6, include_neonatal = TRUE)

  has_cache <- pkg_fn("has_replacement_country_cache")

  testthat::expect_false(has_cache(output_dir, nn.exists = TRUE))
})
