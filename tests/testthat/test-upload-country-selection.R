testthat::test_that("uploaded country picker update replaces stale selections", {
  country_lookup <- data.table::data.table(
    ISO3Code = c("AFG", "AGO", "ALB"),
    OfficialName = c("Afghanistan", "Angola", "Albania")
  )
  membership <- list(
    data = data.table::data.table(ISO3Code = c("AGO", "AFG"))
  )
  all_choices <- c("Afghanistan", "Albania", "Angola")

  update <- pkg_fn("build_uploaded_country_picker_update")(
    country_lookup = country_lookup,
    membership = membership,
    choices = all_choices
  )

  testthat::expect_identical(update$choices, all_choices)
  testthat::expect_setequal(update$selected, c("Afghanistan", "Angola"))
  testthat::expect_false("Albania" %in% update$selected)
})
