testthat::test_that("reset clears previously computed aggregate results", {
  empty_country_results <- data.table::data.table(Region = character(), Year = integer(), Sex = character())
  test_context <- list(
    countries = c("Afghanistan", "Albania"),
    default_select = "Afghanistan",
    adhoc_name = "Selected Countries",
    dc = data.table::data.table(
      ISO3Code = c("AFG", "ALB"),
      OfficialName = c("Afghanistan", "Albania")
    ),
    c_median_total = empty_country_results,
    c_median_f = empty_country_results,
    c_median_m = empty_country_results,
    c_median_total_older = empty_country_results,
    c_median_f_older = empty_country_results,
    c_median_m_older = empty_country_results
  )
  aggregate_results <- list(
    both = data.table::data.table(Region = "Adhoc", Year = 2024, Sex = "Total"),
    f = NULL,
    m = NULL,
    both_5_24 = NULL,
    f_5_24 = NULL,
    m_5_24 = NULL,
    region_code_lookup = NULL
  )
  checkbox_updates <- list()

  testthat::local_mocked_bindings(
    build_app_context = function(force = FALSE) test_context,
    get_CME_aggregate_results = function(region_input) aggregate_results,
    updateCheckboxInput = function(session, inputId, value = NULL, ...) {
      checkbox_updates[[inputId]] <<- value
    },
    .package = "shinyregionalaggregate"
  )

  shiny::testServer(pkg_fn("app_server"), {
    session$setInputs(
      country_input_select = "Afghanistan",
      run_gender = FALSE,
      run_older_total = FALSE
    )
    session$setInputs(click_run = 1)
    testthat::expect_false(is.null(reactive.run()))

    session$setInputs(click_reset = 1)
    testthat::expect_null(reactive.run())
    testthat::expect_identical(checkbox_updates$run_gender, TRUE)
    testthat::expect_identical(checkbox_updates$run_older_total, TRUE)
  })
})
