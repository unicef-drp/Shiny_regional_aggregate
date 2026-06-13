older_children_plot_test_data <- function() {
  columns <- c(
    "Mortality rate age 5-24",
    "Mortality rate age 10-19",
    "Mortality rate age 5-14",
    "Mortality rate age 5-9",
    "Mortality rate age 10-14",
    "Mortality rate age 15-24",
    "Mortality rate age 15-19",
    "Mortality rate age 20-24",
    "Deaths age 5 to 24",
    "Deaths age 10 to 19",
    "Deaths age 5 to 14",
    "Deaths age 5 to 9",
    "Deaths age 10 to 14",
    "Deaths age 15 to 24",
    "Deaths age 15 to 19",
    "Deaths age 20 to 24"
  )

  out <- data.table::data.table(Region = "Adhoc", Year = c(1990L, 2000L), Sex = "Total")
  for (column in columns) {
    out[, (column) := c(100, 90)]
  }
  out
}

testthat::test_that("optional result checkboxes are selected by default", {
  html <- htmltools::renderTags(pkg_fn("app_ui")(NULL))$html
  sex_checkbox <- regmatches(html, regexpr('<input[^>]+id="run_gender"[^>]*>', html, perl = TRUE))
  older_checkbox <- regmatches(html, regexpr('<input[^>]+id="run_older_total"[^>]*>', html, perl = TRUE))

  testthat::expect_match(sex_checkbox, "checked", fixed = TRUE)
  testthat::expect_match(older_checkbox, "checked", fixed = TRUE)
})

testthat::test_that("older children plot panel combines six rates and deaths into shared plots", {
  empty_country_results <- data.table::data.table(Region = character(), Year = integer(), Sex = character())
  older_results <- older_children_plot_test_data()
  test_context <- list(
    countries = c("Afghanistan"),
    default_select = "Afghanistan",
    adhoc_name = "Selected Countries",
    year_started = 1990L,
    year_ended = 2024L,
    dc = data.table::data.table(ISO3Code = "AFG", OfficialName = "Afghanistan"),
    c_median_total = empty_country_results,
    c_median_f = empty_country_results,
    c_median_m = empty_country_results,
    c_median_total_older = empty_country_results,
    c_median_f_older = empty_country_results,
    c_median_m_older = empty_country_results,
    col_order_older_children_all_rate = grep("Mortality rate", names(older_results), value = TRUE)
  )
  aggregate_results <- list(
    both = data.table::data.table(Region = "Adhoc", Year = c(1990L, 2000L), Sex = "Total"),
    f = NULL,
    m = NULL,
    both_5_24 = older_results,
    f_5_24 = NULL,
    m_5_24 = NULL,
    region_code_lookup = NULL
  )

  testthat::local_mocked_bindings(
    build_app_context = function(force = FALSE) test_context,
    get_CME_aggregate_results = function(region_input) aggregate_results,
    .package = "shinyregionalaggregate"
  )

  shiny::testServer(pkg_fn("app_server"), {
    session$setInputs(
      country_input_select = "Afghanistan",
      run_gender = FALSE,
      run_older_total = TRUE,
      show_world = FALSE
    )
    session$setInputs(click_run = 1)

    panel_html <- as.character(output$panel_plot_older_children)
    plot_ids <- regmatches(panel_html, gregexpr('id="plot_[^"]+"', panel_html, perl = TRUE))[[1]]
    testthat::expect_identical(
      plot_ids,
      paste0(
        'id="',
        c(
          "plot_rate_older",
          "plot_death_older"
        ),
        '"'
      )
    )

    rate_plot <- as.character(output$plot_rate_older)
    death_plot <- as.character(output$plot_death_older)

    for (indicator in c(
      "Mortality rate age 5-9",
      "Mortality rate age 10-14",
      "Mortality rate age 15-19",
      "Mortality rate age 20-24",
      "Mortality rate age 10-19",
      "Mortality rate age 5-24"
    )) {
      testthat::expect_match(rate_plot, indicator, fixed = TRUE)
    }

    for (indicator in c(
      "Deaths age 5 to 9",
      "Deaths age 10 to 14",
      "Deaths age 15 to 19",
      "Deaths age 20 to 24",
      "Deaths age 10 to 19",
      "Deaths age 5 to 24"
    )) {
      testthat::expect_match(death_plot, indicator, fixed = TRUE)
    }

    testthat::expect_false(grepl("Mortality rate age 5-14", rate_plot, fixed = TRUE))
    testthat::expect_false(grepl("Mortality rate age 15-24", rate_plot, fixed = TRUE))
    testthat::expect_false(grepl("Deaths age 5 to 14", death_plot, fixed = TRUE))
    testthat::expect_false(grepl("Deaths age 15 to 24", death_plot, fixed = TRUE))
  })
})
