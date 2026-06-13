testthat::test_that("plot panels show loading text while plotly outputs render", {
  empty_country_results <- data.table::data.table(Region = character(), Year = integer(), Sex = character())
  aggregate_results <- list(
    both = data.table::data.table(
      Region = "Selected Countries",
      Year = c(1990L, 2000L),
      Sex = "Total",
      `U5MR median` = c(10, 9),
      `IMR median` = c(7, 6),
      `NMR median` = c(4, 3),
      `Stillbirth rate` = c(12, 11),
      `Under-five deaths median` = c(100, 90),
      `Infant deaths median` = c(40, 35),
      `Neonatal deaths median` = c(25, 20),
      Stillbirths = c(20, 18)
    ),
    f = empty_country_results,
    m = empty_country_results,
    both_5_24 = NULL,
    f_5_24 = NULL,
    m_5_24 = NULL,
    region_code_lookup = NULL
  )
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
    col_order_older_children_all_rate = character()
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
      run_older_total = FALSE,
      show_world = FALSE
    )
    session$setInputs(click_run = 1)

    panel_html <- paste(as.character(output$panel_plot_rate), collapse = "\n")
    testthat::expect_equal(length(gregexpr("Loading plots...", panel_html, fixed = TRUE)[[1]]), 2L)
    for (plot_id in c("plot_rate", "plot_death")) {
      testthat::expect_match(panel_html, paste0('id="', plot_id, '"'), fixed = TRUE)
    }
    testthat::expect_match(panel_html, "height:620px;", fixed = TRUE)
    testthat::expect_false(grepl('id="plot_stillbirth_rate"', panel_html, fixed = TRUE))
    testthat::expect_false(grepl('id="plot_stillbirth_count"', panel_html, fixed = TRUE))

    rate_plot <- as.character(output$plot_rate)
    for (indicator in c(
      "Under-five Mortality Rate",
      "Infant Mortality Rate",
      "Neonatal Mortality Rate",
      "Stillbirth Rate"
    )) {
      testthat::expect_match(rate_plot, indicator, fixed = TRUE)
    }

    death_plot <- as.character(output$plot_death)
    for (indicator in c(
      "Under-five Deaths",
      "Infant Deaths",
      "Neonatal Deaths",
      "Stillbirths"
    )) {
      testthat::expect_match(death_plot, indicator, fixed = TRUE)
    }
  })
})
