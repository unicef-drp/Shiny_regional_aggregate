testthat::test_that("sex-specific chart uses explicit male and female colors", {
  empty_country_results <- data.table::data.table(Region = character(), Year = integer(), Sex = character())
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
  aggregate_results <- list(
    both = data.table::data.table(
      Region = rep(c("Selected Countries", "World"), each = 2L),
      Year = rep(c(1990L, 2000L), 2L),
      Sex = "Total",
      `U5MR median` = c(10, 9, 50, 40),
      `IMR median` = c(7, 6, 30, 20)
    ),
    f = data.table::data.table(
      Region = rep(c("Selected Countries", "World"), each = 2L),
      Year = rep(c(1990L, 2000L), 2L),
      Sex = "Female",
      `U5MR median` = c(9, 8, 45, 35),
      `IMR median` = c(6, 5, 25, 15)
    ),
    m = data.table::data.table(
      Region = rep(c("Selected Countries", "World"), each = 2L),
      Year = rep(c(1990L, 2000L), 2L),
      Sex = "Male",
      `U5MR median` = c(11, 10, 55, 45),
      `IMR median` = c(8, 7, 35, 25)
    ),
    both_5_24 = NULL,
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
      run_gender = TRUE,
      run_older_total = FALSE,
      show_world = TRUE
    )
    session$setInputs(click_run = 1)

    panel_html <- paste(as.character(output$panel_plot_rate_gender), collapse = "\n")
    testthat::expect_match(panel_html, 'data-value="Selected Countries"', fixed = TRUE)
    testthat::expect_match(panel_html, 'data-value="World"', fixed = TRUE)
    testthat::expect_match(panel_html, 'id="plot_rate_gender"', fixed = TRUE)
    testthat::expect_match(panel_html, "width:80%;", fixed = TRUE)
    testthat::expect_match(panel_html, "height:416px;", fixed = TRUE)

    session$setInputs(sex_region_tab = "World")
    plot_json <- as.character(output$plot_rate_gender)
    testthat::expect_match(plot_json, "Under-five Mortality Rate", fixed = TRUE)
    testthat::expect_match(plot_json, "Infant Mortality Rate", fixed = TRUE)
    testthat::expect_match(plot_json, "Rate: 55", fixed = TRUE)
    testthat::expect_false(grepl("Rate: 11", plot_json, fixed = TRUE))
    testthat::expect_true(
      grepl('"color":"rgba\\(0,88,171,1\\)".*?"name":"Male"', plot_json, perl = TRUE)
    )
    testthat::expect_true(
      grepl('"color":"rgba\\(226,35,26,1\\)".*?"name":"Female"', plot_json, perl = TRUE)
    )
    testthat::expect_match(plot_json, "Sex: Male", fixed = TRUE)
    testthat::expect_match(plot_json, "Sex: Female", fixed = TRUE)
  })
})
