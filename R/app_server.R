#' Application server
#' @noRd
app_server <- function(input, output, session) {
  ctx <- build_app_context()
  panel_title1.1 <- "Results of selected regional aggregates for"
  panel_title1.2 <- "Sex-specific results for selected regional aggregates"
  panel_title1.3 <- "Older children and adolescents results for selected regional aggregates"
  panel_title2.1 <- "Table of selected regional aggregates"
  panel_title2.2 <- "Sex-specific results for selected regional aggregates"
  panel_title2.3 <- "Older children and adolescents results for selected regional aggregates"
  panel_title2.4 <- "Sex-specific older children and adolescents results"
  panel_note1 <- "Regional, world, and country data are available for download:"
  panel_note2 <- "Regional, world, and country data (including sex-specific results) are available for download:"

  uploaded_region_structure <- reactiveVal(NULL)
  single_group_run <- reactiveVal(TRUE)
  aggregate_results <- reactiveVal(NULL)

  rename.single.region <- function(values) {
    custom_group_name <- if (is.null(input$adhoc_name)) "" else trimws(input$adhoc_name)
    values <- as.character(values)
    regions_non_world <- unique(values[!is.na(values) & nzchar(values) & values != "World"])

    if (length(regions_non_world) != 1) {
      return(values)
    }

    current_region <- regions_non_world[1]
    replacement <- current_region
    if (identical(current_region, "Adhoc")) {
      replacement <- if (nzchar(custom_group_name)) custom_group_name else ctx$adhoc_name
    } else if (nzchar(custom_group_name)) {
      replacement <- custom_group_name
    }

    values[values == current_region] <- replacement
    values
  }

  rename_result_region <- function(dt) {
    if (is.null(dt) || !"Region" %in% colnames(dt)) {
      return(dt)
    }

    out <- data.table::copy(dt)
    out[, Region := rename.single.region(Region)]
    out[]
  }

  build_manual_region_input <- function(selected_countries) {
    unique(ctx$dc[OfficialName %in% selected_countries, .(Region = "Adhoc", ISO3Code)])
  }

  get_total_regions <- function(region_structure) {
    if (is.null(region_structure)) {
      return(0L)
    }
    data.table::uniqueN(region_structure$normalized$Region)
  }

  show_world_in_current_plots <- function() {
    should_show_world_in_plots(input$show_world, single_group_run())
  }

  sex_specific_regions <- function() {
    if (is.null(reactive.run()$m)) {
      return(character(0))
    }

    regions <- unique(as.character(reactive.run()$m$Region))
    regions <- regions[!is.na(regions) & nzchar(regions)]

    region_dt <- data.table::data.table(Region = regions)
    unique(as.character(limit.plot.regions(region_dt)$Region))
  }

  selected_sex_specific_region <- function() {
    regions <- sex_specific_regions()
    if (length(regions) == 0L) {
      return(NULL)
    }

    selected <- input$sex_region_tab
    if (is.null(selected) || !selected %in% regions) {
      selected <- regions[[1]]
    }
    selected
  }

  plot_loading_output <- function(outputId, width = "100%", height = "400px") {
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(outputId, width = width, height = height),
      caption = "Loading plots..."
    )
  }

  observeEvent(input$click_reset, {
    shinyWidgets::updatePickerInput(
      session,
      inputId = "country_input_select",
      choices = ctx$countries,
      selected = ctx$default_select
    )
    updateCheckboxInput(session, inputId = "input_by_region", value = FALSE)
    updateCheckboxInput(session, inputId = "run_gender", value = TRUE)
    updateCheckboxInput(session, inputId = "run_older_total", value = TRUE)
    uploaded_region_structure(NULL)
    single_group_run(TRUE)
    aggregate_results(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$input_by_region, {
    shinyWidgets::updatePickerInput(
      session,
      inputId = "country_input_select",
      choices = if (isTRUE(input$input_by_region)) ctx$input_country_list else ctx$countries,
      selected = input$country_input_select
    )
  }, ignoreInit = TRUE)

  output$panel_custom_group_name <- renderUI({
    if (!isTRUE(single_group_run())) {
      return(NULL)
    }

    textAreaInput(
      inputId = "adhoc_name",
      label = "(Optional) Display name for the single custom group",
      value = ctx$adhoc_name,
      rows = 1,
      placeholder = "Only changes the displayed/downloaded group name; leave blank to keep the default"
    )
  })

  observeEvent(input$ISO_input, {
    req(input$ISO_input)

    tryCatch(
      {
        normalized <- read_app_region_iso_file(input$ISO_input$datapath)
        membership <- build_region_membership_wide(normalized)

        uploaded_region_structure(c(
          membership,
          list(normalized = normalized)
        ))

        country_picker_update <- build_uploaded_country_picker_update(
          country_lookup = ctx$dc[, .(ISO3Code, OfficialName)],
          membership = membership,
          choices = ctx$countries
        )
        shinyWidgets::updatePickerInput(
          session,
          inputId = "country_input_select",
          choices = country_picker_update$choices,
          selected = country_picker_update$selected
        )

        num_regions <- data.table::uniqueN(normalized$Region)
        single_group_run(num_regions <= 1)

        showModal(modalDialog(
          title = "File uploaded successfully",
          HTML(paste0(
            "Uploaded ", nrow(membership$data), " countries<br>",
            "Region levels: ", length(membership$region_cols), "<br>",
            "Total regions: ", num_regions, "<br><br>",
            "You may click anywhere to dismiss this message"
          )),
          easyClose = TRUE
        ))
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Upload Error",
          paste("Error reading file:", conditionMessage(e)),
          easyClose = TRUE
        ))
      }
    )
  }, ignoreInit = TRUE)

  output$text_selected_countries <- renderText({
    cs <- input$country_input_select

    if (!is.null(reactive.run())) {
      regions <- unique(reactive.run()$both$Region)
      regions_non_world <- regions[regions != "World"]
      if (length(regions_non_world) > 1) {
        return(paste("The selected regions:", paste(sort(regions_non_world), collapse = ", ")))
      }
    }

    paste(length(cs), if (length(cs) == 1) "Country:" else "Countries:", paste(sort(cs), collapse = ", "))
  })

  reactive.selected.countries <- eventReactive(input$click_run, {
    input$country_input_select
  })

  output$selected_countries_click_run <- renderUI({
    cs <- reactive.selected.countries()
    region_structure <- uploaded_region_structure()
    num_regions <- if (is.null(region_structure)) 1L else get_total_regions(region_structure)
    grouped_country_html <- if (!is.null(region_structure) && num_regions > 1L) {
      build_grouped_country_summary_html(cs, region_structure, ctx$dc[, .(ISO3Code, OfficialName)])
    } else {
      NULL
    }

    if (!is.null(grouped_country_html)) {
      return(tagList(
        h5(strong(paste(
          length(cs),
          if (length(cs) == 1) "Country across" else "Countries across",
          num_regions,
          if (num_regions == 1) "region:" else "regions:"
        ))),
        h5(HTML(grouped_country_html))
      ))
    }

    h5(strong(paste(length(cs), if (length(cs) == 1) "Country:" else "Countries:", paste(sort(cs), collapse = ", "))))
  })

  output$header_selected_countries <- renderUI({
    if (!is.null(reactive.run())) {
      regions <- unique(reactive.run()$both$Region)
      regions_non_world <- regions[regions != "World"]
      if (length(regions_non_world) > 1) {
        return(h4("The selected regions:"))
      }
    }
    h4("The selected countries:")
  })

  output$mymap <- renderLeaflet({
    validate(need(input$country_input_select, "Please select countries."))

    selected_map <- build_selected_world_map(
      world_map = ctx$world_map,
      selected_countries = input$country_input_select,
      country_lookup = ctx$dc[, .(ISO3Code, OfficialName)],
      region_structure = uploaded_region_structure(),
      default_region = ctx$adhoc_name
    )
    validate(need(nrow(selected_map) > 0, "Selected countries are not available on the map."))

    build_selected_leaflet_map(selected_map)
  })

  observeEvent(input$click_run, {
    if (length(input$country_input_select) == 0) {
      showModal(modalDialog(
        title = "Please select countries first.",
        footer = "You may click anywhere to dismiss this message",
        easyClose = TRUE
      ))
      aggregate_results(NULL)
      return(invisible(NULL))
    }

    cs <- input$country_input_select
    region_structure <- uploaded_region_structure()
    region_input <- if (is.null(region_structure)) build_manual_region_input(cs) else region_structure$normalized
    num_regions <- if (is.null(region_structure)) 1L else get_total_regions(region_structure)
    flat_country_list <- paste(sort(cs), collapse = ", ")
    grouped_country_html <- if (!is.null(region_structure) && num_regions > 1L) {
      build_grouped_country_summary_html(cs, region_structure, ctx$dc[, .(ISO3Code, OfficialName)])
    } else {
      NULL
    }

    modal_country_title <- if (!is.null(grouped_country_html)) {
      paste(
        length(cs),
        if (length(cs) == 1) "country" else "countries",
        "across",
        num_regions,
        if (num_regions == 1) "region" else "regions"
      )
    } else {
      paste(length(cs), if (length(cs) == 1) "country:" else "countries:", flat_country_list)
    }

    build_modal_body <- function(seconds) {
      HTML(paste0(
        "<br>",
        if (!is.null(grouped_country_html)) paste0(grouped_country_html, "<br><br>") else "",
        "Processing ", num_regions, " region(s).",
        "<br>It takes about ", seconds, " seconds."
      ))
    }

    if (isTRUE(input$run_gender)) {
      if (isTRUE(input$run_older_total)) {
        showModal(modalDialog(
          title = paste0("Running aggregate for sex-specific under-five and older children for ", modal_country_title),
          build_modal_body(60),
          footer = NULL
        ))
      } else {
        showModal(modalDialog(
          title = paste0("Running sex-specific under-five aggregate for ", modal_country_title),
          build_modal_body(30),
          footer = NULL
        ))
      }
    } else if (isTRUE(input$run_older_total)) {
      showModal(modalDialog(
        title = paste0("Running aggregate for under-five and older children for ", modal_country_title),
        build_modal_body(30),
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = paste0("Running under-five aggregate for ", modal_country_title),
        build_modal_body(15),
        footer = NULL
      ))
    }

    on.exit(removeModal(), add = TRUE)

    results <- get_CME_aggregate_results(region_input)
    results$both <- rename_result_region(results$both)
    results$f <- rename_result_region(results$f)
    results$m <- rename_result_region(results$m)
    results$both_5_24 <- rename_result_region(results$both_5_24)
    results$f_5_24 <- rename_result_region(results$f_5_24)
    results$m_5_24 <- rename_result_region(results$m_5_24)

    if (!is.null(results$region_code_lookup) && nrow(results$region_code_lookup) > 0) {
      results$region_code_lookup <- data.table::copy(results$region_code_lookup)
      results$region_code_lookup[, Region := rename.single.region(Region)]
    }

    results$c_median_total <- ctx$c_median_total[Region %in% cs]
    results$c_median_f <- ctx$c_median_f[Region %in% cs]
    results$c_median_m <- ctx$c_median_m[Region %in% cs]
    results$c_median_total_older <- ctx$c_median_total_older[Region %in% cs]
    results$c_median_f_older <- ctx$c_median_f_older[Region %in% cs]
    results$c_median_m_older <- ctx$c_median_m_older[Region %in% cs]
    aggregate_results(results)
  }, ignoreInit = TRUE)

  reactive.run <- reactive({
    aggregate_results()
  })

  output$panel_plot_rate <- renderUI({
    if (is.null(reactive.run())) {
      return()
    }
    fluidRow(
      h4(strong(panel_title1.1)),
      uiOutput("selected_countries_click_run"),
      br(),
      checkboxInput(
        "show_world",
        "Show results for the world in plots",
        value = should_show_world_in_plots(NULL, single_group_run())
      ),
      h5(a("Download definition of indicators", href = "www/Indicator definition and unit.xlsx", target = "_blank")),
      plot_loading_output("plot_rate", height = "620px"),
      br(),
      plot_loading_output("plot_death", height = "620px"),
      br(),
      br()
    )
  })

  output$panel_plot_rate_gender <- renderUI({
    if (is.null(reactive.run()$m) || !isTRUE(input$run_gender)) {
      return()
    }

    regions <- sex_specific_regions()
    if (length(regions) == 0L) {
      return()
    }
    tabs <- lapply(
      regions,
      function(region) {
        tabPanel(title = region, value = region)
      }
    )

    fluidRow(
      h4(strong(panel_title1.2)),
      do.call(tabsetPanel, c(tabs, list(id = "sex_region_tab", selected = regions[[1]]))),
      plot_loading_output("plot_rate_gender", width = "80%", height = "416px")
    )
  })

  output$panel_plot_older_children <- renderUI({
    if (is.null(reactive.run()$both_5_24) || !isTRUE(input$run_older_total)) {
      return()
    }
    fluidRow(
      h4(strong(panel_title1.3)),
      plot_loading_output("plot_rate_older", height = "650px"),
      br(),
      plot_loading_output("plot_death_older", height = "650px"),
      br(),
      br()
    )
  })

  output$panel_results_table <- renderUI({
    if (is.null(reactive.run())) {
      return()
    }
    fluidRow(
      h4(strong(panel_title2.1)),
      if (isTRUE(input$run_gender)) p(panel_note2) else p(panel_note1),
      div(
        style = "display: flex; gap: 10px; flex-wrap: wrap;",
        downloadButton("download_table_all", "Download"),
        downloadButton("download_table_all_long", "Download all in long-format")
      ),
      br(),
      br(),
      DT::dataTableOutput("results_table", width = "80%")
    )
  })

  output$panel_results_table_gender <- renderUI({
    if (is.null(reactive.run()$m) || !isTRUE(input$run_gender)) {
      return()
    }
    fluidRow(
      h4(strong(panel_title2.2)),
      p(strong("Data for the female")),
      downloadButton("download_table_f", "Download"),
      br(),
      br(),
      DT::dataTableOutput("results_table_f", width = "80%"),
      br(),
      p(strong("Data for the male")),
      downloadButton("download_table_m", "Download"),
      br(),
      br(),
      DT::dataTableOutput("results_table_m", width = "80%")
    )
  })

  output$panel_results_table_older_total <- renderUI({
    if (is.null(reactive.run()$both_5_24) || !isTRUE(input$run_older_total)) {
      return()
    }
    fluidRow(
      h4(strong(panel_title2.3)),
      downloadButton("download_table_older_total", "Download"),
      br(),
      br(),
      DT::dataTableOutput("results_table_older_total", width = "80%")
    )
  })

  output$panel_results_table_older_gender <- renderUI({
    if (is.null(reactive.run()$both_5_24) || !isTRUE(input$run_older_total)) {
      return()
    }
    fluidRow(
      h4(strong(panel_title2.4)),
      p(strong("Data for the female")),
      downloadButton("download_table_older_f", "Download"),
      br(),
      br(),
      DT::dataTableOutput("results_table_older_f", width = "80%"),
      br(),
      p(strong("Data for the male")),
      downloadButton("download_table_older_m", "Download"),
      br(),
      br(),
      DT::dataTableOutput("results_table_older_m", width = "80%")
    )
  })

  get.long <- function(dt, vars0) {
    dt <- dt[Year >= ctx$year_started]
    dt_long <- data.table::melt(
      dt,
      measure.vars = vars0,
      value.name = "Rate",
      variable.name = "Indicator",
      variable.factor = FALSE
    )
    dt_long[, Rate := suppressWarnings(as.numeric(Rate))]
    dt_long <- dt_long[!is.na(Rate), .(Region, Year = as.integer(Year), Rate = round(Rate, 2), Indicator)]
    data.table::setorder(dt_long, Indicator, Region, Year)
    dt_long
  }

  titlefont0 <- 14
  limit.plot.regions <- function(dt, max_regions = 10L) {
    if (is.null(dt) || !"Region" %in% colnames(dt)) {
      return(dt)
    }

    region_order <- unique(as.character(dt$Region))
    non_world_regions <- region_order[region_order != "World"]
    if (length(non_world_regions) <= max_regions) {
      return(dt)
    }

    keep_regions <- non_world_regions[seq_len(max_regions)]
    if ("World" %in% region_order) {
      keep_regions <- c(keep_regions, "World")
    }
    dt[Region %in% keep_regions]
  }

  region.legend.layout <- function(regions) {
    region_count <- length(unique(as.character(regions)))
    items_per_row <- max(1L, ceiling(region_count / 2))

    list(
      orientation = "h",
      x = 0,
      xanchor = "left",
      y = -0.18,
      yanchor = "top",
      traceorder = "normal",
      entrywidthmode = "fraction",
      entrywidth = min(1, 1 / items_per_row)
    )
  }

  plot.rate <- function(dt, vars0, title0 = "Deaths per 1,000 live births", ncol = NULL) {
    dt <- limit.plot.regions(dt)
    dt_long <- get.long(dt, vars0)
    dt_long[, Indicator := RecodePlotIndicators(Indicator, indicator.order = vars0)]
    p <- ggplot2::ggplot(dt_long, ggplot2::aes(x = Year, y = Rate, color = Region, group = Region)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_bw() +
      ggplot2::labs(y = "", x = "", color = "") +
      ggplot2::facet_wrap(~ Indicator, ncol = ncol) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_color_manual(values = ResolvePlotColors(data.table::uniqueN(dt_long$Region))) +
      ggplot2::scale_x_continuous(breaks = c(1990, 2000, 2010, ctx$year_ended))

    plotly::ggplotly(p, tooltip = c("Year", "Rate")) |>
      plotly::layout(
        yaxis = list(title = title0, titlefont = list(size = titlefont0)),
        legend = region.legend.layout(dt_long$Region)
      )
  }

  plot.count <- function(dt, vars0, title0 = "Number", ncol = NULL) {
    dt <- limit.plot.regions(dt)
    dt <- dt[Year >= ctx$year_started]
    dt_long <- data.table::melt(dt, measure.vars = vars0, value.name = "Count", variable.name = "type")
    dt_long[, type := RecodePlotIndicators(type, indicator.order = vars0)]

    p <- ggplot2::ggplot(dt_long[!is.na(Count), ], ggplot2::aes(x = Year, y = Count, color = Region)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_bw() +
      ggplot2::labs(y = "", x = "", color = "") +
      ggplot2::facet_wrap(facets = ~ type, ncol = ncol) +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::scale_color_manual(values = ResolvePlotColors(data.table::uniqueN(dt_long$Region))) +
      ggplot2::scale_x_continuous(breaks = c(1990, 2000, 2010, ctx$year_ended)) +
      ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "k", scale = 1E-3, big.mark = ","))

    plotly::ggplotly(p, tooltip = c("Year", "Count")) |>
      plotly::layout(
        yaxis = list(title = title0, titlefont = list(size = titlefont0), tickfont = list(size = 10)),
        legend = region.legend.layout(dt_long$Region)
      )
  }

  output$plot_rate <- plotly::renderPlotly({
    if (is.null(reactive.run())) {
      return()
    }
    dt <- reactive.run()$both
    if (!show_world_in_current_plots()) {
      dt <- dt[Region != "World", ]
    }
    plot.rate(
      dt,
      vars0 = c(under_five_rate_plot_indicators(), stillbirth_rate_plot_indicators()),
      ncol = 2
    )
  })

  output$plot_rate_older <- plotly::renderPlotly({
    if (is.null(reactive.run()$both_5_24) || !isTRUE(input$run_older_total)) {
      return()
    }
    dt <- reactive.run()$both_5_24
    if (!show_world_in_current_plots()) {
      dt <- dt[Region != "World", ]
    }
    plot.rate(
      dt,
      vars0 = c(
        "Mortality rate age 5-9",
        "Mortality rate age 10-14",
        "Mortality rate age 15-19",
        "Mortality rate age 20-24",
        "Mortality rate age 10-19",
        "Mortality rate age 5-24"
      ),
      title0 = "Deaths per 1,000",
      ncol = 3
    )
  })

  output$plot_death_older <- plotly::renderPlotly({
    if (is.null(reactive.run()$both_5_24) || !isTRUE(input$run_older_total)) {
      return()
    }
    dt <- reactive.run()$both_5_24
    if (!show_world_in_current_plots()) {
      dt <- dt[Region != "World", ]
    }
    plot.count(
      dt,
      vars0 = c(
        "Deaths age 5 to 9",
        "Deaths age 10 to 14",
        "Deaths age 15 to 19",
        "Deaths age 20 to 24",
        "Deaths age 10 to 19",
        "Deaths age 5 to 24"
      ),
      title0 = "Number of deaths",
      ncol = 3
    )
  })

  output$plot_death <- plotly::renderPlotly({
    if (is.null(reactive.run())) {
      return()
    }
    dt <- reactive.run()$both
    if (!show_world_in_current_plots()) {
      dt <- dt[Region != "World", ]
    }
    plot.count(
      dt,
      vars0 = c(
        under_five_count_plot_indicators(colnames(dt)),
        stillbirth_count_plot_indicators(colnames(dt))
      ),
      title0 = "Number of deaths and stillbirths",
      ncol = 2
    )
  })

  output$plot_rate_gender <- plotly::renderPlotly({
    if (is.null(reactive.run()$m) || !isTRUE(input$run_gender)) {
      return()
    }
    vars0 <- c("U5MR median", "IMR median")
    dt_long_m <- get.long(reactive.run()$m, vars0)
    dt_long_f <- get.long(reactive.run()$f, vars0)
    dt_long_m$Sex <- "Male"
    dt_long_f$Sex <- "Female"
    dt_long <- rbind(dt_long_f, dt_long_m)
    dt_long$Region <- as.factor(dt_long$Region)
    dt_long$Sex <- factor(as.factor(dt_long$Sex), levels = c("Male", "Female"))
    dt_long <- limit.plot.regions(dt_long)
    selected_region <- selected_sex_specific_region()
    if (is.null(selected_region)) {
      return()
    }
    dt_long <- dt_long[Region == selected_region]
    data.table::setorder(dt_long, Indicator, Region, Sex, Year)
    dt_long[, Indicator := RecodePlotIndicators(Indicator, indicator.order = vars0)]
    dt_long[, tooltip_text := paste0("Year: ", Year, "<br />Rate: ", Rate, "<br />Sex: ", Sex)]

    p <- ggplot2::ggplot(
      dt_long,
      ggplot2::aes(x = Year, y = Rate, color = Sex, group = Sex, text = tooltip_text)
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::theme_bw() +
      ggplot2::labs(y = "", x = "", color = "") +
      ggplot2::scale_color_manual(values = sex_plot_colors(), breaks = names(sex_plot_colors())) +
      ggplot2::scale_x_continuous(breaks = c(1990, 2000, 2010, ctx$year_ended)) +
      ggplot2::facet_wrap(facets = ~ Indicator, ncol = 2) +
      ggplot2::theme(strip.text.x = ggplot2::element_text(margin = ggplot2::margin(.3, 0, .3, 0, "cm")))

    plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        yaxis = list(title = "Deaths per 1,000 live births", titlefont = list(size = titlefont0)),
        legend = list(orientation = "h", x = 0.4, y = -0.1)
      )
  })

  output$results_table <- DT::renderDT({
    if (is.null(reactive.run())) {
      return()
    }
    dt <- reactive.run()$both
    DT::formatRound(
      DT::datatable(clean.table(dt)),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate", "Neonatal Mortality Rate", "Stillbirth Rate"),
      digits = 2
    )
  })

  output$results_table_m <- DT::renderDT({
    if (is.null(reactive.run()$m) || !isTRUE(input$run_gender)) {
      return()
    }
    dt <- data.table::copy(reactive.run()$m)
    dt[, Sex := "Male"]
    DT::formatRound(
      DT::datatable(clean.table(dt)),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate"),
      digits = 2
    )
  })

  output$results_table_f <- DT::renderDT({
    if (is.null(reactive.run()$f) || !isTRUE(input$run_gender)) {
      return()
    }
    dt <- data.table::copy(reactive.run()$f)
    dt[, Sex := "Female"]
    DT::formatRound(
      DT::datatable(clean.table(dt)),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate"),
      digits = 2
    )
  })

  output$results_table_older_total <- DT::renderDT({
    if (is.null(reactive.run()$both_5_24) || !isTRUE(input$run_older_total)) {
      return()
    }
    DT::formatRound(
      DT::datatable(clean_older_children_table(reactive.run()$both_5_24)),
      columns = ctx$col_order_older_children_all_rate,
      digits = 2
    )
  })

  output$results_table_older_f <- DT::renderDT({
    if (is.null(reactive.run()$f_5_24) || !isTRUE(input$run_older_total)) {
      return()
    }
    dt <- data.table::copy(reactive.run()$f_5_24)
    dt[, Sex := "Female"]
    DT::formatRound(
      DT::datatable(clean_older_children_table(dt)),
      columns = ctx$col_order_older_children_all_rate,
      digits = 2
    )
  })

  output$results_table_older_m <- DT::renderDT({
    if (is.null(reactive.run()$m_5_24) || !isTRUE(input$run_older_total)) {
      return()
    }
    dt <- data.table::copy(reactive.run()$m_5_24)
    dt[, Sex := "Male"]
    DT::formatRound(
      DT::datatable(clean_older_children_table(dt)),
      columns = ctx$col_order_older_children_all_rate,
      digits = 2
    )
  })

  get.long.download.region.codes <- function() {
    region_structure <- uploaded_region_structure()
    if (is.null(region_structure) || is.null(region_structure$region_code_lookup)) {
      return(NULL)
    }

    region_code_lookup <- data.table::copy(region_structure$region_code_lookup)
    if (nrow(region_code_lookup) == 0 || !"Region_Code" %in% colnames(region_code_lookup)) {
      return(NULL)
    }

    region_code_lookup <- unique(region_code_lookup[, .(Region, Region_Code)], by = "Region")
    region_code_lookup <- region_code_lookup[!is.na(Region_Code) & nzchar(Region_Code)]
    if (nrow(region_code_lookup) == 0) {
      return(NULL)
    }

    region_code_lookup[, Region := rename.single.region(Region)]
    unique(region_code_lookup, by = "Region")
  }

  output$download_table_all <- downloadHandler(
    filename = function() {
      paste0("Results_under_five_", format(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      pieces <- list(clean.table(reactive.run()$both), reactive.run()$c_median_total)
      if (isTRUE(input$run_gender)) {
        pieces <- c(
          pieces,
          list(
            clean.table(reactive.run()$f),
            reactive.run()$c_median_f,
            clean.table(reactive.run()$m),
            reactive.run()$c_median_m
          )
        )
      }
      dtout <- data.table::rbindlist(Filter(Negate(is.null), pieces), fill = TRUE)
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )

  output$download_table_all_long <- downloadHandler(
    filename = function() {
      paste0("Results_all_long_format_", format(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- build_long_download(reactive.run(), get.long.download.region.codes())
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )

  output$download_table_f <- downloadHandler(
    filename = function() {
      paste0("Results_u5_female_", format(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- data.table::rbindlist(
        list(clean.table(reactive.run()$f), reactive.run()$c_median_f),
        fill = TRUE
      )
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )

  output$download_table_m <- downloadHandler(
    filename = function() {
      paste0("Results_u5_male_", format(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- data.table::rbindlist(
        list(clean.table(reactive.run()$m), reactive.run()$c_median_m),
        fill = TRUE
      )
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )

  output$download_table_older_total <- downloadHandler(
    filename = function() {
      paste0("Results_older_children_", format(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- data.table::rbindlist(
        list(
          clean_older_children_table(reactive.run()$both_5_24),
          reactive.run()$c_median_total_older,
          clean_older_children_table(reactive.run()$f_5_24),
          reactive.run()$c_median_f_older,
          clean_older_children_table(reactive.run()$m_5_24),
          reactive.run()$c_median_m_older
        ),
        fill = TRUE
      )
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )

  output$download_table_older_f <- downloadHandler(
    filename = function() {
      paste0("Results_older_children_female_", format(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- data.table::rbindlist(
        list(clean_older_children_table(reactive.run()$f_5_24), reactive.run()$c_median_f_older),
        fill = TRUE
      )
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )

  output$download_table_older_m <- downloadHandler(
    filename = function() {
      paste0("Results_older_children_male_", format(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- data.table::rbindlist(
        list(clean_older_children_table(reactive.run()$m_5_24), reactive.run()$c_median_m_older),
        fill = TRUE
      )
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )
}
