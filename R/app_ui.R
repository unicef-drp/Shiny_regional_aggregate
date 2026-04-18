#' Application UI
#' @noRd
app_ui <- function(request) {
  ctx <- build_app_context()

  note_header <- p(
    "This ShinyApp produces regional aggregates of child mortality estimates based on individually selected countries. The ",
    a("UN IGME", href = "https://childmortality.org", target = "_blank"),
    "'s latest estimates of neonatal, infant and under-five mortality are used. Country data will also be included in the downloaded dataset from the \"Tables and Data Download\" panel after running the aggregates."
  )

  note_input <- "Please select countries from the drop-down list, or by uploading a file of countries' ISO-alpha3 codes."
  note_map <- "Note: This map is stylized and not to scale and does not reflect a position by UNICEF on the legal status of any country or territory or the delimitation of any country or territory or the delimitation of any frontiers."

  tagList(
    golem_add_external_resources(),
    fluidPage(
      get.headerPanel(),

      sidebarPanel(
        shinyjs::useShinyjs(),

        titlePanel("Aggregate Selected Countries"),
        br(),
        wellPanel(p(note_header), style = "border: 0px"),

        wellPanel(
          helpText(note_input),
          shinyWidgets::pickerInput(
            inputId = "country_input_select",
            label = "Please Select Countries",
            choices = ctx$countries,
            selected = ctx$default_select,
            multiple = TRUE,
            options = list(
              title = "Please select countries",
              `actions-box` = TRUE,
              size = 10
            )
          ),
          checkboxInput(
            inputId = "input_by_region",
            label = "Group countries by the five continents",
            value = FALSE
          ),
          fileInput(
            inputId = "ISO_input",
            label = p(
              "(Optional) Upload your ISO 3 country codes",
              br(),
              "Download examples: ",
              a("single-region", href = "www/Upload_ISO3Code_example_single_region.csv", target = "_blank"),
              " - ",
              a("multi-region", href = "www/Upload_ISO3Code_example_multiple_regions.csv", target = "_blank")
            ),
            placeholder = "Column name shall contain \"ISO\"",
            accept = c(".csv", ".xlsx", ".xls")
          ),
          style = "border: 0px"
        ),

        wellPanel(
          uiOutput("panel_custom_group_name"),
          checkboxInput(
            inputId = "run_gender",
            label = strong("Run sex-specific results?"),
            value = TRUE
          ),
          checkboxInput(
            inputId = "run_older_total",
            label = strong("Run older children and adolescents results?"),
            value = FALSE
          ),
          actionButton("click_run", strong("Run the Aggregates"), width = "200px"),
          br(),
          br(),
          actionButton("click_reset", "Reset App", width = "200px")
        )
      ),

      mainPanel(
        tabsetPanel(
          tabPanel(
            "Plot",
            uiOutput("header_selected_countries"),
            textOutput("text_selected_countries"),
            br(),
            uiOutput("panel_plot_rate"),
            uiOutput("panel_plot_rate_gender"),
            uiOutput("panel_plot_older_children"),
            br(),
            br(),
            h4("Map"),
            leafletOutput("mymap"),
            h6(note_map)
          ),
          tabPanel(
            "Table and Data Download",
            uiOutput("panel_results_table"),
            br(),
            br(),
            uiOutput("panel_results_table_gender"),
            br(),
            br(),
            uiOutput("panel_results_table_older_total"),
            br(),
            br(),
            uiOutput("panel_results_table_older_gender")
          ),
          get.about.panel(update_string = ctx$update_string)
        )
      ),

      theme = "www/bootstrap.css"
    )
  )
}
