#' Application UI
#' @noRd
app_ui <- function(request) {
  ctx <- build_app_context()

  note_header <- p(
    "This Shiny app produces regional aggregates of child mortality estimates for selected countries. The latest ",
    a("UN IGME", href = "https://childmortality.org", target = "_blank"),
    " estimates of neonatal, infant, and under-five mortality are used. Country-level data are also included in the dataset downloaded from the \"Tables and data download\" panel after the aggregates run."
  )

  note_input <- "Please select countries from the drop-down list or upload a file of countries' ISO alpha-3 codes."
  note_map <- "Note: This map is stylized, is not to scale, and does not reflect any position by UNICEF on the legal status of any country or territory, or on the delimitation of frontiers or boundaries."

  tagList(
    golem_add_external_resources(),
    fluidPage(
      get.headerPanel(),

      sidebarPanel(
        shinyjs::useShinyjs(),

        titlePanel("Aggregate selected countries"),
        br(),
        wellPanel(p(note_header), style = "border: 0px"),

        wellPanel(
          helpText(note_input),
          shinyWidgets::pickerInput(
            inputId = "country_input_select",
            label = "Please select countries",
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
            label = "Group countries by continent",
            value = FALSE
          ),
          fileInput(
            inputId = "ISO_input",
            label = p(
              "Optional: Upload ISO alpha-3 country codes",
              br(),
              "Download examples: ",
              a(
                "single-region",
                href = "www/Upload_ISO3Code_example_single_region.csv",
                download = "Upload_ISO3Code_example_single_region.csv"
              ),
              " - ",
              a(
                "multi-region",
                href = "www/Upload_ISO3Code_example_multiple_regions.csv",
                download = "Upload_ISO3Code_example_multiple_regions.csv"
              )
            ),
            placeholder = "Column name must contain \"ISO\"",
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
            value = TRUE
          ),
          actionButton("click_run", strong("Run aggregates"), width = "200px"),
          br(),
          br(),
          actionButton("click_reset", "Reset app", width = "200px")
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
            leafletOutput("mymap", height = "500px"),
            h6(note_map)
          ),
          tabPanel(
            "Tables and data download",
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
          get.about.panel(update_string = ctx$update_string, WPP_Year = ctx$WPP_Year)
        )
      ),

      theme = "www/bootstrap.css"
    )
  )
}
