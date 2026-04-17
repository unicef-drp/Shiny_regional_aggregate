#' Application UI
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      shiny::titlePanel("Regional Child Mortality Aggregates"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 4,
          shiny::p("Upload a file with Region and ISO3Code columns."),
          shiny::fileInput(
            inputId = "region_file",
            label = "Region membership file",
            accept = c(".csv", ".xlsx", ".xls")
          ),
          shiny::actionButton("run_aggregate", "Run aggregates"),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton("download_long", "Download long-format results"),
          shiny::hr(),
          shiny::textOutput("run_status"),
          shiny::textOutput("results_summary")
        ),
        shiny::mainPanel(
          width = 8,
          shiny::h4("Input preview"),
          shiny::tableOutput("region_input_preview"),
          shiny::h4("Long-format preview"),
          shiny::tableOutput("results_preview")
        )
      )
    )
  )
}
