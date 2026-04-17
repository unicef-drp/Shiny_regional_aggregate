#' Application UI
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      shiny::titlePanel("Aggregate Selected Countries"),
      shiny::div(id = "app-root")
    )
  )
}
