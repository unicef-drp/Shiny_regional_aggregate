#' Add external resources to the application
#' @noRd
golem_add_external_resources <- function() {
  shiny::addResourcePath("www", app_sys("app", "www"))
  shiny::tagList()
}
