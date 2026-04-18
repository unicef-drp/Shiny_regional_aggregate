#' Read a value from config
#' @noRd
get_golem_config <- function(value, config = Sys.getenv("GOLEM_CONFIG_ACTIVE", "default")) {
  config::get(
    value = value,
    config = config,
    file = app_sys("golem-config.yml")
  )
}
