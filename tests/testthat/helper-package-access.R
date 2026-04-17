pkg_fn <- function(name) {
  get(name, envir = asNamespace("shinyregionalaggregate"), inherits = FALSE)
}
