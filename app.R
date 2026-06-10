## Prefer the installed package when available; otherwise load this local
## development checkout with pkgload before starting the Shiny app.
if (requireNamespace("shinyregionalaggregate", quietly = TRUE)) {
  shinyregionalaggregate::run_app()
} else {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop(
      "Install the shinyregionalaggregate package or the pkgload package to run the development app.",
      call. = FALSE
    )
  }

  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
  shinyregionalaggregate::run_app()
}


