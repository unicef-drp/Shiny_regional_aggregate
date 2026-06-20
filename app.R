# Prefer the installed package when available; otherwise load this local
# development checkout with pkgload before starting the Shiny app.
#
# This guarded launcher is more portable because it can still run when:
# * shinyregionalaggregate is already installed but pkgload is not installed.
# * The app is launched in a more package-like production environment.
#
# Development note:
# If you are editing this source checkout and shinyregionalaggregate is already
# installed, this launcher will use the installed package. To see local changes,
# run the development checkout directly:
#
# pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
# shinyregionalaggregate::run_app()
#
# Alternatively, remove the installed package first:
# remove.packages("shinyregionalaggregate")

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
