# Shiny Regional Aggregate App

This repository now contains a golem-style R package, `shinyregionalaggregate`, that preserves the original Shiny app and also exposes a programmatic aggregation API.
It produces regional child mortality aggregates from selected countries, based on the BWC aggregation method and the original `outputaggregates-BWC` workflow.
The app is publicly deployed at:
<https://unicef-dapm.shinyapps.io/Shiny_regional_aggregate/>

In practical terms, the app now follows the standard golem layout: `app.R` is a thin launcher, `R/app_ui.R` and `R/app_server.R` define the Shiny shell, `R/run_app.R` exposes the public app entrypoint, and packaged data and web assets live under `inst/`.

## What the app does
- Lets users select countries directly or upload a list of ISO-3 codes.
- Produces aggregate mortality outputs for selected countries.
- Supports sex-specific outputs.
- Supports older children and adolescent age groups.
- Provides downloadable tables for regional, world, and country-level results.

## Run locally
1. Clone the repository:

```bash
git clone https://github.com/UnicefDAPM/Shiny_regional_aggregate.git
```

2. Open `Shiny_regional_aggregate.Rproj` in RStudio (recommended), or open the project in any R session.
3. Run the app from `app.R`, or call `shinyregionalaggregate::run_app()`.

Notes:
- `app.R` remains the main launcher for local development.
- `run_app()` is the package entrypoint and is the preferred way to launch the installed app from R.
- The packaged app reads all shipped data from `inst/extdata/`.
- Static UI assets live under `inst/app/www/`.

## Deploy the app

Deploy the repository root so the hosting platform sees both `app.R` and `DESCRIPTION`. The deployed app will start through `app.R`, which forwards to `shinyregionalaggregate::run_app()`.

For shinyapps.io, a typical deployment flow is:

```r
install.packages("rsconnect")

# this is an example:
rsconnect::setAccountInfo(
	name = "unicef-dapm",
	token = Sys.getenv("SHINYAPPS_TOKEN"), # <- you need to use real token and secret
	secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(
	appDir = ".",
	appName = "Shiny_regional_aggregate",
	appPrimaryDoc = "app.R"
)
```

The existing deployment metadata under `rsconnect/` shows the current shinyapps.io target and can be regenerated after a fresh deploy.

## Use the app from R

If the package is installed, launch the Shiny app directly with `run_app()`:

```r
library(shinyregionalaggregate)

run_app()
```

During local development inside the repository, you can also load the package in place and then launch the app:

```r
pkgload::load_all(".", export_all = FALSE, helpers = FALSE)
shinyregionalaggregate::run_app()
```

Because `run_app()` forwards `...` to `shiny::shinyApp()`, you can also pass Shiny options when needed:

```r
shinyregionalaggregate::run_app(
	options = list(port = 8080, launch.browser = TRUE)
)
```

## Programmatic aggregation API

`get_CME_aggregate()` accepts a `data.frame` or `data.table` with at least `Region` and `ISO3Code` columns and returns the same long-format table that the app downloads.

```r
library(data.table)
library(shinyregionalaggregate)

example_input <- fread(system.file("extdata", "examples", "AU.csv", package = "shinyregionalaggregate"))
dt_agg_out <- get_CME_aggregate(example_input)
```

## Yearly update workflow

Use `update_me_every_year.R` as the canonical checklist each year.
At a high level:
1. Update release date text in `update_me_every_year.R` (used in the app's About panel).
2. Refresh required input datasets in `inst/extdata/input/` when a new IGME round requires them.
3. Rebuild median result folders using scripts under `update/`.
4. Ensure region metadata and packaged output folders under `inst/extdata/` are recreated.
5. Validate that the app runs and exports expected tables.

The script includes detailed comments about which files usually change and which typically remain unchanged.

## Repository structure (key files)

- `app.R`: Main Shiny application launcher.
- `R/`: Package functions, including `run_app()` and `get_CME_aggregate()`.
- `inst/extdata/`: Packaged input data, output data, median result folders, and examples.
- `inst/app/www/`: Static assets for the Shiny UI.
- `update_me_every_year.R`: Yearly update instructions and shared parameters.
- `update/`: Scripts used during annual data refresh and folder regeneration.

## Major updates

### January 2020
- Added support for uploading ISO country lists.
- Added support for renaming the selected country group (default: "Selected Countries").

### March 2022
- Added age-group outputs for older children and adolescents.

### March 2026
- Completed full data and workflow refresh.
- Improved runtime speed.
- Added multi-region output support.  
- Replace the original loose-script app with a golem-style package structure, while preserving the original UI and server behavior.  

