# Shiny Regional Aggregate App

This repository contains `shinyregionalaggregate`, a golem-based R package for the Shiny Regional Aggregate app. The package keeps the original Shiny interface and server workflow while adding a programmatic API for regional child mortality aggregation.

Golem is an R framework for building production-ready Shiny apps.

The app produces regional child mortality aggregates for selected countries using the BWC aggregation method from the original `outputaggregates-BWC` workflow.

The public app is deployed at:
<https://unicef-dapm.shinyapps.io/Shiny_regional_aggregate/>

The project follows the standard golem layout: `app.R` is a thin launcher, `R/app_ui.R` and `R/app_server.R` define the Shiny app, `R/run_app.R` exposes the public app entry point, and packaged data and web assets live under `inst/`.

## What the app does

- Lets users select countries directly or upload a list of ISO3 country codes.
- Produces aggregate mortality outputs for selected countries and custom regions.
- Supports sex-specific outputs.
- Supports older children and adolescent age groups.
- Provides downloadable regional, world, and country-level result tables.

## Run the app locally

1. Clone the repository:

```bash
git clone https://github.com/UnicefDAPM/Shiny_regional_aggregate.git
cd Shiny_regional_aggregate
```

2. Open `Shiny_regional_aggregate.Rproj` in RStudio, or open the project root in any R session.

3. Install dependencies, load the package, and launch the app:

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_deps(dependencies = TRUE)
devtools::load_all(".")
shinyregionalaggregate::run_app()
```

If the package is installed, launch the Shiny app directly with `run_app()`:

```r
library(shinyregionalaggregate)
run_app()
```

Notes:
- `app.R` remains the main launcher for local development.
- `run_app()` is the package entry point and is the preferred way to launch the installed app from R.
- The packaged app reads shipped data from `inst/extdata/`.
- Static UI assets live under `inst/app/www/`.

## Deploy the app

Deploy the repository root so the hosting platform sees both `app.R` and `DESCRIPTION`. The deployed app starts through `app.R`, which forwards to `shinyregionalaggregate::run_app()`.

For shinyapps.io, a typical deployment flow is:

```r
install.packages("rsconnect")

rsconnect::setAccountInfo(
  name = "unicef-dapm",
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

rsconnect::deployApp(
  appDir = ".",
  appName = "Shiny_regional_aggregate",
  appPrimaryDoc = "app.R"
)
```

If the `SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET` environment variables are not set, replace them with the corresponding shinyapps.io credentials.

The existing deployment metadata under `rsconnect/` shows the current shinyapps.io target and can be regenerated after a fresh deployment.

## Programmatic aggregation API

`get_CME_aggregate()` accepts a `data.frame` or `data.table` with at least `Region` and `ISO3Code` columns. It returns the same long-format table used by the app download, including `Region`, `Region_Code` when available, `Shortind`, `Sex`, `Year`, and `Median`.

The `quick_cme_aggregate_example.R` script is a minimal end-to-end example for using this API from a fresh R session. It installs required packages, installs `shinyregionalaggregate` from GitHub if needed, reads an example regional ISO file, and runs `get_CME_aggregate()` without launching the Shiny app.

```r
library(data.table)
library(shinyregionalaggregate)

example_input <- fread(
  system.file("extdata", "examples", "AU.csv", package = "shinyregionalaggregate")
)

dt_agg_out <- get_CME_aggregate(example_input)
```

## Yearly update workflow

Use `update_me_every_year.R` as the canonical yearly update checklist.

At a high level:

1. Update release date text in `update_me_every_year.R`, which is used in the app's About panel.
2. Refresh required input datasets in `inst/extdata/input/` when a new IGME round requires them.
3. Rebuild median result folders using scripts under `update/`.
4. Recreate region metadata and packaged output folders under `inst/extdata/`.
5. Validate that the app runs and exports the expected tables.

The script includes detailed comments about which files usually change and which typically remain unchanged.

## Repository structure

- `app.R`: Main Shiny application launcher.
- `R/`: Package functions, including `run_app()` and `get_CME_aggregate()`.
- `inst/extdata/`: Packaged input data, output data, median result folders, and examples.
- `inst/app/www/`: Static assets for the Shiny UI.
- `update_me_every_year.R`: Yearly update instructions and shared parameters.
- `update/`: Scripts used during annual data refresh and folder regeneration.

## Major updates

### January 2020

- Added support for uploading ISO country lists.
- Added support for renaming the selected country group, with "Selected Countries" as the default.

### March 2022

- Added age-group outputs for older children and adolescents.

### March 2026

- Completed a full data and workflow refresh.
- Improved runtime speed.
- Added multi-region output support.
- Replaced the original loose-script app with a golem-style package structure while preserving the original UI and server behavior.
