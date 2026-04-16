# Shiny Regional Aggregate App

This repository contains a Shiny app that produces regional child mortality aggregates from selected countries, based on the BWC aggregation method and the original `outputaggregates-BWC` workflow.
The app is publicly deployed at:
<https://unicef-dapm.shinyapps.io/Shiny_regional_aggregate/>

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

2. Open `Shiny_regional_aggregate.Rproj` in RStudio (recommended), or open `app.R` in your R session.
3. Run the app from `app.R` (for example with `shiny::runApp()` or the IDE "Run App" button).

Notes:
- `app.R` is the main entry point.
- At startup, `app.R` sources `update_me_every_year.R`, which defines shared paths and update-related variables.
- The app checks required packages and installs missing ones automatically.

## Yearly update workflow

Use `update_me_every_year.R` as the canonical checklist each year.
At a high level:
1. Update release date text in `update_me_every_year.R` (used in the app's About panel).
2. Refresh required input datasets in `input/` when a new IGME round requires them.
3. Rebuild median result folders using scripts under `update/`.
4. Ensure region metadata and output folder structures are recreated.
5. Validate that the app runs and exports expected tables.

The script includes detailed comments about which files usually change and which typically remain unchanged.

## Repository structure (key files)

- `app.R`: Main Shiny application.
- `update_me_every_year.R`: Yearly update instructions and shared parameters.
- `R/`: Helper functions used by the app.
- `input/`: Input data files used to build outputs.
- `median_results_*`: Precomputed median result folders used by the app.
- `update/`: Scripts used during annual data refresh and folder regeneration.
- `www/`: Static assets for the Shiny UI.

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
