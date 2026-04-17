# Golem Package API Design

## Summary

This design refactors `Shiny_regional_aggregate` into a proper `golem` package while preserving the existing aggregation logic and downloadable outputs.

The package will continue to expose the Shiny app through `run_app()`, but the main product surface will become a programmatic API:

- `get_CME_aggregate(region_iso)`

`region_iso` will be a `data.frame` or `data.table` with at least `Region` and `ISO3Code` columns. The function will run the same full aggregation pipeline currently used by the app and return the same combined long-format table currently produced by the app's "Download all in long-format" button.

The Shiny app will become a thin interface over the package API rather than the owner of aggregation logic.

## Goals

- Convert the repo into a standard `golem` package structure.
- Preserve the current scientific logic and output values.
- Keep a supported Shiny entrypoint through `run_app()`.
- Expose a stable package API for scripted use.
- Accept region and ISO3 input as a tabular object rather than relying on browser upload state.
- Preserve the current long-format downloadable output shape, including under-five, sex-specific, and older-children results.
- Reduce global state and runtime sourcing so the code is testable and package-friendly.

## Non-Goals

- Rewriting the scientific aggregation methods.
- Changing the numerical behavior intentionally.
- Removing the current app workflow.
- Designing a new UI.
- Rebuilding the yearly update workflow in this change beyond what is needed to make package paths and initialization work.

## Recommended Approach

Use a hybrid `golem` package design:

1. keep the current scientific scripts as the behavioral reference
2. wrap them behind internal package services
3. export `get_CME_aggregate()` as the stable programmatic API
4. make `run_app()` call those same services

This avoids a shallow package wrapper while also avoiding a risky rewrite of the underlying mortality logic.

## User-Facing API

### `run_app(...)`

`run_app()` remains the public app entrypoint and launches the Shiny application from the installed package.

The app will still support:

- direct country selection
- file upload
- plots
- tables
- downloads

Internally, the app will call the same package services used by `get_CME_aggregate()`.

### `get_CME_aggregate(region_iso)`

`get_CME_aggregate()` is the main exported function for scripted use.

Input requirements:

- object must be coercible to `data.table`
- must contain `Region`
- must contain `ISO3Code`

Optional columns:

- `Region_Code`
- `OfficialName`

Behavior:

1. validate and normalize the input table
2. construct the adhoc country-info files required by the existing pipeline
3. run the full aggregation sequence with `reuse.replacement.country = TRUE`
4. read total, female, male, and older-children outputs
5. return the combined long-format download table

The aggregation sequence is fixed and always includes:

```r
run.outputaggregates(year.lastestimatepublished, reuse.replacement.country = TRUE)
run.outputaggregates.gender(year.lastestimatepublished, reuse.replacement.country = TRUE)
adjust.u5.sex.specific.death()
run.outputaggregates.5.24(year.lastestimatepublished)
run.outputaggregates.5.24.gender(year.lastestimatepublished)
adjust.total.death.5.24()
```

The function will not expose flags to skip sex-specific or older-children runs in this refactor because the required product behavior is the same as the app's current all-in long download.

## Package Structure

The package will use a conventional `golem` layout, with domain logic split out of the app shell.

Planned files:

- `DESCRIPTION`
- `NAMESPACE`
- `R/run_app.R`
- `R/app_ui.R`
- `R/app_server.R`
- `R/golem_utils_ui.R`
- `R/golem_utils_server.R`
- `R/mod_inputs.R`
- `R/mod_plots.R`
- `R/mod_tables_downloads.R`
- `R/mod_about.R`
- `R/aggregate_api.R`
- `R/aggregate_runner.R`
- `R/input_parsing.R`
- `R/adhoc_regions.R`
- `R/output_formatting.R`
- `R/package_paths.R`
- `R/package_data.R`
- `R/legacy_wrappers.R`
- `inst/app/www/`
- `inst/extdata/`
- `tests/testthat/`

The existing scientific helper files under `R/` may be migrated in place at first, but their responsibilities should become explicit and package-oriented rather than being sourced wholesale by `app.R`.

## Application Architecture

### Root App

The root `golem` app will coordinate:

- package startup
- loading bundled reference data
- input normalization
- aggregate execution
- result display

It will not own scientific logic directly.

### Modules

Split the current app into focused modules:

- `mod_inputs`: country selection, file upload, group naming, run/reset
- `mod_plots`: current plots and map output
- `mod_tables_downloads`: tables and CSV downloads
- `mod_about`: about panel and release text

### Shared Result Contract

The app server will work with a structured result object returned by package services. That object will contain:

- normalized input metadata
- under-five aggregate tables
- sex-specific aggregate tables
- older-children aggregate tables
- long-format combined download table
- selected-country metadata used by UI messaging

This replaces the current pattern where the app constructs and reshapes outputs inline inside one large server script.

## Input Normalization Design

The current app accepts uploaded files and infers the ISO column, optional `Region`, optional `Region_Code`, and multi-level region membership. The package will preserve this behavior, but separate it into plain functions.

Planned responsibilities:

- `validate_region_iso_input()`: check required columns and non-empty valid rows
- `normalize_region_iso_input()`: standardize names and types
- `build_uploaded_region_structure()`: reproduce the current app logic that converts long region input into the wide `AdhocCountries`, `AdhocCountries2`, ... structure used downstream
- `rename_single_region()`: preserve the current single-group naming behavior for downloads and displays

The Shiny app's file upload layer becomes a wrapper that reads CSV/XLSX and then passes the resulting table into these functions.

## Adhoc Country-Info Construction

The current app mutates copies of:

- `input/country.info.CME.csv`
- `input/country.info.CME.5_14.csv`
- `input/country.info.CME.15_24.csv`

to build adhoc datasets consumed by the aggregation scripts.

The package will preserve that behavior, but behind explicit helpers:

- load baseline metadata tables from package data roots
- apply normalized region selections to each table
- write derived adhoc files into a package-managed working directory

Planned helper boundaries:

- `load_country_info_baseline()`
- `apply_multi_region_to_country_info()`
- `write_adhoc_country_info()`

This keeps the existing scientific scripts working while moving mutation out of the app server.

## Runtime Data And Working Directories

The package will not require normal package users to write into the installed package directory or into the repository root.

Instead:

- baseline reference data will be resolved from package-owned paths
- derived adhoc files and generated aggregate outputs will be written into a run-specific working directory
- the app and the API will resolve paths through package helpers rather than assuming `here::here()`

The working directory can be temporary by default, with an override for development or debugging if needed.

This is important for:

- installed package behavior
- automated tests
- concurrent use
- avoiding accidental mutation of checked-in data

## Legacy Logic Integration

The existing aggregation code depends heavily on shared variables and path globals currently created by `update_me_every_year.R` and `app.R`.

The package refactor will preserve the legacy algorithms while isolating those globals behind setup helpers.

Planned strategy:

- move path and release constants into package setup functions
- make package startup load required data and assign the minimal legacy globals needed by the existing wrappers
- keep the existing `run.outputaggregates*()` family callable with minimal behavioral change
- avoid broad rewrites inside `OutputAggregates()` and older-children methods during the first pass

This is intentionally conservative. The first successful package version should be mostly a refactor of ownership and boundaries, not a rewrite of the scientific engine.

## Output Formatting Design

The returned table from `get_CME_aggregate()` must match the current app's combined long-format download.

That output includes rows from:

- under-five total
- under-five female
- under-five male
- older-children total
- older-children female
- older-children male

Expected columns:

- `Region`
- `Region_Code` when present in input and resolvable
- `Shortind`
- `Sex`
- `Year`
- `Median`

Planned helper boundaries:

- `table_to_long_download()`
- `get_long_download_region_codes()`
- `build_long_download()`

These helpers should be lifted out of the current app server and used directly by both the API and the download button.

## Error Handling

The package will fail early and clearly for programmatic use while still presenting friendly messages in the app.

Programmatic errors should cover:

- missing required columns
- unreadable or empty input
- invalid ISO codes after filtering
- unsupported file type in app upload wrapper
- missing reference data files
- missing generated outputs after aggregate execution

The API should raise ordinary R errors with actionable messages.

The app should catch those errors and present sanitized modal or notification messages without hiding the cause from logs.

## Testing Strategy

Testing should center on behavioral preservation of the new package API.

### Unit Tests

Add `testthat` coverage for:

- region/ISO input validation
- long-to-wide multi-region normalization
- adhoc country-info file generation
- long-format output construction
- path resolution and working-directory setup

### Regression Tests

Add regression tests that compare `get_CME_aggregate()` output against known expected outputs for representative fixtures, including:

- a single-region input equivalent to current `AU.csv`
- a multi-region input with `Region_Code`
- confirmation that sex-specific and older-children rows are included

### App Tests

Add smoke tests for:

- `run_app()` startup
- upload flow calling the new API pathway
- download flow producing the same structure as the programmatic API

## Migration Plan For Code

Implementation should proceed in this order:

1. scaffold the package and declare dependencies
2. extract package path and startup helpers
3. extract input normalization functions from the current app
4. extract output-formatting helpers from the current app
5. wrap legacy aggregate execution in a package runner
6. implement and export `get_CME_aggregate()`
7. refactor the app to call the new package services
8. move the app into `golem` modules
9. add tests and fixture-based regression checks

This order creates value early because the public API can be validated before the Shiny refactor is fully complete.

## Risks And Mitigations

- Risk: hidden global dependencies in legacy scripts break package execution
  Mitigation: create explicit package setup helpers and migrate path/state assumptions into one place first

- Risk: installed-package path handling breaks because legacy code assumes repository-relative files
  Mitigation: centralize all path resolution and stage generated files in a dedicated working directory

- Risk: output drift during refactor
  Mitigation: add fixture-based regression tests around `get_CME_aggregate()` before deeper cleanup

- Risk: app and API diverge over time
  Mitigation: make both use the same service functions and output-formatting helpers

## Success Criteria

The refactor is successful when:

- the project installs as a package
- `run_app()` launches the app successfully
- `get_CME_aggregate()` accepts a region/`ISO3Code` table and returns the expected long-format output
- the returned output matches the current app's all-in long-format download structure
- the legacy aggregation logic is preserved behind package service boundaries
- tests cover the main API path and key app smoke flows

## Relationship To Earlier Migration Spec

This design narrows and updates the earlier broad golem migration proposal in `docs/superpowers/specs/2026-04-01-golem-migration-design.md`.

The earlier document remains useful background, but this design resolves the product direction for this refactor:

- keep `run_app()`
- add `get_CME_aggregate()`
- accept tabular region/ISO input
- always run the full aggregation pipeline
- return the current combined long-format download table
