# Golem App Parity Design

**Date:** 2026-04-17

## Goal

Restore the golem app so the `shinyApp` launched by `run_app()` matches the original application experience, while keeping the new packaged runtime and the reusable `get_CME_aggregate()` API.

## Approved Direction

Use the original UI and server behavior as the baseline and port that behavior into the package app entrypoints. The package should keep the new backend helpers for runtime workspace creation, adhoc country-info writing, aggregate execution, and long-format download building.

## Required Outcomes

1. `run_app()` launches the same visible app structure as the original app:
   - header/logo
   - sidebar country picker and ISO upload
   - reset flow
   - plot tab
   - table/download tab
   - map
   - about tab
   - original panel labels and user-facing copy
2. The app still supports uploaded Region/ISO3Code inputs, including multi-region inputs and long-format download with `Region_Code` when supplied.
3. The app backend must rely on packaged helpers and release data instead of the old loose-script startup pattern.
4. `get_CME_aggregate()` remains available and unchanged as the programmatic long-format API.

## Architecture

### App surface

`R/app_ui.R` and `R/app_server.R` become the packaged equivalents of the original `ui` and `server`, with the original control ids and output ids preserved where practical.

### App context

Add an internal app-context helper that loads the packaged release metadata and precomputes the static datasets the original app expected:

- country lookup tables
- region groupings
- picker choices
- map geometry
- country-level median tables for download panels
- display metadata such as update string, year bounds, and older-children column order

### Aggregate execution

The app server will no longer write directly into the repo root. Instead it will:

1. build or normalize region membership input
2. create a runtime workspace
3. write adhoc country info into that workspace
4. run the full packaged pipeline
5. read back the same result shapes needed by plots, tables, and downloads

This same backend remains the basis for `get_CME_aggregate()`.

### Resources

Restore the original static resources under `inst/app/www` and register them through golem so the header logo, theme CSS, indicator definition file, and upload examples resolve the same way as before.

## Testing

Add tests that lock down the app surface and new helpers:

- `app_ui()` renders the original tab and control labels
- `run_app()` still returns a `shiny.appobj`
- new app-context helper loads expected structures
- existing aggregate API tests continue passing

## Non-Goals

- rewriting the plotting or table behavior into a different UX
- changing the public `get_CME_aggregate()` signature
- removing packaged runtime helpers in favor of sourcing legacy scripts directly
