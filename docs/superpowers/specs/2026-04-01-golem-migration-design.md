# Shiny Regional Aggregate Golem Migration Design

## Summary

This document describes the migration of `Shiny_regional_aggregate` from a monolithic Shiny app into a modular R package built with `golem`.

The migration will preserve the current scientific behavior of the aggregation pipeline wherever possible while modernizing the application structure, package metadata, configuration, testing, and yearly release workflow.

The target outcome is a package that:

- runs the app through an exported `run_app()` function
- separates UI, server, and domain logic into focused files and modules
- removes runtime package installation and runtime `source()` loading
- supports release-based data management instead of manual code edits each year
- introduces automated validation and regression testing

## Current State

The current project is centered around a large [`app.R`](/C:/liuyanguu/Shiny_regional_aggregate/app.R) file that:

- sources [`update_me_every_year.R`](/C:/liuyanguu/Shiny_regional_aggregate/update_me_every_year.R)
- sources many helper scripts from [`R/`](/C:/liuyanguu/Shiny_regional_aggregate/R)
- loads bundled data and precomputed result folders directly from the repository
- mixes startup, data loading, UI construction, reactive orchestration, plotting, tables, downloads, and operational concerns in one app entry point

The app also includes a manual yearly release process driven by edits to `update_me_every_year.R` and scripts under `update/`.

## Goals

- Convert the app into a standard `golem` package structure.
- Keep the output behavior aligned with the current app unless a bug or clearly better operational pattern is identified.
- Redesign the yearly update process so new IGME rounds are managed as validated data releases instead of code edits.
- Make the app easier to test, maintain, and deploy.
- Create clear boundaries between Shiny modules, data access, aggregation services, and release management.

## Non-Goals

- Rewriting the scientific aggregation algorithms from scratch.
- Changing user-facing results intentionally.
- Removing support for the current input modes, plots, tables, or downloads.
- Building a fundamentally new UI design in this migration.

## Recommended Approach

Use a modular `golem` migration.

This approach keeps the existing scientific logic as the behavioral reference while redesigning the application around package conventions and explicit service interfaces. It avoids the long-term cost of a shallow package wrapper and avoids the risk of a full scientific replatform.

## Target Package Structure

The package will be organized around a standard `golem` layout with a small public API and internal service layers.

Planned structure:

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
- `R/svc_config.R`
- `R/svc_release_paths.R`
- `R/svc_data_access.R`
- `R/svc_country_selection.R`
- `R/svc_aggregation_under5.R`
- `R/svc_aggregation_older.R`
- `R/svc_results.R`
- `R/svc_downloads.R`
- `R/validators.R`
- `R/release_management.R`
- `inst/app/www/`
- `inst/extdata/`
- `tests/testthat/`

## Public API

The package will expose a minimal set of exported functions:

- `run_app(...)`: launch the Shiny application
- `refresh_release(release_id, source_dir, ...)`: prepare or register a yearly data release
- `validate_release(release_id, ...)`: validate required files and folder structure
- `build_release_cache(release_id, ...)`: build any derived caches needed for runtime efficiency

Additional exports may be added if a clear non-Shiny use case emerges, but the package should not expose internal data plumbing unnecessarily.

## Application Architecture

### App Composition

The app will be split into a small root UI and server layer plus focused modules:

- `mod_inputs`: country selection, ISO upload, run options, and run/reset triggers
- `mod_plots`: plots for under-five and older children outputs
- `mod_tables_downloads`: tables and downloadable outputs
- `mod_about`: about page and release metadata

The root server will orchestrate startup, release loading, and the shared analysis result object.

### Shared Result Object

Instead of allowing each output area to reach into raw reactives independently, the app will normalize user input and pass it to a central aggregation service. That service will return a structured result object containing:

- selected countries and resolved group labels
- under-five aggregate outputs
- sex-specific outputs when requested
- older children outputs when requested
- country summary datasets needed for downloads
- metadata needed by plots, tables, and messages

This object becomes the contract between the app logic and the output modules.

### Service Boundaries

The migration will define explicit service responsibilities:

- configuration service: resolve app settings and selected release
- release path service: locate and verify release directories and files
- data access service: load country metadata, maps, summaries, and other static inputs
- country selection service: normalize direct selection and uploaded ISO inputs
- aggregation services: wrap existing under-five and older-children aggregation logic
- download service: build downloadable tables from structured results
- validator service: fail early with clear messages when inputs or release files are invalid

## Data And Release Design

### Release-Based Data Layout

The current repository contains roughly 348 MB of input and result data. The package should not rely on embedding the full production release inside package code paths as the only supported mode.

Instead, the system will support external, versioned data releases with a structure such as:

- `data-releases/igme-2026/input/`
- `data-releases/igme-2026/output/`
- `data-releases/igme-2026/median_results_total/`
- `data-releases/igme-2026/median_results_female/`
- `data-releases/igme-2026/...`

The package will also include small example data under `inst/extdata/` for tests and development.

### Data Root Resolution

The package will resolve the active release using the following precedence:

1. explicit argument to `run_app(data_root = ..., release_id = ...)`
2. package option or environment variable
3. local development default

This keeps deployment explicit and supports multiple yearly releases without changing application code.

### Release Metadata

The manual constants currently stored in `update_me_every_year.R` will be replaced by release metadata, likely a single manifest file per release. The manifest will store items such as:

- release identifier
- display label for the app
- update string shown in the About section
- required file names
- supported year range

This allows yearly updates to become data-release changes instead of application logic changes.

## Migration Of Existing Code

### Scientific Logic

Existing scientific and transformation functions in the current `R/` scripts will be migrated incrementally:

- keep stable functions with minimal behavioral change
- rename and relocate only when it improves clarity
- wrap legacy logic behind well-named package service functions
- avoid broad rewrites unless required to remove hidden coupling or unblock tests

### Startup Logic

The current startup behavior will be modernized:

- remove runtime installation of packages
- remove runtime sourcing of all `R/` files
- declare dependencies in `DESCRIPTION`
- load static assets through package paths
- initialize app state through package services

## Error Handling

The app will preserve sanitized user-facing errors while improving operational diagnostics.

Planned behavior:

- clear validation errors for missing ISO columns, invalid codes, empty selections, or missing release files
- early startup failure when a release is incomplete
- internal logs or diagnostic messages that retain technical detail for debugging

## Testing Strategy

Testing will focus on migration safety and behavioral stability.

### Unit Tests

Add `testthat` coverage for:

- release path resolution
- manifest parsing
- file validation
- ISO upload normalization
- selected-country normalization
- aggregation wrapper behavior on representative sample inputs
- download table construction

### App Tests

Add `shinytest2` smoke and interaction tests for:

- app startup
- run and reset behavior
- direct country selection flow
- uploaded ISO file flow
- optional sex-specific and older-children runs

### Regression Safety

Create a small representative example release for automated testing so refactors can be checked without shipping the full production dataset into the test suite.

## Rollout Plan

The implementation should proceed in stages:

1. scaffold the `golem` package and package metadata
2. move static assets and startup into package conventions
3. create release/config services and a development release layout
4. migrate data loading and validators
5. migrate aggregation wrappers
6. split the Shiny app into modules
7. add tests and run regression checks
8. retire the legacy `app.R` entry point or reduce it to a compatibility launcher

## Risks And Mitigations

- Risk: behavioral drift during refactor
  Mitigation: wrap existing logic first, add regression checks before deeper cleanup

- Risk: package startup breaks because of large data assumptions
  Mitigation: separate production releases from package code and validate release layout explicitly

- Risk: yearly update process remains partly manual
  Mitigation: define a release manifest and package functions for refresh and validation early in the migration

- Risk: helper scripts contain hidden global-state dependencies
  Mitigation: identify globals during service extraction and replace them with explicit arguments or config objects

## Success Criteria

The migration is successful when:

- the app launches via `run_app()`
- the main current workflows behave the same from a user perspective
- yearly releases can be registered and validated without editing app logic
- dependencies are fully declared in the package
- the app has automated tests covering core flows
- the codebase is organized around modules and services rather than a monolithic app script

## Open Decisions Resolved For This Design

- Use modular `golem` migration rather than a shallow package wrapper
- Keep scientific logic behaviorally stable where possible
- Treat yearly data as release-managed external assets rather than hardwired code constants
- Include only small example data inside the package for development and tests
