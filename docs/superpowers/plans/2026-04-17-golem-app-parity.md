# Golem App Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore the original Shiny app UI and behavior in the golem package while preserving the packaged backend and `get_CME_aggregate()`.

**Architecture:** Build a packaged app context that recreates the legacy app's static state, then port the legacy UI/server onto the packaged runtime helpers. Keep the programmatic aggregate API intact and reuse backend helpers rather than duplicating legacy file-path logic.

**Tech Stack:** R, shiny, golem, data.table, DT, plotly, leaflet, sf, shinyWidgets, shinyjs

---

### Task 1: Lock down app-parity expectations

**Files:**
- Modify: `tests/testthat/test-app-entrypoint.R`
- Create: `tests/testthat/test-app-ui-parity.R`

- [ ] **Step 1: Write the failing app UI parity test**

```r
testthat::test_that("app_ui contains the original app structure", {
  html <- htmltools::renderTags(app_ui(shiny::getDefaultReactiveDomain()))$html

  testthat::expect_match(html, "Aggregate Selected Countries")
  testthat::expect_match(html, "Table and Data Download")
  testthat::expect_match(html, "About")
  testthat::expect_match(html, "country_input_select")
  testthat::expect_match(html, "ISO_input")
  testthat::expect_match(html, "click_run")
})
```

- [ ] **Step 2: Run the app UI tests to verify they fail**

Run: `@' testthat::test_file('tests/testthat/test-app-ui-parity.R') '@ | Rscript -`

Expected: failure because the current simplified golem UI does not contain the original structure.

- [ ] **Step 3: Extend the entrypoint test for the packaged app object**

```r
testthat::test_that("run_app returns a shiny app object", {
  app <- run_app()
  testthat::expect_s3_class(app, "shiny.appobj")
  testthat::expect_true(is.function(app$ui))
  testthat::expect_true(is.function(app$server))
})
```

- [ ] **Step 4: Re-run the focused entrypoint tests**

Run: `@' testthat::test_file('tests/testthat/test-app-entrypoint.R') '@ | Rscript -`

Expected: pass.

### Task 2: Add packaged app context and resource support

**Files:**
- Create: `R/fct_app_context.R`
- Modify: `R/golem_utils_ui.R`
- Modify: `DESCRIPTION`

- [ ] **Step 1: Write the failing app-context test**

```r
testthat::test_that("build_app_context loads packaged app metadata", {
  ctx <- pkg_fn("build_app_context")()

  testthat::expect_true(is.data.table(ctx$dc))
  testthat::expect_true(length(ctx$countries) > 0)
  testthat::expect_true(length(ctx$ISOs) > 0)
  testthat::expect_true(inherits(ctx$world_map, "SpatialPolygonsDataFrame"))
  testthat::expect_true("update_string" %in% names(ctx))
})
```

- [ ] **Step 2: Run the new context test to verify it fails**

Run: `@' testthat::test_file('tests/testthat/test-app-context.R') '@ | Rscript -`

Expected: failure because `build_app_context()` does not exist yet.

- [ ] **Step 3: Implement the packaged app context and resource registration**

```r
build_app_context <- function() {
  meta <- release_metadata()
  dc <- data.table::fread(release_path("input", "country.info.CME.csv"))
  dc.5.14 <- data.table::fread(release_path("input", "country.info.CME.5_14.csv"))
  dc.15.24 <- data.table::fread(release_path("input", "country.info.CME.15_24.csv"))
  # preload region labels, picker choices, map, country summaries, and metadata
}
```

```r
golem_add_external_resources <- function() {
  shiny::addResourcePath("www", app_sys("app", "www"))
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/bootstrap.css")
  )
}
```

- [ ] **Step 4: Add runtime app dependencies**

Update `DESCRIPTION` so `Imports` includes:

```text
DT
dplyr
ggplot2
htmltools
leaflet
maps
plotly
readxl
scales
sf
shinyjs
shinyWidgets
sp
```

- [ ] **Step 5: Run the app-context tests**

Run: `@' testthat::test_file('tests/testthat/test-app-context.R') '@ | Rscript -`

Expected: pass.

### Task 3: Restore legacy app UI and server behavior on the packaged backend

**Files:**
- Modify: `R/app_ui.R`
- Modify: `R/app_server.R`
- Modify: `R/fct_aggregate_api.R`
- Create: `R/fct_app_runtime.R`
- Restore: `inst/app/www/logo.png`
- Restore: `inst/app/www/bootstrap.css`
- Restore: `inst/app/www/Indicator definition and unit.xlsx`
- Restore: `inst/app/www/Upload_ISO3Code_example_single_region.csv`
- Restore: `inst/app/www/Upload_ISO3Code_example_multiple_regions.csv`

- [ ] **Step 1: Write the failing long-download parity test**

```r
testthat::test_that("aggregate results can be expanded into app download payloads", {
  region_iso <- read_au_input()
  results <- pkg_fn("get_CME_aggregate_results")(region_iso)

  testthat::expect_true(is.list(results))
  testthat::expect_true(all(c("both", "f", "m", "both_5_24", "f_5_24", "m_5_24") %in% names(results)))
})
```

- [ ] **Step 2: Run the focused backend parity test to verify it fails**

Run: `@' testthat::test_file('tests/testthat/test-app-runtime.R') '@ | Rscript -`

Expected: failure because the packaged app runtime helper does not exist yet.

- [ ] **Step 3: Implement an internal packaged runtime helper for full app outputs**

```r
get_CME_aggregate_results <- function(region_iso, adhoc_name = NULL) {
  normalized <- normalize_region_iso_input(region_iso)
  workspace <- create_runtime_workspace()
  on.exit(unlink(workspace, recursive = TRUE, force = TRUE), add = TRUE)

  membership <- write_adhoc_country_info(normalized, workspace)
  run_full_aggregate_pipeline(workspace)
  results <- read_runtime_results(workspace, adhoc_name = adhoc_name)
  results$region_code_lookup <- membership$region_code_lookup
  results
}
```

- [ ] **Step 4: Port the original app UI**

Replace the simplified page in `R/app_ui.R` with the legacy layout:

```r
app_ui <- function(request) {
  ctx <- build_app_context()
  shiny::fluidPage(
    get.headerPanel(),
    shiny::sidebarPanel(...),
    shiny::mainPanel(...),
    theme = "bootstrap.css"
  )
}
```

- [ ] **Step 5: Port the original app server onto packaged helpers**

Implement `R/app_server.R` so it:

```r
- reads uploaded Region/ISO files with packaged parsing helpers
- supports manual country selection
- preserves reset, modal, map, plot, table, and download behavior
- uses get_CME_aggregate_results() for aggregate runs
- uses build_long_download() for the long-format download
```

- [ ] **Step 6: Re-run the focused app tests**

Run: `@' testthat::test_file('tests/testthat/test-app-ui-parity.R'); testthat::test_file('tests/testthat/test-app-runtime.R') '@ | Rscript -`

Expected: pass.

### Task 4: Run regression and package verification

**Files:**
- Modify: `tests/testthat/test-app-entrypoint.R`
- Modify: `tests/testthat/test-get-CME-aggregate.R`

- [ ] **Step 1: Run targeted package tests**

Run: `@'
testthat::test_dir('tests/testthat', reporter = 'summary')
'@ | Rscript -`

Expected: all testthat files pass.

- [ ] **Step 2: Run the app smoke test**

Run: `@'
old <- setwd('C:/liuyanguu/Shiny_regional_aggregate/.worktrees/codex-golem-package')
on.exit(setwd(old), add = TRUE)
app_value <- source('app.R', local = new.env())$value
print(class(app_value))
'@ | Rscript -`

Expected:

```text
[1] "shiny.appobj"
```

- [ ] **Step 3: Run package build**

Run: `R.exe CMD build .`

Expected: build succeeds.

- [ ] **Step 4: Commit**

```bash
git add R/app_ui.R R/app_server.R R/fct_app_context.R R/fct_app_runtime.R R/fct_aggregate_api.R R/golem_utils_ui.R DESCRIPTION inst/app/www tests/testthat docs/superpowers/specs/2026-04-17-golem-app-parity-design.md docs/superpowers/plans/2026-04-17-golem-app-parity.md
git commit -m "feat: restore legacy app UI on packaged backend"
```
