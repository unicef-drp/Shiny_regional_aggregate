# Older-Children Aggregation Refactor Design

## Summary

This design refactors `R/5_24/outputaggregates.5_24.R` so `OutputAggregates.ori()` follows the same orchestration shape as under-five `OutputAggregates()` in `R/outputaggregates-BWC.R`, while preserving the older-children formulas and outputs.

The goal is architectural parity, not method parity. Older-children aggregation will adopt the same high-level structure for preprocessing, cache management, regional execution, helper naming, progress logging, and timing summaries, but it will not import BWC-specific mortality replacement logic, neonatal handling, or under-five life-table reconstruction.

## Goals

- Keep `OutputAggregates.ori()` as the public older-children entry point.
- Preserve existing older-children outputs, file names, and downstream callers in `R/6outputaggregates_5_24.R` and `app.R`.
- Refactor orchestration into smaller named helpers with `5_24`-specific names to avoid sourcing collisions.
- Replace the long inline regional dispatch block with reusable region-dispatch helpers.
- Add timing/progress output comparable to under-five `OutputAggregates()`.
- Make cache checks, file deletion, and reruns easier to reason about.

## Non-Goals

- Do not add BWC replacement-country logic.
- Do not add neonatal/NMR branches.
- Do not change older-children mortality formulas unless required by a regression uncovered during refactor.
- Do not consolidate older-children and under-five into one shared implementation in this change.
- Do not change public wrapper names such as `run.outputaggregates.5.24()`.

## Current Problems

The current `OutputAggregates.ori()` file mixes several responsibilities in one large function:

- input loading and trajectory alignment
- cache existence checks
- country/world/regional orchestration
- region selection and dispatch
- per-draw regional execution
- combined-output writing

That makes it harder to debug, slower to evolve, and more fragile when `app.R` sources every R file into one shared environment.

Recent collisions already showed that generic helper names such as `CombineAndOutputRegionalResults` are unsafe in this repo shape. The older-children file also has stale cache expectations and duplicated regional dispatch logic that are harder to verify than the newer under-five flow.

## Proposed Architecture

### Entry Point

`OutputAggregates.ori()` remains the top-level function, but becomes a coordinator that:

1. resolves directories and options
2. loads and aligns input data
3. prepares cached trajectory objects
4. runs country aggregation
5. runs world aggregation
6. runs regional aggregation
7. prints a timing summary

It should read similarly to under-five `OutputAggregates()`, but without any BWC-only branches.

### Naming Isolation

All older-children helpers that can plausibly collide with under-five helpers will use `5_24` suffixes. This includes:

- `GetRegionalResults5_24()`
- `CalculateRegionalDeaths5_24()`
- `CombineAndOutputRegionalResults5_24()`
- cache-list helpers such as `RegionalCombinedCacheFiles5_24()`
- orchestration helpers such as `RunRegionalResults5_24()`

Existing already-renamed helpers keep their `5_24` suffixes. The design intentionally avoids generic helper names in the older-children file.

### Preprocess Layer

Split the current preprocessing block into focused helpers:

- `ResolveOutputDirs5_24()`
- `LoadCountryInputs5_24()`
- `LoadTrajectoryArrays5_24()`
- `AlignTrajectoryArrays5_24()`
- `BuildOlderChildrenContext5_24()`

The preprocess layer returns one context list used by later phases, rather than relying on a very large function environment.

The context should contain:

- `country.info`
- `data.pop`
- `data.a0`
- aligned `u5mr.ctj`, `imr.ctj`
- `iso.c`, `est.years`, `est.years.floor`
- `nyears`, `nsim`
- `a0.c`, `a1to4.c`
- `pop0.orig.ct`, `pop1to4.orig.ct`
- output directory paths

### Cache Management

Introduce small helpers for file-list construction and invalidation:

- `CountryCacheFiles5_24()`
- `WorldCacheFiles5_24()`
- `RegionalCombinedCacheFiles5_24(filename)`
- `DeleteWorldOutputs5_24()`

These replace repeated literal file lists spread through the main function and make cache behavior explicit.

### Country Phase

The country phase keeps current formulas and output files, but orchestration becomes:

- `RunCountryResults5_24(ctx, percentiles, ndigits, run.on.server)`

Responsibilities:

- check whether combined country cache files exist
- run per-draw `CalculateCountryDeaths()` only when needed
- combine and output with existing `CombineAndOutputCountryResults()`
- record phase timing

Country-phase math should remain unchanged.

### World Phase

The world phase becomes:

- `RunWorldResults5_24(ctx, percentiles, ndigits)`

Responsibilities:

- check for required world cache files
- generate world outputs when missing
- load existing outputs when present
- use a world cache list that matches what the older-children world code actually writes

This is also where the stale `global.RoDs.ui.rda` expectation should be cleaned up so the world cache contract matches real older-children outputs.

### Regional Dispatch

Replace the long `if (is.element(...))` block with a dispatch table helper:

- `BuildRegionalSpecs5_24(country.info, regiontypes.select)`

Each regional spec should contain:

- `selection_key`
- `filename`
- `regiontypes`
- `regions`

The dispatch builder handles special Adhoc behavior:

- hierarchical `AdhocCountries`, `AdhocCountries1`, `AdhocCountries2`, ...
- single-level `AdhocCountries_*`
- fallback legacy Adhoc case

Other region families should be described declaratively instead of through repeated inline `GetRegionalResults(...)` calls.

### Regional Runner

Regional orchestration becomes:

- `RunRegionalResults5_24(spec, ctx, run.on.server, percentiles, ndigits)`

Responsibilities:

- print consistent progress messages
- execute per-draw regional calculations
- retry missing bundles if regional sample files are missing after execution
- combine and output final regional results
- return timing info

The runner shape should mirror the under-five refactor, but stay simple:

- no replacement-country logic
- no selected-ISO partial rebuild logic
- no BWC membership/life-table rebuild logic

### Regional Calculation

Per-draw regional calculation remains older-children-specific and formula-compatible with the current script, but is isolated in:

- `CalculateRegionalDeaths5_24()`
- `SaveRegionalBundle5_24()`
- `RegionalBundleExists5_24()`

This makes retries and verification possible without keeping large logic in the top-level runner.

### Timing and Logging

Add a phase-timing summary parallel to under-five:

- preprocess
- country
- world
- regional
- total

Logging should remain concise and preserve the current user-facing flow:

- whether processed trajectories were loaded or saved
- whether country/world/regional outputs were loaded or generated
- which Adhoc grouping mode is in use
- regional retry summaries when relevant

## Data and Output Compatibility

This refactor must preserve:

- `Rates & Deaths_*.csv` output structure
- `Rates & Deaths(ADJUSTED)_*.csv` compatibility for downstream adjustment functions
- `samples_combined/*.rda` names that downstream code expects
- older-children country/world/regional formulas

The main intentional cleanup is that cache-file expectations should match actual files written by older-children code. The refactor should remove stale requirements rather than creating unused files just to satisfy old checks.

## Implementation Phases

### Phase 1: Guardrails

- add regression coverage around the current `run.outputaggregates.5.24()` and `run.outputaggregates.5.24.gender()` paths
- add targeted tests for helper-name isolation and cache contracts

### Phase 2: Helper Isolation

- rename any remaining generic older-children helpers to `5_24`-specific names
- isolate preprocess and cache-list helpers without changing behavior

### Phase 3: Orchestration Refactor

- convert `OutputAggregates.ori()` into a coordinator with explicit phase helpers
- add timing summary output

### Phase 4: Regional Dispatch Refactor

- replace the repeated `if (is.element(...))` block with declarative regional specs
- preserve current region families and filenames

### Phase 5: Regional Runner Refactor

- add bundle existence checks and retry logic
- keep formula math unchanged

### Phase 6: Full Verification

- run older-children total and gender wrappers from an `app.R`-sourced session
- confirm downstream adjustment still succeeds

## Testing and Verification

Minimum verification for this work:

- targeted regression tests for helper-name isolation
- targeted regression tests for regional combine dispatch
- `Rscript -e "source('app.R'); run.outputaggregates.5.24(year.lastestimatepublished)"`
- `Rscript -e "source('app.R'); run.outputaggregates.5.24.gender(year.lastestimatepublished)"`
- `Rscript -e "source('app.R'); run.outputaggregates.5.24(year.lastestimatepublished); run.outputaggregates.5.24.gender(year.lastestimatepublished); adjust.total.death.5.24()"`

Where practical, intermediate checks should also verify:

- generated cache files match declared cache lists
- regional bundle files exist for every trajectory
- Adhoc hierarchical grouping still writes `AdhocCountries` outputs

## Risks and Mitigations

### Risk: silent output drift

Mitigation:

- keep formulas untouched while refactoring orchestration
- compare key generated outputs before and after on representative runs

### Risk: helper collisions after `app.R` sourcing

Mitigation:

- suffix older-children orchestration helpers with `5_24`
- keep under-five helpers on `BWC` names

### Risk: cache invalidation bugs

Mitigation:

- centralize file-list definitions
- make phase runners load and write through those helpers only

### Risk: oversized refactor becomes hard to review

Mitigation:

- implement in phases
- keep public entry points stable
- add regression checks before large movement

## Recommended Outcome

After this refactor, `OutputAggregates.ori()` should look and behave like the non-BWC architectural sibling of under-five `OutputAggregates()`: easier to read, safer to source, easier to test, and easier to extend, while still remaining an older-children-specific implementation.
