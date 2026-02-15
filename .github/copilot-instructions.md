# Shiny Regional Aggregate - Project Context

## Project Overview
This is a Shiny web application that produces regional aggregates of child mortality estimates based on individually selected countries. It uses UN IGME (United Nations Inter-agency Group for Child Mortality Estimation) data to calculate and visualize child mortality indicators including:
- Neonatal Mortality Rate (NMR)
- Infant Mortality Rate (IMR)
- Under-Five Mortality Rate (U5MR)
- Mortality rates for older children and adolescents (5-14, 15-24 age groups)
- Sex-specific mortality rates (male/female)

**Deployed at**: https://unicef-dapm.shinyapps.io/Shiny_regional_aggregate/

**Organization**: UNICEF Data and Analytics Section

## Tech Stack
- **Primary Language**: R
- **Framework**: Shiny (web application framework)
- **Key Libraries**:
  - `shiny`, `shinyWidgets`, `shinyjs` - UI and interactivity
  - `leaflet`, `maps`, `sf` - Geographic visualization
  - `DT`, `data.table`, `dplyr` - Data manipulation and display
  - `ggplot2`, `scales`, `plotly` - Data visualization
  - `readxl` - Excel file reading
  - `here` - Path management

## Project Structure

### Core Files
- **`app.R`**: Main Shiny application entry point
- **`update_me_every_year.R`**: Annual update script for refreshing mortality data
- **`DESCRIPTION`**: R package metadata

### Key Directories
- **`R/`**: Contains all R source code modules
  - `6outputaggregates.R` - Main aggregation logic
  - `6outputaggregates_gender.R` - Sex-specific aggregations
  - `6outputaggregates_5_24.R` - Older children/adolescent aggregations
  - `outputaggregates-BWC.R` - Birth-weighted cohort aggregation method
  - `about.R`, `chooseregion.R`, `constructoutput.R` - UI components
  - `headerPanel.R`, `summariseresults.R` - Additional modules

- **`input/`**: Source data files
  - Country information CSV files (CME data for different age groups)
  - Population data from UN World Population Prospects (WPP2024)
  - Live births data (total, male, female)

- **`median_results_*/`**: Pre-calculated median mortality estimates
  - Separate folders for male, female, total, and different age groups (5-14, 15-24)
  - Contains "Rates & Deaths" summary CSV files

- **`output/`**: Computed results by mortality indicator
  - Folders: IMR/, NMR/, U5MR/, 5q5/, 5q15/, 10q5/, 10q15/
  - Sex-specific results in `Sex_forDeathCalculation/`

- **`www/`**: Web assets and example files
  - Bootstrap CSS
  - Example ISO3 code upload files

## Data Flow

1. **Input Data Loading**: App loads country information and mortality estimates from `input/` and `median_results_*/` directories
2. **Country Selection**: Users select countries either through dropdown UI or by uploading ISO3 code CSV files
3. **Aggregation**: Selected countries' data is aggregated using birth-weighted cohort (BWC) method
4. **Visualization**: Results displayed as:
   - Interactive plots (Plotly)
   - Data tables (DT)
   - Maps (Leaflet)
5. **Export**: Users can download aggregated results as CSV files

## Key Conventions

### Naming Conventions
- **Variables**: 
  - `dc` = country info dataset
  - `dc.5.14`, `dc.15.24` = age-specific datasets
  - Mortality indicators use standard abbreviations: `NMR`, `IMR`, `U5MR`, `5q5`, `10q5`, etc.
- **Functions**: Descriptive lowercase with dots (e.g., `check.and.install.pkgs`)
- **Files**: Numbered prefixes indicate execution order (e.g., `6outputaggregates.R`)

### Region Classifications
- **UNICEF regions**: Based on `UNICEFReportRegion1` and `UNICEFReportRegion2`
- **SDG regions**: Sustainable Development Goals regions
- **M49 regions**: UN M49 standard country/area codes

### Mortality Indicators
- **NMR**: Neonatal Mortality Rate (deaths in first 28 days per 1,000 live births)
- **IMR**: Infant Mortality Rate (deaths under 1 year per 1,000 live births)
- **U5MR**: Under-Five Mortality Rate (deaths under 5 years per 1,000 live births)
- **5q5**: Probability of dying between ages 5 and 10
- **10q5**: Probability of dying between ages 5 and 15
- **5q15**: Probability of dying between ages 15 and 20
- **10q15**: Probability of dying between ages 15 and 25

## Aggregation Method (BWC)
The app uses the **Birth-Weighted Cohort (BWC)** method to aggregate country-level estimates into regional estimates. This method weights each country's mortality rate by its birth cohort size, ensuring larger populations have proportional influence on regional estimates.

## Annual Update Process

**Important**: Use `update_me_every_year.R` script to update the application with new UN IGME data releases.

Key update steps:
1. Update input datasets (country info, population, live births)
2. Update median results files
3. Refresh output results
4. Test the app locally
5. Deploy to shinyapps.io

## File Upload Format
Users can upload custom country selections via CSV files with ISO3 codes. Example files provided in `www/` directory:
- Must contain ISO3 country codes
- Can define single or multiple custom regions
- Examples: Arab countries, ASEAN, GAVI countries, World Bank income classifications

## Development Guidelines

### Running Locally
```r
# Clone the repository
git clone https://github.com/unicef-drp/Shiny_regional_aggregate.git

# Open the .Rproj file or run directly
shiny::runApp()
```

### Code Organization
- Keep UI components in separate modules under `R/`
- Source all helper functions before running the app
- Use `here::here()` for all file paths to ensure portability
- Follow the existing pattern for adding new aggregation types

### Error Handling
- `options(shiny.sanitize.errors = TRUE)` is set to avoid exposing system details
- Validate user uploads for correct ISO3 code format
- Provide clear user-facing error messages

## Important Notes
- The repository is private on GitHub
- Data is sourced from UN IGME official estimates (https://childmortality.org)
- Map disclaimer: "This map is stylized and not to scale and does not reflect a position by UNICEF on the legal status of any country or territory..."
- Default selections and example regions are defined in the app initialization

## Typical Code Patterns

### Loading Data
```r
dc <- fread(here::here("input/country.info.CME.csv"))
```

### Region Definitions
```r
dc[, UNICEF_region := ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
```

### Package Management
```r
check.and.install.pkgs <- function(pkgs) {
  # Check, install if needed, and load packages
}
```

## Performance Optimization

**BWC Method Analysis**: See [BWC_ANALYSIS.md](../BWC_ANALYSIS.md) for detailed explanation of:
- How the Birth Week Cohort (BWC) aggregation method works
- Performance hotspots and optimization opportunities
- Minimal patches to improve runtime by 15-100×

Key bottlenecks to be aware of:
- Repeated file I/O in trajectory loops (critical)
- Nested country × year × 52-week calculations
- Regional aggregation with cohort-based life tables

## Contact
**Maintainer**: Yang Liu (yanliu@unicef.org)
**Organization**: UNICEF Data Analytics and Planning Monitoring Section
