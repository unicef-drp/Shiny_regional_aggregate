# Minimal example for installing shinyregionalaggregate from GitHub and running
# get_CME_aggregate() without launching the Shiny app.

cran_packages <- c("data.table", "remotes")

for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

options(timeout = 600)
options(download.file.method = "libcurl")

if (!requireNamespace("shinyregionalaggregate", quietly = TRUE)) {
  remotes::install_github("unicef-drp/Shiny_regional_aggregate", force = TRUE)
}

library("data.table")
library("shinyregionalaggregate")

example_input <- fread(
	"https://raw.githubusercontent.com/unicef-drp/Country-and-Region-Metadata/refs/heads/main/output/AUREC.csv"
)

dt_agg_out <- get_CME_aggregate(example_input)

print(dt_agg_out)
