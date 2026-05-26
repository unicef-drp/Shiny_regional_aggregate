ensure_cran_package <- function(package) {
	if (!requireNamespace(package, quietly = TRUE)) {
		install.packages(package, repos = "https://cloud.r-project.org")
	}
}

ensure_github_package <- function(package, repo) {
	if (!requireNamespace(package, quietly = TRUE)) {
		ensure_cran_package("remotes")
		remotes::install_github(repo, upgrade = "never")
	}
}

ensure_cran_package("data.table")
ensure_github_package("shinyregionalaggregate", "unicef-drp/Shiny_regional_aggregate")

library(data.table)
library(shinyregionalaggregate)

example_input <- fread(
	"https://raw.githubusercontent.com/unicef-drp/Country-and-Region-Metadata/refs/heads/main/output/AUREC.csv"
)

dt_agg_out <- get_CME_aggregate(example_input)

print(dt_agg_out)
