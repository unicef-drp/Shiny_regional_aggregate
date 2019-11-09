# helper.R for shiny

# runname.U5MR <- "GR20190311_all"
# runname.IMR <- "IMR20190314_all"
# runname.NMR <- "NMR_forDeathCalculation"

# Load results data -------------------------------------------------------

#' get three types (Under-five Infant Neonatal) from Rates & Deaths summary
#' @import data.table
#' @importFrom readr parse_number
#' @param year_range a vector of years we want, default to 2000:2018
#' @param get_what "Deaths" or "Rate", default to "Rate": get the three CME rate
#' @examples
#' get.CME.data(year_range = c(2016:2018))
#' @export get.CME.data
#' @return dt of ISO3, UNcode, year, Under-five, Infant, Neonatal, one row for each country each year in year_range
#'
get.CME.data <- function(dt, year_range = c(2000:2018), get_what = "Rate", gender = FALSE){
  # dt <- Rates_Deaths_Country_Summary_2019
  available_years <- readr::parse_number(grep("IMR", names(dt), value = TRUE))
  if (!all(year_range%in%available_years)) {
    warning("Available years are between: ", paste(range(available_years), collapse = " and "),
            ". Set years to default range.")
    year_range <- c(2000:2018) # set to default range
  }
  
  if(get_what == "Rate") {
    CME_types_full <- if (gender) c("U5MR", "IMR") else c("U5MR", "IMR", "NMR")
    vars_wanted <- c("OfficialName",
                     do.call(paste, expand.grid(CME_types_full, year_range)))
  } else {
    CME_types_full <- if (gender) c("Under-five", "Infant") else c("Under-five", "Infant", "Neonatal")
    # get all the combination for variable names: e.g. Under-five Deaths 2000, 19*3 = 57 variables
    vars_wanted <- c("OfficialName",
                     do.call(paste, expand.grid(CME_types_full, "Deaths", year_range)))
  }
  # melt by CME_types_full using `patterns`
  dt_death_long <- data.table::melt(dt[,..vars_wanted],
                                    measure = patterns(paste0("^", CME_types_full)),
                                    value.name = CME_types_full, variable.name = "year")
  levels(dt_death_long$year) <- year_range
  dt_death_long[, year:=as.numeric(levels(year)[year])]
  dt_death_long[order(OfficialName)]
  return(dt_death_long)
}


get.data.all <- function(file, gender0 = FALSE, year_started, year_ended){
  d1 = get.CME.data(fread(file), year_range = c(year_started:year_ended), "Rate", gender = gender0)
  d2 = get.CME.data(fread(file), year_range = c(year_started:year_ended), "Deaths", gender = gender0)
  setkey(d1, OfficialName, year)
  setkey(d2, OfficialName, year)
  d12 <- d1[d2]
  setnames(d12, revise.name(names(d12), new_list = new_varname_list), skip_absent = TRUE)
  setnames(d12, "OfficialName", "Region")
  return(d12[order(Region, -Year),])
}


cp_UNICEF_div = c("#002759", "#0058AB", "#1CABE2", "#69DBFF", "#CFF4FF", "#FFF09C",
                        "#FFC20E", "#F26A21", "#E2231A", "#B50800")

revise.name <- function(x, new_list = NULL, no_line_break = FALSE){
    default_labels <- list(
      "Antigua" = "Antigua and Barbuda",
      "Bolivia" = "Bolivia (Plurinational State of)",
      "Brunei" = "Brunei Darussalam",
      "Cape Verde" = "Cabo Verde",
      "Ivory Coast" = "Cote d'Ivoire",
      "Czech Republic" = "Czechia",
      "North Korea" = "Democratic People's Republic of Korea",
      "Republic of Congo" = "Congo",
      "Swaziland" = "Eswatini",
      "Iran" = "Iran (Islamic Republic of)",
      "Laos" = "Lao People's Democratic Republic",
      "Micronesia" = "Micronesia (Federated States of)",
      "South Korea" = "Republic of Korea",
      "Moldova" = "Republic of Moldova",
      "Macedonia" = "Republic of North Macedonia",
      "Russia" = "Russian Federation",
      "Saint Kitts" = "Saint Kitts and Nevis",
      "Saint Vincent" = "Saint Vincent and the Grenadines",
      "Palestine" = "State of Palestine",
      "Syria" = "Syrian Arab Republic",
      "Tobago" = "Trinidad and Tobago",
      "Trinidad" = "Trinidad and Tobago",
      "Tanzania" = "United Republic of Tanzania",
      "Venezuela" = "Venezuela (Bolivarian Republic of)",
      "Vietnam" = "Viet Nam",
      "UK" = "United Kingdom", 
      "USA" = "United States of America",
      "Republic of Congo" = "Congo"
    )
    if(is.null(new_list)){
      labs <- default_labels
    } else {
      if(is.list(new_list)){
        labs <- new_list
      } else {
        message("new_list must be a list. Still use the default list.")
        labs <- default_labels
      }
    }
    if(!is.character(x)){
      message("Coerse input into character.")
      x <- as.character(x)
    }
    out <- rep(NA, length(x))
    for (i in 1:length(x)){
      if (is.null(labs[[ x[i] ]])){
        out[i] <- x[i]
      }else{
        out[i] <- labs[[ x[i] ]]
      }
    }
    return(if(no_line_break) gsub("\n", "", out) else out)
}


get.world.map <- function(){
  world <- maps::map("world", fill=TRUE, plot=FALSE)
  world_map <- maptools::map2SpatialPolygons(world, sub(":.*$", "", world$names))
  world_map <- sp::SpatialPolygonsDataFrame(world_map, data.frame(country = names(world_map), 
                                                                  stringsAsFactors = FALSE), match.ID = FALSE)
  # rename world map country names correctly using official names 
  world_map$country <- revise.name(world_map$country)
  world_map
}

SDG_list <- list(
  "Western Asia and Northern Africa" = "Northern Africa and Western Asia",
  "Eastern Asia and South-eastern Asia" = "Eastern and South-Eastern Asia",
  "Central Asia and Southern Asia" = "Central and Southern Asia",
  "Landlocked developing countries (LLDCs)" = "Landlocked developing countries",
  "Least developed countries (LDCs)" = "Least developed countries",
  "Small island developing States (SIDS)" = "Small island developing States",
  "Northern America and Europe" = "Europe and Northern America",
  "South-eastern Asia" = "South-Eastern Asia"
)

new_varname_list <- list(
  "year" = "Year",
  "U5MR" = "Under-five Mortality Rate",
    "IMR" = "Infant Mortality Rate",
    "NMR" = "Neonatal Mortality Rate",
  "U5MR median" = "Under-five Mortality Rate",
  "IMR median"  = "Infant Mortality Rate",
  "NMR median"  = "Neonatal Mortality Rate",
  
  "Under-five" = "Under-five Deaths",
  "Infant"     = "Infant Deaths",
  "Neonatal"   = "Neonatal Deaths",
  "Under-five deaths median" = "Under-five Deaths",
  "Infant deaths median"     = "Infant Deaths",
  "Neonatal deaths median"   = "Neonatal Deaths"
)

package_list <- list(
  "shiny" = "1.4.0",
  "DT" = "0.9",
  "data.table" = "1.12.6"
)

update.package.version <- function(pkg){
  if (packageVersion(pkg)< revise.name(pkg, new_list = package_list)) install.packages("shiny")
}


# get.results.file <- function(runname, pattern0 = "Results"){
#   files <- list.files(here::here("output", runname), full.names = TRUE, pattern = pattern0)
# }
# 
# # produce a master datafile that combine all the output in `output` folder
# read.results.file <- function(year_started, year_ended){
#   dcname <- fread(here::here("input", "country.info.CME.csv"))[,.(ISO3Code, OfficialName)]
#   setkey(dcname, ISO3Code)
#   years <- paste0("X", c(year_started:year_ended), ".5")
# 
#   files <- unlist(lapply(c(runname.U5MR, runname.IMR, runname.NMR, "Sex_forDeathCalculation"), get.results.file))
#   filelists <- lapply(files, fread)
#   all <- rbindlist(filelists)
#   
#   setkey(all, ISO.Code)
#   all <- dcname[all]
#   all_selected <- all[Quantile == "Median"][, c("OfficialName", "Indicator", years), with = FALSE]
#   setnames(all_selected, years, as.character(c(year_started:year_ended)))
#   all_long <- melt.data.table(all_selected, measure.vars = as.character(c(year_started:year_ended)),
#                               variable.name = "Year")
#   all_wide <- dcast.data.table(all_long, OfficialName + Year  ~ Indicator, value.var = "value")
#   setnames(all_wide, "OfficialName", "Region")
#   return(all_wide)
# }