# helper.R for shiny

#' function to read "Rates & Deaths_Country Summary.csv" and output long format
read.country.summary <- function(
  dir_dt_cs,      # fread("Rates & Deaths_Country Summary.csv)
  year_wanted = NULL, # e.g. c(1990:2019)
  sex = NULL
){    
  if(!file.exists(dir_dt_cs)) stop("File doesn't exist: ", dir_dt_cs)
  dt_cs <- fread(dir_dt_cs)[ISO3Code!="LIE"]
  setnames(dt_cs, gsub(" ", ".", colnames(dt_cs)))
  setnames(dt_cs, gsub("-", ".", colnames(dt_cs)))
  # find the Quantie column: 
  if("X"%in%colnames(dt_cs))setnames(dt_cs, "X", "Quantile")
  if("V99"%in%colnames(dt_cs))setnames(dt_cs, "V99", "Quantile") # in case leave as blank
  if("Quintile"%in%colnames(dt_cs))setnames(dt_cs, "Quintile", "Quantile")
  # get all the variables available in the datasets: 
  # e.g. c("X5q15", "X10q15", "X5q20")
  vars <- grep(".2018", colnames(dt_cs), value = TRUE, fixed = TRUE)
  vars <- gsub(".2018", "", vars)
  # available years
  year_available <- grep(vars[1], colnames(dt_cs), value = TRUE, fixed = TRUE)
  year_available <- as.numeric(gsub(paste0(vars[1], "."), "", year_available))
  if(is.null(year_wanted)){
    year_wanted <- year_available
  } else {
    year_wanted <- year_wanted[year_wanted%in%year_available]
  }
  vars_wanted <- do.call(paste0, expand.grid(vars, ".", year_wanted))
  dt_cs <- dt_cs[, c("OfficialName", "Quantile", vars_wanted), with = FALSE]
  dt_cs[, (vars_wanted):=lapply(.SD, as.numeric), .SDcols = vars_wanted]
  dt_long <- melt.data.table(dt_cs, id.vars = c("OfficialName", "Quantile"),
                             variable.factor = FALSE)
  dt_long[, year := as.numeric(substr(variable, nchar(variable)-3, nchar(variable)))]
  dt_long[, ind := substr(variable, 1, nchar(variable)-5)]
  dt_long[, variable:= NULL]
  # determine sex from dir
  if(is.null(sex)){
    if(grepl("female", dir_dt_cs)){
      sex <- "Female"
    } else if (grepl("male", dir_dt_cs)) {
      sex <- "Male"
    } else {
      sex <- "Total"
    }
  }
  dt_long[, Sex:= sex]
  dt_long[, ind:=revise.name(ind, new_list = new_varname_list)]
  setnames(dt_long, "OfficialName", "Region")
  setnames(dt_long, "year", "Year")
  dt_long <- dt_long[Quantile=="Median"]
  dt_wide <- dcast.data.table(dt_long, Region + Year + Sex ~ ind)
  vars_new <- revise.name(vars, new_list = new_varname_list)
  setcolorder(dt_wide, c("Region", "Year", "Sex", vars_new))
  return(dt_wide)
}

#' a function to select columns and define some format based on output of adhoc region
get.table <- function(dt){
  if(is.null(dt)) return(NULL)
  # remove some columns
  dt <- dt[, -(grep("Population|population", colnames(dt), value = TRUE)), with = FALSE]
  dt <- dt[Year>=year_started]
  setcolorder(dt, c("Region", "Year", "Sex")) # 2020.09 add Sex
  dt[, Region2:= tolower(Region)]
  setorder(dt, Region2, -Year) 
  dt[, Region2:= NULL]
  # rename 
  colnames(dt) <- revise.name(colnames(dt), new_list = new_varname_list)
  return(dt)
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
  "Neonatal deaths median"   = "Neonatal Deaths",
  "Under.five.Deaths" = "Under-five Deaths",
  "Infant.Deaths"     = "Infant Deaths",
  "Neonatal.Deaths"   = "Neonatal Deaths"
)

# required package version
package_list <- list(
  "shiny" = "1.4.0",
  "DT" = "0.9",
  "data.table" = "1.12.6"
)

update.package.version <- function(pkg){
  if (packageVersion(pkg)< revise.name(pkg, new_list = package_list)) install.packages(pkg)
}

check.dir.exists <- function(dir0){
  if(!dir.exists(dir0)) stop("Check if the directory exists: ", dir0)
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