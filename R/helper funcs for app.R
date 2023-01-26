# helper.R for shiny

#' function to read "Rates & Deaths_Country Summary.csv" and output long format
#' `OfficialName` column named as `Region` for easier binding later in the
#' downloaded file
#' 
read.country.summary <- function(
  dir_dt_cs,      # fread("Rates & Deaths_Country Summary.csv)
  year_wanted = NULL, # e.g. c(1990:2019)
  sex = NULL
){    
  if(!file.exists(dir_dt_cs)) stop("File doesn't exist: ", dir_dt_cs)
  dt_cs <- fread(dir_dt_cs)[ISO3Code!="LIE"]
  setnames(dt_cs, gsub(" ", ".", colnames(dt_cs)))
  setnames(dt_cs, gsub("-", ".", colnames(dt_cs)))

  # find the Quantile column:
  if("Quintile"%in%colnames(dt_cs))setnames(dt_cs, "Quintile", "Quantile")
  if("X"%in%colnames(dt_cs)){
    if(dt_cs$X[1]!=1) setnames(dt_cs, "X", "Quantile") # it could be a indexing column 
  }
  # in case leave as blank, column will have names like V99, V101, etc
  if(!"Quantile"%in%colnames(dt_cs)){
    columnV <- grep("V", colnames(dt_cs), value = TRUE)
    columnV <- columnV[which(nchar(columnV) %in% c(3,4))]
    # message("Assign this column as Quantile column: ", columnV[1])
    setnames(dt_cs, columnV[1], "Quantile")
  }
  
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
  vars_id <- if("Quantile"%in%colnames(dt_cs))c("OfficialName", "Quantile") else "OfficialName"
  vars_wanted <- do.call(paste0, expand.grid(vars, ".", year_wanted))
  dt_cs <- dt_cs[, c(vars_id, vars_wanted), with = FALSE]
  dt_cs[, (vars_wanted):=lapply(.SD, as.numeric), .SDcols = vars_wanted]
  dt_long <- melt.data.table(dt_cs, id.vars = vars_id,
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
  dt_long[, ind:= dplyr::recode(ind, !!!new_varname_list)]
  setnames(dt_long, "OfficialName", "Region")
  setnames(dt_long, "year", "Year")
  if("Quantile"%in%colnames(dt_long)) dt_long <- dt_long[Quantile=="Median"]
  dt_wide <- dcast.data.table(dt_long, Region + Year + Sex ~ ind)
  vars_new <- dplyr::recode(vars, !!!new_varname_list)
  setcolorder(dt_wide, c("Region", "Year", "Sex", vars_new))
  setorder(dt_wide, Region, -Year) 
  return(dt_wide)
}

#' function to read "Rates & Deaths_(region name).csv" and output long format
#' data
read.region.summary <- function(
  dir_file,      # regional summary in aggregate results
  year_range = NULL,# e.g. c(1990, 2019)
  sex = NULL,
  add_regional_grouping = FALSE
){
  if(!file.exists(dir_file)) stop("File doesn't exist: ", dir_file)
  dt_cs <- fread(dir_file)
  # What region is it?
  
  if(grepl("SDG", dir_file)) Regional_Grouping <- "SDG"
  if(grepl("UNICEF", dir_file)) Regional_Grouping <- "UNICEF"
  if(grepl("WB", dir_file)) Regional_Grouping <- "World Bank"
  
  # clean up the col names
  setnames(dt_cs, gsub(" ", ".", colnames(dt_cs)))
  setnames(dt_cs, gsub("-", ".", colnames(dt_cs)))
  #
  vars0 <- colnames(dt_cs)
  vars0 <- vars0[!grepl("Population", vars0, ignore.case = TRUE)]
  vars_wanted <- vars0[!vars0%in%c("Region", "Year")]
  # available years
  available_years <- sort(unique(dt_cs$Year))
  if(!is.null(year_range)){
    # so it is OK to supply years by mistake like year_range = 2000.4, match by
    # flooring
    year_range <- available_years[floor(available_years)%in%floor(as.numeric(year_range))]
    if(length(year_range)==0){
      message("Supplied `year_range` is not in available years.\n",
              "Available years are between ", paste(range(available_years), collapse = " and "),
              " --- will use all available years")
      year_range <- available_years
    }
  } else {
    message("`year_range` set to NULL: use all available years in the dataset: ", paste(range(available_years), collapse = "-"))
    year_range <- available_years
  }
  dt_cs <- dt_cs[Year%in%year_range]
  dt_cs[, (vars_wanted):=lapply(.SD, as.numeric), .SDcols = vars_wanted]
  if("Region" %in% colnames(dt_cs)){
    id_vars <- c("Region", "Year")
  } else {
    id_vars <- c("Year")
    message("There is no `Region` column, assume this is world results")
  }
  dt_long <- melt.data.table(dt_cs[,..vars0], id.vars = id_vars, variable.factor = FALSE)
  dt_long[grepl("upper", variable, ignore.case = TRUE), Quantile := "Upper"]
  dt_long[grepl("median", variable, ignore.case = TRUE), Quantile := "Median"]
  dt_long[grepl("lower", variable, ignore.case = TRUE), Quantile := "Lower"]
  dt_long[, Shortind := gsub("X|.lower.bound|.upper.bound|.median", "", variable)]
  dt_long[, Shortind := gsub("Deaths", "deaths", Shortind)]
  
  dt_long[, variable:= NULL]
  if(add_regional_grouping) dt_long[, Regional_Grouping:= Regional_Grouping]
  dt_long[, Year:= floor(Year) + 0.5]
  # determine sex from dir
  if(is.null(sex)){
    if(grepl("female", dir_file, ignore.case = TRUE)){
      sex <- "Female"
    } else if (grepl("male", dir_file, ignore.case = TRUE)) {
      sex <- "Male"
    } else {
      sex <- "Total"
    }
  }
  dt_long[, Sex:= sex]
  return(dt_long)
}

#' select, rename columns and define some format based on output of adhoc region
#' by default add Sex = "Total"
#' 
clean.table <- function(dt){
  if(is.null(dt)) return(NULL)
  if(!"Sex" %in% colnames(dt)) dt[, Sex := "Total"]
  # remove some columns
  dt <- dt[, -(grep("Population|population", colnames(dt), value = TRUE)), with = FALSE]
  dt <- dt[Year>=year_started]
  setcolorder(dt, c("Region", "Year", "Sex")) # 2020.09 add Sex
  dt[, Region2 := tolower(Region)]
  setorder(dt, Region2, -Year) 
  dt[, Region2 := NULL]
  # rename 
  colnames(dt) <- dplyr::recode(colnames(dt), !!!new_varname_list)
  return(dt)
}

# scales::show_col(cp_UNICEF_div)
cp_UNICEF_div = c("#002759", "#0058AB", "#1CABE2", "#69DBFF", "#CFF4FF", "#FFF09C",
                             "#FFC20E", "#F26A21", "#E2231A", "#B50800")

# for revising the country names for the map
new_country_name_list <- c(
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

get.world.map <- function(){
  world <- maps::map("world", fill=TRUE, plot=FALSE)
  world_map <- maptools::map2SpatialPolygons(world, sub(":.*$", "", world$names))
  world_map <- sp::SpatialPolygonsDataFrame(world_map, data.frame(country = names(world_map), 
                                                                  stringsAsFactors = FALSE), match.ID = FALSE)
  # rename world map country names correctly using official names 
  world_map$country <- dplyr::recode(world_map$country, !!!new_country_name_list)
  world_map
}

# revise SDG region names 
SDG_name_list <- c(
  "Western Asia and Northern Africa" = "Northern Africa and Western Asia",
  "Eastern Asia and South-eastern Asia" = "Eastern and South-Eastern Asia",
  "Central Asia and Southern Asia" = "Central and Southern Asia",
  "Landlocked developing countries (LLDCs)" = "Landlocked developing countries",
  "Least developed countries (LDCs)" = "Least developed countries",
  "Small island developing States (SIDS)" = "Small island developing States",
  "Northern America and Europe" = "Europe and Northern America",
  "South-eastern Asia" = "South-Eastern Asia"
)

# a list used to rename column names in output and plot
new_varname_list <- c(
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
  "Neonatal.Deaths"   = "Neonatal Deaths",
  
  "X10q5"   =   "Mortality rate age 5-14",
  "X5q5"    =   "Mortality rate age 5-9",
  "X5q10"   =   "Mortality rate age 10-14",
  "X10q15"  =   "Mortality rate age 15-24",
  "X5q15"   =   "Mortality rate age 15-19",
  "X5q20"   =   "Mortality rate age 20-24",
  "X10q10"  =   "Mortality rate age 10-19",
  "X20q5"   =   "Mortality rate age 5-24"
)

# required package version, will update library if version is too low. Could add
# more if needed.
package_list <- c(
  "shiny" = "1.4.0",
  "DT" = "0.9",
  "data.table" = "1.12.6"
)

update.package.version <- function(pkg){
  if (packageVersion(pkg) < dplyr::recode(pkg, !!!package_list)) install.packages(pkg)
}

check.dir.exists <- function(dir0){
  if(!dir.exists(dir0)) stop("Check if the directory exists: ", dir0)
}

recode_ind_5_14 <- function(dt){
  recode5_14 <- c("U5MR" = "X10q5", "Under-five Mortality Rate" = "X10q5",
                  "IMR"  = "X5q5",  "Infant Mortality Rate" = "X5q5",
                  "CMR"  = "X5q10",
                  "10q5" = "X10q5", "5q5" = "X5q5", "5q10" = "X5q10",
                  "Mortality rate age 5-14" = "X10q5",
                  "Mortality rate age 5-9"  = "X5q5",
                  "Mortality rate age 10-14"= "X5q10",
                  "Under.five.deaths" = "Deaths age 5 to 14",
                  "Infant.deaths"     = "Deaths age 5 to 9",
                  "Child.deaths"      = "Deaths age 10 to 14",
                  "Under-five deaths" = "Deaths age 5 to 14",
                  "Infant deaths"     = "Deaths age 5 to 9",
                  "Child deaths"      = "Deaths age 10 to 14",
                  "Deaths.age.5to14"  = "Deaths age 5 to 14",
                  "Deaths.age.5to9"   = "Deaths age 5 to 9",
                  "Deaths.age.10to14" = "Deaths age 10 to 14"
  )
  if("Shortind" %in% colnames(dt)) dt[, Shortind := dplyr::recode(Shortind, !!!recode5_14)]
  col_new <- dplyr::recode(colnames(dt), !!!recode5_14)
  setnames(dt, col_new)
  return(dt)
}

recode_ind_15_24 <- function(dt){
  # might need to revise every year, depending on the column names used in the
  # agg file
  recode15_24 <- c("U5MR"  = "X10q15", "Under-five Mortality Rate" = "X10q15", 
                   "IMR"   = "X5q15",  "Infant Mortality Rate" = "X5q15",
                   "CMR"   = "X5q20",
                   "10q15" = "X10q15", 
                   "5q15" = "X5q15", 
                   "5q20" = "X5q20",
                   "Mortality rate age 15-24"= "X10q15",
                   "Mortality rate age 15-19"= "X5q15",
                   "Mortality rate age 20-24"= "X5q20",
                   "Under.five.deaths" = "Deaths age 15 to 24",
                   "Infant.deaths"     = "Deaths age 15 to 19",
                   "Child.deaths"      = "Deaths age 20 to 24",
                   "Under-five deaths" = "Deaths age 15 to 24",
                   "Infant deaths"     = "Deaths age 15 to 19",
                   "Child deaths"      = "Deaths age 20 to 24",
                   "Deaths.age.15to24" = "Deaths age 15 to 24",
                   "Deaths.age.15to19" = "Deaths age 15 to 19",
                   "Deaths.age.20to24" = "Deaths age 20 to 24"
  )
  if("Shortind" %in% colnames(dt)) dt[, Shortind := dplyr::recode(Shortind, !!!recode15_24)]
  col_new <- dplyr::recode(colnames(dt), !!!recode15_24)
  setnames(dt, col_new)
  return(dt)
}

calculate.10q10 <- function(dt){
  get.5q0 <- function(q1, q4){(1 - (1-q1/1E3) * (1-q4/1E3))*1E3}
  get.cme <- function(q1, q5){(1-(1-q5/1000)/(1-q1/1000)) * 1000}
  dt[, `:=`(X10q10 = get.5q0(X5q10, X5q15),
            X20q5  = get.5q0(X10q5, X10q15),
            `Deaths age 10 to 19` = `Deaths age 10 to 14` + `Deaths age 15 to 19`,
            `Deaths age 5 to 24`  = `Deaths age 5 to 14`  + `Deaths age 15 to 24`)]
  return(dt)
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