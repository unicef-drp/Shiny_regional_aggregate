# helper.R for shiny

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
  "U5MR median" = "Under-five Mortality Rate",
  "IMR median"  = "Infant Mortality Rate",
  "NMR median"  = "Neonatal Mortality Rate",
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