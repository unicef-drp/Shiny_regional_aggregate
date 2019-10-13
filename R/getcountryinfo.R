#----------------------------------------------------------------------
# getcountryinfo.R
# Get country name and/or ISO codes
# Jin Rou New, 2013
#----------------------------------------------------------------------
GetCountryInfo <- function(
  data, ##<< Data frame with at least one column of either Country.Code, Country.ISO or Country.Name
  country.info.file = NULL
) {
  if (is.null(country.info.file)) 
    country.info.file <- "input/country.info.CME.csv"
  require(plyr)
  # read in country info with country codes
  country.info.readin <- read.csv(file = country.info.file, header = T, stringsAsFactors = F)
  # get country names and codes
  country.info <- country.info.readin[, is.element(names(country.info.readin), 
                                                   c("ISO3Code", "UNCode", "CountryName"))]
  names(country.info) <- c("Country.Code", "Country.ISO", "Country.Name")[
    match(names(country.info), c("ISO3Code", "UNCode", "CountryName"))]
  country.info <- rbind(country.info, c("SPS", 736, "Sudan pre secession"))
  # get Country.Name and Country.ISO
  res <- join(data, country.info)
  ##value<< List containing
  return(list(Country.Name = res$Country.Name, ##<< Country name. 
              Country.Code = res$Country.Code, ##<< Country 3-character ISO code.
              Country.ISO = res$Country.ISO)) ##<< Country 3-digit ISO code.
}
