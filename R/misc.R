#----------------------------------------------------------------------
# misc.R
# Miscellaneous functions
#----------------------------------------------------------------------
roundoff <- function(# Round off numbers in the conventional way
  x, digits
) {
  z <- trunc(abs(x)*10^digits + 0.5)
  z <- sign(x)*z/10^digits
  return(z)
}

roundoffmidpoint <- function(#Round off numbers in the conventional way but to the midpoint
  x
) {
  return(roundoff(x + 0.5, 0) - 0.5)
}

logit <- function( # Logit function
  p ##<< Value between 0 and 1
) {
  return(log(p/(1-p)))
}

invlogit <- function( # Inverse logit function
  x
) {
  return(exp(x)/(1+exp(x)))
}

GetCountryInfo <- function(# Get country names and/or ISO codes
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

approx.geometric <- function( # Do geometric interpolation given two vectors x and y and unit desired.
  x, y, 
  xoutleft = NULL, ##<< Minimum x value desired in result, default is min(x).
  xoutright = NULL, ##<< Maximum x value desired in result, default is max(x).
  unit = 0.1
) {
  order <- order(x)
  x <- x[order]
  y <- y[order]
  df <- data.frame(x0 = x[-length(x)], x1 = x[-1],
                   y0 = y[-length(y)], y1 = y[-1])        
  res <- data.frame(x = c(mapply(approx.geometric.inner, df$x0, df$x1, df$y0, df$y1, unit = unit, result = "x")), 
                    y = c(mapply(approx.geometric.inner, df$x0, df$x1, df$y0, df$y1, unit = unit, result = "y")))
  if (!is.null(xoutleft)) {
    x.temp <- approx.geometric.inner(x0 = x[1], x1 = x[2], y0 = y[1], y1 = y[2], xoutleft = xoutleft, result = "x")
    y.temp <- approx.geometric.inner(x0 = x[1], x1 = x[2], y0 = y[1], y1 = y[2], xoutleft = xoutleft, result = "y")
    res <- rbind(data.frame(x = x.temp, y = y.temp), res)    
  }
  if (!is.null(xoutright)) {
    x.temp <- approx.geometric.inner(x0 = x[length(x)-1], x1 = x[length(x)], y0 = y[length(x)-1], y1 = y[length(x)], 
                                     xoutright = xoutright, result = "x")
    y.temp <- approx.geometric.inner(x0 = x[length(x)-1], x1 = x[length(x)], y0 = y[length(x)-1], y1 = y[length(x)], 
                                     xoutright = xoutright, result = "y")
    res <- rbind(res, data.frame(x = x.temp, y = y.temp))  
  }
  # remove duplicate rows
  res <- unique(res)
  return(list(x = res$x, y = res$y))
} 

approx.geometric.inner <- function( # Do geometric interpolation given values x0, x1, y0, y1 and unit desired, 
  ## or linear interpolation if y0 = 0.
  ## Adapted from Philip Bastian.
  x0, x1, y0, y1, 
  xoutleft = x0, ##<< Minimum x value desired in result, default is x0.
  xoutright = x1, ##<< Maximum x value desired in result, default is x1.
  result, ##<< "x" or "y" for desired result to return, or "both" to return a list containing both x and y.
  unit = 0.1
) {
  r <- (y1/y0)^(1/(x1-x0))-1 # undefined value obtained if y0 == 0
  x <- seq(xoutleft, xoutright, unit)
  y <- y0*(1+r)^(x-x0)
  if (any(is.nan(y))) {
    y <- approx(x = c(x0, x1), y = c(y0, y1), xout = x, method = "linear", rule = 2)$y
    print("Note that Linear interpolation used!")
  }
  if (result == "x") {
    return(x)
  } else if (result == "y") {
    return(y)
  } else  if (result == "both") {
    return(list(x = x, y = y))
  }
}

SplitStringsAndExtract <- function(# Split vector of strings by pattern and extract the part of the string with the relevant index.
  x, ##<< Vector of strings 
  pattern, ##<< Pattern to split string by
  index ##<< Index of string to extract after splitting
) {
  SplitStringAndExtract <- function(string, pattern, index) {
    strsplit(string, pattern)[[1]][[index]]
  }
  sapply(x, SplitStringAndExtract, pattern = pattern, index = index)
}

LoadFile <- function( # Load file into specified environment.
  filename,  ##<< File name of file to load.
  output.dir,  ##<< Directory of file to load.
  envir ##<< Environment to load file into. 
) {
  if (!file.exists(file.path(output.dir, filename))) {
    cat(paste(file.path(output.dir, filename), "does not exist!\n"))
  } else {
    load(file.path(output.dir, filename), envir = envir)
  }
  ##value<< NULL
  return(invisible())
}
