#----------------------------------------------------------------------
# chooseregion.R
# Jin Rou New, 2012-2014
#----------------------------------------------------------------------
ChooseRegion <- function(
  region, 
  regiontype
) {
  if (regiontype == "UNICEFRegion") {
    r <- ChooseUNICEFRegion(region) 
  } else if (regiontype == "UNICEFRegionSSA") {
    r <- ChooseUNICEFRegionSSA(region)
  } else if (regiontype == "MDGRegion") {
    r <- ChooseMDGRegion(region)
  } else if (regiontype == "WBRegion") {
    r <- ChooseWBRegion(region)
  } else if (regiontype == "UNPDRegion") {
    r <- ChooseUNPDRegion(region)
  } else if (regiontype == "OICRegion") {
    r <- ChooseOICRegion(region)
  } else if (regiontype == "M49Region") {
    r <- ChooseM49Region(region)
  } else {
    stop("regiontype must be one of UNICEFRegion, UNICEFRegionSSA, MDGRegion, WBRegion, UNPDRegion, OICRegion or M49Region.")
  }
  return(r)
}
#----------------------------------------------------------------------
UNICEFRegionAll <- c("Africa",
                     "Sub-Saharan Africa",
                     "Eastern and Southern Africa",
                     "West and Central Africa",
                     "Middle East and North Africa",
                     "Middle East", "North Africa",
                     "Asia",
                     "South Asia",
                     "East Asia and Pacific",
                     "Latin America and the Caribbean",
                     "CEE/CIS",
                     "Industrialized countries",
                     "Developing countries",
                     "Least developed countries")
ChooseUNICEFRegion <- function(region) {
  UNICEFRegs1 <- c("Eastern and Southern Africa",
                   "West and Central Africa",
                   "Middle East and North Africa",
                   "South Asia",
                   "East Asia and Pacific",
                   "Latin America and the Caribbean",
                   "CEE/CIS",
                   "Industrialized countries")
  UNICEFRegs2 <- "Sub-Saharan Africa"
  UNICEFRegs3 <- c("Africa", "Asia")
  UNICEFRegs4 <- "Developing countries"
  UNICEFRegs5 <- "Least developed countries"
  UNICEFRegs6 <- c("Middle East", "North Africa")
  r <- ifelse(is.element(region, UNICEFRegs1), 1, 
              ifelse(is.element(region, UNICEFRegs2), 2, 
                     ifelse(is.element(region, UNICEFRegs3), 3,
                            ifelse(is.element(region, UNICEFRegs4), 4,
                                   ifelse(is.element(region, UNICEFRegs5), 5, 
                                          ifelse(is.element(region, UNICEFRegs6), 6,
                                                 NA))))))
  return(r)
}
#----------------------------------------------------------------------
UNICEFRegionAllSSA <- c("Africa",
                        "Sub-Saharan Africa",
                        "Eastern and Southern Africa",
                        "West and Central Africa",
                        "Middle East and North Africa",
                        "Middle East", "North Africa",
                        "Asia",
                        "South Asia",
                        "East Asia and Pacific",
                        "Latin America and the Caribbean",
                        "CEE/CIS",
                        "Industrialized countries",
                        "Developing countries",
                        "Least developed countries")
ChooseUNICEFRegionSSA <- function(region) {
  UNICEFRegs1 <- c("Eastern and Southern Africa",
                   "West and Central Africa",
                   "South Asia",
                   "East Asia and Pacific",
                   "Latin America and the Caribbean",
                   "CEE/CIS",
                   "Industrialized countries")
  UNICEFRegs2 <- c("Sub-Saharan Africa", "Middle East and North Africa")
  UNICEFRegs3 <- c("Africa", "Asia")
  UNICEFRegs4 <- "Developing countries"
  UNICEFRegs5 <- "Least developed countries"
  UNICEFRegs6 <- c("Middle East", "North Africa")
  r <- ifelse(is.element(region, UNICEFRegs1), 1, 
              ifelse(is.element(region, UNICEFRegs2), 2, 
                     ifelse(is.element(region, UNICEFRegs3), 3,
                            ifelse(is.element(region, UNICEFRegs4), 4,
                                   ifelse(is.element(region, UNICEFRegs5), 5, 
                                          ifelse(is.element(region, UNICEFRegs6), 6,
                                                 NA))))))
  return(r)
}
#----------------------------------------------------------------------
MDGRegionAll <- c("Developed regions", "Developing regions",
                  "Northern Africa", "Sub-Saharan Africa",
                  "Latin America and the Caribbean",
                  "Latin America", "Caribbean",
                  "Caucasus and Central Asia",
                  "Eastern Asia",
                  "Eastern Asia excluding China",
                  "Southern Asia",
                  "Southern Asia excluding India",
                  "South-eastern Asia",
                  "Western Asia",
                  "Oceania",
                  "Least developed countries")
ChooseMDGRegion <- function(region) {
  MDGRegs1 <- c("Developed regions",
                "Northern Africa", "Sub-Saharan Africa",
                "Latin America and the Caribbean",
                "Caucasus and Central Asia",
                "Eastern Asia",
                "Southern Asia",
                "South-eastern Asia",
                "Western Asia",
                "Oceania")
  MDGRegs1excl <- c("Eastern Asia excluding China", "Southern Asia excluding India")
  MDGRegs2 <- "Developing regions"
  MDGRegs3 <- c("North Africa", "South Africa", "Eastern Africa", "West Africa", "Central Africa")
  MDGRegs4 <- "Least developed countries"
  MDGRegs5 <- c("Latin America", "Caribbean")
  r <- ifelse(is.element(region, MDGRegs1), 1, 
              ifelse(is.element(region, MDGRegs1excl), "1excl",
                     ifelse(is.element(region, MDGRegs2), 2, 
                            ifelse(is.element(region, MDGRegs3), 3,
                                   ifelse(is.element(region, MDGRegs4), 4,
                                          ifelse(is.element(region, MDGRegs5), 5,
                                                 NA))))))
  return(r)
}
#----------------------------------------------------------------------
WBRegionAll <- c("Low income", "Middle income", 
                 "Lower middle income", "Upper middle income",
                 "Low and middle income", 
                 "East Asia and Pacific", "Europe and Central Asia",
                 "Latin America and the Caribbean", "Middle East and North Africa",
                 "South Asia", "Sub-Saharan Africa", 
                 "High income")
ChooseWBRegion <- function(region) {
  WBRegs1 <- c("Lower middle income", "Upper middle income")
  WBRegs2 <- c("East Asia and Pacific", "Europe and Central Asia",
               "Latin America and the Caribbean", "Middle East and North Africa",
               "South Asia", "Sub-Saharan Africa")
  WBRegs3 <- c("Low income", "Middle income", "High income")
  WBRegs4 <- "Low and middle income"
  r <- ifelse(is.element(region, WBRegs1), 1, 
              ifelse(is.element(region, WBRegs2), 2, 
                     ifelse(is.element(region, WBRegs3), 3,
                            ifelse(is.element(region, WBRegs4), 4,
                                   NA))))
  return(r)
}
#----------------------------------------------------------------------
UNPDRegionAll <- c("More developed regions", "Less developed regions",
                   "Least developed countries", "Excluding least developed countries", "Excluding China",
                   "Sub-Saharan Africa", "Africa", "Asia", "Europe", "Latin America and the Caribbean",
                   "Northern America", "Oceania")
ChooseUNPDRegion <- function(region) {
  UNPDRegs1 <- c("Africa", "Asia", "Europe", "Latin America and the Caribbean",
                 "Northern America", "Oceania")
  UNPDRegs2 <- "Sub-Saharan Africa"
  UNPDRegs3 <- c("More developed regions", "Less developed regions")
  UNPDRegs4 <- c("Least developed countries", "Excluding least developed countries")
  UNPDRegs5 <- "Excluding China"
  r <- ifelse(is.element(region, UNPDRegs1), 1, 
              ifelse(is.element(region, UNPDRegs2), 2, 
                     ifelse(is.element(region, UNPDRegs3), 3,
                            ifelse(is.element(region, UNPDRegs4), 4,
                                   ifelse(is.element(region, UNPDRegs5), 5,
                                          NA)))))
  return(r)
}
#----------------------------------------------------------------------
OICRegionAll <- c("OIC", "Africa", "Arab", "Asia/other", 
                  "OIC Low income", "OIC Middle income",
                  "OIC Lower middle income", "OIC Upper middle income",
                  "OIC High income", "OIC High income: nonOECD"
                  #, "OIC High income: OECD"
)
ChooseOICRegion <- function (region) {
  OICRegs1 <- "OIC"
  OICRegs2 <- c("Africa", "Arab", "Asia/other")
  OICRegs3 <- c("OIC Low income", "OIC Lower middle income", "OIC Upper middle income",                
                "OIC High income: nonOECD"
                #, "OIC High income: OECD"
  )
  OICRegs4 <- c("OIC Middle income", "OIC High income")
  r <- ifelse(is.element(region, OICRegs1), 1, 
              ifelse(is.element(region, OICRegs2), 2, 
                     ifelse(is.element(region, OICRegs3), 3,
                            ifelse(is.element(region, OICRegs4), 4,
                                   NA))))
  return(r)
}
#----------------------------------------------------------------------
M49RegionAll <- c("Africa",
                  "Northern Africa", "Southern Africa",
                  "Eastern Africa", "Western Africa",
                  "Middle Africa",
                  "Asia",
                  "Eastern Asia", "Central Asia",
                  "Southern Asia", "South-Eastern Asia",
                  "Western Asia",
                  "Americas",
                  "Northern America",
                  "Latin America and the Caribbean",
                  "Central America", "South America", "Caribbean",
                  "Europe",
                  "Northern Europe", "Southern Europe", 
                  "Eastern Europe", "Western Europe",
                  "Oceania",
                  "Australia and New Zealand", "Melanesia", 
                  "Micronesia", "Polynesia")
ChooseM49Region <- function(region) {
  M49Regs1 <- c("Africa",
                "Asia",
                "Americas",
                "Europe",
                "Oceania")
  M49Regs2 <- c("Northern Africa", "Southern Africa",
                "Eastern Africa", "Western Africa",
                "Middle Africa",
                "Eastern Asia", "Central Asia",
                "Southern Asia", "South-Eastern Asia",
                "Western Asia",
                "Northern America",
                "Latin America and the Caribbean",
                "Northern Europe", "Southern Europe", 
                "Eastern Europe", "Western Europe",
                "Australia and New Zealand", "Melanesia", 
                "Micronesia", "Polynesia")
  M49Regs3 <- c("Central America", "South America", "Caribbean")
  r <- ifelse(is.element(region, M49Regs1), 1, 
              ifelse(is.element(region, M49Regs2), 2, 
                     ifelse(is.element(region, M49Regs3), 3,
                            NA)))
  return(r)
}
#----------------------------------------------------------------------
WHORegionAll <- c("Africa", "Americas", "Eastern Mediterranean",
                  "Europe", "South-East Asia", "Western Pacific")
CountdownAll <- "Countdown"
ECAAfricaRegionAll <- c("North Africa", "Southern Africa", "Eastern Africa", "West Africa", "Central Africa")
AURegionAll <- c("Northern Africa", "Southern Africa", "Eastern Africa",  "Western Africa", "Central Africa")
Fragile2013All <- Fragile2014All <- Fragile2015All <- c("Fragile", "Non-fragile")
USAIDAll <- "USAID"
