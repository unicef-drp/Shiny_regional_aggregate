#----------------------------------------------------------------------
# chooseregion.R
# Jin Rou New, 2012-2014
#----------------------------------------------------------------------
ChooseRegion <- function(
 region, 
 regiontype
) {
 if (regiontype == "UNICEFProgRegion") {
  r <- ChooseUNICEFProgRegion(region) 
 } else if (regiontype == "UNICEFRegionSSA") {
  r <- ChooseUNICEFRegionSSA(region)
 } else if (regiontype == "UNICEFReportRegion") {
  r <- ChooseUNICEFReportRegion(region)
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
 } else if (regiontype == "Wealthall") {
   r <- ChooseWealthall(region)
 } else if (regiontype == "Wealthdata") {
   r <- ChooseWealthdata(region)
 } else if (regiontype == "SDGRegion") {
  r <- ChooseSDGRegion(region)
 } else if (regiontype == "SDGSimpleRegion") {
   r <- ChooseSDGSimpleRegion(region)
 # } else if (regiontype == "Fragile2018OECDRegion") {
 #   r <- ChooseFragile2018OECDRegion(region)
 } else {
  stop("regiontype must be one of UNICEFProgRegion, UNICEFRegionSSA, UNICEFReportRegion, MDGRegion, WBRegion, UNPDRegion, OICRegion, M49Region, WealthallRegion, WealthdataRegion, Fragile2018OECDRegion, SDGRegion, or SDGSimpleRegion.")
 }
 return(r)
}
#----------------------------------------------------------------------
UNICEFProgRegionAll <- c("Africa",
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
ChooseUNICEFProgRegion <- function(region) {
  UNICEFProgRegs1 <- c("Eastern and Southern Africa",
                   "West and Central Africa",
                   "Middle East and North Africa",
                   "South Asia",
                   "East Asia and Pacific",
                   "Latin America and the Caribbean",
                   "CEE/CIS",
                   "Industrialized countries")
  UNICEFProgRegs2 <- "Sub-Saharan Africa"
  UNICEFProgRegs3 <- c("Africa", "Asia")
  UNICEFProgRegs4 <- "Developing countries"
  UNICEFProgRegs5 <- "Least developed countries"
  UNICEFProgRegs6 <- c("Middle East", "North Africa")
  r <- ifelse(is.element(region, UNICEFProgRegs1), 1, 
              ifelse(is.element(region, UNICEFProgRegs2), 2, 
                     ifelse(is.element(region, UNICEFProgRegs3), 3,
                            ifelse(is.element(region, UNICEFProgRegs4), 4,
                                   ifelse(is.element(region, UNICEFProgRegs5), 5, 
                                          ifelse(is.element(region, UNICEFProgRegs6), 6,
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
UNICEFReportRegionAll <- c("South Asia",
                  "Sub-Saharan Africa",
                  "Europe and Central Asia",
                  "Middle East and North Africa",
                  "Latin America and Caribbean",
                  "East Asia and Pacific",     
                  "North America",
                  "Eastern and Southern Africa",
                  "Eastern Europe and Central Asia",
                  "Western Europe",
                  "West and Central Africa")
ChooseUNICEFReportRegion <- function(region) {
 UNICEFReportRegs1 <- c("South Asia",
                     "Sub-Saharan Africa",
                     "Europe and Central Asia",
                     "Middle East and North Africa",
                     "Latin America and Caribbean",
                     "East Asia and Pacific",     
                     "North America")
 UNICEFReportRegs2 <- c("Eastern and Southern Africa",
                     "Eastern Europe and Central Asia",
                     "Western Europe",
                     "West and Central Africa") 
 r <- ifelse(is.element(region, UNICEFReportRegs1), 1, 
             ifelse(is.element(region, UNICEFReportRegs2), 2,
                    NA))
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
WBRegionAll <- c("East Asia and Pacific", "Europe and Central Asia",
                 "Latin America and the Caribbean", "Middle East and North Africa",
                 "North America", "South Asia", "Sub-Saharan Africa", 
                 "East Asia and Pacific (excluding high-income)", "Europe and Central Asia (excluding high-income)", 
                 "Latin America and the Caribbean (excluding high-income)", "Middle East and North Africa (excluding high-income)", 
                 "South Asia (excluding high-income)", "Sub-Saharan Africa (excluding high-income)",
                 "Low income", "Middle income", "High income",
                 "Lower middle income", "Upper middle income",
                 "Low and middle income")
ChooseWBRegion <- function(region) {
  WBRegs1 <- c("East Asia and Pacific", "Europe and Central Asia",
               "Latin America and the Caribbean", "Middle East and North Africa",
               "North America", "South Asia", "Sub-Saharan Africa")
  WBRegs2 <- c("East Asia and Pacific (excluding high-income)", "Europe and Central Asia (excluding high-income)", "Latin America and the Caribbean (excluding high-income)", "Middle East and North Africa (excluding high-income)", "South Asia (excluding high-income)", "Sub-Saharan Africa (excluding high-income)")
  WBRegs3 <- c("Low income", "Middle income", "High income")
  WBRegs4 <- c("Lower middle income", "Upper middle income")
  WBRegs5 <- "Low and middle income"

  r <- ifelse(is.element(region, WBRegs1), 1,
              ifelse(is.element(region, WBRegs2), 2,
                     ifelse(is.element(region, WBRegs3), 3,
                            ifelse(is.element(region, WBRegs4), 4,
                                   ifelse(is.element(region, WBRegs5), 5,
                                   NA)))))
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
SDGRegionAll <- c("Developed regions (MDG)", ("Developing regions (MDG)"),
                  "Least developed countries (LDCs)","Landlocked developing countries (LLDCs)",
                  "Small island developing States (SIDS)","Northern America (M49) and Europe (M49)",
                  "Northern America (M49)","Europe (M49)","Latin America and the Caribbean (MDG=M49)",
                  "Central Asia (M49) and Southern Asia (MDG=M49)","Central Asia (M49)", 
                  "Caucasus and Central Asia (MDG)","Southern Asia (MDG=M49)",
                  "Eastern Asia (M49) and South-eastern Asia (MDG=M49)","Eastern Asia (M49)",                                 
                  "Eastern Asia (MDG)","South-eastern Asia (MDG=M49)",                     
                  "Western Asia (M49) and Northern Africa (M49)","Western Asia (M49)",
                  "Western Asia (MDG)","Northern Africa (M49)","Northern Africa (MDG)",
                  "Sub-Saharan Africa (M49)","Sub-Saharan Africa (MDG)","Oceania (M49)",
                  "Oceania (M49) excluding Australia and New Zealand (M49)",
                  "Australia and New Zealand (M49)", "Caribbean (M49)","Central America (M49)",
                  "Eastern Africa (M49)","Eastern Europe (M49)",                                
                  "Melanesia (M49)","Micronesia (M49)","Middle Africa (M49)", "Northern Europe (M49)","Polynesia (M49)",
                  "South America (M49)","Southern Africa (M49)","Southern Europe (M49)",
                  "Western Africa (M49)","Western Europe (M49)")

ChooseSDGRegion <- function(region) {
 SDGRegs1 <- c("Central Asia (M49) and Southern Asia (MDG=M49)","Sub-Saharan Africa (M49)",
               "Northern America (M49) and Europe (M49)","Western Asia (M49) and Northern Africa (M49)",
               "Latin America and the Caribbean (MDG=M49)","Australia and New Zealand (M49)",                       
               "Eastern Asia (M49) and South-eastern Asia (MDG=M49)","Oceania (M49) excluding Australia and New Zealand (M49)")
 SDGRegs2<-c("Europe (M49)","Oceania (M49)","Northern America (M49)","Northern Africa (M49)")
 SDGRegs3 <- c("Southern Asia (MDG=M49)","Middle Africa (M49)","Southern Europe (M49)",
               "Western Asia (M49)","South America (M49)","Caribbean (M49)",                
               "Western Europe (M49)","Eastern Africa (M49)",          
               "Western Africa (M49)","Eastern Europe (M49)","Central America (M49)",          
               "South-eastern Asia (MDG=M49)","Southern Africa (M49)",      
               "Eastern Asia (M49)","Polynesia (M49)","Northern Europe (M49)",
               "Melanesia (M49)","Micronesia (M49)","Central Asia (M49)")
 SDGRegs4 <- c("Sub-Saharan Africa (MDG)" ,"Developed regions (MDG)" ,
               "Western Asia (MDG)","Caucasus and Central Asia (MDG)",
               "Eastern Asia (MDG)","Northern Africa (MDG)")
 SDGRegs5 <- c("Least developed countries (LDCs)")
 SDGRegs6 <- c("Landlocked developing countries (LLDCs)","Small island developing States (SIDS)")
 SDGRegs7 <- c("Developing regions (MDG)")
 
 r <- ifelse(is.element(region, SDGRegs1), 1, 
             ifelse(is.element(region, SDGRegs2), 2, 
                    ifelse(is.element(region, SDGRegs3), 3,
                           ifelse(is.element(region, SDGRegs4), 4,
                                  ifelse(is.element(region, SDGRegs5), 5,
                                         ifelse(is.element(region, SDGRegs6), 6,
                                                ifelse(is.element(region, SDGRegs7), 7,
                           NA)))))))
 return(r)
}
#----------------------------------------------------------------------
SDGSimpleRegionAll <- c("Least developed countries (LDCs)","Landlocked developing countries (LLDCs)", "Small island developing States (SIDS)",
                        "Northern America and Europe", "Northern America","Europe",
                        "Latin America and the Caribbean",
                        "Central Asia and Southern Asia","Central Asia", "Southern Asia",
                        "Eastern Asia and South-eastern Asia","Eastern Asia", "South-eastern Asia",                     
                        "Western Asia and Northern Africa","Western Asia", "Northern Africa",
                        "Sub-Saharan Africa",
                        "Oceania","Oceania excluding Australia and New Zealand", "Australia and New Zealand",
                        "North America, Europe, Australia and New Zealand","South Eastern Asia and Oceania (excl. Australia and New Zealand)")
                     
ChooseSDGSimpleRegion <- function(region) {
  SDGSimpleRegs1 <- c("Northern America and Europe", "Latin America and the Caribbean", "Central Asia and Southern Asia",
                      "Eastern Asia and South-eastern Asia", "Western Asia and Northern Africa", "Sub-Saharan Africa", "Oceania")
  SDGSimpleRegs2<-c("Northern America", "Europe", "Central Asia", "Southern Asia", "Eastern Asia", "South-eastern Asia", 
                    "Western Asia", "Northern Africa", "Oceania excluding Australia and New Zealand", "Australia and New Zealand")
  SDGSimpleRegs3 <- c("Least developed countries (LDCs)")
  SDGSimpleRegs4 <- c("Landlocked developing countries (LLDCs)","Small island developing States (SIDS)")
  SDGSimpleRegs5 <- c( "North America, Europe, Australia and New Zealand","South Eastern Asia and Oceania (excl. Australia and New Zealand)")
  
  r <- ifelse(is.element(region, SDGSimpleRegs1), 1, 
              ifelse(is.element(region, SDGSimpleRegs2), 2, 
                     ifelse(is.element(region, SDGSimpleRegs3), 3,
                            ifelse(is.element(region, SDGSimpleRegs4), 4, 
                                   ifelse(is.element(region, SDGSimpleRegs5), 5, 
                                   NA)))))
  return(r)
}
#----------------------------------------------------------------------
# Fragile2018OECDRegionAll <- c("Fragile", "Non-fragile", "Extremely Fragile", "Other Fragile")
# 
# ChooseFragile2018OECDRegion <- function(region) {
#   Fragile2018OECDRegs1 <- c("Fragile", "Non-fragile")
#   Fragile2018OECDRegs2<-c("Extremely Fragile", "Other Fragile")
# 
#   r <- ifelse(is.element(region, Fragile2018OECDRegs1), 1,
#               ifelse(is.element(region, Fragile2018OECDRegs2), 2,
#                                           NA))
#   return(r)
# }
#----------------------------------------------------------------------
# WealthRegionAll <- c("Wealth", "East Asia and Pacific", "Eastern and Southern Africa", "Eastern Europe and Central Asia", "Latin America and Caribbean", "Middle East and North Africa", "No data", "South Asia", "West and Central Africa", "Low and Middle Income", "Low Income", "Lower Middle Income", "Upper Middle Income")
# ChooseWealthRegion <- function(region) {
#   Wealth1 <- "Wealth"
#   Wealth2 <- c("East Asia and Pacific", "Eastern and Southern Africa", "Eastern Europe and Central Asia", "Latin America and Caribbean", "Middle East and North Africa", "No data", "South Asia", "West and Central Africa")
#   Wealth3 <- c("Low and Middle Income", "No data")
#   Wealth4 <- c("Low Income", "Lower Middle Income", "No data", "Upper Middle Income")
#   r <- ifelse(is.element(region, Wealth1), 1, 
#               ifelse(is.element(region, Wealth2), 2, 
#                      ifelse(is.element(region, Wealth3), 3,
#                             ifelse(is.element(region, Wealth4), 4,
#                             NA))))
#   return(r)
# }
#----------------------------------------------------------------------
WealthdataRegionAll <- c("South Asia",
                        "Sub-Saharan Africa",
                        "Europe and Central Asia",
                        "Middle East and North Africa",
                        "Latin America and Caribbean",
                        "East Asia and Pacific",     
                        "Eastern and Southern Africa",
                        "Eastern Europe and Central Asia",
                        "West and Central Africa",
                        "Low income", "Lower middle income", "Upper middle income")
ChooseWealthdata <- function(region) {
  WealthdataRegs1 <- c("East Asia and Pacific", 
                      "Europe and Central Asia",
                      "Latin America and Caribbean",
                      "Middle East and North Africa",
                      "South Asia", "Sub-Saharan Africa")
  WealthdataRegs2 <- c("Eastern and Southern Africa",
                      "Eastern Europe and Central Asia",
                      "West and Central Africa") 
  WealthdataRegs3 <- c("Low income", "Lower middle income", "Upper middle income")
  r <- ifelse(is.element(region, WealthdataRegs1), 1, 
              ifelse(is.element(region, WealthdataRegs2), 2,
                     ifelse(is.element(region, WealthdataRegs3), 3,
                     NA)))
  return(r)
}
#----------------------------------------------------------------------
WealthallRegionAll <- c("South Asia",
                         "Sub-Saharan Africa",
                         "Europe and Central Asia",
                         "Middle East and North Africa",
                         "Latin America and Caribbean",
                         "East Asia and Pacific",     
                         "Eastern and Southern Africa",
                         "Eastern Europe and Central Asia",
                         "West and Central Africa",
                        "Low income", "Lower middle income", "Upper middle income")
ChooseWealthall <- function(region) {
  WealthallRegs1 <- c("East Asia and Pacific", 
                       "Europe and Central Asia",
                       "Latin America and Caribbean",
                       "Middle East and North Africa",
                       "South Asia", "Sub-Saharan Africa")
  WealthallRegs2 <- c("Eastern and Southern Africa",
                       "Eastern Europe and Central Asia",
                       "West and Central Africa") 
  WealthallRegs3 <- c("Low income", "Lower middle income", "Upper middle income")
  r <- ifelse(is.element(region, WealthallRegs1), 1, 
              ifelse(is.element(region, WealthallRegs2), 2,
                     ifelse(is.element(region, WealthallRegs3), 3,
                            NA)))
  return(r)
}
#----------------------------------------------------------------------
# The regiontypes:  # name of categories
WHORegionAll <- c("Africa", "Americas", "Eastern Mediterranean",
                  "Europe", "South-East Asia", "Western Pacific")
CountdownAll <- "Countdown"
ECAAfricaRegionAll <- c("North Africa", "Southern Africa", "Eastern Africa", "West Africa", "Central Africa")
AURegionAll <- c("Northern Africa", "Southern Africa", "Eastern Africa",  "Western Africa", "Central Africa")
Fragile2013All <- Fragile2014All <- Fragile2015All <- Fragile2017All <- Fragile2018All <- Fragile2018OECD1All <- c("Fragile", "Non-fragile")
Fragile2018OECD2All<-c("Extremely Fragile", "Other Fragile")
HACAll<-"HAC"
GlobalStrategyAll<-"Global Strategy"
USAIDAll <- "USAID"
ConflictAll<-c("Conflict","Non conflict")
STRATEGICAll<-"GS"
WealthdataGlobalAll <- WealthallGlobalAll <- "Low and middle income"
AfricanEconomicCommunityAll  <- c("Economic Community of Central African States", "Economic Community of West African States")
ECAAll <- "Eastern Caribbean Area"
GAVIAll <- "GAVI"
adhoc <- "Adhoc"
# NewWorldBankAll <- c("East Asia and Pacific", "Europe and Central Asia", "Latin America and the Caribbean", "Middle East and North Africa", "North America", "South Asia", "Sub-Saharan Africa")
# WorldBankReg2All <- c("East Asia and Pacific", "Europe and Central Asia",
#                          "Latin America and the Caribbean", "Middle East and North Africa",
#                          "South Asia", "Sub-Saharan Africa")
