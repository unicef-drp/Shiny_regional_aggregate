#----------------------------------------------------------------------
# chooseregion.R
# Jin Rou New, 2012-2014
#----------------------------------------------------------------------

# Updated: 2025.01

# >> note on how to revise this script and add new regions <<
# * The function `ChooseRegion` below returns an integer (`reg.num`) to match column names
# for example, `ChooseRegion(region = "East Asia and Pacific", regiontype = "UNICEFProgRegion")` returns 1

# * `regiontype` is part of the column names in country.info.CME (except the number in the end)
# It's used to match column names by: paste0(regiontype, reg.num)
# in which reg.num = ChooseRegion()
# So for example: `regiontype = FCSCountries` must have column names in "country.info.CME.csv" as "FCSCountries1" and "FCSCountries2"
# you *cannot* add things in between: "FCSCountries_1" or "FCSCountries_2"

# debug tips:
# * Error "subscript out of bounds" means you have extra/wrong region names in this script
# If you get this error when running `OutputAggregates()`, it means you got the
# region names wrong / or you have extra region names in this script
# (they don't match to / exist in "country.info.CME.csv")

# * Error "Error in quantile.default(ARR.year1.year6.j, probs = percentiles) : missing values and NaN's not allowed if 'na.rm' is FALSE"
# Check the all region vector (like `UNICEFProgRegionAll`) used in `outputaggregates.R` script
# It contains regions that don't exist in "country.info.CME.csv"

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
 } else if (regiontype == "AURegion") {
   r <- ChooseAURegion(region)
 } else if (regiontype == "AURECRegion") {
   r <- ChooseAURECRegion(region) # YL 2024.11
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
 } else if (regiontype == "SDGRCRegion") {
   r <- ChooseSDGRCRegion(region) # YL 2020.03
 } else if (regiontype == "EAPRORegion") {
   r <- ChooseEAPRORegion(region) # YL 2021.02
 } else if (regiontype == "FCSCountries") {
   r <- ChooseFCSCountries(region) # YL 2021.11 / 2022.07 / 2024.01
 } else if (regiontype == "FragileCountries2025OECD") {
   r <- ChooseFragile2025OECDRegion(region) # YL 2023.05
 } else if (regiontype == "SDG2015") {
   r <- ChooseSDG2015Region(region) # YL 2024.02 add temporarily for checking countries with data 
 } else if (regiontype == "UNICEFReportRegion_nohigh") {
   r <- ChooseUNICEFReportRegion_nohigh(region)
 } else {
   stop("regiontype must be one of these: UNICEFProgRegion, UNICEFRegionSSA, UNICEFReportRegion, MDGRegion,
       WBRegion, UNPDRegion, OICRegion, M49Region, Wealthall, Wealthdata, 
       SDGRegion, SDGSimpleRegion, SDGRCRegion, EAPRORegion, FCSCountries, FragileCountries2025OECD")
 }
 return(r)
}
#----------------------------------------------------------------------
UNICEFProgRegionAll <- c(
  # "Africa", "Sub-Saharan Africa", # 2022/3: removed
  # "Middle East", "North Africa",
  # "Asia",
  "East Asia and Pacific",
  "Eastern and Southern Africa",
  "Europe and Central Asia",     # 2021/7: changed from "CEE/CIS"
  "Latin America and Caribbean", # 2021/7: changed from "Latin America and the Caribbean"
  "Middle East and North Africa",
  "Non-programme countries",     # 2021/7: changed from "Industrialized countries"
  "South Asia",
  "West and Central Africa",
  # "Developing countries",      # 2022/1: remove developing / developed regions
  "Programme countries"          # 2022/3: added as UNICEFProgRegs2
)
ChooseUNICEFProgRegion <- function(region) {
  UNICEFProgRegs1 <- c("Eastern and Southern Africa",
                       "West and Central Africa",
                       "Middle East and North Africa",
                       "South Asia",
                       "East Asia and Pacific",
                       "Latin America and Caribbean", # 2021/7: changed from "Latin America and the Caribbean"
                       "Europe and Central Asia",     # 2021/7: changed from "CEE/CIS"
                       "Non-programme countries"      # 2021/7: changed from "Industrialized countries"
  )
  UNICEFProgRegs2 <- "Programme countries"
  # UNICEFProgRegs3 <- c("Africa", "Asia")
  # UNICEFProgRegs4 <- "Developing countries"
  # UNICEFProgRegs4 <- "Least developed countries"
  # UNICEFProgRegs5 <- c("Middle East", "North Africa")
  r <- ifelse(is.element(region, UNICEFProgRegs1), 1,
              ifelse(is.element(region, UNICEFProgRegs2), 2, NA))
  return(r)
}
#----------------------------------------------------------------------
UNICEFRegionAllSSA <- c("Africa",
                        "Sub-Saharan Africa",
                        "Eastern and Southern Africa",
                        "West and Central Africa",
                        "Middle East and North Africa",
                        "Middle East and North Africa (excluding Djibouti and Sudan)", # added on 2021/7
                        "Middle East", "North Africa",
                        "Asia",
                        "South Asia",
                        "East Asia and Pacific",
                        "Latin America and Caribbean", # 2021/7: changed from "Latin America and the Caribbean"
                        "Europe and Central Asia",     # 2021/7: changed from "CEE/CIS"
                        "Non-programme countries",     # 2021/7: changed from "Industrialized countries"
                        "Developing countries",
                        "Least developed countries")
ChooseUNICEFRegionSSA <- function(region) {
  UNICEFRegs1 <- c("Eastern and Southern Africa",
                   "West and Central Africa",
                   "Middle East and North Africa", # 2021/7 added since it is renamed in `UNICEFProgRegion2`
                   "South Asia",
                   "East Asia and Pacific",
                   "Latin America and Caribbean", # 2021/7: changed from "Latin America and the Caribbean"
                   "Europe and Central Asia",     # 2021/7: changed from "CEE/CIS"
                   "Non-programme countries"      # 2021/7: changed from "Industrialized countries"
  )
  UNICEFRegs2 <- c("Sub-Saharan Africa",
                   "Middle East and North Africa (excluding Djibouti and Sudan)" # revised on 2021/7
                   )
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


UNICEFReportRegionAll_nohigh <- c("South Asia",
                           "Sub-Saharan Africa",
                           "Europe and Central Asia",
                           "Middle East and North Africa",
                           "Latin America and Caribbean",
                           "East Asia and Pacific",
                           "Eastern and Southern Africa",
                           "Eastern Europe and Central Asia",
                           "West and Central Africa",
                           "Countries excl high-income",
                           "High income countries")
ChooseUNICEFReportRegion_nohigh <- function(region) {
  UNICEFReportRegs1 <- c("South Asia",
                         "Sub-Saharan Africa",
                         "Europe and Central Asia",
                         "Middle East and North Africa",
                         "Latin America and Caribbean",
                         "East Asia and Pacific")
  UNICEFReportRegs2 <- c("Eastern and Southern Africa",
                         "Eastern Europe and Central Asia",
                         "West and Central Africa")
  UNICEFReportRegs3 <- c("High income countries", "Countries excl high-income")
  r <- ifelse(is.element(region, UNICEFReportRegs1), 1,
              ifelse(is.element(region, UNICEFReportRegs2), 2,
              ifelse(is.element(region, UNICEFReportRegs3), 3,
                     NA)))
  return(r)
}

# African Union ----------------------------------------------------------------------
AURegionAll <- c("Africa",
                "Central Africa",
                "Eastern Africa",
                "Northern Africa",
                "Southern Africa",
                "Western Africa"
                 )
ChooseAURegion <- function(region) {
  AURegs1 <- c("Africa")
  AURegs2 <- c("Central Africa",
               "Eastern Africa",
               "Northern Africa",
               "Southern Africa",
               "Western Africa")
  r <- ifelse(is.element(region, AURegs1), 1,
              ifelse(is.element(region, AURegs2), 2,
                     NA))
  return(r)
}


AURECRegionAll <- c("Arab Maghreb Union (AMU)",  # 1
                    "Common Market for Eastern and Southern Africa (COMESA)",  # 2
                    "Community of Sahel-Saharan States (CEN-SAD)",  # 3
                    "East African Community (EAC)",  # 1
                    "Economic Community of Central African States (ECCAS)",  # 4
                    "Economic Community of West African States (ECOWAS)",  # 5
                    "Economic Community of West African States (ECOWAS) 202502",  # 6
                    "Intergovernmental Authority on Development (IGAD)",  # 4
                    "Southern African Development Community (SADC)" # 5 
)
ChooseAURECRegion <- function(region) {
  AURECRegs1 <- c("Arab Maghreb Union (AMU)", "East African Community (EAC)")
  AURECRegs2 <- c("Common Market for Eastern and Southern Africa (COMESA)")
  AURECRegs3 <- c("Community of Sahel-Saharan States (CEN-SAD)")
  AURECRegs4 <- c("Economic Community of Central African States (ECCAS)", "Intergovernmental Authority on Development (IGAD)")
  AURECRegs5 <- c("Economic Community of West African States (ECOWAS)", "Southern African Development Community (SADC)")
  AURECRegs6 <- c("Economic Community of West African States (ECOWAS) 202502")
  r <-  ifelse(is.element(region, AURECRegs1), 1,
               ifelse(is.element(region, AURECRegs2), 2,
                      ifelse(is.element(region, AURECRegs3), 3,
                             ifelse(is.element(region, AURECRegs4), 4,
                                    ifelse(is.element(region, AURECRegs5), 5,
                                    ifelse(is.element(region, AURECRegs6), 6,
                                           NA))))))
  return(r)
}

#----------------------------------------------------------------------
MDGRegionAll <- c(
                  # "Developed regions", "Developing regions", # removed 2022.01
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
  MDGRegs1 <- c(
    # "Developed regions",
                "Northern Africa", "Sub-Saharan Africa",
                "Latin America and the Caribbean",
                "Caucasus and Central Asia",
                "Eastern Asia",
                "Southern Asia",
                "South-eastern Asia",
                "Western Asia",
                "Oceania")
  MDGRegs1excl <- c("Eastern Asia excluding China", "Southern Asia excluding India")
  # MDGRegs2 <- "Developing regions"
  MDGRegs3 <- c("North Africa", "South Africa", "Eastern Africa", "West Africa", "Central Africa")
  MDGRegs4 <- "Least developed countries"
  MDGRegs5 <- c("Latin America", "Caribbean")
  r <- ifelse(is.element(region, MDGRegs1), 1,
              ifelse(is.element(region, MDGRegs1excl), "1excl",
                     # ifelse(is.element(region, MDGRegs2), 2,
                            ifelse(is.element(region, MDGRegs3), 3,
                                   ifelse(is.element(region, MDGRegs4), 4,
                                          ifelse(is.element(region, MDGRegs5), 5,
                                                 NA)))))
  return(r)
}
#----------------------------------------------------------------------
WBRegionAll <- c("East Asia and Pacific", "Europe and Central Asia",
                 "Latin America and the Caribbean", "Middle East, North Africa, Afghanistan and Pakistan",
                 "North America", "South Asia", "Sub-Saharan Africa",
                 "East Asia and Pacific (excluding high-income)", "Europe and Central Asia (excluding high-income)",
                 "Latin America and the Caribbean (excluding high-income)", "Middle East, North Africa, Afghanistan and Pakistan (excluding high-income)",
                 "South Asia (excluding high-income)", "Sub-Saharan Africa (excluding high-income)",
                 "Low income", "Middle income", "High income",
                 "Lower middle income", "Upper middle income",
                 "Low and middle income")
ChooseWBRegion <- function(region) {
  WBRegs1 <- c("East Asia and Pacific", "Europe and Central Asia",
               "Latin America and the Caribbean", "Middle East, North Africa, Afghanistan and Pakistan",
               "North America", "South Asia", "Sub-Saharan Africa")
  WBRegs2 <- c("East Asia and Pacific (excluding high-income)", "Europe and Central Asia (excluding high-income)", "Latin America and the Caribbean (excluding high-income)", "Middle East, North Africa, Afghanistan and Pakistan (excluding high-income)", "South Asia (excluding high-income)", "Sub-Saharan Africa (excluding high-income)")
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
UNPDRegionAll <- c(
                   # "More developed regions", "Less developed regions", # removed 2022.01
                   "Least developed countries", "Excluding least developed countries", "Excluding China",
                   "Sub-Saharan Africa", "Africa", "Asia", "Europe", "Latin America and the Caribbean",
                   "Northern America", "Oceania")
ChooseUNPDRegion <- function(region) {
  UNPDRegs1 <- c("Africa", "Asia", "Europe", "Latin America and the Caribbean",
                 "Northern America", "Oceania")
  UNPDRegs2 <- "Sub-Saharan Africa"
  # UNPDRegs3 <- c("More developed regions", "Less developed regions")
  UNPDRegs4 <- c("Least developed countries", "Excluding least developed countries")
  UNPDRegs5 <- "Excluding China"
  r <- ifelse(is.element(region, UNPDRegs1), 1,
              ifelse(is.element(region, UNPDRegs2), 2,
                     # ifelse(is.element(region, UNPDRegs3), 3,
                            ifelse(is.element(region, UNPDRegs4), 4,
                                   ifelse(is.element(region, UNPDRegs5), 5,
                                          NA))))
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
SDGRegionAll <- c(
  # "Developed regions", ("Developing regions"), # removed 2022.01
  "Least Developed Countries (LDCs)","Landlocked developing countries (LLDCs)",
  "Small island developing States (SIDS)","Europe and Northern America",
  "Northern America","Europe","Latin America and the Caribbean",
  "Central and Southern Asia","Central Asia",
  "Caucasus and Central Asia","Southern Asia",
  "Eastern and South-Eastern Asia","Eastern Asia",
  "Eastern Asia (excluding Japan)","South-Eastern Asia",
  "Northern Africa and Western Asia","Western Asia",
  "Western Asia (exc. Armenia, Azerbaijan, Cyprus, Israel and Georgia)","Northern Africa","Northern Africa (exc. Sudan)",
  "Sub-Saharan Africa","Sub-Saharan Africa (inc. Sudan)","Oceania",
  "Oceania (exc. Australia and New Zealand)",
  "Australia and New Zealand", "Caribbean","Central America",
  "Eastern Africa","Eastern Europe",
  "Melanesia","Micronesia","Middle Africa", "Northern Europe","Polynesia",
  "South America","Southern Africa","Southern Europe",
  "Western Africa","Western Europe")

ChooseSDGRegion <- function(region) {
  SDGRegs1 <- c("Central and Southern Asia","Sub-Saharan Africa",
                "Europe and Northern America","Northern Africa and Western Asia",
                "Latin America and the Caribbean","Australia and New Zealand",
                "Eastern and South-Eastern Asia","Oceania (exc. Australia and New Zealand)")
  SDGRegs2<-c("Europe","Oceania","Northern America","Northern Africa")
  SDGRegs3 <- c("Southern Asia","Middle Africa","Southern Europe",
                "Western Asia","South America","Caribbean",
                "Western Europe","Eastern Africa",
                "Western Africa","Eastern Europe","Central America",
                "South-Eastern Asia","Southern Africa",
                "Eastern Asia","Polynesia","Northern Europe",
                "Melanesia","Micronesia","Central Asia")
  SDGRegs4 <- c("Sub-Saharan Africa (inc. Sudan)" ,
                # "Developed regions" ,
                "Western Asia (exc. Armenia, Azerbaijan, Cyprus, Israel and Georgia)","Caucasus and Central Asia",
                "Eastern Asia (excluding Japan)","Northern Africa (exc. Sudan)")
  SDGRegs5 <- c("Least Developed Countries (LDCs)")
  SDGRegs6 <- c("Landlocked developing countries (LLDCs)","Small island developing States (SIDS)")
  # SDGRegs7 <- c("Developing regions")
  
  r <- ifelse(is.element(region, SDGRegs1), 1,
              ifelse(is.element(region, SDGRegs2), 2,
                     ifelse(is.element(region, SDGRegs3), 3,
                            ifelse(is.element(region, SDGRegs4), 4,
                                   ifelse(is.element(region, SDGRegs5), 5,
                                          ifelse(is.element(region, SDGRegs6), 6,
                                                 NA))))))
  return(r)
}

#----------------------------------------------------------------------
# 2021/09 revise `SDGSimpleRegion`:
# "Western Asia and Northern Africa" -> "Northern Africa and Western Asia",  # in SDGSimpleRegs1
# "Eastern Asia and South-eastern Asia" -> "Eastern and South-Eastern Asia", # in SDGSimpleRegs1
# "Central Asia and Southern Asia" -> "Central and Southern Asia",           # in SDGSimpleRegs1
# "Northern America and Europe" -> "Europe and Northern America",            # in SDGSimpleRegs1
# "Least developed countries (LDCs)" -> "Least developed countries",              # in SDGSimpleRegs3
# "Landlocked developing countries (LLDCs)" -> "Landlocked developing countries", # in SDGSimpleRegs4
# "Small island developing States (SIDS)" -> "Small island developing States",    # in SDGSimpleRegs4
# "South-eastern Asia" -> "South-Eastern Asia",                                              # in SDGSimpleRegs2
# "Oceania excluding Australia and New Zealand" -> "Oceania (exc. Australia and New Zealand)"# in SDGSimpleRegs2

# 2022/07 # in SDGSimpleRegs5
# "North America, Europe, Australia and New Zealand" -> "Europe, North America, Australia and New Zealand",
# "South Eastern Asia and Oceania (excl. Australia and New Zealand)" -> "South Eastern Asia and Oceania (exc. Australia and New Zealand)"

SDGSimpleRegionAll <- c("Least developed countries","Landlocked developing countries", "Small island developing States",
                        "Europe and Northern America", "Northern America","Europe",
                        "Latin America and the Caribbean",
                        "Central and Southern Asia","Central Asia", "Southern Asia",
                        "Eastern and South-Eastern Asia","Eastern Asia", "South-Eastern Asia",
                        "Northern Africa and Western Asia","Western Asia", "Northern Africa",
                        "Sub-Saharan Africa",
                        "Oceania","Oceania (exc. Australia and New Zealand)", "Australia and New Zealand",
                        "Europe, Northern America, Australia and New Zealand", "South-Eastern Asia and Oceania (exc. Australia and New Zealand)")

ChooseSDGSimpleRegion <- function(region) {
  SDGSimpleRegs1 <- c("Europe and Northern America", "Latin America and the Caribbean", "Central and Southern Asia",
                      "Eastern and South-Eastern Asia", "Northern Africa and Western Asia", "Sub-Saharan Africa", "Oceania")
  SDGSimpleRegs2<-c("Northern America", "Europe", "Central Asia", "Southern Asia", "Eastern Asia", "South-Eastern Asia",
                    "Western Asia", "Northern Africa", "Oceania (exc. Australia and New Zealand)", "Australia and New Zealand")
  SDGSimpleRegs3 <- c("Least developed countries")
  SDGSimpleRegs4 <- c("Landlocked developing countries","Small island developing States")
  SDGSimpleRegs5 <- c("Europe, Northern America, Australia and New Zealand",
                      "South-Eastern Asia and Oceania (exc. Australia and New Zealand)")

  r <- ifelse(is.element(region, SDGSimpleRegs1), 1,
              ifelse(is.element(region, SDGSimpleRegs2), 2,
                     ifelse(is.element(region, SDGSimpleRegs3), 3,
                            ifelse(is.element(region, SDGSimpleRegs4), 4,
                                   ifelse(is.element(region, SDGSimpleRegs5), 5,
                                   NA)))))
  return(r)
}




SDG2015RegionAll <- c("Least developed countries","Landlocked developing countries", "Small island developing States",
                        "Europe and Northern America", "Northern America","Europe",
                        "Latin America and the Caribbean",
                        "Central and Southern Asia","Central Asia", "Southern Asia",
                        "Eastern and South-Eastern Asia","Eastern Asia", "South-Eastern Asia",
                        "Northern Africa and Western Asia","Western Asia", "Northern Africa",
                        "Sub-Saharan Africa",
                        "Oceania","Oceania (exc. Australia and New Zealand)", "Australia and New Zealand",
                        "Europe, Northern America, Australia and New Zealand", "South-Eastern Asia and Oceania (exc. Australia and New Zealand)",
                      "Countries with data")

ChooseSDG2015Region <- function(region) {
  SDG2015Regs1 <- c("Europe and Northern America", "Latin America and the Caribbean", "Central and Southern Asia",
                      "Eastern and South-Eastern Asia", "Northern Africa and Western Asia", "Sub-Saharan Africa", "Oceania")
  SDG2015Regs2<-c("Northern America", "Europe", "Central Asia", "Southern Asia", "Eastern Asia", "South-Eastern Asia",
                    "Western Asia", "Northern Africa", "Oceania (exc. Australia and New Zealand)", "Australia and New Zealand")
  SDG2015Regs3 <- c("Least developed countries")
  SDG2015Regs4 <- c("Landlocked developing countries","Small island developing States")
  SDG2015Regs5 <- c("Europe, Northern America, Australia and New Zealand",
                      "South-Eastern Asia and Oceania (exc. Australia and New Zealand)")
  SDG2015Regs6 <- c("Countries with data")
  r <- ifelse(is.element(region, SDG2015Regs1), 1,
              ifelse(is.element(region, SDG2015Regs2), 2,
                     ifelse(is.element(region, SDG2015Regs3), 3,
                            ifelse(is.element(region, SDG2015Regs4), 4,
                                   ifelse(is.element(region, SDG2015Regs5), 5,
                                   ifelse(is.element(region, SDG2015Regs6), 6,
                                          NA))))))
  return(r)
}

# SDGRCRegion -------------------------------------------------------------
# YL 2025.12 updated
SDGRCRegionAll <- c(
  "ECA_Central Africa", "ECA_Eastern Africa", "ECA_North Africa", "ECA_Southern Africa", "ECA_West Africa",
  "ECA_All countries", "ECE_All countries", "ECLAC_All countries",
  "ECE_Eastern Europe, Caucasus and Central Asia (CIS)", "ECE_European Union (E25)",
  "ECE_CIS (CWG)", "ECE_Euro area (EMU)", "ECE_West Balkans (ST7)",
  "ECE_Eurasian Economic Union (EAEU)", # 2023 new
  "ECLAC_Carribean", "ECLAC_Latin America",
  # "ESCAP_ADB Group A (ADB_DMC_A)", "ESCAP_ADB Group B (ADB_DMC_B)", "ESCAP_ADB Group C (ADB_DMC_C)", # 2022 removed
  "ESCAP_ASEAN (ASEAN)", "ESCAP_Pacific island developing economies (PIDE)", "ESCAP_SAARC (SAARC)",
  # "ESCAP_ENEA including Russian Fed (ENEA_RUS)",  # 2022 removed
  "ESCAP_LDC (LDC_E)",
  "ESCAP_LLDC (LLDC_E)",
  "ESCAP_East and North-East Asia (ENEA)", "ESCAP_North and Central Asia (NCA)", "ESCAP_PACIFIC (PAC)", "ESCAP_South-East Asia (SEA)", "ESCAP_South and South-West Asia (SSWA)",
  "ESCAP_All countries", 
  # "ESCAP_ECO (ECO)", "ESCAP_Africa (AFR)", "ESCAP_Europe (Europe)", "ESCAP_Latin America (LAC)", "ESCAP_North America (NAM)", # 2025 removed
  "ESCAP_Other Area (OTH_REGION)",
  "ECA_African Union Commission", 
  # "ESCAP_ADB Developing member countries (ADB_DMC)",   # 2022 removed
  "ECA_AMU", "ECA_ECCAS", "ECA_ECOWAS", "ECA_IGAD",
  "ECA_SADC",
  "ECA_CEN-SAD", "ECA_EAC",
  "ECA_COMESA", "ECA_Sahel",
  "ECA_Island economies", "ECA_Land-locked countries",
  "ECA_Land-locked LDCs", "ECA_Small Island states",
  "ECA_Non-Oil Producing", "ECA_Oil producing economies",
  "ECA_Least developed countries",
  "ECA_Mineral-rich countries",
  "ECA_Non-Oil LDCs",
  "ECA_Oil LDCs",
  "ECA_Sub-Saharan Africa",
  "ECA_Southern Africa Customs Union (SACU)", # 2024 new
  "ESCAP_Countries with Special Needs",
  "ESCAP_Small Islands Developing States",
  "ESCAP_WB High Income Economies", "ESCAP_WB Low Income Economies", "ESCAP_WB Lower Middle Income Economies", "ESCAP_WB Upper Middle Income Economies",
  "ESCWA_Arab countries",
  "ESCWA_Gulf Cooperation Council (GCC)", "ESCWA_Arab LDCs subregion", "ESCWA_Maghreb subregion", "ESCWA_Mashreq subregion",
  "ESCWA_Countries in conflict", "ESCWA_Non-conflict middle income countries", # 2022 new
  "ESCWA_Arab Middle-Income Countries (MICs)", "ESCWA_Arab Low-Income Countries (LICs)", # 2023 new
  "ESCWA_Arab High-Income Countries (HICs)" # 2024 new
)

ChooseSDGRCRegion <- function(region) {
  name_list <- list(
    r1	=	c("ECA_Central Africa", "ECA_Eastern Africa", "ECA_North Africa", "ECA_Southern Africa", "ECA_West Africa"),
    r2	=	c("ECA_All countries", "ECA_All countries", "ECE_All countries", "ECLAC_All countries", "ECLAC_All countries"),
    r3	=	c("ECE_Eastern Europe, Caucasus and Central Asia (CIS)", "ECE_European Union (E25)"),
    r4	=	c("ECE_CIS (CWG)", "ECE_Euro area (EMU)", "ECE_West Balkans (ST7)"),
    r5	=	c("ECE_Eurasian Economic Union (EAEU)"),
    r6	=	c("ECLAC_Carribean", "ECLAC_Carribean", "ECLAC_Latin America", "ECLAC_Latin America"),
    r7	=	c("ESCAP_ASEAN (ASEAN)", "ESCAP_Pacific island developing economies (PIDE)", "ESCAP_SAARC (SAARC)"),
    r8	=	c("ESCAP_LDC (LDC_E)"),
    r9	=	c("ESCAP_LLDC (LLDC_E)"),
    r10	=	c("ESCAP_East and North-East Asia (ENEA)", "ESCAP_North and Central Asia (NCA)", "ESCAP_PACIFIC (PAC)", "ESCAP_South-East Asia (SEA)", "ESCAP_South and South-West Asia (SSWA)"),
    r11	=	c("ESCAP_All countries", "ESCAP_Other Area (OTH_REGION)"),
    r12	=	c("ECA_African Union Commission"),
    r13	=	c("ECA_ECCAS", "ECA_ECOWAS", "ECA_IGAD", "ECA_AMU"),
    r14	=	c("ECA_SADC"),
    r15	=	c("ECA_CEN-SAD", "ECA_EAC"),
    r16	=	c("ECA_COMESA", "ECA_Sahel"),
    r17	=	c("ECA_Island economies", "ECA_Land-locked countries"),
    r18	=	c("ECA_Land-locked LDCs", "ECA_Small Island states"),
    r19	=	c("ECA_Non-Oil Producing", "ECA_Oil producing economies"),
    r20	=	c("ECA_Least developed countries"),
    r21	=	c("ECA_Mineral-rich countries"),
    r22	=	c("ECA_Non-Oil LDCs"),
    r23	=	c("ECA_Oil LDCs"),
    r24	=	c("ECA_Sub-Saharan Africa"),
    r25	=	c("ESCAP_Countries with Special Needs"),
    r26	=	c("ESCAP_Small Islands Developing States"),
    r27	=	c("ESCAP_WB High Income Economies", "ESCAP_WB Upper Middle Income Economies", "ESCAP_WB Lower Middle Income Economies", "ESCAP_WB Low Income Economies"),
    r28	=	c("ESCWA_Arab countries"),
    r29	=	c("ESCWA_Gulf Cooperation Council (GCC)", "ESCWA_Mashreq subregion", "ESCWA_Maghreb subregion", "ESCWA_Arab LDCs subregion"),
    r30	=	c("ESCWA_Countries in conflict", "ESCWA_Non-conflict middle income countries"),
    r31	=	c("ESCWA_Arab Middle-Income Countries (MICs)", "ESCWA_Arab Low-Income Countries (LICs)"),
    r32	=	c("ECA_Southern Africa Customs Union (SACU)", "ESCWA_Arab High-Income Countries (HICs)")
    
  )
  to.df <- function(i) as.data.frame(cbind(No = rep(i, length(name_list[[i]])), Name = name_list[[i]]))
  dtname <- do.call(rbind, lapply(1:32, to.df))
  r <- as.numeric(dtname[dtname$Name==region, ]$No) # return the number, e.g. 32 for r32
  return(r)
}
# test:
# ChooseSDGRCRegion("ECA_Central Africa")


# EAPRORegion  --------------------------------------------------
# YL 2021/02
EAPRORegionAll <- c(
  "EAPR Program Countries"     ,
  "EAPR Program Countries (excluding China)" ,
  "Pacific Program Countries"  ,
  "Pacific Program Countries (excluding PNG)",
  "EAPR Countries"             ,
  "EAPR Countries (excluding China)"  ,
  "Pacific Countries"          ,
  "Pacific Countries (excluding PNG)" ,
  "ASEAN Countries"
)
ChooseEAPRORegion <- function(region){
  # since every region group is binary (contains only one region),
  # can return the order directly (i.e. which region group the region is in)
  which(EAPRORegionAll == region)
}
# ChooseEAPRORegion("EAPR Program Countries")


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
# WealthRegionAll <- c("Wealth", "East Asia and Pacific", "Eastern and Southern Africa", "Eastern Europe and Central Asia", "Latin America and Caribbean", "Middle East, North Africa, Afghanistan and Pakistan", "No data", "South Asia", "West and Central Africa", "Low and Middle Income", "Low Income", "Lower Middle Income", "Upper Middle Income")
# ChooseWealthRegion <- function(region) {
#   Wealth1 <- "Wealth"
#   Wealth2 <- c("East Asia and Pacific", "Eastern and Southern Africa", "Eastern Europe and Central Asia", "Latin America and Caribbean", "Middle East, North Africa, Afghanistan and Pakistan", "No data", "South Asia", "West and Central Africa")
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
                        "Middle East, North Africa, Afghanistan and Pakistan",
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
                      "Middle East, North Africa, Afghanistan and Pakistan",
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
                         "Middle East, North Africa, Afghanistan and Pakistan",
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
                       "Middle East, North Africa, Afghanistan and Pakistan",
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


# Add / update FCSCountries ----------------------------------------------------
FCS2021All <- c("Fragile and Conflict-affected Situation", "non-FCS",
                "High-intensity Conflict",
                "High-intensity Conflict (International)",
                "Medium-intensity Conflict",
                "High Institutional and Social Fragility")
ChooseFCSCountries2021 <- function(region) {
  FCSCountries2021_1 <- c("Fragile and Conflict-affected Situation", "non-FCS")
  FCSCountries2021_2 <- c("High-intensity Conflict", "High-intensity Conflict (International)",
                          "Medium-intensity Conflict",
                          "High Institutional and Social Fragility")
  r <- ifelse(is.element(region, FCSCountries2021_1), 1,
              ifelse(is.element(region, FCSCountries2021_2), 2,
                     NA))
  return(r)
}


# starting 2023: replace earlier years, don't create new functions
FCSCountriesAll <- c("Fragile and Conflict-affected Situation", "non-FCS",
                     "Conflict", "Institutional and Social Fragility")
ChooseFCSCountries <- function(region) {
  FCSCountries_1 <- c("Fragile and Conflict-affected Situation", "non-FCS")
  FCSCountries_2 <- c("Conflict", "Institutional and Social Fragility")
  r <- ifelse(is.element(region, FCSCountries_1), 1,
              ifelse(is.element(region, FCSCountries_2), 2,
                     NA))
  return(r)
}

# Add Fragile 2025OECD -----------------------------------------------------------
# YL revised 2026.01
Fragile2025OECDRegionAll <- c("Extreme to high fragility", "Medium ro low fragile", "Extreme fragility", "High fragility")

ChooseFragile2025OECDRegion <- function(region) {
  Fragile2025OECDRegs1 <- c("Extreme to high fragility", "Medium ro low fragile")
  Fragile2025OECDRegs2 <- c("Extreme fragility", "High fragility")
  
  r <- ifelse(is.element(region, Fragile2025OECDRegs1), 1,
              ifelse(is.element(region, Fragile2025OECDRegs2), 2, NA))
  return(r)
}

# Single column regions -------------------------------------------------------------
# These are for single column regions that doesn't need to select column using `ChooseRegion()`:

WHORegionAll <- c("Africa", "Americas", "Eastern Mediterranean",
                  "Europe", "South-East Asia", "Western Pacific")
CountdownAll <- "Countdown"
ECAAfricaRegionAll <- c("North Africa", "Southern Africa", "Eastern Africa", "West Africa", "Central Africa")
Fragile2013All <- Fragile2014All <- Fragile2015All <- Fragile2017All <- Fragile2018All <- Fragile2018OECD1All <-Fragile2019All<-c("Fragile", "Non-fragile")
JHUFragile2021All <- 'Fragile'
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
SPhumanitarianAll <- c("Humanitarian", "Non-humanitarian")
SPhighburdenAll <- c("High burden","Non-high burden")
JHUAll<-c("Eastern and Southern Africa","West and Central Africa","Middle East and North Africa","East Asia and the Pacific",
                 "South Asia","Latin America and Caribbean", "Eastern Europe and Central Asia","High Income Countries")
LiST_allAll<-"LiST"
MENAEMRORegionAll<-"MENAEMRO"
EECARegionAll<-"EECA"

