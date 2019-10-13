#--------------------------------------------------
# standardisecountrynames.R
# Leontine Alkema and Jin Rou New, 2012-2013
#--------------------------------------------------
StandardiseCountryNames <- function( # Standardise vector of country names to CME Info country names
  name.c ##<< Vector of country names to standardise.
) {
  # name.c = ifelse(name.c==, paste(), paste(name.c))
  name.c = ifelse(name.c=="Antigua and Barbuda"|name.c=="Antigua & B.", paste("Antigua & Barbuda"), paste(name.c))
  name.c = ifelse(name.c=="Bolivia (Plurinational State of)", paste("Bolivia"), paste(name.c))
  name.c = ifelse(name.c=="Bosnia and Herzegovina"|name.c=="Bosn&Herze", paste("Bosnia & Herzegovina"), paste(name.c))
  name.c = ifelse(name.c=="Brunei Darussalam", paste("Brunei"), paste(name.c))
  name.c = ifelse(name.c=="CAR"| name.c=="Central African Rep.", paste("Central African Republic"), paste(name.c))
  name.c = ifelse(name.c=="Congo, Dem. Rep."|name.c=="Democratic Republic of the Congo"|name.c=="Democratic Republic of Congo"|
    name.c=="Dem. Rep. of the Congo"|name.c=="DRC"|name.c=="DRCongo"|name.c=="Congo Democratic Republic", paste("Congo DR"), paste(name.c))
  name.c = ifelse(name.c=="PRC"| name.c=="People's Republic of the Congo" | 
    name.c=="Congo(Brazzaville)"|name.c=="Congo (Brazzaville)", 
                  paste("Congo"), paste(name.c)) 
  name.c = ifelse(name.c=="Costa rica", paste("Costa Rica"), paste(name.c))
  name.c = ifelse(name.c=="Cote d'Ivoire" | name.c=="Cote Divoire" | name.c=="Cote DIvoire"| name.c=="Cote dIvoire" |
                    name.c == "CotedIvoire" | name.c == "Cote divoire" | name.c=="Cote D'ivoire" | name.c=="Cote D'Ivoire" |
                    name.c == "C\x99te d'Ivoire", # | name.c=="C?te d'Ivoire",
                  paste("Cote d Ivoire"), paste(name.c))
  name.c = ifelse(name.c=="Dominican Rep." | name.c=="Dominican Rep", paste("Dominican Republic"), paste(name.c))
  name.c = ifelse(name.c=="Gambia", paste("Gambia The"), paste(name.c))
  name.c = ifelse(name.c=="GuineaBissau" | name.c=="Guinea Bissau", paste("Guinea-Bissau"), paste(name.c))
  name.c = ifelse(name.c=="China, Hong Kong SAR" | name.c=="China Hong Kong SAR" |
    name.c=="China, Hong Kong Special Administrative Region", paste("Hong Kong"), paste(name.c))
  name.c = ifelse(name.c=="Iran Islamic Republic of" | name.c=="Iran (Islamic Republic of)", 
                  paste("Iran"), paste(name.c))
  name.c = ifelse(name.c=="Republic of Korea"| name.c == "Republic of Korea "|
    name.c == "South Korea"|name.c=="Korea, Rep. of"| name.c == "Korea", paste("Korea Rep"), paste(name.c))
  name.c = ifelse(name.c=="Democratic People's Republic of Korea"|name.c=="North Korea"|
    name.c=="Dem. People's Republic of Korea"| name.c=="Korea, Dem. People's Rep."|
    name.c=="Dem. People's Rep. of Korea" | name.c=="Democratic Peoples Republic of Korea"|
    name.c=="Democratic Peoples's Republic of Korea", 
                  paste("Korea DPR"), paste(name.c))
  name.c = ifelse(name.c=="Lao People's Democratic Republic" | name.c=="Lao People's Dem. Rep."|
    name.c=="Lao Peoples Democratic Republic" | name.c =="Laos", 
                  paste("Lao PDR"), paste(name.c))
  name.c = ifelse(name.c=="Libyan Arab Jamahiriya", paste("Libya"), paste(name.c))
  name.c = ifelse(name.c=="KyrgyzRepublic"|name.c=="Kyrgyz Republic", paste("Kyrgyzstan"), paste(name.c))
  name.c = ifelse(name.c=="The former Yugoslav Republic of Macedonia"|name.c=="TFYR Macedonia", 
                  paste("Macedonia"), paste(name.c))
  name.c = ifelse(name.c=="Micronesia" | 
    name.c=="Micronesia (Federated States of )"| name.c=="Micronesia, Federated States of"| 
    name.c=="Micronesia, Fed. States of"| name.c=="Micronesia (Fed. States of)"| name.c=="Micronesia Fed States of", 
                  paste("Federated States of Micronesia"), paste(name.c))
  name.c = ifelse(name.c=="Republic of Moldova"|name.c=="Moldova, Rep. of", 
                  paste("Moldova"), paste(name.c))
  name.c = ifelse(name.c=="Northern Mariana Islands", paste("N. Mariana Isl."), paste(name.c))
  name.c = ifelse(name.c=="Occupied Palestinian Territory"|name.c=="Occ. Palestinian Terr."|
    name.c =="Occupied Palestinian Terr." | name.c == "OPT", paste("State of Palestine"), paste(name.c))
  name.c = ifelse(name.c=="Russia", paste("Russian Federation"), paste(name.c))
  name.c = ifelse(name.c=="Saint Vincent and the Grenadines" | name.c == "Saint Vincent and Grenadines" |
    name.c =="Saint Vincent & the Grenadines" | name.c=="Saint Vincent/Grenadines"|
    name.c =="St Vincent & the Grenadines" | name.c=="St. Vincent & Gren." | name.c == "St Vincent and the Grenadines", 
                  paste("St Vincent & the Grenadines"), paste(name.c))  
  name.c = ifelse(name.c=="Saint Kitts and Nevis", paste("Saint Kitts & Nevis"), paste(name.c))
  name.c = ifelse(name.c=="Sao Tome and Principe" | name.c=="Sao Tome Pr"|name.c=="SaoTome",
                  paste("Sao Tome & Principe"), paste(name.c))
  name.c = ifelse(name.c=="Syrian Arab Republic", paste("Syria"), paste(name.c))
  name.c = ifelse(name.c=="United Republic of Tanzania"|name.c=="United Repulic of Tanzania"| name.c=="Tanzania, United Republic of", 
                  paste("Tanzania"), paste(name.c))  
  name.c = ifelse(name.c=="Timor-Leste", paste("Timor Leste"), paste(name.c))
  name.c = ifelse(name.c=="Trinidad and Tobago"|name.c=="Trinidad&T", paste("Trinidad & Tobago"), paste(name.c))
  name.c = ifelse(name.c=="Arab Emirates", paste("United Arab Emirates"), paste(name.c))
  name.c = ifelse(name.c=="U.K.", paste("United Kingdom"), paste(name.c))
  name.c = ifelse(name.c=="U.S." | name.c=="United States", paste("United States of America"), paste(name.c))
  name.c = ifelse(name.c=="United States Virgin Islands", paste("US Virgin Isl."), paste(name.c))
  name.c = ifelse(name.c=="Ukraine ", paste("Ukraine"), paste(name.c))
  name.c = ifelse(name.c=="Venezuela (Bolivarian Republic of)" | name.c=="Venezuela Bolivarian Republic of", paste("Venezuela"), paste(name.c)) 
  name.c = ifelse(name.c=="Viet Nam", paste("Vietnam"), paste(name.c))
  ##value<< Vector of standardised country names
  return(name.c)
}
