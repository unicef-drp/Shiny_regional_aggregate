##
# The About panel
# Yang Liu
# 10/24/2020
##
get.about.panel <- function(update_string, WPP_Year = release_metadata()$WPP_Year){
  WPP_Year <- as.integer(WPP_Year)

  shiny::tabPanel("About",
           shiny::p(),
           shiny::p("
             Global and regional estimates of under-five mortality rates are
             derived by aggregating country-specific under-five deaths estimated
             by UN IGME with country-specific births from the United Nations
             Population Division, using a birth-week cohort approach.
             "),
           
           shiny::p(paste0(
             "A birth-week cohort method is used to calculate the absolute number of deaths ",
             "among neonates, infants, and children under age 5. First, each annual birth ",
             "cohort is divided into 52 equal birth-week cohorts. Then each birth-week ",
             "cohort is exposed throughout the first five years of life to the appropriate ",
             "calendar-year- and age-specific mortality rates, depending on cohort age. For ",
             "example, the twentieth birth-week cohort of 2000 would be exposed to ",
             "the infant mortality rates in both 2000 and 2001. All deaths from birth-week ",
             "cohorts that occur as a result of exposure to the mortality rate for a given ",
             "calendar year are allocated to that year and summed by age group at death ",
             "to obtain the total number of deaths for that year and age group. Continuing ",
             "with the above example, deaths from the twentieth birth-week cohort of the ",
             "year 2000 would contribute to infant deaths in 2000 and 2001. Any deaths ",
             "occurring among that cohort after the twentieth week of 2001 would contribute ",
             "to under-five deaths for 2001, and so forth. Under-five deaths in each calendar ",
             "year are calculated by summing all deaths under age 5 across all age-group ",
             "cohorts in that year. The annual estimate of live births in each country from World ",
             "Population Prospects ", WPP_Year, " is used to calculate the number of deaths."
           )),
           
           
           shiny::p("For more information, please see",
             shiny::a("childmortality.org", 
               href = "https://childmortality.org", target = "_blank"), "."),
           shiny::p("This app was created and is maintained by UN IGME.",
             "For questions, feedback, or suggestions, please contact us at childmortality@unicef.org."),
           shiny::p("Please use a current version of Google Chrome, Mozilla Firefox, or Microsoft Edge."),
           shiny::br(), shiny::br(),
           shiny::p(
             update_string,
             shiny::br(),
             "Created in ",
             shiny::a("R", href = "http://www.r-project.org/", target = "_blank"),
             " | Powered by ",
             shiny::a("Shiny", href = "http://www.rstudio.com/shiny/", target = "_blank"),
             align = "center")
  )
}
