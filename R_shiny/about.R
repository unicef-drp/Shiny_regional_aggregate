##
# The About panel
# Yang Liu
# 10/24/2019
##
function() {
  tabPanel("About",
           p(),
           p("
             Global and regional estimates of under-five mortality rates are
             derived by aggregating the number of country-specific under-five
             deaths estimated by the UN IGME and the country-specific births
             from the United Nations Population Division, based on a birth-week
             cohort approach.
             "),
           
           p("
              A birth-week cohort method is used to calculate the absolute number of deaths
              among neonates, infants and children under age 5. First, each annual birth
              cohort is divided into 52 equal birth-week cohorts. Then each birth-week
              cohort is exposed throughout the first five years of life to the appropriate
              calendar year- and age- specific mortality rates depending on cohort age. For
              example, the twentieth birth-week cohort of the year 2000 will be exposed to
              the infant mortality rates in both 2000 and 2001. All deaths from birth-week
              cohorts occurring as a result of exposure to the mortality rate for a given
              calendar year are allocated to that year and are summed by age group at death
              to get the total number of deaths for a given year and age group. Continuing
              with the above example, deaths from the twentieth birth-week cohort of the
              year 2000 would contribute to infant deaths in year 2000 and 2001. Any deaths
              occurring among the twentieth birth-week cohort of year 2000 after the
              twentieth week in 2001 would contribute to under-five deaths for year 2001 and
              so forth. Under-five deaths in each calendar year are calculated by summing up
              all the deaths under age 5 across all age group cohorts in that year. The
              annual estimate of the number of live births in each country from the World
              Population Prospects 2019 is used to calculate the number of deaths.
             "),
           
           
           p("For more information, please refer to",
             a("www.childmortality.org", 
               href = "https://childmortality.org", target = "_blank"), "."),
           p("This app was created and maintained by UN IGME.",
             "For any questions, feedback or suggestion, please contact us at childmortality@unicef.org."),
           p("Please use the latest version of Google Chrome, Mozilla Firefox or Internet Explorer."),
           br(), br(),
           p("Last updated: September, 2019",
             br("Created in ",
                a("R", 
                  href = "http://www.r-project.org/", target = "_blank"),
                "| Powered by ",
                a("Shiny", 
                  href = "http://www.rstudio.com/shiny/", target = "_blank")),
             align = "center")
  )
}
