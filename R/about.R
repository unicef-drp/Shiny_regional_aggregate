##
# The About panel
# Yang Liu
# 10/24/2020
##
get.about.panel <- function(update_string,
                            WPP_Year = release_metadata()$WPP_Year,
                            IGME_YEAR = release_metadata()$IGME_YEAR,
                            IGME_SB_YEAR = release_metadata()$IGME_SB_YEAR,
                            IGME_NOTE_URL = release_metadata()$IGME_NOTE_URL,
                            IGME_SB_NOTE_URL = release_metadata()$IGME_SB_NOTE_URL) {
  IGME_YEAR <- as.integer(IGME_YEAR)
  IGME_SB_YEAR <- as.integer(IGME_SB_YEAR)
  IGME_report_year <- IGME_YEAR - 1L

  shiny::tabPanel("About",
           shiny::p(),
           shiny::p(
             "World and regional results are calculated by aggregating country-level estimates. ",
             "More information on the methods for estimating country-level mortality rates and calculating deaths can be found in the ",
             shiny::a("child mortality explanatory notes",
               href = IGME_NOTE_URL, target = "_blank"), ". ",
             "More information on the methods for estimating country-level stillbirth rates and calculating stillbirths can be found in the ",
             shiny::a("stillbirth explanatory notes",
               href = IGME_SB_NOTE_URL, target = "_blank"), ". ",
             "The term \"World\" refers to the 200 countries and areas that the UN IGME produces estimates for."
           ),

           shiny::p("For more information, please refer to",
             shiny::a("childmortality.org", 
               href = "https://childmortality.org/all-cause-mortality/methods", target = "_blank"), "."),
           shiny::p("This app was created and is maintained by the UN IGME.",
             "For any questions, feedback, or suggestions, please contact us at childmortality@unicef.org."),
           shiny::p("Please use a current version of Google Chrome, Mozilla Firefox, or Microsoft Edge."),
           shiny::p(
             shiny::strong("Suggested citation for mortality estimates: "),
             paste0(
               "United Nations Inter-agency Group for Child Mortality Estimation (UN IGME), ",
               "Levels & Trends in Child Mortality: Report ",
               IGME_report_year,
               " - Estimates developed by the United Nations Inter-agency Group for Child Mortality Estimation, ",
               "United Nations Children's Fund, New York, ",
               IGME_YEAR,
               "."
             )
           ),
           shiny::p(
             shiny::strong("Suggested citation for stillbirth estimates: "),
             paste0(
               "United Nations Inter-agency Group for Child Mortality Estimation (UN IGME), ",
               "Standing Up for Stillbirth: Current estimates and key interventions, ",
               "United Nations Children's Fund, New York, ",
               IGME_SB_YEAR,
               "."
             )
           ),
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
