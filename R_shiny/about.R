##
# The About panel
# Yang Liu
# 10/24/2019
##
function() {
  tabPanel("About",
           p("This app offers the option to generate aggregated results based on user-defined selection of countries."),
           br(), 
           p("This app was created and maintained by UN IGME.",
             "For any questions, feedback or suggestions, please contact us at childmortality@unicef.org."),
           p("Please use the latest version of Google Chrome, Mozilla Firefox or Internet Explorer."),
           br(), br(),
           p("Last updated: Oct, 2019",
             br("Created in ",
                a("R", 
                  href = "http://www.r-project.org/", target = "_blank"),
                "| Powered by ",
                a("Shiny", 
                  href = "http://www.rstudio.com/shiny/", target = "_blank")),
             align = "center")
  )
}
