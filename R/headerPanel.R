#----------------------------------------------------------------------
# headerPanel.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
#headerPanel(img(src = "http://childmortality.org/themes/default/img/cmeinfo_logoblue.png"), 
#            windowTitle = "Child Mortality Estimation")

get.headerPanel <- function(){
shiny::headerPanel(
  shiny::tags$img(src = "www/logo.png", height = "60px", width = "250px"),
  windowTitle = "Child Mortality Estimation"
)
}
