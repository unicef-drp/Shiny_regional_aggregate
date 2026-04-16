#----------------------------------------------------------------------
# headerPanel.R
# Jin Rou New, 2013
#----------------------------------------------------------------------
#headerPanel(img(src = "http://childmortality.org/themes/default/img/cmeinfo_logoblue.png"), 
#            windowTitle = "Child Mortality Estimation")

get.headerPanel <- function(){
headerPanel(img(src = "logo.png", height = "60px", width = "250px"), 
            windowTitle = "Child Mortality Estimation")
}