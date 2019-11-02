#
# Shiny app to aggregate selected countries in each region
# Yang Liu, 
# Oct.11, 2019
#
pkg_vt <- rownames(installed.packages())
for (pck in c("shiny", "shinyWidgets", "leaflet",
              "maps", "maptools", "rgeos",
              "DT", "ggplot2", "data.table")){
  if(!pck %in% pkg_vt){install.packages(pck)}
  library(pck, character.only = T)
}


suppressPackageStartupMessages({
  library("shiny")    # for shiny apps
  library("shinyWidgets")
  library("leaflet")  # for openstreetmap
  library("maps")     # provide shap files for selected countries
  library("maptools") # modify shap files
  library("rgeos")
  library("DT")       # for shiny table 
  library("ggplot2")
  library("data.table") 
})

note1 <- "Note 1. This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."


# Dataset -----------------------------------------------------------------

# dc: country.info.CME dataset; regions: MDG regions in country.info.CME
dc <- fread(here::here("input", "country.info.CME.csv"))
regions <- sort(dc[, unique(MDGRegion1)])
countries <- sort(dc[,unique(OfficialName)])
# source the world map with modified country names 
source(here::here("R_shiny/helper.R"))
world_map <- get.world.map()
# source the About panel, which is a function to be called in ui
tabPanel.about <- source(here::here("R_shiny/about.R"))$value
# UI ----------------------------------------------------------

ui = fluidPage(
  # head panel 
  source(here::here("R_shiny/headerPanel.R"), local = TRUE)$value,
  
  # side panel
  sidebarPanel(
    titlePanel("Aggregate Selected Countries"),
    selectInput(inputId = "supplied_region", label = "World Regions",
                       choices = regions,
                       selected = "Northern Africa"),
    # select all or not
    shinyWidgets::materialSwitch(inputId = "select_all", 
                  label = "Select all countries in this region by default", value = TRUE),

    # populate the country selection in the meun, 
    # rendered by renderUI in server
      p("Available countries:"),
      uiOutput("country_in_region"),
    
      p("If wish to add more countries:"),
      uiOutput("country_out_region")

  ),
  
  # main panel 
  mainPanel(
    # a 2-panel set up
    tabsetPanel(
      tabPanel("Run",
        h3("Map of Selected Countries"),
        leafletOutput(outputId = "mymap"),
        p(note1),
        h4("Country List:"),
        textOutput(outputId = "selected_countries"),
        br(), 
        h3("Actions"),
        p("Please confirm your selection first to double check the countries. Thank you."),
        actionButton("click_write",  "1. Confirm your selection", width = "40%"),
        actionButton("click_run",    "2. Run aggregates", width = "40%"),
        br(),
        # to obtain gender results?
        # after click_run, optional to run by gender
    
        # actionButton("click_run_gender", "3. Run aggregates by gender", width = "80%"),
        # conditionalPanel("input.click_run",
        #                 actionButton("click_run_gender", "3. Run aggregates by gender", width = "80%")),
        br(),
        # plots:
        h3("Plots"),
        textOutput(outputId = "selected_countries_on_file"),
        # plotOutput("plot_rate_gender", width = "800px", height = "800px"),
        plotOutput("plot_rate",  width = "800px",  height = "400px"),
        plotOutput("plot_death", width = "800px", height = "400px"),
        br()
      ),
      tabPanel("Tables",
               # data:
               h3("The most recent five years"),
               DT::dataTableOutput("results_table_recent", width = "80%"),
               h3("All the aggregated results"),
               # optional to download
               downloadButton("download_table", "Download"),
               br(),
               DT::dataTableOutput("results_table", width = "80%"),
               br()
               ),
      
      tabPanel("byGender",
               # figures and tables
               plotOutput("plot_rate_gender", width = "800px", height = "800px"),
               br(),br(),
               p(strong("Data for the female")),
               downloadButton("download_table_f", "Download"),
               br(),
               DT::dataTableOutput("results_table_f", width = "80%"),
               
               br(),br(),
               p(strong("Data for the male")),
               downloadButton("download_table_m", "Download"),
               br(),
               DT::dataTableOutput("results_table_m", width = "80%"),
               br()        
      ),      
      # tabPanel ("About")
      tabPanel.about()
    ) #tabsetPanel
  ), #mainpanel
theme = "bootstrap.css"
) #ui fluidpage

# Server ------------------------------------------------------------------

server = function(input, output, session) {
  # Render UI
  # output$UI_tabPanel_byGender <- renderUI({
  #   if (is.null(reactive.get.results.gender())){
  #     return(NULL)
  #   }
  # 
  #            
  #   )
  # })
  
  # render check boxes of countries to send back to ui
  output$country_in_region <- renderUI({
    checkboxGroupInput(inputId = "country_input", label = "Countries",
                       choices = reactive.get.sub.c(), selected = if(input$select_all) reactive.get.sub.c() else NULL)
  })  
  # render the droplist, default to select `country_input`
  # `country_input_more` is the final selection
  output$country_out_region <- renderUI({
    # select picker
    shinyWidgets::pickerInput(
      inputId = "country_input_more", label = "", 
      choices = countries, 
      selected = input$country_input,  # default select the countries in region
      multiple = TRUE,  # allow multiple selection
      options = list(
        title = "Please select countries",
        `actions-box` = TRUE, 
        size = 12
      )
    )
  })
  
  
  
  
  # filter and return a vector of country names by selected region
  reactive.get.sub.c = reactive({
    # the countries selected, select by renderUI:country_input:
    dc[dc$MDGRegion1%in%input$supplied_region, OfficialName]
  })

  # map the selected countries (if available in this map)
  output$mymap = renderLeaflet({
    validate(need(input$country_input_more, "Please select countries."))
    # plot on pre-loaded world_map from get.world.map()
    leaflet::leaflet() %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
      leaflet::addPolygons(data = subset(world_map, country %in% input$country_input_more), weight = 1)
    })
  
  # print the selected countries in app 
  output$selected_countries  = renderText({
    paste(sort(input$country_input_more), collapse = ", ")
  })
  
  # write selected countries into the master 'country.info.CME_adhoc.csv' file
  observeEvent(input$click_write, {
    # clear the current column (or create new), then write the wanted countries
    showModal(modalDialog("Selection confirmed:", paste(input$country_input_more, collapse = ", "),
                          footer = modalButton("OK"), size = "s"))
    dc[,AdhocCountries:=""]
    dc[OfficialName%in%input$country_input_more, AdhocCountries:="Adhoc"]
    write.csv(dc, file = here::here("input", "country.info.CME_adhoc.csv"))
    # removeModal()
  })
  
  
  # reactive.get.results ----------------------------------------------------
  # check the names in `country.info.CME_adhoc.csv` file and 
  # run David's script `6outputaggregates.R`
  reactive.get.results <- eventReactive(input$click_run, {
    cnames <- data.table::fread("input/country.info.CME_adhoc.csv")[AdhocCountries=="Adhoc", sort(OfficialName)]
    # only run if countries to be run >0 and equal selected countries
    if (length(cnames) >0 & identical(sort(cnames), sort(input$country_input_more))) {
        # showModal will show that the scripe is running, and removed when scripts are done 
        showModal(modalDialog("Running aggregate for", paste(cnames, collapse = ", "), footer=NULL))
          # where we run the aggregates:
          source(here::here("6outputaggregates.R"))
          message("Read the results file: 'Rates & Deaths_AdhocCountries.csv'.")
        removeModal()
        file.dir.median <- "Aggregate results (median) 2019-08-15"
        return(fread(here::here(file.dir.median, "Rates & Deaths_AdhocCountries.csv")))
      } else {
        showModal(modalDialog(
          "Please press <1. Confirm your selection> first.\n  The countries on the record file now are ", 
          paste(cnames, collapse = ", "), ", which doesn't match your selected countries."
        ))
        return(NULL)
      }
    
  })
  
  reactive.get.results.gender <- eventReactive(input$click_run, {
    cnames <- data.table::fread("input/country.info.CME_adhoc.csv")[AdhocCountries=="Adhoc", sort(OfficialName)]
    # only run if countries to be run >0 and equal selected countries
    if (length(cnames) >0 & identical(sort(cnames), sort(input$country_input_more))) {
      showModal(modalDialog("Running aggregate by gender for", paste(cnames, collapse = ", "), footer=NULL))
      source(here::here("6outputaggregates_gender.R"))
      removeModal()
      return(
        list(m = fread(here::here("Aggregate results (median) 2019-08-15 (male)", "Rates & Deaths_AdhocCountries.csv")),
             f = fread(here::here("Aggregate results (median) 2019-08-15 (female)", "Rates & Deaths_AdhocCountries.csv")))
        )
    } else {
      showModal(modalDialog(
        "Please press <1. Confirm your selection> first.\n  The countries on the record file now are ", 
        paste(cnames, collapse = ", "), ", which doesn't match your selected countries."
      ))
      return(NULL)
    }
    
  })
  

  # Figure 1 Rate by Year ---------------------------------------------------
  get.long <- function(dt, vars0){
    dt_long <- data.table::melt(dt, measure.vars = vars0, 
                                value.name = "rate", variable.name = "type_of_rate")
    return(dt_long[,.(Region, Year, rate, type_of_rate)])
  }
  plot.rate <- function(dt, vars0){
    ggplot(get.long(dt, vars0)[!is.na(rate),], aes(x = Year, y = rate, color = Region, type = Region)) +
      geom_line(size = 1) + 
      theme_bw() + 
      ggtitle("Median U5MR, IMR, and NMR by Year") + 
      labs(y = "Deaths per 1,000 live births") + 
      facet_wrap(facets = ~ type_of_rate) + 
      theme(legend.position = "bottom") + 
      scale_color_discrete(labels = c("Selected Countries", "World")) +
      theme(plot.title = element_text(size = 20))
  }
  
  output$plot_rate <- renderPlot({
    if (is.null(reactive.get.results())){
      return()
    }
    vars0 <- c("U5MR median", "IMR median", "NMR median")
    dt <- reactive.get.results()
    plot.rate(dt, vars0)
  })
  
  output$plot_rate_gender <- renderPlot({
    if (is.null(reactive.get.results.gender())){
      return()
    }
    vars0 <- c("U5MR median", "IMR median")
    dt_long_m <- get.long(reactive.get.results.gender()$m, vars0)
    dt_long_f <- get.long(reactive.get.results.gender()$f, vars0)
    dt_long_m$gender <- "Male"
    dt_long_f$gender <- "Female"
    dt_long <- rbind(dt_long_f, dt_long_m)
    dt_long$Region <- as.factor(dt_long$Region)
    levels(dt_long$Region) <- c("Selected Region", "World")
    ggplot(dt_long[!is.na(rate),], aes(x = Year, y = rate, color = gender)) +
      geom_line(size = 1) + 
      theme_bw() + 
      ggtitle("Median U5MR and IMR by Gender and Year", subtitle = "Selected Region vs. World") + 
      labs(y = "Deaths per 1,000 live births", color = "Gender") + 
      facet_wrap(facets = ~ type_of_rate + Region) + 
      theme(legend.position = "bottom") + 
      theme(plot.title = element_text(size = 20))
  })
  

  # Figure 2. death by year -------------------------------------------------
  plot.death <- function(dt){
    dt_long <- data.table::melt(dt, measure.vars = grep("deaths", colnames(dt), value = TRUE), 
                                  value.name = "deaths", variable.name = "type")
    ggplot(dt_long[!is.na(deaths),], aes(x = Year, y = deaths, color = Region)) +
        geom_line(size = 1) + 
        theme_bw() + 
        ggtitle("Median Deaths of U5MR, IMR, and NMR by Year") + 
        labs(y = "Deaths") + 
        facet_wrap(facets= ~ type) + 
        theme(legend.position="bottom") + 
        scale_color_discrete(labels = c("Selected Countries", "World")) + 
        theme(plot.title = element_text(size = 20))
  }
  
  output$plot_death <- renderPlot({
    dt <- reactive.get.results()
    if (is.null(dt)){
      return()
    }
    plot.death(dt)
  })
  
  

  # print results as data table  --------------------------------------------
  # a function to select columns and define some format 
  get.table <- function(dt){
    # remove some columns 
    dt <- dt[, -(grep("Population|population", colnames(dt), value = TRUE)), with = FALSE]
    dt <- dt[Year>=1990][order( Region, -Year, )] # reorder
    return(dt)
    # DT::formatRound(datatable(dt), columns = grep("median", colnames(dt), value = TRUE), digits = 2)
  }
    
  output$results_table_recent <-  DT::renderDT({
    if (is.null(reactive.get.results())){
      return()
    }
    dt <- reactive.get.results()
    
    # only print the most recent 5 years of Adhoc group, order by year
    DT::formatRound(
      DT::datatable(get.table(dt)[Region=="Adhoc"][1:5,-1]),
      # format number columns with digits
      columns = c("U5MR median", "IMR median", "NMR median"), digits = 2)
  })
  

  # results tables ----------------------------------------------------------
  output$results_table <- DT::renderDT({
    if (is.null(reactive.get.results())){
      return()
    }
    dt <- reactive.get.results()
    
    DT::formatRound(
      DT::datatable( get.table(dt) ), 
      columns = c("U5MR median", "IMR median", "NMR median"), digits = 2)
  })
  
  output$results_table_m <- DT::renderDT({
    if (is.null(reactive.get.results.gender())){
      return()
    }
    dt <- reactive.get.results.gender()$m
    DT::formatRound(
      DT::datatable( get.table(dt) ), 
      columns = c("U5MR median", "IMR median"), digits = 2)
  })
  
  output$results_table_f <- DT::renderDT({
    if (is.null(reactive.get.results.gender())){
      return()
    }
    dt <- reactive.get.results.gender()$f
    DT::formatRound(
      DT::datatable( get.table(dt) ), 
      columns = c("U5MR median", "IMR median"), digits = 2)
  })
  
  
  # Make results available for download
   down.load.dt <- function(dt){
    downloadHandler(
      filename <- function() {
        paste0("Results ", Sys.Date(), ".csv")
      },
      content <- function(file) {
        write.csv(dt, file, row.names = F, na = "")
    }
  )}

  output$download_table <- down.load.dt(dt = isolate(reactive.get.results()))
  output$download_table_m <- down.load.dt(dt = isolate(reactive.get.results.gender())$m)
  output$download_table_f <- down.load.dt(dt = isolate(reactive.get.results.gender())$f)
}
# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
