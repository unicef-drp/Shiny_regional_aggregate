#
# Shiny app to aggregate selected countries in each region
# Yang Liu, 
# Oct.11, 2019
#
suppressPackageStartupMessages({
library("shiny")    # for shiny apps
library("shinyWidgets")
library("leaflet")  # for openstreetmap
library("maps")     # provide shap files for selected countries
library("maptools") # modify shap files
library("DT")       # for shiny table 
library("ggplot2")
library("data.table") 
})
options("digits" = 2)
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
                  label = "Select all countries by default", value = TRUE),

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
        # plots:
        h3("Results"),
        textOutput(outputId = "selected_countries_on_file"),
        plotOutput("plot_rate"),
        plotOutput("plot_death"),
        br()
      ),
      tabPanel("Tables",
               # data:
               h3("The most recent five years"),
               DT::dataTableOutput("results_table_recent", width = "80%"),
               h3("All the aggregated results"),
               # optional to download
               downloadButton("download_table", "Download Results"),
               DT::dataTableOutput("results_table", width = "80%"),
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
  # filter and return a vector of country names by selected region
  reactive.get.sub.c = reactive({
    # the countries selected, select by renderUI:country_input:
    dc[dc$MDGRegion1%in%input$supplied_region, OfficialName]
  })
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
  
  # check the names in `country.info.CME_adhoc.csv` file and 
  # run David's script `6outputaggregates.R`
  reactive.get.results <- eventReactive(input$click_run, {
    cnames <- data.table::fread("input/country.info.CME_adhoc.csv")[AdhocCountries=="Adhoc", sort(OfficialName)]
    # only run if countries to be run >0 and equal selected countries
    if (length(cnames) >0 & identical(sort(cnames), sort(input$country_input_more))) {
        # showModal will show that the scripe is running, and removed when scripts are done 
        showModal(modalDialog("Running aggregate for", paste(cnames, collapse = ", "), footer=NULL))
          source(here::here("6outputaggregates.R"))
          message("Read the results file: 'Rates & Deaths_AdhocCountries.csv'.")
        removeModal()
        if(!exists("file.dir.median")) file.dir.median <- "Aggregate results (median) 2019-08-15"
        return(fread(here::here(file.dir.median, "Rates & Deaths_AdhocCountries.csv")))
      } else {
        showModal(modalDialog(
          "Please press <1. Confirm your selection> first.\n  The countries on the record file now are ", 
          paste(cnames, collapse = ", "), ", which doesn't match your selected countries."
        ))
        return(NULL)
      }
    
  })
  
  # Figure 1: Rate by year
  output$plot_rate <- renderPlot({
    if (is.null(reactive.get.results())){
      return()
    }
    dt_long <- data.table::melt(reactive.get.results(), measure.vars = c("U5MR median", "IMR median", "NMR median"), 
                                value.name = "rate", variable.name = "type_of_rate")
    ggplot(dt_long[!is.na(rate),], aes(x = Year, y = rate, color = Region, type = Region)) +
      geom_line(size = 1) + 
      theme_bw() + 
      ggtitle("Median U5MR, IMR, and NMR by Year") + 
      labs(y = "Deaths per 1,000 live births") + 
      facet_wrap(facets = ~ type_of_rate) + 
      theme(legend.position = "bottom") + 
      scale_color_discrete(labels = c("Selected Countries", "World")) +
      theme(plot.title = element_text(size = 20))
    
    
  })
  
  # Figure 2: Number of deaths by year 
  output$plot_death <- renderPlot({
    dt <- reactive.get.results()
    if (is.null(dt)){
      return()
    }
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
  })
  
  # print results as data table 
  # a function to select columns and define some format 
  get.table <- function(){
    if (is.null(reactive.get.results())){
      return()
    }
    # remove some columns 
    dt <- reactive.get.results()
    dt <- dt[, -(grep("Population|population", colnames(dt), value = TRUE)), with = FALSE]
    dt <- dt[Year>=1990][order(-Year)] # reorder
    return(dt)
    # DT::formatRound(datatable(dt), columns = grep("median", colnames(dt), value = TRUE), digits = 2)
  }
    
  output$results_table_recent <-  DT::renderDT({
    # only print the most recent 5 years of Adhoc group, order by year
    DT::formatRound(
      DT::datatable(get.table()[Region=="Adhoc"][1:5,-1]),
      # format number columns with digits
      columns = c("U5MR median", "IMR median", "NMR median"), digits = 2)
  })
  
  output$results_table <- DT::renderDT({
    DT::formatRound(
      DT::datatable(get.table()), 
      columns = c("U5MR median", "IMR median", "NMR median"), digits = 2)
  })
  
  # Make results available for download
  output$download_table <- downloadHandler(
    filename <- function() {
      paste0("Results ", Sys.Date(), ".csv")
    },
    content <- function(file) {
      write.csv(get.table(), file, row.names = F, na = "")
    }
  )
  
}
# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
