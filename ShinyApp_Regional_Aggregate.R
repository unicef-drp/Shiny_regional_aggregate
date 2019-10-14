##########################################################
# Shiny app to aggregate selected countries in each region
# Yang Liu, 
# Oct.11, 2019
##########################################################


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
note1 <- "Note 1. This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."

# dc: country.info.CME dataset; regions: MDG regions in country.info.CME
dc <- fread(here::here("input", "country.info.CME.csv"))
regions <- sort(dc[, unique(MDGRegion1)])
countries <- sort(dc[,unique(CountryName)])

# User Interface ----------------------------------------------------------

ui = fluidPage(
  # side panel
  sidebarPanel(
    titlePanel("Aggregate Selected Countries"),
    selectInput(inputId = "supplied_region", label = "World Regions",
                       choices = regions,
                       selected = "Northern Africa"),
    # select all or not
    shinyWidgets::materialSwitch(inputId = "select_all", 
                  label = "Select all countries by default", value = TRUE),

    # populate the country selection in the meun
      p("Available countries:"),
      uiOutput("country_in_region"),
    
      p("If wish to add more countries:"),
      uiOutput("country_out_region")

  ),
  
  # main panel 
  mainPanel(
    h3("Map of Selected Countries"),
    leafletOutput(outputId = "mymap"),
    p(note1),
    h4("Country List:"),
    textOutput(outputId = "selected_countries"),
    br(), 
    h3("Actions"),
    actionButton("click_write",  "1. Write selected", width = "40%"),
    actionButton("click_check",  "2. Check countries", width = "40%"),
    br(),
    actionButton("click_run",    "3. Run outputaggregates", width = "40%"),
    actionButton("click_show",   "4. Show results", width = "40%"),
    br(),
    # plots:
    h3("Results"),
    plotOutput("plot_rate"),
    plotOutput("plot_death"),
    br(), 
    # data:
    h3("The most recent five years"),
    DT::dataTableOutput("results_table_recent"),
    h3("All the aggregated results"),
    DT::dataTableOutput("results_table"),
    br()
    )
)


# Server ------------------------------------------------------------------

server = function(input, output, session) {
  # filter and return a vector of country names by selected region
  get.sub.c = reactive({
    # the countries selected, select by renderUI:country_input:
    dc[dc$MDGRegion1%in%input$supplied_region, CountryName]
  })
  # render check boxes of countries to send back to ui
  output$country_in_region <- renderUI({
    checkboxGroupInput(inputId = "country_input", label = "Countries",
                       choices = get.sub.c(), selected = if(input$select_all) get.sub.c() else NULL)
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
        `actions-box` = TRUE, 
        size = 11,
        `selected-text-format` = "count > 3"
      )
    )
  })
  
  # map the selected countries (if available in this map)
  output$mymap = renderLeaflet({
    validate(need(input$country_input_more, "Please select countries."))
    # load map
    world <- maps::map("world", fill=TRUE, plot=FALSE)
    world_map <- maptools::map2SpatialPolygons(world, sub(":.*$", "", world$names))
    world_map <- sp::SpatialPolygonsDataFrame(world_map, data.frame(country = names(world_map), 
                                                     stringsAsFactors = FALSE), match.ID = FALSE)
    # world_map$country[!world_map$country%in%dc$CountryName]
    world_map$country <- plyr::revalue(world_map$country, c("UK" = "United Kingdom", 
                                                            "USA" = "United States of America",
                                                            "Republic of Congo" = "Congo",
                                                            "Democratic Republic of the Congo" = "Congo DR"))
    leaflet() %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
      addPolygons(data = subset(world_map, country %in% input$country_input_more), weight = 1)
    })
  
  # print the selected countries in app 
  output$selected_countries  = renderText({
    paste(sort(input$country_input_more), collapse = ", ")
  })
  
  # write selected countries into the master 'country.info.CME_adhoc.csv' file
  observeEvent(input$click_write, {
    # clear the current column (or create new), then write the wanted countries
    dc[,AdhocCountries:=""]
    dc[CountryName%in%input$country_input_more, AdhocCountries:="Adhoc"]
    write.csv(dc, file = here::here("input", "country.info.CME_adhoc.csv"))
  })
  
  # double check if the adhoc countries to run is correct
  observeEvent(input$click_check, {
    if(file.exists(here::here("input", "country.info.CME_adhoc.csv"))){
      message("file created")
      
      dc2 <- data.table::fread(here::here("input", "country.info.CME_adhoc.csv"))
      message("Read in and check the file.\n Adhoc countries are ", paste(dc2[AdhocCountries=="Adhoc", sort(CountryName)], collapse = ", "))
      
      
    } else {
      warning("file not created")
    }
  })
  
  # run the David's script
  observeEvent(input$click_run, {
    cnames <- data.table::fread("input/country.info.CME_adhoc.csv")[AdhocCountries=="Adhoc", sort(CountryName)]
    
    
    if (length(cnames) >0) {
        message("Double check the file.\n Adhoc countries are ", cnames, collapse = ", ")
        source(here::here("6outputaggregates.R")) 
      } else {
        warning("No country selected in the file.")
      }
  })
  
  # An reactive action to read the result dataset, trigger the printing of figures and tables 
  show.results <- eventReactive(input$click_show, {
    if (!exists("file.dir.median")) file.dir.median <- "Aggregate results (median) 2019-08-15"
    if (file.exists(here::here(file.dir.median, "Rates & Deaths_AdhocCountries.csv"))){
      message("Read the results file: 'Rates & Deaths_AdhocCountries.csv'.")
      fread(here::here(file.dir.median, "Rates & Deaths_AdhocCountries.csv"))
    } else {NULL}
  })

  # Figure 1: Rate by year
  output$plot_rate <- renderPlot({
    if (is.null(show.results())){
      return()
    }
    dt_long <- data.table::melt(show.results(), measure.vars = c("U5MR median", "IMR median", "NMR median"), 
                                value.name = "rate", variable.name = "type_of_rate")
    ggplot(dt_long[!is.na(rate),], aes(x = Year, y = rate, color = Region, type = Region)) +
      geom_line(size = 1) + 
      theme_bw() + 
      ggtitle("Median U5MR, IMR, and NMR by Year") + 
      labs(y = "Deaths per 1,000 live births") + 
      facet_wrap(facets= ~ type_of_rate) + 
      theme(legend.position="bottom") + 
      scale_color_discrete(labels = c("Selected Countries", "World"))
    
  })
  
  # Figure 2: Number of deaths by year 
  output$plot_death <- renderPlot({
    dt <- show.results()
    if (is.null(dt)){
      return()
    }
    dt_long <- data.table::melt(show.results(), measure.vars = grep("deaths", colnames(dt), value = TRUE), 
                                value.name = "deaths", variable.name = "type")
    ggplot(dt_long[!is.na(deaths),], aes(x = Year, y = log10(deaths), color = Region)) +
      geom_line(size = 1) + 
      theme_bw() + 
      ggtitle("Median Deaths of U5MR, IMR, and NMR by Year") + 
      labs(y = "log10(Deaths)") + 
      facet_wrap(facets= ~ type) + 
      theme(legend.position="bottom") + 
      scale_color_discrete(labels = c("Selected Countries", "World"))
  })
  
  # print results as data table 
  output$results_table_recent <-  DT::renderDataTable({
    dt2 <- copy(show.results())
    # only print the most recent 5 years
    dt2[Region=="Adhoc"][order(-Year),][1:5,-1]
  })
  
  output$results_table <- DT::renderDataTable({
    # print all 
    show.results()
  })
}

# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
