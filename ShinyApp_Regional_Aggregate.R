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
    actionButton("click_write",  "1. Confirm your selection", width = "50%"),
    br(),
    actionButton("click_run",    "2. Run aggregates", width = "50%"),
    br(),
    # plots:
    h3("Results"),
    textOutput(outputId = "selected_countries_on_file"),
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
    showModal(modalDialog("Selection confirmed:", paste(input$country_input_more, collapse = ", "),
                          footer = modalButton("OK"), size = "s"))
    dc[,AdhocCountries:=""]
    dc[CountryName%in%input$country_input_more, AdhocCountries:="Adhoc"]
    write.csv(dc, file = here::here("input", "country.info.CME_adhoc.csv"))
    # removeModal()
  })
  
  # check the names in `country.info.CME_adhoc.csv` file and 
  # run David's script `6outputaggregates.R`
  get.results <- eventReactive(input$click_run, {
    cnames <- data.table::fread("input/country.info.CME_adhoc.csv")[AdhocCountries=="Adhoc", sort(CountryName)]
    # only run if countries to be run >0 and equal selected countries
    if (length(cnames) >0 & identical(sort(cnames), sort(input$country_input_more))) {
        # showModal will show that the scripe is running 
        showModal(modalDialog("Running aggregate for", paste(cnames, collapse = ", "), footer=NULL))
          source(here::here("6outputaggregates.R"))
          message("Read the results file: 'Rates & Deaths_AdhocCountries.csv'.")
        removeModal()
        return(fread(here::here(file.dir.median, "Rates & Deaths_AdhocCountries.csv")))
      } else {
        showModal(modalDialog(
          "Please press <1. Confirm your selection> first.\n  The countries on the record file now are ", 
          paste(cnames, collapse = ", "), ", which doesn't match your selected countries."
        ))
        return(NULL)
      }
    
    # output results 
  })

  # Figure 1: Rate by year
  output$plot_rate <- renderPlot({
    if (is.null(get.results())){
      return()
    }
    dt_long <- data.table::melt(get.results(), measure.vars = c("U5MR median", "IMR median", "NMR median"), 
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
    dt <- get.results()
    if (is.null(dt)){
      return()
    }
    dt_long <- data.table::melt(dt, measure.vars = grep("deaths", colnames(dt), value = TRUE), 
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
    if (is.null(get.results())){
      return()
    }
    # only print the most recent 5 years
    get.results()[Region=="Adhoc"][order(-Year),][1:5,-1]
  })
  
  output$results_table <- DT::renderDataTable({
    if (is.null(get.results())){
      return()
    } 
    # prints all results
    get.results()
  })
}

# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
