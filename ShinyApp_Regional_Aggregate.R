##########################################################
# Shiny app to aggregate selected countries in each region
# Yang Liu, 
# Oct.11, 2019
##########################################################


suppressPackageStartupMessages({
library("shiny")    # for shiny apps
library("leaflet")  # renderLeaflet map 
library("spData")   # loads the world dataset 
library("DT")       # for shiny table 
library("ggplot2")
library("data.table") 
})
note1 <- "Note 1. This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."

# dc: dataset of country.info.CME
dc <- fread(here::here("input", "country.info.CME.csv"))
regions <- sort(unique(dc$MDGRegion1))

ui = fluidPage(
  # side panel
  sidebarPanel(
    titlePanel("Aggregate Selected Countries"),
    selectInput(inputId = "supplied_region", label = "World Regions",
                       choices = regions,
                       selected = "Northern Africa"),
    # select all? 
    checkboxInput(inputId = "select_all", 
                  label = "Select all countries by default", value = TRUE),
    
    # populate the country selection
      p("Available countries:"),
      uiOutput("country_in_region")
  ),
  
  # main panel 
  mainPanel(
    h2("World Map"),
    br(),
    leafletOutput(outputId = "mymap"),
    p(note1),
    br(),
    h3("Countries selected are:"),
    textOutput(outputId = "selected_countries"),
    br(), br(),
    # action buttons:
    actionButton("click_write",  "1. Write selected", width = "40%"),
    actionButton("click_check",  "2. Check countries", width = "40%"),
    br(),
    actionButton("click_run",    "3. Run outputaggregates", width = "40%"),
    actionButton("click_show",   "4. Show results", width = "40%"),
    br(),br(),
    # plots:
    h3("Plot the results"),
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

server = function(input, output, session) {
  # filter and return a vector of country names by selected region
  get.sub.c = reactive({
    # the countries selected, select by renderUI:country_input:
    dc[dc$MDGRegion1%in%input$supplied_region, CountryName]
  })
  # render check boxes of countries to select back in ui
  output$country_in_region <- renderUI({
    checkboxGroupInput(inputId = "country_input", label = "Countries",
                       choices = get.sub.c(), selected = if(input$select_all) get.sub.c() else NULL)
  })  
  
  # map the selected countries (if available in this map)
  output$mymap = renderLeaflet({
    if (is.null(input$country_input)){
      return()
    }
    leaflet() %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
      # world is the leaflet map dataset
      addPolygons(data = world[world$name_long%in%input$country_input,])
    })
  
  # print the selected countries in app 
  output$selected_countries  = renderText({
    paste(sort(input$country_input), collapse = ", ")
  })

  observeEvent(input$click_write, {
    # clear the current column (or create new), then write the wanted countries
    dc[,AdhocCountries:=""]
    dc[CountryName%in%input$country_input, AdhocCountries:="Adhoc"]
    write.csv(dc, file = here::here("input", "country.info.CME_adhoc.csv"))
  })
  
  # double check if the adhoc countries to run is correct
  observeEvent(input$click_check, {
    if(file.exists(here::here("input", "country.info.CME_adhoc.csv"))){
      message("file created")
      dc2 <- fread(here::here("input", "country.info.CME_adhoc.csv"))
      message("Read in and check the file.\n Adhoc countries are ", paste(dc2[AdhocCountries=="Adhoc", sort(CountryName)], collapse = ", "))
    } else {
      warning("file not created")
    }
  })
  
  # run the David's script
  observeEvent(input$click_run, {
    source(here::here("6outputaggregates.R"))
  })
  
  # An reactive action to read the result dataset, trigger the printing of figures and tables 
  show.results <- eventReactive(input$click_show, {
    if(file.exists(here::here(file.dir.median, "Rates & Deaths_AdhocCountries.csv"))){
      message("Read the file 'Rates & Deaths_AdhocCountries.csv'.")
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
    ggplot(dt_long[!is.na(rate),], aes(x = Year, y = rate, color = Region)) +
      geom_line(size = 0.5) + 
      theme_bw() + 
      ggtitle("Median U5MR, IMR, and NMR by Year") + 
      labs(y = "Deaths per 1,000 live births") + 
      facet_wrap(facets= ~type_of_rate) + 
      theme(legend.position="bottom") + 
      scale_color_discrete(labels = c("Selected Countries", "World"))
    
  })
  
  # Figure 2: Number of deaths by year 
  output$plot_death <- renderPlot({
    dt <- show.results()
    if (is.null(dt)){
      return()
    }
    dt_long <- data.table::melt(dt, measure.vars = grep("deaths", colnames(dt), value = TRUE), 
                                value.name = "deaths", variable.name = "type")
    ggplot(dt_long[!is.na(deaths),], aes(x = Year, y = deaths, color = Region)) +
      geom_line(size = 0.5) + 
      theme_bw() + 
      ggtitle("Median Deaths of U5MR, IMR, and NMR by Year") + 
      labs(y = "Deaths") + 
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
shinyApp(ui, server)
