#
# Shiny app to aggregate selected countries in each region
# Please click "Run App" or type shiny::runApp() to run. Don't run all the code (slower).
#
# Yang Liu, 
# Oct.11, 2019
#
pkg_vt <- rownames(installed.packages())
for (pck in c("shiny", "shinyWidgets", "leaflet",
              "maps", "maptools", "rgeos",
              "DT","data.table", "here", 
              "ggplot2", "scales", "plotly")){
  if(!pck %in% pkg_vt){install.packages(pck)}
}

source(here::here("R_shiny/helper.R"))
# update packages if certain visions are required
invisible(sapply(c("shiny", "DT", "data.table"), update.package.version))

suppressPackageStartupMessages({
  library("here")
  library("shiny")    # for shiny apps
  library("shinyWidgets")
  library("leaflet")  # for openstreetmap
  library("maps")     # provide shap files for selected countries
  library("maptools") # modify shap files
  library("rgeos")
  library("DT")       # for shiny table 
  library("data.table") 
  library("ggplot2")
  library("scales")
  library("plotly")
})


# Language -------------------------------------------------------------------
note_header <- "This ShinyApp produces median estimates (no uncertainty interval) of
Under-five Mortality Rate (U5MR), Infant Mortality Rate (IMR), and Neonatal
Mortality Rate (NMR). Only U5MR and IMR estimates are produced for
sex-specific estimates."

note_map <- "Note: This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."

panel_title1   <- "Plots for Aggregated Results"
panel_title1.2 <- "Tables for Aggregated Results"
panel_title2 <- "Sex-specific Aggregated Results"

#' function to rename "Adhoc" in dataset
change.dt.name <- function(dt){
  dt[Region=="Adhoc", Region:= "New Region"]
  return(dt)
}

# Dataset and Parameters -----------------------------------------------------------------


# dc: country.info.CME dataset
dc <- fread(here::here("input", "country.info.CME.csv"))

# Define region
dc[, UNICEF_region:= ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
dc[, SDG_region:= ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
dc$SDG_region <- revise.name(dc$SDG_region, new_list = SDG_list)
regions_1 <- sort(dc[, unique(UNICEF_region)])
regions_2 <- sort(dc[, unique(SDG_region)])
# a list by countries grouped by regions for shinyWidgets::pickerInput(country_input)
input_country_list <- list()
for (i in 1:length(regions_1)){
  input_country_list[[regions_1[i]]] <- sort(unique(dc[UNICEF_region==regions_1[i], OfficialName]))
}

# Country names
countries <- sort(dc[,unique(OfficialName)])

# Get world map with modified country names 
world_map <- get.world.map()

# Source the About panel, which is a function to be called in ui
tabPanel.about <- source(here::here("R_shiny/about.R"))$value
  
# Show data from: 
year_started <- 1985


# UI: side panel ----------------------------------------------------------

ui = fluidPage(
  # head panel with pic
  source(here::here("R_shiny/headerPanel.R"), local = TRUE)$value,
  
  # side panel
  sidebarPanel(
    titlePanel("Aggregate Selected Countries"), # title
    p(note_header), 
    
    # country_input
    shinyWidgets::pickerInput(
      inputId = "country_input", label = "Please Select Countries", 
      choices = input_country_list, 
      selected = c("Finland"), 
      multiple = TRUE,  # allow multiple selection
      options = list(
        title = "Please select countries",
        `actions-box` = TRUE, 
        size = 12
      )),    
    br(),
    
    p("You can also further revise the selection here:"),
    # country_input_select
    uiOutput("country_select_more"),
    br(),
    
    p(strong("Run the Aggregate")),
    # run_gender
    shinyWidgets::switchInput(inputId = "run_gender", 
                              label = strong("Run sex-specific results?"), value = FALSE,
                              onLabel = "Yes", offLabel = "No", labelWidth = "300px", inline = TRUE),
    br(),
    # click_run
    actionButton("click_run",  strong("Run Aggregates")),
    br(),br(),
    
    # show_world
    conditionalPanel("input.click_run",
    shinyWidgets::switchInput(inputId = "show_world", 
                              label = "Show results for the world in plots", value = FALSE,
                              onLabel = "Yes", offLabel = "No", labelWidth = "300px", inline = TRUE)
    )
  ),
  

  # UI: main panel ----------------------------------------------------------
  mainPanel(
  # plots (and map)
    tabsetPanel(
      tabPanel("Plots",
        h4("The list of selected countries"),
        textOutput(outputId = "selected_countries"),
        
        # Plot of Aggregated Results
        uiOutput("panel_plot_rate"),
        
        # Plot of Sex-specific Aggregated Results
        uiOutput("panel_plot_rate_gender"),
        # end conditionalPanel
        
        br(),br(),
        h4("Map"),
        leafletOutput(outputId = "mymap"),
        p(note_map)
      ),
  # tables
      tabPanel("Tables and Data Download",
        uiOutput("panel_results_table"),
        br(),br(),
        
        uiOutput("panel_results_table_gender") 
      ),      
  # About
      tabPanel.about()
    ) #tabsetPanel
  ), #mainpanel
theme = "bootstrap.css"
) #ui fluidpage

# Server ------------------------------------------------------------------

server = function(input, output, session) {
  # renderUI: side panel ---------------------------------------
  # `country_input_select` is further modified list
  output$country_select_more <- renderUI({
     selectInput(inputId = 'country_input_select', label = 'Selected Countries', choices = countries, 
                selected = input$country_input, 
                multiple = TRUE, selectize = TRUE)
    
  })
  
  # Update selected in country_input (shinyWidgets::pickerInput)
  observeEvent(input$country_input_select, {
    updatePickerInput(session, inputId = "country_input", selected = input$country_input_select)
  })


  # renderUI: main panel ----------------------------------------------------------------
  # renderUI: print the selected countries in app 
  output$selected_countries  = renderText({
    paste(sort(input$country_input_select), collapse = ", ")
  })
  
  # renderUI: map the selected countries (if available in this map)
  output$mymap = renderLeaflet({
    validate(need(input$country_input_select, "Please select countries."))
    # plot on pre-loaded world_map from get.world.map()
    leaflet::leaflet() %>% addProviderTiles("Esri.WorldTopoMap", provider = "Esri") %>%
      leaflet::addPolygons(data = subset(world_map, country %in% input$country_input_select), weight = 1)
  })
  
  # renderUI: plots
  output$panel_plot_rate <- renderUI({
    if (is.null(reactive.run())){
      return()
    }
    fluidRow(
      br(),
      h3(panel_title1),
      plotlyOutput("plot_rate", inline = TRUE),
      br(),
      plotlyOutput("plot_death", inline = TRUE),
      br(),br()
    )
  })
  # renderUI: plots by sex
  output$panel_plot_rate_gender <- renderUI({
    if (is.null(reactive.run()$m)){
      return()
    }
    if(!input$run_gender) return()
    fluidRow(
      h3(panel_title2),
          if (input$show_world) {
          plotlyOutput("plot_rate_gender", inline = TRUE, height = "800px")
          } else {plotlyOutput("plot_rate_gender", inline = TRUE)}
      
    )
  })
  
  # renderUI:tables
  output$panel_results_table <- renderUI({
    if (is.null(reactive.run())){
      return()
    }
    fluidRow(
      h3(panel_title1.2),
      # optional to download
      downloadButton("download_table", "Download"),
      br(),br(),
      DT::dataTableOutput("results_table", width = "80%")
    )
  })
  # renderUI:tables by sex
  output$panel_results_table_gender <- renderUI({
    if (is.null(reactive.run()$m)){
      return()
    }
    if(!input$run_gender) return()
    fluidRow(            
      h3(panel_title2),
      p(strong("Data for the female")),
      downloadButton("download_table_f", "Download"),
      br(),br(),
      DT::dataTableOutput("results_table_f", width = "80%"),
      
      br(),
      p(strong("Data for the male")),
      downloadButton("download_table_m", "Download"),
      br(),br(),
      DT::dataTableOutput("results_table_m", width = "80%")      
    )
  })
  
  # Main Reactive ----------------------------------------------------
  # check the names in `country.info.CME_adhoc.csv` file and 
  # run David's script `6outputaggregates.R`
  reactive.run <- eventReactive(input$click_run, {
    if (length(input$country_input_select)==0) {
      showModal(modalDialog(title = "Please select countries first.",  
                            "You may click anywhere to dismiss this message", easyClose = TRUE))
    } else {
    
    dc[,AdhocCountries:=""]
    dc[OfficialName %in% input$country_input_select, AdhocCountries:="Adhoc"]
    write.csv(dc, file = here::here("input", "country.info.CME_adhoc.csv"))
    time0 <- Sys.time()
    # sex-specific?
    if(!input$run_gender){
      # showModal will show that the scripe is running, and removed when scripts are done 
      showModal(modalDialog(title = paste("Running aggregate for", paste(input$country_input_select, collapse = ", "), 
                                          "."),
                            HTML("<br> It takes about 10 - 20 seconds."), footer=NULL))
    } else {
      showModal(modalDialog(title = paste("Running sex-specific aggregate for", paste(input$country_input_select, collapse = ", "),
                            "."), 
                            HTML("<br> It takes about 20 - 30 seconds."), footer=NULL))
    }
    # where we run the aggregates:
    source(here::here("6outputaggregates.R"))
    
    if(input$run_gender){
      source(here::here("6outputaggregates_gender.R"))
    }
    
    removeModal()
    message("Time spent is ", round(Sys.time() - time0, 1), " seconds.")
    if(!input$run_gender){
          return(list(all = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15", "Rates & Deaths_AdhocCountries.csv")))))
        } else {        
          return(
              list(
                  all = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15", "Rates & Deaths_AdhocCountries.csv"))),
                  m = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15 (male)", "Rates & Deaths_AdhocCountries.csv"))),
                  f = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15 (female)", "Rates & Deaths_AdhocCountries.csv")))
                  )
                )
        }
     
    } # for the length!=0 check
  })
  
  # Figure 1 Rate by Year ---------------------------------------------------
  # some helper functions:
  get.long <- function(dt, vars0){
    dt <- dt[Year>=year_started,]
    dt_long <- data.table::melt(dt, measure.vars = vars0, 
                                value.name = "rate", variable.name = "type_of_rate")
    return(dt_long[,.(Region, Year, rate, type_of_rate)])
  }
  
  plot.rate <- function(dt, vars0){
    dt_long <- get.long(dt, vars0)[!is.na(rate), Rate:=round(rate,2)]
    levels(dt_long$type_of_rate) <- revise.name(levels(dt_long$type_of_rate), new_list = new_varname_list)
    p = ggplot(dt_long, aes(x = Year, y = Rate, color = Region, type = Region)) +
      geom_line(size = 1) + 
      theme_bw() + 
      # ggtitle("U5MR, IMR, and NMR by Year") + 
      labs(y = "", x = "") +
      facet_wrap(facets = ~ type_of_rate) + 
      scale_color_manual(values = cp_UNICEF_div[(1:uniqueN(dt_long$Region))*2])+
      theme(legend.position = "bottom") + 
      # scale_color_discrete(labels = c("New Region (Selected Countries)", "World")) +
      theme(plot.title = element_text(size = 20))
    
    return(plotly::ggplotly(p, tooltip = c("Year", "Rate"))%>% layout(yaxis = list(title = "Deaths per 1,000 live births"),
                                  xaxis = list(title = "Year")))
  }
  
  output$plot_rate <- plotly::renderPlotly({
    if (is.null(reactive.run())){
      return()
    }
    vars0 <- c("U5MR median", "IMR median", "NMR median")
    dt <- reactive.run()$all
    if(!input$show_world) dt <- dt[Region=="New Region",]
    plot.rate(dt, vars0)
  })
  

  # Figure 2. death by year -------------------------------------------------
  plot.death <- function(dt){
    dt <- dt[Year>=year_started,]
    dt_long <- data.table::melt(dt, measure.vars = grep("deaths", colnames(dt), value = TRUE), 
                                  value.name = "Deaths", variable.name = "type")
    levels(dt_long$type) <- revise.name(levels(dt_long$type), new_list = new_varname_list)
    # dt_long[, Death_Number_1K := round(Deaths/1E3)]
    p = ggplot(dt_long[!is.na(Deaths),], aes(x = Year, y = Deaths, color = Region)) +
        geom_line(size = 1) + 
        theme_bw() + 
        # ggtitle("Deaths of U5MR, IMR, and NMR by Year") + 
        labs(y = "", x = "", color = "") +
        facet_wrap(facets= ~ type) + 
        theme(legend.position="bottom") + 
        # scale_y_continuous(label=scales::comma) + 
        scale_color_manual(values = cp_UNICEF_div[(1:uniqueN(dt_long$Region))*2])+
        theme(plot.title = element_text(size = 20))
    plotly::ggplotly(p, tooltip = c("Year", "Deaths"))%>% 
                                  layout(yaxis = list(title =  "Number of Deaths"),
                                  xaxis = list(title = "Year"))
  }

  output$plot_death <- plotly::renderPlotly({
    if (!is.null(reactive.run())){
    dt <- reactive.run()$all
    if(!input$show_world) dt <- dt[Region=="New Region",]
    plot.death(dt)
    }
  })


  # Figure 3. Rate by Sex ---------------------------------------------------
  output$plot_rate_gender <- plotly::renderPlotly({
    if (is.null(reactive.run()$m)){
      return()
    }
    vars0 <- c("U5MR median", "IMR median")
    dt_long_m <- get.long(reactive.run()$m, vars0)
    dt_long_f <- get.long(reactive.run()$f, vars0)
    dt_long_m$gender <- "Male"
    dt_long_f$gender <- "Female"
    dt_long <- rbind(dt_long_f, dt_long_m)
    dt_long$Region <- as.factor(dt_long$Region)
    # show world or not? 
    if(!input$show_world) dt_long <- dt_long[Region=="New Region",]
    levels(dt_long$type_of_rate) <- revise.name(levels(dt_long$type_of_rate), new_list = new_varname_list)
    
    p = ggplot(dt_long[!is.na(rate), Rate := round(rate,2)], aes(x = Year, y = Rate, color = gender)) +
      geom_line(size = 1) + 
      theme_bw() + 
      # ggtitle("U5MR and IMR by Gender and Year", subtitle = "Selected Region vs. the World") + 
      labs(y = "", x = "",  color = "") +
      facet_wrap(facets = ~ type_of_rate + Region) + 
      theme(legend.position = "bottom") + 
      theme(plot.title = element_text(size = 20))
    
    plotly::ggplotly(p, tooltip = c("Year", "Rate"))%>% 
      layout(yaxis = list(title = "Deaths per 1,000 live births"),
             xaxis = list(title = "Year"))
  })
  

  # Tables ------------------------------------------------------------------
  #' a function to select columns and define some format
  get.table <- function(dt){
    # remove some columns
    dt <- dt[, -(grep("Population|population", colnames(dt), value = TRUE)), with = FALSE]
    dt <- dt[Year>=year_started][order(Region, -Year)] # reorder
    return(dt)
  }

  output$results_table <- DT::renderDT({
    if (!is.null(reactive.run())){
    dt <- reactive.run()$all
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("U5MR median", "IMR median", "NMR median"), digits = 2)
    }
  })

  output$results_table_m <- DT::renderDT({
    if (!is.null(reactive.run()$m)){
    dt <- reactive.run()$m
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("U5MR median", "IMR median"), digits = 2)
    }
  })

  output$results_table_f <- DT::renderDT({
    if (!is.null(reactive.run()$f)){
    dt <- reactive.run()$f
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("U5MR median", "IMR median"), digits = 2)
    }
  })


  # Make results available for download
   down.load.dt <- function(dt, name0 = ""){
    downloadHandler(
      filename = function() {
        paste0("Results",name0,"_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
      },
      content = function(file) {
        write.csv(dt, file, row.names = FALSE, na = "")
    }
  )}

  output$download_table <- down.load.dt(dt = get.table(reactive.run()$all))
  output$download_table_f <- down.load.dt(dt = get.table(reactive.run()$f), name0 = "_female")
  output$download_table_m <- down.load.dt(dt = get.table(reactive.run()$m), name0 = "_male")
  
}
# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
