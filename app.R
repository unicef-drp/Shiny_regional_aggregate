#
# Shiny app to aggregate selected countries in each region
# Please click "Run App" or type shiny::runApp() to run. Don't run all the code (slow).
#
# Yang Liu, 
# Oct.11, 2019
#
# check packages
pkg_vt <- rownames(installed.packages())
for (pck in c("shiny", "shinyWidgets", "shinyjs", "leaflet",
              "maps", "maptools", "rgeos",
              "DT","data.table", "dplyr", "here", 
              "ggplot2", "scales", "plotly")){
  if(!pck %in% pkg_vt){install.packages(pck)}
}
# source code
invisible(sapply(list.files(here::here("R"), full.names = TRUE), source))

# update packages if certain visions are required
invisible(sapply(c("shiny", "DT", "data.table"), update.package.version))

suppressPackageStartupMessages({
  # listing the libraries is necessary if want to publish on shinyapps.io
  library("here")
  library("shiny")    # for shiny apps
  library("shinyWidgets")
  library("shinyjs")  # for  reset
  library("leaflet")  # for openstreetmap
  library("maps")     # provide shap files for selected countries
  library("maptools") # modify shap files
  library("rgeos")
  library("DT")       # for shiny table 
  library("data.table") 
  library("dplyr")
  library("ggplot2")
  library("scales")
  library("plotly")
  
})

# Language -------------------------------------------------------------------
note_header <- "This ShinyApp produces regional aggregates of child mortality
estimates based on individually selected countries. UN IGME’s latest estimates
of neonatal, infant and under-five mortality are used."

note_map <- "Note: This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."

panel_title1   <- "Results for selected regional aggregates"
panel_title1.2 <- "Tables for selected regional aggregates"
panel_note1.2 <- "Regional and world aggregates as well as country estimates are available for downloading."
panel_title2 <- "Sex-specific results for selected regional aggregates"

Adhoc_name <- "Selected Countries"

#' function to rename "Adhoc" in dataset
change.dt.name <- function(dt){
  dt[Region=="Adhoc", Region:= Adhoc_name]
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
regions_3 <- sort(dc[, unique(M49Region1)])
# a list by countries grouped by regions for shinyWidgets::pickerInput(country_input)
input_country_list <- list()
for (i in 1:length(regions_3)){
  input_country_list[[regions_3[i]]] <- sort(unique(dc[M49Region1==regions_3[i], OfficialName]))
}

# Country names
countries <- sort(dc[,unique(OfficialName)])

# Get world map with modified country names 
world_map <- get.world.map()

# Source the About panel, which is a function to be called in ui
tabPanel.about <- source(here::here("R_shiny/about.R"))$value
  
# Show data from: 
year_started <- 1985
year_ended <- 2018
default_select <- "Finland"

# median results for all countries
file_all <- here::here("Aggregate results (median) 2019-08-15", "Rates & Deaths_Country Summary.csv")
file_f <- here::here("Aggregate results (median) 2019-08-15 (female)", "Rates & Deaths_Country Summary.csv")
file_m <- here::here("Aggregate results (median) 2019-08-15 (male)", "Rates & Deaths_Country Summary.csv")
c_median_all <- get.data.all(file = file_all, year_started = year_started, year_ended = year_ended)
c_median_f <- get.data.all(file = file_f, gender0 = TRUE, year_started = year_started, year_ended = year_ended)
c_median_m <- get.data.all(file = file_m, gender0 = TRUE, year_started = year_started, year_ended = year_ended)

# for reset
jscode <- "shinyjs.refresh = function() { location.reload(); }"


# UI: side panel ----------------------------------------------------------

ui = fluidPage(
  # head panel with pic
  source(here::here("R_shiny/headerPanel.R"), local = TRUE)$value,
  
  # side panel
  sidebarPanel(
    
    useShinyjs(),
    extendShinyjs(text = jscode, functions = "refresh"),
    
    titlePanel("Aggregate Selected Countries"), # title
    p(note_header), 
    
    # country_input
    checkboxInput(inputId = "input_by_region", label = ("Group countries by the five world regions"), value = FALSE),
    uiOutput("panel_country_select"),    
    br(),
    
    p("You can also further revise the selection here by typing names. To delete, use backspace or select and delete:"),
    # country_input_select
    uiOutput("panel_country_select_more"),
    br(),
    
    # run_gender
    checkboxInput(inputId = "run_gender", label = strong("Run sex-specific results?"), value = FALSE),
    # click_run
    actionButton("click_run",  strong("Run Aggregates"), width = '200px'), 
    br(),br(), 
    actionButton("click_reset",  ("Reset App"), width = '200px')
  ),
  

  # UI: main panel ----------------------------------------------------------
  mainPanel(
  # plots (and map)
    tabsetPanel(
      tabPanel("Plots",
        h4("The results for selected countries"),
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
  observe({
    if (input$click_reset) {
      js$refresh();
    }
  })  
  
  # renderUI: side panel ---------------------------------------
  # `country_input` is the first level selection`
  output$panel_country_select <- renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "country_input", label = "Please Select Countries", 
      choices = if(input$input_by_region) input_country_list else countries, 
      selected = default_select, 
      multiple = TRUE,  # allow multiple selection
      options = list(
        title = "Please select countries",
        `actions-box` = TRUE, 
        size = 12
      ))
  })
  
  
  # `country_input_select` is further modified list
  output$panel_country_select_more <- renderUI({
     selectInput(inputId = 'country_input_select', label = 'Selected Countries', choices = countries, 
                selected = input$country_input, 
                multiple = TRUE, selectize = TRUE)
    
  })
  
  
  # Update selected in country_input (shinyWidgets::pickerInput)
  observeEvent(input$country_input_select, {
    updatePickerInput(session, inputId = "country_input", selected = input$country_input_select)
  })

  # Reset selection
  observeEvent(input$click_reset,{
    updatePickerInput(session, inputId = "country_input", selected = default_select)
    updateCheckboxInput(session, inputId = "run_gender", value = FALSE)
  })

  # renderUI: main panel ----------------------------------------------------------------
  # renderUI: print the selected countries in app 
  # output$selected_countries  = renderText({
  #   paste(sort(input$country_input_select), collapse = ", ")
  # })
  # 
  reactive.selected.countries <- eventReactive(input$click_run, {
    input$country_input_select
  })
  
  output$selected_countries <- renderText({
    paste(sort(reactive.selected.countries()), collapse = ", ")
  })
  
  # renderUI: map the selected countries (if available in this map)
  output$mymap = renderLeaflet({
    validate(need(input$country_input_select, "Please select countries."))
    # plot on pre-loaded world_map from get.world.map()
    leaflet::leaflet(options = leafletOptions(minZoom = 2, maxZoom = 5)) %>% 
      leaflet::addProviderTiles("Esri.WorldTopoMap", provider = "Esri") %>%
      leaflet::addPolygons(data = subset(world_map, country %in% input$country_input_select), weight = 1)
  })
  
  # renderUI: plots
  output$panel_plot_rate <- renderUI({
    if (is.null(reactive.run())){
      return()
    }
    fluidRow(
      # show_world
      h3(panel_title1),
      checkboxInput(inputId = "show_world", label = "Show results for the world in plots", value = FALSE),
      plotlyOutput("plot_rate", inline = F),
      br(),
      plotlyOutput("plot_death", inline = F),
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
      

      if (length(input$show_world != 0)){
        if (input$show_world) {
          plotlyOutput("plot_rate_gender", inline = F, height = "800px", width = "80%")
        } else {
          plotlyOutput("plot_rate_gender", inline = F, width = "80%")
        }}
    )
  })
  
  # renderUI:tables
  output$panel_results_table <- renderUI({
    if (is.null(reactive.run())){
      return()
    }
    fluidRow(
      h3(panel_title1.2),
      p(panel_note1.2),
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
          return(list(
                    all = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15", "Rates & Deaths_AdhocCountries.csv"))),
                    c_median_all = c_median_all[Region%in%input$country_input_select,]
                    ))
        } else {        
          return(
              list(
                  all = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15", "Rates & Deaths_AdhocCountries.csv"))),
                  m = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15 (male)", "Rates & Deaths_AdhocCountries.csv"))),
                  f = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15 (female)", "Rates & Deaths_AdhocCountries.csv"))),
                  c_median_f = c_median_f[Region%in%input$country_input_select,],
                  c_median_m = c_median_m[Region%in%input$country_input_select,]
                  ))
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
      labs(y = "", x = "", color = "", type = "") +
      facet_wrap(~ type_of_rate) +
      scale_color_manual(values = cp_UNICEF_div[(1:uniqueN(dt_long$Region))*2]) +
      scale_x_continuous(breaks = c(1990, 2000, 2010, year_ended))

    return(plotly::ggplotly(p, tooltip = c("Year", "Rate"))%>% 
             layout(yaxis = list(title = "Deaths per 1,000 live births"),
                    legend = list(orientation = "h", x = 0.4, y = -0.1)))
  }
  
  output$plot_rate <- plotly::renderPlotly({
    if (is.null(reactive.run())){
      return()
    }
    vars0 <- c("U5MR median", "IMR median", "NMR median")
    dt <- reactive.run()$all
    if(!input$show_world) dt <- dt[Region==Adhoc_name,]
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
        # scale_y_continuous(label=scales::comma) + 
        scale_color_manual(values = cp_UNICEF_div[(1:uniqueN(dt_long$Region))*2])+
        scale_x_continuous(breaks = c(1990, 2000, 2010, year_ended)) +
        facet_wrap(facets= ~ type) + 
        theme(legend.position="bottom") 
      
    plotly::ggplotly(p, tooltip = c("Year", "Deaths"))%>% 
                                  layout(yaxis = list(title =  "Number of Deaths"),
                                         legend = list(orientation = "h", x = 0.4, y = -0.1))
  }

  output$plot_death <- plotly::renderPlotly({
    if (!is.null(reactive.run())){
    dt <- reactive.run()$all
    if(!input$show_world) dt <- dt[Region==Adhoc_name,]
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
    if(!input$show_world) dt_long <- dt_long[Region==Adhoc_name,]
    levels(dt_long$type_of_rate) <- revise.name(levels(dt_long$type_of_rate), new_list = new_varname_list)
    
    p = ggplot(dt_long[!is.na(rate), Rate := round(rate,2)], aes(x = Year, y = Rate, color = gender)) +
      geom_line(size = 1) + 
      theme_bw() + 
      # ggtitle("U5MR and IMR by Gender and Year", subtitle = "Selected Region vs. the World") + 
      labs(y = "", x = "",  color = "") +
      scale_x_continuous(breaks = c(1990, 2000, 2010, year_ended)) +
      facet_wrap(facets = ~ type_of_rate + Region) +
      # increase margin to avoid flattened strip when pass to `plotly`
      theme(strip.text.x = element_text(margin = margin(.3, 0, .3, 0, "cm")))

    plotly::ggplotly(p, tooltip = c("Year", "Rate"))%>% 
      layout(yaxis = list(title = "Deaths per 1,000 live births"))
  })
  

  # Tables ------------------------------------------------------------------
  #' a function to select columns and define some format
  get.table <- function(dt){
    # remove some columns
    dt <- dt[, -(grep("Population|population", colnames(dt), value = TRUE)), with = FALSE]
    dt <- dt[Year>=year_started][order(Region, -Year)] # reorder
    # rename 
    colnames(dt) <- revise.name(colnames(dt), new_list = new_varname_list)
    return(dt)
  }

  output$results_table <- DT::renderDT({
    if (!is.null(reactive.run())){
    dt <- reactive.run()$all
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate", "Neonatal Mortality Rate"), digits = 2)
    }
  })

  output$results_table_m <- DT::renderDT({
    if (!is.null(reactive.run()$m)){
    dt <- reactive.run()$m
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate"), digits = 2)
    }
  })

  output$results_table_f <- DT::renderDT({
    if (!is.null(reactive.run()$f)){
    dt <- reactive.run()$f
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate"), digits = 2)
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

  output$download_table <- down.load.dt(dt = rbind(get.table(reactive.run()$all), 
                                                   reactive.run()$c_median_all))
  output$download_table_f <- down.load.dt(dt = rbind(get.table(reactive.run()$f),
                                                               reactive.run()$c_median_f), name0 = "_female")
  output$download_table_m <- down.load.dt(dt = rbind(get.table(reactive.run()$m),
                                                               reactive.run()$c_median_m), name0 = "_male")
  
}
# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
