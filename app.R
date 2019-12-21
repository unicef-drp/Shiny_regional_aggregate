#
# Shiny app to aggregate selected countries in each region
# Please click "Run App" or type shiny::runApp() to run. Don't run all the code (slow).
# On ShinyApp.io: https://u5met2017.shinyapps.io/Shiny_regional_aggregate/
# Yang Liu, 
# Oct.11, 2019
#
# check packages
pkg_vt <- rownames(installed.packages())
for (pck in c("shiny", "shinyWidgets", "shinyjs", "leaflet",
              "maps", "maptools", "rgeos",
              "DT","data.table", "dplyr", "here", 
              "ggplot2", "plotly", "readr", "readxl")){
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
  library("plotly")
  library("readr")
})

# Sanitizing error messages
options(shiny.sanitize.errors = TRUE)

# Language -------------------------------------------------------------------
note_header <- p("This ShinyApp produces regional aggregates of child mortality
estimates based on individually selected countries.", 
a("UN IGME", href = "https://childmortality.org", target = "_blank"),
"\'s latest estimates of neonatal, infant and under-five mortality are used. 
Country data will also be included in the downloaded dataset from the \"Tables and Data Download\" panel
after running the aggregates.")

note_input <- "Please add countries by clicking the list, or typing names and press enter."
default_select <- "Afghanistan"

panel_title1   <- "Results of selected regional aggregates for"
panel_title1.2 <- "Tables of selected regional aggregates"
panel_note1.2 <- "Regional, world, and country data are available for download."
panel_title2 <- "Sex-specific results for selected regional aggregates"

note_map <- "Note: This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."

adhoc_name <- "Selected Countries"

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

# Country names and ISOs
countries <- sort(dc[,unique(OfficialName)])
ISOs <- sort(dc[,unique(ISO3Code)])

# Get world map with modified country names 
world_map <- get.world.map()

# Source the About panel, which is a function to be called in ui
tabPanel.about <- source(here::here("R_shiny", "about.R"))$value
  
# Show data from: 
year_started <- 1985
year_ended <- 2018

# median results for all countries
file_all <- here::here("Aggregate results (median) 2019-08-15", "Rates & Deaths_Country Summary.csv")
file_f <- here::here("Aggregate results (median) 2019-08-15 (female)", "Rates & Deaths_Country Summary.csv")
file_m <- here::here("Aggregate results (median) 2019-08-15 (male)", "Rates & Deaths_Country Summary.csv")
c_median_all <- get.data.all(file = file_all, year_started = year_started, year_ended = year_ended)
c_median_f <- get.data.all(file = file_f, gender0 = TRUE, year_started = year_started, year_ended = year_ended)
c_median_m <- get.data.all(file = file_m, gender0 = TRUE, year_started = year_started, year_ended = year_ended)

# for the reset function
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
    br(),
    p(note_header), 
    
    # country_input_select
    p(note_input),
    checkboxInput(inputId = "input_by_region", label = ("Group countries by the five continents"), value = FALSE),
    shinyWidgets::pickerInput(
      inputId = "country_input_select", label = "Please Select Countries", 
      choices = countries, 
      selected = default_select, 
      multiple = TRUE,  # allow multiple selection
      options = list(
        title = "Please select countries",
        `actions-box` = TRUE, 
        size = 10
      )),
    
    # upload ISO
    fileInput('ISO_input', label = "(Optional) Upload a csv file of selected ISO3 Codes",
              placeholder = "", accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
              ),
    br(),
    
    # rename the group
    # p("To name the group: "),
    textAreaInput(inputId = "adhoc_name", label = "(Optional) Name the selected group of countries",
                  value = adhoc_name, rows  = 1,
                  placeholder = "Default name is \"Selected Countries\" if leave as blank"),
    # run_gender
    checkboxInput(inputId = "run_gender", label = strong("Run sex-specific results?"), value = FALSE),
    # click_run
    actionButton("click_run",  strong("Run the Aggregates"), width = '200px'), 
    br(),br(), 
    actionButton("click_reset",  ("Reset App"), width = '200px')
  ),
  

  # UI: main panel ----------------------------------------------------------
  mainPanel(
  # plots (and map)
    tabsetPanel(
      tabPanel("Plots",
               
               # print results 
               h4("The list of selected countries:"),
               (textOutput("text_selected_countries")),
               br(),

        # Plot of Aggregated Results
        uiOutput("panel_plot_rate"),
        
        # Plot of Sex-specific Aggregated Results
        uiOutput("panel_plot_rate_gender"),
        # end conditionalPanel
        
        br(),br(),
        h4("Map"),
        leafletOutput(outputId = "mymap"),
        h6(note_map)
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
  # Reset session 
  observe({
    if (input$click_reset) {
      js$refresh();
    }
  })  
  
  # renderUI: side panel ---------------------------------------------------
  # Update choices in country_input_select to be listed by region
  observeEvent(input$input_by_region, {
    updatePickerInput(session, inputId = "country_input_select", 
                      choices = if(input$input_by_region) input_country_list else countries,
                      selected = input$country_input_select)
  })
  
  # Read self-uploaded ISO file (19/12/21)
  observeEvent(input$ISO_input, {
    file_type <- tolower(tools::file_ext(input$ISO_input$datapath))
    file_path <- input$ISO_input$datapath
    tryCatch({
      if (file_type == "csv"){
        dt_iso <- fread(file_path)
      } else if (file_type %in% c("xlsx", "xls")) {
        dt_iso <- setDT(readxl::read_excel(file_path))
      } else {
        showModal(modalDialog(title == "Currently accept csv, xlsx, xls file.", "Please re-upload."))
        dt_iso <- NULL
      }

      if (!is.null(dt_iso)&length(colnames(dt_iso))!=0){
        message(paste("Read in", file_type ,"file"))
        # find the column name cloest to "ISO", use the 1st column if not found
        ISO_column_name <- colnames(dt_iso)[grep("ISO", toupper(colnames(dt_iso)))] 
        if(length(ISO_column_name) == 0){
          ISO_column_name <- colnames(dt_iso)[1]
          message("Couldn't match a column name that looks like \"ISO\". The first column is used. Please check if it is right.")
          message(paste("The name of the column is", ISO_column_name))
        }
        if(length(ISO_column_name) > 1){
          ISO_column_name <- ISO_column_name[1]
          message("There are multiple columns that look like \"ISO\". The first column is used. Please check if it is right.")
          message(paste("The name of the column is", ISO_column_name))
        }
        
        ISO_selected <- dt_iso[[ISO_column_name]]
        ISO_selected <- ISO_selected[ISO_selected%in%ISOs]
        if (ISO_column_name%in%ISOs){
          message("I have included the column name, as an ISO is on the first row.")
          ISO_selected <- c(ISO_column_name, ISO_selected)
        }
        
        showModal(modalDialog(title = paste("Uploaded and recognized ISOs in column", ISO_column_name ,"are:"), 
                              length(ISO_selected), " ISOs: ",  HTML("<br>"), 
                                     paste0(ISO_selected, collapse = ", "), ".",
                              HTML("<br><br>You may click anywhere to dismiss this message"), easyClose = TRUE
                              ))
        
        updatePickerInput(session, inputId = "country_input_select", 
                          choices = if(input$input_by_region) input_country_list else countries,
                          selected = dc[ISO3Code%in%ISO_selected, CountryName])
      }
      
    }, error = function(e){
      message("The uploaded file is not correct.")
      },
    warning = function(e){
      message("The uploaded file is not correct.")
      }
    )
  })
  
  # Reset selection
  observeEvent(input$click_reset,{
    updatePickerInput(session, inputId = "country_input", selected = default_select)
    updateCheckboxInput(session, inputId = "run_gender", value = FALSE)
  })

  # renderUI: main panel ----------------------------------------------------------------
  
  # print the currently selected countries dynamically
  output$text_selected_countries  = renderText({
    paste(sort(input$country_input_select), collapse = ", ")
  })
  
  # reactive: print and record countries for the click_run
  reactive.selected.countries <- eventReactive(input$click_run, {
    input$country_input_select
  })

  output$selected_countries_click_run <- renderText({
    paste(sort(reactive.selected.countries()), collapse = ", ")
  })
  
  
  # renderUI: map the selected countries (if available in this map)
  output$mymap = renderLeaflet({
    validate(need(input$country_input_select, "Please select countries."))
    # plot on pre-loaded world_map from get.world.map()
    leaflet::leaflet(quakes, options = leafletOptions(minZoom = 2, maxZoom =3)) %>% 
      addTiles("http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png") %>%
      leaflet::addPolygons(data = subset(world_map, country %in% input$country_input_select), weight = 2)
  })
  
  # renderUI: plots
  output$panel_plot_rate <- renderUI({
    if (is.null(reactive.run())){
      return()
    }
    fluidRow(
      h4(strong(panel_title1)),
      h5(strong(textOutput(outputId = "selected_countries_click_run"))),
      br(),
      # show_world
      checkboxInput(inputId = "show_world", label = "Show results for the world in plots", value = FALSE),
      plotlyOutput("plot_rate"),
      br(),
      plotlyOutput("plot_death"),
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
          plotlyOutput("plot_rate_gender", height = "800px", width = "80%")
        } else {
          plotlyOutput("plot_rate_gender", width = "80%")
        }}
    )
  })
  
  # renderUI:tables
  output$panel_results_table <- renderUI({
    if (is.null(reactive.run())){
      return()
    }
    fluidRow(
      h4(panel_title1.2),
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
  # to apply adhoc_name
  reactive.adhoc.name <- eventReactive(input$click_run, {
    if (is.null(input$adhoc_name)|input$adhoc_name=="") adhoc_name else input$adhoc_name
  })
  
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
    change.dt.name <- function(dt){
      dt[Region=="Adhoc", Region:= if(is.null(input$adhoc_name)|input$adhoc_name=="") adhoc_name else input$adhoc_name]
      return(dt)
    }
    
    if(!input$run_gender){
          return(list(
                    all = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15", "Rates & Deaths_AdhocCountries.csv"))),
                    c_median_all = c_median_all[Region%in%input$country_input_select,]
                    ))
        } else {        
          return(
              list(
                  all = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15", "Rates & Deaths_AdhocCountries.csv"))),
                  f = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15 (female)", "Rates & Deaths_AdhocCountries.csv"))),
                  m = change.dt.name(fread(here::here("Aggregate results (median) 2019-08-15 (male)", "Rates & Deaths_AdhocCountries.csv"))),
                  c_median_all = c_median_all[Region%in%input$country_input_select,],
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
    if(!input$show_world) dt <- dt[Region==reactive.adhoc.name(),]
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
    if(!input$show_world) dt <- dt[Region==reactive.adhoc.name(),]
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
    if(!input$show_world) dt_long <- dt_long[Region==reactive.adhoc.name(),]
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
