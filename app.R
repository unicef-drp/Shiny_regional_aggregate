#
# Shiny app to aggregate selected countries in each region
# Please click "Run App" or type shiny::runApp() to run. Don't run all the code (slow).
# On ShinyApp.io: https://u5met2017.shinyapps.io/Shiny_regional_aggregate/
# Yang Liu, 
# Oct.11, 2019

# The only script to update:
source("update_me_every_year.R")

# Libraries
check.and.install.pkgs <- function(pkgs){
  search_package <- sapply(pkgs, find.package, quiet = TRUE) # return a string or character(0)
  new.packages <- pkgs[sapply(search_package, function(x)length(x)==0)]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))
}
check.and.install.pkgs(c("shiny", "shinyWidgets", "shinyjs", "leaflet",
                         "maps", "maptools", "rgeos",
                         "DT","data.table", "dplyr", "here", 
                         "ggplot2", "plotly", "readxl"))

# source code
invisible(sapply(list.files(here::here("R"), full.names = TRUE), source))

# update packages if certain visions are required
invisible(sapply(c("shiny", "DT", "data.table"), update.package.version))

suppressPackageStartupMessages({
  # it seems listing the libraries is necessary if want to publish on shinyapps.io
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
  library("readxl")
})

# Sanitizing error messages
options(shiny.sanitize.errors = TRUE)

# Language -------------------------------------------------------------------
note_header <- p("This ShinyApp produces regional aggregates of child mortality
estimates based on individually selected countries. The ", 
a("UN IGME", href = "https://childmortality.org", target = "_blank"),
"\'s latest estimates of neonatal, infant and under-five mortality are used. 
Country data will also be included in the downloaded dataset from the \"Tables and Data Download\" panel
after running the aggregates.")

note_input <- "Please add countries by clicking the list, or uploading a file of selected ISOs."
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
dc <- country.info <- fread(here::here("input", "country.info.CME.csv"))

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

# median results by country 
# (results for each country will be included in the download as well)
c_median_all <- read.country.summary(dir_dt_cs = file.path(dir_median_total, file_name_total), year_wanted = year_started:2030)
c_median_f <- read.country.summary(dir_dt_cs = file.path(dir_median_female, file_name_female), year_wanted = year_started:2030)
c_median_m <- read.country.summary(dir_dt_cs = file.path(dir_median_male, file_name_male), year_wanted = year_started:2030)

year_ended <- floor(max(c_median_all$Year))
year.lastestimatepublished <- year_ended + 0.5  # e.g. 2019.5 for IGME 2020

# for the reset function
jscode <- "shinyjs.refresh = function() { location.reload(); }"

# check dir
if(!grepl("female", dir_median_female))stop("Check dir_median_female: ", dir_median_female)
if(!grepl("male", dir_median_male))stop("Check dir_median_male: ", dir_median_male)
invisible(lapply(c(dir_median_total, dir_median_female, dir_median_male), check.dir.exists))

# UI: side panel ----------------------------------------------------------

ui = fluidPage(
  # head panel with pic
  get.headerPanel(),
  
  # side panel
  sidebarPanel(
    # for reset 
    useShinyjs(),
    extendShinyjs(text = jscode, functions = "refresh"), 
    
    titlePanel("Aggregate Selected Countries"), # title
    br(),
    wellPanel(p(note_header), style = "border: 0px"), 
    
    # The country selection part
    wellPanel(
    # country_input_select
    helpText(note_input),
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
    checkboxInput(inputId = "input_by_region", label = ("Group countries by the five continents"), value = FALSE),
    
    # upload ISO
    fileInput('ISO_input', label = p("(Optional) Upload a csv file of selected", a("ISO3 country code", href = "https://unstats.un.org/unsd/tradekb/knowledgebase/country-code", target = "_blank")),
              placeholder = "Column name shall contain \"ISO\"", accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
              ),
    style = "border: 0px"
    ),
    
    # rename the group
    # p("To name the group: "),
    wellPanel(
    textAreaInput(inputId = "adhoc_name", label = "(Optional) Name the selected group of countries",
                  value = adhoc_name, rows  = 1,
                  placeholder = "Default name is \"Selected Countries\" if leave as blank"),
    # run_gender
    checkboxInput(inputId = "run_gender", label = strong("Run sex-specific results?"), value = FALSE),
    # click_run
    actionButton("click_run",  strong("Run the Aggregates"), width = '200px'), 
    br(),br(), 
    actionButton("click_reset",  ("Reset App"), width = '200px')
    )),
  

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
        # include panel_title1, selected_countries_click_run, and plotly output
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
    get.about.panel(update_string = update_string0)
    ) #tabsetPanel
  ), #mainpanel
theme = "bootstrap.css"
) #ui fluidpage

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
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
        dt_iso <- fread(file_path, header = TRUE)
      } else if (file_type %in% c("xlsx", "xls")) {
        dt_iso <- setDT(readxl::read_excel(file_path))
      } else {
        showModal(modalDialog(title == "Currently accept csv, xlsx, or xls files.", "Please re-upload."))
        dt_iso <- NULL
      }

      if (!is.null(dt_iso)&length(colnames(dt_iso))!=0){
        message(paste("Read in", file_type ,"file"))
        # find the column names that contain "ISO", then used the cloest if have to guess
        ISO_columns <- colnames(dt_iso)[grepl("ISO", toupper(colnames(dt_iso)))] 
        ISO_column_name <- ISO_columns[which.min(adist("ISO", toupper(ISO_columns)))] 
        if(length(ISO_column_name) == 0){
          ISO_column_name <- colnames(dt_iso)[which.min(adist("ISO", toupper(colnames(dt_iso))))] 
          message("Couldn't match a column name that looks like \"ISO\". Please check if the selected column is right.")
          message(paste("The column assumed is", ISO_column_name))
        }
        ISO_selected <- dt_iso[[ISO_column_name]]
        ISO_selected <- ISO_selected[ISO_selected%in%ISOs]
        if (ISO_column_name%in%ISOs){
          message("The column name is included as an ISO.")
          ISO_selected <- c(ISO_column_name, ISO_selected)
        }
        ISO_selected <- unique(ISO_selected)
        
        showModal(modalDialog(title = paste("Uploaded and recognized ISOs in column", ISO_column_name ,"are:"), 
                              length(ISO_selected), " ISOs: ",  HTML("<br>"), 
                                     paste0(ISO_selected, collapse = ", "), ".",
                              HTML("<br><br>You may click anywhere to dismiss this message"), easyClose = TRUE
                              ))
        
        if(length(ISO_selected)!=0){
          updatePickerInput(session, inputId = "country_input_select", 
                            choices = if(input$input_by_region) input_country_list else countries,
                            selected = dc[ISO3Code%in%ISO_selected, OfficialName])
        }
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
    cs <- input$country_input_select
    paste(length(cs), if(length(cs)==1) "Country:" else "Countries:", 
          paste(sort(cs), collapse = ", "))
  })
  
  # reactive: record countries for the click_run, and render Text
  reactive.selected.countries <- eventReactive(input$click_run, {
    input$country_input_select
  })
  output$selected_countries_click_run <- renderText({
    cs <- reactive.selected.countries()
    paste(length(cs), if(length(cs)==1) "Country:" else "Countries:", 
          paste(sort(cs), collapse = ", "))
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
      downloadButton("download_table_all", "Download"),
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
    cs <- input$country_input_select
    if(!input$run_gender){
      # showModal will show that the scripe is running, and removed when scripts are done 
      paste()
      
      showModal(modalDialog(title = paste("Running aggregate for", 
                                          length(cs), 
                                          if(length(cs)==1) "country:" else "countries:", 
                                          paste(sort(cs), collapse = ", ")),
                            HTML("<br> It takes about 10 - 20 seconds."), footer=NULL))
    } else {
      showModal(modalDialog(title = paste("Running sex-specific aggregate for", 
                                          length(cs), 
                                          if(length(cs)==1) "country:" else "countries:", 
                                          paste(sort(cs), collapse = ", ")), 
                            HTML("<br> It takes about 20 - 30 seconds."), footer=NULL))
    }
    # where we run the aggregates:
    run.outputaggregates(year.lastestimatepublished)
    # source("6outputaggregates.R")
    if(input$run_gender){
      run.outputaggregates.gender(year.lastestimatepublished)
      adjust.death()
      # source("6outputaggregates_gender.R")
    }
    
    removeModal()
    message("Time spent is ", round(Sys.time() - time0, 1), " seconds.")
    
    # Add the use-defined `adhoc_name`:
    change.dt.name <- function(dt){
      setnames(dt, gsub("\\.", " ", colnames(dt))) # revise colnames from adjusted sex-specific output
      setnames(dt, gsub("Under five", "Under-five", colnames(dt)))
      dt[Region=="Adhoc", Region:= if(is.null(input$adhoc_name)|input$adhoc_name=="") adhoc_name else input$adhoc_name]
      return(dt)
    }
    
    if(!input$run_gender){
      # only total sex
      return(list(
                  all = change.dt.name(fread(file.path(dir_median_total, "Rates & Deaths_AdhocCountries.csv"))),
                  c_median_all = c_median_all[Region%in%input$country_input_select,],
                  f = NULL,
                  m = NULL,
                  c_median_f = NULL,
                  c_median_m = NULL
                  ))
        } else {
          # incl. sex-specific
          return(
              list(
                  all = change.dt.name(fread(file.path(dir_median_total, "Rates & Deaths_AdhocCountries.csv"))),
                  f = change.dt.name(fread(file.path(dir_median_female, "Rates & Deaths(ADJUSTED)_female_AdhocCountries.csv"))),
                  m = change.dt.name(fread(file.path(dir_median_male, "Rates & Deaths(ADJUSTED)_male_AdhocCountries.csv"))),
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
    p <- ggplot(dt_long, aes(x = Year, y = Rate, color = Region, type = Region)) +
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
  

  # Figure 2. Death by year -------------------------------------------------
  plot.death <- function(dt){
    dt <- dt[Year>=year_started,]
    dt_long <- data.table::melt(dt, measure.vars = grep("deaths", colnames(dt), value = TRUE), 
                                  value.name = "Deaths", variable.name = "type")
    levels(dt_long$type) <- revise.name(levels(dt_long$type), new_list = new_varname_list)
    # dt_long[, Death_Number_1K := round(Deaths/1E3)]
    p <- ggplot(dt_long[!is.na(Deaths),], aes(x = Year, y = Deaths, color = Region)) +
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
    
    p <- ggplot(dt_long[!is.na(rate), Rate := round(rate,2)], aes(x = Year, y = Rate, color = gender)) +
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
  output$results_table <- DT::renderDT({
    if (!is.null(reactive.run())){
    dt <- reactive.run()$all
    dt[, Sex:= "Total"]
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate", "Neonatal Mortality Rate"), digits = 2)
    }
  })

  output$results_table_m <- DT::renderDT({
    if (!is.null(reactive.run()$m)){
    dt <- reactive.run()$m
    dt[, Sex:= "Male"]
    
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate"), digits = 2)
    }
  })

  output$results_table_f <- DT::renderDT({
    if (!is.null(reactive.run()$f)){
    dt <- reactive.run()$f
    dt[, Sex:= "Female"]
    
    DT::formatRound(
      DT::datatable( get.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate"), digits = 2)
    }
  })


  # Make results available for downloading, this is a function to make `downloadHandler` 
   down.load.dt <- function(dt, name0){
    downloadHandler(
      filename = function() {
        paste0("Results",name0,"_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
      },
      content = function(file) {
        write.csv(dt, file, row.names = FALSE, na = "")
    }
  )}

  output$download_table_all <- down.load.dt(dt = 
                                      rbindlist(list(get.table(reactive.run()$all), 
                                                     reactive.run()$c_median_all,
                                                     get.table(reactive.run()$f),
                                                     reactive.run()$c_median_f,
                                                     get.table(reactive.run()$m),
                                                     reactive.run()$c_median_m), 
                                                fill = TRUE),
                                            name0 = "_total")
  output$download_table_f <- down.load.dt(dt = rbind(get.table(reactive.run()$f),
                                                     reactive.run()$c_median_f), 
                                          name0 = "_female")
  output$download_table_m <- down.load.dt(dt = rbind(get.table(reactive.run()$m),
                                                     reactive.run()$c_median_m), 
                                          name0 = "_male")
  
}
# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
