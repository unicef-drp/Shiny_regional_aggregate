# Shiny app to aggregate selected countries
# Please click "Run App" or type shiny::runApp() to run. 
# Deployed on Shinyapps.io: https://u5met2017.shinyapps.io/Shiny_regional_aggregate/
# Yang Liu, 
# Oct.11, 2019 - 2022

# Open this script to go through the updating process every year
# No need to update anything in this `app.R`
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
                         "ggplot2", "scales", "plotly", "readxl"))

# source code
invisible(sapply(list.files(here::here("R"), full.names = TRUE, recursive = TRUE), source))

# update packages if certain visions are required
invisible(sapply(c("shiny", "DT", "data.table"), update.package.version))

suppressPackageStartupMessages({
  # it seems listing the libraries is still necessary if want to publish the app
  # on shinyapps.io
  library("here")
  library("shiny")
  library("shinyWidgets")
  library("shinyjs")  # for reset app
  library("leaflet")  # for openstreetmap
  library("maps")     # provide shape files for selected countries
  library("maptools") # modify shape files
  library("rgeos")
  library("DT")       # for shiny table 
  library("data.table") 
  library("dplyr")    
  library("ggplot2")
  library("scales")
  library("plotly")
  library("readxl")
})

# Sanitizing error messages
options(shiny.sanitize.errors = TRUE)

# Texts -------------------------------------------------------------------
note_header <- p("This ShinyApp produces regional aggregates of child mortality
estimates based on individually selected countries. The ", 
a("UN IGME", href = "https://childmortality.org", target = "_blank"),
"\'s latest estimates of neonatal, infant and under-five mortality are used. 
Country data will also be included in the downloaded dataset from the \"Tables and Data Download\" panel
after running the aggregates.")

note_input     <- "Please select countries from the drop-down list, or by uploading a file of countries' ISO-alpha3 codes."
default_select <- "Afghanistan"

panel_title1.1 <- "Results of selected regional aggregates for"
panel_title1.2 <- "Sex-specific results for selected regional aggregates"
panel_title1.3 <- "Older children and adolescents results for selected regional aggregates"
panel_title2.1 <- "Table of selected regional aggregates"
panel_title2.2 <- "Sex-specific results for selected regional aggregates"
panel_title2.3 <- "Older children and adolescents results for selected regional aggregates"
panel_note1    <- "Regional, world, and country data are available for download:"
panel_note2    <- "Regional, world, and country data (including sex-specific results) are available for download:"

note_map <- "Note: This map is stylized and not to scale and does not reflect 
a position by UNICEF on the legal status of any country or territory or the delimitation 
of any country or territory or the delimitation of any frontiers."

adhoc_name <- "Selected Countries"

# Dataset and Parameters -----------------------------------------------------------------


# dc: country.info.CME dataset
dc <- fread(here::here("input/country.info.CME.csv"))
dc.5.14 <- fread(here::here("input/country.info.CME.5_14.csv"))
dc.15.24 <- fread(here::here("input/country.info.CME.15_24.csv"))

# Define region
dc[, UNICEF_region:= ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
dc[, SDG_region:= ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
dc$SDG_region <- dplyr::recode(dc$SDG_region, !!!SDG_name_list)
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

# Get world map with modified country names, a `sp` object supplied to leaflet later
world_map <- get.world.map()

# median results for selected countries will be included in the downloaded data
# but won't be shown in the app. Rates are not rounded in the downloaded data
c_median_total <- read.country.summary(dir_dt_cs = file.path(dir_median_total, file_name_total), year_wanted = year_started:2030)
c_median_f     <- read.country.summary(dir_dt_cs = file.path(dir_median_female, file_name_female), year_wanted = year_started:2030)
c_median_m     <- read.country.summary(dir_dt_cs = file.path(dir_median_male, file_name_male), year_wanted = year_started:2030)
c_median_total_5_14  <- read.country.summary(dir_dt_cs = file.path(dir_median_total_5_14, file_name_total), year_wanted = year_started:2030)
c_median_total_15_24 <- read.country.summary(dir_dt_cs = file.path(dir_median_total_15_24, file_name_total), year_wanted = year_started:2030)
c_median_total_older <- merge(recode_ind_5_14(c_median_total_5_14), 
                              recode_ind_15_24(c_median_total_15_24))
c_median_total_older <- calculate.10q10(c_median_total_older)
col_order_older_children <- copy(colnames(c_median_total_older)) # colnames containing "X"
setnames(c_median_total_older, dplyr::recode(col_order_older_children, !!!new_varname_list))
col_order_older_children_all_rate <- colnames(c_median_total_older)[grepl("Mortality rate", colnames(c_median_total_older))]
year_ended <- floor(max(c_median_total$Year))
year.lastestimatepublished <- year_ended + 0.5  # e.g. 2019.5 for IGME 2020

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
    # if `input_by_region` update the choices in `country_input_select`
    checkboxInput(inputId = "input_by_region", label = ("Group countries by the five continents"), value = FALSE),
    
    # upload ISO
    fileInput('ISO_input', label = p("(Optional) Upload selected", 
                                     a("ISO3 country code", href = "https://unstats.un.org/unsd/methodology/m49/", target = "_blank"),
                                     a("(download an example)", href = "Upload_ISO3Code_example_single_region.xlsx", target = "_blank")
                                     ),
              placeholder = "Column name shall contain \"ISO\"", 
              accept = c(".csv", ".xlsx")
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
    checkboxInput(inputId = "run_gender", label = strong("Run sex-specific results?"),
                  value = FALSE),
    # run_older_total
    checkboxInput(inputId = "run_older_total", label = strong("Run older children and adolescents results?"),
                  value = FALSE),
    # click_run
    actionButton("click_run",  strong("Run the Aggregates"), width = '200px'), 
    br(),br(), 
    actionButton("click_reset",  ("Reset App"), width = '200px')
    )),
  

  # UI: main panel ----------------------------------------------------------
  mainPanel(
  # plots (and map)
    tabsetPanel(
      tabPanel("Plot",
               
               # print results 
               h4("The selected countries:"),
               (textOutput("text_selected_countries")),
               br(),

        # Plot of Aggregated Results
        # include panel_title1, selected_countries_click_run, and plotly output
        uiOutput("panel_plot_rate"),
        
        # Plot of Sex-specific Aggregated Results
        uiOutput("panel_plot_rate_gender"),
        
        # Plot of older children
        uiOutput("panel_plot_older_children"),
        # end conditionalPanel
        
        br(),br(),
        h4("Map"),
        leafletOutput(outputId = "mymap"),
        h6(note_map)
      ),
  # tables
      tabPanel("Table and Data Download",
        uiOutput("panel_results_table"),
        br(),br(),
        uiOutput("panel_results_table_gender"),
        br(),br(),
        uiOutput("panel_results_table_older_total") 
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
      refresh();
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

      if (!is.null(dt_iso) & length(colnames(dt_iso))!=0){
        message(paste("Read in", file_type ,"file"))
        # find the column names that contain "ISO", then used the closest if have to guess
        ISO_columns <- colnames(dt_iso)[grepl("ISO", toupper(colnames(dt_iso)))] 
        ISO_column_name <- ISO_columns[which.min(utils::adist("ISO", toupper(ISO_columns)))] 
        if(length(ISO_column_name) == 0){
          ISO_column_name <- colnames(dt_iso)[which.min(utils::adist("ISO", toupper(colnames(dt_iso))))] 
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
        
        # use region name if available
        Region_name <- unique(dt_iso$Region)[1]
        
        if(length(Region_name)!=0){
          updateTextAreaInput(session, inputId = "adhoc_name", value = Region_name)
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
    updateCheckboxInput(session, inputId = "run_older_total", value = FALSE)
  })

  # renderUI: main panel ----------------------------------------------------------------
  
  # print the currently selected countries dynamically
  output$text_selected_countries  = renderText({
    cs <- input$country_input_select
    paste(length(cs), if(length(cs)==1) "Country:" else "Countries:", 
          paste(sort(cs), collapse = ", "))
  })
  
  # reactive: record countries after the click_run, and render Text because the
  # `text_selected_countries` could change, but after click_run, there will be a
  # fixed selected group of countries
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
      h4(strong(panel_title1.1)),
      h5(strong(textOutput(outputId = "selected_countries_click_run"))),
      br(),
      # download definition
      # show_world
      checkboxInput(inputId = "show_world", label = "Show results for the world in plots", value = FALSE),
      h5(a("Download definition of indicators", href = "Indicator definition and unit.xlsx", target = "_blank")),
      plotly::plotlyOutput("plot_rate"),
      br(),
      plotly::plotlyOutput("plot_death"),
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
      h4(strong(panel_title1.2)),
      

      if (length(input$show_world != 0)){
        if (input$show_world) {
          plotly::plotlyOutput("plot_rate_gender", height = "800px")
        } else {
          plotly::plotlyOutput("plot_rate_gender")
        }}
    )
  })
  
  # renderUI: plots by sex
  output$panel_plot_older_children <- renderUI({
    if (is.null(reactive.run()$both_5_24)){
      return()
    }
    if(!input$run_older_total) return()
    fluidRow(
      h4(strong(panel_title1.3)),
        plotly::plotlyOutput("plot_rate_older1"),
        br(),
        plotly::plotlyOutput("plot_rate_older2"),
        br(),
        plotly::plotlyOutput("plot_rate_older3"),
        br(), br()
    )
  })
  
  # renderUI: tables
  output$panel_results_table <- renderUI({
    if (is.null(reactive.run())){
      return()
    }
    fluidRow(
      h4(strong(panel_title2.1)),
      if(input$run_gender) p(panel_note2) else p(panel_note1),
      # optional to download
      downloadButton("download_table_all", "Download"),
      br(),br(),
      DT::dataTableOutput("results_table", width = "80%")
    )
  })
  # renderUI:tables by sex
  output$panel_results_table_gender <- renderUI({
    if (is.null(reactive.run()$m)) return()
    if (!input$run_gender) return()
    fluidRow(            
      h4(strong(panel_title2.2)),
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
  
  output$panel_results_table_older_total <- renderUI({
    if (is.null(reactive.run())) return()
    if (!input$run_older_total) return()
    
    fluidRow(
      h4(strong(panel_title2.3)),
      downloadButton("download_table_older_total", "Download"),
      br(),br(),
      DT::dataTableOutput("results_table_older_total", width = "80%")
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
                            footer = "You may click anywhere to dismiss this message", 
                            easyClose = TRUE))
    } else {
    # run aggregates for the selected countries
    dc[,AdhocCountries:=""]
    dc[OfficialName %in% input$country_input_select, AdhocCountries:="Adhoc"]
    write.csv(dc, file = here::here("input", "country.info.CME_adhoc.csv"))

    if(input$run_older_total){
      dc.5.14[,AdhocCountries:=""]
      dc.5.14[OfficialName %in% input$country_input_select, AdhocCountries:="Adhoc"]
      write.csv(dc.5.14, file = here::here("input", "country.info.CME.5_14_adhoc.csv"))
      dc.15.24[,AdhocCountries:=""]
      dc.15.24[OfficialName %in% input$country_input_select, AdhocCountries:="Adhoc"]
      write.csv(dc.15.24, file = here::here("input", "country.info.CME.15_24_adhoc.csv"))
    }
    
    cs <- input$country_input_select # country list
    time0 <- Sys.time()
    
    # showModal
    # showModal will show that the scripe is running, and removed when scripts are done 
    if(input$run_gender){
      if(input$run_older_total){
        # sex-specific
        showModal(modalDialog(title = paste("Running aggregate for sex-specific under-five and older children for ", 
                                            length(cs), 
                                            if(length(cs)==1) "country:" else "countries:", 
                                            paste(sort(cs), collapse = ", ")), 
                              HTML("<br> It takes about 30 - 40 seconds."), 
                              footer=NULL))
      } else {
        # sex-specific
        showModal(modalDialog(title = paste("Running sex-specific under-five aggregate for ", 
                                            length(cs), 
                                            if(length(cs)==1) "country:" else "countries:", 
                                            paste(sort(cs), collapse = ", ")), 
                              HTML("<br> It takes about 20 - 30 seconds."), 
                              footer=NULL))
      }
     
    } else if (input$run_older_total) {
      # if no sex-specific, but incl older children
      showModal(modalDialog(title = paste("Running aggregate for under-five and older children for ", 
                                          length(cs), 
                                          if(length(cs)==1) "country:" else "countries:", 
                                          paste(sort(cs), collapse = ", ")), 
                            HTML("<br> It takes about 20 - 30 seconds."), 
                            footer=NULL))
    } else {
      showModal(modalDialog(title = paste("Running under-five aggregate for ", 
                                          length(cs), 
                                          if(length(cs)==1) "country:" else "countries:", 
                                          paste(sort(cs), collapse = ", ")), 
                            HTML("<br> It takes about 10 - 20 seconds."), 
                            footer=NULL))
    }
    
    # where we run the aggregates:
    run.outputaggregates(year.lastestimatepublished) # (always run the under-five total)
    
    if(input$run_gender){
      run.outputaggregates.gender(year.lastestimatepublished)
      adjust.death()
    }
    if(input$run_older_total){
      run.outputaggregates.5.24(year.lastestimatepublished)
    }
    
    removeModal()
    message("Time spent is ", round(Sys.time() - time0, 1), " seconds.")
    
    # Modify colnames and add the user-defined `adhoc_name`:
    change.adhoc.name <- function(dt){
      setnames(dt, gsub("\\.", " ", colnames(dt))) # revise colnames from adjusted sex-specific output
      setnames(dt, gsub("Under five", "Under-five", colnames(dt)))
      dt[Region=="Adhoc", Region:= if(is.null(input$adhoc_name)|input$adhoc_name=="") adhoc_name else input$adhoc_name]
      return(dt)
    }
    
    both_5_24 <- NULL
    if(input$run_older_total){
      dt5_14  <- change.adhoc.name(fread(file.path(dir_median_total_5_14,  "Rates & Deaths_AdhocCountries.csv")))
      dt15_24 <- change.adhoc.name(fread(file.path(dir_median_total_15_24, "Rates & Deaths_AdhocCountries.csv")))
      # dt5_14  <- (fread(file.path(dir_median_total_5_14,  "Rates & Deaths_AdhocCountries.csv")))
      # dt15_24 <- (fread(file.path(dir_median_total_15_24, "Rates & Deaths_AdhocCountries.csv")))
      setnames(dt5_14, gsub(" median", "", colnames(dt5_14)))
      setnames(dt15_24, gsub(" median", "", colnames(dt15_24)))
      dt5_14  <- recode_ind_5_14(dt5_14)
      dt15_24 <- recode_ind_15_24(dt15_24)
      setkey(dt5_14, Region, Year)
      setkey(dt15_24, Region, Year)
      dt15_24 <- dt15_24[dt5_14]
      dt15_24 <- calculate.10q10(dt15_24)[, Sex := "Both"]
      both_5_24 <- dt15_24[Year >= 1990,..col_order_older_children]
    }
    
    # output ----
    if(input$run_gender){
      output_list <- list(
        both = change.adhoc.name(fread(file.path(dir_median_total,  "Rates & Deaths_AdhocCountries.csv"))),
        f    = change.adhoc.name(fread(file.path(dir_median_female, "Rates & Deaths(ADJUSTED)_female_AdhocCountries.csv"))),
        m    = change.adhoc.name(fread(file.path(dir_median_male,   "Rates & Deaths(ADJUSTED)_male_AdhocCountries.csv"))),
        both_5_24 = both_5_24, 
        c_median_total = c_median_total[Region%in%input$country_input_select,],
        c_median_f     = c_median_f[Region%in%input$country_input_select,],
        c_median_m     = c_median_m[Region%in%input$country_input_select,],
        c_median_total_older = c_median_total_older[Region%in%input$country_input_select,]
      )
    } else {
      output_list <- list(
        both = change.adhoc.name(fread(file.path(dir_median_total,  "Rates & Deaths_AdhocCountries.csv"))),
        f    = NULL,
        m    = NULL,
        both_5_24 = both_5_24, 
        c_median_total = c_median_total[Region%in%input$country_input_select,],
        c_median_f     = NULL,
        c_median_m     = NULL,
        c_median_total_older = c_median_total_older[Region%in%input$country_input_select,]
      )
    }
    
    # another option is: 
    # if(!input$run_gender){
    #   output_list$f <- NULL
    #   output_list$m <- NULL
    #   output_list$c_median_f <- NULL
    #   output_list$c_median_m <- NULL
    # }
    
    return(output_list)
    
    } # for the `length(input$country_input_select)==0` check
  })
  
  # Figure 1 Rate by Year ---------------------------------------------------
  # some helper functions:
  get.long <- function(dt, vars0){
    dt <- dt[Year>=year_started,]
    dt_long <- data.table::melt(dt, measure.vars = vars0, 
                                value.name = "rate", variable.name = "Indicator")
    return(dt_long[,.(Region, Year, rate, Indicator)])
  }
  
  titlefont0 <- 14 # xlab size
  plot.rate <- function(dt, vars0, title0 = "Deaths per 1,000 live births"){
    dt_long <- get.long(dt, vars0)[!is.na(rate), Rate:=round(rate,2)]
    # note that the final indicator names are matched here: 
    levels(dt_long$Indicator) <- dplyr::recode(levels(dt_long$Indicator), !!!new_varname_list)
    p <- ggplot(dt_long, aes(x = Year, y = Rate, color = Region, type = Region)) +
      geom_line(size = 1) + 
      theme_bw() + 
      labs(y = "", x = "", color = "", type = "") +
      facet_wrap(~ Indicator) +
      scale_color_manual(values = cp_UNICEF_div[(1:uniqueN(dt_long$Region))*2]) +
      scale_x_continuous(breaks = c(1990, 2000, 2010, year_ended))  
    
    return(plotly::ggplotly(p, tooltip = c("Year", "Rate")) %>% 
             # layout(yaxis = list(title = title0), font_size = 9,
             layout(yaxis = list(title = title0, titlefont = list(size = titlefont0)), 
                    legend = list(orientation = "h", x = 0.4, y = -0.1))) # legend.position = "bottom"
  }
  
  output$plot_rate <- plotly::renderPlotly({
    if (is.null(reactive.run())) return()
    dt <- reactive.run()$both
    if(!input$show_world) dt <- dt[Region==reactive.adhoc.name(),]
    plot.rate(dt, vars0 = c("U5MR median", "IMR median", "NMR median"))
  })
  
  output$plot_rate_older1 <- plotly::renderPlotly({
    if (is.null(reactive.run())) return()
    dt <- reactive.run()$both_5_24
    if(!input$show_world) dt <- dt[Region==reactive.adhoc.name(),]
    plot.rate(dt, vars0 = c("X20q5", "X10q10"), title0 = "Deaths per 1,000")
  })
  
  output$plot_rate_older2 <- plotly::renderPlotly({
    if (is.null(reactive.run())) return()
    dt <- reactive.run()$both_5_24
    if(!input$show_world) dt <- dt[Region==reactive.adhoc.name(),]
    plot.rate(dt, vars0 = c("X10q5", "X5q5", "X5q10"), title0 = "Deaths per 1,000")
  })
  
  output$plot_rate_older3 <- plotly::renderPlotly({
    if (is.null(reactive.run())) return()
    dt <- reactive.run()$both_5_24
    if(!input$show_world) dt <- dt[Region==reactive.adhoc.name(),]
    plot.rate(dt, vars0 = c("X10q15", "X5q15", "X5q20"), title0 = "Deaths per 1,000")
  })


  # Figure 2. Death by year -------------------------------------------------
  # separate plot function for death to modify tooltip, legend, etc, function is similar
  plot.death <- function(dt){
    dt <- dt[Year>=year_started,]
    dt_long <- data.table::melt(dt, measure.vars = grep("deaths", colnames(dt), value = TRUE), 
                                  value.name = "Deaths", variable.name = "type")
    levels(dt_long$type) <- dplyr::recode(levels(dt_long$type),!!!new_varname_list)
    # dt_long[, Death_Number_1K := round(Deaths/1E3)]
    p <- ggplot(dt_long[!is.na(Deaths),], aes(x = Year, y = Deaths, color = Region)) +
        geom_line(size = 1) + 
        theme_bw() + 
        # ggtitle("Deaths of U5MR, IMR, and NMR by Year") + 
        labs(y = "", x = "", color = "") +
        facet_wrap(facets= ~ type) + 
        scale_color_manual(values = cp_UNICEF_div[(1:uniqueN(dt_long$Region))*2])+
        scale_x_continuous(breaks = c(1990, 2000, 2010, year_ended)) + 
        scale_y_continuous(labels = scales::label_number(suffix = "k", scale = 1E-3, big.mark = ","))
    
    plotly::ggplotly(p, tooltip = c("Year", "Deaths")) %>% 
                                  layout(yaxis = list(title =  "Number of deaths", 
                                                      titlefont = list(size = titlefont0),
                                                      tickfont = list(size = 10)),
                                         legend = list(orientation = "h", x = 0.4, y = -0.1))
  }

  output$plot_death <- plotly::renderPlotly({
    if (!is.null(reactive.run())){
    dt <- reactive.run()$both
    if(!input$show_world) dt <- dt[Region==reactive.adhoc.name(),]
    plot.death(dt)
    }
  })


  # Figure 3. by Sex by year --------------------------------------------------
  # U5MR and IMR 
  output$plot_rate_gender <- plotly::renderPlotly({
    if (is.null(reactive.run()$m)){
      return()
    }
    vars0 <- c("U5MR median", "IMR median")
    dt_long_m <- get.long(reactive.run()$m, vars0)
    dt_long_f <- get.long(reactive.run()$f, vars0)
    dt_long_m$Sex <- "Male"
    dt_long_f$Sex <- "Female"
    dt_long <- rbind(dt_long_f, dt_long_m)
    dt_long$Region <- as.factor(dt_long$Region)
    dt_long$Sex <- factor(as.factor(dt_long$Sex), levels = c("Male", "Female"))
    # show world or not? 
    if(!input$show_world) dt_long <- dt_long[Region==reactive.adhoc.name(),]
    levels(dt_long$Indicator) <- dplyr::recode(levels(dt_long$Indicator), !!!new_varname_list)
    
    p <- ggplot(dt_long[!is.na(rate), Rate := round(rate,2)], aes(x = Year, y = Rate, color = Sex)) +
      geom_line(size = 1) + 
      theme_bw() + 
      # ggtitle("U5MR and IMR by Sex and Year", subtitle = "Selected Region vs. the World") + 
      labs(y = "", x = "",  color = "") +
      scale_x_continuous(breaks = c(1990, 2000, 2010, year_ended)) +
      facet_wrap(facets = ~ Indicator + Region) +
      # increase margin to increase height of the title strip of `facet_wrap` when passed to `plotly`
      theme(strip.text.x = element_text(margin = margin(.3, 0, .3, 0, "cm")))

    plotly::ggplotly(p, tooltip = c("Year", "Rate"))%>% 
      layout(yaxis = list(title = "Deaths per 1,000 live births", titlefont = list(size = titlefont0)),
             legend = list(orientation = "h", x = 0.4, y = -0.1))  # legend.position = "bottom"
  })
  

  # Tables ------------------------------------------------------------------
  output$results_table <- DT::renderDT({
    if (!is.null(reactive.run())){
    dt <- reactive.run()$both
    DT::formatRound(
      DT::datatable( clean.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate", "Neonatal Mortality Rate"), digits = 2)
    }
  })

  output$results_table_m <- DT::renderDT({
    if (!is.null(reactive.run()$m)){
    dt <- reactive.run()$m
    dt[, Sex:= "Male"]
    
    DT::formatRound(
      DT::datatable( clean.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate"), digits = 2)
    }
  })

  output$results_table_f <- DT::renderDT({
    if (!is.null(reactive.run()$f)){
    dt <- reactive.run()$f
    dt[, Sex:= "Female"]
    
    DT::formatRound(
      DT::datatable( clean.table(dt) ),
      columns = c("Under-five Mortality Rate", "Infant Mortality Rate"), digits = 2)
    }
  })

  output$results_table_older_total <- DT::renderDT({
    if (!is.null(reactive.run()$both_5_24)){
      dt <- reactive.run()$both_5_24
      DT::formatRound(
        DT::datatable( clean.table(dt) ),
        columns = col_order_older_children_all_rate, digits = 2)
    }
  })

  # Download ----------------------------------------------------------------
  # Make results available for downloading
  
  # note: using a function to make `downloadHandler` creates a hidden bug that
  # the reactive inside is only activated once and won't refresh when a new run
  # is clicked. This would cause a bug that the downloaded content won't change
  # after new run --- removed 2022.01
  # 
  #  down.load.dt <- function(dt, name0){
  #   downloadHandler(
  #     filename = function() {
  #       paste0("Results",name0,"_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
  #     },
  #     content = function(file) {
  #       write.csv(dt, file, row.names = FALSE, na = "")
  #   }
  # )}

  # download incl sex.specific data if run sex-specific
  output$download_table_all <- downloadHandler(
    filename = function() {
      paste0("Results_under_five_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- rbindlist(list(clean.table(reactive.run()$both),
                              reactive.run()$c_median_total,
                              clean.table(reactive.run()$f),
                              reactive.run()$c_median_f,
                              clean.table(reactive.run()$m),
                              reactive.run()$c_median_m
                              ), fill = TRUE)
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )
  
  output$download_table_f <- downloadHandler(
    filename = function() {
      paste0("Results_u5_female_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- rbindlist(list(clean.table(reactive.run()$f),
                              reactive.run()$c_median_f
                              ), fill = TRUE)
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )
    
  output$download_table_m <- downloadHandler(
    filename = function() {
      paste0("Results_u5_male_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- rbindlist(list(clean.table(reactive.run()$m),
                              reactive.run()$c_median_m
                              ), fill = TRUE)
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )
  
  
  output$download_table_older_total <- downloadHandler(
    filename = function() {
      paste0("Results_older_children_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- rbindlist(list(clean.table(reactive.run()$both_5_24),
                              reactive.run()$c_median_total_older
      ), fill = TRUE)
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )
}

# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
