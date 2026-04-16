# Shiny app to aggregate selected countries
# Please click "Run App" or type shiny::runApp() to run. 
# Deployed on Shinyapps.io: https://unicef-dapm.shinyapps.io/Shiny_regional_aggregate/
# Yang Liu, 
# Oct.11, 2019 - 2022

# options(
#   rsconnect.http = "libcurl",
#   rsconnect.libcurl.options = list(
#     connecttimeout_ms = 60000,  # 60s to connect
#     timeout_ms = 600000         # 10 min overall
#   )
# )

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
                         "maps", "sf",  
                         "DT","data.table", "dplyr", "here", 
                         "ggplot2", "scales", "plotly", "readxl"))

# source code
invisible(sapply(list.files(here::here("R"), pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE), source))

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
  library("sf")
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
panel_title2.4 <- "Sex-specific older children and adolescents results"
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
c_median_total_5_14  <- read.country.summary(dir_dt_cs = file.path(dir_median_total_5_14, file_name_total_5_24), year_wanted = year_started:2030)
c_median_f_5_14      <- read.country.summary(dir_dt_cs = file.path(dir_median_female_5_14, file_name_female_5_24), year_wanted = year_started:2030)
c_median_m_5_14      <- read.country.summary(dir_dt_cs = file.path(dir_median_male_5_14, file_name_male_5_24), year_wanted = year_started:2030)
c_median_total_15_24 <- read.country.summary(dir_dt_cs = file.path(dir_median_total_15_24, file_name_total_5_24), year_wanted = year_started:2030)
c_median_f_15_24     <- read.country.summary(dir_dt_cs = file.path(dir_median_female_15_24, file_name_female_5_24), year_wanted = year_started:2030)
c_median_m_15_24     <- read.country.summary(dir_dt_cs = file.path(dir_median_male_15_24, file_name_male_5_24), year_wanted = year_started:2030)

c_median_total_5_14 <- recode_ind_5_14(c_median_total_5_14)
c_median_f_5_14 <- recode_ind_5_14(c_median_f_5_14)
c_median_m_5_14 <- recode_ind_5_14(c_median_m_5_14)
c_median_total_15_24 <- recode_ind_15_24(c_median_total_15_24)
c_median_f_15_24 <- recode_ind_15_24(c_median_f_15_24)
c_median_m_15_24 <- recode_ind_15_24(c_median_m_15_24)

c_median_5_14    <- rbindlist(list(c_median_total_5_14, c_median_f_5_14, c_median_m_5_14))
c_median_15_24   <- rbindlist(list(c_median_total_15_24, c_median_f_15_24, c_median_m_15_24)) 
c_median_total_older <- merge(c_median_5_14, c_median_15_24)
c_median_total_older <- calculate.10q10(c_median_total_older)

# Separate female older children data
c_median_f_older <- merge(c_median_f_5_14, c_median_f_15_24)
c_median_f_older <- calculate.10q10(c_median_f_older)

# Separate male older children data
c_median_m_older <- merge(c_median_m_5_14, c_median_m_15_24)
c_median_m_older <- calculate.10q10(c_median_m_older)

col_order_older_children <- copy(colnames(c_median_total_older)) # colnames containing "X"
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
    fileInput('ISO_input', label = p("(Optional) Upload your ISO 3 country codes", 
                                     br(),
                                     "Download examples: ",
                                     a("single-region", href = "Upload_ISO3Code_example_single_region.csv", target = "_blank"),
                                     "·",
                                     a("multi-region", href = "Upload_ISO3Code_example_multiple_regions.csv", target = "_blank")
                                     ),
              placeholder = "Column name shall contain \"ISO\"", 
              accept = c(".csv", ".xlsx")
              ),
    style = "border: 0px"
    ),
    
    # rename the group
    # p("To name the group: "),
    wellPanel(
    uiOutput("panel_custom_group_name"),
    # run_gender
    checkboxInput(inputId = "run_gender", label = strong("Run sex-specific results?"),
                  value = TRUE),
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
               uiOutput("header_selected_countries"),
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
        uiOutput("panel_results_table_older_total"),
        br(),br(),
        uiOutput("panel_results_table_older_gender") 
      ),      
  # About
    get.about.panel(update_string = update_string0)
    ) #tabsetPanel
  ), #mainpanel
theme = "bootstrap.css"
) #ui fluidpage

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Initialize reactive value for uploaded region structure
  uploaded_region_structure <- NULL
  single_group_run <- reactiveVal(TRUE)

  build_grouped_country_html <- function(selected_countries, region_structure, country_lookup) {
    if (is.null(region_structure)) {
      return(NULL)
    }

    dt_wide <- copy(region_structure$data)
    region_cols <- intersect(region_structure$region_cols, colnames(dt_wide))
    if (length(region_cols) == 0) {
      return(NULL)
    }

    selected_iso <- unique(country_lookup[OfficialName %in% selected_countries, ISO3Code])
    dt_wide <- dt_wide[ISO3Code %in% selected_iso]
    if (nrow(dt_wide) == 0) {
      return(NULL)
    }

    dt_wide[country_lookup[, .(ISO3Code, OfficialName)], OfficialName := i.OfficialName, on = "ISO3Code"]
    dt_wide[, country_order := seq_len(.N)]

    dt_long <- melt(
      dt_wide,
      id.vars = c("ISO3Code", "OfficialName", "country_order"),
      measure.vars = region_cols,
      variable.name = "RegionLevel",
      value.name = "Region",
      variable.factor = FALSE
    )
    dt_long <- dt_long[Region != "" & !is.na(Region)]
    if (nrow(dt_long) == 0) {
      return(NULL)
    }

    dt_long[, DisplayName := ifelse(is.na(OfficialName) | OfficialName == "", ISO3Code, OfficialName)]
    dt_long <- unique(dt_long[, .(Region, DisplayName, country_order)])

    region_groups <- dt_long[, .(
      Countries = paste(sort(unique(DisplayName)), collapse = ", "),
      group_order = min(country_order)
    ), by = Region][order(group_order)]

    if (nrow(region_groups) <= 1) {
      return(NULL)
    }

    assigned_countries <- unique(dt_long$DisplayName)
    ungrouped_countries <- sort(unique(setdiff(selected_countries, assigned_countries)))
    if (length(ungrouped_countries) > 0) {
      region_groups <- rbind(
        region_groups,
        data.table(
          Region = "Ungrouped",
          Countries = paste(ungrouped_countries, collapse = ", "),
          group_order = Inf
        ),
        fill = TRUE
      )
    }

    paste(
      sprintf(
        "<strong>%s:</strong> %s",
        htmltools::htmlEscape(region_groups$Region),
        htmltools::htmlEscape(region_groups$Countries)
      ),
      collapse = "<br>"
    )
  }

  rename.single.region <- function(values) {
    custom_group_name <- if (is.null(input$adhoc_name)) "" else trimws(input$adhoc_name)
    values <- as.character(values)
    regions_non_world <- unique(values[!is.na(values) & nzchar(values) & values != "World"])

    if (length(regions_non_world) != 1) {
      return(values)
    }

    current_region <- regions_non_world[1]
    replacement <- current_region
    if (identical(current_region, "Adhoc")) {
      replacement <- if (nzchar(custom_group_name)) custom_group_name else adhoc_name
    } else if (nzchar(custom_group_name)) {
      replacement <- custom_group_name
    }

    values[values == current_region] <- replacement
    values
  }
  
  # Reset session 
  observeEvent(input$click_reset, {
    refresh()
  }, ignoreInit = TRUE)
  
  # renderUI: side panel ---------------------------------------------------
  # Update choices in country_input_select to be listed by region
  observeEvent(input$input_by_region, {
    updatePickerInput(session, inputId = "country_input_select", 
                      choices = if(input$input_by_region) input_country_list else countries,
                      selected = input$country_input_select)
  })

  output$panel_custom_group_name <- renderUI({
    if (!isTRUE(single_group_run())) {
      return(NULL)
    }

    textAreaInput(
      inputId = "adhoc_name",
      label = "(Optional) Display name for the single custom group",
      value = adhoc_name,
      rows = 1,
      placeholder = "Only changes the displayed/downloaded group name; leave blank to keep the default"
    )
  })
  
  # Read self-uploaded ISO file - now supports multi-region long format
  observeEvent(input$ISO_input, {
    req(input$ISO_input)
    
    file_type <- tolower(tools::file_ext(input$ISO_input$datapath))
    file_path <- input$ISO_input$datapath
    
    tryCatch({
      # Read file based on type
      if (file_type == "csv"){
        dt_iso <- fread(file_path, header = TRUE)
      } else if (file_type %in% c("xlsx", "xls")) {
        dt_iso <- setDT(readxl::read_excel(file_path))
      } else {
        showModal(modalDialog(
          title = "Invalid file type",
          "Currently accept csv, xlsx, or xls files. Please re-upload.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Validate file has content
      if (is.null(dt_iso) || length(colnames(dt_iso)) == 0) {
        showModal(modalDialog(
          title = "Invalid file",
          "The uploaded file is empty or cannot be read.",
          easyClose = TRUE
        ))
        return()
      }
      
      message(paste("Read in", file_type, "file with", nrow(dt_iso), "rows"))
      
      # Find ISO column
      ISO_columns <- colnames(dt_iso)[grepl("ISO", toupper(colnames(dt_iso)))]
      ISO_column_name <- if(length(ISO_columns) > 0) {
        ISO_columns[which.min(utils::adist("ISO", toupper(ISO_columns)))]
      } else {
        colnames(dt_iso)[which.min(utils::adist("ISO", toupper(colnames(dt_iso))))]
      }
      
      if(length(ISO_column_name) == 0 || !(ISO_column_name %in% colnames(dt_iso))){
        showModal(modalDialog(
          title = "Invalid file format",
          "Could not find ISO3Code column. Please ensure your file has an 'ISO3Code' column.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Standardize column name to ISO3Code
      if(ISO_column_name != "ISO3Code") {
        setnames(dt_iso, ISO_column_name, "ISO3Code")
      }
      
      # Filter to valid ISOs
      dt_iso <- dt_iso[ISO3Code %in% ISOs]
      
      if(nrow(dt_iso) == 0) {
        showModal(modalDialog(
          title = "No valid countries found",
          "None of the ISO codes in the uploaded file match valid country codes.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Check if Region column exists (long format support)
      has_region <- "Region" %in% colnames(dt_iso)
      region_code_lookup <- NULL
      if (has_region && "Region_Code" %in% colnames(dt_iso)) {
        region_code_lookup <- dt_iso[
          !is.na(Region) & nzchar(Region),
          .(Region_Code = {
            codes <- unique(as.character(Region_Code))
            codes <- codes[!is.na(codes) & nzchar(codes)]
            if (length(codes) == 0) NA_character_ else codes[1]
          }),
          by = Region
        ]
      }
      
      if(has_region) {
        # Long format with Region column - convert to wide format
        # Count how many times each ISO appears (determines number of region levels)
        iso_counts <- dt_iso[, .N, by = ISO3Code]
        max_levels <- max(iso_counts$N)
        
        if(max_levels == 1) {
          # Each ISO appears once - single region level
          dt_wide <- dt_iso[, .(ISO3Code, AdhocCountries = Region)]
          if("OfficialName" %in% colnames(dt_iso)) {
            dt_wide[, OfficialName := dt_iso$OfficialName]
          }
          region_cols <- "AdhocCountries"
          
        } else {
          # Multiple region levels - convert long to wide
          # Add region level indicator
          dt_iso[, region_level := seq_len(.N), by = ISO3Code]
          
          # Get OfficialName if exists
          if("OfficialName" %in% colnames(dt_iso)) {
            dt_official <- unique(dt_iso[, .(ISO3Code, OfficialName)])
          }
          
          # Cast to wide format
          dt_wide <- dcast(dt_iso, ISO3Code ~ region_level, 
                          value.var = "Region", 
                          fill = "")
          
          # Add OfficialName back if it exists
          if("OfficialName" %in% colnames(dt_iso)) {
            dt_wide <- dt_wide[dt_official, on = "ISO3Code"]
          }
          
          # Rename columns to AdhocCountries, AdhocCountries2, ...
          region_level_cols <- as.character(1:max_levels)
          new_names <- c("AdhocCountries", 
                        if(max_levels > 1) paste0("AdhocCountries", 2:max_levels) else NULL)
          setnames(dt_wide, region_level_cols, new_names)
          region_cols <- new_names
        }
        
      } else {
        # No Region column - all selected countries in single "AdhocCountries"
        dt_wide <- dt_iso[, .(ISO3Code)]
        if("OfficialName" %in% colnames(dt_iso)) {
          dt_wide[, OfficialName := dt_iso$OfficialName]
        }
        dt_wide[, AdhocCountries := "Adhoc"]
        region_cols <- "AdhocCountries"
      }
      
      # Store for later use in reactive.run()
      uploaded_region_structure <<- list(
        data = dt_wide,
        region_cols = region_cols,
        region_code_lookup = region_code_lookup
      )
      
      # Update UI selection to match uploaded ISOs
      matched_countries <- dc[ISO3Code %in% dt_wide$ISO3Code, OfficialName]
      updatePickerInput(session, inputId = "country_input_select",
                        selected = matched_countries)
      
      # Count unique regions for user feedback
      num_regions <- 0
      for(col in region_cols) {
        if(col %in% colnames(dt_wide)) {
          num_regions <- num_regions + uniqueN(dt_wide[get(col) != "", get(col)])
        }
      }
      single_group_run(num_regions <= 1)
      
      showModal(modalDialog(
        title = "File uploaded successfully",
        HTML(paste0(
          "Uploaded ", nrow(dt_wide), " countries<br>",
          "Region levels: ", length(region_cols), "<br>",
          "Total regions: ", num_regions, "<br><br>",
          "You may click anywhere to dismiss this message"
        )),
        easyClose = TRUE
      ))
      
    }, error = function(e){
      showModal(modalDialog(
        title = "Upload Error",
        paste("Error reading file:", e$message),
        easyClose = TRUE
      ))
      message("Upload error: ", e$message)
    })
  })
  
  # Reset selection
  observeEvent(input$click_reset,{
    updatePickerInput(session, inputId = "country_input", selected = default_select)
    updateCheckboxInput(session, inputId = "run_gender", value = FALSE)
    updateCheckboxInput(session, inputId = "run_older_total", value = FALSE)
    uploaded_region_structure <<- NULL  # Clear uploaded region structure
    single_group_run(TRUE)
  })

  # renderUI: main panel ----------------------------------------------------------------
  
  # print the currently selected countries dynamically
  output$text_selected_countries  = renderText({
    cs <- input$country_input_select
    
    # Check if results have multiple regions
    if (!is.null(reactive.run())) {
      regions <- unique(reactive.run()$both$Region)
      regions_non_world <- regions[regions != "World"]
      
      if (length(regions_non_world) > 1) {
        # Multiple regions: show region names only
        return(paste("The selected regions:", paste(sort(regions_non_world), collapse = ", ")))
      }
    }
    
    # Single region: show country list
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
  
  
  # Dynamic header for selected countries/regions
  output$header_selected_countries <- renderUI({
    if (!is.null(reactive.run())) {
      regions <- unique(reactive.run()$both$Region)
      regions_non_world <- regions[regions != "World"]
      
      if (length(regions_non_world) > 1) {
        return(h4("The selected regions:"))
      }
    }
    h4("The selected countries:")
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
      checkboxInput(inputId = "show_world", label = "Show results for the world in plots", value = TRUE),
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
    
    # Count number of regions (excluding World)
    regions <- unique(reactive.run()$both$Region)
    regions_non_world <- regions[regions != "World"]
    n_regions <- length(regions_non_world)
    
    # Dynamic height based on number of regions
    base_height <- 800
    plot_height <- if(n_regions > 2) base_height * 1.2 else base_height
    
    fluidRow(
      h4(strong(panel_title1.2)),
      

      if (length(input$show_world != 0)){
        if (input$show_world) {
          plotly::plotlyOutput("plot_rate_gender", height = paste0(plot_height * 1.1, "px"))  # Extra height for World
        } else {
          plotly::plotlyOutput("plot_rate_gender", height = paste0(plot_height, "px"))
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
      div(
        style = "display: flex; gap: 10px; flex-wrap: wrap;",
        downloadButton("download_table_all", "Download"),
        downloadButton("download_table_all_long", "Download all in long-format")
      ),
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
  
  output$panel_results_table_older_gender <- renderUI({
    if (is.null(reactive.run())) return()
    if (!input$run_older_total) return()
    
    fluidRow(
      h4(strong(panel_title2.4)),
      p(strong("Data for the female")),
      downloadButton("download_table_older_f", "Download"),
      br(),br(),
      DT::dataTableOutput("results_table_older_f", width = "80%"),
      
      br(),
      p(strong("Data for the male")),
      downloadButton("download_table_older_m", "Download"),
      br(),br(),
      DT::dataTableOutput("results_table_older_m", width = "80%")
    )
  })
  
  # check the names in `country.info.CME_adhoc.csv` file and 
  # run David's script `6outputaggregates.R`
  reactive.run <- eventReactive(input$click_run, {
    if (length(input$country_input_select)==0) {
      showModal(modalDialog(title = "Please select countries first.",  
                            footer = "You may click anywhere to dismiss this message", 
                            easyClose = TRUE))
      return(NULL)
    } else {
    
    # Prepare country info files with adhoc regions
    if(!is.null(uploaded_region_structure)) {
      # CASE: User uploaded file with region structure
      dt_wide <- uploaded_region_structure$data
      region_cols <- uploaded_region_structure$region_cols
      
      # Start with base dc and initialize all adhoc columns to empty
      dc_with_adhoc <- copy(dc)
      for(col in region_cols) {
        dc_with_adhoc[, (col) := ""]
      }
      
      # Merge uploaded region structure
      for(col in region_cols) {
        if(col %in% colnames(dt_wide)) {
          # Create a temporary dataset for merging
          dt_merge <- dt_wide[, .(ISO3Code, region_value = get(col))]
          dc_with_adhoc[dt_merge, on = "ISO3Code", 
                        (col) := ifelse(is.na(i.region_value) | i.region_value == "", 
                                       "", 
                                       i.region_value)]
        }
      }
      
    } else {
      # CASE: Manual selection via dropdown (single AdhocCountries column)
      dc_with_adhoc <- copy(dc)
      dc_with_adhoc[, AdhocCountries := ""]
      dc_with_adhoc[OfficialName %in% input$country_input_select, AdhocCountries := "Adhoc"]
      region_cols <- "AdhocCountries"
    }
    
    # Write to file
    write.csv(dc_with_adhoc, file = here::here("input", "country.info.CME_adhoc.csv"), row.names = FALSE)
    
    # Apply same logic to older children datasets
    if(input$run_older_total) {
      # 5-14 age group
      dc_5_14_adhoc <- copy(dc.5.14)
      for(col in region_cols) {
        dc_5_14_adhoc[, (col) := ""]
      }
      
      if(!is.null(uploaded_region_structure)) {
        dt_wide <- uploaded_region_structure$data
        for(col in region_cols) {
          if(col %in% colnames(dt_wide)) {
            dt_merge <- dt_wide[, .(ISO3Code, region_value = get(col))]
            dc_5_14_adhoc[dt_merge, on = "ISO3Code", 
                          (col) := ifelse(is.na(i.region_value) | i.region_value == "", 
                                         "", 
                                         i.region_value)]
          }
        }
      } else {
        dc_5_14_adhoc[OfficialName %in% input$country_input_select, AdhocCountries := "Adhoc"]
      }
      write.csv(dc_5_14_adhoc, file = here::here("input", "country.info.CME.5_14_adhoc.csv"), row.names = FALSE)
      
      # 15-24 age group
      dc_15_24_adhoc <- copy(dc.15.24)
      for(col in region_cols) {
        dc_15_24_adhoc[, (col) := ""]
      }
      
      if(!is.null(uploaded_region_structure)) {
        dt_wide <- uploaded_region_structure$data
        for(col in region_cols) {
          if(col %in% colnames(dt_wide)) {
            dt_merge <- dt_wide[, .(ISO3Code, region_value = get(col))]
            dc_15_24_adhoc[dt_merge, on = "ISO3Code", 
                           (col) := ifelse(is.na(i.region_value) | i.region_value == "", 
                                          "", 
                                          i.region_value)]
          }
        }
      } else {
        dc_15_24_adhoc[OfficialName %in% input$country_input_select, AdhocCountries := "Adhoc"]
      }
      write.csv(dc_15_24_adhoc, file = here::here("input", "country.info.CME.15_24_adhoc.csv"), row.names = FALSE)
    }
    
    cs <- input$country_input_select # country list
    time0 <- Sys.time()
    
    # Determine number of regions for modal message
    num_regions <- if(!is.null(uploaded_region_structure)) {
      dt_wide <- uploaded_region_structure$data
      region_cols <- uploaded_region_structure$region_cols
      total_regions <- 0
      for(col in region_cols) {
        if(col %in% colnames(dt_wide)) {
          total_regions <- total_regions + uniqueN(dt_wide[get(col) != "", get(col)])
        }
      }
      total_regions
    } else {
      1  # Single adhoc region
    }

    flat_country_list <- paste(sort(cs), collapse = ", ")
    grouped_country_html <- if (num_regions > 1) {
      build_grouped_country_html(cs, uploaded_region_structure, dc[, .(ISO3Code, OfficialName)])
    } else {
      NULL
    }
    modal_country_title <- if (!is.null(grouped_country_html)) {
      paste(
        length(cs),
        if (length(cs) == 1) "country" else "countries",
        "across",
        num_regions,
        if (num_regions == 1) "region" else "regions"
      )
    } else {
      paste(length(cs), if (length(cs) == 1) "country:" else "countries:", flat_country_list)
    }
    build_modal_body <- function(seconds) {
      HTML(paste0(
        "<br>",
        if (!is.null(grouped_country_html)) paste0(grouped_country_html, "<br><br>") else "",
        "Processing ", num_regions, " region(s).",
        "<br>It takes about ", seconds, " seconds."
      ))
    }
    
    # showModal
    # showModal will show that the script is running, and removed when scripts are done 
    if(input$run_gender){
      if(input$run_older_total){
        # sex-specific + older children
        showModal(modalDialog(title = paste0(
                                            "Running aggregate for sex-specific under-five and older children for ",
                                            modal_country_title), 
                              build_modal_body(60), 
                              footer=NULL))
      } else {
        # sex-specific only
        showModal(modalDialog(title = paste0(
                                            "Running sex-specific under-five aggregate for ",
                                            modal_country_title), 
                              build_modal_body(30), 
                              footer=NULL))
      }
     
    } else if (input$run_older_total) {
      # older children only (no sex-specific under-five, but sex-specific older children always runs)
      showModal(modalDialog(title = paste0(
                                          "Running aggregate for under-five and older children for ",
                                          modal_country_title), 
                            build_modal_body(30), 
                            footer=NULL))
    } else {
      # under-five only
      showModal(modalDialog(title = paste0(
                                          "Running under-five aggregate for ",
                                          modal_country_title), 
                            build_modal_body(15), 
                            footer=NULL))
    }
    
    # where we run the aggregates:
    run.outputaggregates(year.lastestimatepublished) # (always run the under-five total)
    
    if(input$run_gender){
      run.outputaggregates.gender(year.lastestimatepublished)
      adjust.u5.sex.specific.death() # adjust final death 
    }
    if(input$run_older_total){
      run.outputaggregates.5.24(year.lastestimatepublished)
      # Always run sex-specific for older children
      run.outputaggregates.5.24.gender(year.lastestimatepublished)
      adjust.total.death.5.24() # adjust final death for older children
    }
    
    removeModal()
    message("Time spent is ", round(Sys.time() - time0, 1), " seconds.")
    
    # Modify colnames and add the user-defined `adhoc_name`:
    change.adhoc.name <- function(dt){
      setnames(dt, gsub("\\.", " ", colnames(dt))) # revise colnames from adjusted sex-specific output
      setnames(dt, gsub("Under five", "Under-five", colnames(dt)))
      dt[, Region := rename.single.region(Region)]

      return(dt)
    }
    
    both_5_24 <- NULL
    f_5_24 <- NULL
    m_5_24 <- NULL
    if(input$run_older_total){
      # Process total older children
      dt5_14  <- change.adhoc.name(fread(file.path(dir_median_total_5_14,  "Rates & Deaths(ADJUSTED)_AdhocCountries.csv")))
      dt15_24 <- change.adhoc.name(fread(file.path(dir_median_total_15_24, "Rates & Deaths(ADJUSTED)_AdhocCountries.csv")))
      setnames(dt5_14, gsub(" median", "", colnames(dt5_14)))
      setnames(dt15_24, gsub(" median", "", colnames(dt15_24)))
      dt5_14  <- recode_ind_5_14(dt5_14)
      dt15_24 <- recode_ind_15_24(dt15_24)
      setkey(dt5_14, Region, Year)
      setkey(dt15_24, Region, Year)
      dt15_24 <- dt15_24[dt5_14]
      dt15_24 <- calculate.10q10(dt15_24)[, Sex := "Both"]
      both_5_24 <- dt15_24[Year >= 1990,..col_order_older_children]
      
      # Always process sex-specific older children
      # Process female 5-24
      dt5_14_f  <- change.adhoc.name(fread(file.path(dir_median_female_5_14,  "Rates & Deaths_AdhocCountries.csv")))
      dt15_24_f <- change.adhoc.name(fread(file.path(dir_median_female_15_24, "Rates & Deaths_AdhocCountries.csv")))
      setnames(dt5_14_f, gsub(" median", "", colnames(dt5_14_f)))
      setnames(dt15_24_f, gsub(" median", "", colnames(dt15_24_f)))
      dt5_14_f  <- recode_ind_5_14(dt5_14_f)
      dt15_24_f <- recode_ind_15_24(dt15_24_f)
      setkey(dt5_14_f, Region, Year)
      setkey(dt15_24_f, Region, Year)
      dt15_24_f <- dt15_24_f[dt5_14_f]
      dt15_24_f <- calculate.10q10(dt15_24_f)[, Sex := "Female"]
      f_5_24 <- dt15_24_f[Year >= 1990,..col_order_older_children]
      
      # Process male 5-24
      dt5_14_m  <- change.adhoc.name(fread(file.path(dir_median_male_5_14,  "Rates & Deaths_AdhocCountries.csv")))
      dt15_24_m <- change.adhoc.name(fread(file.path(dir_median_male_15_24, "Rates & Deaths_AdhocCountries.csv")))
      setnames(dt5_14_m, gsub(" median", "", colnames(dt5_14_m)))
      setnames(dt15_24_m, gsub(" median", "", colnames(dt15_24_m)))
      dt5_14_m  <- recode_ind_5_14(dt5_14_m)
      dt15_24_m <- recode_ind_15_24(dt15_24_m)
      setkey(dt5_14_m, Region, Year)
      setkey(dt15_24_m, Region, Year)
      dt15_24_m <- dt15_24_m[dt5_14_m]
      dt15_24_m <- calculate.10q10(dt15_24_m)[, Sex := "Male"]
      m_5_24 <- dt15_24_m[Year >= 1990,..col_order_older_children]
    }
    
    # output ----
    if(input$run_gender){
      output_list <- list(
        both = change.adhoc.name(fread(file.path(dir_median_total,  "Rates & Deaths_AdhocCountries.csv"))),
        f    = change.adhoc.name(fread(file.path(dir_median_female, "Rates & Deaths(ADJUSTED)_female_AdhocCountries.csv"))),
        m    = change.adhoc.name(fread(file.path(dir_median_male,   "Rates & Deaths(ADJUSTED)_male_AdhocCountries.csv"))),
        both_5_24 = both_5_24,
        f_5_24    = f_5_24,
        m_5_24    = m_5_24,
        c_median_total = c_median_total[Region%in%input$country_input_select,],
        c_median_f     = c_median_f[Region%in%input$country_input_select,],
        c_median_m     = c_median_m[Region%in%input$country_input_select,],
        c_median_total_older = c_median_total_older[Region%in%input$country_input_select,],
        c_median_f_older     = c_median_f_older[Region%in%input$country_input_select,],
        c_median_m_older     = c_median_m_older[Region%in%input$country_input_select,]
      )
    } else {
      # When run_gender is FALSE, under-five sex-specific is NULL
      # but older children sex-specific is still included (always run)
      output_list <- list(
        both = change.adhoc.name(fread(file.path(dir_median_total,  "Rates & Deaths_AdhocCountries.csv"))),
        f    = NULL,
        m    = NULL,
        both_5_24 = both_5_24,
        f_5_24    = f_5_24,
        m_5_24    = m_5_24,
        c_median_total = c_median_total[Region%in%input$country_input_select,],
        c_median_f     = NULL,
        c_median_m     = NULL,
        c_median_total_older = c_median_total_older[Region%in%input$country_input_select,],
        c_median_f_older     = c_median_f_older[Region%in%input$country_input_select,],
        c_median_m_older     = c_median_m_older[Region%in%input$country_input_select,]
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
                                value.name = "Rate", variable.name = "Indicator",
                                variable.factor = FALSE)
    dt_long[, Rate := suppressWarnings(as.numeric(Rate))]
    dt_long <- dt_long[!is.na(Rate), .(Region, Year = as.integer(Year), Rate = round(Rate, 2), Indicator)]
    data.table::setorder(dt_long, Indicator, Region, Year)
    return(dt_long)
  }
  
  titlefont0 <- 14 # xlab size
  limit.plot.regions <- function(dt, max_regions = 10L) {
    if (is.null(dt) || !"Region" %in% colnames(dt)) return(dt)

    region_order <- unique(as.character(dt$Region))
    non_world_regions <- region_order[region_order != "World"]
    if (length(non_world_regions) <= max_regions) return(dt)

    keep_regions <- non_world_regions[seq_len(max_regions)]
    if ("World" %in% region_order) keep_regions <- c(keep_regions, "World")
    dt[Region %in% keep_regions]
  }

  region.legend.layout <- function(regions) {
    region_count <- length(unique(as.character(regions)))
    items_per_row <- max(1L, ceiling(region_count/2))

    list(
      orientation = "h",
      x = 0,
      xanchor = "left",
      y = -0.18,
      yanchor = "top",
      traceorder = "normal",
      entrywidthmode = "fraction",
      entrywidth = min(1, 1/items_per_row)
    )
  }

  plot.rate <- function(dt, vars0, title0 = "Deaths per 1,000 live births"){
    dt <- limit.plot.regions(dt)
    dt_long <- get.long(dt, vars0)
    # note that the final indicator names are matched here: 
    dt_long[, Indicator := RecodePlotIndicators(Indicator, indicator.order = vars0)]
    p <- ggplot(dt_long, aes(x = Year, y = Rate, color = Region, group = Region)) +
      geom_line(linewidth = 1) + 
      theme_bw() + 
      labs(y = "", x = "", color = "") +
      facet_wrap(~ Indicator) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      theme(legend.position = "bottom") +
      scale_color_manual(values = ResolvePlotColors(uniqueN(dt_long$Region))) +
      scale_x_continuous(breaks = c(1990, 2000, 2010, year_ended))  
    
    return(plotly::ggplotly(p, tooltip = c("Year", "Rate")) %>% 
             # layout(yaxis = list(title = title0), font_size = 9,
             layout(yaxis = list(title = title0, titlefont = list(size = titlefont0)), 
                    legend = region.legend.layout(dt_long$Region))) # legend.position = "bottom"
  }
  
  output$plot_rate <- plotly::renderPlotly({
    if (is.null(reactive.run())) return()
    dt <- reactive.run()$both
    if(!input$show_world) dt <- dt[Region!="World",]
    plot.rate(dt, vars0 = c("U5MR median", "IMR median", "NMR median"))
  })
  
  output$plot_rate_older1 <- plotly::renderPlotly({
    if (is.null(reactive.run())) return()
    dt <- reactive.run()$both_5_24
    if(!input$show_world) dt <- dt[Region!="World",]
    plot.rate(dt, vars0 = c("Mortality rate age 5-24", "Mortality rate age 10-19"), title0 = "Deaths per 1,000")
  })
  
  output$plot_rate_older2 <- plotly::renderPlotly({
    if (is.null(reactive.run())) return()
    dt <- reactive.run()$both_5_24
    if(!input$show_world) dt <- dt[Region!="World",]
    plot.rate(dt, vars0 = c("Mortality rate age 5-14", "Mortality rate age 5-9", "Mortality rate age 10-14"), title0 = "Deaths per 1,000")
  })
  
  output$plot_rate_older3 <- plotly::renderPlotly({
    if (is.null(reactive.run())) return()
    dt <- reactive.run()$both_5_24
    if(!input$show_world) dt <- dt[Region!="World",]
    plot.rate(dt, vars0 = c("Mortality rate age 15-24", "Mortality rate age 15-19", "Mortality rate age 20-24"), title0 = "Deaths per 1,000")
  })


  # Figure 2. Death by year -------------------------------------------------
  # separate plot function for death to modify tooltip, legend, etc, function is similar
  plot.death <- function(dt){
    dt <- limit.plot.regions(dt)
    dt <- dt[Year>=year_started,]
    death.vars <- grep("deaths", colnames(dt), value = TRUE)
    dt_long <- data.table::melt(dt, measure.vars = death.vars, 
                                  value.name = "Deaths", variable.name = "type")
    dt_long[, type := RecodePlotIndicators(type, indicator.order = death.vars)]
    # dt_long[, Death_Number_1K := round(Deaths/1E3)]
    p <- ggplot(dt_long[!is.na(Deaths),], aes(x = Year, y = Deaths, color = Region)) +
        geom_line(linewidth = 1) + 
        theme_bw() + 
        # ggtitle("Deaths of U5MR, IMR, and NMR by Year") + 
        labs(y = "", x = "", color = "") +
        facet_wrap(facets= ~ type) + 
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      theme(legend.position = "bottom") +
        scale_color_manual(values = ResolvePlotColors(uniqueN(dt_long$Region)))+
        scale_x_continuous(breaks = c(1990, 2000, 2010, year_ended)) + 
        scale_y_continuous(labels = scales::label_number(suffix = "k", scale = 1E-3, big.mark = ","))
    
    plotly::ggplotly(p, tooltip = c("Year", "Deaths")) %>% 
                                  layout(yaxis = list(title =  "Number of deaths", 
                                                      titlefont = list(size = titlefont0),
                                                      tickfont = list(size = 10)),
                       legend = region.legend.layout(dt_long$Region))
  }

  output$plot_death <- plotly::renderPlotly({
    if (!is.null(reactive.run())){
    dt <- reactive.run()$both
    if(!input$show_world) dt <- dt[Region!="World",]
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
    if(!input$show_world) dt_long <- dt_long[Region!="World",]
    dt_long <- limit.plot.regions(dt_long)
    data.table::setorder(dt_long, Indicator, Region, Sex, Year)
    dt_long[, Indicator := RecodePlotIndicators(Indicator, indicator.order = vars0)]
    
    p <- ggplot(dt_long, aes(x = Year, y = Rate, color = Sex, group = interaction(Region, Sex))) +
      geom_line(linewidth = 1) + 
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
  
  output$results_table_older_f <- DT::renderDT({
    if (!is.null(reactive.run()$f_5_24)){
      dt <- reactive.run()$f_5_24
      dt[, Sex:= "Female"]
      DT::formatRound(
        DT::datatable( clean.table(dt) ),
        columns = col_order_older_children_all_rate, digits = 2)
    }
  })
  
  output$results_table_older_m <- DT::renderDT({
    if (!is.null(reactive.run()$m_5_24)){
      dt <- reactive.run()$m_5_24
      dt[, Sex:= "Male"]
      DT::formatRound(
        DT::datatable( clean.table(dt) ),
        columns = col_order_older_children_all_rate, digits = 2)
    }
  })

  # Download ----------------------------------------------------------------
  # Make results available for downloading
  table.to.long <- function(dt, needs_clean = FALSE) {
    if (is.null(dt)) return(NULL)

    dt_long <- data.table::copy(dt)
    if (needs_clean) dt_long <- clean.table(dt_long)
    if (!"Sex" %in% colnames(dt_long)) dt_long[, Sex := "Total"]
    dt_long[Sex == "Both", Sex := "Total"]

    value_cols <- setdiff(colnames(dt_long), c("Region", "Year", "Sex"))
    if (length(value_cols) == 0) return(NULL)
    for (vc in value_cols) set(dt_long, j = vc, value = as.double(dt_long[[vc]]))

    dt_long <- melt.data.table(
      dt_long,
      id.vars = c("Region", "Year", "Sex"),
      measure.vars = value_cols,
      variable.name = "Shortind",
      value.name = "Median",
      variable.factor = FALSE
    )
    dt_long <- dt_long[!is.na(Median), .(Region, Shortind, Sex, Year, Median)]
    setorder(dt_long, Region, -Shortind, -Sex, -Year)
    dt_long
  }

  get.long.download.region.codes <- function() {
    if (is.null(uploaded_region_structure) || is.null(uploaded_region_structure$region_code_lookup)) {
      return(NULL)
    }

    region_code_lookup <- copy(uploaded_region_structure$region_code_lookup)
    if (nrow(region_code_lookup) == 0 || !"Region_Code" %in% colnames(region_code_lookup)) {
      return(NULL)
    }

    region_code_lookup <- unique(region_code_lookup[, .(Region, Region_Code)], by = "Region")
    region_code_lookup <- region_code_lookup[!is.na(Region_Code) & nzchar(Region_Code)]
    if (nrow(region_code_lookup) == 0) {
      return(NULL)
    }

    region_code_lookup[, Region := rename.single.region(Region)]
    unique(region_code_lookup, by = "Region")
  }

  build.long.download <- function(results) {
    dt_list <- list(
      table.to.long(results$both, needs_clean = TRUE),
      table.to.long(results$f, needs_clean = TRUE),
      table.to.long(results$m, needs_clean = TRUE)
    )

    if (!is.null(results$both_5_24)) {
      dt_list <- c(
        dt_list,
        list(
          table.to.long(results$both_5_24, needs_clean = TRUE),
          table.to.long(results$f_5_24, needs_clean = TRUE),
          table.to.long(results$m_5_24, needs_clean = TRUE)
        )
      )
    }

    dtout <- rbindlist(Filter(Negate(is.null), dt_list), fill = TRUE)
    region_code_lookup <- get.long.download.region.codes()
    if (!is.null(region_code_lookup)) {
      dtout[region_code_lookup, Region_Code := i.Region_Code, on = "Region"]
      setcolorder(dtout, c("Region", "Region_Code", "Shortind", "Sex", "Year", "Median"))
    }
    setorder(dtout, Region, -Shortind, -Sex, -Year)
    dtout
  }
  
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

  output$download_table_all_long <- downloadHandler(
    filename = function() {
      paste0("Results_all_long_format_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- build.long.download(reactive.run())
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
                              reactive.run()$c_median_total_older,
                              clean.table(reactive.run()$f_5_24),
                              reactive.run()$c_median_f_older,
                              clean.table(reactive.run()$m_5_24),
                              reactive.run()$c_median_m_older
      ), fill = TRUE)
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )
  
  output$download_table_older_f <- downloadHandler(
    filename = function() {
      paste0("Results_older_children_female_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- rbindlist(list(clean.table(reactive.run()$f_5_24),
                              reactive.run()$c_median_f_older
      ), fill = TRUE)
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )
  
  output$download_table_older_m <- downloadHandler(
    filename = function() {
      paste0("Results_older_children_male_", format.Date(Sys.Date(), "%y_%m_%d"), ".csv")
    },
    content = function(file) {
      dtout <- rbindlist(list(clean.table(reactive.run()$m_5_24),
                              reactive.run()$c_median_m_older
      ), fill = TRUE)
      write.csv(dtout, file, row.names = FALSE, na = "")
    }
  )
}

# Run App ---------------------------------------------------------------------
shinyApp(ui, server)
