#' Application server
#' @noRd
app_server <- function(input, output, session) {
  region_input <- shiny::reactiveVal(NULL)
  results_long <- shiny::reactiveVal(NULL)
  run_status <- shiny::reactiveVal("Upload a Region / ISO3Code file to begin.")

  shiny::observeEvent(input$region_file, {
    region_input(NULL)
    results_long(NULL)
    run_status("Ready to run.")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$run_aggregate, {
    shiny::req(input$region_file)

    out <- tryCatch(
      shiny::withProgress(message = "Running regional aggregates", value = 0, {
        shiny::incProgress(0.1, detail = "Reading region membership")
        uploaded <- read_region_iso_file(input$region_file$datapath)
        normalized <- normalize_region_iso_input(uploaded)
        region_input(normalized)

        shiny::incProgress(0.2, detail = "Computing long-format output")
        long_results <- get_CME_aggregate(normalized)
        results_long(long_results)
        long_results
      }),
      error = function(e) {
        results_long(NULL)
        run_status(paste("Run failed:", conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(out)) {
      run_status(
        sprintf(
          "Finished %s rows across %s regions.",
          prettyNum(nrow(out), big.mark = ","),
          data.table::uniqueN(out$Region)
        )
      )
    }
  }, ignoreInit = TRUE)

  output$run_status <- shiny::renderText(run_status())

  output$results_summary <- shiny::renderText({
    out <- results_long()
    if (is.null(out)) {
      return("")
    }

    sprintf(
      "%s indicators across %s years.",
      data.table::uniqueN(out$Shortind),
      data.table::uniqueN(out$Year)
    )
  })

  output$region_input_preview <- shiny::renderTable({
    dt <- region_input()
    if (is.null(dt)) {
      return(NULL)
    }

    utils::head(dt, 12)
  }, rownames = FALSE)

  output$results_preview <- shiny::renderTable({
    out <- results_long()
    if (is.null(out)) {
      return(NULL)
    }

    utils::head(out, 25)
  }, rownames = FALSE)

  output$download_long <- shiny::downloadHandler(
    filename = function() {
      paste0("cme_aggregate_long_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      shiny::req(results_long())
      data.table::fwrite(results_long(), file)
    }
  )
}
