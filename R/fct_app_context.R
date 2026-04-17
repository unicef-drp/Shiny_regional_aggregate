#' Build cached app context from packaged release data
#' @noRd
build_app_context <- local({
  cache <- NULL

  function(force = FALSE) {
    if (!isTRUE(force) && !is.null(cache)) {
      return(cache)
    }

    meta <- release_metadata()
    dc <- data.table::fread(release_path("input", "country.info.CME.csv"))
    dc.5.14 <- data.table::fread(release_path("input", "country.info.CME.5_14.csv"))
    dc.15.24 <- data.table::fread(release_path("input", "country.info.CME.15_24.csv"))

    dc[, UNICEF_region := ifelse(UNICEFReportRegion2 == "", UNICEFReportRegion1, UNICEFReportRegion2)]
    dc[, SDG_region := ifelse(SDGSimpleRegion1 != "Oceania", SDGSimpleRegion1, SDGSimpleRegion2)]
    dc[, SDG_region := dplyr::recode(SDG_region, !!!SDG_name_list)]

    regions_3 <- sort(unique(dc$M49Region1))
    input_country_list <- vector("list", length(regions_3))
    names(input_country_list) <- regions_3
    for (region_name in regions_3) {
      input_country_list[[region_name]] <- sort(unique(dc[M49Region1 == region_name, OfficialName]))
    }

    countries <- sort(unique(dc$OfficialName))
    ISOs <- sort(unique(dc$ISO3Code))
    world_map <- get.world.map()

    c_median_total <- read.country.summary(
      dir_dt_cs = release_path("median_results_total", meta$file_name_total),
      year_wanted = meta$year_started:2030
    )
    c_median_f <- read.country.summary(
      dir_dt_cs = release_path("median_results_female", meta$file_name_female),
      year_wanted = meta$year_started:2030
    )
    c_median_m <- read.country.summary(
      dir_dt_cs = release_path("median_results_male", meta$file_name_male),
      year_wanted = meta$year_started:2030
    )

    c_median_total_5_14 <- read.country.summary(
      dir_dt_cs = release_path("median_results_total_5_14", meta$file_name_total_5_24),
      year_wanted = meta$year_started:2030
    )
    c_median_f_5_14 <- read.country.summary(
      dir_dt_cs = release_path("median_results_female_5_14", meta$file_name_female_5_24),
      year_wanted = meta$year_started:2030
    )
    c_median_m_5_14 <- read.country.summary(
      dir_dt_cs = release_path("median_results_male_5_14", meta$file_name_male_5_24),
      year_wanted = meta$year_started:2030
    )
    c_median_total_15_24 <- read.country.summary(
      dir_dt_cs = release_path("median_results_total_15_24", meta$file_name_total_5_24),
      year_wanted = meta$year_started:2030
    )
    c_median_f_15_24 <- read.country.summary(
      dir_dt_cs = release_path("median_results_female_15_24", meta$file_name_female_5_24),
      year_wanted = meta$year_started:2030
    )
    c_median_m_15_24 <- read.country.summary(
      dir_dt_cs = release_path("median_results_male_15_24", meta$file_name_male_5_24),
      year_wanted = meta$year_started:2030
    )

    c_median_total_5_14 <- recode_ind_5_14(c_median_total_5_14)
    c_median_f_5_14 <- recode_ind_5_14(c_median_f_5_14)
    c_median_m_5_14 <- recode_ind_5_14(c_median_m_5_14)
    c_median_total_15_24 <- recode_ind_15_24(c_median_total_15_24)
    c_median_f_15_24 <- recode_ind_15_24(c_median_f_15_24)
    c_median_m_15_24 <- recode_ind_15_24(c_median_m_15_24)

    c_median_total_older <- calculate.10q10(merge(
      c_median_total_15_24,
      c_median_total_5_14,
      by = c("Region", "Year", "Sex"),
      sort = FALSE
    ))
    c_median_f_older <- calculate.10q10(merge(
      c_median_f_15_24,
      c_median_f_5_14,
      by = c("Region", "Year", "Sex"),
      sort = FALSE
    ))
    c_median_m_older <- calculate.10q10(merge(
      c_median_m_15_24,
      c_median_m_5_14,
      by = c("Region", "Year", "Sex"),
      sort = FALSE
    ))

    col_order_older_children <- colnames(c_median_total_older)
    col_order_older_children_all_rate <- grep("Mortality rate", col_order_older_children, value = TRUE)
    year_ended <- floor(max(c_median_total$Year))

    cache <<- list(
      meta = meta,
      update_string = meta$update_string,
      year_started = meta$year_started,
      year_ended = year_ended,
      year.lastestimatepublished = year_ended + 0.5,
      default_select = "Afghanistan",
      adhoc_name = "Selected Countries",
      dc = dc,
      dc.5.14 = dc.5.14,
      dc.15.24 = dc.15.24,
      countries = countries,
      ISOs = ISOs,
      input_country_list = input_country_list,
      world_map = world_map,
      c_median_total = c_median_total,
      c_median_f = c_median_f,
      c_median_m = c_median_m,
      c_median_total_older = c_median_total_older,
      c_median_f_older = c_median_f_older,
      c_median_m_older = c_median_m_older,
      col_order_older_children = col_order_older_children,
      col_order_older_children_all_rate = col_order_older_children_all_rate
    )

    cache
  }
})
