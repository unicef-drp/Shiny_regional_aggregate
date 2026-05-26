polygon_longitude_spans <- function(spatial_polygons) {
  unlist(lapply(spatial_polygons@polygons, function(country_polygons) {
    vapply(country_polygons@Polygons, function(poly) {
      diff(range(poly@coords[, 1]))
    }, numeric(1))
  }), use.names = FALSE)
}

testthat::test_that("world map polygons selected by WB upload do not wrap across the map", {
  raw <- pkg_fn("read_region_iso_file")(
    testthat::test_path("..", "..", "inst", "extdata", "examples", "Upload_ISO_example_WB.csv")
  )
  selected_countries <- unique(raw$OfficialName)
  world_map <- pkg_fn("get.world.map")()
  selected_map <- world_map[world_map$country %in% selected_countries, ]

  testthat::expect_lt(max(polygon_longitude_spans(selected_map)), 180)
})
