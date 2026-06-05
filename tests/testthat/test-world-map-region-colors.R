testthat::test_that("uploaded multi-region map data assigns region fill colors", {
  raw <- data.table::data.table(
    Region = c("Group A", "Group A", "Group B", "Group B"),
    ISO3Code = c("CHN", "ETH", "VEN", "AFG")
  )
  normalized <- pkg_fn("normalize_region_iso_input")(raw)
  membership <- pkg_fn("build_region_membership_wide")(normalized)
  region_structure <- c(membership, list(normalized = normalized))
  country_lookup <- data.table::fread(
    testthat::test_path("..", "..", "inst", "extdata", "input", "country.info.CME.csv")
  )[, .(ISO3Code, OfficialName)]
  selected_countries <- country_lookup[ISO3Code %in% raw$ISO3Code, OfficialName]

  selected_map <- pkg_fn("build_selected_world_map")(
    world_map = pkg_fn("get.world.map")(),
    selected_countries = selected_countries,
    country_lookup = country_lookup,
    region_structure = region_structure
  )

  testthat::expect_true(all(c(
    "Afghanistan",
    "China",
    "Ethiopia",
    "Taiwan",
    "Venezuela (Bolivarian Republic of)"
  ) %in% selected_map$country))
  testthat::expect_identical(
    selected_map$Region[selected_map$country == "Ethiopia"][[1]],
    "Group A"
  )
  testthat::expect_identical(
    selected_map$Region[selected_map$country == "Venezuela (Bolivarian Republic of)"][[1]],
    "Group B"
  )
  testthat::expect_length(unique(selected_map$fillColor[selected_map$Region %in% c("Group A", "Group B")]), 2L)
})

testthat::test_that("Taiwan inherits China's map fill color", {
  raw <- data.table::data.table(Region = "China group", ISO3Code = "CHN")
  normalized <- pkg_fn("normalize_region_iso_input")(raw)
  membership <- pkg_fn("build_region_membership_wide")(normalized)
  region_structure <- c(membership, list(normalized = normalized))
  country_lookup <- data.table::fread(
    testthat::test_path("..", "..", "inst", "extdata", "input", "country.info.CME.csv")
  )[, .(ISO3Code, OfficialName)]

  selected_map <- pkg_fn("build_selected_world_map")(
    world_map = pkg_fn("get.world.map")(),
    selected_countries = "China",
    country_lookup = country_lookup,
    region_structure = region_structure
  )

  testthat::expect_setequal(selected_map$country, c("China", "Taiwan"))
  testthat::expect_identical(
    selected_map$fillColor[selected_map$country == "Taiwan"][[1]],
    selected_map$fillColor[selected_map$country == "China"][[1]]
  )
})
