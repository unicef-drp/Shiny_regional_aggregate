if (!isNamespaceLoaded("shinyregionalaggregate")) {
  pkgload::load_all(
    path = testthat::test_path("..", ".."),
    export_all = FALSE,
    helpers = FALSE,
    quiet = TRUE
  )
}
