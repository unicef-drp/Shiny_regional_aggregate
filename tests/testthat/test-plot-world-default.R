testthat::test_that("world plot default follows single versus multi-region runs", {
  should_show_world_in_plots <- pkg_fn("should_show_world_in_plots")

  testthat::expect_true(should_show_world_in_plots(NULL, single_group_run = TRUE))
  testthat::expect_false(should_show_world_in_plots(NULL, single_group_run = FALSE))
})

testthat::test_that("explicit world plot checkbox choices override the default", {
  should_show_world_in_plots <- pkg_fn("should_show_world_in_plots")

  testthat::expect_false(should_show_world_in_plots(FALSE, single_group_run = TRUE))
  testthat::expect_true(should_show_world_in_plots(TRUE, single_group_run = FALSE))
})
