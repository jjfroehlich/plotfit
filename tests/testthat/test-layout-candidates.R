test_that("generated layout candidates are rectangular and include all plots", {
  plot_ids <- c("p1", "p2", "p3")
  frontiers <- list(
    p1 = list(preferred_width_mm = 40, preferred_height_mm = 30, min_acceptable_area_mm2 = 1200),
    p2 = list(preferred_width_mm = 50, preferred_height_mm = 30, min_acceptable_area_mm2 = 1500),
    p3 = list(preferred_width_mm = 90, preferred_height_mm = 60, min_acceptable_area_mm2 = 5400)
  )

  layouts <- patchworkLayoutOptimizer:::generate_equal_grid_layouts(plot_ids, max_grid_cols = 3, max_grid_rows = 3)
  layouts <- c(
    layouts,
    patchworkLayoutOptimizer:::generate_row_band_layouts(plot_ids, frontiers, 4, 4, TRUE),
    patchworkLayoutOptimizer:::generate_one_large_plot_layouts(plot_ids, frontiers, 4, 4)
  )

  expect_gt(length(layouts), 0)
  expect_true(all(vapply(layouts, function(page) {
    patchworkLayoutOptimizer:::validate_rectangular_design(page$layout_matrix) &&
      setequal(page$areas$plot_id, plot_ids)
  }, logical(1))))
})

test_that("overfit compact spacer layout is not generated for eight plot page", {
  plot_ids <- paste0("p", seq_len(8))
  frontiers <- lapply(plot_ids, function(plot_id) {
    list(
      preferred_width_mm = 30,
      preferred_height_mm = 25,
      min_acceptable_area_mm2 = 750,
      geometry = list(is_faceted_scatter = FALSE)
    )
  })
  names(frontiers) <- plot_ids
  frontiers$p6$geometry$is_faceted_scatter <- TRUE

  layouts <- patchworkLayoutOptimizer:::generate_compact_spacer_layouts(
    plot_ids = plot_ids,
    frontiers = frontiers,
    max_grid_cols = 10,
    max_grid_rows = 4
  )

  expect_equal(length(layouts), 0)
})

test_that("overfit scaled report layout is not generated for eight plot page", {
  plot_ids <- paste0("p", seq_len(8))
  frontiers <- lapply(plot_ids, function(plot_id) {
    list(
      preferred_width_mm = 30,
      preferred_height_mm = 25,
      min_acceptable_area_mm2 = 750,
      geometry = list(is_faceted_scatter = FALSE)
    )
  })
  names(frontiers) <- plot_ids
  frontiers$p6$geometry$is_faceted_scatter <- TRUE

  layouts <- patchworkLayoutOptimizer:::generate_scaled_report_layouts(
    plot_ids = plot_ids,
    frontiers = frontiers,
    max_grid_cols = 20,
    max_grid_rows = 5
  )

  expect_equal(length(layouts), 0)
})
