test_that("layout weight optimization returns positive weights and diagnostics", {
  layout_matrix <- rbind(c("1", "2"))
  page <- patchworkLayoutOptimizer:::make_layout_page(c("p1", "p2"), layout_matrix)
  page_spec <- list(width_mm = 120, height_mm = 80)

  fit_functions <- list(
    p1 = function(width_mm, height_mm) {
      violation <- max(0, 70 - width_mm)
      list(
        total_loss = violation^2,
        hard_violation_mm = violation,
        panel_width_mm = width_mm,
        panel_height_mm = height_mm,
        min_x_label_gap_mm = Inf,
        min_y_label_gap_mm = Inf,
        label_gap_loss = 0,
        panel_minimum_loss = violation^2,
        legend_loss = 0,
        facet_loss = 0,
        data_density_loss = 0,
        warnings = character()
      )
    },
    p2 = function(width_mm, height_mm) {
      list(
        total_loss = 0,
        hard_violation_mm = 0,
        panel_width_mm = width_mm,
        panel_height_mm = height_mm,
        min_x_label_gap_mm = Inf,
        min_y_label_gap_mm = Inf,
        label_gap_loss = 0,
        panel_minimum_loss = 0,
        legend_loss = 0,
        facet_loss = 0,
        data_density_loss = 0,
        warnings = character()
      )
    }
  )

  preferences <- list(
    complexity_penalty = 0,
    empty_cell_penalty = 0,
    uneven_weight_penalty = 0.01
  )

  optimized_page <- patchworkLayoutOptimizer:::optimize_layout_weights(page, fit_functions, page_spec, preferences)

  expect_true(all(optimized_page$widths > 0))
  expect_true(all(optimized_page$heights > 0))
  expect_equal(nrow(optimized_page$diagnostics), 2)
  expect_gt(optimized_page$widths[1], optimized_page$widths[2])
})
