test_that("fit loss increases when allocated panel space is too small", {
  device_state <- patchworkLayoutOptimizer:::open_measurement_device(
    device = "pdf",
    width_in = 8.27,
    height_in = 11.69,
    base_family = "Helvetica",
    base_size = 7
  )
  on.exit(patchworkLayoutOptimizer:::close_measurement_device(device_state), add = TRUE)

  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  preferences <- patchworkLayoutOptimizer:::validate_layout_inputs(
    page_width_in = 8.27,
    page_height_in = 11.69,
    page_margin_mm = 0,
    base_size = 7,
    min_label_gap_mm = 1.5,
    target_label_gap_mm = NULL,
    min_panel_width_mm = NULL,
    min_panel_height_mm = NULL,
    min_panel_area_mm2 = NULL,
    max_grid_cols = 3,
    max_grid_rows = 3,
    allow_multipage = TRUE,
    max_pages = 2,
    multipage_penalty = 25,
    search_budget = 10,
    return_candidates = 2,
    device = "pdf",
    output_style = "design"
  )
  page_spec <- list(width_mm = 8.27 * 25.4, height_mm = 11.69 * 25.4)
  profile <- patchworkLayoutOptimizer:::measure_plot_profile(
    plot = plot,
    plot_id = "p1",
    plot_index = 1,
    page_spec = page_spec,
    measurement_spec = list(base_size = 7, base_family = "Helvetica")
  )

  small_fit <- patchworkLayoutOptimizer:::evaluate_plot_fit(profile, 20, 20, preferences)
  large_fit <- patchworkLayoutOptimizer:::evaluate_plot_fit(profile, 90, 70, preferences)

  expect_gt(small_fit$total_loss, large_fit$total_loss)
  expect_gt(small_fit$hard_violation_mm, 0)
})

test_that("effective panel size reports wasted area for fixed-aspect panels", {
  profile <- list(
    geometry = list(target_panel_aspect = 1),
    page = list(width_mm = 100, height_mm = 100)
  )

  tall_panel <- patchworkLayoutOptimizer:::estimate_effective_panel_size(
    profile = profile,
    panel_width_mm = 40,
    panel_height_mm = 100
  )
  square_panel <- patchworkLayoutOptimizer:::estimate_effective_panel_size(
    profile = profile,
    panel_width_mm = 40,
    panel_height_mm = 40
  )

  expect_equal(tall_panel$effective_panel_width_mm, 40)
  expect_equal(tall_panel$effective_panel_height_mm, 40)
  expect_gt(tall_panel$unused_panel_area_mm2, 0)
  expect_equal(square_panel$unused_panel_area_mm2, 0)
})

test_that("gtable sizing resolves panel area at an allocated physical size", {
  device_state <- patchworkLayoutOptimizer:::open_measurement_device(
    device = "pdf",
    width_in = 8.27,
    height_in = 11.69,
    base_family = "Helvetica",
    base_size = 7
  )
  on.exit(patchworkLayoutOptimizer:::close_measurement_device(device_state), add = TRUE)

  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  profile <- patchworkLayoutOptimizer:::measure_plot_profile(
    plot = plot,
    plot_id = "p1",
    plot_index = 1,
    page_spec = list(width_mm = 120, height_mm = 80),
    measurement_spec = list(base_size = 7, base_family = "Helvetica")
  )

  measured <- patchworkLayoutOptimizer:::measure_plot_at_size(profile, 80, 50)

  expect_equal(measured$width_mm, 80)
  expect_equal(measured$height_mm, 50)
  expect_gt(measured$panel_width_mm, 0)
  expect_gt(measured$panel_height_mm, 0)
  expect_lte(measured$panel_width_mm, 80)
  expect_lte(measured$panel_height_mm, 50)
})

test_that("axis gap estimation uses neighbouring label positions", {
  axis_labels <- data.frame(
    width_mm = c(5, 5, 5),
    height_mm = c(2, 2, 2),
    rotation = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  preferences <- list(
    min_label_gap_mm = 1,
    target_label_gap_mm = 2,
    hard_label_penalty = 1000,
    soft_excess_gap_penalty = 0.01
  )

  even_gap <- patchworkLayoutOptimizer:::estimate_one_axis_gap(
    axis_labels = axis_labels,
    axis_positions = list(positions = c(0, 0.5, 1), fallback = FALSE),
    available_mm = 100,
    axis = "x",
    preferences = preferences
  )
  crowded_gap <- patchworkLayoutOptimizer:::estimate_one_axis_gap(
    axis_labels = axis_labels,
    axis_positions = list(positions = c(0, 0.05, 1), fallback = FALSE),
    available_mm = 100,
    axis = "x",
    preferences = preferences
  )

  expect_gt(even_gap$min_gap_mm, crowded_gap$min_gap_mm)
  expect_equal(even_gap$violation_mm, 0)
  expect_gt(crowded_gap$violation_mm, 0)
})
