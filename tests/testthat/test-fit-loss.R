test_that("fit loss increases when allocated panel space is too small", {
  device_state <- plotfit:::open_measurement_device(
    device = "pdf",
    width_in = 8.27,
    height_in = 11.69,
    base_family = "Helvetica",
    base_size = 7
  )
  on.exit(plotfit:::close_measurement_device(device_state), add = TRUE)

  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  preferences <- plotfit:::validate_layout_inputs(
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
  profile <- plotfit:::measure_plot_profile(
    plot = plot,
    plot_id = "p1",
    plot_index = 1,
    page_spec = page_spec,
    measurement_spec = list(base_size = 7, base_family = "Helvetica")
  )

  small_fit <- plotfit:::evaluate_plot_fit(profile, 20, 20, preferences)
  large_fit <- plotfit:::evaluate_plot_fit(profile, 90, 70, preferences)

  expect_gt(small_fit$total_loss, large_fit$total_loss)
  expect_gt(small_fit$hard_violation_mm, 0)
})

test_that("effective panel size reports wasted area for fixed-aspect panels", {
  profile <- list(
    geometry = list(target_panel_aspect = 1),
    page = list(width_mm = 100, height_mm = 100)
  )

  tall_panel <- plotfit:::estimate_effective_panel_size(
    profile = profile,
    panel_width_mm = 40,
    panel_height_mm = 100
  )
  square_panel <- plotfit:::estimate_effective_panel_size(
    profile = profile,
    panel_width_mm = 40,
    panel_height_mm = 40
  )

  expect_equal(tall_panel$effective_panel_width_mm, 40)
  expect_equal(tall_panel$effective_panel_height_mm, 40)
  expect_gt(tall_panel$unused_panel_area_mm2, 0)
  expect_equal(square_panel$unused_panel_area_mm2, 0)
})

test_that("minimum panel area contributes an independent physical violation", {
  preferences <- list(hard_panel_penalty = 1000)
  roomy_shape_small_area <- plotfit:::estimate_panel_minimum_loss(
    panel_width_mm = 25,
    panel_height_mm = 25,
    min_panel_width_mm = 20,
    min_panel_height_mm = 20,
    min_panel_area_mm2 = 900,
    preferences = preferences
  )
  sufficient_area <- plotfit:::estimate_panel_minimum_loss(
    panel_width_mm = 30,
    panel_height_mm = 30,
    min_panel_width_mm = 20,
    min_panel_height_mm = 20,
    min_panel_area_mm2 = 900,
    preferences = preferences
  )

  expect_gt(roomy_shape_small_area$panel_area_violation_mm, 0)
  expect_gt(roomy_shape_small_area$hard_loss, 0)
  expect_equal(sufficient_area$panel_area_violation_mm, 0)
  expect_equal(sufficient_area$hard_loss, 0)
})

test_that("gtable sizing resolves panel area at an allocated physical size", {
  device_state <- plotfit:::open_measurement_device(
    device = "pdf",
    width_in = 8.27,
    height_in = 11.69,
    base_family = "Helvetica",
    base_size = 7
  )
  on.exit(plotfit:::close_measurement_device(device_state), add = TRUE)

  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  profile <- plotfit:::measure_plot_profile(
    plot = plot,
    plot_id = "p1",
    plot_index = 1,
    page_spec = list(width_mm = 120, height_mm = 80),
    measurement_spec = list(base_size = 7, base_family = "Helvetica")
  )

  measured <- plotfit:::measure_plot_at_size(profile, 80, 50)

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

  even_gap <- plotfit:::estimate_one_axis_gap(
    axis_labels = axis_labels,
    axis_positions = list(positions = c(0, 0.5, 1), fallback = FALSE),
    available_mm = 100,
    axis = "x",
    preferences = preferences
  )
  crowded_gap <- plotfit:::estimate_one_axis_gap(
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

test_that("generic built-mark density continuously expands only preferred panel limits", {
  preferences <- list(min_panel_width_mm = 20, min_panel_height_mm = 10)
  sparse_profile <- list(
    density = list(n_points_per_panel_estimate = 500),
    geometry = list(n_nonempty_text_labels = 0), panels = list(n_panels = 1),
    plot = ggplot2::ggplot()
  )
  medium_profile <- list(
    density = list(n_points_per_panel_estimate = 1000),
    geometry = list(n_nonempty_text_labels = 0), panels = list(n_panels = 1),
    plot = ggplot2::ggplot()
  )
  dense_profile <- list(
    density = list(n_points_per_panel_estimate = 6000),
    geometry = list(n_nonempty_text_labels = 0), panels = list(n_panels = 1),
    plot = ggplot2::ggplot()
  )

  sparse <- plotfit:::adjusted_panel_limits_for_density(sparse_profile, preferences)
  medium <- plotfit:::adjusted_panel_limits_for_density(medium_profile, preferences)
  dense <- plotfit:::adjusted_panel_limits_for_density(dense_profile, preferences)

  expect_equal(sparse$density_factor, 1)
  expect_equal(medium$density_factor, 1.1)
  expect_gt(dense$density_factor, medium$density_factor)
  expect_lte(dense$density_factor, 1.5)
  expect_equal(sparse$hard_density_factor, 1)
  expect_equal(medium$hard_density_factor, 1)
  expect_equal(dense$hard_density_factor, 1)
  expect_equal(sparse$min_panel_width_mm, medium$min_panel_width_mm)
  expect_equal(medium$min_panel_width_mm, dense$min_panel_width_mm)
  expect_lt(medium$preferred_panel_width_mm, dense$preferred_panel_width_mm)
})

test_that("physical targets do not branch on geom class", {
  preferences <- list(min_panel_width_mm = 20, min_panel_height_mm = 10)
  profile_a <- list(
    density = list(n_points_per_panel_estimate = 1000),
    geometry = list(n_nonempty_text_labels = 0, geom_classes = "GeomBoxplot"),
    panels = list(n_panels = 1), plot = ggplot2::ggplot()
  )
  profile_b <- profile_a
  profile_b$geometry$geom_classes <- "CompletelyUnknownGeom"

  limits_a <- plotfit:::adjusted_panel_limits_for_density(profile_a, preferences)
  limits_b <- plotfit:::adjusted_panel_limits_for_density(profile_b, preferences)

  expect_equal(limits_a, limits_b)
})

test_that("multi-row horizontal legends create a panel-balance target", {
  device_state <- plotfit:::open_measurement_device(
    device = "pdf",
    width_in = 8.27,
    height_in = 11.69,
    base_family = "Helvetica",
    base_size = 7
  )
  on.exit(plotfit:::close_measurement_device(device_state), add = TRUE)

  groups <- paste("Arm", LETTERS[seq_len(12)])
  legend_data <- data.frame(
    x = rep(seq_len(10), times = 12),
    y = rep(seq_len(12), each = 10) + stats::rnorm(120, sd = 0.1),
    group = factor(rep(groups, each = 10), levels = groups)
  )
  plot <- ggplot2::ggplot(legend_data, ggplot2::aes(x, y, colour = group)) +
    ggplot2::geom_line() +
    ggplot2::theme(aspect.ratio = 0.7, legend.position = "bottom") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3, byrow = TRUE))
  profile <- plotfit:::measure_plot_profile(
    plot = plot,
    plot_id = "many_legend_entries",
    plot_index = 1,
    page_spec = list(width_mm = 210, height_mm = 297),
    measurement_spec = list(base_size = 7, base_family = "Helvetica")
  )

  targets <- plotfit:::horizontal_legend_panel_targets(profile)
  cramped <- plotfit:::estimate_legend_loss(profile, 210, 40, 20, 14)
  balanced <- plotfit:::estimate_legend_loss(
    profile,
    210,
    80,
    targets$width_mm,
    targets$height_mm
  )

  expect_gt(profile$geometry$legend_width_mm, 0)
  expect_gt(profile$geometry$legend_height_mm, 0)
  expect_gt(targets$width_mm, 40)
  expect_gt(targets$height_mm, 25)
  expect_gt(cramped$legend_loss, balanced$legend_loss)
})
