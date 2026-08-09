make_footprint_profile <- function(
    x_widths = c(4, 4, 4),
    y_heights = c(4, 4, 4),
    x_rotation = 0,
    x_positions = c(0, 0.5, 1),
    y_positions = c(0, 0.5, 1),
    panel_rows = 1,
    panel_cols = 1,
    target_aspect = NA_real_,
    outer_title_width = 0) {
  text_grobs <- rbind(
    data.frame(
      component_type = "axis_b", component_name = "axis-b-1",
      text = paste0("x", seq_along(x_widths)), width_mm = x_widths,
      height_mm = rep(3, length(x_widths)), rotation = rep(x_rotation, length(x_widths)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      component_type = "axis_l", component_name = "axis-l-1",
      text = paste0("y", seq_along(y_heights)), width_mm = rep(3, length(y_heights)),
      height_mm = y_heights, rotation = 0,
      stringsAsFactors = FALSE
    )
  )
  if (outer_title_width > 0) {
    text_grobs <- rbind(text_grobs, data.frame(
      component_type = "title", component_name = "title",
      text = "title", width_mm = outer_title_width, height_mm = 4, rotation = 0,
      stringsAsFactors = FALSE
    ))
  }
  list(
    text_grobs = text_grobs,
    axis_positions = list(
      x = list(positions = x_positions, fallback = FALSE),
      y = list(positions = y_positions, fallback = FALSE)
    ),
    panels = list(
      n_panel_rows = panel_rows, n_panel_cols = panel_cols,
      n_panels = panel_rows * panel_cols
    ),
    density = list(n_points_per_panel_estimate = 20),
    geometry = list(n_nonempty_text_labels = 0, target_panel_aspect = target_aspect),
    component_sizes = data.frame(type = character(), width_mm = numeric(), height_mm = numeric()),
    plot = ggplot2::ggplot()
  )
}

footprint_preferences <- function() list(
  min_label_gap_mm = 1.5,
  min_panel_width_mm = 30,
  min_panel_height_mm = 20
)

test_that("longer x labels increase only the horizontal axis requirement", {
  short <- make_footprint_profile(x_widths = c(4, 4, 4))
  long <- make_footprint_profile(x_widths = c(14, 14, 14))
  expect_gt(
    plotfit:::required_axis_panel_span_mm(long, "x", 1.5),
    plotfit:::required_axis_panel_span_mm(short, "x", 1.5)
  )
  expect_equal(
    plotfit:::required_axis_panel_span_mm(long, "y", 1.5),
    plotfit:::required_axis_panel_span_mm(short, "y", 1.5)
  )
  footprint <- plotfit:::estimate_required_plot_footprint(
    long,
    list(left_mm = 2, right_mm = 2, top_mm = 2, bottom_mm = 2),
    footprint_preferences()
  )
  expect_identical(footprint$width_limiting_constraint, "axis_labels")
})

test_that("rotation and irregular positions affect measured label requirements", {
  ordinary <- make_footprint_profile(x_widths = c(2, 2, 2), x_rotation = 0)
  rotated <- make_footprint_profile(x_widths = c(2, 2, 2), x_rotation = 90)
  crowded <- make_footprint_profile(x_widths = c(4, 4, 4), x_positions = c(0, 0.05, 1))
  expect_gt(
    plotfit:::required_axis_panel_span_mm(rotated, "x", 1.5),
    plotfit:::required_axis_panel_span_mm(ordinary, "x", 1.5)
  )
  expect_gt(
    plotfit:::required_axis_panel_span_mm(crowded, "x", 1.5),
    plotfit:::required_axis_panel_span_mm(ordinary, "x", 1.5)
  )
})

test_that("facet rows and columns increase the corresponding required dimension", {
  preferences <- footprint_preferences()
  one <- plotfit:::estimate_required_plot_footprint(
    make_footprint_profile(), list(left_mm = 5, right_mm = 2, top_mm = 3, bottom_mm = 4), preferences
  )
  columns <- plotfit:::estimate_required_plot_footprint(
    make_footprint_profile(panel_cols = 3), list(left_mm = 5, right_mm = 2, top_mm = 3, bottom_mm = 4), preferences
  )
  rows <- plotfit:::estimate_required_plot_footprint(
    make_footprint_profile(panel_rows = 3), list(left_mm = 5, right_mm = 2, top_mm = 3, bottom_mm = 4), preferences
  )
  expect_gt(columns$required_width_mm, one$required_width_mm)
  expect_equal(columns$required_height_mm, one$required_height_mm)
  expect_gt(rows$required_height_mm, one$required_height_mm)
  expect_equal(rows$required_width_mm, one$required_width_mm)
})

test_that("explicit aspect and intrinsic outer text constrain the whole footprint", {
  preferences <- footprint_preferences()
  base <- plotfit:::estimate_required_plot_footprint(
    make_footprint_profile(), list(left_mm = 2, right_mm = 2, top_mm = 2, bottom_mm = 2), preferences
  )
  aspect <- plotfit:::estimate_required_plot_footprint(
    make_footprint_profile(target_aspect = 2),
    list(left_mm = 2, right_mm = 2, top_mm = 2, bottom_mm = 2), preferences
  )
  title <- plotfit:::estimate_required_plot_footprint(
    make_footprint_profile(outer_title_width = 90),
    list(left_mm = 2, right_mm = 2, top_mm = 2, bottom_mm = 2), preferences
  )
  expect_gt(aspect$required_height_mm, base$required_height_mm)
  expect_gte(title$required_width_mm, 90)
  expect_true(title$measurement_reliable)
})

test_that("legend envelopes include a conservative measured safety margin", {
  profile <- make_footprint_profile()
  profile$component_sizes <- data.frame(
    type = "legend", width_mm = 50, height_mm = 10,
    stringsAsFactors = FALSE
  )
  footprint <- plotfit:::estimate_required_plot_footprint(
    profile,
    list(left_mm = 2, right_mm = 2, top_mm = 2, bottom_mm = 2),
    footprint_preferences()
  )
  expect_gte(footprint$required_width_mm, 58)
  expect_gte(footprint$required_height_mm, 14)
})

test_that("generic built-coordinate measurements distinguish content burdens", {
  grid_data <- expand.grid(x = seq_len(8), y = seq_len(6))
  grid_plot <- ggplot2::ggplot(grid_data, ggplot2::aes(x, y)) + ggplot2::geom_tile()
  grid_geometry <- plotfit:::estimate_content_geometry(ggplot2::ggplot_build(grid_plot))
  expect_gte(grid_geometry$regular_grid_score, 0.99)

  detail_plot <- ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = 0.1)
  detail_geometry <- plotfit:::estimate_content_geometry(ggplot2::ggplot_build(detail_plot))
  expect_gt(detail_geometry$summary_detail_score, 0.9)
  expect_gt(detail_geometry$max_glyph_rows_per_panel, 0)

  line_plot <- ggplot2::ggplot(data.frame(x = 1:20, y = cumsum(rep(1, 20))), ggplot2::aes(x, y)) +
    ggplot2::geom_line()
  line_geometry <- plotfit:::estimate_content_geometry(ggplot2::ggplot_build(line_plot))
  expect_equal(line_geometry$has_trajectory_content, 1)
  expect_equal(line_geometry$max_glyph_rows_per_panel, 0)
})

test_that("regular-grid cell text uses measured panel-label extents", {
  make_cell_text_profile <- function(labels) {
    grid_data <- expand.grid(x = seq_len(4), y = seq_len(3))
    grid_data$label <- labels
    plot <- ggplot2::ggplot(grid_data, ggplot2::aes(x, y)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = label), size = 2)
    built <- ggplot2::ggplot_build(plot)
    gt <- ggplot2::ggplotGrob(plot)
    components <- plotfit:::extract_gtable_components(gt)
    panel_text <- plotfit:::collect_panel_text_grobs(gt, components)
    geometry <- plotfit:::estimate_content_geometry(built, panel_text_grobs = panel_text)
    profile <- make_footprint_profile()
    profile$geometry <- utils::modifyList(profile$geometry, geometry)
    profile$geometry$n_nonempty_text_labels <- nrow(grid_data)
    profile
  }

  short <- make_cell_text_profile(rep("1", 12))
  long <- make_cell_text_profile(rep("long", 12))
  short_requirement <- plotfit:::required_regular_grid_cell_text_span_mm(short, 1)
  long_requirement <- plotfit:::required_regular_grid_cell_text_span_mm(long, 1)

  expect_gt(long$geometry$max_panel_text_width_mm, short$geometry$max_panel_text_width_mm)
  expect_gt(long_requirement$width_mm, short_requirement$width_mm)
  expect_equal(long_requirement$height_mm, short_requirement$height_mm, tolerance = 0.1)
})

test_that("continuous content preferences act independently by dimension", {
  base <- make_footprint_profile()
  base$density$n_points_per_panel_estimate <- 48
  base$geometry <- c(base$geometry, list(
    max_unique_x_per_panel = 10,
    max_unique_y_per_panel = 10,
    max_layer_rows_per_panel = 48,
    max_glyph_rows_per_panel = 0,
    total_glyph_rows = 0,
    max_groups_per_panel = 1,
    has_trajectory_content = 1,
    regular_grid_score = 0,
    summary_detail_score = 0,
    bounded_layer_fraction = 0
  ))
  resolved <- plotfit:::estimate_inner_content_multipliers(base)

  long_x <- base
  long_x$geometry$max_unique_x_per_panel <- 80
  horizontal <- plotfit:::estimate_inner_content_multipliers(long_x)
  expect_gt(horizontal$width_multiplier, resolved$width_multiplier)
  expect_gt(horizontal$height_multiplier, resolved$height_multiplier)

  dense_x <- long_x
  dense_x$geometry$max_layer_rows_per_panel <- 5000
  dense <- plotfit:::estimate_inner_content_multipliers(dense_x)
  expect_lt(dense$height_multiplier, horizontal$height_multiplier)

  faceted <- base
  faceted$panels <- list(n_panel_rows = 2, n_panel_cols = 3, n_panels = 6)
  faceted_height <- plotfit:::estimate_inner_content_multipliers(faceted)
  expect_gt(faceted_height$height_multiplier, resolved$height_multiplier)

  summary_detail <- base
  summary_detail$geometry$summary_detail_score <- 1
  summary_detail$geometry$legend_width_mm <- 50
  detail <- plotfit:::estimate_inner_content_multipliers(summary_detail)
  expect_gt(detail$width_multiplier, resolved$width_multiplier)
  expect_gt(detail$height_multiplier, resolved$height_multiplier)
})

test_that("annotation, glyph, and many-panel preferences use generic measurements", {
  base <- make_footprint_profile()
  base$fixed_size <- list(left_mm = 5, right_mm = 2, top_mm = 5, bottom_mm = 8)
  base$geometry <- c(base$geometry, list(
    max_unique_x_per_panel = 12,
    max_unique_y_per_panel = 12,
    max_layer_rows_per_panel = 144,
    max_glyph_rows_per_panel = 0,
    total_glyph_rows = 0,
    max_groups_per_panel = 1,
    has_trajectory_content = 0,
    regular_grid_score = 1,
    summary_detail_score = 0,
    bounded_layer_fraction = 0.5
  ))

  unlabelled <- plotfit:::estimate_inner_content_multipliers(base)
  annotated_profile <- base
  annotated_profile$geometry$n_nonempty_text_labels <- 144
  annotated <- plotfit:::estimate_inner_content_multipliers(annotated_profile)
  expect_gt(annotated$width_multiplier, unlabelled$width_multiplier)
  expect_gt(annotated$height_multiplier, unlabelled$height_multiplier)

  smaller_annotated_grid <- annotated_profile
  smaller_annotated_grid$geometry$max_unique_x_per_panel <- 6
  smaller_annotated_grid$geometry$max_unique_y_per_panel <- 6
  smaller_annotated_grid$geometry$max_layer_rows_per_panel <- 36
  smaller_annotated_grid$geometry$n_nonempty_text_labels <- 36
  smaller_annotated <- plotfit:::estimate_inner_content_multipliers(smaller_annotated_grid)
  expect_lt(smaller_annotated$width_multiplier, annotated$width_multiplier)
  expect_lt(smaller_annotated$height_multiplier, annotated$height_multiplier)

  glyph_profile <- base
  glyph_profile$geometry$regular_grid_score <- 0
  glyph_profile$geometry$bounded_layer_fraction <- 0
  glyph_profile$geometry$max_glyph_rows_per_panel <- 80
  glyph_profile$geometry$total_glyph_rows <- 80
  glyph <- plotfit:::estimate_inner_content_multipliers(glyph_profile)
  expect_gt(glyph$width_multiplier, 1)
  expect_gt(glyph$height_multiplier, 1)

  labelled_glyph_profile <- glyph_profile
  labelled_glyph_profile$geometry$n_nonempty_text_labels <- 8
  labelled_glyph_profile$geometry$max_panel_text_width_mm <- 14
  labelled_glyph <- plotfit:::estimate_inner_content_multipliers(labelled_glyph_profile)
  expect_gt(labelled_glyph$width_multiplier, glyph$width_multiplier)
  expect_gt(labelled_glyph$height_multiplier, glyph$height_multiplier)

  many_panels <- glyph_profile
  many_panels$panels <- list(n_panel_rows = 7, n_panel_cols = 5, n_panels = 35)
  many_panels$geometry$total_glyph_rows <- 1800
  compact_facets <- plotfit:::estimate_inner_content_multipliers(many_panels)
  expect_lt(compact_facets$width_multiplier, glyph$width_multiplier)

  large_unlabelled_grid <- base
  large_unlabelled_grid$geometry$max_layer_rows_per_panel <- 352
  large_unlabelled_grid$geometry$max_unique_x_per_panel <- 16
  large_unlabelled_grid$geometry$max_unique_y_per_panel <- 22
  large_unlabelled <- plotfit:::estimate_inner_content_multipliers(large_unlabelled_grid)
  expect_gte(large_unlabelled$width_multiplier, 0.65)
  expect_gte(large_unlabelled$height_multiplier, 0.65)

  dense_cloud <- glyph_profile
  dense_cloud$geometry$max_glyph_rows_per_panel <- 18000
  dense_cloud$geometry$total_glyph_rows <- 18000
  dense_cloud_multiplier <- plotfit:::estimate_inner_content_multipliers(dense_cloud)
  expect_gte(dense_cloud_multiplier$width_multiplier, 2)
  expect_gte(dense_cloud_multiplier$height_multiplier, 1.1)

  faceted_grid <- base
  faceted_grid$panels <- list(n_panel_rows = 1, n_panel_cols = 2, n_panels = 2)
  faceted_grid$geometry$max_layer_rows_per_panel <- 120
  faceted_grid_multiplier <- plotfit:::estimate_inner_content_multipliers(faceted_grid)
  expect_gte(faceted_grid_multiplier$width_multiplier, 1.2)

  high_bottom_burden <- base
  high_bottom_burden$fixed_size$bottom_mm <- 21
  high_bottom_burden$geometry$max_unique_x_per_panel <- 5
  burdened <- plotfit:::estimate_inner_content_multipliers(high_bottom_burden)
  expect_gt(burdened$width_multiplier, unlabelled$width_multiplier)
  expect_gt(burdened$height_multiplier, unlabelled$height_multiplier)

  few_x_dense_y <- base
  few_x_dense_y$geometry$regular_grid_score <- 0.14
  few_x_dense_y$geometry$bounded_layer_fraction <- 1
  few_x_dense_y$geometry$max_unique_x_per_panel <- 7
  few_x_dense_y$geometry$max_unique_y_per_panel <- 3500
  few_x_dense_y$geometry$max_layer_rows_per_panel <- 3500
  distribution <- plotfit:::estimate_inner_content_multipliers(few_x_dense_y)
  expect_gt(distribution$height_multiplier, unlabelled$height_multiplier)
})
