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
