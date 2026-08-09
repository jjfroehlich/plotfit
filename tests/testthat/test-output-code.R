test_that("patchwork output includes a printable object and editable code", {
  plots <- list(
    p1 = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
    p2 = ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_boxplot()
  )
  page <- plotfit:::make_layout_page(names(plots), rbind(c("1", "2")))
  page$widths <- c(1.2, 0.8)
  page$heights <- 1
  page$score <- 0
  page$diagnostics <- data.frame()

  pages <- plotfit:::build_patchwork_pages(
    best_candidate = list(pages = list(page)), plots = plots,
    output_style = "design", collect_guides = FALSE, collect_axes = FALSE
  )

  expect_s3_class(pages[[1]]$patchwork, "patchwork")
  expect_match(pages[[1]]$patchwork_code, "patchwork::wrap_plots")
  expect_match(pages[[1]]$patchwork_code, "patchwork::plot_spacer")
  expect_match(pages[[1]]$patchwork_code, "clip = TRUE")
  expect_silent(parse(text = pages[[1]]$patchwork_code))
})

test_that("grid output includes physical and editable patchwork artifacts", {
  plots <- list(
    p1 = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
    p2 = ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_boxplot()
  )
  page <- plotfit:::make_layout_page(names(plots), rbind(c("1", "2")))
  page$widths <- c(1.2, 0.8)
  page$heights <- 1
  page$col_widths_mm <- c(72, 48)
  page$row_heights_mm <- 80
  page$score <- 0
  page$diagnostics <- data.frame()

  pages <- plotfit:::build_layout_pages(
    best_candidate = list(pages = list(page)), plots = plots,
    output_style = "design", collect_guides = FALSE, collect_axes = FALSE,
    layout_engine = "grid", page_spec = list(width_mm = 120, height_mm = 80),
    preferences = list(page_margin_mm = 5)
  )

  expect_s3_class(pages[[1]]$patchwork, "patchwork")
  expect_s3_class(pages[[1]]$grob, "grob")
  expect_equal(pages[[1]]$engine, "grid")
  expect_equal(pages[[1]]$col_widths_mm, c(72, 48))
  expect_invisible(plotfit::draw_layout_pages(pages))
})

test_that("grid output applies measured inner footprints as physical viewports", {
  plots <- list(p1 = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point())
  page <- plotfit:::make_layout_page(names(plots), matrix("1", 1, 1))
  page$widths <- page$heights <- 1
  page$col_widths_mm <- 100
  page$row_heights_mm <- 80
  page$score <- 0
  page$diagnostics <- data.frame(
    plot_id = "p1", allocated_width_mm = 100, allocated_height_mm = 80,
    required_width_mm = 60, required_height_mm = 40,
    footprint_measurement_reliable = TRUE, hard_loss = 0,
    stringsAsFactors = FALSE
  )

  pages <- plotfit:::build_layout_pages(
    best_candidate = list(pages = list(page)), plots = plots,
    output_style = "design", collect_guides = FALSE, collect_axes = FALSE,
    layout_engine = "grid", page_spec = list(width_mm = 100, height_mm = 80),
    preferences = list(page_margin_mm = 0, base_size = 7)
  )
  child_viewport <- pages[[1]]$grob$children[[1]]$vp

  expect_equal(pages[[1]]$inner_scales$scale_x, 0.6)
  expect_equal(pages[[1]]$inner_scales$scale_y, 0.5)
  expect_equal(as.numeric(child_viewport$width), 60)
  expect_equal(as.numeric(child_viewport$height), 40)
})

test_that("measured scaling uses independent required dimensions", {
  diagnostic <- data.frame(
    allocated_width_mm = 100, allocated_height_mm = 100,
    required_width_mm = 50, required_height_mm = 70,
    footprint_measurement_reliable = TRUE,
    stringsAsFactors = FALSE
  )
  scale <- plotfit:::infer_measured_inner_plot_scale(diagnostic)
  expect_equal(scale$scale_x, 0.5)
  expect_equal(scale$scale_y, 0.7)

  diagnostic$required_width_mm <- 75
  wider <- plotfit:::infer_measured_inner_plot_scale(diagnostic)
  expect_equal(wider$scale_x, 0.75)
  expect_equal(wider$scale_y, scale$scale_y)
})

test_that("unreliable or incomplete measurements conservatively keep full size", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  unreliable <- data.frame(
    allocated_width_mm = 100, allocated_height_mm = 100,
    required_width_mm = 40, required_height_mm = 40,
    footprint_measurement_reliable = FALSE, hard_loss = 0
  )
  incomplete <- data.frame(allocated_width_mm = 100, allocated_height_mm = 100, hard_loss = 0)
  expect_equal(plotfit:::infer_inner_plot_scale(plot, unreliable), list(scale_x = 1, scale_y = 1))
  expect_equal(plotfit:::infer_inner_plot_scale(plot, incomplete), list(scale_x = 1, scale_y = 1))
})

test_that("hard violations at the allocation disable shrinking", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  diagnostic <- data.frame(
    allocated_width_mm = 100, allocated_height_mm = 100,
    required_width_mm = 40, required_height_mm = 40,
    footprint_measurement_reliable = TRUE, hard_loss = 1
  )
  expect_equal(plotfit:::infer_inner_plot_scale(plot, diagnostic), list(scale_x = 1, scale_y = 1))
})

test_that("bounded validation enlarges only failing dimensions", {
  fit_function <- function(width_mm, height_mm) {
    width_violation <- max(0, 60 - width_mm)
    height_violation <- max(0, 25 - height_mm)
    list(
      hard_violation_mm = max(width_violation, height_violation),
      x_label_violation_mm = width_violation,
      y_label_violation_mm = height_violation,
      panel_width_violation_mm = 0, panel_height_violation_mm = 0,
      facet_panel_width_violation_mm = 0, facet_panel_height_violation_mm = 0,
      footprint_width_violation_mm = width_violation,
      footprint_height_violation_mm = height_violation,
      inner_footprint_width_violation_mm = width_violation,
      inner_footprint_height_violation_mm = height_violation
    )
  }
  validated <- plotfit:::validate_inner_plot_scale(
    list(scale_x = 0.4, scale_y = 0.5),
    data.frame(allocated_width_mm = 100, allocated_height_mm = 100),
    fit_function, max_iterations = 4
  )
  expect_equal(validated$status, "validated")
  expect_gt(validated$scale_x, 0.4)
  expect_equal(validated$scale_y, 0.5)
  expect_lte(validated$iterations, 4)
})

test_that("bounded validation enlarges both dimensions for aspect-constrained panels", {
  fit_function <- function(width_mm, height_mm) {
    violation <- max(0, 60 - min(width_mm, height_mm))
    list(
      hard_violation_mm = violation,
      x_label_violation_mm = 0, y_label_violation_mm = 0,
      panel_width_violation_mm = violation, panel_height_violation_mm = 0,
      facet_panel_width_violation_mm = 0, facet_panel_height_violation_mm = 0,
      footprint_width_violation_mm = violation,
      footprint_height_violation_mm = 0,
      inner_footprint_width_violation_mm = violation,
      inner_footprint_height_violation_mm = 0,
      target_panel_aspect = 1
    )
  }
  validated <- plotfit:::validate_inner_plot_scale(
    list(scale_x = 0.4, scale_y = 0.4),
    data.frame(allocated_width_mm = 100, allocated_height_mm = 100),
    fit_function, max_iterations = 4
  )
  expect_equal(validated$status, "validated")
  expect_gt(validated$scale_x, 0.4)
  expect_gt(validated$scale_y, 0.4)
})

test_that("failed bounded validation falls back to the full footprint", {
  failing_fit <- function(width_mm, height_mm) list(
    hard_violation_mm = 1, x_label_violation_mm = 1, y_label_violation_mm = 1,
    panel_width_violation_mm = 0, panel_height_violation_mm = 0,
    facet_panel_width_violation_mm = 0, facet_panel_height_violation_mm = 0,
    footprint_width_violation_mm = 1, footprint_height_violation_mm = 1,
    inner_footprint_width_violation_mm = 1, inner_footprint_height_violation_mm = 1
  )
  result <- plotfit:::validate_inner_plot_scale(
    list(scale_x = 0.2, scale_y = 0.2),
    data.frame(allocated_width_mm = 100, allocated_height_mm = 100),
    failing_fit, max_iterations = 4
  )
  expect_equal(result$scale_x, 1)
  expect_equal(result$scale_y, 1)
  expect_equal(result$status, "fallback_full_size")
  expect_equal(result$iterations, 4)
})

test_that("inner scaling is independent of geom class", {
  point_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  line_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_line()
  diagnostic <- data.frame(
    allocated_width_mm = 120, allocated_height_mm = 100,
    required_width_mm = 60, required_height_mm = 75,
    footprint_measurement_reliable = TRUE, hard_loss = 0
  )
  expect_equal(
    plotfit:::infer_inner_plot_scale(point_plot, diagnostic),
    plotfit:::infer_inner_plot_scale(line_plot, diagnostic)
  )
})

test_that("generated patchwork code contains optimizer-selected measured scales", {
  code <- plotfit:::format_patchwork_code(
    plot_ids = "p1",
    areas = data.frame(symbol = "1", plot_id = "p1", t = 1, l = 1, b = 1, r = 1),
    layout_string = "1", widths = 1, heights = 1,
    output_style = "design", collect_guides = FALSE, collect_axes = FALSE,
    page_margin_mm = 0,
    plot_scales = data.frame(plot_id = "p1", scale_x = 0.6, scale_y = 0.8),
    base_size = 7
  )
  expect_match(code, "scale_plot\\(p1, 0.6, 0.8")
  expect_silent(parse(text = code))
})
