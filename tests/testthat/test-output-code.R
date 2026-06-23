test_that("patchwork output includes a printable object and editable code", {
  plots <- list(
    p1 = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
    p2 = ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_boxplot()
  )
  layout_matrix <- rbind(c("1", "2"))
  page <- plotfit:::make_layout_page(names(plots), layout_matrix)
  page$widths <- c(1.2, 0.8)
  page$heights <- 1
  page$score <- 0
  page$diagnostics <- data.frame()

  pages <- plotfit:::build_patchwork_pages(
    best_candidate = list(pages = list(page)),
    plots = plots,
    output_style = "design",
    collect_guides = FALSE,
    collect_axes = FALSE
  )

  expect_s3_class(pages[[1]]$patchwork, "patchwork")
  expect_match(pages[[1]]$patchwork_code, "patchwork::wrap_plots")
  expect_match(pages[[1]]$patchwork_code, "widths = c")
})

test_that("grid output includes a drawable grob and physical dimensions", {
  plots <- list(
    p1 = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
    p2 = ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_boxplot()
  )
  layout_matrix <- rbind(c("1", "2"))
  page <- plotfit:::make_layout_page(names(plots), layout_matrix)
  page$widths <- c(1.2, 0.8)
  page$heights <- 1
  page$col_widths_mm <- c(72, 48)
  page$row_heights_mm <- 80
  page$score <- 0
  page$diagnostics <- data.frame()

  pages <- plotfit:::build_layout_pages(
    best_candidate = list(pages = list(page)),
    plots = plots,
    output_style = "design",
    collect_guides = FALSE,
    collect_axes = FALSE,
    layout_engine = "grid",
    page_spec = list(width_mm = 120, height_mm = 80),
    preferences = list(page_margin_mm = 5)
  )

  expect_null(pages[[1]]$patchwork)
  expect_s3_class(pages[[1]]$grob, "grob")
  expect_equal(pages[[1]]$engine, "grid")
  expect_equal(pages[[1]]$col_widths_mm, c(72, 48))
  expect_equal(pages[[1]]$content_width_mm, 120)
  expect_invisible(plotfit::draw_layout_pages(pages))
})

test_that("grid output applies automatic inner footprints as physical viewports", {
  plots <- list(
    p1 = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      ggplot2::theme(aspect.ratio = 1)
  )
  layout_matrix <- matrix("1", nrow = 1, ncol = 1)
  page <- plotfit:::make_layout_page(names(plots), layout_matrix)
  page$widths <- 1
  page$heights <- 1
  page$col_widths_mm <- 100
  page$row_heights_mm <- 80
  page$score <- 0
  page$diagnostics <- data.frame(
    plot_id = "p1",
    allocated_width_mm = 100,
    allocated_height_mm = 80,
    preferred_width_mm = 100,
    preferred_height_mm = 80,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  pages <- plotfit:::build_layout_pages(
    best_candidate = list(pages = list(page)),
    plots = plots,
    output_style = "design",
    collect_guides = FALSE,
    collect_axes = FALSE,
    layout_engine = "grid",
    page_spec = list(width_mm = 100, height_mm = 80),
    preferences = list(page_margin_mm = 0, base_size = 7)
  )

  child_viewport <- pages[[1]]$grob$children[[1]]$vp

  expect_equal(pages[[1]]$inner_scales$scale_x, 2 / 3)
  expect_equal(pages[[1]]$inner_scales$scale_y, 0.5)
  expect_equal(as.numeric(child_viewport$width), 100 * 2 / 3)
  expect_equal(as.numeric(child_viewport$height), 80 * 0.5)
})

test_that("inner plot scale is inferred from measured selected footprint", {
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 50,
    preferred_height_mm = 70,
    fixed_width_mm = 20,
    fixed_height_mm = 10,
    panel_width_mm = 80,
    panel_height_mm = 90,
    effective_panel_width_mm = 40,
    effective_panel_height_mm = 90,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_measured_inner_plot_scale(diagnostic)

  expect_equal(scale$scale_x, 0.6)
  expect_equal(scale$scale_y, 1)
})

test_that("hard violations disable inner shrinking", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::theme(aspect.ratio = 1)
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 50,
    preferred_height_mm = 50,
    hard_loss = 1,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 1)
  expect_equal(scale$scale_y, 1)
})

test_that("structural compactness caps measured inner scaling", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, label = rownames(mtcars))) +
    ggplot2::geom_text(size = 2) +
    ggplot2::theme(aspect.ratio = 1)
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 0.5)
  expect_equal(scale$scale_y, 0.5)
})

test_that("facets and rotated labels add general axis-specific compactness caps", {
  faceted <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~cyl)
  rotated <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  facet_scale <- plotfit:::infer_inner_plot_scale(faceted, diagnostic)
  rotated_scale <- plotfit:::infer_inner_plot_scale(rotated, diagnostic)

  expect_equal(facet_scale$scale_x, 0.8)
  expect_equal(facet_scale$scale_y, 2 / 3)
  expect_equal(rotated_scale$scale_x, 4 / 9)
  expect_equal(rotated_scale$scale_y, 2 / 3)
})

test_that("faceted count plots and line plots get compact general footprints", {
  faceted_histogram <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, fill = factor(cyl))) +
    ggplot2::geom_histogram(binwidth = 5) +
    ggplot2::facet_wrap(~gear)
  line_plot <- ggplot2::ggplot(ggplot2::economics, ggplot2::aes(date, unemploy)) +
    ggplot2::geom_line()
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  facet_scale <- plotfit:::infer_inner_plot_scale(faceted_histogram, diagnostic)
  line_scale <- plotfit:::infer_inner_plot_scale(line_plot, diagnostic)

  expect_equal(facet_scale$scale_x, 2 / 3)
  expect_equal(facet_scale$scale_y, 0.5)
  expect_equal(line_scale$scale_x, 2 / 3)
  expect_equal(line_scale$scale_y, 0.5)
})

test_that("simple fixed-aspect plots get compacted in both dimensions", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::theme(aspect.ratio = 1)
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 2 / 3)
  expect_equal(scale$scale_y, 0.5)
})

test_that("right-side legends do not trigger the strongest fixed-aspect compactness cap", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::theme(aspect.ratio = 1, legend.position = "right")
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 2 / 3)
  expect_equal(scale$scale_y, 2 / 3)
})

test_that("bottom legends on fixed-aspect plots keep a readable compact footprint", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    ggplot2::theme(aspect.ratio = 1, legend.position = "bottom")
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 0.5)
  expect_equal(scale$scale_y, 0.5)
})

test_that("low-aspect diagnostics get width caps based on density pressure", {
  simple <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme(aspect.ratio = 0.5)
  dense <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme(aspect.ratio = 0.5)
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    data_density_loss = 0,
    warning = "",
    stringsAsFactors = FALSE
  )
  dense_diagnostic <- diagnostic
  dense_diagnostic$data_density_loss <- 1

  simple_scale <- plotfit:::infer_inner_plot_scale(simple, diagnostic)
  dense_scale <- plotfit:::infer_inner_plot_scale(dense, dense_diagnostic)

  expect_equal(simple_scale$scale_x, 0.5)
  expect_equal(simple_scale$scale_y, 1)
  expect_equal(dense_scale$scale_x, 0.8)
  expect_equal(dense_scale$scale_y, 1)
})
