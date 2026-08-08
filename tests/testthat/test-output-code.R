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
  expect_match(pages[[1]]$patchwork_code, "patchwork::plot_spacer")
  expect_match(pages[[1]]$patchwork_code, "clip = TRUE")
  expect_match(pages[[1]]$patchwork_code, "widths = c")
  expect_silent(parse(text = pages[[1]]$patchwork_code))
})

test_that("grid output includes a drawable grob, physical dimensions, and editable patchwork artifacts", {
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

  expect_s3_class(pages[[1]]$patchwork, "patchwork")
  expect_type(pages[[1]]$patchwork_code, "character")
  expect_gt(nchar(pages[[1]]$patchwork_code), 0)
  expect_match(pages[[1]]$patchwork_code, "patchwork::wrap_plots")
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
  expect_equal(pages[[1]]$inner_scales$scale_y, 2 / 3)
  expect_equal(as.numeric(child_viewport$width), 100 * 2 / 3)
  expect_equal(as.numeric(child_viewport$height), 80 * 2 / 3)
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

test_that("ordinary annotated plots keep their measured full footprint", {
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

  expect_equal(scale$scale_x, 1)
  expect_equal(scale$scale_y, 1)
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

  expect_equal(facet_scale$scale_x, 1)
  expect_equal(facet_scale$scale_y, 0.5)
  expect_equal(rotated_scale$scale_x, 2 / 3)
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

  expect_equal(facet_scale$scale_x, 0.5)
  expect_equal(facet_scale$scale_y, 0.5)
  expect_equal(line_scale$scale_x, 2 / 3)
  expect_equal(line_scale$scale_y, 0.5)
})

test_that("simple fixed-aspect point plots use a compact square footprint", {
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
  expect_equal(scale$scale_y, 2 / 3)
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

  expect_equal(scale$scale_x, 1)
  expect_equal(scale$scale_y, 1)
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

  expect_equal(scale$scale_x, 2 / 3)
  expect_equal(scale$scale_y, 2 / 3)
})

test_that("line plots with horizontal legends use measured legend bounds", {
  plot <- ggplot2::ggplot(
    ggplot2::economics,
    ggplot2::aes(date, unemploy, colour = factor(round(date, "year")))
  ) +
    ggplot2::geom_line() +
    ggplot2::theme(aspect.ratio = 0.7, legend.position = "bottom")
  diagnostic <- data.frame(
    allocated_width_mm = c(100, 210),
    allocated_height_mm = c(100, 72.4),
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    legend_width_mm = c(NA_real_, 96),
    legend_height_mm = c(NA_real_, 9.4),
    stringsAsFactors = FALSE
  )

  unmeasured <- plotfit:::infer_inner_plot_scale(plot, diagnostic[1, , drop = FALSE])
  multirow <- plotfit:::infer_inner_plot_scale(plot, diagnostic[2, , drop = FALSE])

  expect_equal(unmeasured$scale_x, 2 / 3)
  expect_equal(unmeasured$scale_y, 0.5)
  expect_equal(multirow$scale_x, 100 / 210)
  expect_equal(multirow$scale_y, 17.4 / 72.4)
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

  expect_equal(simple_scale$scale_x, 1)
  expect_equal(simple_scale$scale_y, 1)
  expect_equal(dense_scale$scale_x, 1)
  expect_equal(dense_scale$scale_y, 1)
})

test_that("sparse annotations use a stable compact footprint", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, label = rownames(mtcars))) +
    ggplot2::geom_point() +
    ggplot2::geom_text() +
    ggplot2::theme(aspect.ratio = 1)
  diagnostic <- data.frame(
    allocated_width_mm = c(105, 70),
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    n_nonempty_text_labels = c(7, 8),
    stringsAsFactors = FALSE
  )

  sparse_scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic[1, , drop = FALSE])
  labelled_scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic[2, , drop = FALSE])

  expect_equal(sparse_scale$scale_x, 2 / 3)
  expect_equal(sparse_scale$scale_y, 2 / 3)
  expect_equal(labelled_scale$scale_x, 2 / 3)
  expect_equal(labelled_scale$scale_y, 2 / 3)
})

test_that("ribbon lines and rotated faceted heatmaps get distinct compact footprints", {
  ribbon_line <- ggplot2::ggplot(
    ggplot2::economics,
    ggplot2::aes(date, unemploy)
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = unemploy - 100, ymax = unemploy + 100)) +
    ggplot2::geom_line()
  faceted_heatmap <- ggplot2::ggplot(
    expand.grid(x = letters[1:6], y = letters[1:4], panel = c("A", "B")),
    ggplot2::aes(x, y, fill = as.numeric(x))
  ) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~panel) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  ribbon_scale <- plotfit:::infer_inner_plot_scale(ribbon_line, diagnostic)
  heatmap_scale <- plotfit:::infer_inner_plot_scale(faceted_heatmap, diagnostic)

  expect_equal(ribbon_scale$scale_x, 4 / 9)
  expect_equal(ribbon_scale$scale_y, 0.5)
  expect_equal(heatmap_scale$scale_x, 4 / 9)
  expect_equal(heatmap_scale$scale_y, 0.75)
})

test_that("dense faceted heatmaps grow continuously for legible cells", {
  faceted_heatmap <- ggplot2::ggplot(
    expand.grid(x = letters[1:10], y = letters[1:12], panel = c("A", "B")),
    ggplot2::aes(x, y, fill = as.numeric(x))
  ) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~panel) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  diagnostic <- data.frame(
    hard_loss = 0,
    n_marks_per_panel = 120,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(faceted_heatmap, diagnostic)

  expect_equal(scale$scale_x, 5 / 9)
  expect_equal(scale$scale_y, 0.9)
})

test_that("faceted line plots compact with panel count", {
  faceted_line <- ggplot2::ggplot(
    expand.grid(x = seq_len(10), panel = factor(seq_len(4))),
    ggplot2::aes(x, x)
  ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~panel, ncol = 2)
  diagnostic <- data.frame(
    hard_loss = 0,
    n_panels = 4,
    label_gap_loss = 0.08,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(faceted_line, diagnostic)

  expect_equal(scale$scale_x, 0.9)
  expect_equal(scale$scale_y, 0.9)
})

test_that("sparse rotated bars use measured mark count on both axes", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_col() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    n_marks_per_panel = c(5, 20),
    stringsAsFactors = FALSE
  )

  sparse_scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic[1, , drop = FALSE])
  stacked_scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic[2, , drop = FALSE])

  expect_equal(sparse_scale$scale_x, 1 / 3)
  expect_equal(sparse_scale$scale_y, 2 / 3)
  expect_equal(stacked_scale$scale_x, 4 / 9)
  expect_equal(stacked_scale$scale_y, 1)
})

test_that("unresolvable rotated-label violations use a compact fallback", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_col() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1))
  diagnostic <- data.frame(
    allocated_width_mm = 200,
    allocated_height_mm = 160,
    hard_loss = 1000,
    label_gap_loss = 1000,
    panel_minimum_loss = 0,
    facet_loss = 0,
    n_marks_per_panel = 18,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 0.625)
  expect_equal(scale$scale_y, 0.5)
})

test_that("portrait facet grids trade width for additional height", {
  facet_data <- expand.grid(
    x = seq_len(10),
    y = seq_len(7),
    panel_row = factor(seq_len(7)),
    panel_col = factor(seq_len(5))
  )
  plot <- ggplot2::ggplot(facet_data, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(panel_row ~ panel_col)
  diagnostic <- data.frame(
    hard_loss = 0,
    n_panel_rows = 7,
    n_panel_cols = 5,
    n_panels = 35,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 1 / 3)
  expect_equal(scale$scale_y, 0.75)
})

test_that("interval and reference-line point plots get distinct footprints", {
  interval_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg, ymin = mpg - 1, ymax = mpg + 1)) +
    ggplot2::geom_pointrange()
  diagnostic_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_hline(yintercept = 20) +
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

  interval_scale <- plotfit:::infer_inner_plot_scale(interval_plot, diagnostic)
  reference_scale <- plotfit:::infer_inner_plot_scale(diagnostic_plot, diagnostic)

  expect_equal(interval_scale$scale_x, 0.75)
  expect_equal(interval_scale$scale_y, 2 / 3)
  expect_equal(reference_scale$scale_x, 0.5)
  expect_equal(reference_scale$scale_y, 0.5)
})

test_that("long-label intervals preserve a compact visible data panel", {
  interval_data <- data.frame(
    endpoint = factor(paste("Long endpoint", seq_len(14))),
    estimate = seq_len(14),
    low = seq_len(14) - 1,
    high = seq_len(14) + 1
  )
  plot <- ggplot2::ggplot(
    interval_data,
    ggplot2::aes(endpoint, estimate, ymin = low, ymax = high)
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip()
  diagnostic <- data.frame(
    hard_loss = 0,
    n_marks_per_panel = 14,
    fixed_width_mm = 55,
    effective_panel_width_mm = 150,
    allocated_width_mm = 210,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 70 / 210)
  expect_equal(scale$scale_y, 0.75)
})

test_that("simple distributions compact horizontally while dense points keep space", {
  box_jitter <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter() +
    ggplot2::theme(aspect.ratio = 0.5)
  dense_scatter <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::theme(aspect.ratio = 0.65)
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 50,
    preferred_height_mm = 50,
    hard_loss = 0,
    n_point_marks_per_panel = NA_real_,
    stringsAsFactors = FALSE
  )
  dense_diagnostic <- diagnostic
  dense_diagnostic$n_point_marks_per_panel <- 6000

  distribution_scale <- plotfit:::infer_inner_plot_scale(box_jitter, diagnostic)
  dense_scale <- plotfit:::infer_inner_plot_scale(dense_scatter, dense_diagnostic)

  expect_equal(distribution_scale$scale_x, 0.5)
  expect_equal(distribution_scale$scale_y, 1)
  expect_equal(dense_scale$scale_x, 1)
  expect_equal(dense_scale$scale_y, 1)
})

test_that("simple distribution width responds to measured label pressure", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point()
  diagnostic <- data.frame(
    hard_loss = 0,
    label_gap_loss = 2,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 0.625)
  expect_equal(scale$scale_y, 1)
})

test_that("saturated fixed-aspect point clouds shrink after useful density", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::theme(aspect.ratio = 1)
  diagnostic <- data.frame(
    hard_loss = 0,
    n_point_marks_per_panel = 18000,
    stringsAsFactors = FALSE
  )

  scale <- plotfit:::infer_inner_plot_scale(plot, diagnostic)

  expect_equal(scale$scale_x, 1 / 3)
  expect_equal(scale$scale_y, 1 / 3)
})

test_that("unannotated heatmaps and density curves use compact general footprints", {
  heatmap <- ggplot2::ggplot(
    expand.grid(x = seq_len(10), y = seq_len(12)),
    ggplot2::aes(x, y, fill = x + y)
  ) +
    ggplot2::geom_tile()
  density <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, colour = Species)) +
    ggplot2::geom_density() +
    ggplot2::theme(legend.position = "bottom")
  diagnostic <- data.frame(hard_loss = 0, stringsAsFactors = FALSE)

  heatmap_scale <- plotfit:::infer_inner_plot_scale(heatmap, diagnostic)
  density_scale <- plotfit:::infer_inner_plot_scale(density, diagnostic)

  expect_equal(heatmap_scale$scale_x, 2 / 3)
  expect_equal(heatmap_scale$scale_y, 2 / 3)
  expect_equal(density_scale$scale_x, 2 / 3)
  expect_equal(density_scale$scale_y, 1)
})

test_that("distribution footprints preserve explicit aspect while growing taller", {
  grouped <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg, colour = factor(gear))) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter() +
    ggplot2::theme(
      aspect.ratio = 0.55,
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      legend.position = "bottom"
    )
  violin <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_violin() +
    ggplot2::theme(aspect.ratio = 0.5)
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 50,
    preferred_height_mm = 50,
    hard_loss = 0,
    stringsAsFactors = FALSE
  )

  grouped_scale <- plotfit:::infer_inner_plot_scale(grouped, diagnostic)
  violin_scale <- plotfit:::infer_inner_plot_scale(violin, diagnostic)

  expect_equal(grouped_scale$scale_x, 1)
  expect_equal(grouped_scale$scale_y, 1)
  expect_equal(violin_scale$scale_x, 1)
  expect_equal(violin_scale$scale_y, 1)
})

test_that("histograms and text matrices keep readable class-specific footprints", {
  histogram <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
    ggplot2::geom_histogram(binwidth = 5) +
    ggplot2::theme(aspect.ratio = 0.5)
  matrix_plot <- ggplot2::ggplot(
    expand.grid(x = letters[1:4], y = letters[1:4]),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = x)) +
    ggplot2::theme(aspect.ratio = 1)
  diagnostic <- data.frame(
    allocated_width_mm = 100,
    allocated_height_mm = 100,
    preferred_width_mm = 100,
    preferred_height_mm = 100,
    hard_loss = 0,
    n_nonempty_text_labels = 16,
    stringsAsFactors = FALSE
  )

  histogram_scale <- plotfit:::infer_inner_plot_scale(histogram, diagnostic)
  matrix_scale <- plotfit:::infer_inner_plot_scale(matrix_plot, diagnostic)

  expect_equal(histogram_scale$scale_x, 0.5)
  expect_equal(histogram_scale$scale_y, 1)
  expect_equal(matrix_scale$scale_x, 0.625)
  expect_equal(matrix_scale$scale_y, 0.625)
})

test_that("labelled matrix footprints decrease continuously with cell count", {
  matrix_plot <- ggplot2::ggplot(
    expand.grid(x = letters[1:4], y = letters[1:4]),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = x)) +
    ggplot2::theme(aspect.ratio = 1)
  diagnostic <- data.frame(
    hard_loss = 0,
    n_nonempty_text_labels = c(36, 144),
    stringsAsFactors = FALSE
  )

  medium <- plotfit:::infer_inner_plot_scale(matrix_plot, diagnostic[1, , drop = FALSE])
  large <- plotfit:::infer_inner_plot_scale(matrix_plot, diagnostic[2, , drop = FALSE])

  expect_equal(medium$scale_x, 0.5)
  expect_equal(medium$scale_y, 0.5)
  expect_equal(large$scale_x, 0.375)
  expect_equal(large$scale_y, 0.375)
})
