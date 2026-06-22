test_that("suggest_patchwork_layout returns pages, diagnostics, and assumptions", {
  plots <- list(
    p1 = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
    p2 = ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_boxplot()
  )

  result <- suggest_patchwork_layout(
    plots = plots,
    max_grid_cols = 2,
    max_grid_rows = 2,
    search_budget = 5,
    return_candidates = 1,
    verbose = FALSE
  )

  expect_gt(length(result$pages), 0)
  expect_s3_class(result$pages[[1]]$patchwork, "patchwork")
  expect_equal(nrow(result$plot_diagnostics), 2)
  expect_true(result$assumptions$allow_multipage)
  expect_equal(result$assumptions$layout_engine, "patchwork")
  expect_equal(result$assumptions$validation_level, "layout")
  expect_true(is.numeric(result$pages[[1]]$col_widths_mm))
  expect_true(is.numeric(result$pages[[1]]$row_heights_mm))
  expect_match(result$pages[[1]]$patchwork_code, "combined_plot")
})

test_that("suggest_patchwork_layout can return grid pages", {
  plots <- list(
    p1 = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
    p2 = ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_boxplot()
  )

  result <- suggest_patchwork_layout(
    plots = plots,
    max_grid_cols = 2,
    max_grid_rows = 2,
    search_budget = 5,
    return_candidates = 1,
    layout_engine = "grid",
    verbose = FALSE
  )

  expect_equal(result$pages[[1]]$engine, "grid")
  expect_null(result$pages[[1]]$patchwork)
  expect_s3_class(result$pages[[1]]$grob, "grob")
  expect_equal(nrow(result$plot_diagnostics), 2)
})

test_that("suggest_patchwork_layout handles mixed plot families with finite automatic footprints", {
  long_labels <- mtcars
  long_labels$cyl_label <- factor(
    long_labels$cyl,
    labels = c("four cylinder cars", "six cylinder cars", "eight cylinder cars")
  )

  plots <- list(
    dense_scatter = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
      ggplot2::geom_point() +
      ggplot2::theme(aspect.ratio = 1),
    rotated_boxplot = ggplot2::ggplot(long_labels, ggplot2::aes(cyl_label, mpg)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)),
    histogram = ggplot2::ggplot(mtcars, ggplot2::aes(mpg)) +
      ggplot2::geom_histogram(bins = 8),
    faceted = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~cyl)
  )

  result <- suggest_patchwork_layout(
    plots = plots,
    max_grid_cols = 4,
    max_grid_rows = 4,
    max_pages = 2,
    search_budget = 10,
    return_candidates = 2,
    verbose = FALSE
  )

  page_scales <- do.call(rbind, lapply(result$pages, function(page) page$inner_scales))

  expect_equal(sort(page_scales$plot_id), sort(names(plots)))
  expect_true(all(is.finite(page_scales$scale_x)))
  expect_true(all(is.finite(page_scales$scale_y)))
  expect_true(all(page_scales$scale_x > 0 & page_scales$scale_x <= 1))
  expect_true(all(page_scales$scale_y > 0 & page_scales$scale_y <= 1))
  expect_true(all(vapply(result$pages, function(page) {
    all(is.finite(page$col_widths_mm)) && all(is.finite(page$row_heights_mm))
  }, logical(1))))
})
