make_zero_loss_fit <- function() {
  function(width_mm, height_mm) {
    list(
      total_loss = 0,
      hard_loss = 0,
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
}

make_search_preferences <- function(...) {
  defaults <- list(
    allow_multipage = FALSE,
    max_pages = 1,
    page_groups = NULL,
    max_grid_cols = 3,
    max_grid_rows = 3,
    search_budget = 20,
    search_timeout_seconds = Inf,
    early_stop_patience = Inf,
    return_candidates = 1,
    multipage_penalty = 25,
    complexity_penalty = 0,
    empty_cell_penalty = 0,
    uneven_weight_penalty = 0,
    row_height_prior_penalty = 0,
    weight_optimization_steps = 0.7,
    weight_optimization_passes = 1,
    verbose = FALSE
  )
  utils::modifyList(defaults, list(...))
}

test_that("plot fit functions cache canonical 0.1 mm evaluations", {
  device_state <- plotfit:::open_measurement_device(
    device = "pdf", width_in = 4, height_in = 4,
    base_family = "Helvetica", base_size = 7
  )
  on.exit(plotfit:::close_measurement_device(device_state), add = TRUE)
  page_spec <- list(width_mm = 101.6, height_mm = 101.6)
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  profile <- plotfit:::measure_plot_profile(
    plot, "p1", 1, page_spec,
    measurement_spec = list(base_size = 7, base_family = "Helvetica")
  )
  preferences <- plotfit:::validate_layout_inputs(
    page_width_in = 4, page_height_in = 4, page_margin_mm = 0,
    base_size = 7, min_label_gap_mm = 1.5, target_label_gap_mm = NULL,
    min_panel_width_mm = NULL, min_panel_height_mm = NULL, min_panel_area_mm2 = NULL,
    max_grid_cols = 2, max_grid_rows = 2, allow_multipage = TRUE,
    max_pages = 2, multipage_penalty = 25, search_budget = 5,
    return_candidates = 1, device = "pdf", output_style = "design"
  )
  fit_function <- plotfit:::make_plot_fit_function(profile, preferences)

  first <- fit_function(40.04, 50.04)
  equivalent <- fit_function(40.049, 50.049)
  distinct <- fit_function(40.16, 50.16)
  stats <- attr(fit_function, "plotfit_cache_stats")()

  expect_identical(first, equivalent)
  expect_equal(first$width_mm, 40)
  expect_equal(distinct$width_mm, 40.2)
  expect_equal(stats$requests, 3)
  expect_equal(stats$evaluations, 2)
  expect_equal(stats$hits, 1)
})

test_that("adaptive frontier refines promising regions with fewer evaluations", {
  calls <- 0L
  fit_function <- function(width_mm, height_mm) {
    calls <<- calls + 1L
    violation <- max(0, 43 - width_mm, 38 - height_mm)
    list(
      width_mm = width_mm,
      height_mm = height_mm,
      total_loss = violation^2,
      hard_violation_mm = violation,
      hard_loss = violation^2,
      panel_width_mm = width_mm,
      panel_height_mm = height_mm
    )
  }

  frontier <- plotfit:::estimate_size_frontier(
    fit_function,
    page_spec = list(width_mm = 100, height_mm = 100),
    coarse_step_mm = 20,
    refine = TRUE,
    refine_step_mm = 5
  )

  expect_lt(calls, 100)
  expect_true(any(frontier$width_mm == 100 & frontier$height_mm == 100))
  expect_true(any(frontier$width_mm %% 20 != 0 & !frontier$width_mm %in% c(5, 80, 90, 100)))
  expect_true(any(frontier$acceptable))
})

test_that("adaptive frontier handles plots with no acceptable point", {
  fit_function <- function(width_mm, height_mm) {
    list(
      width_mm = width_mm,
      height_mm = height_mm,
      total_loss = 100,
      hard_violation_mm = 1,
      hard_loss = 1,
      panel_width_mm = width_mm,
      panel_height_mm = height_mm
    )
  }
  frontier <- plotfit:::estimate_size_frontier(
    fit_function,
    page_spec = list(width_mm = 100, height_mm = 100),
    coarse_step_mm = 20,
    refine = TRUE
  )
  summary <- plotfit:::summarise_size_frontier(frontier)

  expect_false(any(frontier$acceptable))
  expect_true(summary$impossible_on_page)
})

test_that("fixed page groups bypass assignment search and preserve order", {
  plot_ids <- c("p1", "p2", "p3")
  groups <- list(c("p2", "p1"), "p3")
  frontiers <- lapply(plot_ids, function(id) {
    list(preferred_width_mm = 40, preferred_height_mm = 40, min_acceptable_area_mm2 = 1600)
  })
  names(frontiers) <- plot_ids
  fit_functions <- setNames(lapply(plot_ids, function(id) make_zero_loss_fit()), plot_ids)
  preferences <- make_search_preferences(
    allow_multipage = TRUE,
    max_pages = 2,
    page_groups = groups,
    search_budget = 5
  )

  selected <- plotfit:::select_best_solution(
    plot_ids, frontiers, fit_functions,
    page_spec = list(width_mm = 100, height_mm = 100),
    preferences = preferences
  )

  expect_equal(selected$best_candidate$page_assignment, groups)
  expect_equal(length(selected$best_candidate$pages), 2)
})

test_that("candidate search reports progress and stops on patience", {
  plot_ids <- c("p1", "p2", "p3")
  frontiers <- setNames(lapply(plot_ids, function(id) {
    list(preferred_width_mm = 30, preferred_height_mm = 30, min_acceptable_area_mm2 = 900)
  }), plot_ids)
  fit_functions <- setNames(lapply(plot_ids, function(id) make_zero_loss_fit()), plot_ids)
  preferences <- make_search_preferences(
    search_budget = 20,
    early_stop_patience = 1,
    verbose = TRUE
  )

  messages <- capture.output(
    selected <- plotfit:::select_best_solution(
      plot_ids, frontiers, fit_functions,
      page_spec = list(width_mm = 100, height_mm = 100),
      preferences = preferences
    ),
    type = "message"
  )
  expect_true(any(grepl("Scored", messages)))
  expect_true(any(grepl("Candidate search stopped", messages)))
  expect_equal(selected$search_diagnostics$stop_reason, "patience")
  expect_lt(selected$search_diagnostics$scored, preferences$search_budget)
})

test_that("candidate search timeout is soft and retains a scored candidate", {
  plot_ids <- c("p1", "p2")
  frontiers <- setNames(lapply(plot_ids, function(id) {
    list(preferred_width_mm = 30, preferred_height_mm = 30, min_acceptable_area_mm2 = 900)
  }), plot_ids)
  fit_functions <- setNames(lapply(plot_ids, function(id) make_zero_loss_fit()), plot_ids)
  preferences <- make_search_preferences(search_timeout_seconds = 0)

  selected <- plotfit:::select_best_solution(
    plot_ids, frontiers, fit_functions,
    page_spec = list(width_mm = 100, height_mm = 100),
    preferences = preferences
  )

  expect_equal(selected$search_diagnostics$stop_reason, "timeout")
  expect_equal(selected$search_diagnostics$scored, 1)
  expect_length(selected$candidates, 1)
})

test_that("page group validation rejects incomplete and conflicting partitions", {
  expect_equal(
    plotfit:::validate_page_groups(list(c("p2", "p1"), "p3"), c("p1", "p2", "p3"), TRUE, 2),
    list(c("p2", "p1"), "p3")
  )
  expect_error(
    plotfit:::validate_page_groups(list(c("p1", "p2")), c("p1", "p2", "p3"), TRUE, 2),
    "missing"
  )
  expect_error(
    plotfit:::validate_page_groups(list(c("p1", "p2"), c("p2", "p3")), c("p1", "p2", "p3"), TRUE, 2),
    "only once"
  )
  expect_error(
    plotfit:::validate_page_groups(list("p1", c("p2", "p3")), c("p1", "p2", "p3"), FALSE, 2),
    "allow_multipage"
  )
  expect_error(
    plotfit:::validate_page_groups(list("p1", "p2", "p3"), c("p1", "p2", "p3"), TRUE, 2),
    "exceeds"
  )
})

test_that("fast mode resolves defaults and returns performance diagnostics", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  result <- suggest_patchwork_layout(
    list(p1 = plot),
    search_mode = "fast",
    return_candidates = 1,
    verbose = FALSE
  )

  expect_equal(result$assumptions$search_mode, "fast")
  expect_equal(result$assumptions$search_budget, 75)
  expect_equal(result$assumptions$max_pages, 2)
  expect_equal(result$assumptions$search_timeout_seconds, 15)
  expect_equal(result$assumptions$early_stop_patience, 20)
  expect_equal(result$assumptions$frontier_coarse_step_mm, 25)
  expect_false(result$assumptions$frontier_refine)
  expect_equal(result$assumptions$weight_optimization_passes, 1)
  expect_named(result$performance_diagnostics, c("stages", "candidates", "fit_cache"))
  expect_true(all(result$performance_diagnostics$stages$elapsed_seconds >= 0))
  expect_equal(
    result$performance_diagnostics$fit_cache$requests,
    result$performance_diagnostics$fit_cache$evaluations + result$performance_diagnostics$fit_cache$hits
  )
})

test_that("explicit search controls override fast mode defaults", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  result <- suggest_patchwork_layout(
    list(p1 = plot),
    search_mode = "fast",
    search_budget = 3,
    max_pages = 4,
    search_timeout_seconds = Inf,
    early_stop_patience = 7,
    return_candidates = 1,
    verbose = FALSE
  )

  expect_equal(result$assumptions$search_budget, 3)
  expect_equal(result$assumptions$max_pages, 4)
  expect_equal(result$assumptions$search_timeout_seconds, Inf)
  expect_equal(result$assumptions$early_stop_patience, 7)
})

test_that("fixed page groups override the fast preset page default", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  plots <- list(p1 = plot, p2 = plot, p3 = plot)
  result <- suggest_patchwork_layout(
    plots,
    search_mode = "fast",
    page_groups = list("p1", "p2", "p3"),
    search_budget = 1,
    search_timeout_seconds = Inf,
    return_candidates = 1,
    verbose = FALSE
  )

  expect_equal(result$assumptions$max_pages, 3)
  expect_equal(result$assumptions$page_groups, list("p1", "p2", "p3"))
  expect_equal(length(result$pages), 3)
  expect_equal(result$performance_diagnostics$candidates$stop_reason, "budget")
})
