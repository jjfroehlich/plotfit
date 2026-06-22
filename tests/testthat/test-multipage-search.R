test_that("multipage search can beat a violating one-page layout", {
  plot_ids <- c("p1", "p2")
  frontiers <- list(
    p1 = list(preferred_width_mm = 90, preferred_height_mm = 80, min_acceptable_area_mm2 = 7200),
    p2 = list(preferred_width_mm = 90, preferred_height_mm = 80, min_acceptable_area_mm2 = 7200)
  )

  fit_functions <- lapply(plot_ids, function(plot_id) {
    force(plot_id)
    function(width_mm, height_mm) {
      violation <- max(0, 80 - width_mm, 80 - height_mm)
      list(
        total_loss = 1000 * violation^2,
        hard_violation_mm = violation,
        panel_width_mm = width_mm,
        panel_height_mm = height_mm,
        min_x_label_gap_mm = Inf,
        min_y_label_gap_mm = Inf,
        label_gap_loss = 0,
        panel_minimum_loss = 1000 * violation^2,
        legend_loss = 0,
        facet_loss = 0,
        data_density_loss = 0,
        warnings = character()
      )
    }
  })
  names(fit_functions) <- plot_ids

  preferences <- list(
    allow_multipage = TRUE,
    max_pages = 2,
    max_grid_cols = 2,
    max_grid_rows = 2,
    search_budget = 20,
    return_candidates = 3,
    multipage_penalty = 25,
    complexity_penalty = 0,
    empty_cell_penalty = 0,
    uneven_weight_penalty = 0.01
  )

  selected <- patchworkLayoutOptimizer:::select_best_solution(
    plot_ids = plot_ids,
    frontiers = frontiers,
    fit_functions = fit_functions,
    page_spec = list(width_mm = 100, height_mm = 100),
    preferences = preferences
  )

  expect_equal(length(selected$best_candidate$pages), 2)
})

test_that("multipage search can beat a wasteful one-page layout", {
  plot_ids <- c("p1", "p2")
  frontiers <- list(
    p1 = list(preferred_width_mm = 40, preferred_height_mm = 40, min_acceptable_area_mm2 = 1600),
    p2 = list(preferred_width_mm = 40, preferred_height_mm = 40, min_acceptable_area_mm2 = 1600)
  )

  fit_functions <- lapply(plot_ids, function(plot_id) {
    force(plot_id)
    function(width_mm, height_mm) {
      aspect_waste <- (height_mm - width_mm)^2
      list(
        total_loss = aspect_waste,
        hard_violation_mm = 0,
        panel_width_mm = width_mm,
        panel_height_mm = height_mm,
        effective_panel_width_mm = width_mm,
        effective_panel_height_mm = height_mm,
        unused_panel_area_mm2 = aspect_waste,
        min_x_label_gap_mm = Inf,
        min_y_label_gap_mm = Inf,
        label_gap_loss = 0,
        panel_minimum_loss = 0,
        legend_loss = 0,
        facet_loss = 0,
        data_density_loss = 0,
        aspect_ratio_loss = aspect_waste,
        unused_panel_area_loss = aspect_waste,
        warnings = character()
      )
    }
  })
  names(fit_functions) <- plot_ids

  preferences <- list(
    allow_multipage = TRUE,
    max_pages = 2,
    max_grid_cols = 3,
    max_grid_rows = 3,
    search_budget = 20,
    return_candidates = 3,
    multipage_penalty = 100,
    complexity_penalty = 0,
    empty_cell_penalty = 0,
    uneven_weight_penalty = 0,
    row_height_prior_penalty = 0,
    compact_spacer_bonus = 0
  )

  selected <- patchworkLayoutOptimizer:::select_best_solution(
    plot_ids = plot_ids,
    frontiers = frontiers,
    fit_functions = fit_functions,
    page_spec = list(width_mm = 100, height_mm = 100),
    preferences = preferences
  )

  expect_equal(length(selected$best_candidate$pages), 2)
})

test_that("one-page search wins when plots fit cleanly", {
  plot_ids <- c("p1", "p2")
  frontiers <- list(
    p1 = list(preferred_width_mm = 40, preferred_height_mm = 40, min_acceptable_area_mm2 = 1600),
    p2 = list(preferred_width_mm = 40, preferred_height_mm = 40, min_acceptable_area_mm2 = 1600)
  )

  fit_functions <- lapply(plot_ids, function(plot_id) {
    force(plot_id)
    function(width_mm, height_mm) {
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
  })
  names(fit_functions) <- plot_ids

  preferences <- list(
    allow_multipage = TRUE,
    max_pages = 2,
    max_grid_cols = 2,
    max_grid_rows = 2,
    search_budget = 20,
    return_candidates = 3,
    multipage_penalty = 100,
    complexity_penalty = 0,
    empty_cell_penalty = 0,
    uneven_weight_penalty = 0,
    row_height_prior_penalty = 0,
    compact_spacer_bonus = 0
  )

  selected <- patchworkLayoutOptimizer:::select_best_solution(
    plot_ids = plot_ids,
    frontiers = frontiers,
    fit_functions = fit_functions,
    page_spec = list(width_mm = 100, height_mm = 100),
    preferences = preferences
  )

  expect_equal(length(selected$best_candidate$pages), 1)
})
