# ---- suggest_patchwork_layout.R ----
suggest_patchwork_layout <- function(
    plots,
    plot_names = names(plots),
    page_width_in = 8.27,
    page_height_in = 11.69,
    page_margin_mm = 0,
    base_size = 7,
    base_family = "Helvetica",
    min_label_gap_mm = 1.5,
    target_label_gap_mm = NULL,
    min_panel_width_mm = NULL,
    min_panel_height_mm = NULL,
    min_panel_area_mm2 = NULL,
    max_grid_cols = 6,
    max_grid_rows = 8,
    allow_multipage = TRUE,
    max_pages = Inf,
    page_groups = NULL,
    multipage_penalty = 25,
    keep_plot_order = TRUE,
    allow_empty_cells = TRUE,
    collect_guides = FALSE,
    collect_axes = FALSE,
    device = c("pdf", "cairo_pdf"),
    use_check_overlap_oracle = TRUE,
    search_mode = c("standard", "fast"),
    search_budget = 500,
    search_timeout_seconds = Inf,
    early_stop_patience = Inf,
    return_candidates = 5,
    output_style = c("design", "nested_patchwork"),
    layout_engine = c("patchwork", "grid"),
    validation_level = c("layout", "render"),
    max_empty_fraction = 0.25,
    verbose = TRUE) {

  total_started_at <- proc.time()[["elapsed"]]
  max_pages_was_missing <- missing(max_pages)
  search_budget_was_missing <- missing(search_budget)
  search_timeout_was_missing <- missing(search_timeout_seconds)
  early_stop_was_missing <- missing(early_stop_patience)
  search_mode <- match.arg(search_mode)

  if (identical(search_mode, "fast")) {
    if (search_budget_was_missing) search_budget <- 75
    if (max_pages_was_missing) max_pages <- if (is.null(page_groups)) 2 else length(page_groups)
    if (search_timeout_was_missing) search_timeout_seconds <- 15
    if (early_stop_was_missing) early_stop_patience <- 20
  }

  preferences <- validate_layout_inputs(
    page_width_in = page_width_in,
    page_height_in = page_height_in,
    page_margin_mm = page_margin_mm,
    base_size = base_size,
    min_label_gap_mm = min_label_gap_mm,
    target_label_gap_mm = target_label_gap_mm,
    min_panel_width_mm = min_panel_width_mm,
    min_panel_height_mm = min_panel_height_mm,
    min_panel_area_mm2 = min_panel_area_mm2,
    max_grid_cols = max_grid_cols,
    max_grid_rows = max_grid_rows,
    allow_multipage = allow_multipage,
    max_pages = max_pages,
    search_mode = search_mode,
    multipage_penalty = multipage_penalty,
    allow_empty_cells = allow_empty_cells,
    search_budget = search_budget,
    search_timeout_seconds = search_timeout_seconds,
    early_stop_patience = early_stop_patience,
    return_candidates = return_candidates,
    device = device,
    output_style = output_style,
    layout_engine = layout_engine,
    validation_level = validation_level,
    max_empty_fraction = max_empty_fraction
  )
  preferences$verbose <- verbose

  plots <- normalize_plot_list(plots, plot_names)
  preferences$page_groups <- validate_page_groups(
    page_groups = page_groups,
    plot_ids = names(plots),
    allow_multipage = preferences$allow_multipage,
    max_pages = preferences$max_pages
  )

  if (!keep_plot_order) {
    warning("`keep_plot_order = FALSE` is accepted but the current search still prioritizes order-preserving candidates.")
  }
  if (!allow_empty_cells) {
    warning("`allow_empty_cells = FALSE` is accepted but the current search may still use empty cells when needed for rectangular editable designs.")
  }
  if (use_check_overlap_oracle) {
    overlap_note <- "guide_axis(check.overlap = TRUE) oracle is not used; explicit physical measurements are used instead."
  } else {
    overlap_note <- character()
  }

  page_spec <- list(
    width_mm = page_width_in * 25.4 - 2 * page_margin_mm,
    height_mm = page_height_in * 25.4 - 2 * page_margin_mm
  )

  measurement_spec <- list(
    base_size = base_size,
    base_family = base_family
  )

  if (verbose) {
    message("Measuring ", length(plots), " plot(s) on a target-like ", preferences$device, " device.")
  }

  measurement_started_at <- proc.time()[["elapsed"]]
  device_state <- open_measurement_device(
    device = preferences$device,
    width_in = page_width_in,
    height_in = page_height_in,
    base_family = base_family,
    base_size = base_size
  )
  on.exit(close_measurement_device(device_state), add = TRUE)

  profiles <- vector("list", length(plots))
  names(profiles) <- names(plots)

  for (plot_index in seq_along(plots)) {
    profiles[[plot_index]] <- measure_plot_profile(
      plot = plots[[plot_index]],
      plot_id = names(plots)[plot_index],
      plot_index = plot_index,
      page_spec = page_spec,
      measurement_spec = measurement_spec
    )
  }
  measurement_elapsed <- proc.time()[["elapsed"]] - measurement_started_at

  fit_functions <- lapply(profiles, make_plot_fit_function, preferences = preferences)
  names(fit_functions) <- names(plots)

  if (verbose) {
    message("Estimating size frontiers and scoring layout candidates.")
  }

  frontier_started_at <- proc.time()[["elapsed"]]
  frontiers <- lapply(
    fit_functions,
    estimate_size_frontier,
    page_spec = page_spec,
    coarse_step_mm = preferences$frontier_coarse_step_mm,
    refine = preferences$frontier_refine,
    refine_step_mm = preferences$frontier_refine_step_mm
  )
  frontier_summaries <- lapply(frontiers, summarise_size_frontier)
  for (plot_id in names(frontier_summaries)) {
    frontier_summaries[[plot_id]]$geometry <- profiles[[plot_id]]$geometry
  }
  frontier_elapsed <- proc.time()[["elapsed"]] - frontier_started_at

  search_started_at <- proc.time()[["elapsed"]]
  selected <- select_best_solution(
    plot_ids = names(plots),
    frontiers = frontier_summaries,
    fit_functions = fit_functions,
    page_spec = page_spec,
    preferences = preferences
  )
  search_elapsed <- proc.time()[["elapsed"]] - search_started_at

  output_started_at <- proc.time()[["elapsed"]]
  pages <- build_layout_pages(
    best_candidate = selected$best_candidate,
    plots = plots,
    output_style = preferences$output_style,
    collect_guides = collect_guides,
    collect_axes = collect_axes,
    layout_engine = preferences$layout_engine,
    page_spec = page_spec,
    preferences = preferences,
    fit_functions = fit_functions
  )
  output_elapsed <- proc.time()[["elapsed"]] - output_started_at

  validation_started_at <- proc.time()[["elapsed"]]
  validation_warnings <- validate_final_solution(
    pages = pages,
    page_width_in = page_width_in,
    page_height_in = page_height_in,
    base_family = base_family,
    base_size = base_size,
    validation_level = preferences$validation_level
  )
  validation_elapsed <- proc.time()[["elapsed"]] - validation_started_at

  plot_diagnostics <- make_plot_diagnostics(
    profiles,
    frontier_summaries,
    selected$best_candidate,
    pages = pages
  )
  layout_diagnostics <- do.call(
    rbind,
    lapply(selected$candidates, function(candidate) candidate$diagnostics)
  )
  result_warnings <- make_result_warnings(profiles, selected$best_candidate, plot_diagnostics, preferences)
  result_warnings <- unique(c(overlap_note, result_warnings, validation_warnings))

  performance_diagnostics <- list(
    stages = data.frame(
      stage = c("measurement", "frontier_estimation", "candidate_search", "output_construction", "validation", "total"),
      elapsed_seconds = c(
        measurement_elapsed,
        frontier_elapsed,
        search_elapsed,
        output_elapsed,
        validation_elapsed,
        proc.time()[["elapsed"]] - total_started_at
      ),
      stringsAsFactors = FALSE
    ),
    candidates = selected$search_diagnostics,
    fit_cache = collect_fit_cache_diagnostics(fit_functions)
  )

  list(
    pages = pages,
    plot_diagnostics = plot_diagnostics,
    layout_diagnostics = layout_diagnostics,
    candidates = selected$candidates,
    performance_diagnostics = performance_diagnostics,
    warnings = result_warnings,
    assumptions = list(
      page_width_in = page_width_in,
      page_height_in = page_height_in,
      device = preferences$device,
      base_size = base_size,
      base_family = base_family,
      min_label_gap_mm = preferences$min_label_gap_mm,
      min_panel_width_mm = preferences$min_panel_width_mm,
      min_panel_height_mm = preferences$min_panel_height_mm,
      allow_multipage = preferences$allow_multipage,
      max_pages = preferences$max_pages,
      page_groups = preferences$page_groups,
      search_mode = preferences$search_mode,
      search_budget = preferences$search_budget,
      search_timeout_seconds = preferences$search_timeout_seconds,
      early_stop_patience = preferences$early_stop_patience,
      frontier_coarse_step_mm = preferences$frontier_coarse_step_mm,
      frontier_refine = preferences$frontier_refine,
      frontier_refine_step_mm = preferences$frontier_refine_step_mm,
      weight_optimization_steps = preferences$weight_optimization_steps,
      weight_optimization_passes = preferences$weight_optimization_passes,
      layout_engine = preferences$layout_engine,
      validation_level = preferences$validation_level,
      max_empty_fraction = preferences$max_empty_fraction,
      selected_page_count = length(pages)
    )
  )
}


# ---- validation.R ----
normalize_plot_list <- function(plots, plot_names = NULL) {
  if (inherits(plots, "ggplot")) {
    plots <- list(p1 = plots)
  }

  if (!is.list(plots) || length(plots) == 0) {
    stop("`plots` must be a non-empty list of ggplot objects.", call. = FALSE)
  }

  is_ggplot <- vapply(plots, inherits, logical(1), what = "ggplot")
  if (!all(is_ggplot)) {
    bad_positions <- which(!is_ggplot)
    stop(
      "`plots` must contain only ggplot objects. Non-ggplot entries at positions: ",
      paste(bad_positions, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(plot_names)) {
    plot_names <- names(plots)
  }

  if (is.null(plot_names) || length(plot_names) != length(plots)) {
    plot_names <- character(length(plots))
  }

  missing_names <- is.na(plot_names) | plot_names == ""
  plot_names[missing_names] <- paste0("p", which(missing_names))
  plot_names <- make.unique(plot_names, sep = "_")
  names(plots) <- plot_names

  plots
}

validate_layout_inputs <- function(
    page_width_in,
    page_height_in,
    page_margin_mm,
    base_size,
    min_label_gap_mm,
    target_label_gap_mm,
    min_panel_width_mm,
    min_panel_height_mm,
    min_panel_area_mm2,
    max_grid_cols,
    max_grid_rows,
    allow_multipage,
    max_pages,
    search_mode = "standard",
    multipage_penalty,
    allow_empty_cells = TRUE,
    search_budget,
    search_timeout_seconds = Inf,
    early_stop_patience = Inf,
    return_candidates,
    device,
    output_style,
    layout_engine = c("patchwork", "grid"),
    validation_level = c("layout", "render"),
    max_empty_fraction = 0.25) {

  if (!is.numeric(page_width_in) || length(page_width_in) != 1 || page_width_in <= 0) {
    stop("`page_width_in` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(page_height_in) || length(page_height_in) != 1 || page_height_in <= 0) {
    stop("`page_height_in` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(page_margin_mm) || length(page_margin_mm) != 1 || page_margin_mm < 0) {
    stop("`page_margin_mm` must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(base_size) || length(base_size) != 1 || base_size <= 0) {
    stop("`base_size` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(min_label_gap_mm) || length(min_label_gap_mm) != 1 || min_label_gap_mm < 0) {
    stop("`min_label_gap_mm` must be a non-negative number.", call. = FALSE)
  }
  if (!is.logical(allow_multipage) || length(allow_multipage) != 1) {
    stop("`allow_multipage` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(max_pages) || length(max_pages) != 1 || max_pages < 1) {
    stop("`max_pages` must be at least 1 or Inf.", call. = FALSE)
  }
  if (!is.numeric(search_budget) || length(search_budget) != 1 ||
      !is.finite(search_budget) || search_budget < 1) {
    stop("`search_budget` must be a positive finite number.", call. = FALSE)
  }
  if (!is.numeric(return_candidates) || length(return_candidates) != 1 ||
      !is.finite(return_candidates) || return_candidates < 1) {
    stop("`return_candidates` must be a positive finite number.", call. = FALSE)
  }
  if (!is.numeric(search_timeout_seconds) || length(search_timeout_seconds) != 1 ||
      is.na(search_timeout_seconds) || search_timeout_seconds < 0) {
    stop("`search_timeout_seconds` must be a non-negative number or Inf.", call. = FALSE)
  }
  if (!is.numeric(early_stop_patience) || length(early_stop_patience) != 1 ||
      is.na(early_stop_patience) || early_stop_patience < 1) {
    stop("`early_stop_patience` must be a positive number or Inf.", call. = FALSE)
  }
  if (!is.numeric(multipage_penalty) || length(multipage_penalty) != 1 || multipage_penalty < 0) {
    stop("`multipage_penalty` must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(max_grid_cols) || length(max_grid_cols) != 1 || max_grid_cols < 1) {
    stop("`max_grid_cols` must be at least 1.", call. = FALSE)
  }
  if (!is.numeric(max_grid_rows) || length(max_grid_rows) != 1 || max_grid_rows < 1) {
    stop("`max_grid_rows` must be at least 1.", call. = FALSE)
  }
  if (!is.numeric(max_empty_fraction) || length(max_empty_fraction) != 1 ||
      max_empty_fraction < 0 || max_empty_fraction > 1) {
    stop("`max_empty_fraction` must be a number between 0 and 1.", call. = FALSE)
  }

  text_height_mm <- base_size * 25.4 / 72

  if (is.null(min_panel_width_mm)) {
    min_panel_width_mm <- 8 * text_height_mm
  }
  if (is.null(min_panel_height_mm)) {
    min_panel_height_mm <- 6 * text_height_mm
  }
  if (is.null(min_panel_area_mm2)) {
    min_panel_area_mm2 <- min_panel_width_mm * min_panel_height_mm
  }
  if (is.null(target_label_gap_mm)) {
    target_label_gap_mm <- 1.5 * min_label_gap_mm
  }

  if (min_panel_width_mm <= 0 || min_panel_height_mm <= 0 || min_panel_area_mm2 <= 0) {
    stop("Panel-size limits must be positive.", call. = FALSE)
  }

  list(
    page_width_in = page_width_in,
    page_height_in = page_height_in,
    page_margin_mm = page_margin_mm,
    base_size = base_size,
    min_label_gap_mm = min_label_gap_mm,
    target_label_gap_mm = target_label_gap_mm,
    min_panel_width_mm = min_panel_width_mm,
    min_panel_height_mm = min_panel_height_mm,
    target_panel_width_mm = 2 * min_panel_width_mm,
    target_panel_height_mm = 2 * min_panel_height_mm,
    min_panel_area_mm2 = min_panel_area_mm2,
    max_grid_cols = as.integer(max_grid_cols),
    max_grid_rows = as.integer(max_grid_rows),
    allow_multipage = allow_multipage,
    max_pages = max_pages,
    search_mode = match.arg(search_mode, c("standard", "fast")),
    multipage_penalty = multipage_penalty,
    search_budget = as.integer(search_budget),
    search_timeout_seconds = search_timeout_seconds,
    early_stop_patience = if (is.finite(early_stop_patience)) as.integer(early_stop_patience) else Inf,
    frontier_coarse_step_mm = if (identical(search_mode, "fast")) 25 else 20,
    frontier_refine = !identical(search_mode, "fast"),
    frontier_refine_step_mm = 5,
    weight_optimization_steps = if (identical(search_mode, "fast")) 0.7 else c(0.7, 0.35, 0.15),
    weight_optimization_passes = if (identical(search_mode, "fast")) 1L else 4L,
    return_candidates = as.integer(return_candidates),
    device = match.arg(device, c("pdf", "cairo_pdf")),
    output_style = match.arg(output_style, c("design", "nested_patchwork")),
    layout_engine = match.arg(layout_engine, c("patchwork", "grid")),
    validation_level = match.arg(validation_level, c("layout", "render")),
    max_empty_fraction = max_empty_fraction,
    hard_label_penalty = 1000,
    hard_panel_penalty = 1000,
    hard_facet_penalty = 1000,
    hard_solution_penalty = 50000,
    soft_excess_gap_penalty = 0.01,
    soft_oversize_penalty = 1.00,
    empty_cell_penalty = if (allow_empty_cells) 0.25 else 8,
    excess_empty_cell_penalty = 250,
    compact_spacer_bonus = 0,
    scaled_report_bonus = 0,
    complexity_penalty = 0.25,
    uneven_weight_penalty = 10.00,
    row_height_prior_penalty = 35.00,
    unused_panel_area_penalty = 2.50
  )
}

validate_page_groups <- function(page_groups, plot_ids, allow_multipage, max_pages) {
  if (is.null(page_groups)) {
    return(NULL)
  }
  if (!is.list(page_groups) || length(page_groups) == 0 ||
      any(vapply(page_groups, length, integer(1)) == 0)) {
    stop("`page_groups` must be a non-empty list of non-empty plot-ID vectors.", call. = FALSE)
  }
  valid_group <- vapply(page_groups, function(group) {
    is.character(group) && !anyNA(group) && all(nzchar(group))
  }, logical(1))
  if (!all(valid_group)) {
    stop("Every `page_groups` entry must contain non-missing plot IDs.", call. = FALSE)
  }

  grouped_ids <- unlist(page_groups, use.names = FALSE)
  duplicated_ids <- unique(grouped_ids[duplicated(grouped_ids)])
  unknown_ids <- setdiff(grouped_ids, plot_ids)
  missing_ids <- setdiff(plot_ids, grouped_ids)
  if (length(duplicated_ids) > 0) {
    stop("Plot IDs may appear only once in `page_groups`: ", paste(duplicated_ids, collapse = ", "), call. = FALSE)
  }
  if (length(unknown_ids) > 0) {
    stop("Unknown plot IDs in `page_groups`: ", paste(unknown_ids, collapse = ", "), call. = FALSE)
  }
  if (length(missing_ids) > 0) {
    stop("Every plot must appear in `page_groups`; missing: ", paste(missing_ids, collapse = ", "), call. = FALSE)
  }
  if (length(page_groups) > 1 && !allow_multipage) {
    stop("Multiple `page_groups` require `allow_multipage = TRUE`.", call. = FALSE)
  }
  if (length(page_groups) > max_pages) {
    stop("The number of `page_groups` exceeds `max_pages`.", call. = FALSE)
  }

  unname(page_groups)
}


