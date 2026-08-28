# ---- fit-loss.R ----
make_plot_fit_function <- function(profile, preferences) {
  force(profile)
  force(preferences)
  cache <- new.env(hash = TRUE, parent = emptyenv())
  counters <- new.env(parent = emptyenv())
  counters$requests <- 0L
  counters$evaluations <- 0L
  cache_resolution_mm <- 0.1

  fit_function <- function(width_mm, height_mm) {
    canonical_width_mm <- round(width_mm / cache_resolution_mm) * cache_resolution_mm
    canonical_height_mm <- round(height_mm / cache_resolution_mm) * cache_resolution_mm
    cache_key <- sprintf("%.1f|%.1f", canonical_width_mm, canonical_height_mm)
    counters$requests <- counters$requests + 1L

    if (exists(cache_key, envir = cache, inherits = FALSE)) {
      return(get(cache_key, envir = cache, inherits = FALSE))
    }

    fit <- evaluate_plot_fit(
      profile = profile,
      width_mm = canonical_width_mm,
      height_mm = canonical_height_mm,
      preferences = preferences
    )
    assign(cache_key, fit, envir = cache)
    counters$evaluations <- counters$evaluations + 1L
    fit
  }

  attr(fit_function, "plotfit_cache_stats") <- function() {
    list(
      plot_id = profile$plot_id,
      requests = counters$requests,
      evaluations = counters$evaluations,
      hits = counters$requests - counters$evaluations
    )
  }
  fit_function
}

collect_fit_cache_diagnostics <- function(fit_functions) {
  rows <- lapply(names(fit_functions), function(plot_id) {
    stats_function <- attr(fit_functions[[plot_id]], "plotfit_cache_stats")
    if (is.null(stats_function)) {
      return(data.frame(
        plot_id = plot_id, requests = NA_integer_, evaluations = NA_integer_,
        hits = NA_integer_, hit_rate = NA_real_, stringsAsFactors = FALSE
      ))
    }
    stats <- stats_function()
    data.frame(
      plot_id = plot_id,
      requests = stats$requests,
      evaluations = stats$evaluations,
      hits = stats$hits,
      hit_rate = if (stats$requests > 0) stats$hits / stats$requests else 0,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

evaluate_plot_fit <- function(profile, width_mm, height_mm, preferences) {
  measured <- measure_plot_at_size(profile, width_mm, height_mm, page_spec = profile$page)
  fixed_size <- measured$fixed_size

  panel_width_mm <- measured$panel_width_mm
  panel_height_mm <- measured$panel_height_mm
  effective_panel <- estimate_effective_panel_size(profile, panel_width_mm, panel_height_mm)

  density_limits <- adjusted_panel_limits_for_density(profile, preferences)
  footprint <- estimate_required_plot_footprint(
    profile = profile,
    fixed_size = fixed_size,
    preferences = preferences
  )
  axis_gaps <- estimate_axis_label_gaps(
    profile,
    effective_panel$effective_panel_width_mm,
    effective_panel$effective_panel_height_mm,
    preferences
  )
  panel_loss <- estimate_panel_minimum_loss(
    effective_panel$effective_panel_width_mm,
    effective_panel$effective_panel_height_mm,
    preferences$min_panel_width_mm,
    preferences$min_panel_height_mm,
    preferences$min_panel_area_mm2,
    preferences
  )
  facet_loss <- estimate_facet_loss(
    profile,
    effective_panel$effective_panel_width_mm,
    effective_panel$effective_panel_height_mm,
    preferences
  )
  legend_loss <- estimate_legend_loss(
    profile,
    width_mm,
    height_mm,
    effective_panel$effective_panel_width_mm,
    effective_panel$effective_panel_height_mm
  )
  data_density_loss <- estimate_data_density_loss(
    profile,
    effective_panel$effective_panel_width_mm,
    effective_panel$effective_panel_height_mm,
    preferences
  )
  aspect_loss <- estimate_aspect_ratio_loss(profile, panel_width_mm, panel_height_mm)
  unused_panel_area_loss <- estimate_unused_panel_area_loss(
    effective_panel$unused_panel_area_mm2,
    profile$page$width_mm * profile$page$height_mm,
    preferences
  )

  preferred_width_mm <- footprint$preferred_width_mm
  preferred_height_mm <- footprint$preferred_height_mm
  preferred_area_mm2 <- preferred_width_mm * preferred_height_mm
  allocated_area_mm2 <- width_mm * height_mm
  page_area_mm2 <- profile$page$width_mm * profile$page$height_mm

  oversize_loss <- preferences$soft_oversize_penalty *
    max(0, allocated_area_mm2 - preferred_area_mm2)^2 / max(page_area_mm2, 1)

  footprint_width_violation_mm <- if (isTRUE(footprint$measurement_reliable)) {
    max(0, footprint$required_width_mm - width_mm)
  } else {
    0
  }
  footprint_height_violation_mm <- if (isTRUE(footprint$measurement_reliable)) {
    max(0, footprint$required_height_mm - height_mm)
  } else {
    0
  }
  inner_footprint_width_violation_mm <- if (isTRUE(footprint$measurement_reliable)) {
    max(0, footprint$validation_required_width_mm - width_mm)
  } else {
    0
  }
  inner_footprint_height_violation_mm <- if (isTRUE(footprint$measurement_reliable)) {
    max(0, footprint$validation_required_height_mm - height_mm)
  } else {
    0
  }
  footprint_hard_loss <- preferences$hard_panel_penalty *
    (footprint_width_violation_mm^2 + footprint_height_violation_mm^2)

  total_loss <- axis_gaps$label_gap_loss +
    panel_loss$panel_minimum_loss +
    facet_loss$facet_loss +
    footprint_hard_loss +
    legend_loss$legend_loss +
    data_density_loss$data_density_loss +
    aspect_loss$aspect_ratio_loss +
    unused_panel_area_loss +
    oversize_loss

  hard_violation_mm <- max(
    axis_gaps$x_label_violation_mm,
    axis_gaps$y_label_violation_mm,
    panel_loss$panel_width_violation_mm,
    panel_loss$panel_height_violation_mm,
    panel_loss$panel_area_violation_mm,
    facet_loss$facet_panel_width_violation_mm,
    facet_loss$facet_panel_height_violation_mm,
    footprint_width_violation_mm,
    footprint_height_violation_mm,
    na.rm = TRUE
  )
  hard_loss <- axis_gaps$hard_loss +
    panel_loss$hard_loss +
    facet_loss$hard_loss +
    footprint_hard_loss

  list(
    width_mm = width_mm,
    height_mm = height_mm,
    preferred_width_mm = preferred_width_mm,
    preferred_height_mm = preferred_height_mm,
    inner_target_width_mm = footprint$inner_target_width_mm,
    inner_target_height_mm = footprint$inner_target_height_mm,
    required_width_mm = footprint$required_width_mm,
    required_height_mm = footprint$required_height_mm,
    width_limiting_constraint = footprint$width_limiting_constraint,
    height_limiting_constraint = footprint$height_limiting_constraint,
    footprint_measurement_reliable = footprint$measurement_reliable,
    target_panel_aspect = scalar_or_default(profile$geometry$target_panel_aspect, NA_real_),
    fixed_width_mm = fixed_size$left_mm + fixed_size$right_mm,
    fixed_height_mm = fixed_size$top_mm + fixed_size$bottom_mm,
    panel_width_mm = panel_width_mm,
    panel_height_mm = panel_height_mm,
    effective_panel_width_mm = effective_panel$effective_panel_width_mm,
    effective_panel_height_mm = effective_panel$effective_panel_height_mm,
    unused_panel_area_mm2 = effective_panel$unused_panel_area_mm2,
    n_marks_per_panel = profile$density$n_points_per_panel_estimate,
    n_point_marks_per_panel = profile$geometry$n_points_per_panel_in_point_layers,
    n_nonempty_text_labels = profile$geometry$n_nonempty_text_labels,
    n_panel_rows = profile$geometry$n_panel_rows,
    n_panel_cols = profile$geometry$n_panel_cols,
    n_panels = profile$geometry$n_panels,
    legend_width_mm = profile$geometry$legend_width_mm,
    legend_height_mm = profile$geometry$legend_height_mm,
    legend_area_mm2 = profile$geometry$legend_area_mm2,
    min_x_label_gap_mm = axis_gaps$min_x_label_gap_mm,
    min_y_label_gap_mm = axis_gaps$min_y_label_gap_mm,
    x_label_violation_mm = axis_gaps$x_label_violation_mm,
    y_label_violation_mm = axis_gaps$y_label_violation_mm,
    panel_width_violation_mm = panel_loss$panel_width_violation_mm,
    panel_height_violation_mm = panel_loss$panel_height_violation_mm,
    panel_area_violation_mm = panel_loss$panel_area_violation_mm,
    facet_panel_width_violation_mm = facet_loss$facet_panel_width_violation_mm,
    facet_panel_height_violation_mm = facet_loss$facet_panel_height_violation_mm,
    footprint_width_violation_mm = footprint_width_violation_mm,
    footprint_height_violation_mm = footprint_height_violation_mm,
    inner_footprint_width_violation_mm = inner_footprint_width_violation_mm,
    inner_footprint_height_violation_mm = inner_footprint_height_violation_mm,
    label_gap_loss = axis_gaps$label_gap_loss,
    panel_minimum_loss = panel_loss$panel_minimum_loss,
    facet_loss = facet_loss$facet_loss,
    legend_loss = legend_loss$legend_loss,
    data_density_loss = data_density_loss$data_density_loss,
    aspect_ratio_loss = aspect_loss$aspect_ratio_loss,
    unused_panel_area_loss = unused_panel_area_loss,
    clipping_risk_loss = 0,
    oversize_loss = oversize_loss,
    hard_violation_mm = hard_violation_mm,
    hard_loss = hard_loss,
    total_loss = total_loss,
    warnings = c(axis_gaps$warnings, facet_loss$warnings, legend_loss$warnings, data_density_loss$warnings, aspect_loss$warnings)
  )
}

estimate_required_plot_footprint <- function(profile, fixed_size, preferences) {
  fixed_width_mm <- scalar_or_default(fixed_size$left_mm, 0) +
    scalar_or_default(fixed_size$right_mm, 0)
  fixed_height_mm <- scalar_or_default(fixed_size$top_mm, 0) +
    scalar_or_default(fixed_size$bottom_mm, 0)
  panel_cols <- max(1, scalar_or_default(profile$panels$n_panel_cols, 1))
  panel_rows <- max(1, scalar_or_default(profile$panels$n_panel_rows, 1))

  axis_width <- required_axis_panel_span_mm(profile, "x", preferences$min_label_gap_mm)
  axis_height <- required_axis_panel_span_mm(profile, "y", preferences$min_label_gap_mm)
  min_panel_width <- preferences$min_panel_width_mm * panel_cols
  min_panel_height <- preferences$min_panel_height_mm * panel_rows
  inner_label_gap_mm <- scalar_or_default(preferences$inner_min_label_gap_mm, preferences$min_label_gap_mm)
  inner_min_panel_width_mm <- scalar_or_default(
    preferences$inner_min_panel_width_mm,
    preferences$min_panel_width_mm
  )
  inner_min_panel_height_mm <- scalar_or_default(
    preferences$inner_min_panel_height_mm,
    preferences$min_panel_height_mm
  )
  validation_axis_width <- required_axis_panel_span_mm(profile, "x", inner_label_gap_mm)
  validation_axis_height <- required_axis_panel_span_mm(profile, "y", inner_label_gap_mm)
  validation_min_panel_width <- inner_min_panel_width_mm * panel_cols
  validation_min_panel_height <- inner_min_panel_height_mm * panel_rows

  content_limits <- adjusted_panel_limits_for_density(profile, preferences)
  inner_content <- estimate_inner_content_multipliers(profile)
  cell_text_requirements <- required_regular_grid_cell_text_span_mm(
    profile,
    gap_mm = preferences$min_label_gap_mm
  )
  panel_width_candidates <- c(
    panel_minimum = min_panel_width,
    axis_labels = axis_width * panel_cols,
    cell_text = cell_text_requirements$width_mm * panel_cols
  )
  panel_height_candidates <- c(
    panel_minimum = min_panel_height,
    axis_labels = axis_height * panel_rows,
    cell_text = cell_text_requirements$height_mm * panel_rows
  )
  panel_width_candidates[!is.finite(panel_width_candidates)] <- 0
  panel_height_candidates[!is.finite(panel_height_candidates)] <- 0
  validation_panel_width_candidates <- c(
    panel_minimum = validation_min_panel_width,
    axis_labels = validation_axis_width * panel_cols,
    cell_text = cell_text_requirements$width_mm * panel_cols
  )
  validation_panel_height_candidates <- c(
    panel_minimum = validation_min_panel_height,
    axis_labels = validation_axis_height * panel_rows,
    cell_text = cell_text_requirements$height_mm * panel_rows
  )
  validation_panel_width_candidates[!is.finite(validation_panel_width_candidates)] <- 0
  validation_panel_height_candidates[!is.finite(validation_panel_height_candidates)] <- 0

  base_required_panel_width <- max(panel_width_candidates)
  base_required_panel_height <- max(panel_height_candidates)
  panel_width_constraint <- names(panel_width_candidates)[which.max(panel_width_candidates)]
  panel_height_constraint <- names(panel_height_candidates)[which.max(panel_height_candidates)]
  required_panel_width <- base_required_panel_width
  required_panel_height <- base_required_panel_height
  validation_panel_width <- max(validation_panel_width_candidates)
  validation_panel_height <- max(validation_panel_height_candidates)
  target_aspect <- scalar_or_default(profile$geometry$target_panel_aspect, NA_real_)
  aspect_adjusted <- FALSE
  if (is.finite(target_aspect) && target_aspect > 0 &&
      required_panel_width > 0 && required_panel_height > 0) {
    actual_aspect <- required_panel_height / required_panel_width
    if (actual_aspect < target_aspect) {
      required_panel_height <- required_panel_width * target_aspect
      aspect_adjusted <- TRUE
    } else if (actual_aspect > target_aspect) {
      required_panel_width <- required_panel_height / target_aspect
      aspect_adjusted <- TRUE
    }
  }
  if (is.finite(target_aspect) && target_aspect > 0 &&
      validation_panel_width > 0 && validation_panel_height > 0) {
    validation_aspect <- validation_panel_height / validation_panel_width
    if (validation_aspect < target_aspect) {
      validation_panel_height <- validation_panel_width * target_aspect
    } else if (validation_aspect > target_aspect) {
      validation_panel_width <- validation_panel_height / target_aspect
    }
  }

  legends <- profile$component_sizes[profile$component_sizes$type == "legend", , drop = FALSE]
  legend_width_mm <- if (nrow(legends) > 0) max(legends$width_mm, na.rm = TRUE) else 0
  legend_height_mm <- if (nrow(legends) > 0) max(legends$height_mm, na.rm = TRUE) else 0
  wide_text <- profile$text_grobs[
    profile$text_grobs$component_type %in% c("title", "xlab", "strip"),
    , drop = FALSE
  ]
  tall_text <- profile$text_grobs[
    profile$text_grobs$component_type %in% c("title", "ylab", "strip"),
    , drop = FALSE
  ]
  text_width_mm <- if (nrow(wide_text) > 0) max(wide_text$width_mm, na.rm = TRUE) else 0
  text_height_mm <- if (nrow(tall_text) > 0) max(tall_text$height_mm, na.rm = TRUE) else 0
  component_width_mm <- max(
    legend_width_mm + if (legend_width_mm > 0) 8 else 0,
    text_width_mm + 2,
    na.rm = TRUE
  )
  component_height_mm <- max(
    legend_height_mm + if (legend_height_mm > 0) 4 else 0,
    text_height_mm + 2,
    na.rm = TRUE
  )
  legend_width_scale <- 0.64 - 0.15 * min(1, max(0, (legend_width_mm - 50) / 40))
  validation_component_width_mm <- max(
    (legend_width_mm + if (legend_width_mm > 0) 8 else 0) * legend_width_scale,
    (text_width_mm + 2) * 0.65,
    na.rm = TRUE
  )
  validation_component_height_mm <- max(
    (legend_height_mm + if (legend_height_mm > 0) 4 else 0) * 0.65,
    (text_height_mm + 2) * 0.65,
    na.rm = TRUE
  )
  if (!is.finite(component_width_mm)) component_width_mm <- 0
  if (!is.finite(component_height_mm)) component_height_mm <- 0
  if (!is.finite(validation_component_width_mm)) validation_component_width_mm <- 0
  if (!is.finite(validation_component_height_mm)) validation_component_height_mm <- 0

  width_candidates <- c(
    stats::setNames(fixed_width_mm + base_required_panel_width, panel_width_constraint),
    outer_components = component_width_mm
  )
  height_candidates <- c(
    stats::setNames(fixed_height_mm + base_required_panel_height, panel_height_constraint),
    outer_components = component_height_mm
  )
  if (aspect_adjusted) {
    if (required_panel_width > base_required_panel_width) {
      width_candidates <- c(width_candidates, aspect_ratio = fixed_width_mm + required_panel_width)
    }
    if (required_panel_height > base_required_panel_height) {
      height_candidates <- c(height_candidates, aspect_ratio = fixed_height_mm + required_panel_height)
    }
  }

  required_width_mm <- max(width_candidates)
  required_height_mm <- max(height_candidates)
  validation_required_width_mm <- max(
    inner_content$validation_fixed_width_scale * fixed_width_mm + validation_panel_width,
    validation_component_width_mm
  )
  validation_required_height_mm <- max(
    inner_content$validation_fixed_height_scale * fixed_height_mm + validation_panel_height,
    validation_component_height_mm
  )
  preferred_panel_width <- max(
    required_panel_width,
    content_limits$preferred_panel_width_mm * panel_cols
  )
  preferred_panel_height <- max(
    required_panel_height,
    content_limits$preferred_panel_height_mm * panel_rows
  )
  if (is.finite(target_aspect) && target_aspect > 0) {
    preferred_aspect <- preferred_panel_height / preferred_panel_width
    if (preferred_aspect < target_aspect) {
      preferred_panel_height <- preferred_panel_width * target_aspect
    } else if (preferred_aspect > target_aspect) {
      preferred_panel_width <- preferred_panel_height / target_aspect
    }
  }
  preferred_width_mm <- max(required_width_mm, fixed_width_mm + preferred_panel_width)
  preferred_height_mm <- max(required_height_mm, fixed_height_mm + preferred_panel_height)
  inner_target_width_mm <- required_width_mm * inner_content$width_multiplier
  inner_target_height_mm <- required_height_mm * inner_content$height_multiplier
  measurement_reliable <- is.finite(required_width_mm) && required_width_mm > 0 &&
    is.finite(required_height_mm) && required_height_mm > 0 &&
    is.finite(validation_required_width_mm) && validation_required_width_mm > 0 &&
    is.finite(validation_required_height_mm) && validation_required_height_mm > 0 &&
    is.finite(fixed_width_mm) && is.finite(fixed_height_mm)

  list(
    required_width_mm = required_width_mm,
    required_height_mm = required_height_mm,
    preferred_width_mm = preferred_width_mm,
    preferred_height_mm = preferred_height_mm,
    inner_target_width_mm = inner_target_width_mm,
    inner_target_height_mm = inner_target_height_mm,
    validation_required_width_mm = validation_required_width_mm,
    validation_required_height_mm = validation_required_height_mm,
    width_limiting_constraint = names(width_candidates)[which.max(width_candidates)],
    height_limiting_constraint = names(height_candidates)[which.max(height_candidates)],
    measurement_reliable = measurement_reliable
  )
}

required_regular_grid_cell_text_span_mm <- function(profile, gap_mm = 0) {
  regular_grid_score <- min(1, max(0, scalar_or_default(profile$geometry$regular_grid_score, 0)))
  n_annotations <- scalar_or_default(profile$geometry$n_nonempty_text_labels, 0)
  if (regular_grid_score <= 0 || n_annotations <= 0) {
    return(list(width_mm = 0, height_mm = 0))
  }

  n_x <- max(1, scalar_or_default(profile$geometry$max_unique_x_per_panel, 1))
  n_y <- max(1, scalar_or_default(profile$geometry$max_unique_y_per_panel, 1))
  text_width_mm <- max(0, scalar_or_default(profile$geometry$max_panel_text_width_mm, 0))
  text_height_mm <- max(0, scalar_or_default(profile$geometry$max_panel_text_height_mm, 0))
  cell_count <- max(1, n_x * n_y)
  grid_size_score <- min(1, max(0, (cell_count - 36) / 108))
  safe_gap_mm <- max(0, scalar_or_default(gap_mm, 0)) *
    (0.25 + 0.75 * grid_size_score)

  list(
    width_mm = regular_grid_score * n_x * (text_width_mm + safe_gap_mm),
    height_mm = regular_grid_score * n_y * (text_height_mm + safe_gap_mm)
  )
}

required_axis_panel_span_mm <- function(profile, axis = c("x", "y"), min_gap_mm = 0) {
  axis <- match.arg(axis)
  component_types <- if (axis == "x") c("axis_b", "axis_t") else c("axis_l", "axis_r")
  labels <- profile$text_grobs[profile$text_grobs$component_type %in% component_types, , drop = FALSE]
  if (nrow(labels) <= 1) return(0)

  group_key <- interaction(labels$component_type, labels$component_name, drop = TRUE, lex.order = TRUE)
  groups <- split(labels, group_key)
  requirements <- vapply(groups, function(group) {
    if (nrow(group) <= 1) return(0)
    positions <- profile$axis_positions[[axis]]$positions
    if (length(positions) != nrow(group) || any(!is.finite(positions))) {
      positions <- seq(0, 1, length.out = nrow(group))
    }
    order_index <- order(positions)
    positions <- positions[order_index]
    group <- group[order_index, , drop = FALSE]
    position_gaps <- diff(positions)
    valid <- is.finite(position_gaps) & position_gaps > 0
    if (!any(valid)) return(Inf)
    adjacent_extent <- adjacent_axis_text_requirements(group, axis, min_gap_mm)
    max(adjacent_extent[valid] / position_gaps[valid], na.rm = TRUE)
  }, numeric(1))
  if (any(is.infinite(requirements))) return(Inf)
  max(requirements, 0, na.rm = TRUE)
}

adjacent_axis_text_requirements <- function(axis_labels, axis = c("x", "y"), gap_mm = 0) {
  axis <- match.arg(axis)
  if (nrow(axis_labels) <= 1) return(numeric())
  projected_sizes <- vapply(seq_len(nrow(axis_labels)), function(index) {
    projected <- projected_text_extent(
      axis_labels$width_mm[index], axis_labels$height_mm[index], axis_labels$rotation[index]
    )
    if (axis == "x") projected$width_mm else projected$height_mm
  }, numeric(1))
  bounding_requirement <- 0.5 * utils::head(projected_sizes, -1) +
    0.5 * utils::tail(projected_sizes, -1) + gap_mm

  rotations_a <- utils::head(axis_labels$rotation, -1)
  rotations_b <- utils::tail(axis_labels$rotation, -1)
  rotation_difference <- abs(((rotations_a - rotations_b + 180) %% 360) - 180)
  theta <- 0.5 * (rotations_a + rotations_b) * pi / 180
  normal_component <- if (axis == "x") abs(sin(theta)) else abs(cos(theta))
  intrinsic_thickness <- 0.5 * utils::head(axis_labels$height_mm, -1) +
    0.5 * utils::tail(axis_labels$height_mm, -1)
  parallel_requirement <- rep(Inf, length(bounding_requirement))
  use_parallel_geometry <- rotation_difference < 1e-6 & normal_component > 1e-6
  parallel_requirement[use_parallel_geometry] <-
    intrinsic_thickness[use_parallel_geometry] / normal_component[use_parallel_geometry] + gap_mm

  pmin(bounding_requirement, parallel_requirement)
}

estimate_effective_panel_size <- function(profile, panel_width_mm, panel_height_mm) {
  target_aspect <- profile$geometry$target_panel_aspect

  if (is.null(target_aspect) || !is.finite(target_aspect) || target_aspect <= 0 ||
      panel_width_mm <= 0 || panel_height_mm <= 0) {
    return(list(
      effective_panel_width_mm = panel_width_mm,
      effective_panel_height_mm = panel_height_mm,
      unused_panel_area_mm2 = 0
    ))
  }

  actual_aspect <- panel_height_mm / panel_width_mm
  if (actual_aspect > target_aspect) {
    effective_panel_width_mm <- panel_width_mm
    effective_panel_height_mm <- panel_width_mm * target_aspect
  } else {
    effective_panel_width_mm <- panel_height_mm / target_aspect
    effective_panel_height_mm <- panel_height_mm
  }

  effective_panel_width_mm <- min(panel_width_mm, max(0, effective_panel_width_mm))
  effective_panel_height_mm <- min(panel_height_mm, max(0, effective_panel_height_mm))

  unused_panel_area_mm2 <- max(
    0,
    panel_width_mm * panel_height_mm -
      effective_panel_width_mm * effective_panel_height_mm
  )

  list(
    effective_panel_width_mm = effective_panel_width_mm,
    effective_panel_height_mm = effective_panel_height_mm,
    unused_panel_area_mm2 = unused_panel_area_mm2
  )
}

estimate_unused_panel_area_loss <- function(unused_panel_area_mm2, page_area_mm2, preferences) {
  if (!is.finite(unused_panel_area_mm2) || unused_panel_area_mm2 <= 0) {
    return(0)
  }

  scalar_or_default(preferences$unused_panel_area_penalty, 0) *
    unused_panel_area_mm2^2 / max(page_area_mm2, 1)
}

estimate_axis_label_gaps <- function(profile, panel_width_mm, panel_height_mm, preferences) {
  text_grobs <- profile$text_grobs

  x_labels <- text_grobs[text_grobs$component_type %in% c("axis_b", "axis_t"), , drop = FALSE]
  y_labels <- text_grobs[text_grobs$component_type %in% c("axis_l", "axis_r"), , drop = FALSE]

  x_gap <- estimate_one_axis_gap(
    axis_labels = x_labels,
    axis_positions = profile$axis_positions$x,
    available_mm = panel_width_mm,
    axis = "x",
    preferences = preferences
  )

  y_gap <- estimate_one_axis_gap(
    axis_labels = y_labels,
    axis_positions = profile$axis_positions$y,
    available_mm = panel_height_mm,
    axis = "y",
    preferences = preferences
  )

  label_gap_loss <- x_gap$loss + y_gap$loss
  hard_loss <- x_gap$hard_loss + y_gap$hard_loss

  list(
    min_x_label_gap_mm = x_gap$min_gap_mm,
    min_y_label_gap_mm = y_gap$min_gap_mm,
    x_label_violation_mm = x_gap$violation_mm,
    y_label_violation_mm = y_gap$violation_mm,
    label_gap_loss = label_gap_loss,
    hard_loss = hard_loss,
    warnings = c(x_gap$warnings, y_gap$warnings)
  )
}

estimate_one_axis_gap <- function(axis_labels, axis_positions = NULL, available_mm, axis, preferences) {
  if (nrow(axis_labels) <= 1) {
    return(list(min_gap_mm = Inf, violation_mm = 0, loss = 0, hard_loss = 0, warnings = character()))
  }

  label_count <- nrow(axis_labels)
  position_values <- NULL
  fallback <- TRUE
  if (!is.null(axis_positions$positions) &&
      length(axis_positions$positions) == label_count &&
      all(is.finite(axis_positions$positions))) {
    position_values <- as.numeric(axis_positions$positions)
    fallback <- isTRUE(axis_positions$fallback)
  }
  if (is.null(position_values)) {
    position_values <- if (label_count == 1) 0.5 else seq(0, 1, length.out = label_count)
  }

  order_index <- order(position_values)
  position_values <- position_values[order_index]
  axis_labels <- axis_labels[order_index, , drop = FALSE]

  centre_distances_mm <- diff(position_values) * available_mm
  adjacent_required_mm <- adjacent_axis_text_requirements(axis_labels, axis, gap_mm = 0)
  gap_values_mm <- centre_distances_mm - adjacent_required_mm
  min_gap_mm <- min(gap_values_mm, na.rm = TRUE)

  violation_mm <- max(0, preferences$min_label_gap_mm - min_gap_mm)

  if (violation_mm > 0) {
    loss <- preferences$hard_label_penalty * violation_mm^2
  } else {
    loss <- preferences$soft_excess_gap_penalty *
      max(0, min_gap_mm - preferences$target_label_gap_mm)^2
  }
  hard_loss <- preferences$hard_label_penalty * violation_mm^2

  warnings <- character()
  if (violation_mm > 0) {
    warnings <- paste0(axis, "-axis label gap below minimum by ", round(violation_mm, 2), " mm.")
  } else if (fallback && label_count > 1) {
    warnings <- paste0(axis, "-axis label positions used equal-spacing fallback.")
  }

  list(
    min_gap_mm = min_gap_mm,
    violation_mm = violation_mm,
    loss = loss,
    hard_loss = hard_loss,
    warnings = warnings
  )
}

estimate_panel_minimum_loss <- function(
    panel_width_mm,
    panel_height_mm,
    min_panel_width_mm,
    min_panel_height_mm,
    min_panel_area_mm2,
    preferences) {

  panel_width_violation_mm <- max(0, min_panel_width_mm - panel_width_mm)
  panel_height_violation_mm <- max(0, min_panel_height_mm - panel_height_mm)
  panel_area_violation_mm <- max(
    0,
    sqrt(min_panel_area_mm2) - sqrt(max(0, panel_width_mm * panel_height_mm))
  )

  panel_minimum_loss <- preferences$hard_panel_penalty *
    (panel_width_violation_mm^2 + panel_height_violation_mm^2 + panel_area_violation_mm^2)
  hard_loss <- panel_minimum_loss

  list(
    panel_width_violation_mm = panel_width_violation_mm,
    panel_height_violation_mm = panel_height_violation_mm,
    panel_area_violation_mm = panel_area_violation_mm,
    panel_minimum_loss = panel_minimum_loss,
    hard_loss = hard_loss
  )
}

estimate_facet_loss <- function(profile, panel_width_mm, panel_height_mm, preferences) {
  n_panel_cols <- max(1, profile$panels$n_panel_cols)
  n_panel_rows <- max(1, profile$panels$n_panel_rows)

  facet_panel_width_mm <- panel_width_mm / n_panel_cols
  facet_panel_height_mm <- panel_height_mm / n_panel_rows

  min_facet_panel_width_mm <- 0.75 * preferences$min_panel_width_mm
  min_facet_panel_height_mm <- 0.75 * preferences$min_panel_height_mm

  facet_panel_width_violation_mm <- max(0, min_facet_panel_width_mm - facet_panel_width_mm)
  facet_panel_height_violation_mm <- max(0, min_facet_panel_height_mm - facet_panel_height_mm)

  facet_loss <- preferences$hard_facet_penalty *
    (facet_panel_width_violation_mm^2 + facet_panel_height_violation_mm^2)
  hard_loss <- facet_loss

  warnings <- character()
  if (facet_panel_width_violation_mm > 0 || facet_panel_height_violation_mm > 0) {
    warnings <- paste0(
      "Per-facet panel size is below preferred minimum by up to ",
      round(max(facet_panel_width_violation_mm, facet_panel_height_violation_mm), 2),
      " mm."
    )
  }

  list(
    facet_panel_width_violation_mm = facet_panel_width_violation_mm,
    facet_panel_height_violation_mm = facet_panel_height_violation_mm,
    facet_loss = facet_loss,
    hard_loss = hard_loss,
    warnings = warnings
  )
}

horizontal_legend_panel_targets <- function(profile) {
  legend_position <- profile$plot$theme$legend.position
  has_horizontal_legend <- is.character(legend_position) &&
    length(legend_position) > 0 && legend_position[1] %in% c("bottom", "top")
  legend_width_mm <- scalar_or_default(profile$geometry$legend_width_mm, 0)
  legend_height_mm <- scalar_or_default(profile$geometry$legend_height_mm, 0)

  if (!has_horizontal_legend || legend_width_mm <= 0 || legend_height_mm <= 0) {
    return(list(width_mm = 0, height_mm = 0))
  }

  target_width_mm <- 0.75 * legend_width_mm
  target_aspect <- profile$geometry$target_panel_aspect
  target_height_mm <- if (is.finite(target_aspect) && target_aspect > 0) {
    target_width_mm * target_aspect
  } else {
    2 * legend_height_mm
  }

  list(width_mm = target_width_mm, height_mm = target_height_mm)
}

estimate_legend_loss <- function(
    profile,
    width_mm,
    height_mm,
    effective_panel_width_mm = Inf,
    effective_panel_height_mm = Inf) {
  component_sizes <- profile$component_sizes
  legends <- component_sizes[component_sizes$type == "legend", , drop = FALSE]

  if (nrow(legends) == 0) {
    return(list(legend_loss = 0, warnings = character()))
  }

  legend_area_mm2 <- sum(legends$width_mm * legends$height_mm)
  legend_area_fraction <- legend_area_mm2 / max(width_mm * height_mm, 1)

  area_loss <- if (legend_area_fraction > 0.25) {
    100 * (legend_area_fraction - 0.25)^2
  } else {
    0
  }

  panel_targets <- horizontal_legend_panel_targets(profile)
  panel_width_violation_mm <- max(0, panel_targets$width_mm - effective_panel_width_mm)
  panel_height_violation_mm <- max(0, panel_targets$height_mm - effective_panel_height_mm)
  panel_balance_loss <- 10 * (
    panel_width_violation_mm^2 + panel_height_violation_mm^2
  )
  legend_loss <- area_loss + panel_balance_loss

  warnings <- character()
  if (legend_area_fraction > 0.25) {
    warnings <- paste0("Legend consumes ", round(100 * legend_area_fraction, 1), "% of allocated plot area.")
  }
  if (panel_width_violation_mm > 0 || panel_height_violation_mm > 0) {
    warnings <- c(
      warnings,
      paste0(
        "Horizontal legend leaves the effective panel about ",
        round(max(panel_width_violation_mm, panel_height_violation_mm), 1),
        " mm below its legend-balanced target."
      )
    )
  }

  list(legend_loss = legend_loss, warnings = warnings)
}

adjusted_panel_limits_for_density <- function(profile, preferences) {
  n_marks_per_panel <- scalar_or_default(profile$density$n_points_per_panel_estimate, NA_real_)
  n_annotations <- scalar_or_default(profile$geometry$n_nonempty_text_labels, 0)
  n_panels <- max(1, scalar_or_default(profile$panels$n_panels, 1))
  annotations_per_panel <- n_annotations / n_panels

  density_factor <- 1
  if (is.finite(n_marks_per_panel) && n_marks_per_panel > 500) {
    density_factor <- min(1.5, 1 + 0.1 * log2(n_marks_per_panel / 500))
  }
  annotation_factor <- 1
  if (is.finite(annotations_per_panel) && annotations_per_panel > 0) {
    annotation_factor <- min(1.25, 1 + sqrt(annotations_per_panel) / 50)
  }
  preferred_size_factor <- max(density_factor, annotation_factor)

  target_panel_width_mm <- scalar_or_default(
    preferences$target_panel_width_mm,
    2 * preferences$min_panel_width_mm
  )
  target_panel_height_mm <- scalar_or_default(
    preferences$target_panel_height_mm,
    2 * preferences$min_panel_height_mm
  )
  preferred_panel_width_mm <- target_panel_width_mm * preferred_size_factor
  preferred_panel_height_mm <- target_panel_height_mm * preferred_size_factor
  legend_panel_targets <- horizontal_legend_panel_targets(profile)
  preferred_panel_width_mm <- max(preferred_panel_width_mm, legend_panel_targets$width_mm)
  preferred_panel_height_mm <- max(preferred_panel_height_mm, legend_panel_targets$height_mm)

  list(
    min_panel_width_mm = preferences$min_panel_width_mm,
    min_panel_height_mm = preferences$min_panel_height_mm,
    density_factor = density_factor,
    annotation_factor = annotation_factor,
    preferred_size_factor = preferred_size_factor,
    hard_density_factor = 1,
    preferred_panel_width_mm = preferred_panel_width_mm,
    preferred_panel_height_mm = preferred_panel_height_mm,
    preferred_width_multiplier = preferred_panel_width_mm / preferences$min_panel_width_mm,
    preferred_height_multiplier = preferred_panel_height_mm / preferences$min_panel_height_mm
  )
}

estimate_inner_content_multipliers <- function(profile) {
  n_panels <- max(1, scalar_or_default(profile$panels$n_panels, 1))
  n_annotations <- scalar_or_default(profile$geometry$n_nonempty_text_labels, 0)
  annotations_per_panel <- n_annotations / n_panels
  max_unique_x <- scalar_or_default(profile$geometry$max_unique_x_per_panel, NA_real_)
  max_unique_y <- scalar_or_default(profile$geometry$max_unique_y_per_panel, NA_real_)
  safe_unique_x <- if (is.finite(max_unique_x)) max_unique_x else 0
  safe_unique_y <- if (is.finite(max_unique_y)) max_unique_y else 0
  max_layer_rows <- scalar_or_default(
    profile$geometry$max_layer_rows_per_panel,
    profile$density$n_points_per_panel_estimate
  )
  regular_grid_score <- min(1, max(0, scalar_or_default(profile$geometry$regular_grid_score, 0)))
  summary_detail_score <- min(1, max(0, scalar_or_default(profile$geometry$summary_detail_score, 0)))
  bounded_layer_fraction <- min(1, max(0, scalar_or_default(profile$geometry$bounded_layer_fraction, 0)))
  max_glyph_rows <- scalar_or_default(profile$geometry$max_glyph_rows_per_panel, 0)
  total_glyph_rows <- scalar_or_default(profile$geometry$total_glyph_rows, 0)
  max_groups <- max(1, scalar_or_default(profile$geometry$max_groups_per_panel, 1))
  has_trajectory <- min(1, max(0, scalar_or_default(profile$geometry$has_trajectory_content, 0)))
  annotation_suppression <- 1 - min(1, annotations_per_panel / 5)
  x_resolution_score <- if (is.finite(max_unique_x) && max_unique_x > 20) {
    min(1, max(0, log2(max_unique_x / 20) / 2))
  } else {
    0
  }
  sequence_score <- x_resolution_score * has_trajectory * (1 - regular_grid_score) *
    (1 - summary_detail_score) * annotation_suppression
  annotation_fraction <- if (is.finite(max_layer_rows) && max_layer_rows > 0) {
    annotations_per_panel / max_layer_rows
  } else {
    0
  }
  annotation_shrink_score <- min(1, max(0, (annotation_fraction - 0.08) / 0.14)) *
    (1 - regular_grid_score)
  grid_cell_count_score <- if (is.finite(max_layer_rows)) {
    min(1, max(0, (max_layer_rows - 64) / 288))
  } else {
    0
  }
  compact_grid_score <- regular_grid_score * grid_cell_count_score *
    as.numeric(n_panels == 1) * as.numeric(n_annotations == 0)
  matrix_annotation_score <- regular_grid_score * min(1, max(0, annotation_fraction))
  annotation_grid_size_score <- if (is.finite(max_layer_rows)) {
    0.5 + 0.5 * min(1, max(0, (max_layer_rows - 36) / 108))
  } else {
    0.5
  }
  legend_width_mm <- scalar_or_default(profile$geometry$legend_width_mm, 0)
  legend_height_mm <- scalar_or_default(profile$geometry$legend_height_mm, 0)
  legend_shrink_score <- min(1, max(0, (legend_width_mm - 40) / 45))
  summary_legend_score <- summary_detail_score * min(1, max(0, legend_width_mm / 40))
  glyph_reference_rows <- max(max_glyph_rows, total_glyph_rows / sqrt(n_panels))
  glyph_count_score <- if (glyph_reference_rows > 0) {
    0.35 + 0.65 * min(1, max(0, log2(glyph_reference_rows / 8) / 4))
  } else {
    0
  }
  glyph_dense_attenuation <- 1 - min(1, max(0, (max_glyph_rows - 600) / 1200))
  glyph_content_score <- glyph_count_score * glyph_dense_attenuation
  max_panel_text_width_mm <- scalar_or_default(profile$geometry$max_panel_text_width_mm, 0)
  labelled_glyph_score <- glyph_content_score * min(1, annotations_per_panel / 5) *
    min(1, max(0, max_panel_text_width_mm / 8))
  faceted_glyph_score <- glyph_content_score * min(1, log2(n_panels) / 3)
  dense_cloud_score <- as.numeric(max_glyph_rows > 0) *
    min(1, max(0, (max_glyph_rows - 1000) / 4000)) *
    (1 - bounded_layer_fraction)
  series_score <- has_trajectory * min(1, max(0, log2(max_groups) / log2(12)))
  dense_curve_score <- bounded_layer_fraction * (1 - regular_grid_score) *
    min(1, max(0, (safe_unique_x - 150) / 350)) *
    min(1, max(0, (max_layer_rows - 500) / 1000))
  few_x_dense_y_score <- bounded_layer_fraction * (1 - regular_grid_score) *
    min(1, max(0, (12 - safe_unique_x) / 8)) *
    min(1, max(0, if (safe_unique_y > 0) log2(safe_unique_y / 100) / 4 else 0))
  mixed_bound_score <- 4 * bounded_layer_fraction * (1 - bounded_layer_fraction) *
    (1 - regular_grid_score)
  mixed_bound_structure_score <- max(
    min(1, max(0, (safe_unique_x - 15) / 10)),
    if (is.finite(max_unique_y)) {
      1 - min(1, max(0, (safe_unique_y - 8) / 8))
    } else {
      0
    }
  )
  faceted_grid_score <- regular_grid_score * as.numeric(n_panels > 1) *
    min(1, max(0, (max_layer_rows - 50) / 70))
  fixed_width_mm <- scalar_or_default(profile$fixed_size$left_mm, 0) +
    scalar_or_default(profile$fixed_size$right_mm, 0)
  bottom_burden_mm <- scalar_or_default(profile$fixed_size$bottom_mm, 0)
  bottom_axis_burden_score <- min(1, max(0, (bottom_burden_mm - 12) / 12))
  few_category_score <- min(1, max(0, (10 - safe_unique_x) / 5))
  many_y_label_score <- min(1, max(0, (fixed_width_mm - 30) / 35)) *
    min(1, max(0, (safe_unique_y - 8) / 6))
  many_panel_width_score <- min(1, max(0, (n_panels - 12) / 23))
  faceted_glyph_score <- faceted_glyph_score * (1 - many_panel_width_score)
  annotation_shrink_strength <- annotation_shrink_score^3

  width_growth <- max(
    1 + (0.25 + 0.32 * log2(n_panels)) * sequence_score,
    1 + 0.5 * summary_legend_score,
    1 + 0.32 * glyph_content_score,
    1 + 0.5 * faceted_glyph_score,
    1 + 1.5 * dense_cloud_score,
    1 + 0.4 * series_score,
    1 + 0.95 * matrix_annotation_score * annotation_grid_size_score,
    1 + 0.8 * labelled_glyph_score,
    1 + 0.2 * faceted_grid_score,
    1 + 0.2 * bottom_axis_burden_score * few_category_score,
    1 + 0.3 * many_y_label_score
  )
  height_growth <- max(
    1 + (0.12 * log2(n_panels) + 0.1 * bounded_layer_fraction) * (1 - sequence_score)^2,
    1 + 0.5 * summary_legend_score,
    1 + 0.35 * summary_detail_score * (1 - min(1, legend_width_mm / 40)),
    1 + 0.32 * glyph_content_score * (1 - many_y_label_score),
    1 + (0.35 + 0.2 * as.numeric(max_glyph_rows > 0)) * sequence_score *
      as.numeric(n_panels == 1),
    1 + 0.3 * series_score,
    1 + 1.6 * few_x_dense_y_score,
    1 + 0.5 * faceted_glyph_score,
    1 + 0.22 * mixed_bound_score * mixed_bound_structure_score,
    1 + 0.35 * faceted_grid_score,
    1 + 0.5 * matrix_annotation_score * annotation_grid_size_score,
    1 + 1.1 * bottom_axis_burden_score * few_category_score,
    1 + 0.56 * dense_cloud_score,
    1 + 0.8 * labelled_glyph_score
  )
  width_shrink <- min(
    1 - 0.5 * legend_shrink_score,
    1 - 0.35 * compact_grid_score,
    1 - 0.55 * annotation_shrink_strength,
    1 - 0.33 * dense_curve_score,
    1 - 0.45 * many_panel_width_score
  )
  moderate_sequence_rows_score <- if (is.finite(max_layer_rows)) {
    1 - min(1, max(0, (max_layer_rows - 150) / 500))
  } else {
    0
  }
  legend_free_single_panel_sequence <- sequence_score * moderate_sequence_rows_score *
    as.numeric(n_panels == 1 && legend_width_mm <= 0 && legend_height_mm <= 0)
  sequence_height_coefficient <- (0.4 +
    0.15 * min(1, log2(n_panels) / 2)) *
    (1 - 0.5 * legend_free_single_panel_sequence)
  height_shrink <- min(
    1 - sequence_height_coefficient * sequence_score^1.5,
    1 - 0.35 * compact_grid_score,
    1 - 0.55 * annotation_shrink_strength,
    1 - 0.25 * dense_curve_score,
    1 - 0.1 * dense_cloud_score,
    1 - 0.2 * many_y_label_score
  )
  width_multiplier <- if (summary_legend_score > 0.5) width_growth else width_growth * width_shrink
  height_multiplier <- if (summary_legend_score > 0.5) height_growth else height_growth * height_shrink
  validation_fixed_width_scale <- max(
    0.8,
    1 - 0.1 * legend_shrink_score
  )
  validation_fixed_height_scale <- max(
    0.5,
    1 - 0.5 * sequence_score / (1 + log2(n_panels))
  )
  list(
    width_multiplier = width_multiplier,
    height_multiplier = height_multiplier,
    sequence_score = sequence_score,
    summary_detail_score = summary_detail_score,
    compact_grid_score = compact_grid_score,
    annotation_shrink_score = annotation_shrink_score,
    legend_shrink_score = legend_shrink_score,
    validation_fixed_width_scale = validation_fixed_width_scale,
    validation_fixed_height_scale = validation_fixed_height_scale
  )
}

estimate_data_density_loss <- function(profile, panel_width_mm, panel_height_mm, preferences) {
  limits <- adjusted_panel_limits_for_density(profile, preferences)

  if (limits$preferred_size_factor <= 1) {
    return(list(data_density_loss = 0, warnings = character()))
  }

  width_violation_mm <- max(0, limits$preferred_panel_width_mm - panel_width_mm)
  height_violation_mm <- max(0, limits$preferred_panel_height_mm - panel_height_mm)

  data_density_loss <- 25 * (width_violation_mm^2 + height_violation_mm^2)

  warnings <- paste0(
    "Content-density heuristic inflated preferred panel size by ",
    round(limits$preferred_size_factor, 2),
    "x."
  )

  list(data_density_loss = data_density_loss, warnings = warnings)
}

estimate_aspect_ratio_loss <- function(profile, panel_width_mm, panel_height_mm) {
  if (is.null(profile$geometry$target_panel_aspect) || !is.finite(profile$geometry$target_panel_aspect)) {
    return(list(aspect_ratio_loss = 0, warnings = character()))
  }

  if (panel_width_mm <= 0 || panel_height_mm <= 0) {
    return(list(aspect_ratio_loss = 0, warnings = character()))
  }

  actual_aspect <- panel_height_mm / panel_width_mm
  target_aspect <- profile$geometry$target_panel_aspect

  log_ratio <- abs(log(actual_aspect / target_aspect))
  tolerance <- if (isTRUE(profile$geometry$is_scatter_like)) log(1.20) else log(1.35)
  excess_log_ratio <- max(0, log_ratio - tolerance)

  if (excess_log_ratio == 0) {
    aspect_ratio_loss <- 0
  } else {
    aspect_ratio_loss <- profile$geometry$aspect_penalty * excess_log_ratio^2
  }

  warnings <- character()
  if (aspect_ratio_loss > 20) {
    warnings <- paste0(
      "Panel aspect ratio is far from target ",
      round(target_aspect, 2),
      " for this plot geometry."
    )
  }

  list(aspect_ratio_loss = aspect_ratio_loss, warnings = warnings)
}


# ---- size-frontier.R ----
estimate_size_frontier <- function(
    fit_function,
    page_spec,
    coarse_step_mm = 20,
    refine = TRUE,
    refine_step_mm = 5,
    acceptable_loss_threshold = 10) {
  width_grid_mm <- make_frontier_axis_grid(page_spec$width_mm, coarse_step_mm)
  height_grid_mm <- make_frontier_axis_grid(page_spec$height_mm, coarse_step_mm)
  coarse_grid <- expand.grid(width_mm = width_grid_mm, height_mm = height_grid_mm)
  frontier <- evaluate_frontier_grid(fit_function, coarse_grid)
  frontier <- mark_acceptable_frontier(frontier, acceptable_loss_threshold)

  if (refine && nrow(frontier) > 0) {
    promising_index <- if (any(frontier$acceptable)) {
      which.min(frontier$area_mm2 + ifelse(frontier$acceptable, 0, Inf))
    } else {
      which.min(frontier$total_loss)
    }
    promising <- frontier[promising_index, , drop = FALSE]
    refinement_grid <- make_frontier_refinement_grid(
      width_mm = promising$width_mm,
      height_mm = promising$height_mm,
      page_spec = page_spec,
      radius_mm = coarse_step_mm,
      step_mm = refine_step_mm
    )
    refined <- evaluate_frontier_grid(fit_function, refinement_grid)
    refined <- mark_acceptable_frontier(refined, acceptable_loss_threshold)
    frontier <- rbind(frontier, refined)
    frontier <- frontier[!duplicated(frontier[c("width_mm", "height_mm")]), , drop = FALSE]
    frontier <- mark_acceptable_frontier(frontier, acceptable_loss_threshold)
  }

  rownames(frontier) <- NULL
  frontier
}

make_frontier_axis_grid <- function(limit_mm, step_mm) {
  stepped_values <- if (limit_mm >= step_mm) seq(step_mm, limit_mm, by = step_mm) else numeric()
  values <- c(5, stepped_values, 80, 90, limit_mm)
  unique(sort(values[is.finite(values) & values > 0 & values <= limit_mm]))
}

make_frontier_refinement_grid <- function(width_mm, height_mm, page_spec, radius_mm, step_mm) {
  offsets <- seq(-radius_mm, radius_mm, by = step_mm)
  candidates <- rbind(
    data.frame(width_mm = width_mm + offsets, height_mm = height_mm),
    data.frame(width_mm = width_mm, height_mm = height_mm + offsets),
    data.frame(width_mm = width_mm + offsets, height_mm = height_mm + offsets),
    data.frame(width_mm = width_mm + offsets, height_mm = height_mm - offsets)
  )
  candidates <- candidates[
    candidates$width_mm > 0 & candidates$width_mm <= page_spec$width_mm &
      candidates$height_mm > 0 & candidates$height_mm <= page_spec$height_mm,
    , drop = FALSE
  ]
  unique(candidates)
}

evaluate_frontier_grid <- function(fit_function, grid) {
  fit_rows <- vector("list", nrow(grid))
  for (grid_index in seq_len(nrow(grid))) {
    fit <- fit_function(grid$width_mm[grid_index], grid$height_mm[grid_index])

    fit_rows[[grid_index]] <- data.frame(
      width_mm = fit$width_mm,
      height_mm = fit$height_mm,
      total_loss = fit$total_loss,
      hard_violation_mm = fit$hard_violation_mm,
      hard_loss = scalar_or_default(fit$hard_loss, fit$hard_violation_mm^2),
      panel_width_mm = fit$panel_width_mm,
      panel_height_mm = fit$panel_height_mm
    )
  }
  do.call(rbind, fit_rows)
}

mark_acceptable_frontier <- function(frontier, acceptable_loss_threshold) {
  frontier$acceptable <- frontier$total_loss <= acceptable_loss_threshold &
    frontier$hard_violation_mm <= 0
  frontier$area_mm2 <- frontier$width_mm * frontier$height_mm
  frontier
}

summarise_size_frontier <- function(frontier) {
  acceptable <- frontier[frontier$acceptable, , drop = FALSE]

  if (nrow(acceptable) == 0) {
    best_index <- which.min(frontier$total_loss)
    best <- frontier[best_index, , drop = FALSE]

    return(list(
      min_acceptable_width_mm = NA_real_,
      min_acceptable_height_mm = NA_real_,
      min_acceptable_area_mm2 = NA_real_,
      preferred_width_mm = best$width_mm,
      preferred_height_mm = best$height_mm,
      impossible = TRUE,
      impossible_on_page = TRUE,
      frontier_points = acceptable
    ))
  }

  acceptable$area_mm2 <- acceptable$width_mm * acceptable$height_mm
  min_area_index <- which.min(acceptable$area_mm2)
  preferred <- acceptable[min_area_index, , drop = FALSE]

  list(
    min_acceptable_width_mm = min(acceptable$width_mm),
    min_acceptable_height_mm = min(acceptable$height_mm),
    min_acceptable_area_mm2 = min(acceptable$area_mm2),
    preferred_width_mm = preferred$width_mm,
    preferred_height_mm = preferred$height_mm,
    impossible = FALSE,
    impossible_on_page = FALSE,
    frontier_points = acceptable
  )
}


