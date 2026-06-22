# ---- fit-loss.R ----
make_plot_fit_function <- function(profile, preferences) {
  force(profile)
  force(preferences)

  function(width_mm, height_mm) {
    evaluate_plot_fit(
      profile = profile,
      width_mm = width_mm,
      height_mm = height_mm,
      preferences = preferences
    )
  }
}

evaluate_plot_fit <- function(profile, width_mm, height_mm, preferences) {
  measured <- measure_plot_at_size(profile, width_mm, height_mm, page_spec = profile$page)
  fixed_size <- measured$fixed_size

  panel_width_mm <- measured$panel_width_mm
  panel_height_mm <- measured$panel_height_mm
  effective_panel <- estimate_effective_panel_size(profile, panel_width_mm, panel_height_mm)

  density_limits <- adjusted_panel_limits_for_density(profile, preferences)
  axis_gaps <- estimate_axis_label_gaps(
    profile,
    effective_panel$effective_panel_width_mm,
    effective_panel$effective_panel_height_mm,
    preferences
  )
  panel_loss <- estimate_panel_minimum_loss(
    effective_panel$effective_panel_width_mm,
    effective_panel$effective_panel_height_mm,
    density_limits$min_panel_width_mm,
    density_limits$min_panel_height_mm,
    preferences
  )
  facet_loss <- estimate_facet_loss(
    profile,
    effective_panel$effective_panel_width_mm,
    effective_panel$effective_panel_height_mm,
    preferences
  )
  legend_loss <- estimate_legend_loss(profile, width_mm, height_mm)
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

  preferred_width_mm <- fixed_size$left_mm + fixed_size$right_mm + density_limits$preferred_width_multiplier * density_limits$min_panel_width_mm
  preferred_height_mm <- fixed_size$top_mm + fixed_size$bottom_mm + density_limits$preferred_height_multiplier * density_limits$min_panel_height_mm
  preferred_area_mm2 <- preferred_width_mm * preferred_height_mm
  allocated_area_mm2 <- width_mm * height_mm
  page_area_mm2 <- profile$page$width_mm * profile$page$height_mm

  oversize_loss <- preferences$soft_oversize_penalty *
    max(0, allocated_area_mm2 - preferred_area_mm2)^2 / max(page_area_mm2, 1)

  total_loss <- axis_gaps$label_gap_loss +
    panel_loss$panel_minimum_loss +
    facet_loss$facet_loss +
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
    facet_loss$facet_panel_width_violation_mm,
    facet_loss$facet_panel_height_violation_mm,
    na.rm = TRUE
  )
  hard_loss <- axis_gaps$hard_loss +
    panel_loss$hard_loss +
    facet_loss$hard_loss

  list(
    width_mm = width_mm,
    height_mm = height_mm,
    preferred_width_mm = preferred_width_mm,
    preferred_height_mm = preferred_height_mm,
    fixed_width_mm = fixed_size$left_mm + fixed_size$right_mm,
    fixed_height_mm = fixed_size$top_mm + fixed_size$bottom_mm,
    panel_width_mm = panel_width_mm,
    panel_height_mm = panel_height_mm,
    effective_panel_width_mm = effective_panel$effective_panel_width_mm,
    effective_panel_height_mm = effective_panel$effective_panel_height_mm,
    unused_panel_area_mm2 = effective_panel$unused_panel_area_mm2,
    min_x_label_gap_mm = axis_gaps$min_x_label_gap_mm,
    min_y_label_gap_mm = axis_gaps$min_y_label_gap_mm,
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

  projected_sizes <- numeric(nrow(axis_labels))
  for (label_index in seq_len(nrow(axis_labels))) {
    projected <- projected_text_extent(
      width_mm = axis_labels$width_mm[label_index],
      height_mm = axis_labels$height_mm[label_index],
      rotation = axis_labels$rotation[label_index]
    )

    if (axis == "x") {
      projected_sizes[label_index] <- projected$width_mm
    } else {
      projected_sizes[label_index] <- projected$height_mm
    }
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
  projected_sizes <- projected_sizes[order_index]

  centre_distances_mm <- diff(position_values) * available_mm
  adjacent_required_mm <- 0.5 * utils::head(projected_sizes, -1) +
    0.5 * utils::tail(projected_sizes, -1)
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
    preferences) {

  panel_width_violation_mm <- max(0, min_panel_width_mm - panel_width_mm)
  panel_height_violation_mm <- max(0, min_panel_height_mm - panel_height_mm)

  panel_minimum_loss <- preferences$hard_panel_penalty *
    (panel_width_violation_mm^2 + panel_height_violation_mm^2)
  hard_loss <- panel_minimum_loss

  list(
    panel_width_violation_mm = panel_width_violation_mm,
    panel_height_violation_mm = panel_height_violation_mm,
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

estimate_legend_loss <- function(profile, width_mm, height_mm) {
  component_sizes <- profile$component_sizes
  legends <- component_sizes[component_sizes$type == "legend", , drop = FALSE]

  if (nrow(legends) == 0) {
    return(list(legend_loss = 0, warnings = character()))
  }

  legend_area_mm2 <- sum(legends$width_mm * legends$height_mm)
  legend_area_fraction <- legend_area_mm2 / max(width_mm * height_mm, 1)

  legend_loss <- if (legend_area_fraction > 0.25) {
    100 * (legend_area_fraction - 0.25)^2
  } else {
    0
  }

  warnings <- character()
  if (legend_area_fraction > 0.25) {
    warnings <- paste0("Legend consumes ", round(100 * legend_area_fraction, 1), "% of allocated plot area.")
  }

  list(legend_loss = legend_loss, warnings = warnings)
}

adjusted_panel_limits_for_density <- function(profile, preferences) {
  n_points_per_panel <- profile$density$n_points_per_panel_estimate

  density_factor <- 1
  if (is.finite(n_points_per_panel) && n_points_per_panel > 500) {
    density_factor <- min(1.5, sqrt(n_points_per_panel / 500))
  }

  height_factor <- density_factor
  width_factor <- density_factor
  preferred_height_multiplier <- 0.85
  preferred_width_multiplier <- 0.85

  list(
    min_panel_width_mm = preferences$min_panel_width_mm * width_factor,
    min_panel_height_mm = preferences$min_panel_height_mm * height_factor,
    density_factor = density_factor,
    preferred_width_multiplier = preferred_width_multiplier,
    preferred_height_multiplier = preferred_height_multiplier
  )
}

estimate_data_density_loss <- function(profile, panel_width_mm, panel_height_mm, preferences) {
  limits <- adjusted_panel_limits_for_density(profile, preferences)

  if (limits$density_factor <= 1) {
    return(list(data_density_loss = 0, warnings = character()))
  }

  width_violation_mm <- max(0, limits$min_panel_width_mm - panel_width_mm)
  height_violation_mm <- max(0, limits$min_panel_height_mm - panel_height_mm)

  data_density_loss <- 50 * (width_violation_mm^2 + height_violation_mm^2)

  warnings <- paste0(
    "Dense data heuristic inflated minimum panel size by ",
    round(limits$density_factor, 2),
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
estimate_size_frontier <- function(fit_function, page_spec, grid_step_mm = 5, acceptable_loss_threshold = 10) {
  width_grid_mm <- unique(sort(c(
    seq(grid_step_mm, min(page_spec$width_mm, 80), by = 2 * grid_step_mm),
    seq(90, page_spec$width_mm, length.out = 6),
    page_spec$width_mm
  )))
  height_grid_mm <- unique(sort(c(
    seq(grid_step_mm, min(page_spec$height_mm, 80), by = 2 * grid_step_mm),
    seq(90, page_spec$height_mm, length.out = 7),
    page_spec$height_mm
  )))
  width_grid_mm <- width_grid_mm[is.finite(width_grid_mm) & width_grid_mm > 0 & width_grid_mm <= page_spec$width_mm]
  height_grid_mm <- height_grid_mm[is.finite(height_grid_mm) & height_grid_mm > 0 & height_grid_mm <= page_spec$height_mm]

  grid <- expand.grid(
    width_mm = width_grid_mm,
    height_mm = height_grid_mm
  )

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

  frontier <- do.call(rbind, fit_rows)
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


