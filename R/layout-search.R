# ---- layout-candidates.R ----
patchwork_symbols <- function(n) {
  symbol_pool <- c(as.character(1:9), LETTERS, letters)

  if (n > length(symbol_pool)) {
    stop("At most ", length(symbol_pool), " plots are supported in one patchwork design.", call. = FALSE)
  }

  symbol_pool[seq_len(n)]
}

generate_page_assignments <- function(plot_ids, frontiers, allow_multipage, max_pages, keep_plot_order) {
  max_pages <- min(length(plot_ids), max_pages)
  if (!is.finite(max_pages)) {
    max_pages <- length(plot_ids)
  }

  assignments <- list(list(plot_ids))

  if (!allow_multipage || max_pages <= 1 || length(plot_ids) <= 1) {
    return(assignments)
  }

  if (keep_plot_order) {
    for (page_count in 2:max_pages) {
      split_sets <- possible_contiguous_splits(plot_ids, page_count)
      split_sets <- prune_contiguous_splits(split_sets, max_splits = 5)
      assignments <- c(assignments, split_sets)
    }
  }

  demanding_plot_ids <- rank_plots_by_min_area(frontiers)
  for (plot_id in utils::head(demanding_plot_ids, min(3, length(demanding_plot_ids)))) {
    isolated_assignment <- isolate_plot_assignment(plot_ids, plot_id, keep_plot_order)
    assignments <- c(assignments, list(isolated_assignment))
  }

  deduplicate_assignments(assignments)
}

prune_contiguous_splits <- function(assignments, max_splits = 10) {
  if (length(assignments) <= max_splits) {
    return(assignments)
  }

  balance_scores <- vapply(assignments, function(assignment) {
    page_lengths <- vapply(assignment, length, integer(1))
    stats::sd(page_lengths) + 0.01 * max(page_lengths)
  }, numeric(1))

  assignments[order(balance_scores, seq_along(assignments))[seq_len(max_splits)]]
}

possible_contiguous_splits <- function(plot_ids, page_count) {
  if (page_count <= 1) {
    return(list(list(plot_ids)))
  }

  split_positions <- seq_len(length(plot_ids) - 1)
  split_matrix <- utils::combn(split_positions, page_count - 1)

  assignments <- vector("list", ncol(split_matrix))
  for (split_index in seq_len(ncol(split_matrix))) {
    cuts <- c(0, split_matrix[, split_index], length(plot_ids))
    pages <- vector("list", page_count)

    for (page_index in seq_len(page_count)) {
      pages[[page_index]] <- plot_ids[(cuts[page_index] + 1):cuts[page_index + 1]]
    }

    assignments[[split_index]] <- pages
  }

  assignments
}

rank_plots_by_min_area <- function(frontiers) {
  areas <- vapply(frontiers, function(frontier_summary) {
    area <- frontier_summary$min_acceptable_area_mm2
    if (!is.finite(area)) {
      area <- frontier_summary$preferred_width_mm * frontier_summary$preferred_height_mm
    }
    area
  }, numeric(1))

  names(sort(areas, decreasing = TRUE))
}

isolate_plot_assignment <- function(plot_ids, plot_id, keep_plot_order = TRUE) {
  other_plot_ids <- setdiff(plot_ids, plot_id)

  if (keep_plot_order) {
    plot_position <- match(plot_id, plot_ids)
    before <- if (plot_position > 1) {
      plot_ids[seq_len(plot_position - 1)]
    } else {
      character()
    }
    after <- if (plot_position < length(plot_ids)) {
      plot_ids[seq.int(plot_position + 1, length(plot_ids))]
    } else {
      character()
    }

    pages <- list()
    if (length(before) > 0) {
      pages[[length(pages) + 1]] <- before
    }
    pages[[length(pages) + 1]] <- plot_id
    if (length(after) > 0) {
      pages[[length(pages) + 1]] <- after
    }

    return(pages)
  }

  list(plot_id, other_plot_ids)
}

deduplicate_assignments <- function(assignments) {
  keys <- vapply(assignments, function(assignment) {
    paste(vapply(assignment, paste, character(1), collapse = ","), collapse = "|")
  }, character(1))

  assignments[!duplicated(keys)]
}

generate_layout_candidates_for_assignment <- function(
    assignment,
    frontiers,
    max_grid_cols,
    max_grid_rows,
    keep_plot_order,
    search_budget = 500) {

  page_layout_sets <- vector("list", length(assignment))
  for (page_index in seq_along(assignment)) {
    plot_ids <- assignment[[page_index]]
    page_layouts <- c(
      generate_single_plot_spacer_layouts(plot_ids, frontiers, max_grid_cols, max_grid_rows),
      generate_equal_grid_layouts(plot_ids, max_grid_cols, max_grid_rows),
      generate_row_band_layouts(plot_ids, frontiers, max_grid_cols, max_grid_rows, keep_plot_order),
      generate_one_large_plot_layouts(plot_ids, frontiers, max_grid_cols, max_grid_rows)
    )

    page_layout_sets[[page_index]] <- deduplicate_page_layouts(page_layouts)
  }

  layout_grid <- expand.grid(lapply(page_layout_sets, seq_along))
  if (nrow(layout_grid) > search_budget) {
    layout_grid <- layout_grid[seq_len(search_budget), , drop = FALSE]
  }

  candidates <- vector("list", nrow(layout_grid))
  for (candidate_index in seq_len(nrow(layout_grid))) {
    pages <- vector("list", length(assignment))

    for (page_index in seq_along(assignment)) {
      layout_index <- layout_grid[candidate_index, page_index]
      page <- page_layout_sets[[page_index]][[layout_index]]
      page$page_id <- page_index
      pages[[page_index]] <- page
    }

    candidates[[candidate_index]] <- list(
      candidate_id = candidate_index,
      page_assignment = assignment,
      pages = pages,
      score = Inf,
      diagnostics = data.frame()
    )
  }

  candidates
}

generate_scaled_report_layouts <- function(plot_ids, frontiers, max_grid_cols, max_grid_rows) {
  list()
}

generate_single_plot_spacer_layouts <- function(plot_ids, frontiers, max_grid_cols, max_grid_rows) {
  if (length(plot_ids) != 1 || max_grid_cols < 3 || max_grid_rows < 3) {
    return(list())
  }

  symbol <- patchwork_symbols(1)
  geometry <- frontiers[[plot_ids[1]]]$geometry
  target_aspect <- if (!is.null(geometry$target_panel_aspect)) geometry$target_panel_aspect else NA_real_

  layout_matrices <- list()
  if (is.finite(target_aspect) && target_aspect >= 0.75) {
    layout_matrices[[length(layout_matrices) + 1]] <- rbind(
      c(symbol, symbol, "#"),
      c("#", "#", "#"),
      c("#", "#", "#")
    )
  }

  layout_matrices[[length(layout_matrices) + 1]] <- rbind(
    c(symbol, symbol, symbol),
    c("#", "#", "#"),
    c("#", "#", "#")
  )
  layout_matrices[[length(layout_matrices) + 1]] <- rbind(
    c(symbol, symbol, "#"),
    c(symbol, symbol, "#"),
    c("#", "#", "#")
  )

  layouts <- list()
  for (layout_matrix in layout_matrices) {
    page <- make_layout_page(plot_ids, layout_matrix)
    page$height_prior <- c(0.55, 1.0, 1.0)
    if (validate_rectangular_design(page$layout_matrix)) {
      layouts[[length(layouts) + 1]] <- page
    }
  }

  layouts
}

generate_equal_grid_layouts <- function(plot_ids, max_grid_cols, max_grid_rows) {
  plot_count <- length(plot_ids)
  layouts <- list()

  for (cols in seq_len(min(max_grid_cols, plot_count))) {
    rows <- ceiling(plot_count / cols)
    if (rows > max_grid_rows) {
      next
    }

    symbols <- patchwork_symbols(plot_count)
    layout_matrix <- matrix("#", nrow = rows, ncol = cols)
    for (plot_index in seq_len(plot_count)) {
      row_index <- ceiling(plot_index / cols)
      col_index <- ((plot_index - 1) %% cols) + 1
      layout_matrix[row_index, col_index] <- symbols[plot_index]
    }

    page <- make_layout_page(plot_ids, layout_matrix)
    if (validate_rectangular_design(page$layout_matrix)) {
      layouts[[length(layouts) + 1]] <- page
    }
  }

  layouts
}

generate_row_band_layouts <- function(plot_ids, frontiers, max_grid_cols, max_grid_rows, keep_plot_order) {
  plot_count <- length(plot_ids)
  if (plot_count <= 1) {
    return(list())
  }

  layouts <- list()
  if (!keep_plot_order) {
    plot_ids <- intersect(rank_plots_by_min_area(frontiers), plot_ids)
  }
  symbols <- patchwork_symbols(plot_count)

  for (row_count in 2:min(max_grid_rows, plot_count)) {
    groups <- split_plot_ids_into_rows(plot_ids, row_count)
    if (length(groups) != row_count) {
      next
    }

    col_count <- min(max_grid_cols, max(vapply(groups, length, integer(1))) * 2)
    layout_matrix <- matrix("#", nrow = row_count, ncol = col_count)

    for (row_index in seq_along(groups)) {
      row_plot_ids <- groups[[row_index]]
      row_symbols <- symbols[match(row_plot_ids, plot_ids)]
      spans <- row_spans_from_frontiers(row_plot_ids, frontiers, col_count)

      start_col <- 1
      for (plot_index in seq_along(row_plot_ids)) {
        end_col <- min(col_count, start_col + spans[plot_index] - 1)
        layout_matrix[row_index, start_col:end_col] <- row_symbols[plot_index]
        start_col <- end_col + 1
        if (start_col > col_count) {
          break
        }
      }
    }

    page <- make_layout_page(plot_ids, layout_matrix)
    if (validate_rectangular_design(page$layout_matrix)) {
      layouts[[length(layouts) + 1]] <- page
    }
  }

  layouts
}

split_plot_ids_into_rows <- function(plot_ids, row_count) {
  row_indices <- split(seq_along(plot_ids), cut(seq_along(plot_ids), breaks = row_count, labels = FALSE))
  lapply(row_indices, function(indices) plot_ids[indices])
}

row_spans_from_frontiers <- function(row_plot_ids, frontiers, col_count) {
  pressures <- vapply(row_plot_ids, function(plot_id) {
    frontier <- frontiers[[plot_id]]
    width <- frontier$preferred_width_mm
    if (!is.finite(width)) {
      width <- 1
    }
    width
  }, numeric(1))

  raw_spans <- pmax(1, round(col_count * pressures / sum(pressures)))
  span_difference <- col_count - sum(raw_spans)

  while (span_difference != 0) {
    if (span_difference > 0) {
      target <- which.max(pressures / raw_spans)
      raw_spans[target] <- raw_spans[target] + 1
      span_difference <- span_difference - 1
    } else {
      reducible <- which(raw_spans > 1)
      if (length(reducible) == 0) {
        break
      }
      target <- reducible[which.min(pressures[reducible] / raw_spans[reducible])]
      raw_spans[target] <- raw_spans[target] - 1
      span_difference <- span_difference + 1
    }
  }

  raw_spans
}

generate_one_large_plot_layouts <- function(plot_ids, frontiers, max_grid_cols, max_grid_rows) {
  plot_count <- length(plot_ids)
  if (plot_count < 3 || max_grid_cols < 3 || max_grid_rows < 3) {
    return(list())
  }

  demanding_plot_ids <- intersect(rank_plots_by_min_area(frontiers), plot_ids)
  demanding_plot_ids <- utils::head(demanding_plot_ids, min(2, length(demanding_plot_ids)))

  layouts <- list()
  symbols <- patchwork_symbols(plot_count)

  for (large_plot_id in demanding_plot_ids) {
    row_count <- min(max_grid_rows, max(3, ceiling(plot_count / 2)))
    col_count <- min(max_grid_cols, 4)
    layout_matrix <- matrix("#", nrow = row_count, ncol = col_count)

    large_symbol <- symbols[match(large_plot_id, plot_ids)]
    layout_matrix[1:2, 1:2] <- large_symbol

    remaining_plot_ids <- setdiff(plot_ids, large_plot_id)
    remaining_symbols <- symbols[match(remaining_plot_ids, plot_ids)]
    remaining_cells <- which(layout_matrix == "#", arr.ind = TRUE)

    for (plot_index in seq_along(remaining_plot_ids)) {
      if (plot_index > nrow(remaining_cells)) {
        break
      }
      cell <- remaining_cells[plot_index, ]
      layout_matrix[cell[["row"]], cell[["col"]]] <- remaining_symbols[plot_index]
    }

    page <- make_layout_page(plot_ids, layout_matrix)
    if (validate_rectangular_design(page$layout_matrix)) {
      layouts[[length(layouts) + 1]] <- page
    }
  }

  layouts
}

generate_compact_spacer_layouts <- function(plot_ids, frontiers, max_grid_cols, max_grid_rows) {
  list()
}

make_layout_page <- function(plot_ids, layout_matrix) {
  areas <- layout_matrix_to_area_table(layout_matrix, plot_ids)

  list(
    page_id = NA_integer_,
    plot_ids = plot_ids,
    layout_matrix = layout_matrix,
    areas = areas,
    widths = rep(1, ncol(layout_matrix)),
    heights = rep(1, nrow(layout_matrix)),
    height_prior = NULL
  )
}

validate_rectangular_design <- function(layout_matrix) {
  if (!is.matrix(layout_matrix)) {
    return(FALSE)
  }

  symbols <- setdiff(unique(as.vector(layout_matrix)), "#")

  for (symbol in symbols) {
    cells <- which(layout_matrix == symbol, arr.ind = TRUE)
    row_range <- range(cells[, "row"])
    col_range <- range(cells[, "col"])

    expected_n <- length(seq(row_range[1], row_range[2])) *
      length(seq(col_range[1], col_range[2]))
    actual_n <- nrow(cells)

    if (actual_n != expected_n) {
      return(FALSE)
    }
  }

  TRUE
}

layout_matrix_to_area_table <- function(layout_matrix, plot_ids = NULL) {
  symbols <- setdiff(unique(as.vector(layout_matrix)), "#")
  symbol_order <- patchwork_symbols(length(symbols))
  symbols <- symbol_order[symbol_order %in% symbols]

  if (is.null(plot_ids)) {
    plot_ids <- symbols
  }

  rows <- vector("list", length(symbols))
  for (symbol_index in seq_along(symbols)) {
    symbol <- symbols[symbol_index]
    cells <- which(layout_matrix == symbol, arr.ind = TRUE)

    rows[[symbol_index]] <- data.frame(
      plot_id = plot_ids[symbol_index],
      symbol = symbol,
      t = min(cells[, "row"]),
      l = min(cells[, "col"]),
      b = max(cells[, "row"]),
      r = max(cells[, "col"]),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

layout_matrix_to_string <- function(layout_matrix) {
  paste(
    "",
    paste(apply(layout_matrix, 1, paste0, collapse = ""), collapse = "\n"),
    "",
    sep = "\n"
  )
}

deduplicate_page_layouts <- function(page_layouts) {
  if (length(page_layouts) == 0) {
    return(page_layouts)
  }

  keys <- vapply(page_layouts, function(page) {
    paste(as.vector(page$layout_matrix), collapse = "")
  }, character(1))

  page_layouts[!duplicated(keys)]
}


# ---- layout-weights.R ----
optimize_layout_weights <- function(layout_page, fit_functions, page_spec, preferences) {
  n_cols <- ncol(layout_page$layout_matrix)
  n_rows <- nrow(layout_page$layout_matrix)

  widths0 <- if (length(layout_page$widths) == n_cols) layout_page$widths else rep(1, n_cols)
  heights0 <- if (length(layout_page$heights) == n_rows) layout_page$heights else rep(1, n_rows)
  if (!is.null(layout_page$height_prior) && length(layout_page$height_prior) == n_rows) {
    heights0 <- layout_page$height_prior
  }
  theta0 <- log(c(widths0, heights0))

  objective <- function(theta) {
    if (any(!is.finite(theta)) || any(abs(theta) > 8)) {
      return(Inf)
    }

    widths <- clamp_layout_weights(exp(theta[seq_len(n_cols)]))
    heights <- clamp_layout_weights(exp(theta[n_cols + seq_len(n_rows)]))

    page_score <- score_layout_page(
      layout_page = layout_page,
      widths = widths,
      heights = heights,
      fit_functions = fit_functions,
      page_spec = page_spec,
      preferences = preferences
    )

    if (!is.finite(page_score$score)) {
      return(Inf)
    }

    page_score$hard_loss * 1000 + page_score$score
  }

  optimization_steps <- preferences$weight_optimization_steps
  if (is.null(optimization_steps)) {
    optimization_steps <- c(0.7, 0.35, 0.15)
  }
  optimization_passes <- scalar_or_default(preferences$weight_optimization_passes, 4L)
  theta <- optimize_layout_weights_greedy(
    theta0,
    objective,
    steps = optimization_steps,
    max_passes = optimization_passes
  )

  widths <- clamp_layout_weights(exp(theta[seq_len(n_cols)]))
  heights <- clamp_layout_weights(exp(theta[n_cols + seq_len(n_rows)]))

  page_score <- score_layout_page(
    layout_page = layout_page,
    widths = widths,
    heights = heights,
    fit_functions = fit_functions,
    page_spec = page_spec,
    preferences = preferences
  )

  physical_sizes <- layout_weights_to_mm(widths, heights, page_spec)
  layout_page$widths <- as.numeric(widths / mean(widths))
  layout_page$heights <- as.numeric(heights / mean(heights))
  layout_page$col_widths_mm <- physical_sizes$col_widths_mm
  layout_page$row_heights_mm <- physical_sizes$row_heights_mm
  layout_page$score <- page_score$score
  layout_page$soft_score <- page_score$soft_score
  layout_page$hard_loss <- page_score$hard_loss
  layout_page$diagnostics <- page_score$diagnostics
  layout_page$max_hard_violation <- page_score$max_hard_violation
  layout_page$empty_cell_fraction <- page_score$empty_cell_fraction
  layout_page$order_penalty <- 0
  layout_page$complexity_penalty <- page_score$complexity_penalty
  layout_page$row_height_prior_penalty <- page_score$row_height_prior_penalty

  layout_page
}

optimize_layout_weights_greedy <- function(
    theta0,
    objective,
    steps = c(0.7, 0.35, 0.15),
    max_passes = 4L) {
  theta <- theta0
  best_score <- objective(theta)

  for (step in steps) {
    improved <- TRUE
    pass_count <- 0
    while (improved && pass_count < max_passes) {
      pass_count <- pass_count + 1
      improved <- FALSE
      for (index in seq_along(theta)) {
        for (direction in c(-1, 1)) {
          candidate <- theta
          candidate[index] <- candidate[index] + direction * step
          score <- objective(candidate)
          if (is.finite(score) && score + 1e-7 < best_score) {
            theta <- candidate
            best_score <- score
            improved <- TRUE
          }
        }
      }
    }
  }

  theta
}

clamp_layout_weights <- function(weights, lower = 0.25, upper = 4.00) {
  pmin(pmax(weights, lower), upper)
}

layout_weights_to_mm <- function(widths, heights, page_spec) {
  list(
    col_widths_mm = page_spec$width_mm * widths / sum(widths),
    row_heights_mm = page_spec$height_mm * heights / sum(heights)
  )
}

score_layout_page <- function(layout_page, widths, heights, fit_functions, page_spec, preferences) {
  allocations <- allocation_from_layout(layout_page, widths, heights, page_spec)

  rows <- vector("list", nrow(allocations))
  total_fit_loss <- 0
  total_hard_loss <- 0
  max_hard_violation <- 0

  for (allocation_index in seq_len(nrow(allocations))) {
    allocation <- allocations[allocation_index, , drop = FALSE]
    fit <- fit_functions[[allocation$plot_id]](
      allocation$width_mm,
      allocation$height_mm
    )

    total_fit_loss <- total_fit_loss + fit$total_loss
    total_hard_loss <- total_hard_loss + scalar_or_default(fit$hard_loss, fit$hard_violation_mm^2)
    max_hard_violation <- max(max_hard_violation, fit$hard_violation_mm, na.rm = TRUE)

    rows[[allocation_index]] <- data.frame(
      plot_id = allocation$plot_id,
      symbol = allocation$symbol,
      allocated_width_mm = allocation$width_mm,
      allocated_height_mm = allocation$height_mm,
      preferred_width_mm = scalar_or_default(fit$preferred_width_mm, NA_real_),
      preferred_height_mm = scalar_or_default(fit$preferred_height_mm, NA_real_),
      inner_target_width_mm = scalar_or_default(fit$inner_target_width_mm, NA_real_),
      inner_target_height_mm = scalar_or_default(fit$inner_target_height_mm, NA_real_),
      required_width_mm = scalar_or_default(fit$required_width_mm, NA_real_),
      required_height_mm = scalar_or_default(fit$required_height_mm, NA_real_),
      width_limiting_constraint = scalar_or_default(fit$width_limiting_constraint, "unavailable"),
      height_limiting_constraint = scalar_or_default(fit$height_limiting_constraint, "unavailable"),
      footprint_measurement_reliable = isTRUE(fit$footprint_measurement_reliable),
      target_panel_aspect = scalar_or_default(fit$target_panel_aspect, NA_real_),
      fixed_width_mm = scalar_or_default(fit$fixed_width_mm, NA_real_),
      fixed_height_mm = scalar_or_default(fit$fixed_height_mm, NA_real_),
      panel_width_mm = fit$panel_width_mm,
      panel_height_mm = fit$panel_height_mm,
      effective_panel_width_mm = scalar_or_default(fit$effective_panel_width_mm, fit$panel_width_mm),
      effective_panel_height_mm = scalar_or_default(fit$effective_panel_height_mm, fit$panel_height_mm),
      unused_panel_area_mm2 = scalar_or_default(fit$unused_panel_area_mm2, 0),
      n_marks_per_panel = scalar_or_default(fit$n_marks_per_panel, NA_real_),
      n_point_marks_per_panel = scalar_or_default(fit$n_point_marks_per_panel, NA_real_),
      n_nonempty_text_labels = scalar_or_default(fit$n_nonempty_text_labels, NA_real_),
      n_panel_rows = scalar_or_default(fit$n_panel_rows, 1L),
      n_panel_cols = scalar_or_default(fit$n_panel_cols, 1L),
      n_panels = scalar_or_default(fit$n_panels, 1L),
      legend_width_mm = scalar_or_default(fit$legend_width_mm, 0),
      legend_height_mm = scalar_or_default(fit$legend_height_mm, 0),
      legend_area_mm2 = scalar_or_default(fit$legend_area_mm2, 0),
      min_x_label_gap_mm = fit$min_x_label_gap_mm,
      min_y_label_gap_mm = fit$min_y_label_gap_mm,
      x_label_violation_mm = scalar_or_default(fit$x_label_violation_mm, 0),
      y_label_violation_mm = scalar_or_default(fit$y_label_violation_mm, 0),
      panel_width_violation_mm = scalar_or_default(fit$panel_width_violation_mm, 0),
      panel_height_violation_mm = scalar_or_default(fit$panel_height_violation_mm, 0),
      panel_area_violation_mm = scalar_or_default(fit$panel_area_violation_mm, 0),
      facet_panel_width_violation_mm = scalar_or_default(fit$facet_panel_width_violation_mm, 0),
      facet_panel_height_violation_mm = scalar_or_default(fit$facet_panel_height_violation_mm, 0),
      label_gap_loss = fit$label_gap_loss,
      panel_minimum_loss = fit$panel_minimum_loss,
      legend_loss = fit$legend_loss,
      facet_loss = fit$facet_loss,
      data_density_loss = fit$data_density_loss,
      aspect_ratio_loss = scalar_or_default(fit$aspect_ratio_loss, 0),
      unused_panel_area_loss = scalar_or_default(fit$unused_panel_area_loss, 0),
      total_fit_loss = fit$total_loss,
      hard_loss = scalar_or_default(fit$hard_loss, fit$hard_violation_mm^2),
      hard_violation_mm = fit$hard_violation_mm,
      warning = paste(unique(fit$warnings), collapse = " "),
      stringsAsFactors = FALSE
    )
  }

  diagnostics <- do.call(rbind, rows)

  empty_cell_fraction <- mean(layout_page$layout_matrix == "#")
  complexity_penalty <- preferences$complexity_penalty *
    nrow(layout_page$layout_matrix) *
    ncol(layout_page$layout_matrix)
  empty_cell_penalty <- preferences$empty_cell_penalty * empty_cell_fraction
  excess_empty_cell_penalty <- scalar_or_default(preferences$excess_empty_cell_penalty, 0) *
    max(0, empty_cell_fraction - scalar_or_default(preferences$max_empty_fraction, 1))^2
  uneven_weight_penalty <- preferences$uneven_weight_penalty *
    (sum((log(widths) - mean(log(widths)))^2) + sum((log(heights) - mean(log(heights)))^2))
  row_height_prior_penalty <- estimate_row_height_prior_penalty(layout_page, heights, preferences)

  soft_fit_loss <- max(0, total_fit_loss - total_hard_loss)
  score <- soft_fit_loss +
    complexity_penalty +
    empty_cell_penalty +
    excess_empty_cell_penalty +
    uneven_weight_penalty +
    row_height_prior_penalty

  list(
    score = score,
    soft_score = score,
    hard_loss = total_hard_loss,
    diagnostics = diagnostics,
    max_hard_violation = max_hard_violation,
    empty_cell_fraction = empty_cell_fraction,
    complexity_penalty = complexity_penalty,
    row_height_prior_penalty = row_height_prior_penalty
  )
}

estimate_row_height_prior_penalty <- function(layout_page, heights, preferences) {
  prior <- layout_page$height_prior
  if (is.null(prior) || length(prior) != length(heights)) {
    return(0)
  }

  prior <- pmax(as.numeric(prior), 0.05)
  heights <- pmax(as.numeric(heights), 0.05)
  prior <- prior / mean(prior)
  heights <- heights / mean(heights)

  scalar_or_default(preferences$row_height_prior_penalty, 0) *
    sum((log(heights) - log(prior))^2)
}

allocation_from_layout <- function(layout_page, widths, heights, page_spec) {
  areas <- layout_page$areas
  rows <- vector("list", nrow(areas))
  physical_sizes <- layout_weights_to_mm(widths, heights, page_spec)

  for (area_index in seq_len(nrow(areas))) {
    area <- areas[area_index, , drop = FALSE]

    allocated_width_mm <- sum(physical_sizes$col_widths_mm[area$l:area$r])
    allocated_height_mm <- sum(physical_sizes$row_heights_mm[area$t:area$b])

    rows[[area_index]] <- data.frame(
      plot_id = area$plot_id,
      symbol = area$symbol,
      width_mm = allocated_width_mm,
      height_mm = allocated_height_mm,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

scalar_or_default <- function(x, default) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(default)
  }

  x[1]
}

score_layout_candidate <- function(candidate, fit_functions, page_spec, preferences) {
  optimized_pages <- vector("list", length(candidate$pages))
  page_scores <- numeric(length(candidate$pages))
  page_hard_losses <- numeric(length(candidate$pages))
  max_hard_violation <- 0

  for (page_index in seq_along(candidate$pages)) {
    optimized_page <- optimize_layout_weights(
      layout_page = candidate$pages[[page_index]],
      fit_functions = fit_functions,
      page_spec = page_spec,
      preferences = preferences
    )

    optimized_pages[[page_index]] <- optimized_page
    page_scores[page_index] <- optimized_page$score
    page_hard_losses[page_index] <- scalar_or_default(optimized_page$hard_loss, 0)
    max_hard_violation <- max(max_hard_violation, optimized_page$max_hard_violation, na.rm = TRUE)
  }

  candidate$pages <- optimized_pages
  candidate$max_hard_violation <- max_hard_violation
  candidate$hard_loss <- sum(page_hard_losses)
  candidate$soft_score <- score_multipage_solution(page_scores, length(optimized_pages), preferences)
  candidate$score <- candidate$soft_score
  candidate$diagnostics <- make_candidate_layout_diagnostics(candidate, preferences)

  candidate
}


# ---- multipage-search.R ----
score_multipage_solution <- function(page_scores, n_pages, preferences) {
  sum(page_scores) + preferences$multipage_penalty * (n_pages - 1)
}

select_best_solution <- function(plot_ids, frontiers, fit_functions, page_spec, preferences) {
  assignments <- if (!is.null(preferences$page_groups)) {
    list(preferences$page_groups)
  } else {
    generate_page_assignments(
      plot_ids = plot_ids,
      frontiers = frontiers,
      allow_multipage = preferences$allow_multipage,
      max_pages = preferences$max_pages,
      keep_plot_order = scalar_or_default(preferences$keep_plot_order, TRUE)
    )
  }

  scored_candidates <- list()
  candidate_id <- 1L
  candidate_budget <- max(1L, as.integer(preferences$search_budget))
  generated_count <- 0L
  feasible_count <- 0L
  candidates_since_improvement <- 0L
  best_so_far <- c(Inf, Inf, Inf)
  search_started_at <- proc.time()[["elapsed"]]
  progress_interval <- max(1L, ceiling(candidate_budget / 10))
  stop_reason <- "candidate_space_exhausted"
  stop_requested <- FALSE

  candidates_by_assignment <- lapply(assignments, function(assignment) {
    generate_layout_candidates_for_assignment(
      assignment = assignment,
      frontiers = frontiers,
      max_grid_cols = preferences$max_grid_cols,
      max_grid_rows = preferences$max_grid_rows,
      keep_plot_order = scalar_or_default(preferences$keep_plot_order, TRUE),
      search_budget = preferences$search_budget
    )
  })
  generated_count <- sum(vapply(candidates_by_assignment, length, integer(1)))
  primary_quota <- if (length(candidates_by_assignment) == 1) {
    candidate_budget
  } else {
    ceiling(candidate_budget / 2)
  }
  candidate_queue <- utils::head(candidates_by_assignment[[1]], primary_quota)

  if (length(candidates_by_assignment) > 1 && length(candidate_queue) < candidate_budget) {
    alternative_candidates <- candidates_by_assignment[-1]
    max_alternative_candidates <- max(vapply(alternative_candidates, length, integer(1)))
    for (generated_index in seq_len(max_alternative_candidates)) {
      for (assignment_index in seq_along(alternative_candidates)) {
        generated_candidates <- alternative_candidates[[assignment_index]]
        if (generated_index <= length(generated_candidates)) {
          candidate_queue[[length(candidate_queue) + 1L]] <- generated_candidates[[generated_index]]
        }
        if (length(candidate_queue) >= candidate_budget) {
          break
        }
      }
      if (length(candidate_queue) >= candidate_budget) {
        break
      }
    }
  }

  if (length(candidate_queue) < candidate_budget) {
    used_primary <- min(primary_quota, length(candidates_by_assignment[[1]]))
    remaining_primary <- if (used_primary < length(candidates_by_assignment[[1]])) {
      candidates_by_assignment[[1]][seq.int(used_primary + 1L, length(candidates_by_assignment[[1]]))]
    } else {
      list()
    }
    candidate_queue <- c(candidate_queue, remaining_primary)
  }
  candidate_queue <- utils::head(candidate_queue, candidate_budget)

  for (candidate in candidate_queue) {
      candidate$candidate_id <- candidate_id

      scored_candidate <- score_layout_candidate(
        candidate = candidate,
        fit_functions = fit_functions,
        page_spec = page_spec,
        preferences = preferences
      )

      scored_candidates[[length(scored_candidates) + 1]] <- scored_candidate
      candidate_id <- candidate_id + 1L

      candidate_tuple <- c(
        scalar_or_default(scored_candidate$max_hard_violation, Inf),
        scalar_or_default(scored_candidate$hard_loss, Inf),
        scalar_or_default(scored_candidate$soft_score, scored_candidate$score)
      )
      if (candidate_tuple[1] <= 0) {
        feasible_count <- feasible_count + 1L
      }
      if (is_lexicographically_better(candidate_tuple, best_so_far)) {
        best_so_far <- candidate_tuple
        candidates_since_improvement <- 0L
      } else {
        candidates_since_improvement <- candidates_since_improvement + 1L
      }

      scored_count <- length(scored_candidates)
      elapsed_seconds <- proc.time()[["elapsed"]] - search_started_at
      if (isTRUE(preferences$verbose) &&
          (scored_count == 1L || scored_count %% progress_interval == 0L || scored_count == candidate_budget)) {
        message(
          "Scored ", scored_count, "/", candidate_budget,
          " candidates; feasible: ", feasible_count,
          "; best score: ", format(best_so_far[3], digits = 5),
          "; elapsed: ", format(round(elapsed_seconds, 1), nsmall = 1), "s."
        )
      }

      timeout_seconds <- scalar_or_default(preferences$search_timeout_seconds, Inf)
      patience <- scalar_or_default(preferences$early_stop_patience, Inf)
      if (is.finite(timeout_seconds) && elapsed_seconds >= timeout_seconds) {
        stop_reason <- "timeout"
        stop_requested <- TRUE
      } else if (is.finite(patience) && feasible_count >= preferences$return_candidates &&
                 candidates_since_improvement >= patience) {
        stop_reason <- "patience"
        stop_requested <- TRUE
      }
      if (stop_requested) {
        break
      }
  }

  if (length(scored_candidates) == 0) {
    stop("No layout candidates could be generated.", call. = FALSE)
  }
  if (!stop_requested && length(scored_candidates) >= candidate_budget) {
    stop_reason <- "budget"
  }
  search_elapsed <- proc.time()[["elapsed"]] - search_started_at
  if (isTRUE(preferences$verbose)) {
    message(
      "Candidate search stopped: ", stop_reason,
      " after ", length(scored_candidates), " candidate(s) and ",
      format(round(search_elapsed, 1), nsmall = 1), "s."
    )
  }

  hard_violations <- vapply(scored_candidates, function(candidate) {
    scalar_or_default(candidate$max_hard_violation, Inf)
  }, numeric(1))
  hard_losses <- vapply(scored_candidates, function(candidate) {
    scalar_or_default(candidate$hard_loss, Inf)
  }, numeric(1))
  soft_scores <- vapply(scored_candidates, function(candidate) {
    scalar_or_default(candidate$soft_score, candidate$score)
  }, numeric(1))

  candidate_order <- order(hard_violations, hard_losses, soft_scores, seq_along(scored_candidates))
  best_index <- candidate_order[1]
  for (candidate_index in seq_along(scored_candidates)) {
    scored_candidates[[candidate_index]]$selected <- candidate_index == best_index
    scored_candidates[[candidate_index]]$diagnostics <- make_candidate_layout_diagnostics(
      scored_candidates[[candidate_index]],
      preferences
    )
  }
  best_candidate <- scored_candidates[[best_index]]

  ordered_candidates <- scored_candidates[candidate_order]
  list(
    best_candidate = best_candidate,
    candidates = utils::head(ordered_candidates, preferences$return_candidates),
    search_diagnostics = data.frame(
      generated = generated_count,
      scored = length(scored_candidates),
      feasible = feasible_count,
      retained = min(length(ordered_candidates), preferences$return_candidates),
      budget = candidate_budget,
      stop_reason = stop_reason,
      stringsAsFactors = FALSE
    )
  )
}

is_lexicographically_better <- function(candidate, incumbent) {
  for (index in seq_along(candidate)) {
    if (candidate[index] < incumbent[index]) {
      return(TRUE)
    }
    if (candidate[index] > incumbent[index]) {
      return(FALSE)
    }
  }
  FALSE
}


