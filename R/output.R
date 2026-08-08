# ---- patchwork-output.R ----
build_patchwork_pages <- function(best_candidate, plots, output_style, collect_guides, collect_axes) {
  first_page <- best_candidate$pages[[1]]
  page_spec <- list(
    width_mm = sum(first_page$col_widths_mm %||% first_page$widths),
    height_mm = sum(first_page$row_heights_mm %||% first_page$heights)
  )
  build_layout_pages(
    best_candidate = best_candidate,
    plots = plots,
    output_style = output_style,
    collect_guides = collect_guides,
    collect_axes = collect_axes,
    layout_engine = "patchwork",
    page_spec = page_spec,
    preferences = list(page_margin_mm = 0)
  )
}

build_layout_pages <- function(
    best_candidate,
    plots,
    output_style,
    collect_guides,
    collect_axes,
    layout_engine,
    page_spec,
    preferences) {

  pages <- vector("list", length(best_candidate$pages))

  for (page_index in seq_along(best_candidate$pages)) {
    page <- best_candidate$pages[[page_index]]
    layout_string <- layout_matrix_to_string(page$layout_matrix)
    col_widths_mm <- page$col_widths_mm %||% (page$widths / sum(page$widths) * page_spec$width_mm)
    row_heights_mm <- page$row_heights_mm %||% (page$heights / sum(page$heights) * page_spec$height_mm)
    plot_scales <- infer_inner_plot_scales(plots[page$plot_ids], page$diagnostics)

    patchwork_object <- build_patchwork_object(
      plot_list = plots[page$plot_ids],
      areas = page$areas,
      layout_string = layout_string,
      widths = page$widths,
      heights = page$heights,
      collect_guides = collect_guides,
      collect_axes = collect_axes,
      page_margin_mm = preferences$page_margin_mm,
      plot_scales = plot_scales,
      base_size = scalar_or_default(preferences$base_size, 7)
    )
    grid_grob <- if (layout_engine == "grid") {
      build_grid_page_grob(
        plot_list = plots[page$plot_ids],
        areas = page$areas,
        col_widths_mm = col_widths_mm,
        row_heights_mm = row_heights_mm,
        page_spec = page_spec,
        page_margin_mm = preferences$page_margin_mm,
        plot_scales = plot_scales
      )
    } else {
      NULL
    }

    patchwork_code <- format_patchwork_code(
      plot_ids = page$plot_ids,
      areas = page$areas,
      layout_string = layout_string,
      widths = page$widths,
      heights = page$heights,
      output_style = output_style,
      collect_guides = collect_guides,
      collect_axes = collect_axes,
      page_margin_mm = preferences$page_margin_mm,
      plot_scales = plot_scales,
      base_size = scalar_or_default(preferences$base_size, 7)
    )

    pages[[page_index]] <- list(
      page_id = page_index,
      plot_ids = page$plot_ids,
      layout = layout_string,
      widths = page$widths,
      heights = page$heights,
      col_widths_mm = col_widths_mm,
      row_heights_mm = row_heights_mm,
      content_width_mm = page_spec$width_mm,
      content_height_mm = page_spec$height_mm,
      engine = layout_engine,
      inner_scales = plot_scales,
      patchwork = patchwork_object,
      grob = grid_grob,
      patchwork_code = patchwork_code,
      score = page$score,
      diagnostics = page$diagnostics
    )
  }

  pages
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

build_patchwork_object <- function(
    plot_list,
    areas,
    layout_string,
    widths,
    heights,
    collect_guides,
    collect_axes,
    page_margin_mm = 0,
    plot_scales = NULL,
    base_size = 7) {

  named_plots <- plot_list[areas$plot_id]
  names(named_plots) <- areas$symbol
  named_plots <- apply_inner_plot_scales(named_plots, areas, plot_scales, base_size = base_size)

  wrap_args <- c(
    named_plots,
    list(
      design = layout_string,
      widths = widths,
      heights = heights,
      guides = if (collect_guides) "collect" else "keep",
      axes = if (collect_axes) "collect" else "keep",
      axis_titles = if (collect_axes) "collect" else "keep"
    )
  )

  patchwork_object <- do.call(patchwork::wrap_plots, wrap_args)
  if (is.finite(page_margin_mm) && page_margin_mm > 0) {
    patchwork_object <- patchwork_object +
      patchwork::plot_annotation(
        theme = ggplot2::theme(
          plot.margin = grid::unit(rep(page_margin_mm, 4), "mm")
        )
      )
  }

  patchwork_object
}

infer_inner_plot_scales <- function(plot_list, diagnostics = NULL) {
  rows <- vector("list", length(plot_list))

  for (plot_index in seq_along(plot_list)) {
    plot_id <- names(plot_list)[plot_index]
    diagnostic <- if (!is.null(diagnostics) && "plot_id" %in% names(diagnostics)) {
      diagnostics[diagnostics$plot_id == plot_id, , drop = FALSE]
    } else {
      data.frame()
    }
    if (nrow(diagnostic) == 0) {
      diagnostic <- data.frame(hard_loss = 0, stringsAsFactors = FALSE)
    }

    scale <- infer_inner_plot_scale(plot_list[[plot_index]], diagnostic)
    rows[[plot_index]] <- data.frame(
      plot_id = plot_id,
      scale_x = scale$scale_x,
      scale_y = scale$scale_y,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

infer_inner_plot_scale <- function(plot, diagnostic = data.frame()) {
  hard_loss <- scalar_or_default(diagnostic$hard_loss, 0)
  label_gap_loss <- scalar_or_default(diagnostic$label_gap_loss, NA_real_)
  panel_minimum_loss <- scalar_or_default(diagnostic$panel_minimum_loss, NA_real_)
  facet_loss <- scalar_or_default(diagnostic$facet_loss, NA_real_)
  label_only_hard_violation <- is.finite(label_gap_loss) && label_gap_loss > 0 &&
    is.finite(panel_minimum_loss) && panel_minimum_loss <= 0 &&
    is.finite(facet_loss) && facet_loss <= 0
  if (is.finite(hard_loss) && hard_loss > 0 && !label_only_hard_violation) {
    return(list(scale_x = 1, scale_y = 1))
  }

  structural_scale <- infer_structural_inner_plot_scale(plot, diagnostic)
  scale <- structural_scale

  structural_floor <- infer_structural_inner_plot_floor(plot, diagnostic)
  list(
    scale_x = min(1, max(scale$scale_x, structural_floor$scale_x)),
    scale_y = min(1, max(scale$scale_y, structural_floor$scale_y))
  )
}

infer_measured_inner_plot_scale <- function(diagnostic = data.frame()) {
  if (is.null(diagnostic) || nrow(diagnostic) == 0) {
    return(NULL)
  }

  allocated_width_mm <- scalar_or_default(diagnostic$allocated_width_mm, NA_real_)
  allocated_height_mm <- scalar_or_default(diagnostic$allocated_height_mm, NA_real_)
  if (!is.finite(allocated_width_mm) || !is.finite(allocated_height_mm) ||
      allocated_width_mm <= 0 || allocated_height_mm <= 0) {
    return(NULL)
  }

  panel_width_mm <- scalar_or_default(diagnostic$panel_width_mm, NA_real_)
  panel_height_mm <- scalar_or_default(diagnostic$panel_height_mm, NA_real_)
  effective_panel_width_mm <- scalar_or_default(diagnostic$effective_panel_width_mm, panel_width_mm)
  effective_panel_height_mm <- scalar_or_default(diagnostic$effective_panel_height_mm, panel_height_mm)
  preferred_width_mm <- scalar_or_default(diagnostic$preferred_width_mm, NA_real_)
  preferred_height_mm <- scalar_or_default(diagnostic$preferred_height_mm, NA_real_)

  fixed_width_mm <- scalar_or_default(diagnostic$fixed_width_mm, NA_real_)
  if (!is.finite(fixed_width_mm) && is.finite(panel_width_mm)) {
    fixed_width_mm <- max(0, allocated_width_mm - panel_width_mm)
  }
  fixed_height_mm <- scalar_or_default(diagnostic$fixed_height_mm, NA_real_)
  if (!is.finite(fixed_height_mm) && is.finite(panel_height_mm)) {
    fixed_height_mm <- max(0, allocated_height_mm - panel_height_mm)
  }

  target_widths <- preferred_width_mm
  if (is.finite(fixed_width_mm) && is.finite(effective_panel_width_mm)) {
    target_widths <- c(target_widths, fixed_width_mm + effective_panel_width_mm)
  }
  target_heights <- preferred_height_mm
  if (is.finite(fixed_height_mm) && is.finite(effective_panel_height_mm)) {
    target_heights <- c(target_heights, fixed_height_mm + effective_panel_height_mm)
  }

  target_widths <- target_widths[is.finite(target_widths) & target_widths > 0]
  target_heights <- target_heights[is.finite(target_heights) & target_heights > 0]
  if (length(target_widths) == 0 && length(target_heights) == 0) {
    return(NULL)
  }

  min_inner_scale <- 0.5
  scale_x <- if (length(target_widths) == 0) 1 else max(min_inner_scale, min(1, max(target_widths) / allocated_width_mm))
  scale_y <- if (length(target_heights) == 0) 1 else max(min_inner_scale, min(1, max(target_heights) / allocated_height_mm))

  if (scale_x > 0.95) {
    scale_x <- 1
  }
  if (scale_y > 0.95) {
    scale_y <- 1
  }

  list(scale_x = scale_x, scale_y = scale_y)
}

infer_structural_inner_plot_scale <- function(plot, diagnostic = data.frame()) {
  scale_x <- 1
  scale_y <- 1
  aspect <- infer_theme_aspect_ratio(plot)
  geom_classes <- vapply(plot$layers, function(layer) class(layer$geom)[1], character(1))
  has_text_geom <- any(grepl("GeomText|GeomLabel|GeomTextRepel|GeomLabelRepel", geom_classes))
  has_point_geom <- any(grepl("GeomPoint|GeomJitter|GeomDotplot", geom_classes))
  has_line_geom <- any(grepl("GeomLine|GeomPath|GeomStep", geom_classes))
  has_ribbon_geom <- any(grepl("GeomRibbon|GeomArea", geom_classes))
  has_density_geom <- any(grepl("GeomDensity", geom_classes))
  has_bar_geom <- any(grepl("GeomBar|GeomCol|GeomHistogram", geom_classes))
  has_boxplot_geom <- any(grepl("GeomBoxplot", geom_classes))
  has_tile_geom <- any(grepl("GeomTile|GeomRaster", geom_classes))
  has_interval_geom <- any(grepl("GeomPointrange|GeomErrorbar|GeomLinerange|GeomCrossbar", geom_classes))
  has_reference_line <- any(grepl("GeomHline|GeomVline|GeomAbline", geom_classes))
  has_rotated_x_labels <- plot_has_rotated_x_labels(plot)
  has_horizontal_legend <- plot_external_legend_position(plot) %in% c("bottom", "top")
  n_marks_per_panel <- scalar_or_default(diagnostic$n_marks_per_panel, NA_real_)
  n_point_marks_per_panel <- scalar_or_default(diagnostic$n_point_marks_per_panel, NA_real_)
  n_nonempty_text_labels <- scalar_or_default(diagnostic$n_nonempty_text_labels, NA_real_)
  n_panel_rows <- scalar_or_default(diagnostic$n_panel_rows, 1)
  n_panel_cols <- scalar_or_default(diagnostic$n_panel_cols, 1)
  n_panels <- scalar_or_default(diagnostic$n_panels, 1)
  fixed_width_mm <- scalar_or_default(diagnostic$fixed_width_mm, 0)
  allocated_width_mm <- scalar_or_default(diagnostic$allocated_width_mm, NA_real_)
  allocated_height_mm <- scalar_or_default(diagnostic$allocated_height_mm, NA_real_)
  legend_width_mm <- scalar_or_default(diagnostic$legend_width_mm, NA_real_)
  legend_height_mm <- scalar_or_default(diagnostic$legend_height_mm, NA_real_)
  label_gap_loss <- scalar_or_default(diagnostic$label_gap_loss, 0)
  panel_minimum_loss <- scalar_or_default(diagnostic$panel_minimum_loss, 0)
  facet_loss <- scalar_or_default(diagnostic$facet_loss, 0)
  label_only_hard_violation <- label_gap_loss > 0 &&
    panel_minimum_loss <= 0 && facet_loss <= 0

  if (plot_is_faceted(plot)) {
    if (has_bar_geom) {
      scale_x <- min(scale_x, 0.5)
      scale_y <- min(scale_y, 0.5)
    } else if (has_tile_geom && has_rotated_x_labels) {
      heatmap_density_pressure <- if (is.finite(n_marks_per_panel)) {
        min(1, max(0, log2(max(30, n_marks_per_panel) / 30) / 2))
      } else {
        0
      }
      scale_x <- min(scale_x, 4 / 9 + heatmap_density_pressure / 9)
      scale_y <- min(scale_y, 0.75 + 0.15 * heatmap_density_pressure)
    } else if (has_point_geom) {
      if (is.finite(n_panels) && n_panels >= 12 &&
          is.finite(n_panel_rows) && is.finite(n_panel_cols) &&
          n_panel_rows > n_panel_cols) {
        scale_x <- min(scale_x, 1 / 3)
        scale_y <- min(scale_y, 0.75)
      } else {
        scale_y <- min(scale_y, 0.5)
      }
    } else if (has_line_geom) {
      facet_line_scale <- min(0.9, max(0.75, 1 - 0.1 * sqrt(max(1, n_panels))))
      label_gap_protection <- min(0.1, sqrt(max(0, label_gap_loss)))
      facet_line_scale <- min(0.95, facet_line_scale + label_gap_protection)
      scale_x <- min(scale_x, facet_line_scale)
      scale_y <- min(scale_y, facet_line_scale)
    }
  } else if (has_line_geom) {
    line_scale_x <- if (has_point_geom) {
      1
    } else if (has_ribbon_geom) {
      4 / 9
    } else {
      2 / 3
    }
    scale_x <- min(scale_x, line_scale_x)
    scale_y <- min(scale_y, 0.5)
  }

  if (!plot_is_faceted(plot) && has_boxplot_geom && has_point_geom &&
      !has_rotated_x_labels && !plot_has_external_legend(plot)) {
    distribution_label_protection <- min(0.125, 0.1 * sqrt(max(0, label_gap_loss)))
    scale_x <- min(scale_x, 0.5 + distribution_label_protection)
  }

  if (!plot_is_faceted(plot) && has_tile_geom && !has_text_geom) {
    scale_x <- min(scale_x, 2 / 3)
    scale_y <- min(scale_y, 2 / 3)
  }

  if (!plot_is_faceted(plot) && has_density_geom) {
    scale_x <- min(scale_x, 2 / 3)
  }

  if (has_interval_geom && !plot_is_faceted(plot)) {
    interval_scale_x <- if (is.finite(n_marks_per_panel) && n_marks_per_panel >= 12 &&
        is.finite(fixed_width_mm) && fixed_width_mm >= 40) {
      0.25
    } else {
      0.75
    }
    long_label_interval <- is.finite(n_marks_per_panel) && n_marks_per_panel >= 12 &&
      is.finite(fixed_width_mm) && fixed_width_mm >= 40
    scale_x <- min(scale_x, interval_scale_x)
    scale_y <- min(scale_y, if (long_label_interval) 0.75 else 2 / 3)
  }

  if (has_reference_line && has_point_geom && !has_text_geom &&
      !has_interval_geom && !plot_is_faceted(plot) &&
      is.finite(aspect) && aspect >= 0.75) {
    scale_x <- min(scale_x, 0.5)
    scale_y <- min(scale_y, 0.5)
  }

  if (is.finite(aspect) && aspect >= 0.75 && has_horizontal_legend) {
    scale_x <- min(scale_x, 2 / 3)
    scale_y <- min(scale_y, 2 / 3)
  }

  if (has_line_geom && has_horizontal_legend &&
      is.finite(allocated_width_mm) && allocated_width_mm > 0 &&
      is.finite(allocated_height_mm) && allocated_height_mm > 0 &&
      is.finite(legend_width_mm) && legend_width_mm > 0 &&
      is.finite(legend_height_mm) && legend_height_mm > 0) {
    legend_row_pressure <- min(1, max(0, (legend_height_mm - 3) / 6))
    legend_width_scale <- min(2 / 3, max(0.25, (legend_width_mm + 4) / allocated_width_mm))
    legend_height_scale <- min(2 / 3, max(0.2, (legend_height_mm + 8) / allocated_height_mm))
    scale_x <- min(scale_x, (1 - legend_row_pressure) * (2 / 3) +
      legend_row_pressure * legend_width_scale)
    scale_y <- min(scale_y, (1 - legend_row_pressure) * 0.5 +
      legend_row_pressure * legend_height_scale)
  }

  if (!plot_is_faceted(plot) && has_point_geom && !has_text_geom &&
      !has_interval_geom && !has_reference_line &&
      !plot_has_external_legend(plot) &&
      is.finite(aspect) && aspect >= 0.75) {
    point_scale <- 2 / 3
    if (is.finite(n_point_marks_per_panel) && n_point_marks_per_panel > 6000) {
      saturation_pressure <- min(1, log2(n_point_marks_per_panel / 6000) / log2(3))
      point_scale <- 2 / 3 - saturation_pressure / 3
    }
    scale_x <- min(scale_x, point_scale)
    scale_y <- min(scale_y, point_scale)
  }

  if (!plot_is_faceted(plot) && has_bar_geom && !has_rotated_x_labels &&
      is.finite(aspect) && aspect < 0.75) {
    scale_x <- min(scale_x, 0.5)
  }

  if (has_boxplot_geom && has_rotated_x_labels && !has_horizontal_legend) {
    scale_x <- min(scale_x, 2 / 3)
    scale_y <- min(scale_y, 2 / 3)
  }

  if (has_text_geom && is.finite(aspect) && aspect >= 0.75) {
    annotation_scale <- 1
    if (is.finite(n_nonempty_text_labels) && n_nonempty_text_labels < 10) {
      annotation_scale <- 2 / 3
    }
    if (has_tile_geom && is.finite(n_nonempty_text_labels) && n_nonempty_text_labels >= 10) {
      matrix_scale <- 0.25 + 0.25 * sqrt(36 / n_nonempty_text_labels)
      matrix_scale <- min(0.625, max(0.375, matrix_scale))
      annotation_scale <- min(annotation_scale, matrix_scale)
    }
    scale_x <- min(scale_x, annotation_scale)
    scale_y <- min(scale_y, annotation_scale)
  }

  if (has_rotated_x_labels && has_bar_geom && is.finite(n_marks_per_panel)) {
    sparse_bar_scale_x <- if (label_only_hard_violation) {
      0.625
    } else {
      min(0.5, max(1 / 3, 8 / 27 + n_marks_per_panel / 135))
    }
    sparse_bar_scale_y <- if (label_only_hard_violation) {
      0.5
    } else {
      min(1, max(2 / 3, 5 / 9 + n_marks_per_panel / 45))
    }
    scale_x <- min(scale_x, sparse_bar_scale_x)
    scale_y <- min(scale_y, sparse_bar_scale_y)
  }

  list(scale_x = scale_x, scale_y = scale_y)
}

infer_structural_inner_plot_floor <- function(plot, diagnostic = data.frame()) {
  scale_x <- 0
  scale_y <- 0
  geom_classes <- vapply(plot$layers, function(layer) class(layer$geom)[1], character(1))
  has_point_geom <- any(grepl("GeomPoint|GeomJitter|GeomDotplot", geom_classes))
  has_text_geom <- any(grepl("GeomText|GeomLabel|GeomTextRepel|GeomLabelRepel", geom_classes))
  has_tile_geom <- any(grepl("GeomTile|GeomRaster", geom_classes))
  has_line_geom <- any(grepl("GeomLine|GeomPath|GeomStep", geom_classes))
  has_interval_geom <- any(grepl("GeomPointrange|GeomErrorbar|GeomLinerange|GeomCrossbar", geom_classes))
  has_distribution_geom <- any(grepl("GeomBoxplot|GeomViolin", geom_classes))
  has_violin_geom <- any(grepl("GeomViolin", geom_classes))
  has_bottom_or_top_legend <- plot_external_legend_position(plot) %in% c("bottom", "top")
  has_rotated_x_labels <- plot_has_rotated_x_labels(plot)

  allocated_width_mm <- scalar_or_default(diagnostic$allocated_width_mm, NA_real_)
  allocated_height_mm <- scalar_or_default(diagnostic$allocated_height_mm, NA_real_)
  fixed_width_mm <- scalar_or_default(diagnostic$fixed_width_mm, NA_real_)
  effective_panel_width_mm <- scalar_or_default(diagnostic$effective_panel_width_mm, NA_real_)
  legend_width_mm <- scalar_or_default(diagnostic$legend_width_mm, NA_real_)
  legend_height_mm <- scalar_or_default(diagnostic$legend_height_mm, NA_real_)

  n_point_marks_per_panel <- scalar_or_default(diagnostic$n_point_marks_per_panel, NA_real_)
  if (is.finite(n_point_marks_per_panel) && n_point_marks_per_panel > 500) {
    density_floor <- if (n_point_marks_per_panel <= 6000) {
      density_pressure <- min(1, log2(n_point_marks_per_panel / 500) / log2(12))
      0.8 + 0.2 * density_pressure
    } else {
      extreme_pressure <- min(1, log2(n_point_marks_per_panel / 6000) / log2(3))
      1 - 2 * extreme_pressure / 3
    }
    scale_x <- max(scale_x, density_floor)
    scale_y <- max(scale_y, density_floor)
  }

  if (has_distribution_geom && has_point_geom && !has_rotated_x_labels) {
    scale_y <- 1
  }
  if (has_distribution_geom && has_point_geom && has_bottom_or_top_legend) {
    scale_x <- 1
    scale_y <- 1
  }
  if (has_violin_geom) {
    scale_x <- 1
    scale_y <- 1
  }
  if (has_line_geom && has_bottom_or_top_legend &&
      is.finite(allocated_width_mm) && allocated_width_mm > 0 &&
      is.finite(allocated_height_mm) && allocated_height_mm > 0 &&
      is.finite(legend_width_mm) && legend_width_mm > 0 &&
      is.finite(legend_height_mm) && legend_height_mm > 0) {
    scale_x <- max(scale_x, min(1, (legend_width_mm + 4) / allocated_width_mm))
    scale_y <- max(scale_y, min(1, (legend_height_mm + 8) / allocated_height_mm))
  }
  if (has_interval_geom &&
      is.finite(allocated_width_mm) && allocated_width_mm > 0 &&
      is.finite(fixed_width_mm) && fixed_width_mm >= 0 &&
      is.finite(effective_panel_width_mm) && effective_panel_width_mm > 0) {
    retained_panel_width_mm <- max(15, 0.1 * effective_panel_width_mm)
    interval_width_floor <- min(1, (fixed_width_mm + retained_panel_width_mm) / allocated_width_mm)
    scale_x <- max(scale_x, interval_width_floor)
  }
  if (has_text_geom && !has_tile_geom) {
    scale_y <- max(scale_y, 0.4)
  }

  list(scale_x = scale_x, scale_y = scale_y)
}

plot_has_external_legend <- function(plot) {
  !is.na(plot_external_legend_position(plot))
}

plot_external_legend_position <- function(plot) {
  legend_position <- plot$theme$legend.position
  if (is.null(legend_position)) {
    return(NA_character_)
  }
  if (is.character(legend_position) && length(legend_position) > 0) {
    position <- legend_position[1]
    if (!position %in% c("none", "inside")) {
      return(position)
    }
  }
  NA_character_
}

plot_is_faceted <- function(plot) {
  !inherits(plot$facet, "FacetNull")
}

plot_has_rotated_x_labels <- function(plot) {
  axis_text_x <- plot$theme$axis.text.x
  if (is.null(axis_text_x) || is.null(axis_text_x$angle)) {
    return(FALSE)
  }
  angle <- suppressWarnings(as.numeric(axis_text_x$angle))
  is.finite(angle) && abs(angle) > 0
}

attach_plot_title <- function(plot, base_size = 7) {
  compose_plot_with_title_and_inner_scale(
    plot,
    scale_x = 1,
    scale_y = 1,
    base_size = base_size
  )
}

compose_plot_with_title_and_inner_scale <- function(plot, scale_x = 1, scale_y = 1, base_size = 7) {
  if (!inherits(plot, "ggplot")) {
    return(plot)
  }

  titled_plot <- plot +
    ggplot2::theme(
      plot.margin = grid::unit(c(0, 1.5, 1.5, 1.5), "mm")
    )
  titled_plot <- patchwork::wrap_elements(full = titled_plot, clip = FALSE)

  wrap_plot_with_inner_scale(
    titled_plot,
    scale_x = scale_x,
    scale_y = scale_y,
    anchor_y = "top"
  )
}

apply_inner_plot_scales <- function(named_plots, areas, plot_scales = NULL, base_size = 7) {
  if (is.null(plot_scales) || nrow(plot_scales) == 0) {
    plot_scales <- data.frame(
      plot_id = areas$plot_id,
      scale_x = 1,
      scale_y = 1,
      stringsAsFactors = FALSE
    )
  }

  for (symbol in names(named_plots)) {
    plot_id <- areas$plot_id[areas$symbol == symbol]
    scale_row <- plot_scales[plot_scales$plot_id == plot_id, , drop = FALSE]
    if (nrow(scale_row) == 0) {
      next
    }
    named_plots[[symbol]] <- compose_plot_with_title_and_inner_scale(
      named_plots[[symbol]],
      scale_x = scale_row$scale_x[1],
      scale_y = scale_row$scale_y[1],
      base_size = base_size
    )
  }

  named_plots
}

wrap_plot_with_inner_scale <- function(plot, scale_x = 1, scale_y = 1, anchor_y = c("center", "top")) {
  anchor_y <- match.arg(anchor_y)
  scale_x <- min(1, max(0.05, scalar_or_default(scale_x, 1)))
  scale_y <- min(1, max(0.05, scalar_or_default(scale_y, 1)))

  if (scale_x >= 0.995 && scale_y >= 0.995) {
    return(plot)
  }

  left <- max(0.001, (1 - scale_x) / 2)
  right <- max(0.001, 1 - scale_x - left)
  if (anchor_y == "top") {
    top <- 0.001
    bottom <- max(0.001, 1 - scale_y - top)
  } else {
    top <- max(0.001, (1 - scale_y) / 2)
    bottom <- max(0.001, 1 - scale_y - top)
  }

  spacer <- patchwork::plot_spacer()
  scaled_plot <- patchwork::wrap_plots(
    c(
      rep(list(spacer), 4),
      list(plot),
      rep(list(spacer), 4)
    ),
    ncol = 3,
    widths = c(left, scale_x, right),
    heights = c(top, scale_y, bottom)
  )
  patchwork::wrap_elements(full = scaled_plot, clip = TRUE)
}

build_grid_page_grob <- function(
    plot_list,
    areas,
    col_widths_mm,
    row_heights_mm,
    page_spec,
    page_margin_mm = 0,
    plot_scales = NULL) {

  children <- list()

  for (area_index in seq_len(nrow(areas))) {
    area <- areas[area_index, , drop = FALSE]
    plot_grob <- ggplot2::ggplotGrob(plot_list[[area$plot_id]])

    left_mm <- page_margin_mm + sum(col_widths_mm[seq_len(max(area$l - 1, 0))])
    top_mm <- page_margin_mm + sum(row_heights_mm[seq_len(max(area$t - 1, 0))])
    width_mm <- sum(col_widths_mm[area$l:area$r])
    height_mm <- sum(row_heights_mm[area$t:area$b])
    scale_row <- if (!is.null(plot_scales)) {
      plot_scales[plot_scales$plot_id == area$plot_id, , drop = FALSE]
    } else {
      data.frame()
    }
    scale_x <- if (nrow(scale_row) > 0) scale_row$scale_x[1] else 1
    scale_y <- if (nrow(scale_row) > 0) scale_row$scale_y[1] else 1
    inner_width_mm <- width_mm * min(1, max(0.05, scale_x))
    inner_height_mm <- height_mm * min(1, max(0.05, scale_y))
    left_mm <- left_mm + (width_mm - inner_width_mm) / 2
    top_mm <- top_mm + (height_mm - inner_height_mm) / 2

    child <- grid::grobTree(
      plot_grob,
      vp = grid::viewport(
        x = grid::unit(left_mm, "mm"),
        y = grid::unit(page_spec$height_mm + 2 * page_margin_mm - top_mm, "mm"),
        width = grid::unit(inner_width_mm, "mm"),
        height = grid::unit(inner_height_mm, "mm"),
        just = c("left", "top"),
        clip = "on"
      )
    )
    children[[length(children) + 1]] <- child
  }

  grid::gTree(children = do.call(grid::gList, children))
}

format_patchwork_code <- function(
    plot_ids,
    areas,
    layout_string,
    widths,
    heights,
    output_style,
    collect_guides,
    collect_axes,
    page_margin_mm = 0,
    plot_scales = NULL,
    base_size = 7) {

  if (output_style == "nested_patchwork") {
    nested_code <- format_nested_patchwork_code(plot_ids, areas, layout_string)
    if (!is.na(nested_code)) {
      return(nested_code)
    }
  }

  plot_expr <- areas$plot_id
  plot_expr <- paste0("attach_title(", plot_expr, ")")
  scale_helper_needed <- !is.null(plot_scales) &&
    any(plot_scales$scale_x < 0.995 | plot_scales$scale_y < 0.995)
  if (scale_helper_needed) {
    for (area_index in seq_len(nrow(areas))) {
      scale_row <- plot_scales[plot_scales$plot_id == areas$plot_id[area_index], , drop = FALSE]
      if (nrow(scale_row) > 0 &&
          (scale_row$scale_x[1] < 0.995 || scale_row$scale_y[1] < 0.995)) {
        plot_expr[area_index] <- paste0(
          "scale_plot(",
          areas$plot_id[area_index],
          ", ",
          format(round(scale_row$scale_x[1], 3), trim = TRUE),
          ", ",
          format(round(scale_row$scale_y[1], 3), trim = TRUE),
          ")"
        )
      }
    }
  }

  plot_lines <- paste0(
    "  \"",
    areas$symbol,
    "\" = ",
    plot_expr,
    collapse = ",\n"
  )

  helper_code <- paste0(
    "scale_plot <- function(plot, scale_x = 1, scale_y = 1, fontsize = ",
    format(round(base_size, 3), trim = TRUE),
    ") {\n",
    "  titled_plot <- plot +\n",
    "    ggplot2::theme(\n",
    "      plot.margin = grid::unit(c(0, 1.5, 1.5, 1.5), \"mm\")\n",
    "    )\n",
    "  titled_plot <- patchwork::wrap_elements(full = titled_plot, clip = FALSE)\n",
    "  scale_x <- min(1, max(0.05, scale_x))\n",
    "  scale_y <- min(1, max(0.05, scale_y))\n",
    "  if (scale_x < 0.995 || scale_y < 0.995) {\n",
    "    left <- max(0.001, (1 - scale_x) / 2)\n",
    "    right <- max(0.001, 1 - scale_x - left)\n",
    "    top <- 0.001\n",
    "    bottom <- max(0.001, 1 - scale_y - top)\n",
    "    spacer <- patchwork::plot_spacer()\n",
    "    titled_plot <- patchwork::wrap_elements(\n",
    "      full = patchwork::wrap_plots(\n",
    "        c(\n",
    "          rep(list(spacer), 4),\n",
    "          list(titled_plot),\n",
    "          rep(list(spacer), 4)\n",
    "        ),\n",
    "        ncol = 3,\n",
    "        widths = c(left, scale_x, right),\n",
    "        heights = c(top, scale_y, bottom)\n",
    "      ),\n",
    "      clip = TRUE\n",
    "    )\n",
    "  }\n",
    "  titled_plot\n",
    "}\n\n",
    "attach_title <- function(plot, fontsize = ",
    format(round(base_size, 3), trim = TRUE),
    ") {\n",
    "  scale_plot(plot, 1, 1, fontsize)\n",
    "}\n\n"
  )

  base_code <- paste0(
    helper_code,
    "layout <- ",
    deparse(layout_string),
    "\n\n",
    "combined_plot <- patchwork::wrap_plots(\n",
    "  list(\n",
    plot_lines,
    "\n  ),\n",
    "  design = layout,\n",
    "  widths = c(",
    paste(format(round(widths, 3), trim = TRUE), collapse = ", "),
    "),\n",
    "  heights = c(",
    paste(format(round(heights, 3), trim = TRUE), collapse = ", "),
    "),\n",
    "  guides = \"",
    if (collect_guides) "collect" else "keep",
    "\",\n",
    "  axes = \"",
    if (collect_axes) "collect" else "keep",
    "\",\n",
    "  axis_titles = \"",
    if (collect_axes) "collect" else "keep",
    "\"\n",
    ")\n"
  )

  if (is.finite(page_margin_mm) && page_margin_mm > 0) {
    return(
      paste0(
        sub("\\)\\n$", ") +\n", base_code),
        "  patchwork::plot_annotation(\n",
        "    theme = ggplot2::theme(plot.margin = grid::unit(c(",
        paste(rep(format(round(page_margin_mm, 3), trim = TRUE), 4), collapse = ", "),
        "), \"mm\"))\n",
        "  )\n"
      )
    )
  }

  base_code
}

format_nested_patchwork_code <- function(plot_ids, areas, layout_string) {
  layout_lines <- strsplit(trimws(layout_string), "\n", fixed = TRUE)[[1]]
  row_expressions <- character(length(layout_lines))

  for (row_index in seq_along(layout_lines)) {
    row_symbols <- strsplit(layout_lines[row_index], "", fixed = TRUE)[[1]]
    row_symbols <- row_symbols[!duplicated(row_symbols)]

    row_plot_ids <- character()
    for (symbol in row_symbols) {
      if (symbol == "#") {
        row_plot_ids <- c(row_plot_ids, "p0")
      } else {
        plot_id <- areas$plot_id[areas$symbol == symbol]
        row_plot_ids <- c(row_plot_ids, plot_id)
      }
    }

    row_expressions[row_index] <- paste(row_plot_ids, collapse = " | ")
  }

  paste0(
    "p0 <- patchwork::plot_spacer()\n\n",
    "combined_plot <- (\n  ",
    paste(row_expressions, collapse = "\n) / (\n  "),
    "\n)\n"
  )
}

validate_final_solution <- function(
    pages,
    page_width_in,
    page_height_in,
    base_family,
    base_size,
    validation_level = "layout") {

  if (validation_level != "render") {
    return(character())
  }

  device_state <- open_measurement_device(
    device = "pdf",
    width_in = page_width_in,
    height_in = page_height_in,
    base_family = base_family,
    base_size = base_size
  )
  on.exit(close_measurement_device(device_state), add = TRUE)

  warnings <- character()
  render_result <- try(draw_layout_pages(list(pages = pages)), silent = TRUE)
  if (inherits(render_result, "try-error")) {
    warnings <- "Render validation failed while drawing selected layout pages."
  }

  warnings
}

draw_layout_pages <- function(result) {
  pages <- if (!is.null(result$pages)) result$pages else result

  for (page in pages) {
    if (identical(page$engine, "grid")) {
      grid::grid.newpage()
      grid::grid.draw(page$grob)
    } else {
      print(page$patchwork)
    }
  }

  invisible(result)
}


# ---- diagnostics.R ----
make_candidate_layout_diagnostics <- function(candidate, preferences) {
  rows <- vector("list", length(candidate$pages))

  for (page_index in seq_along(candidate$pages)) {
    page <- candidate$pages[[page_index]]

    rows[[page_index]] <- data.frame(
      candidate_id = candidate$candidate_id,
      selected = isTRUE(candidate$selected),
      n_pages = length(candidate$pages),
      page = page_index,
      rows = nrow(page$layout_matrix),
      cols = ncol(page$layout_matrix),
      score = page$score,
      soft_score = scalar_or_default(page$soft_score, page$score),
      hard_loss = scalar_or_default(page$hard_loss, NA_real_),
      max_hard_violation = page$max_hard_violation,
      empty_cell_fraction = page$empty_cell_fraction,
      order_penalty = page$order_penalty,
      complexity_penalty = page$complexity_penalty,
      row_height_prior_penalty = page$row_height_prior_penalty,
      multipage_penalty = if (page_index == 1) preferences$multipage_penalty * (length(candidate$pages) - 1) else 0,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

make_plot_diagnostics <- function(profiles, frontier_summaries, best_candidate) {
  selected_diagnostics <- do.call(
    rbind,
    lapply(seq_along(best_candidate$pages), function(page_index) {
      diagnostics <- best_candidate$pages[[page_index]]$diagnostics
      diagnostics$page <- page_index
      diagnostics
    })
  )

  rows <- vector("list", length(profiles))

  for (profile_index in seq_along(profiles)) {
    profile <- profiles[[profile_index]]
    plot_id <- profile$plot_id
    frontier <- frontier_summaries[[plot_id]]
    selected <- selected_diagnostics[selected_diagnostics$plot_id == plot_id, , drop = FALSE]

    if (nrow(selected) == 0) {
      selected <- data.frame(
        min_x_label_gap_mm = NA_real_,
        min_y_label_gap_mm = NA_real_,
        panel_width_mm = NA_real_,
        panel_height_mm = NA_real_,
        effective_panel_width_mm = NA_real_,
        effective_panel_height_mm = NA_real_,
        unused_panel_area_mm2 = NA_real_,
        n_marks_per_panel = NA_real_,
        n_point_marks_per_panel = NA_real_,
        n_nonempty_text_labels = NA_real_,
        n_panel_rows = NA_integer_,
        n_panel_cols = NA_integer_,
        n_panels = NA_integer_,
        legend_width_mm = NA_real_,
        legend_height_mm = NA_real_,
        legend_area_mm2 = NA_real_,
        label_gap_loss = NA_real_,
        panel_minimum_loss = NA_real_,
        legend_loss = NA_real_,
        facet_loss = NA_real_,
        data_density_loss = NA_real_,
        aspect_ratio_loss = NA_real_,
        unused_panel_area_loss = NA_real_,
        total_fit_loss = NA_real_,
        hard_loss = NA_real_,
        warning = "",
        stringsAsFactors = FALSE
      )
    }

    rows[[profile_index]] <- data.frame(
      plot_id = plot_id,
      min_acceptable_width_mm = frontier$min_acceptable_width_mm,
      min_acceptable_height_mm = frontier$min_acceptable_height_mm,
      preferred_width_mm = frontier$preferred_width_mm,
      preferred_height_mm = frontier$preferred_height_mm,
      selected_page = selected$page[1],
      min_x_label_gap_mm_at_selected_size = selected$min_x_label_gap_mm[1],
      min_y_label_gap_mm_at_selected_size = selected$min_y_label_gap_mm[1],
      panel_width_mm_at_selected_size = selected$panel_width_mm[1],
      panel_height_mm_at_selected_size = selected$panel_height_mm[1],
      effective_panel_width_mm_at_selected_size = selected$effective_panel_width_mm[1],
      effective_panel_height_mm_at_selected_size = selected$effective_panel_height_mm[1],
      unused_panel_area_mm2_at_selected_size = selected$unused_panel_area_mm2[1],
      n_marks_per_panel = selected$n_marks_per_panel[1],
      n_point_marks_per_panel = selected$n_point_marks_per_panel[1],
      n_nonempty_text_labels = selected$n_nonempty_text_labels[1],
      n_panel_rows = selected$n_panel_rows[1],
      n_panel_cols = selected$n_panel_cols[1],
      n_panels = selected$n_panels[1],
      legend_width_mm = selected$legend_width_mm[1],
      legend_height_mm = selected$legend_height_mm[1],
      legend_area_mm2 = selected$legend_area_mm2[1],
      label_gap_loss = selected$label_gap_loss[1],
      panel_minimum_loss = selected$panel_minimum_loss[1],
      legend_loss = selected$legend_loss[1],
      facet_loss = selected$facet_loss[1],
      data_density_loss = selected$data_density_loss[1],
      aspect_ratio_loss = selected$aspect_ratio_loss[1],
      unused_panel_area_loss = selected$unused_panel_area_loss[1],
      total_fit_loss = selected$total_fit_loss[1],
      hard_loss = selected$hard_loss[1],
      warning = selected$warning[1],
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

make_result_warnings <- function(profiles, best_candidate, plot_diagnostics, preferences = list()) {
  warnings <- unique(unlist(lapply(profiles, function(profile) profile$warnings)))
  warnings <- warnings[nzchar(warnings)]

  selected_warnings <- plot_diagnostics$warning[nzchar(plot_diagnostics$warning)]
  warnings <- unique(c(warnings, selected_warnings))

  marginal_x <- plot_diagnostics[
    is.finite(plot_diagnostics$min_x_label_gap_mm_at_selected_size) &
      plot_diagnostics$min_x_label_gap_mm_at_selected_size < 2,
    ,
    drop = FALSE
  ]
  if (nrow(marginal_x) > 0) {
    warnings <- c(
      warnings,
      paste0(
        marginal_x$plot_id,
        " x-axis labels are marginal: estimated minimum gap ",
        round(marginal_x$min_x_label_gap_mm_at_selected_size, 2),
        " mm."
      )
    )
  }

  marginal_y <- plot_diagnostics[
    is.finite(plot_diagnostics$min_y_label_gap_mm_at_selected_size) &
      plot_diagnostics$min_y_label_gap_mm_at_selected_size < 2,
    ,
    drop = FALSE
  ]
  if (nrow(marginal_y) > 0) {
    warnings <- c(
      warnings,
      paste0(
        marginal_y$plot_id,
        " y-axis labels are marginal: estimated minimum gap ",
        round(marginal_y$min_y_label_gap_mm_at_selected_size, 2),
        " mm."
      )
    )
  }

  if (length(best_candidate$pages) > 1) {
    warnings <- c(
      warnings,
      paste0("Selected solution uses ", length(best_candidate$pages), " pages after applying the multipage readability penalty.")
    )
  }

  high_empty <- vapply(best_candidate$pages, function(page) {
    is.finite(page$empty_cell_fraction) &&
      page$empty_cell_fraction > scalar_or_default(preferences$max_empty_fraction, Inf)
  }, logical(1))
  if (any(high_empty)) {
    warnings <- c(warnings, "Selected layout contains more empty cells than the preferred threshold.")
  }

  unique(warnings[nzchar(warnings)])
}


