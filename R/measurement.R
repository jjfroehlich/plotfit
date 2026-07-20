# ---- measurement-device.R ----
open_measurement_device <- function(device, width_in, height_in, base_family, base_size) {
  old_device <- grDevices::dev.cur()

  if (device == "pdf") {
    grDevices::pdf(
      file = NULL,
      width = width_in,
      height = height_in,
      family = base_family,
      pointsize = base_size,
      useKerning = TRUE
    )
  } else if (device == "cairo_pdf") {
    grDevices::cairo_pdf(
      filename = NULL,
      width = width_in,
      height = height_in,
      family = base_family,
      pointsize = base_size
    )
  } else {
    stop("Unsupported measurement device: ", device, call. = FALSE)
  }

  list(
    device = grDevices::dev.cur(),
    old_device = old_device
  )
}

close_measurement_device <- function(device_state) {
  if (is.null(device_state$device)) {
    return(invisible(FALSE))
  }

  active_devices <- grDevices::dev.list()
  if (!is.null(active_devices) && device_state$device %in% active_devices) {
    grDevices::dev.set(device_state$device)
    grDevices::dev.off()
  }

  invisible(TRUE)
}


# ---- utils-units.R ----
safe_convert_width_mm <- function(unit_object) {
  value <- try(
    grid::convertWidth(unit_object, "mm", valueOnly = TRUE),
    silent = TRUE
  )

  if (inherits(value, "try-error") || length(value) == 0 || !is.finite(value[1])) {
    return(0)
  }

  as.numeric(value[1])
}

safe_convert_height_mm <- function(unit_object) {
  value <- try(
    grid::convertHeight(unit_object, "mm", valueOnly = TRUE),
    silent = TRUE
  )

  if (inherits(value, "try-error") || length(value) == 0 || !is.finite(value[1])) {
    return(0)
  }

  as.numeric(value[1])
}

measure_grob_mm <- function(grob) {
  list(
    width_mm = safe_convert_width_mm(grid::grobWidth(grob)),
    height_mm = safe_convert_height_mm(grid::grobHeight(grob))
  )
}

projected_text_extent <- function(width_mm, height_mm, rotation) {
  theta <- rotation * pi / 180

  list(
    width_mm = abs(cos(theta)) * width_mm + abs(sin(theta)) * height_mm,
    height_mm = abs(sin(theta)) * width_mm + abs(cos(theta)) * height_mm
  )
}


# ---- collect-text-grobs.R ----
measure_text_label_mm <- function(label, gp = grid::gpar(), rotation = 0) {
  text_grob <- grid::textGrob(label = label, gp = gp, rot = rotation)

  list(
    width_mm = safe_convert_width_mm(grid::grobWidth(text_grob)),
    height_mm = safe_convert_height_mm(grid::grobHeight(text_grob))
  )
}

collect_text_grobs <- function(grob, component_type, component_name, path = character()) {
  rows <- list()

  if (inherits(grob, "text")) {
    labels <- as.character(grob$label)
    labels <- labels[!is.na(labels) & labels != ""]

    if (length(labels) > 0) {
      gp <- grob$gp
      if (is.null(gp)) {
        gp <- grid::gpar()
      }

      fontsize <- gp$fontsize
      if (is.null(fontsize)) {
        fontsize <- NA_real_
      }

      fontfamily <- gp$fontfamily
      if (is.null(fontfamily)) {
        fontfamily <- NA_character_
      }

      fontface <- gp$fontface
      if (is.null(fontface)) {
        fontface <- NA_character_
      }

      rotation <- grob$rot
      if (is.null(rotation)) {
        rotation <- 0
      }

      for (label_index in seq_along(labels)) {
        text_size <- measure_text_label_mm(labels[label_index], gp = gp, rotation = rotation)

        rows[[length(rows) + 1]] <- data.frame(
          component_type = component_type,
          component_name = component_name,
          text = labels[label_index],
          width_mm = text_size$width_mm,
          height_mm = text_size$height_mm,
          fontsize = as.numeric(fontsize[1]),
          fontfamily = as.character(fontfamily[1]),
          fontface = as.character(fontface[1]),
          rotation = as.numeric(rotation[1]),
          visible = TRUE,
          path = paste(c(path, "text"), collapse = "/"),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  child_list <- list()
  child_names <- character()

  if (!is.null(grob$children)) {
    child_list <- c(child_list, as.list(grob$children))
    child_names <- c(child_names, names(grob$children))
  }

  if (!is.null(grob$grobs)) {
    child_list <- c(child_list, grob$grobs)
    grob_names <- names(grob$grobs)
    if (is.null(grob_names)) {
      grob_names <- paste0("grob", seq_along(grob$grobs))
    }
    child_names <- c(child_names, grob_names)
  }

  if (length(child_list) > 0) {
    for (child_index in seq_along(child_list)) {
      child_name <- child_names[child_index]
      if (is.na(child_name) || child_name == "") {
        child_name <- paste0("child", child_index)
      }

      child_rows <- collect_text_grobs(
        child_list[[child_index]],
        component_type = component_type,
        component_name = component_name,
        path = c(path, child_name)
      )

      if (nrow(child_rows) > 0) {
        rows[[length(rows) + 1]] <- child_rows
      }
    }
  }

  if (length(rows) == 0) {
    return(empty_text_grobs())
  }

  do.call(rbind, rows)
}

empty_text_grobs <- function() {
  data.frame(
    component_type = character(),
    component_name = character(),
    text = character(),
    width_mm = numeric(),
    height_mm = numeric(),
    fontsize = numeric(),
    fontfamily = character(),
    fontface = character(),
    rotation = numeric(),
    visible = logical(),
    path = character(),
    stringsAsFactors = FALSE
  )
}


# ---- measure-plot-profile.R ----
measure_plot_profile <- function(plot, plot_id, plot_index, page_spec, measurement_spec) {
  warnings <- character()

  built_plot <- try(ggplot2::ggplot_build(plot), silent = TRUE)
  if (inherits(built_plot, "try-error")) {
    built_plot <- NULL
    warnings <- c(warnings, "ggplot_build() failed; data-density diagnostics are unavailable.")
  }

  gt <- ggplot2::ggplotGrob(plot)
  if (!gtable::is.gtable(gt)) {
    stop("ggplot2::ggplotGrob() did not return a gtable object.", call. = FALSE)
  }

  components <- extract_gtable_components(gt)
  component_sizes <- measure_component_sizes(gt, components)
  text_grobs <- collect_plot_text_grobs(gt, components)
  fixed_size <- estimate_fixed_non_panel_size(gt, components)
  unit_profile <- prepare_gtable_unit_profile(gt, components)
  panel_structure <- estimate_panel_structure(gt, built_plot)
  density <- estimate_data_density(plot, built_plot, panel_structure)
  geometry <- estimate_plot_geometry(plot, built_plot, panel_structure, component_sizes)
  axis_positions <- estimate_axis_label_positions(built_plot, text_grobs)

  list(
    plot_id = plot_id,
    plot_index = plot_index,
    plot = plot,
    gtable = gt,
    page = page_spec,
    components = components,
    component_sizes = component_sizes,
    text_grobs = text_grobs,
    fixed_size = fixed_size,
    unit_profile = unit_profile,
    panels = panel_structure,
    density = density,
    geometry = geometry,
    axis_positions = axis_positions,
    warnings = warnings
  )
}

extract_gtable_components <- function(gt) {
  layout_names <- gt$layout$name

  component_table <- data.frame(
    index = seq_along(layout_names),
    name = layout_names,
    type = "other",
    t = gt$layout$t,
    l = gt$layout$l,
    b = gt$layout$b,
    r = gt$layout$r,
    stringsAsFactors = FALSE
  )

  component_table$type[grepl("^panel", layout_names)] <- "panel"
  component_table$type[grepl("^axis-b", layout_names)] <- "axis_b"
  component_table$type[grepl("^axis-t", layout_names)] <- "axis_t"
  component_table$type[grepl("^axis-l", layout_names)] <- "axis_l"
  component_table$type[grepl("^axis-r", layout_names)] <- "axis_r"
  component_table$type[grepl("^strip-", layout_names)] <- "strip"
  component_table$type[grepl("guide-box", layout_names)] <- "legend"
  component_table$type[grepl("^xlab", layout_names)] <- "xlab"
  component_table$type[grepl("^ylab", layout_names)] <- "ylab"
  component_table$type[grepl("title|subtitle|caption|tag", layout_names)] <- "title"

  component_table
}

measure_component_sizes <- function(gt, components) {
  rows <- vector("list", nrow(components))

  for (component_index in seq_len(nrow(components))) {
    grob <- gt$grobs[[components$index[component_index]]]
    grob_size <- measure_grob_mm(grob)

    rows[[component_index]] <- data.frame(
      index = components$index[component_index],
      name = components$name[component_index],
      type = components$type[component_index],
      width_mm = grob_size$width_mm,
      height_mm = grob_size$height_mm,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

collect_plot_text_grobs <- function(gt, components) {
  rows <- list()
  informative_components <- components[components$type != "panel", , drop = FALSE]

  for (component_index in seq_len(nrow(informative_components))) {
    component <- informative_components[component_index, , drop = FALSE]
    grob <- gt$grobs[[component$index]]

    component_rows <- collect_text_grobs(
      grob = grob,
      component_type = component$type,
      component_name = component$name,
      path = component$name
    )

    if (nrow(component_rows) > 0) {
      rows[[length(rows) + 1]] <- component_rows
    }
  }

  if (length(rows) == 0) {
    return(empty_text_grobs())
  }

  do.call(rbind, rows)
}

estimate_fixed_non_panel_size <- function(gt, components) {
  panel_components <- components[components$type == "panel", , drop = FALSE]

  if (nrow(panel_components) == 0) {
    return(list(left_mm = 0, right_mm = 0, top_mm = 0, bottom_mm = 0))
  }

  panel_col_min <- min(panel_components$l)
  panel_col_max <- max(panel_components$r)
  panel_row_min <- min(panel_components$t)
  panel_row_max <- max(panel_components$b)

  left_mm <- sum(vapply(gt$widths[seq_len(max(panel_col_min - 1, 0))], safe_convert_width_mm, numeric(1)))
  right_indices <- if (panel_col_max < length(gt$widths)) {
    seq.int(panel_col_max + 1, length(gt$widths))
  } else {
    integer()
  }
  right_mm <- if (length(right_indices) > 0) {
    sum(vapply(gt$widths[right_indices], safe_convert_width_mm, numeric(1)))
  } else {
    0
  }

  top_mm <- sum(vapply(gt$heights[seq_len(max(panel_row_min - 1, 0))], safe_convert_height_mm, numeric(1)))
  bottom_indices <- if (panel_row_max < length(gt$heights)) {
    seq.int(panel_row_max + 1, length(gt$heights))
  } else {
    integer()
  }
  bottom_mm <- if (length(bottom_indices) > 0) {
    sum(vapply(gt$heights[bottom_indices], safe_convert_height_mm, numeric(1)))
  } else {
    0
  }

  list(
    left_mm = left_mm,
    right_mm = right_mm,
    top_mm = top_mm,
    bottom_mm = bottom_mm
  )
}

estimate_panel_structure <- function(gt, built_plot = NULL) {
  panel_components <- gt$layout[grepl("^panel", gt$layout$name), , drop = FALSE]

  if (nrow(panel_components) == 0) {
    return(list(n_panel_rows = 0L, n_panel_cols = 0L, n_panels = 0L))
  }

  n_panel_rows <- length(unique(panel_components$t))
  n_panel_cols <- length(unique(panel_components$l))

  if (!is.null(built_plot) && !is.null(built_plot$layout$layout)) {
    n_panels <- nrow(built_plot$layout$layout)
  } else {
    n_panels <- nrow(panel_components)
  }

  list(
    n_panel_rows = as.integer(max(1, n_panel_rows)),
    n_panel_cols = as.integer(max(1, n_panel_cols)),
    n_panels = as.integer(max(1, n_panels))
  )
}

estimate_data_density <- function(plot, built_plot = NULL, panel_structure = NULL) {
  if (is.null(built_plot)) {
    built_plot <- try(ggplot2::ggplot_build(plot), silent = TRUE)
  }

  if (inherits(built_plot, "try-error") || is.null(built_plot$data)) {
    return(list(n_points_estimate = NA_real_, n_points_per_panel_estimate = NA_real_))
  }

  layer_counts <- vapply(built_plot$data, nrow, numeric(1))
  n_points_estimate <- sum(layer_counts, na.rm = TRUE)

  n_panels <- 1
  if (!is.null(panel_structure$n_panels) && is.finite(panel_structure$n_panels)) {
    n_panels <- max(1, panel_structure$n_panels)
  }

  list(
    n_points_estimate = n_points_estimate,
    n_points_per_panel_estimate = n_points_estimate / n_panels
  )
}

estimate_plot_geometry <- function(
    plot,
    built_plot = NULL,
    panel_structure = NULL,
    component_sizes = NULL) {
  geom_classes <- vapply(plot$layers, function(layer) {
    class(layer$geom)[1]
  }, character(1))

  point_count <- 0
  point_layer_indices <- which(grepl("GeomPoint|GeomJitter|GeomDotplot", geom_classes))
  if (!is.null(built_plot) && !is.null(built_plot$data) && length(point_layer_indices) > 0) {
    point_count <- sum(vapply(built_plot$data[point_layer_indices], nrow, numeric(1)), na.rm = TRUE)
  }

  nonempty_text_label_count <- 0
  text_layer_indices <- which(grepl("GeomText|GeomLabel|GeomTextRepel|GeomLabelRepel", geom_classes))
  if (!is.null(built_plot) && !is.null(built_plot$data) && length(text_layer_indices) > 0) {
    nonempty_text_label_count <- sum(vapply(text_layer_indices, function(layer_index) {
      layer_data <- built_plot$data[[layer_index]]
      if (is.null(layer_data$label)) {
        return(0)
      }
      labels <- as.character(layer_data$label)
      sum(!is.na(labels) & nzchar(trimws(labels)))
    }, numeric(1)), na.rm = TRUE)
  }

  n_panels <- 1
  if (!is.null(panel_structure$n_panels) && is.finite(panel_structure$n_panels)) {
    n_panels <- max(1, panel_structure$n_panels)
  }

  n_panel_rows <- scalar_or_default(panel_structure$n_panel_rows, 1)
  n_panel_cols <- scalar_or_default(panel_structure$n_panel_cols, 1)
  legend_components <- if (!is.null(component_sizes) && "type" %in% names(component_sizes)) {
    component_sizes[component_sizes$type == "legend" &
      component_sizes$width_mm > 0 & component_sizes$height_mm > 0, , drop = FALSE]
  } else {
    data.frame()
  }
  legend_width_mm <- if (nrow(legend_components) > 0) max(legend_components$width_mm) else 0
  legend_height_mm <- if (nrow(legend_components) > 0) max(legend_components$height_mm) else 0
  legend_area_mm2 <- if (nrow(legend_components) > 0) {
    sum(legend_components$width_mm * legend_components$height_mm)
  } else {
    0
  }

  target_panel_aspect <- infer_theme_aspect_ratio(plot)
  aspect_penalty <- if (is.finite(target_panel_aspect)) 25 else 0

  list(
    geom_classes = geom_classes,
    has_point_geom = length(point_layer_indices) > 0,
    has_distribution_geom = any(grepl("GeomBoxplot|GeomViolin|GeomHistogram|GeomDensity", geom_classes)),
    is_scatter_like = FALSE,
    is_faceted_scatter = FALSE,
    n_points_in_point_layers = point_count,
    n_points_per_panel_in_point_layers = point_count / n_panels,
    n_nonempty_text_labels = nonempty_text_label_count,
    n_panel_rows = as.integer(max(1, n_panel_rows)),
    n_panel_cols = as.integer(max(1, n_panel_cols)),
    n_panels = as.integer(n_panels),
    legend_width_mm = legend_width_mm,
    legend_height_mm = legend_height_mm,
    legend_area_mm2 = legend_area_mm2,
    target_panel_aspect = target_panel_aspect,
    aspect_penalty = aspect_penalty
  )
}

infer_theme_aspect_ratio <- function(plot) {
  aspect <- NA_real_

  theme_aspect <- try(plot$theme$aspect.ratio, silent = TRUE)
  if (!inherits(theme_aspect, "try-error") && is.numeric(theme_aspect) && length(theme_aspect) == 1) {
    aspect <- as.numeric(theme_aspect)
  }

  if (!is.finite(aspect)) {
    coord_ratio <- try(plot$coordinates$ratio, silent = TRUE)
    if (!inherits(coord_ratio, "try-error") && is.numeric(coord_ratio) && length(coord_ratio) == 1) {
      aspect <- as.numeric(coord_ratio)
    }
  }

  if (!is.finite(aspect) || aspect <= 0) {
    return(NA_real_)
  }

  aspect
}

resolve_gtable_at_size <- function(gt, width_mm, height_mm) {
  width_units <- resolve_unit_vector_mm(gt$widths, width_mm, axis = "width")
  height_units <- resolve_unit_vector_mm(gt$heights, height_mm, axis = "height")

  components <- extract_gtable_components(gt)
  panel_components <- components[components$type == "panel", , drop = FALSE]

  if (nrow(panel_components) == 0) {
    return(list(
      widths_mm = width_units,
      heights_mm = height_units,
      fixed_size = list(left_mm = 0, right_mm = 0, top_mm = 0, bottom_mm = 0),
      panel_width_mm = 0,
      panel_height_mm = 0,
      facet_panel_width_mm = 0,
      facet_panel_height_mm = 0
    ))
  }

  panel_col_min <- min(panel_components$l)
  panel_col_max <- max(panel_components$r)
  panel_row_min <- min(panel_components$t)
  panel_row_max <- max(panel_components$b)

  left_mm <- sum(width_units[seq_len(max(panel_col_min - 1, 0))])
  right_indices <- if (panel_col_max < length(width_units)) seq.int(panel_col_max + 1, length(width_units)) else integer()
  right_mm <- sum(width_units[right_indices])
  top_mm <- sum(height_units[seq_len(max(panel_row_min - 1, 0))])
  bottom_indices <- if (panel_row_max < length(height_units)) seq.int(panel_row_max + 1, length(height_units)) else integer()
  bottom_mm <- sum(height_units[bottom_indices])

  panel_cols <- unique(panel_components$l)
  panel_rows <- unique(panel_components$t)
  panel_width_mm <- sum(width_units[seq.int(panel_col_min, panel_col_max)])
  panel_height_mm <- sum(height_units[seq.int(panel_row_min, panel_row_max)])

  list(
    widths_mm = width_units,
    heights_mm = height_units,
    fixed_size = list(left_mm = left_mm, right_mm = right_mm, top_mm = top_mm, bottom_mm = bottom_mm),
    panel_width_mm = panel_width_mm,
    panel_height_mm = panel_height_mm,
    facet_panel_width_mm = if (length(panel_cols) > 0) panel_width_mm / length(panel_cols) else panel_width_mm,
    facet_panel_height_mm = if (length(panel_rows) > 0) panel_height_mm / length(panel_rows) else panel_height_mm
  )
}

resolve_plot_profile_at_size <- function(profile, width_mm, height_mm) {
  if (is.null(profile$unit_profile)) {
    return(resolve_gtable_at_size(profile$gtable, width_mm, height_mm))
  }

  unit_profile <- profile$unit_profile
  width_units <- resolve_cached_unit_profile(unit_profile$widths, width_mm)
  height_units <- resolve_cached_unit_profile(unit_profile$heights, height_mm)
  panel <- unit_profile$panel

  if (is.null(panel) || !is.finite(panel$panel_col_min)) {
    return(list(
      widths_mm = width_units,
      heights_mm = height_units,
      fixed_size = list(left_mm = 0, right_mm = 0, top_mm = 0, bottom_mm = 0),
      panel_width_mm = 0,
      panel_height_mm = 0,
      facet_panel_width_mm = 0,
      facet_panel_height_mm = 0
    ))
  }

  left_mm <- sum(width_units[seq_len(max(panel$panel_col_min - 1, 0))])
  right_indices <- if (panel$panel_col_max < length(width_units)) seq.int(panel$panel_col_max + 1, length(width_units)) else integer()
  right_mm <- sum(width_units[right_indices])
  top_mm <- sum(height_units[seq_len(max(panel$panel_row_min - 1, 0))])
  bottom_indices <- if (panel$panel_row_max < length(height_units)) seq.int(panel$panel_row_max + 1, length(height_units)) else integer()
  bottom_mm <- sum(height_units[bottom_indices])

  panel_width_mm <- sum(width_units[seq.int(panel$panel_col_min, panel$panel_col_max)])
  panel_height_mm <- sum(height_units[seq.int(panel$panel_row_min, panel$panel_row_max)])

  list(
    widths_mm = width_units,
    heights_mm = height_units,
    fixed_size = list(left_mm = left_mm, right_mm = right_mm, top_mm = top_mm, bottom_mm = bottom_mm),
    panel_width_mm = panel_width_mm,
    panel_height_mm = panel_height_mm,
    facet_panel_width_mm = panel_width_mm / max(1, panel$n_panel_cols),
    facet_panel_height_mm = panel_height_mm / max(1, panel$n_panel_rows)
  )
}

prepare_gtable_unit_profile <- function(gt, components = NULL) {
  if (is.null(components)) {
    components <- extract_gtable_components(gt)
  }

  panel_components <- components[components$type == "panel", , drop = FALSE]
  if (nrow(panel_components) == 0) {
    panel <- list(
      panel_col_min = NA_real_,
      panel_col_max = NA_real_,
      panel_row_min = NA_real_,
      panel_row_max = NA_real_,
      n_panel_cols = 0L,
      n_panel_rows = 0L
    )
  } else {
    panel <- list(
      panel_col_min = min(panel_components$l),
      panel_col_max = max(panel_components$r),
      panel_row_min = min(panel_components$t),
      panel_row_max = max(panel_components$b),
      n_panel_cols = length(unique(panel_components$l)),
      n_panel_rows = length(unique(panel_components$t))
    )
  }

  list(
    widths = prepare_unit_vector_profile(gt$widths, axis = "width"),
    heights = prepare_unit_vector_profile(gt$heights, axis = "height"),
    panel = panel
  )
}

prepare_unit_vector_profile <- function(unit_vector, axis = c("width", "height")) {
  axis <- match.arg(axis)
  unit_count <- length(unit_vector)
  fixed_mm <- numeric(unit_count)
  null_weights <- numeric(unit_count)

  for (unit_index in seq_len(unit_count)) {
    unit_item <- unit_vector[unit_index]
    unit_type <- try(grid::unitType(unit_item), silent = TRUE)
    if (!inherits(unit_type, "try-error") && identical(unit_type, "null")) {
      weight <- suppressWarnings(as.numeric(unit_item))
      null_weights[unit_index] <- if (is.finite(weight) && weight > 0) weight else 1
    } else {
      fixed_mm[unit_index] <- if (axis == "width") {
        safe_convert_width_mm(unit_item)
      } else {
        safe_convert_height_mm(unit_item)
      }
    }
  }

  list(fixed_mm = fixed_mm, null_weights = null_weights)
}

resolve_cached_unit_profile <- function(unit_profile, total_mm) {
  values <- unit_profile$fixed_mm
  null_weights <- unit_profile$null_weights
  null_total <- sum(null_weights)
  fixed_total <- sum(values)
  remaining <- max(0, total_mm - fixed_total)

  if (null_total > 0) {
    values[null_weights > 0] <- remaining * null_weights[null_weights > 0] / null_total
  } else if (fixed_total > 0 && is.finite(total_mm) && total_mm > 0) {
    values <- values * min(1, total_mm / fixed_total)
  }

  values
}

resolve_unit_vector_mm <- function(unit_vector, total_mm, axis = c("width", "height")) {
  axis <- match.arg(axis)
  unit_count <- length(unit_vector)
  if (unit_count == 0) {
    return(numeric())
  }

  values <- numeric(unit_count)
  null_weights <- numeric(unit_count)

  for (unit_index in seq_len(unit_count)) {
    unit_item <- unit_vector[unit_index]
    unit_type <- try(grid::unitType(unit_item), silent = TRUE)
    if (!inherits(unit_type, "try-error") && identical(unit_type, "null")) {
      weight <- suppressWarnings(as.numeric(unit_item))
      null_weights[unit_index] <- if (is.finite(weight) && weight > 0) weight else 1
    } else {
      values[unit_index] <- if (axis == "width") {
        safe_convert_width_mm(unit_item)
      } else {
        safe_convert_height_mm(unit_item)
      }
    }
  }

  fixed_total <- sum(values)
  null_total <- sum(null_weights)
  remaining <- max(0, total_mm - fixed_total)

  if (null_total > 0) {
    values[null_weights > 0] <- remaining * null_weights[null_weights > 0] / null_total
  } else if (fixed_total > 0 && is.finite(total_mm) && total_mm > 0) {
    values <- values * min(1, total_mm / fixed_total)
  }

  values
}

measure_plot_at_size <- function(plot, width_mm, height_mm, page_spec = NULL, plot_id = "plot") {
  if (is.list(plot) && !is.null(plot$gtable) && !is.null(plot$components)) {
    profile <- plot
  } else if (inherits(plot, "ggplot")) {
    if (is.null(page_spec)) {
      page_spec <- list(width_mm = width_mm, height_mm = height_mm)
    }
    profile <- measure_plot_profile(
      plot = plot,
      plot_id = plot_id,
      plot_index = 1L,
      page_spec = page_spec,
      measurement_spec = list()
    )
  } else {
    stop("`plot` must be a ggplot object or a measured plot profile.", call. = FALSE)
  }

  resolved <- resolve_plot_profile_at_size(profile, width_mm, height_mm)
  legends <- profile$component_sizes[profile$component_sizes$type == "legend", , drop = FALSE]
  legend_area_mm2 <- if (nrow(legends) > 0) sum(legends$width_mm * legends$height_mm) else 0

  list(
    width_mm = width_mm,
    height_mm = height_mm,
    panel_width_mm = resolved$panel_width_mm,
    panel_height_mm = resolved$panel_height_mm,
    facet_panel_width_mm = resolved$facet_panel_width_mm,
    facet_panel_height_mm = resolved$facet_panel_height_mm,
    fixed_size = resolved$fixed_size,
    legend_area_mm2 = legend_area_mm2,
    text_grobs = profile$text_grobs
  )
}

estimate_axis_label_positions <- function(built_plot, text_grobs) {
  list(
    x = estimate_axis_positions_for_axis(built_plot, text_grobs, axis = "x"),
    y = estimate_axis_positions_for_axis(built_plot, text_grobs, axis = "y")
  )
}

estimate_axis_positions_for_axis <- function(built_plot, text_grobs, axis = c("x", "y")) {
  axis <- match.arg(axis)
  component_types <- if (axis == "x") c("axis_b", "axis_t") else c("axis_l", "axis_r")
  labels <- text_grobs[text_grobs$component_type %in% component_types, , drop = FALSE]

  if (nrow(labels) <= 1) {
    return(list(positions = numeric(), labels = character(), fallback = FALSE))
  }

  positions <- extract_axis_positions_from_panel_params(built_plot, axis, nrow(labels))
  fallback <- is.null(positions) || length(positions) != nrow(labels) || any(!is.finite(positions))
  if (fallback) {
    positions <- if (nrow(labels) == 1) 0.5 else seq(0, 1, length.out = nrow(labels))
  }

  list(
    positions = pmin(pmax(as.numeric(positions), 0), 1),
    labels = labels$text,
    fallback = fallback
  )
}

extract_axis_positions_from_panel_params <- function(built_plot, axis, expected_count) {
  if (is.null(built_plot) || is.null(built_plot$layout$panel_params) ||
      length(built_plot$layout$panel_params) == 0) {
    return(NULL)
  }

  panel_params <- built_plot$layout$panel_params[[1]]
  axis_obj <- try(panel_params[[axis]], silent = TRUE)
  if (inherits(axis_obj, "try-error") || is.null(axis_obj)) {
    return(NULL)
  }

  positions <- try(axis_obj$break_positions(), silent = TRUE)
  if (inherits(positions, "try-error") || is.null(positions)) {
    positions <- try(axis_obj$get_breaks(), silent = TRUE)
  }
  if (inherits(positions, "try-error") || is.null(positions)) {
    positions <- try(axis_obj$breaks, silent = TRUE)
  }
  if (inherits(positions, "try-error") || is.null(positions)) {
    return(NULL)
  }

  positions <- suppressWarnings(as.numeric(positions))
  positions <- positions[is.finite(positions)]
  if (length(positions) == 0) {
    return(NULL)
  }

  if (length(positions) != expected_count) {
    return(NULL)
  }

  if (min(positions) < 0 || max(positions) > 1) {
    range_value <- range(positions)
    span <- diff(range_value)
    if (span <= 0) {
      return(NULL)
    }
    positions <- (positions - range_value[1]) / span
  }

  positions
}


