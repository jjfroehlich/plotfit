###################################################################################################################/
# Unified demo runner ####
###################################################################################################################/

# This is the canonical demo entry point. It builds several ordinary ggplot
# lists, runs the optimizer without per-plot footprint inputs, and writes final
# PDF reports under demo_output/. Diagnostics are written only when requested.

script_path_from_command_args <- function() {
  script_args <- commandArgs(trailingOnly = FALSE)
  script_file_arg <- grep("^--file=", script_args, value = TRUE)
  if (length(script_file_arg) == 0) {
    return(NA_character_)
  }
  normalizePath(sub("^--file=", "", script_file_arg[1]), winslash = "/", mustWork = FALSE)
}

current_source_path <- function() {
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(error) NA_character_)
  if (is.null(source_file) || length(source_file) == 0 || is.na(source_file)) {
    return(script_path_from_command_args())
  }
  normalizePath(source_file, winslash = "/", mustWork = FALSE)
}

canonical_demo_script <- current_source_path()
canonical_project_dir <- normalizePath(file.path(dirname(canonical_demo_script), ".."), winslash = "/", mustWork = TRUE)

source_optimizer <- function(project_dir) {
  r_files <- list.files(file.path(project_dir, "R"), pattern = "\\.R$", full.names = TRUE)
  for (r_file in sort(r_files)) {
    source(r_file)
  }
}

set_demo_theme <- function(text_size = 7, base_family = "sans") {
  theme_set(
    theme_classic(base_size = text_size, base_family = base_family) +
      theme(
        text = element_text(size = text_size, color = "black"),
        plot.title = element_text(size = text_size, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = text_size, color = "grey30", hjust = 0.5),
        axis.title = element_text(size = text_size, color = "black"),
        axis.text = element_text(size = text_size, color = "black"),
        legend.text = element_text(size = text_size, color = "black"),
        legend.title = element_text(size = text_size, color = "black"),
        strip.text.x = element_text(size = text_size, color = "black"),
        strip.text.y = element_text(size = text_size, color = "black"),
        strip.background = element_rect(fill = "grey85", colour = NA),
        axis.line.x = element_line(lineend = "square", color = "black"),
        axis.line.y = element_line(lineend = "square", color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.key.size = grid::unit(0.2, "cm"),
        legend.key.height = grid::unit(0.75, "lines"),
        legend.background = element_rect(fill = "white", linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.1)
      )
  )
}

demo_palette <- function() {
  c(
    black = "#000000",
    orange = "#E69F00",
    sky_blue = "#56B4E9",
    bluish_green = "#009E73",
    yellow = "#F0E442",
    blue = "#0072B2",
    vermillion = "#D55E00",
    reddish_purple = "#CC79A7",
    grey = "#999999"
  )
}

demo_compact_theme <- function() {
  theme(
    plot.margin = grid::unit(c(1.5, 1.5, 1.5, 1.5), "mm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.x = grid::unit(1, "mm"),
    legend.spacing.y = grid::unit(0.5, "mm")
  )
}

write_layout_result <- function(
    layout_result,
    output_dir,
    prefix,
    page_width_in,
    page_height_in,
    text_size,
    label,
    diagnostics = FALSE) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  pdf_filename <- file.path(output_dir, paste0(prefix, ".pdf"))
  pdf(pdf_filename, width = page_width_in, height = page_height_in, family = "Helvetica", pointsize = text_size)
  draw_layout_pages(layout_result)
  dev.off()

  message(label, " PDF written to: ", pdf_filename)

  diagnostics_filename <- NA_character_
  layout_diagnostics_filename <- NA_character_
  patchwork_code_filename <- NA_character_
  warnings_filename <- NA_character_

  if (diagnostics) {
    diagnostics_filename <- file.path(output_dir, paste0(prefix, "_plot_diagnostics.tsv"))
    utils::write.table(
      layout_result$plot_diagnostics,
      diagnostics_filename,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE
    )

    layout_diagnostics_filename <- file.path(output_dir, paste0(prefix, "_layout_diagnostics.tsv"))
    utils::write.table(
      layout_result$layout_diagnostics,
      layout_diagnostics_filename,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE
    )

    patchwork_code_filename <- file.path(output_dir, paste0(prefix, "_patchwork_code.R"))
    patchwork_code_lines <- unlist(lapply(seq_along(layout_result$pages), function(page_index) {
      c(
        paste0("# Page ", page_index, " ####"),
        layout_result$pages[[page_index]]$patchwork_code,
        ""
      )
    }))
    writeLines(patchwork_code_lines, patchwork_code_filename)

    warnings_filename <- file.path(output_dir, paste0(prefix, "_warnings.txt"))
    writeLines(layout_result$warnings, warnings_filename)

    message(label, " plot diagnostics written to: ", diagnostics_filename)
    message(label, " layout diagnostics written to: ", layout_diagnostics_filename)
    message(label, " editable patchwork code written to: ", patchwork_code_filename)
  }

  data.frame(
    scenario = label,
    prefix = prefix,
    pdf = normalizePath(pdf_filename, winslash = "/", mustWork = FALSE),
    plot_diagnostics = normalizePath(diagnostics_filename, winslash = "/", mustWork = FALSE),
    layout_diagnostics = normalizePath(layout_diagnostics_filename, winslash = "/", mustWork = FALSE),
    patchwork_code = normalizePath(patchwork_code_filename, winslash = "/", mustWork = FALSE),
    warnings = normalizePath(warnings_filename, winslash = "/", mustWork = FALSE),
    n_pages = length(layout_result$pages),
    n_warnings = length(layout_result$warnings),
    stringsAsFactors = FALSE
  )
}

make_original_feedback_plots <- function() {
  set.seed(20260602)
  okabe_ito <- demo_palette()
  compact_theme <- demo_compact_theme()

  iris_data <- iris
  iris_data$Species <- factor(iris_data$Species)

  mtcars_data <- mtcars
  mtcars_data$car <- rownames(mtcars)
  mtcars_data$cyl <- factor(mtcars_data$cyl)

  airquality_data <- airquality[complete.cases(airquality[, c("Ozone", "Solar.R", "Wind", "Temp", "Month")]), ]
  airquality_data$Month <- factor(
    airquality_data$Month,
    levels = 5:9,
    labels = c("May", "June", "July", "August", "September")
  )

  mpg_data <- ggplot2::mpg
  mpg_data$class <- factor(mpg_data$class)

  diamonds_sample <- ggplot2::diamonds[sample(seq_len(nrow(ggplot2::diamonds)), 3000), ]
  diamond_color_levels <- levels(diamonds_sample$color)

  state_pca <- stats::prcomp(USArrests, scale. = TRUE)
  state_pca_data <- data.frame(
    state = rownames(USArrests),
    PC1 = state_pca$x[, 1],
    PC2 = state_pca$x[, 2],
    Murder = USArrests$Murder,
    UrbanPop = USArrests$UrbanPop,
    stringsAsFactors = FALSE
  )
  state_pca_data$urban_group <- ifelse(
    state_pca_data$UrbanPop >= median(state_pca_data$UrbanPop),
    "higher urban",
    "lower urban"
  )
  state_label_data <- state_pca_data[order(state_pca_data$Murder, decreasing = TRUE), ][seq_len(8), ]

  list(
    p1_iris_scatter = ggplot(iris_data, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
      geom_point(size = 1.15, alpha = 0.78) +
      scale_color_manual(values = c(
        setosa = unname(okabe_ito["bluish_green"]),
        versicolor = unname(okabe_ito["blue"]),
        virginica = unname(okabe_ito["vermillion"])
      )) +
      labs(title = "Iris length relationship", x = "Sepal length", y = "Petal length", color = "Species") +
      theme(legend.position = "bottom", aspect.ratio = 1) +
      compact_theme,

    p2_mtcars_box_jitter = ggplot(mtcars_data, aes(x = cyl, y = mpg, fill = cyl)) +
      geom_boxplot(outlier.shape = NA, linewidth = 0.35, width = 0.55) +
      geom_jitter(width = 0.10, height = 0, alpha = 0.68, size = 1.0, color = "grey25") +
      scale_fill_manual(values = c(
        "4" = unname(okabe_ito["sky_blue"]),
        "6" = unname(okabe_ito["orange"]),
        "8" = unname(okabe_ito["reddish_purple"])
      )) +
      labs(title = "Fuel economy by cylinders", x = "Cylinders", y = "Miles per gallon") +
      theme(legend.position = "none", aspect.ratio = 0.5) +
      compact_theme,

    p3_diamond_histogram = ggplot(diamonds_sample, aes(x = carat)) +
      geom_histogram(binwidth = 0.10, boundary = 0, fill = okabe_ito["sky_blue"], color = "black", linewidth = 0.25) +
      geom_vline(xintercept = stats::median(diamonds_sample$carat), linetype = "dashed", color = "grey35", linewidth = 0.25) +
      labs(title = "Diamond carat distribution", x = "Carat", y = "Count") +
      theme(aspect.ratio = 0.45) +
      compact_theme,

    p4_usarrests_pca = ggplot(state_pca_data, aes(x = PC1, y = PC2, color = urban_group)) +
      geom_hline(yintercept = 0, color = "grey75", linewidth = 0.25) +
      geom_vline(xintercept = 0, color = "grey75", linewidth = 0.25) +
      geom_point(aes(size = Murder), alpha = 0.82) +
      geom_text(
        data = state_label_data,
        aes(label = state),
        color = "black",
        size = 2.1,
        nudge_y = 0.18,
        check_overlap = TRUE,
        show.legend = FALSE
      ) +
      scale_color_manual(values = c(
        "higher urban" = unname(okabe_ito["blue"]),
        "lower urban" = unname(okabe_ito["orange"])
      )) +
      scale_size_continuous(range = c(1.0, 2.8)) +
      labs(title = "USArrests PCA", x = "PC1", y = "PC2", color = "Urban group", size = "Murder") +
      theme(legend.position = "none", aspect.ratio = 1) +
      compact_theme,

    p5_diamond_violin = ggplot(diamonds_sample, aes(x = color, y = price, fill = color)) +
      geom_violin(trim = FALSE, color = "black", linewidth = 0.25, draw_quantiles = 0.5) +
      scale_fill_manual(values = setNames(
        unname(c(
          okabe_ito["sky_blue"],
          okabe_ito["orange"],
          okabe_ito["bluish_green"],
          okabe_ito["vermillion"],
          okabe_ito["blue"],
          okabe_ito["reddish_purple"],
          okabe_ito["grey"]
        )),
        diamond_color_levels
      )) +
      labs(title = "Diamond price by color", x = "Color grade", y = "Price") +
      theme(legend.position = "none", aspect.ratio = 0.5) +
      compact_theme,

    p6_mpg_faceted_scatter = ggplot(mpg_data, aes(x = displ, y = hwy, color = drv)) +
      geom_point(size = 0.95, alpha = 0.72) +
      facet_wrap(~class, nrow = 2) +
      scale_color_manual(values = c(
        "4" = unname(okabe_ito["blue"]),
        "f" = unname(okabe_ito["bluish_green"]),
        "r" = unname(okabe_ito["vermillion"])
      )) +
      labs(title = "Engine size and highway mileage", x = "Engine displacement", y = "Highway mpg", color = "Drive") +
      theme(legend.position = "none") +
      compact_theme,

    p7_airquality_box_jitter = ggplot(airquality_data, aes(x = Month, y = Ozone, fill = Month)) +
      geom_boxplot(outlier.shape = NA, linewidth = 0.35, width = 0.55) +
      geom_jitter(width = 0.10, height = 0, alpha = 0.45, size = 0.75, color = "grey25") +
      scale_fill_manual(values = c(
        May = unname(okabe_ito["sky_blue"]),
        June = unname(okabe_ito["bluish_green"]),
        July = unname(okabe_ito["orange"]),
        August = unname(okabe_ito["vermillion"]),
        September = unname(okabe_ito["reddish_purple"])
      )) +
      labs(title = "Ozone by month", x = "Month", y = "Ozone") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "none", aspect.ratio = 0.5) +
      compact_theme,

    p8_mtcars_scatter = ggplot(mtcars_data, aes(x = wt, y = mpg, color = cyl)) +
      geom_point(size = 1.3, alpha = 0.84) +
      scale_color_manual(values = c(
        "4" = unname(okabe_ito["sky_blue"]),
        "6" = unname(okabe_ito["orange"]),
        "8" = unname(okabe_ito["reddish_purple"])
      )) +
      labs(title = "Weight and fuel economy", x = "Weight", y = "Miles per gallon", color = "Cylinders") +
      theme(legend.position = "none", aspect.ratio = 1) +
      compact_theme
  )
}

make_generalization_feedback_plots <- function() {
  set.seed(20260622)
  okabe_ito <- demo_palette()
  compact_theme <- demo_compact_theme()

  economics_small <- ggplot2::economics[seq(1, nrow(ggplot2::economics), by = 8), ]
  economics_small$unemploy_k <- economics_small$unemploy / 1000

  long_label_data <- data.frame(
    group = factor(
      c(
        "Very long condition A",
        "Very long condition B",
        "Very long condition C",
        "Very long condition D",
        "Very long condition E"
      ),
      levels = c(
        "Very long condition A",
        "Very long condition B",
        "Very long condition C",
        "Very long condition D",
        "Very long condition E"
      )
    ),
    value = c(7.2, 5.8, 8.9, 6.3, 7.7)
  )

  volcano_small <- as.data.frame(as.table(volcano[seq(1, nrow(volcano), by = 4), seq(1, ncol(volcano), by = 4)]))
  names(volcano_small) <- c("row", "col", "height")
  volcano_small$row <- as.integer(volcano_small$row)
  volcano_small$col <- as.integer(volcano_small$col)

  mtcars_data <- mtcars
  mtcars_data$cyl <- factor(mtcars_data$cyl)
  mtcars_data$gear <- factor(mtcars_data$gear)
  mtcars_data$car <- rownames(mtcars)
  mtcars_data$label <- ifelse(mtcars_data$mpg > 30 | mtcars_data$wt > 5, mtcars_data$car, "")

  iris_data <- iris
  iris_data$size_band <- cut(
    iris_data$Sepal.Length,
    breaks = quantile(iris_data$Sepal.Length, probs = seq(0, 1, length.out = 5)),
    include.lowest = TRUE
  )

  list(
    unemployment_line = ggplot(economics_small, aes(date, unemploy_k)) +
      geom_line(color = okabe_ito["blue"], linewidth = 0.35) +
      geom_point(color = okabe_ito["blue"], size = 0.55) +
      labs(title = "Unemployment over time", x = "Date", y = "Unemployed (thousands)") +
      compact_theme,

    long_label_bars = ggplot(long_label_data, aes(group, value, fill = group)) +
      geom_col(width = 0.65, color = "black", linewidth = 0.25) +
      scale_fill_manual(values = rep(unname(okabe_ito[c("sky_blue", "orange", "bluish_green", "vermillion", "reddish_purple")]), length.out = 5)) +
      labs(title = "Long category labels", x = "Condition", y = "Response") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "none", aspect.ratio = 0.45) +
      compact_theme,

    volcano_heatmap = ggplot(volcano_small, aes(col, row, fill = height)) +
      geom_tile() +
      scale_fill_gradient(low = "#F7FBFF", high = okabe_ito["blue"]) +
      labs(title = "Volcano height heatmap", x = "Column", y = "Row", fill = "Height") +
      theme(aspect.ratio = 1, legend.position = "right") +
      compact_theme,

    faceted_histograms = ggplot(mtcars_data, aes(mpg, fill = cyl)) +
      geom_histogram(bins = 8, color = "black", linewidth = 0.2) +
      facet_wrap(~gear, nrow = 1) +
      scale_fill_manual(values = unname(okabe_ito[c("sky_blue", "orange", "reddish_purple")])) +
      labs(title = "Mileage by gear", x = "Miles per gallon", y = "Count", fill = "Cylinders") +
      theme(legend.position = "none") +
      compact_theme,

    grouped_boxplot = ggplot(iris_data, aes(size_band, Petal.Length, fill = Species)) +
      geom_boxplot(outlier.shape = NA, linewidth = 0.25) +
      geom_jitter(width = 0.08, height = 0, alpha = 0.35, size = 0.55, color = "grey25") +
      scale_fill_manual(values = unname(okabe_ito[c("bluish_green", "blue", "vermillion")])) +
      labs(title = "Petal length by size band", x = "Sepal length band", y = "Petal length", fill = "Species") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "bottom", aspect.ratio = 0.55) +
      compact_theme,

    annotated_scatter = ggplot(mtcars_data, aes(wt, mpg, color = cyl)) +
      geom_point(size = 1.1, alpha = 0.82) +
      geom_text(aes(label = label), color = "black", size = 2.0, nudge_y = 0.7, check_overlap = TRUE, show.legend = FALSE) +
      scale_color_manual(values = unname(okabe_ito[c("sky_blue", "orange", "reddish_purple")])) +
      labs(title = "Annotated vehicle outliers", x = "Weight", y = "Miles per gallon", color = "Cylinders") +
      theme(legend.position = "none", aspect.ratio = 1) +
      compact_theme,

    density_overlay = ggplot(iris_data, aes(Petal.Width, color = Species, fill = Species)) +
      geom_density(alpha = 0.18, linewidth = 0.35) +
      scale_color_manual(values = unname(okabe_ito[c("bluish_green", "blue", "vermillion")])) +
      scale_fill_manual(values = unname(okabe_ito[c("bluish_green", "blue", "vermillion")])) +
      labs(title = "Petal width density", x = "Petal width", y = "Density", color = "Species", fill = "Species") +
      theme(legend.position = "bottom", aspect.ratio = 0.5) +
      compact_theme,

    fixed_aspect_scatter = ggplot(iris_data, aes(Sepal.Width, Petal.Width, color = Species)) +
      geom_point(size = 0.95, alpha = 0.78) +
      scale_color_manual(values = unname(okabe_ito[c("bluish_green", "blue", "vermillion")])) +
      labs(title = "Fixed aspect scatter", x = "Sepal width", y = "Petal width", color = "Species") +
      theme(legend.position = "none", aspect.ratio = 1) +
      compact_theme
  )
}

make_real_world_stress_plots <- function() {
  set.seed(20260623)
  okabe_ito <- demo_palette()
  compact_theme <- demo_compact_theme()

  time_data <- data.frame(
    day = seq.Date(as.Date("2024-01-01"), by = "week", length.out = 80),
    estimate = cumsum(stats::rnorm(80, 0.05, 0.45)) + 10
  )
  time_data$lower <- time_data$estimate - runif(80, 0.4, 0.9)
  time_data$upper <- time_data$estimate + runif(80, 0.4, 0.9)

  forest_data <- data.frame(
    endpoint = factor(
      c(
        "Primary clinical endpoint",
        "Biomarker response at week 4",
        "Biomarker response at week 12",
        "Safety composite",
        "Patient reported outcome",
        "Exploratory subgroup"
      ),
      levels = rev(c(
        "Primary clinical endpoint",
        "Biomarker response at week 4",
        "Biomarker response at week 12",
        "Safety composite",
        "Patient reported outcome",
        "Exploratory subgroup"
      ))
    ),
    estimate = c(0.18, 0.35, 0.42, -0.05, 0.22, 0.55),
    low = c(0.05, 0.12, 0.18, -0.20, 0.02, 0.10),
    high = c(0.31, 0.58, 0.66, 0.10, 0.42, 1.00)
  )

  genes <- paste0("Gene ", LETTERS[1:12])
  samples <- paste0("S", sprintf("%02d", seq_len(10)))
  heatmap_data <- expand.grid(gene = genes, sample = samples, cohort = c("Discovery", "Validation"))
  heatmap_data$score <- stats::rnorm(nrow(heatmap_data))

  diamonds_dense <- ggplot2::diamonds[sample(seq_len(nrow(ggplot2::diamonds)), 6000), ]
  diamonds_dense$price_band <- cut(diamonds_dense$price, breaks = 4)

  stacked_data <- expand.grid(
    condition = factor(paste("Condition", LETTERS[1:5])),
    source = factor(c("Baseline", "Treatment A", "Treatment B", "Rescue medication"))
  )
  stacked_data$value <- sample(15:75, nrow(stacked_data), replace = TRUE)

  economics_long <- data.frame(
    date = rep(ggplot2::economics$date[seq(1, nrow(ggplot2::economics), by = 12)], 4),
    value = c(
      scale(ggplot2::economics$unemploy[seq(1, nrow(ggplot2::economics), by = 12)]),
      scale(ggplot2::economics$psavert[seq(1, nrow(ggplot2::economics), by = 12)]),
      scale(ggplot2::economics$uempmed[seq(1, nrow(ggplot2::economics), by = 12)]),
      scale(ggplot2::economics$pce[seq(1, nrow(ggplot2::economics), by = 12)])
    ),
    series = rep(c("Unemployment", "Savings", "Median unemployment", "Consumption"), each = length(seq(1, nrow(ggplot2::economics), by = 12)))
  )
  economics_long$value <- as.numeric(economics_long$value)

  residual_data <- data.frame(
    fitted = stats::fitted(stats::lm(mpg ~ wt + hp, data = mtcars)),
    residual = stats::resid(stats::lm(mpg ~ wt + hp, data = mtcars)),
    cyl = factor(mtcars$cyl)
  )

  corr_matrix <- round(stats::cor(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]), 2)
  corr_data <- as.data.frame(as.table(corr_matrix))
  names(corr_data) <- c("metric_x", "metric_y", "correlation")

  list(
    longitudinal_ribbon = ggplot(time_data, aes(day, estimate)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = okabe_ito["sky_blue"], alpha = 0.25) +
      geom_line(color = okabe_ito["blue"], linewidth = 0.35) +
      labs(title = "Longitudinal estimate", x = "Date", y = "Estimate") +
      compact_theme,

    forest_intervals = ggplot(forest_data, aes(endpoint, estimate, ymin = low, ymax = high)) +
      geom_hline(yintercept = 0, color = "grey70", linewidth = 0.25) +
      geom_pointrange(color = okabe_ito["blue"], linewidth = 0.35) +
      coord_flip() +
      labs(title = "Effect estimates", x = NULL, y = "Standardized effect") +
      compact_theme,

    faceted_heatmap = ggplot(heatmap_data, aes(sample, gene, fill = score)) +
      geom_tile() +
      facet_wrap(~cohort, nrow = 1) +
      scale_fill_gradient2(low = okabe_ito["blue"], mid = "white", high = okabe_ito["vermillion"], midpoint = 0) +
      labs(title = "Faceted assay heatmap", x = "Sample", y = "Feature", fill = "Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") +
      compact_theme,

    dense_price_scatter = ggplot(diamonds_dense, aes(carat, price, color = price_band)) +
      geom_point(alpha = 0.18, size = 0.45) +
      scale_color_manual(values = unname(okabe_ito[c("sky_blue", "blue", "orange", "vermillion")])) +
      labs(title = "Dense price scatter", x = "Carat", y = "Price", color = "Price band") +
      theme(legend.position = "none", aspect.ratio = 0.65) +
      compact_theme,

    stacked_composition = ggplot(stacked_data, aes(condition, value, fill = source)) +
      geom_col(color = "black", linewidth = 0.15) +
      scale_fill_manual(values = unname(okabe_ito[c("sky_blue", "orange", "bluish_green", "reddish_purple")])) +
      labs(title = "Stacked composition", x = "Condition", y = "Count", fill = "Source") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "bottom", aspect.ratio = 0.5) +
      compact_theme,

    faceted_time_series = ggplot(economics_long, aes(date, value)) +
      geom_line(color = okabe_ito["blue"], linewidth = 0.3) +
      facet_wrap(~series, ncol = 2, scales = "free_y") +
      labs(title = "Small-multiple time series", x = "Date", y = "Scaled value") +
      compact_theme,

    residual_diagnostic = ggplot(residual_data, aes(fitted, residual, color = cyl)) +
      geom_hline(yintercept = 0, color = "grey70", linewidth = 0.25) +
      geom_point(size = 1.0, alpha = 0.78) +
      scale_color_manual(values = unname(okabe_ito[c("sky_blue", "orange", "reddish_purple")])) +
      labs(title = "Residual diagnostic", x = "Fitted value", y = "Residual", color = "Cylinders") +
      theme(legend.position = "none", aspect.ratio = 1) +
      compact_theme,

    correlation_matrix = ggplot(corr_data, aes(metric_x, metric_y, fill = correlation)) +
      geom_tile(color = "white", linewidth = 0.25) +
      geom_text(aes(label = correlation), size = 2.0) +
      scale_fill_gradient2(low = okabe_ito["blue"], mid = "white", high = okabe_ito["vermillion"], limits = c(-1, 1)) +
      labs(title = "Correlation matrix", x = NULL, y = NULL, fill = "r") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right", aspect.ratio = 1) +
      compact_theme
  )
}

demo_scenarios <- function() {
  list(
    original = list(
      label = "Original feedback",
      prefix = "original_feedback",
      builder = make_original_feedback_plots,
      max_grid_cols = 20,
      max_grid_rows = 6,
      search_budget = 6,
      return_candidates = 6,
      max_pages = 3
    ),
    generalization = list(
      label = "Generalization feedback",
      prefix = "generalization_feedback",
      builder = make_generalization_feedback_plots,
      max_grid_cols = 18,
      max_grid_rows = 7,
      search_budget = 8,
      return_candidates = 6,
      max_pages = 3
    ),
    stress = list(
      label = "Real-world stress",
      prefix = "real_world_stress",
      builder = make_real_world_stress_plots,
      max_grid_cols = 20,
      max_grid_rows = 8,
      search_budget = 10,
      return_candidates = 6,
      max_pages = 4
    )
  )
}

write_readme_comparison_image <- function(
    plots,
    optimized_result,
    project_dir,
    page_width_in,
    page_height_in,
    text_size) {

  if (is.null(optimized_result$pages) ||
      length(optimized_result$pages) == 0 ||
      !identical(optimized_result$pages[[1]]$engine, "grid") ||
      is.null(optimized_result$pages[[1]]$grob)) {
    return(NA_character_)
  }

  figures_dir <- file.path(project_dir, "man", "figures")
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(figures_dir, "readme-layout-comparison.png")

  baseline <- patchwork::wrap_plots(
    plots,
    ncol = 2,
    guides = "keep",
    axes = "keep",
    axis_titles = "keep"
  )

  header_height_in <- 0.55
  grDevices::png(
    filename = output_file,
    width = page_width_in * 2,
    height = page_height_in + header_height_in,
    units = "in",
    res = 120,
    bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  grid::grid.text(
    "Before: equal patchwork grid",
    x = grid::unit(page_width_in / 2, "in"),
    y = grid::unit(page_height_in + 0.32, "in"),
    gp = grid::gpar(fontsize = text_size + 3, fontface = "bold")
  )
  grid::grid.text(
    "After: optimized physical layout",
    x = grid::unit(page_width_in * 1.5, "in"),
    y = grid::unit(page_height_in + 0.32, "in"),
    gp = grid::gpar(fontsize = text_size + 3, fontface = "bold")
  )

  grid::pushViewport(grid::viewport(
    x = grid::unit(0, "in"),
    y = grid::unit(0, "in"),
    width = grid::unit(page_width_in, "in"),
    height = grid::unit(page_height_in, "in"),
    just = c("left", "bottom"),
    clip = "on"
  ))
  grid::grid.draw(patchwork::patchworkGrob(baseline))
  grid::popViewport()

  grid::pushViewport(grid::viewport(
    x = grid::unit(page_width_in, "in"),
    y = grid::unit(0, "in"),
    width = grid::unit(page_width_in, "in"),
    height = grid::unit(page_height_in, "in"),
    just = c("left", "bottom"),
    clip = "on"
  ))
  grid::grid.draw(optimized_result$pages[[1]]$grob)
  grid::popViewport()

  message("README comparison image written to: ", output_file)
  normalizePath(output_file, winslash = "/", mustWork = FALSE)
}

normalize_requested_scenarios <- function(scenarios) {
  available <- names(demo_scenarios())
  if (is.null(scenarios) || length(scenarios) == 0 || identical(scenarios, "all")) {
    return(available)
  }
  scenarios <- unlist(strsplit(paste(scenarios, collapse = ","), ",", fixed = TRUE))
  scenarios <- trimws(scenarios)
  unknown <- setdiff(scenarios, available)
  if (length(unknown) > 0) {
    stop("Unknown demo scenario(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  scenarios
}

run_demo <- function(
    scenarios = "all",
    project_dir = canonical_project_dir,
    output_dir = file.path(project_dir, "demo_output"),
    layout_engine = "grid",
    diagnostics = FALSE,
    text_size = 7,
    page_width_in = 8.27,
    page_height_in = 11.69,
    verbose = TRUE) {

  library(ggplot2)
  source_optimizer(project_dir)
  set_demo_theme(text_size)

  scenario_specs <- demo_scenarios()
  scenario_names <- normalize_requested_scenarios(scenarios)
  rows <- vector("list", length(scenario_names))
  results <- vector("list", length(scenario_names))
  plot_sets <- vector("list", length(scenario_names))
  names(results) <- scenario_names
  names(plot_sets) <- scenario_names

  for (scenario_index in seq_along(scenario_names)) {
    scenario_name <- scenario_names[scenario_index]
    spec <- scenario_specs[[scenario_name]]
    message("Running demo scenario: ", spec$label)

    plots <- spec$builder()
    list2env(plots, envir = globalenv())
    plot_sets[[scenario_name]] <- plots

    layout_result <- suggest_patchwork_layout(
      plots = plots,
      page_width_in = page_width_in,
      page_height_in = page_height_in,
      base_size = text_size,
      base_family = "Helvetica",
      min_label_gap_mm = 1.0,
      min_panel_width_mm = 18,
      min_panel_height_mm = 10,
      allow_multipage = TRUE,
      max_pages = spec$max_pages,
      multipage_penalty = 500,
      max_grid_cols = spec$max_grid_cols,
      max_grid_rows = spec$max_grid_rows,
      search_budget = spec$search_budget,
      return_candidates = spec$return_candidates,
      output_style = "design",
      layout_engine = layout_engine,
      verbose = verbose
    )

    rows[[scenario_index]] <- write_layout_result(
      layout_result = layout_result,
      output_dir = output_dir,
      prefix = spec$prefix,
      page_width_in = page_width_in,
      page_height_in = page_height_in,
      text_size = text_size,
      label = spec$label,
      diagnostics = diagnostics
    )
    results[[scenario_name]] <- layout_result
  }

  index <- do.call(rbind, rows)
  if (diagnostics) {
    index_filename <- file.path(output_dir, "demo_report_index.tsv")
    utils::write.table(index, index_filename, sep = "\t", quote = FALSE, row.names = FALSE)
    message("Demo report index written to: ", index_filename)
  }

  readme_image <- NA_character_
  if ("generalization" %in% names(results) && identical(layout_engine, "grid")) {
    readme_image <- write_readme_comparison_image(
      plots = plot_sets$generalization,
      optimized_result = results$generalization,
      project_dir = project_dir,
      page_width_in = page_width_in,
      page_height_in = page_height_in,
      text_size = text_size
    )
  }

  invisible(list(index = index, results = results, readme_image = readme_image))
}

parse_demo_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  scenario <- "all"
  output_dir <- NULL
  layout_engine <- "grid"
  diagnostics <- FALSE

  for (arg in args) {
    if (grepl("^--scenario=", arg)) {
      scenario <- sub("^--scenario=", "", arg)
    } else if (grepl("^--output-dir=", arg)) {
      output_dir <- sub("^--output-dir=", "", arg)
    } else if (grepl("^--layout-engine=", arg)) {
      layout_engine <- sub("^--layout-engine=", "", arg)
    } else if (identical(arg, "--diagnostics")) {
      diagnostics <- TRUE
    } else if (arg %in% c("all", names(demo_scenarios()))) {
      scenario <- arg
    }
  }

  list(
    scenario = scenario,
    output_dir = output_dir,
    layout_engine = layout_engine,
    diagnostics = diagnostics
  )
}

is_direct_script_execution <- function() {
  command_script <- script_path_from_command_args()
  if (is.na(command_script)) {
    return(FALSE)
  }
  identical(normalizePath(command_script, winslash = "/", mustWork = FALSE), canonical_demo_script)
}

if (is_direct_script_execution()) {
  parsed_args <- parse_demo_args()
  output_dir <- if (is.null(parsed_args$output_dir)) {
    file.path(canonical_project_dir, "demo_output")
  } else {
    parsed_args$output_dir
  }

  run_demo(
    scenarios = parsed_args$scenario,
    project_dir = canonical_project_dir,
    output_dir = output_dir,
    layout_engine = parsed_args$layout_engine,
    diagnostics = parsed_args$diagnostics
  )
}
