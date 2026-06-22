test_that("demo scripts do not hard-code optimizer plot scale factors", {
  scripts_dir <- "scripts"
  if (!dir.exists(scripts_dir)) {
    scripts_dir <- file.path("..", "..", "scripts")
  }
  skip_if_not(dir.exists(scripts_dir))

  demo_files <- list.files(scripts_dir, pattern = "\\.R$", full.names = TRUE)
  expect_equal(basename(demo_files), "demo.R")

  demo_source <- unlist(lapply(demo_files, readLines, warn = FALSE))
  demo_source <- demo_source[!grepl("^\\s*#", demo_source)]

  expect_false(any(grepl("\\bscale_plot\\s*\\(", demo_source)))
  expect_false(any(grepl("\\battach_title\\s*\\(", demo_source)))
  expect_false(any(grepl("\\binner_scales\\b", demo_source)))
  expect_false(any(grepl("\\bscale_[xy]\\s*=", demo_source)))
})

test_that("unified demo script exposes the expected automatic scenarios", {
  runner_file <- file.path("scripts", "demo.R")
  if (!file.exists(runner_file)) {
    runner_file <- file.path("..", "..", "scripts", "demo.R")
  }
  skip_if_not(file.exists(runner_file))

  runner_source <- paste(readLines(runner_file, warn = FALSE), collapse = "\n")

  expect_match(runner_source, "make_original_feedback_plots", fixed = TRUE)
  expect_match(runner_source, "make_generalization_feedback_plots", fixed = TRUE)
  expect_match(runner_source, "make_real_world_stress_plots", fixed = TRUE)
  expect_match(runner_source, "original_feedback", fixed = TRUE)
  expect_match(runner_source, "generalization_feedback", fixed = TRUE)
  expect_match(runner_source, "real_world_stress", fixed = TRUE)
  expect_match(runner_source, "--diagnostics", fixed = TRUE)
  expect_match(runner_source, "readme-layout-comparison.png", fixed = TRUE)
})

test_that("optimizer source contains no demo-specific plot sizing branches", {
  source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  if (length(source_files) == 0) {
    source_files <- list.files(file.path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  }
  expect_gt(length(source_files), 0)
  source_text <- paste(unlist(lapply(source_files, readLines, warn = FALSE)), collapse = "\n")

  forbidden_demo_tokens <- c(
    "p1_iris_scatter",
    "p2_mtcars_box_jitter",
    "p3_diamond_histogram",
    "p4_usarrests_pca",
    "p5_diamond_violin",
    "p6_mpg_faceted_scatter",
    "p7_airquality_box_jitter",
    "p8_mtcars_scatter",
    "iris_scatter",
    "mtcars_box_jitter",
    "diamond_histogram",
    "usarrests_pca",
    "diamond_violin",
    "mpg_faceted_scatter",
    "airquality_box_jitter",
    "mtcars_scatter"
  )

  matches <- forbidden_demo_tokens[vapply(
    forbidden_demo_tokens,
    function(token) grepl(token, source_text, fixed = TRUE),
    logical(1)
  )]

  expect_equal(matches, character())
})

test_that("automatic inner scaling is independent of plot names and order", {
  fixed_text_plot <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(wt, mpg, label = rownames(mtcars))
  ) +
    ggplot2::geom_text(size = 2) +
    ggplot2::theme(aspect.ratio = 1)

  low_aspect_plot <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(factor(cyl), mpg)
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::theme(aspect.ratio = 0.5)

  plots_a <- list(
    p1_iris_scatter = fixed_text_plot,
    p2_mtcars_box_jitter = low_aspect_plot
  )
  diagnostics_a <- data.frame(
    plot_id = names(plots_a),
    hard_loss = 0,
    data_density_loss = 0,
    warning = "",
    stringsAsFactors = FALSE
  )

  plots_b <- list(
    renamed_second = low_aspect_plot,
    renamed_first = fixed_text_plot
  )
  diagnostics_b <- data.frame(
    plot_id = names(plots_b),
    hard_loss = 0,
    data_density_loss = 0,
    warning = "",
    stringsAsFactors = FALSE
  )

  scales_a <- patchworkLayoutOptimizer:::infer_inner_plot_scales(plots_a, diagnostics_a)
  scales_b <- patchworkLayoutOptimizer:::infer_inner_plot_scales(plots_b, diagnostics_b)

  fixed_a <- scales_a[scales_a$plot_id == "p1_iris_scatter", c("scale_x", "scale_y")]
  fixed_b <- scales_b[scales_b$plot_id == "renamed_first", c("scale_x", "scale_y")]
  low_a <- scales_a[scales_a$plot_id == "p2_mtcars_box_jitter", c("scale_x", "scale_y")]
  low_b <- scales_b[scales_b$plot_id == "renamed_second", c("scale_x", "scale_y")]

  expect_equal(as.numeric(fixed_a[1, ]), as.numeric(fixed_b[1, ]))
  expect_equal(as.numeric(low_a[1, ]), as.numeric(low_b[1, ]))
})
