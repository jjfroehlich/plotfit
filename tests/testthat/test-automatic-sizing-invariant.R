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
  expect_match(runner_source, "make_extreme_feedback_plots", fixed = TRUE)
  expect_match(runner_source, "label_feedback_plots", fixed = TRUE)
  expect_match(runner_source, "layout_feedback.pdf", fixed = TRUE)
  expect_match(runner_source, "archive_previous = canonical_run", fixed = TRUE)
  expect_match(runner_source, "--diagnostics", fixed = TRUE)
  expect_false(grepl("--scenario=", runner_source, fixed = TRUE))
  expect_match(runner_source, "readme-layout-comparison.png", fixed = TRUE)
})

test_that("feedback titles are globally numbered and prior PDFs are preserved", {
  runner_file <- file.path("scripts", "demo.R")
  if (!file.exists(runner_file)) {
    runner_file <- file.path("..", "..", "scripts", "demo.R")
  }
  skip_if_not(file.exists(runner_file))

  demo_environment <- new.env(parent = globalenv())
  sys.source(runner_file, envir = demo_environment)

  plots <- list(
    first_name = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "First"),
    second_name = ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Second")
  )
  labelled <- demo_environment$label_feedback_plots(plots, first_plot_number = 9)

  expect_equal(names(labelled), c("p9", "p10"))
  expect_equal(labelled[[1]]$labels$title, "(p9) First")
  expect_equal(labelled[[2]]$labels$title, "(p10) Second")
  expect_error(
    demo_environment$parse_demo_args("--scenario=generalization"),
    "Unsupported demo option"
  )

  output_dir <- tempfile("plotfit-feedback-")
  dir.create(output_dir, recursive = TRUE)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  current_pdf <- file.path(output_dir, "layout_feedback.pdf")
  baseline_bytes <- charToRaw("reviewed baseline")
  writeBin(baseline_bytes, current_pdf)

  demo_environment$draw_layout_pages <- function(result) {
    grid::grid.newpage()
    grid::grid.draw(grid::rectGrob())
  }
  demo_environment$write_feedback_pdf(
    layout_results = list(list(pages = list())),
    output_dir = output_dir,
    filename = "layout_feedback.pdf",
    page_width_in = 2,
    page_height_in = 2,
    text_size = 7,
    archive_previous = TRUE
  )

  previous_pdf <- file.path(output_dir, "previous", "layout_feedback.pdf")
  expect_true(file.exists(previous_pdf))
  expect_equal(readBin(previous_pdf, what = "raw", n = length(baseline_bytes)), baseline_bytes)
})


test_that("demo defaults to patchwork review while preserving an engine override", {
  runner_file <- file.path("scripts", "demo.R")
  if (!file.exists(runner_file)) {
    runner_file <- file.path("..", "..", "scripts", "demo.R")
  }
  skip_if_not(file.exists(runner_file))

  runner_source <- paste(readLines(runner_file, warn = FALSE), collapse = "\n")

  expect_match(runner_source, "run_demo <- function", fixed = TRUE)
  expect_match(runner_source, "layout_engine = \"patchwork\"", fixed = TRUE)
  expect_match(runner_source, "layout_engine <- \"patchwork\"", fixed = TRUE)
  expect_match(runner_source, "--layout-engine=", fixed = TRUE)
  expect_match(runner_source, "patchwork::patchworkGrob(optimized_page$patchwork)", fixed = TRUE)
})

test_that("optimizer source contains no demo-specific plot sizing branches", {
  source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  if (length(source_files) == 0) {
    source_files <- list.files(file.path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  }
  skip_if(length(source_files) == 0, "repository source files are not available")
  source_text <- paste(unlist(lapply(source_files, readLines, warn = FALSE)), collapse = "\n")

  runner_file <- file.path("scripts", "demo.R")
  if (!file.exists(runner_file)) {
    runner_file <- file.path("..", "..", "scripts", "demo.R")
  }
  skip_if_not(file.exists(runner_file))
  demo_environment <- new.env(parent = asNamespace("ggplot2"))
  sys.source(runner_file, envir = demo_environment)
  scenario_specs <- demo_environment$demo_scenarios()
  demo_plot_sets <- lapply(scenario_specs, function(spec) spec$builder())
  expect_equal(
    demo_environment$feedback_scenario_start_numbers(demo_plot_sets),
    c(original = 1L, generalization = 9L, stress = 17L, extremes = 25L)
  )
  forbidden_demo_tokens <- unique(c(
    unlist(lapply(demo_plot_sets, names), use.names = FALSE),
    unlist(lapply(demo_plot_sets, function(plots) {
      vapply(plots, function(plot) plot$labels$title, character(1))
    }), use.names = FALSE)
  ))

  matches <- forbidden_demo_tokens[vapply(
    forbidden_demo_tokens,
    function(token) grepl(token, source_text, fixed = TRUE),
    logical(1)
  )]

  expect_equal(matches, character())
})

test_that("active sizing code contains no geom-category branches", {
  source_paths <- file.path("R", c("fit.R", "output.R"))
  if (!all(file.exists(source_paths))) {
    source_paths <- file.path("..", "..", source_paths)
  }
  source_text <- paste(unlist(lapply(source_paths, readLines, warn = FALSE)), collapse = "\n")
  expect_false(grepl("Geom[A-Z]", source_text))
  expect_false(grepl("geom_classes", source_text, fixed = TRUE))
})

test_that("permanent structural baseline PDF remains immutable", {
  archive_pdf <- file.path("demo_output", "archive", "structural-scaling-baseline.pdf")
  archive_manifest <- file.path("demo_output", "archive", "README.md")
  if (!file.exists(archive_pdf)) {
    archive_pdf <- file.path("..", "..", archive_pdf)
    archive_manifest <- file.path("..", "..", archive_manifest)
  }
  expect_true(file.exists(archive_pdf))
  expect_true(file.exists(archive_manifest))
  expect_equal(
    unname(tools::md5sum(archive_pdf)),
    "523e0ccd60b3afd80aa9709710bca850"
  )
  manifest <- paste(readLines(archive_manifest, warn = FALSE), collapse = "\n")
  expect_match(manifest, "baseline-structural-scaling-2026-08-08", fixed = TRUE)
  expect_match(manifest, "59A4FE2598C5340473BBF1D0FEFDC2E3AB287B8CE2B87A40D415CF3FF80D7D63", fixed = TRUE)
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

  scales_a <- plotfit:::infer_inner_plot_scales(plots_a, diagnostics_a)
  scales_b <- plotfit:::infer_inner_plot_scales(plots_b, diagnostics_b)

  fixed_a <- scales_a[scales_a$plot_id == "p1_iris_scatter", c("scale_x", "scale_y")]
  fixed_b <- scales_b[scales_b$plot_id == "renamed_first", c("scale_x", "scale_y")]
  low_a <- scales_a[scales_a$plot_id == "p2_mtcars_box_jitter", c("scale_x", "scale_y")]
  low_b <- scales_b[scales_b$plot_id == "renamed_second", c("scale_x", "scale_y")]

  expect_equal(as.numeric(fixed_a[1, ]), as.numeric(fixed_b[1, ]))
  expect_equal(as.numeric(low_a[1, ]), as.numeric(low_b[1, ]))
})
