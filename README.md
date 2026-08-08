# plotfit

Automatic plot dimension optimization for PDF exports of many ggplots. 

`plotfit` analyzes ggplot objects on a PDF device, and returns a suggested layout for the 'patchwork' package. The aim is, that individual plots will have optimal dimensions and sizing. From here one can then manually assemble them in a vector graphic editor into publication figures. The overall arrangement of plots and the spacing between them is therefore not optimized.

![Before and after layout comparison](man/figures/readme-layout-comparison.png)

## What It Does

- Accepts a list of `ggplot2` plots.
- Infers plot footprints automatically from axes, labels, legends, facets, and aspect constraints.
- Chooses one or more pages.
- Searches plot arrangements and row and column sizes for sensible physical plot dimensions.
- Returns an editable `patchwork` layout and generated R code.

## Installation

```r
remotes::install_github("jjfroehlich/plotfit")
```

## Basic Usage

Load the installed package:

```r
library(plotfit)
```

Optimize and inspect the suggested patchwork layout:

```r
layout_result <- suggest_patchwork_layout(
  plots = list(plot_a = p1, plot_b = p2, plot_c = p3),
  page_width_in = 8.27,
  page_height_in = 11.69
)

# Main artifacts for review, manual adjustment, and reuse:
layout_result$pages[[1]]$patchwork
cat(layout_result$pages[[1]]$patchwork_code)
```

## Faster and Constrained Searches

Use the fast preset for interactive iteration. Explicit limits still override
the preset defaults:

```r
quick_result <- suggest_patchwork_layout(plots, search_mode = "fast")
quick_result$performance_diagnostics$stages
quick_result$performance_diagnostics$candidates
quick_result$performance_diagnostics$fit_cache
```

By default, `plotfit` decides which plots share a page. Use `page_groups` when
the report's narrative requires a particular page assignment. Each element of
the list defines one page, using the names from the `plots` list:

```r
plots <- list(
  overview = p1,
  trend = p2,
  full_page_map = p3,
  details = p4,
  appendix = p5
)

report_result <- suggest_patchwork_layout(
  plots,
  page_groups = list(
    c("overview", "trend"),
    "full_page_map",
    c("details", "appendix")
  )
)
```

This requests three pages: `overview` with `trend`, `full_page_map` by itself,
and `details` with `appendix`. Every plot name must appear exactly once.
`page_groups` controls only which plots share a page; `plotfit` still determines
their arrangement and physical sizes automatically.

## Patchwork Output

`layout_engine = "patchwork"` is the default because it returns an optimized
layout that remains easy to inspect, copy, paste, and adjust in R.

```r
layout_result <- suggest_patchwork_layout(plots)

# Draw or further customize the editable patchwork object.
layout_result$pages[[1]]$patchwork

# Copy/paste this generated code into your own script as a starting point.
cat(layout_result$pages[[1]]$patchwork_code)
```

## Grid Output

`layout_engine = "grid"` renders the optimizer's physical row, column, and plot
footprint measurements more exactly. It is available for diagnostic comparisons
when you want to distinguish sizing decisions from patchwork rendering behavior.

```r
layout_result <- suggest_patchwork_layout(plots, layout_engine = "grid")
pdf("optimized-preview.pdf", width = 8.27, height = 11.69)
draw_layout_pages(layout_result)
dev.off()
```

## Demo

Run the single demo script:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R
```

Default output:

- `demo_output/layout_feedback.pdf`
- `demo_output/previous/layout_feedback.pdf`
- `man/figures/readme-layout-comparison.png`

The unified feedback PDF numbers every plot globally in its title (`p1`, `p2`,
and so on). Before a canonical all-scenarios run replaces the PDF, the existing
version is copied to `demo_output/previous/` for side-by-side review. The demo
uses patchwork so the feedback PDF exercises the same editable output that the
package API returns by default. Grid rendering remains available through
`--layout-engine=grid` for diagnostic comparisons.

Run one scenario:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R --scenario=generalization
```

This writes `demo_output/layout_feedback_generalization.pdf`. Focused scenario
PDFs keep the same global plot identifiers as the unified PDF (here `p9` to
`p16`) and do not rotate the canonical current/previous comparison pair.

Write optional diagnostics, warnings, and generated patchwork code:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R --diagnostics
```

## Development

Run tests:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' -e "testthat::test_local('.', reporter = 'summary')"
```

## Give Feedback and Contribute

- Please get in touch if you have feedback or want to contribute.
