# plotfit

Automatic plot dimension optimization for PDF exports of many ggplots. 

`plotfit` analyzes ggplot objects on a PDF device, to return a suggested layout for the 'patchwork' package, 'patchwork::plot_layout()'. The goal is for individual plots to have optimal dimensions and sizing. The space between plots is not optimized. One can then manually assemble the plots into publication figures in a vector graphics editor like Illustrator or Inkscape.

![Before and after layout comparison](man/figures/readme-layout-comparison.png)

## What It Does

- Accepts a list of `ggplot2` plots.
- Infers plot footprints automatically from axes, labels, legends, facets, and aspect constraints.
- Chooses one or more pages.
- Searches plot arrangements, row and column sizes for sensible physical plot dimensions.
- Returns an editable `patchwork` layout and R code.

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
  plots = list(p1, p2, p3, p4, p5, p6),
  page_width_in = 8.27,
  page_height_in = 11.69
)

# Main artifacts for review, manual adjustment, and reuse:
layout_result$pages[[1]]$patchwork
cat(layout_result$pages[[1]]$patchwork_code)
```

## Patchwork Output

`layout_engine = "patchwork"` is the default output. It returns an optimized
layout that remains easy to inspect, copy, paste, and adjust in R.

```r
layout_result <- suggest_patchwork_layout(plots)

# Draw or further customize the editable patchwork object.
layout_result$pages[[1]]$patchwork

# Copy/paste this generated code into your own script as a starting point.
cat(layout_result$pages[[1]]$patchwork_code)
```

## Constrained Groups
By default, `plotfit` decides which plots share a page. Use `page_groups` when
the report's narrative requires a particular page assignment. Each element of
the list defines one page, using the names from the `plots` list:

```r
plots <- list(
  p1 = p1,
  p2 = p2,
  p3 = p3,
  p4 = p4,
  p5 = p5,
  p6 = p6,
  p7 = p7,
  p8 = p8, 
  p9 = p9, 
  p10 = p10
)

report_result <- suggest_patchwork_layout(
  plots,
  page_groups = list(
    group_1 = c("p1", "p2", "p3", "p4", "p5", "p6"),
    group_2 = "p7",
    group_3 = c("p8", "p9", "p10")
  )
)
```

This requests three pages. The group names (`group_1`, `group_2`, and
`group_3`) are illustrative only; each group's values are the ggplot object
names from `plots`. Every plot name must appear exactly once. `page_groups`
controls only which plots share a page; `plotfit` still determines their
arrangement and physical sizes automatically.

## Faster Search 

Use the fast preset for interactive iteration. Explicit limits will still override
the preset defaults:

```r
quick_result <- suggest_patchwork_layout(plots, search_mode = "fast")
quick_result$performance_diagnostics$stages
quick_result$performance_diagnostics$candidates
quick_result$performance_diagnostics$fit_cache
```

## Grid Output

Alternative diagnostic option:
`layout_engine = "grid"` renders the optimizer's physical row, column, and plot
footprint measurements more exactly. It is mainly available for diagnostic comparisons
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
- `demo_output/archive/structural-scaling-baseline.pdf`
- `man/figures/readme-layout-comparison.png`

The unified feedback PDF numbers every plot globally in its title (`p1`, `p2`,
and so on). The previously existing PDF version is copied to
`demo_output/previous/` for side-by-side review.

Write optional diagnostics, warnings, and generated patchwork code:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R --diagnostics
```

## Development

Run tests:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' -e "testthat::test_local('.', reporter = 'summary')"
```

## Support

Get in touch if you have feedback or want to contribute.
