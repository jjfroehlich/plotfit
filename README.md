# plotfit

Automatic layout optimization for multi-plot ggplot PDF reports.

`plotfit` measures ggplot objects on a target-like PDF device,
infers how much physical space each plot needs, searches page layouts, and
returns drawable pages plus diagnostics. It is useful when a report contains a
mix of simple plots, long labels, facets, legends, dense data, and fixed-aspect
plots, and a plain equal grid wastes space or clips readable detail.

![Before and after layout comparison](man/figures/readme-layout-comparison.png)

## What It Does

- Accepts an ordinary named list of `ggplot2` plots.
- Infers plot footprints automatically from grobs, axes, labels, legends,
  facets, aspect constraints, and density diagnostics.
- Chooses one or more pages when multipage output improves readability.
- Returns editable patchwork output and exact grid-based PDF output.
- Keeps the original plots unchanged.

Users do not specify per-plot widths, heights, scale factors, or footprint
metadata. Plot names are identifiers for diagnostics and generated code only;
they must not affect sizing, scoring, page assignment, or inner plot scaling.

## Installation

From a local checkout:

```r
install.packages("remotes")
remotes::install_local("D:/code/r/plotfit")
```

Or, from inside the project directory:

```r
remotes::install_local(".")
```

After the package is published on GitHub, install it with:

```r
remotes::install_github("jjfroehlich/plotfit")
```

For quick development without installing, source the files directly as shown
below.

## Basic Usage

Load the installed package:

```r
library(plotfit)
```

Source the package files during local development:

```r
for (r_file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(r_file)
}
```

Then optimize and draw:

```r
layout_result <- suggest_patchwork_layout(
  plots = list(plot_a = p1, plot_b = p2, plot_c = p3),
  page_width_in = 8.27,
  page_height_in = 11.69,
  layout_engine = "grid"
)

pdf("report.pdf", width = 8.27, height = 11.69)
draw_layout_pages(layout_result)
dev.off()
```

## Patchwork vs Grid Output

`layout_engine = "patchwork"` is the default for backward compatibility. It
returns editable patchwork objects and generated patchwork code:

```r
layout_result <- suggest_patchwork_layout(plots)
layout_result$pages[[1]]$patchwork
cat(layout_result$pages[[1]]$patchwork_code)
```

`layout_engine = "grid"` is recommended for final PDF rendering. It draws plots
inside exact physical viewports using optimized row and column sizes:

```r
layout_result <- suggest_patchwork_layout(plots, layout_engine = "grid")
pdf("optimized-report.pdf", width = 8.27, height = 11.69)
draw_layout_pages(layout_result)
dev.off()
```

Best-effort editable patchwork code remains available for both engines.

## Demo

Run the single demo script:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R
```

Default output:

- `demo_output/original_feedback.pdf`
- `demo_output/generalization_feedback.pdf`
- `demo_output/real_world_stress.pdf`
- `man/figures/readme-layout-comparison.png`

Run one scenario:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R --scenario=generalization
```

Write optional diagnostics, warnings, and generated patchwork code:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R --diagnostics
```

## Development

Run tests:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' -e "testthat::test_local('.', reporter = 'summary')"
```

Key invariants:

- No demo-specific plot IDs, titles, datasets, or positions may influence
  optimizer behavior.
- The demo script must pass ordinary ggplot objects, not plot-specific footprint
  overrides.
- Visual PDF review should use `layout_engine = "grid"`.

## Limitations

Exact visual collision detection is not guaranteed. Text measurements are
device-dependent, PDF viewers and font substitution may shift appearance
slightly, inside legends and in-panel annotations are hard to assess, and dense
data readability is approximate. Facet-heavy reports may still need visual
review.
