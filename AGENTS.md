# AGENTS.md

## Collaboration Style

The user often gives iterative visual feedback using demo plot numbers, for
example "plot 6 can be half as tall" or "plot 8 can be smaller in both
dimensions." Treat those numbers as references to the current rendered output,
not identifiers that may be hard-coded.

When receiving visual feedback:

1. Inspect the current rendered PDF or PNG first when possible.
2. Interpret size comments as relative to the current rendered size unless the
   user explicitly says otherwise.
3. Translate the feedback into automatic rules based on measurable plot
   properties: ggplot grobs, fixed non-panel burden, effective panel size,
   selected physical allocation, hard violations, facets, labels, legends,
   explicit aspect constraints, rotated axis text, and density diagnostics.
4. Do not add branches keyed to demo plot names, plot positions, dataset names,
   titles, or numbered examples.
5. Keep demo scripts free of per-plot footprint inputs. Numeric widths,
   heights, or scale factors may appear in generated patchwork code only as
   optimizer output.
6. Add or update tests for the generalized rule being changed.
7. Regenerate the demo PDF and render a preview after layout changes so the
   result can be visually inspected.

## Product Rules

This project is an automatic ggplot layout optimizer. Users should not need to
provide per-plot dimensions, footprint metadata, manual scale factors, or
plot-specific layout instructions.

Plot names are identifiers only. They may label diagnostics, generated code,
and output objects, but they must not influence sizing, scoring, page
assignment, or inner plot scaling.

The tool is layout-only. It should not rotate or wrap labels, move legends,
paginate facets, or edit themes unless an explicit public option already asks
for that behavior.

Simple plots should use only as much page space as needed for readable axes,
legends, facets, labels, and data. Dense, label-heavy, legend-heavy, or
facet-heavy plots should receive more space or move to another page through the
normal automatic scoring path.

Use continuous physical measurements in millimetres in the optimization core.
Human-readable labels such as compact, wide, or dense can be diagnostics, but
they should not replace physical fit loss and measured constraints.

Prefer public ggplot2, grid, gtable, and patchwork APIs. Avoid package
internals unless there is no viable public alternative and the reason is
documented in code.

## Repo Shape

Keep one package, one demo script, and one README:

- `R/api.R`
- `R/measurement.R`
- `R/fit.R`
- `R/layout-search.R`
- `R/output.R`
- `scripts/demo.R`
- `demo_output/*.pdf`
- `man/figures/readme-layout-comparison.png`

Do not restore wrapper demo scripts, old implementation plans, transient
diagnostics, or `tmp/` previews as tracked project files.

## Demo Workflow

Use `scripts/demo.R` as the only demo entry point. It should build the original
feedback plots, the generalization feedback plots, and stress-test plots from
ordinary ggplot objects.

Default demo output should be only:

- `demo_output/original_feedback.pdf`
- `demo_output/generalization_feedback.pdf`
- `demo_output/real_world_stress.pdf`
- `man/figures/readme-layout-comparison.png`

Use `--diagnostics` only when TSV diagnostics, warnings, or generated
patchwork code are intentionally needed.

For visual PDF review, prefer `layout_engine = "grid"`. Patchwork output stays
the backward-compatible default API engine and is useful for editable code, but
grid output is the high-fidelity physical-sizing target.

## Verification Commands

Run the test suite with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' -e "testthat::test_local('.', reporter = 'summary')"
```

Run the demo with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R
```
