# AGENTS.md

## Collaboration Style

The user often gives iterative visual feedback using the globally unique
`(pN)` identifiers printed in plot titles, for example "p6 can be half as
tall" or "p8 can be smaller in both dimensions." Treat those identifiers as
references to the current rendered output, not values that may be hard-coded.

Size feedback is approximate and directional. Interpret a comment such as
"p1 1/2 width, 2/3 height" as asking for p1's next visible rendered footprint
to be roughly one-half of its current rendered width and roughly two-thirds of
its current rendered height. Apply the two multipliers independently to the
current whole visible plot footprint (panel, axes, labels, title, and legend),
not to the page, grid cell, data range, or an absolute millimetre size. A
percentage such as 150% means about 1.5 times the current dimension. If only
one dimension is mentioned, leave the other approximately unchanged. These
ratios describe a target neighbourhood, not exact constraints or permission
for per-plot overrides; preserve readability and translate the direction into
general measurable rules.

When receiving visual feedback:

1. Compare the current and immediately previous rendered PDFs first when
   possible.
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
7. Regenerate the unified demo PDF and render every page after layout changes
   so the result can be visually inspected.

## Product Rules

This project is an automatic ggplot layout optimizer. Users should not need to
provide per-plot dimensions, footprint metadata, or manual scale factors.
Explicit narrative page membership through the public `page_groups` option is
allowed; it must never alter automatic physical sizing.

Plot names are identifiers only during automatic search. They may label
diagnostics, generated code, and output objects, and may be referenced by an
explicit `page_groups` constraint, but must not influence sizing, scoring, or
inner plot scaling.

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

`plotfit` should remain an editable patchwork layout suggestor. The optimizer
should produce patchwork layouts and generated patchwork code that users can
copy into their own scripts and manually adjust afterward. Grid rendering may
remain available for visual validation or exact previewing, but it should not
displace editable patchwork output as the primary user-facing suggestion
workflow.

## Repo Shape

Keep one package, one demo script, and one README:

- `R/api.R`
- `R/measurement.R`
- `R/fit.R`
- `R/layout-search.R`
- `R/output.R`
- `scripts/demo.R`
- `demo_output/layout_feedback.pdf`
- `demo_output/previous/layout_feedback.pdf`
- `man/figures/readme-layout-comparison.png`

Do not restore wrapper demo scripts, old implementation plans, transient
diagnostics, or `tmp/` previews as tracked project files.

## Demo Workflow

Use `scripts/demo.R` as the only demo entry point. It should build the original
feedback plots, the generalization feedback plots, stress-test plots, and an
additional set of extreme plots from ordinary ggplot objects. All plots share
one globally unique `(pN)` sequence in their visible titles so feedback remains
unambiguous across pages.

Default demo output should be only:

- `demo_output/layout_feedback.pdf`
- `demo_output/previous/layout_feedback.pdf`
- `man/figures/readme-layout-comparison.png`

A canonical all-scenarios run must copy the existing feedback PDF to the
`previous/` path immediately before replacing it. Scenario-only and temporary
runs must use a separate PDF name or output directory, retain the canonical
global `(pN)` identifiers, and must not rotate the canonical comparison pair.

Use `--diagnostics` only when TSV diagnostics, warnings, or generated
patchwork code are intentionally needed.

For visual PDF review and the default demo, use `layout_engine = "grid"`.
Patchwork output stays the backward-compatible default package API engine and
is useful for editable code, but grid output is the high-fidelity
physical-sizing target.

## Verification Commands

Run the test suite with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' -e "testthat::test_local('.', reporter = 'summary')"
```

Run the demo with:

```powershell
& 'C:\Program Files\R\R-4.2.2\bin\Rscript.exe' scripts\demo.R
```
