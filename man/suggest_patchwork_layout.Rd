\name{suggest_patchwork_layout}
\alias{suggest_patchwork_layout}
\alias{draw_layout_pages}
\title{Suggest readable ggplot layouts}
\usage{
suggest_patchwork_layout(
  plots,
  plot_names = names(plots),
  page_width_in = 8.27,
  page_height_in = 11.69,
  page_margin_mm = 0,
  base_size = 7,
  base_family = "Helvetica",
  min_label_gap_mm = 1.5,
  target_label_gap_mm = NULL,
  min_panel_width_mm = NULL,
  min_panel_height_mm = NULL,
  min_panel_area_mm2 = NULL,
  max_grid_cols = 6,
  max_grid_rows = 8,
  allow_multipage = TRUE,
  max_pages = Inf,
  multipage_penalty = 25,
  keep_plot_order = TRUE,
  allow_empty_cells = TRUE,
  collect_guides = FALSE,
  collect_axes = FALSE,
  device = c("pdf", "cairo_pdf"),
  use_check_overlap_oracle = TRUE,
  search_budget = 500,
  return_candidates = 5,
  output_style = c("design", "nested_patchwork"),
  layout_engine = c("patchwork", "grid"),
  validation_level = c("layout", "render"),
  max_empty_fraction = 0.25,
  verbose = TRUE
)

draw_layout_pages(result)
}
\arguments{
\item{plots}{A non-empty named list of \code{ggplot2} plot objects.}
\item{plot_names}{Optional plot names. Missing names are replaced with stable \code{p1}, \code{p2}, ... names.}
\item{page_width_in, page_height_in}{Output page size in inches. Defaults to A4 portrait.}
\item{page_margin_mm}{Margin removed from the available layout area on each side, in millimetres.}
\item{base_size, base_family}{Text size and font family used for target-like PDF measurement.}
\item{min_label_gap_mm}{Minimum estimated gap between neighbouring axis labels.}
\item{target_label_gap_mm}{Soft target label gap. If \code{NULL}, uses \code{1.5 * min_label_gap_mm}.}
\item{min_panel_width_mm, min_panel_height_mm, min_panel_area_mm2}{Lower panel-size limits. If omitted, defaults are derived from \code{base_size}.}
\item{max_grid_cols, max_grid_rows}{Maximum columns and rows for generated patchwork design strings.}
\item{allow_multipage}{Whether the optimizer may split plots across multiple pages. Defaults to \code{TRUE}.}
\item{max_pages}{Maximum number of pages to consider.}
\item{multipage_penalty}{Penalty added for each additional page.}
\item{keep_plot_order}{Whether to prioritize order-preserving layouts. Version 1 still prioritizes ordered candidates.}
\item{allow_empty_cells}{Whether empty \code{#} cells may appear in design strings. Version 1 may use them for editable rectangular designs.}
\item{collect_guides, collect_axes}{Whether to ask patchwork to collect guides or axes. Defaults keep guides and axes unchanged.}
\item{device}{Measurement device, either \code{"pdf"} or \code{"cairo_pdf"}.}
\item{use_check_overlap_oracle}{Reserved for future diagnostics. Version 1 uses explicit grid measurements instead.}
\item{search_budget}{Maximum generated layout combinations scored per page assignment.}
\item{return_candidates}{Number of top scored candidates to retain in the result.}
\item{output_style}{Code style for editable output. \code{"design"} is the canonical reproducible style.}
\item{layout_engine}{Output engine. \code{"patchwork"} returns editable patchwork pages by default for compatibility; \code{"grid"} returns exact grid grobs using physical row and column sizes and is recommended for final PDF rendering.}
\item{validation_level}{Final validation level. \code{"layout"} reuses selected physical diagnostics; \code{"render"} draws selected pages on a temporary PDF-like device.}
\item{max_empty_fraction}{Preferred maximum fraction of empty layout cells before soft penalties and warnings are added.}
\item{verbose}{Whether to print progress messages.}
\item{result}{A result object returned by \code{suggest_patchwork_layout()}, or a list of page objects.}
}
\value{
A list with drawable \code{pages}, \code{plot_diagnostics}, \code{layout_diagnostics}, retained \code{candidates}, \code{warnings}, and measurement \code{assumptions}. Each page includes physical \code{col_widths_mm}, \code{row_heights_mm}, content dimensions, inferred \code{inner_scales}, best-effort editable \code{patchwork_code}, and either a \code{patchwork} object or a \code{grob}, depending on \code{layout_engine}.
}
\description{
Optimizes layouts for ordinary lists of \code{ggplot2} plots. The function measures plots on a target-like PDF device, infers how much physical space each plot needs, searches page layouts, optimizes row and column sizes, and optionally chooses a multipage solution.
}
\details{
Use this helper when a plain equal grid wastes page space or gives too little room to plots with long labels, legends, facets, dense data, or explicit aspect constraints.

The optimizer keeps the input plots unchanged. It does not rotate labels, wrap labels, resize the PDF page, rewrite facets, move legends, paginate facets, or collect guides and axes unless explicitly requested.

Automatic sizing rule: plot footprints must be inferred from the supplied ggplot objects and their measured grobs. Users should not provide per-plot widths, heights, scale factors, or footprint metadata. \code{plot_names} are identifiers only; they label diagnostics and generated code but must not affect sizing, scoring, page assignment, or inner plot scaling. Numeric widths, heights, and scale factors that appear in generated patchwork code are optimizer output, not user input.

Use \code{layout_engine = "grid"} for final visual PDF review when exact physical plot footprints matter. Use \code{layout_engine = "patchwork"} when editable patchwork objects are the primary deliverable. Patchwork code is still returned for both engines.

Limitations: exact visual collision detection is not guaranteed; text measurements are device-dependent; PDF viewers and font substitution may change appearance slightly; inside legends and in-panel annotations are hard to assess; dense data readability is approximated; facet-heavy plots may still need manual review; final PDF visual inspection is recommended.
}
\examples{
\dontrun{
plots <- list(p1 = p1, p2 = p2, p3 = p3)

layout_result <- suggest_patchwork_layout(plots, layout_engine = "grid")

pdf("report.pdf", width = 8.27, height = 11.69)
draw_layout_pages(layout_result)
dev.off()

cat(layout_result$pages[[1]]$patchwork_code)
}
}
