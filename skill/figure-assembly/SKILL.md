---
name: figure-assembly
description: Assemble paper figures from R plots with the figure-assembly toolkit (figure_assembly/ folder, panel_specs.tsv, Inkscape working canvases). Use when the user asks to set up figure assembly in a project, build or update a figure ("build Figure 3", "update Figure 3"), translate box notes into plot settings, propose a first layout, check a figure against journal rules, switch journal, or export the journal PDF.
---

# Figure assembly (appearance only)

Read `PROTOCOL.md` in the toolkit repository for the full workflow. This file tells Claude how to take part.

## Non-negotiable boundary

This skill changes **appearance only**. Permitted: plot size and position, font sizes and family, axis line and tick widths, layer line widths, fixed point size, fixed transparency, legend position or removal, panel letters, and annotation text style.

Refuse, and say the change belongs in the analysis code (handled elsewhere): filtering or subsetting, thresholds, statistics settings (bins, smoothing, bandwidth, clustering), scales and axis limits, zooming, axis transformations, changing which variable is mapped to colour, size or position, reordering that implies ranking, statistical annotations, outlier removal, and colour changes that alter what a colour encodes.

Never edit analysis lines in the figures document. The only edits to it are the one-time setup chunk, placeholder insertion (`+ figure_style()`, `figure_pheatmap()`, `figure_euler_plot()`, `figure_upset()`), and new placeholders attached only to appearance arguments.

## Never do arithmetic on SVG transforms by hand

Use the toolkit (`read_canvas_boxes()`, `read_canvas_plot_positions()`, `compare_canvas_with_tables()`, `measure_text_sizes()`) or Inkscape's `--query-all`, which reports px at 96 per inch; set `LANG=en_US.UTF-8`, or its output is cut off. Check every result with `render_canvas_preview()` and look at the PNG. Previews go to a temporary file or Claude's scratch folder, never into the project: the SVG is the user's preview.

## "Build / update Figure N"

1. Load the toolkit from `figure_assembly/R/`, with the project folder as the working directory.
2. If a working canvas exists, run `compare_canvas_with_tables()` on the latest one and report the differences in plain language.
3. Setting lines with known names (see `box_note_settings` in `R/assembly_canvas.R`) are already exact table differences; report any marked "NOT applied" back to the user. Translate only the free-text requests (field `notes`) into values of existing `panel_specs.tsv` columns, for example:
   - "axis text 5 pt" → `axis_text_pt = 5`
   - "no legend" → `legend_position = none`
   - "smaller dots" → a lower `point_size_ggplot` (state the value chosen)
   - "thinner lines" → `line_width_pt`; "axis lines 0.5 pt" → `axis_line_pt`
   - heatmap text → `heatmap_font_pt` (row and column variants exist)
   - Euler labels or numbers → `euler_label_pt` / `euler_quantity_pt`

   - "legend as annotation", "I'll place the labels myself" → `move_to_annotations` (`legend`, `strip labels`; `set names` for Euler). Never move values computed from data (counts, statistics): they would stop updating.
   - renaming or formatting facet labels or set names (italics, superscripts) → move them to annotations; the user retypes them in Inkscape. Do not rename data values.
   - bold/plain axis text → `axis_text_face`; dots mapped to data too big → `mapped_size_factor`.

   If a request has no column, say so and propose a new appearance-only placeholder; never touch analysis arguments. If a note is ambiguous, or names a plot that matches several chunk labels, ask.
4. Show the proposed table changes and the annotations to be copied before writing, unless the project's rules say otherwise. Then write them with `update_tables_from_canvas()`, `write_panel_specs()`, `write_figure_layout()` and `extract_annotations_from_canvas()`.
5. The user renders the figures document. Claude renders only when the project's rules allow it or the user asks.
6. Build the next round with `build_figure_canvas("FigureN")`, render a preview, and run `check_figure_svg()` with the project's journal preset. Report the effective font sizes per layer, any issues, and annotations that may need a nudge because a plot was redrawn.

## Round 0 (first layout)

Read the figure chunks and group them by figure (label pattern or table). Choose sizes by plot type and content: near-square UMAPs and Euler diagrams, heatmaps from matrix rows and columns, forest plots wide and short, dot and bar plots from the number of categories and label length. Stay within the preset's page width and maximum height. Line widths are enforced and unified across all figures: apply `apply_project_line_widths(panel_specs, project_line_width_pt(project_settings, journal_preset))` to every round-0 row, and state the value in the chat. Place panels with `layout_panels_in_rows()`, set `plot_kind` for every row (ggplot, pheatmap, euler, upset, external), write the first table rows, and explain the choices in the chat. Never write explanations into the `notes` column: box notes hold only the user's instructions.

Wording changes (`title text`, `y axis title text`, …): warn, and do not apply without confirmation, when the current wording contains values computed from the data (test statistics, counts, percentages).

## Final figure

Never build the journal figure while the latest canvas and the tables disagree: report the differences and let the user decide. Then run `build_figure_canvas("FigureN", "journal")`, `export_figure_pdf()`, `check_figure_svg()` and `check_pdf_fonts()`.

## Files

- Never overwrite a user's canvas; builds always write a new round.
- Never hand-edit generated SVGs; regenerate them.
- `drafting/` is disposable; `specs/`, `annotations/` and the figures document are the record.
