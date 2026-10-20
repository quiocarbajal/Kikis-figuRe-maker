---
name: figure-assembly
description: Assemble paper figures from R plots with the figure-assembly toolkit (figure_assembly/ folder, panel_specs.tsv, Inkscape working canvases). Use when the user asks to set up figure assembly in a project, build or update a figure ("build Figure 3", "update Figure 3"), translate box notes into plot settings, propose a first layout, check a figure against journal rules, switch journal, or export the journal PDF.
---

# Figure assembly (appearance only)

Read `PROTOCOL.md` in the toolkit repository for the full workflow. This file tells Claude how to take part.

Before acting on a request, check `GOTCHAS.md` in the toolkit repository: places where the natural first approach is known to fail.

## Non-negotiable boundary

This skill changes **appearance only**. Permitted: plot size and position, font sizes and family, axis line and tick widths, layer line widths, fixed point size, fixed transparency, legend position or removal, panel letters, and annotation text style.

Refuse, and say the change belongs in the analysis code (handled elsewhere): filtering or subsetting, thresholds, statistics settings (bins, smoothing, bandwidth, clustering), scales and axis limits, zooming, axis transformations, changing which variable is mapped to colour, size or position, reordering that implies ranking, statistical annotations, outlier removal, and colour changes that alter what a colour encodes.

Never edit analysis lines in the figures document or in any other report that loads the toolkit. The only edits to them are the one-time setup chunk, placeholder insertion (`+ figure_style()`, `figure_pheatmap()`, `figure_euler_plot()`, `figure_upset()`), new placeholders attached only to appearance arguments, and the edits listed under "Several source reports" (chunk labels, combining a chunk's plots, removing a report-wide `fig.path`).

## Never do arithmetic on SVG transforms by hand

Use the toolkit (`read_canvas_boxes()`, `read_canvas_plot_positions()`, `compare_canvas_with_tables()`, `measure_text_sizes()`) or Inkscape's `--query-all`, which reports px at 96 per inch; set `LANG=en_US.UTF-8`, or its output is cut off. Check every result with `render_canvas_preview()` and look at the PNG. Previews go to a temporary file or Claude's scratch folder, never into the project: the SVG is the user's preview.

## "Build / update Figure N"

1. Load the toolkit from `figure_assembly/R/`, with the project folder as the working directory.
2. Read the files in `figure_assembly/drafting/project_checks/`, if any: one per report, each the record of that report's last render (mode, time, findings, chunks that drew several plots, canvas comparisons, and why a final render stopped). Findings in older files may be out of date; the newest file and `check_project()` are current. The user rarely reads the render log, so tell them anything there they may not have seen, such as a final render that stopped. Then run `check_project()` (PROTOCOL.md, "Project checks") and report its findings for this figure in plain language, problems first. `duplicate_label`: stop and report both files; the plot file in `drafting/` comes from whichever was rendered last. `multi_plot_chunk`: only `-1.svg` is assembled; propose combining the plots, as under "Several source reports". `stale_plot_files`: propose deleting them. `leftover_row` and `report_fig_path`: as under "Several source reports". Act on a finding (delete files, edit a report or a table) only after the user agrees.
3. If a working canvas exists, run `compare_canvas_with_tables()` on the latest one and report the differences in plain language. Positions and sizes come only from the boxes. If `format_moved_plots()` lists plots moved or resized on the canvas itself, warn the user: those moves are ignored, the next round redraws each plot at its box, and the box is what to move or resize.
4. Setting lines with known names (see `box_note_settings` in `R/assembly_canvas.R`) are already exact table differences; report any marked "NOT applied" back to the user. Translate only the free-text requests (field `notes`) into values of existing `panel_specs.tsv` columns, for example:
   - "axis text 5 pt" → `axis_text_pt = 5`
   - "no legend" → `legend_position = none`
   - "smaller dots" → a lower `point_size_ggplot` (state the value chosen). With it set, solid dots (shapes 15–20, shape and size not mapped) get no stroke, so a value draws a smaller dot than the same size in plain ggplot2.
   - "thinner lines" → `line_width_pt`; "axis lines 0.5 pt" → `axis_line_pt` (also sets colour-bar ticks)
   - heatmap text → `heatmap_font_pt` (row and column variants exist)
   - Euler labels or numbers → `euler_label_pt` / `euler_quantity_pt`

   - "legend as annotation", "I'll place the labels myself" → `move_to_annotations` (`legend`, `strip labels`, `point labels`; `set names` for Euler). Never move values computed from data (counts, statistics, e.g. a forest plot's odds-ratio labels): they would stop updating. `moved_placed` is written by the build; clear an entry only when the user asks for a fresh copy.
   - several legends too wide → `legend_box_layout = vertical`.
   - renaming or formatting facet labels or set names (italics, superscripts) → move them to annotations; the user retypes them in Inkscape. Do not rename data values.
   - bold/plain axis text → `axis_text_face`; dots mapped to data too big → `mapped_size_factor`.

   If a figure plot differs between renders without a code change, look for an unseeded random step and propose a seed (`PROTOCOL.md`, conventions). It's analysis code, so add it only with the user's agreement.

   If a request has no column, say so and propose a new appearance-only placeholder; never touch analysis arguments. If a note is ambiguous, or names a plot that matches several chunk labels, ask.
5. Show the proposed table changes and the annotations to be copied before writing, unless the project's rules say otherwise. Then write them with `update_tables_from_canvas()`, `write_panel_specs()`, `write_figure_layout()` and `extract_annotations_from_canvas()`.
6. The user renders the figures document. Claude renders only when the project's rules allow it or the user asks.
7. Build the next round with `build_figure_canvas("FigureN")`, render a preview, and run `check_figure_svg()` with the project's journal preset. Report the effective font sizes per layer, any issues, and annotations that may need a nudge because a plot was redrawn.

## Round 0 (first layout)

Read the figure chunks and group them by figure (label pattern or table). Choose sizes by plot type and content: near-square UMAPs and Euler diagrams, heatmaps from matrix rows and columns, forest plots wide and short, dot and bar plots from the number of categories and label length. Stay within the preset's page width and maximum height. Line widths are enforced and unified across all figures: apply `apply_project_line_widths(panel_specs, project_line_width_pt(project_settings, journal_preset))` to every round-0 row, and state the value in the chat. Place panels with `layout_panels_in_rows()`, set `plot_kind` for every row (ggplot, pheatmap, euler, upset, external), write the first table rows, and explain the choices in the chat. Never write explanations into the `notes` column: box notes hold only the user's instructions. For a panel not drawn by R (named by the user; there is no chunk), add a row with `plot_kind = external` and a plot id that starts with `external-` and is the label of no chunk, then tell the user to import its content on the `annotations` layer with Embed (PROTOCOL.md, "Panels not drawn by R").

Wording changes (`title text`, `y axis title text`, …): warn, and do not apply without confirmation, when the current wording contains values computed from the data (test statistics, counts, percentages).

## Final figure

If the user says a final render failed or stopped, read the figures document's file in `figure_assembly/drafting/project_checks/` first (e.g. `Figures.qmd.md`): it records the reason. Never build the journal figure while the latest canvas and the tables disagree: report the differences and let the user decide. Then run `build_figure_canvas("FigureN", "journal")`, `export_figure_pdf()`, `check_figure_svg()` and `check_pdf_fonts()`.

## Panels not drawn by R (`plot_kind = external`)

- The user places the content; never insert or edit images in canvases or annotation files.
- Box notes of an external panel have no setting lines. A request about the content itself cannot be applied: say that it belongs in the program that made the image.
- When `check_figure_svg()` reports images below the journal minimum or recommendation, give each image's pixel size, printed size and dpi, and name the two remedies: a smaller box and image, or more pixels exported from the source software. Never propose resampling to reach the number.
- For a stretched image, report once which image it is and how much it is stretched (compare `printed_width_mm / width_px` with `printed_height_mm / height_px` in `raster_images`). The user may stretch images on purpose (e.g. a heatmap): never ask for a fix, and never hold back a build or export because of it.
- Images in `raster_images` whose `raster_role` is "data cells" or "gradient strip" are left out of both checks by design; do not report them as problems.

## Several source reports

Exploration reports feed tentative figures; final figures come only from the figures document. See PROTOCOL.md, "Several source reports".

- **Adding the toolkit to a report:** insert the setup chunk with `draft_mode <- TRUE`, and check that `embed-resources: true` is set and that the toolkit path resolves to the paper project's `figure_assembly/`. Never set `draft_mode <- FALSE` in an exploration report. If `check_project()` reports `report_fig_path` for the report (a report-wide `fig.path` in the report, its header, `_quarto.yml` or `_metadata.yml`), warn that every unmarked plot is kept as a file in that folder and offer to remove the setting. Files already in that folder stay unless the user deletes them.
- **Combining the plots of a chunk:** when a marked chunk draws several plots, propose one edit and make it only after the user agrees. Apply the placeholder to each plot, and end the chunk with one combining call: `gridExtra::grid.arrange()` for grid objects such as Euler plots, `patchwork::wrap_plots()` for ggplot2 plots, and, for pheatmap, `grid.arrange()` of each result's `$gtable` (drawn with `silent = TRUE`). Choose the arrangement (one row, a grid) from the number of plots and the panel's space, and say which. The drawn plots stay the same; never change data, statistics or mappings to do it. Moving the legend to annotations on a combined plot keeps only the last plot's legend, because every plot of the chunk writes the same legend file; tell the user.
- **Moving a panel** ("move the UMAP to 4b"):
  1. Sync the latest canvas of every figure involved first, so no canvas holds unsynced edits under the old id.
  2. Rename the chunk label and every reference to it in the same report (`@<label>`, `ref.label`, `dependson`); `find_plot_id_references("<old id>")` lists them. Warn when the chunk is cached: a new label discards its cache, and the next render recomputes it. An external panel has no chunk: skip this step, and for another figure say that its image stays in the old figure's annotations until the user copies it across.
  3. In `panel_specs.tsv`, rename `plot_id` and set `figure` and `panel_letter`. For another figure, place the panel in free space on that page and say where; extend the page height in `figure_layout.tsv` if needed, or add the figure's row if the figure is new.
  4. Do not edit canvases or annotation files to rename ids. Moved copies keep their old ids (`moved__<old id>__…`), which is harmless. For another figure, say that freehand content and moved copies stay in the old figure, and offer to clear `moved_placed` so the new figure gets fresh copies.
  5. Show the diff. Render the report as in step 6 of "Build / update Figure N", then build new rounds of every figure involved.
- **Tentative to final:** the copy in the figures document keeps its label. Rename the exploration copy's label so it no longer matches `figure_label_pattern` (prefix `explored-`), and say that its plot returns to the report's own size. Then compare the figure's plot ids in the figures document with those of the tentative figure. Never decide alone what happens to the tentative layout.
  - Same plot ids: use the last tentative settings and say so. Build the next round, render a preview, and look at it to list annotations that may no longer fit the new plots.
  - Plot ids added, removed or replaced: list the changes, and ask whether to keep the tentative settings for the remaining plots or start fresh. Start fresh: clear the size, position and setting columns of the figure's rows (keep `figure`, `panel_letter`, `plot_kind`), propose a new first layout as in "Round 0", and ask before renaming the figure's annotations file.
- **Leftovers:** `check_project()` lists `panel_specs.tsv` rows whose `plot_id` is the label of no chunk in any report that loads the toolkit (`leftover_row`; rows with `plot_kind = external` are never listed). Before the final render, propose deleting them.
- **Limits:** the checks also run at every render, but they write only to the render log, which the user may not read, and a duplicate label does not stop a draft render. When a duplicate label is found, say that the other report's plot file was overwritten and that re-rendering that report restores it. The checks read reports as text: labels made by code and chunks of child documents are not seen.

## Files

- Never overwrite a user's canvas; builds always write a new round.
- Never hand-edit generated SVGs; regenerate them.
- `drafting/` is disposable; `specs/`, `annotations/` and the figures document are the record.
- A document-wide `fig.path` keeps every unmarked plot as a file (`report_fig_path` in `check_project()`). Warn about it when setting up any figures document, and offer to remove it.
- When your natural first approach fails in real use and another one works (or none does), propose a `GOTCHAS.md` entry in the same two-part format. Save it only after the user agrees.
