# Protocol: paper-ready figures from R, assembled in Inkscape

Goal: every plot comes out of R at its final physical size, with text at the intended point size and lines at the intended width, so the assembled figure needs no resizing in Inkscape. Only small nudges of annotations remain.

## Hard rules

1. **Appearance only.** This toolkit and its placeholders change sizes, fonts, line widths, legend placement, fixed point sizes and transparency. They never change data, filtering, thresholds, statistics (bins, smoothing, bandwidth), scales, axis limits or zoom, axis transformations, aesthetic mappings, or statistical annotations. Such changes belong in the analysis code and in a separate protocol.
2. **Never resize an R plot in Inkscape.** Resizing also resizes its text and lines. To change a plot's size, resize its *box*; R redraws the plot.
3. **Your files are never overwritten.** Each build writes a new working canvas (`round<N>_working.svg`). Generated files are never edited by hand, except the latest working canvas, which is where you draft.
4. **Nothing is decided silently.** If a canvas and the tables disagree at the final step, the build stops and lists the differences.

## Vocabulary

| Term | Meaning |
|---|---|
| plot id | The knitr chunk label of a plot, e.g. `fig-3a-umap-by-genotype` |
| `panel_specs.tsv` | One row per plot: figure, panel letter, position, size, font sizes, appearance settings, latest notes |
| `figure_layout.tsv` | One row per figure: page width and height |
| working canvas | `figure_assembly/drafting/<Figure>/round<N>_working.svg`, where you draft |
| journal figure | `<final_output_folder>/<Figure>_journal.svg` and `.pdf`, for submission |
| box | A translucent blue rectangle on the `boxes` layer, grouped with its note, marking where a plot goes; the note holds your instructions |

## Folder layout in a paper project

```
<paper_project>/
├── Figures.qmd                    ← draft_mode <- TRUE at the top
├── Figures/                       ← journal figures only (SVG + PDF)
└── figure_assembly/
    ├── R/                         ← toolkit copy (version in settings)
    ├── settings/
    │   ├── journal_presets.tsv
    │   └── project_settings.tsv
    ├── specs/
    │   ├── figure_layout.tsv
    │   └── panel_specs.tsv
    ├── annotations/<Figure>_annotations.svg   ← freehand content (keep)
    └── drafting/                  ← disposable
        └── <Figure>/plots/, final_plots/, round<N>_working.svg
```

Code, `specs/` and `annotations/` reproduce every journal figure. `drafting/` can be deleted at any time.

---

## Step 1: one-time setup in a paper project

1. Install the toolkit from a local clone of this repository:

   ```r
   for (toolkit_file in list.files("<toolkit>/R", pattern = "\\.R$", full.names = TRUE)) source(toolkit_file)
   install_figure_assembly("<paper_project>", "<toolkit>", journal_id = "nature_communications")
   ```

2. Add a setup chunk at the top of the figures document:

   ````r
   ```{r}
   #| include: false
   draft_mode <- TRUE # FALSE for the final render
   for (toolkit_file in list.files(file.path("figure_assembly", "R"), pattern = "\\.R$", full.names = TRUE)) source(toolkit_file)
   figure_assembly <- start_figure_assembly(draft_mode = draft_mode)
   figure_style <- figure_assembly$figure_style
   figure_pheatmap <- figure_assembly$figure_pheatmap
   figure_euler_plot <- figure_assembly$figure_euler_plot
   figure_upset <- figure_assembly$figure_upset
   ```
   ````

3. Insert the placeholders, one per plot. Claude can do this; review the diff.

   | Plot type | Placeholder |
   |---|---|
   | ggplot2 (including Seurat `DimPlot`, `DotPlot`) | add `+ figure_style()` as the last line |
   | patchwork | `& figure_style()` (applies to every sub-plot; not yet tested) |
   | pheatmap | `pheatmap(...)` → `figure_pheatmap(...)` |
   | eulerr | `plot(fit, ...)` → `figure_euler_plot(fit, ...)` |
   | UpSetR | `upset(...)` → `figure_upset(...)` |

   Size and output folder need no placeholder: they come from the table through the chunk label.

4. **Safety check, once.** Render the document before and after inserting the placeholders, with an empty `panel_specs.tsv`. Every figure plot must be identical, which proves the placeholders changed nothing. The first render with the toolkit already switches figure chunks from their previous device to `svglite`, so make the "before" render after step 2.

Conventions in the figures document:

- Set `embed-resources: true` under `format: html:`. Figure plots are written into `figure_assembly/drafting/`; without embedding, the HTML report only links to them and loses its images when `drafting/` is deleted.
- One figure plot per chunk; the first plot of a chunk is the one assembled.
- Chunk labels follow `figure_label_pattern` in `project_settings.tsv`. The default `^fig-(s?[0-9]+)` assigns `fig-3a-…` to Figure3 and `fig-s2b-…` to FigureS2. Chunks that do not match are left exactly as written.

## Step 2: round 0, a first layout (optional, done by Claude)

Claude reads the plotting code and the journal preset, then chooses a starting size per plot by plot type and content: UMAPs and Euler diagrams near-square, heatmaps from matrix dimensions, forest plots wide and short, dot and bar plots from the number of categories. It sets one line width for axis lines and plot lines in every ggplot2 plot of every figure (`line_width_pt` in `project_settings.tsv`, or the journal's `default_line_pt` when that is empty; e.g. 1 pt for Nature Communications, 0.5 pt for Nature), places panels in rows with `layout_panels_in_rows()`, sets each plot's `plot_kind` (ggplot, pheatmap, euler, upset, or external for content not drawn by R, such as a deepTools PDF), and writes the first rows of `panel_specs.tsv` and `figure_layout.tsv`. Claude explains its choices in the chat, not in the boxes. Seeding from an existing assembled figure is possible when you ask for it.

## Step 3: the drafting loop

1. **Render** the figures document with `draft_mode <- TRUE` (RStudio Render button). Draft plots go to `drafting/<Figure>/plots/`.
2. **Build** the next working canvas with `build_figure_canvas("Figure3")`, or ask Claude. It holds four layers:
   - `plots`: locked; the R plots at their real size.
   - `panel_letters`: generated from the table and the journal preset.
   - `annotations`: your freehand content, carried over from round to round.
   - `boxes`: one box per plot, grouped with its note.
3. **Draft in Inkscape** on the latest `round<N>_working.svg`, then save it:
   - Move or resize a *box group* to change where a plot goes and how big it will be. The plot is redrawn at that size in the next round, and its text stays at the requested point size.
   - Each box has a note on a white card. It lists the plot (`plot: <plot id>`), one line per setting that currently has a value (e.g. `axis text: 6 pt`), and a `notes:` line.
   - **To change a setting, edit its number** (`axis text: 7 pt`). To set one that is not listed, add a line with its name (`legend text: 6 pt`, `y axis title text: Fold change\n(log2)`, where a typed `\n` is a line break). To return a setting to the plot's own value, write `–`. Deleting a line changes nothing.
   - Lines with a known setting name are applied exactly by code. Anything else (`dots smaller`, `make the title bold`) is a request that Claude translates, asking when unsure. Unreadable values (`points: big`) are reported and not applied.
   - Known setting names, by plot kind:
     - ggplot2: `title`, `subtitle`, `caption`, `axis title`, `axis text`, `x axis text angle` (degrees, e.g. 45), `legend title`, `legend text`, `legend keys` (key size in mm), `strip text`, `labels` (text drawn inside the plot), `axis lines`, `grid lines`, `lines`, `points`, `transparency`, `legend` (none, right, left, top, bottom, inside), and the wording settings `title text`, `subtitle text`, `caption text`, `x axis title text`, `y axis title text`.
     - ggplot2, also: `axis text style` (plain, bold, italic, bold italic), `mapped point sizes` (a factor for sizes mapped to data, e.g. `0.5` halves DotPlot dots; relative sizes and legend values stay the same), `legend layout` (vertical or horizontal: how several legends are arranged), `move to annotations` (`legend`, `strip labels`, `point labels`, e.g. Seurat's cluster names).
     - pheatmap: `heatmap text`, `row labels`, `column labels`, `cell borders`.
     - eulerr: `title`, `labels`, `numbers`, `move to annotations` (`set names`).
   - **Move to annotations:** R draws the plot without the named elements, and the next canvas places an editable copy of them on the `annotations` layer once, inside the plot's box (legend at the top right, words below the note card). From then on they are yours: move them, retype them (e.g. italics, superscripts), or delete parts. Copies are static: they do not update if the data change, which is why Euler numbers always stay in the plot. Each element is placed only once: the build records it in the `moved_placed` column of `panel_specs.tsv`, so moving, ungrouping, editing or deleting the copy never brings a new one back. To get a fresh copy (e.g. after the data changed), clear that entry in `moved_placed`.
     - UpSetR: `text scale`.
   - Wording changes are refused or flagged for text that contains values computed from the data (e.g. a test result in a subtitle).
   - For a box you draw yourself, start its note with `plot: <plot id>` and place the note inside the box.
   - Hide the `boxes` layer to see the plots without the note cards.
   - Add freehand content on the `annotations` layer only.
   - Create text by clicking with the Text tool, not by dragging; dragging creates flowed text, which many programs cannot display.
4. **Sync** by asking Claude to "update Figure 3":
   - The canvas is compared with the tables (`compare_canvas_with_tables()`), and positions and sizes are copied into `panel_specs.tsv`.
   - Setting lines are applied to the table by code. Claude translates the free-text requests into table values, e.g. "dots smaller" → `point_size_ggplot = 0.6`. Anything that would change the analysis is refused.
   - The annotations layer is copied to `annotations/<Figure>_annotations.svg`.
5. Repeat from 1 until satisfied. Each round is a new file; earlier rounds can be deleted.

## Step 4: the final figure

1. Set `draft_mode <- FALSE` and render. Before anything is drawn, each figure's latest working canvas is compared with the tables. If they disagree, rendering stops and lists the differences, and you decide: sync the tables from the canvas, or keep the tables with `start_figure_assembly(draft_mode = FALSE, on_canvas_mismatch = "use_table")`.
2. Build the journal figure with `build_figure_canvas("Figure3", "journal")` (plots, letters and annotations; no boxes), then export it with `export_figure_pdf("Figures/Figure3_journal.svg")`, which embeds fonts and keeps text as text. `export_figure_eps()` exists for journals that ask for EPS; EPS cannot store transparency.
3. Check with `check_figure_svg()` (effective font sizes, line widths, fonts, stretched text, flowed text, linked images, page size) and `check_pdf_fonts()` (text is text, fonts embedded). The PDF check is a basic scan; when in doubt, confirm in Acrobat under File > Properties > Fonts.

## Changing journal after a figure is finished

1. Set `journal` in `project_settings.tsv`.
2. Claude re-fits sizes and positions in the tables to the new page width, respecting the new font limits.
3. Render and rebuild. Panel letters follow the new preset, and R text is redrawn at the requested sizes. Freehand annotations stay where they are on the page and may need a nudge; Claude can restyle annotation text (font family and size).

## Journal presets

`presets/journal_presets.tsv` holds Nature, Nature Communications, Cell Press, Science, PNAS and eLife, checked on 2026-10-16. The `source_url` and `notes` columns say where each value comes from; values from archived pages and toolkit defaults are marked in `notes`. None of these journals lists SVG as an accepted format, so the deliverable is a vector PDF with embedded fonts and live text. Add or edit rows as needed.

## Known limitations (version 0.1.0)

- Tested with emulated knitr chunks and with a real Quarto render of `examples/example_figures.qmd`; not yet tested on a full research figures document.
- One figure plot per chunk.
- Base-R plots without a wrapper are sized and placed but not restyled.
- UpSetR font size is a multiplier (`upset_text_scale`), not points.
- `& figure_style()` on patchwork objects is untested.
- Text drawn by plots that are not ggplot2, pheatmap, eulerr or UpSetR is not restyled.
