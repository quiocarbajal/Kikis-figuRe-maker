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
| plot id | The knitr chunk label of a plot, e.g. `fig-3a-umap-by-genotype`; for a panel not drawn by R, a name given to it, e.g. `external-3c-confocal-ko` |
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
├── <exploration reports>.qmd      ← optional, anywhere in the project; tentative figures only
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
        ├── project_checks/        ← record of each report's last render
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
- If the document saves every plot to a folder of its own (a document-wide `fig.path`), every unmarked plot is also kept there at each render; the project checks report it at every render (see "Project checks"), and Claude offers to remove the setting.
- One figure plot per chunk; the first plot of a chunk is the one assembled.
- Seed every random step that shapes a figure plot, so each render draws the same plot: call `set.seed()` with any fixed number right before it, or pass the function's own `seed` argument where it has one (e.g. `position_jitter(seed = 1)`, `ggrepel::geom_text_repel(seed = 1)`). Typical random steps are Euler layouts (`eulerr::euler()`), jittered points, repelled labels, random subsampling of cells for plotting, and UMAP or t-SNE run without a fixed seed. Without a seed, the before/after safety check fails, and the plot you annotate in a draft round can differ from the final one. A seed is a change to the analysis code, so it's your decision: Claude proposes it and never adds it silently.
- Chunk labels follow `figure_label_pattern` in `project_settings.tsv`. The default `^fig-(s?[0-9]+)` assigns `fig-3a-…` to Figure3 and `fig-s2b-…` to FigureS2. Chunks that do not match are left exactly as written.

## Step 2: round 0, a first layout (optional, done by Claude)

Claude reads the plotting code and the journal preset, then chooses a starting size per plot by plot type and content: UMAPs and Euler diagrams near-square, heatmaps from matrix dimensions, forest plots wide and short, dot and bar plots from the number of categories. It sets one line width for axis lines and plot lines in every ggplot2 plot of every figure (`line_width_pt` in `project_settings.tsv`, or the journal's `default_line_pt` when that is empty; e.g. 1 pt for Nature Communications, 0.5 pt for Nature), places panels in rows with `layout_panels_in_rows()`, sets each plot's `plot_kind` (ggplot, pheatmap, euler, upset, or external for content not drawn by R, such as a deepTools PDF), and writes the first rows of `panel_specs.tsv` and `figure_layout.tsv`. Claude explains its choices in the chat, not in the boxes. Seeding from an existing assembled figure is possible when you ask for it.

## Step 3: the drafting loop

1. **Render** the figures document with `draft_mode <- TRUE` (RStudio Render button, or ask Claude when the project's rules allow it). Draft plots go to `drafting/<Figure>/plots/`.
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
     - ggplot2: `title`, `subtitle`, `caption`, `axis title`, `axis text`, `x axis text angle` (degrees, e.g. 45), `legend title`, `legend text`, `legend keys` (key size in mm), `strip text`, `labels` (text drawn inside the plot), `axis lines` (axis lines, axis ticks and colour-bar ticks), `grid lines`, `lines`, `points` (fixed dot size; solid dots then get no ggplot2 stroke, so the size alone sets the dot), `transparency`, `legend` (none, right, left, top, bottom, inside), and the wording settings `title text`, `subtitle text`, `caption text`, `x axis title text`, `y axis title text`.
     - ggplot2, also: `axis text style` (plain, bold, italic, bold italic), `mapped point sizes` (a factor for sizes mapped to data, e.g. `0.5` halves DotPlot dots; relative sizes and legend values stay the same), `legend layout` (vertical or horizontal: how several legends are arranged), `move to annotations` (`legend`, `strip labels`, `point labels`, e.g. Seurat's cluster names).
     - pheatmap: `heatmap text`, `row labels`, `column labels`, `cell borders`.
     - eulerr: `title`, `labels`, `numbers`, `move to annotations` (`set names`).
   - **Move to annotations:** R draws the plot without the named elements, and the next canvas places an editable copy of them on the `annotations` layer once, inside the plot's box (legend at the top right, words below the note card). From then on they are yours: move them, retype them (e.g. italics, superscripts), or delete parts. Copies are static: they do not update if the data change, which is why Euler numbers always stay in the plot. Each element is placed only once: the build records it in the `moved_placed` column of `panel_specs.tsv`, so moving, ungrouping, editing or deleting the copy never brings a new one back. To get a fresh copy (e.g. after the data changed), clear that entry in `moved_placed`.
     - UpSetR: `text scale`.
   - Wording changes are refused or flagged for text that contains values computed from the data (e.g. a test result in a subtitle).
   - For a box you draw yourself, start its note with `plot: <plot id>` and place the note inside the box.
   - Hide the `boxes` layer to see the plots without the note cards.
   - Add freehand content on the `annotations` layer only.
   - For a panel not drawn by R (`plot_kind = external`), see "Panels not drawn by R".
   - Create text by clicking with the Text tool, not by dragging; dragging creates flowed text, which many programs cannot display.
4. **Sync** by asking Claude to "update Figure 3":
   - The canvas is compared with the tables (`compare_canvas_with_tables()`), and positions and sizes are copied into `panel_specs.tsv`. They always come from the boxes. A plot moved or resized on the canvas itself, instead of its box, is ignored and reported to you, because the next round redraws it at its box.
   - Setting lines are applied to the table by code. Claude translates the free-text requests into table values, e.g. "dots smaller" → `point_size_ggplot = 0.6`. Anything that would change the analysis is refused.
   - The annotations layer is copied to `annotations/<Figure>_annotations.svg`.
5. Repeat from 1 until satisfied. Each round is a new file; earlier rounds can be deleted.

## Step 4: the final figure

1. Set `draft_mode <- FALSE` and render. Before anything is drawn, the project checks run (see "Project checks"), and each figure's latest working canvas is compared with the tables. If they disagree, rendering stops and lists the differences (also recorded in the report's file in `drafting/project_checks/`), and you decide: sync the tables from the canvas, or keep the tables with `start_figure_assembly(draft_mode = FALSE, on_canvas_mismatch = "use_table")`.
2. Build the journal figure with `build_figure_canvas("Figure3", "journal")` (plots, letters and annotations; no boxes), then export it with `export_figure_pdf("Figures/Figure3_journal.svg")`, which embeds fonts and keeps text as text. `export_figure_eps()` exists for journals that ask for EPS; EPS cannot store transparency.
3. Check with `check_figure_svg()` (effective font sizes, line widths, fonts, stretched text, flowed text, linked images, image resolution, stretched images, page size) and `check_pdf_fonts()` (text is text, fonts embedded). The PDF check is a basic scan; when in doubt, confirm in Acrobat under File > Properties > Fonts.

## Project checks

`check_project()` reads every report of the paper project that loads the toolkit (a `.qmd` or `.Rmd` file whose R code calls `start_figure_assembly(`), the tables, and the plot files in `drafting/`, and lists what it finds. It only reports: it never deletes files, edits a report or changes a table.

| Check | Severity | What it finds |
|---|---|---|
| `duplicate_label` | problem | A figure label used by more than one chunk, e.g. in the figures document and in an exploration report. The plot file in `drafting/` comes from whichever report was rendered last. |
| `multi_plot_chunk` | problem | A figure chunk whose latest render drew several plots. Only the first is assembled (see "Several source reports", item 3). |
| `leftover_row` | warning | A `panel_specs.tsv` row whose plot id is the label of no chunk. Rows with `plot_kind = external` have no chunk by design and are never listed. |
| `report_fig_path` | warning | A report-wide `fig.path` (in `knitr::opts_chunk$set()`, in a report's header, or in `_quarto.yml` or `_metadata.yml`): every unmarked plot is also kept as a file at each render. |
| `no_reports` | warning | No report loads the toolkit, usually because the checks were run from another folder. |
| `stale_plot_files` | note | Numbered plot files (`-2.svg`, …) left by an earlier render. They are not used. |

- **At every render**, `start_figure_assembly()` runs the checks and writes the problems and warnings to the render log (RStudio's Background Jobs pane, or the terminal that runs `quarto render`), followed by the number of notes. The HTML report does not show them, because the setup chunk is hidden. A figure chunk that draws several plots is also reported in the log right after it runs.
- **The record of each report's last render:** every render also rewrites that report's file in `figure_assembly/drafting/project_checks/`, for people and AI assistants who did not see the render log. There is one file per report, named after its path in the project with `/` as `__` (e.g. `Figures.qmd.md`, `exploration__RNAseq.qmd.md`), so rendering one report never erases another report's record. Each file records the mode and time of the render, and every finding, notes included; the findings cover the whole project, so a file whose report has not been rendered for a while holds older findings. During the render it adds the figure chunks that drew several plots, the canvas comparisons of a final render, and the reason a final render stopped. It is written only during a render, so running the setup chunk in the console does not replace it. A render that ends early for another reason (e.g. an error in the analysis code) leaves no entry.
- **A final render stops** (`draft_mode <- FALSE`) on a duplicate figure label, before anything is drawn, and at a figure chunk that draws several plots, but only for figures that have a working canvas. For figures not yet drafted, problems are written to the log and the render continues, so a figure in progress never blocks finishing another.
- **By hand or by Claude:** run `check_project()` from the project folder. It prints every finding, notes included, and returns them as a table (`check_id`, `severity`, `location`, `plot_id`, `finding`, `suggested_action`).
- **Before renaming a chunk:** `find_plot_id_references("<plot id>")` lists every line of every report that mentions the plot id: its chunk label, `@` cross-references, chunk options such as `ref.label` and `dependson`, and any other mention.
- **Where it looks:** every `.qmd` and `.Rmd` file under the project folder, except in hidden folders, `figure_assembly/`, `renv/`, `packrat/` and `node_modules/`. Links to folders are not followed, because they often point to raw data.

## Panels not drawn by R

A panel that R does not draw (a micrograph, a blot, a deepTools PDF, a schematic made in another program) has `plot_kind = external`. It goes through the same rounds as an R plot, but you place its content by hand.

1. **Declaring the panel.** Tell Claude about it, e.g. "3c is a confocal image, about 60 × 45 mm". There is no chunk to read, so Claude adds the row to `panel_specs.tsv`: a plot id that is the label of no chunk (Claude uses the prefix `external-`, e.g. `external-3c-confocal-ko`), the figure, the panel letter, `plot_kind = external`, and a position and size.
2. **What each working canvas holds for it:** a box and a panel letter, but nothing on the `plots` layer. The box note lists only `plot:` and `notes:`. No setting lines apply, because R does not draw the content; requests about the content itself (e.g. a thicker scale bar) belong in the program that made it.
3. **Placing the content, once, in Inkscape.** On the `annotations` layer, use File > Import:
   - For a PNG, TIFF or JPEG, choose **Embed**, not Link. A linked image is only a path to a file on your computer; it is missing from the PDF you send.
   - A PDF is imported as editable vector objects (text stays text).
   - Scale and move the content to fill the box. Keep its proportions (lock icon between W and H in the toolbar), so a micrograph or blot is not stretched.
4. **The box and the content are not tied.** Moving or resizing the box moves the panel letter in the next round, not the image. If you move one, move the other. Moving the panel to another figure leaves the image in the old figure's annotations; copy it across in Inkscape.
5. **Sync** as usual ("update Figure 3"). The box goes into `panel_specs.tsv`; the image is saved with the rest of the `annotations` layer in `annotations/<Figure>_annotations.svg`. That file is the panel's only record: keep it. When the source image changes, delete the old copy on the latest canvas and import the new file.
6. **Checks.** `check_figure_svg()` measures every embedded image at its printed size:
   - **Resolution**, in dpi (image pixels per inch at the printed size). An image below the journal's minimum (`min_image_dpi` in the preset; 300 dpi for all six journals) gets a warning. Nature's own pages disagree: the final-submission page asks for at least 300 dpi, while its research figure guide asks for 450 dpi, so for Nature an image between 300 and 450 dpi gets a separate warning (`recommended_image_dpi`). Shrinking an image on the page raises its dpi; enlarging it lowers it. Resampling an image to more pixels raises the number but adds no detail, and Science does not permit it; export more pixels from the acquisition or analysis software instead.
   - **Stretching.** An image resized more in one direction than the other gets a warning. Stretching is fine when intended (e.g. a heatmap), but it distorts a photograph, micrograph or blot.
   - Images that are not pictures are left out of both checks: data cells drawn with sharp pixels (ggplot2 `geom_raster`, or a heatmap from another program saved that way) and one-pixel colour bars (ggplot2 colour-bar legends).
   - Text in imported vector content (e.g. deepTools axis labels) is measured like any other text, so text that shrinks below the journal minimum when a PDF is scaled down is reported. Text inside a raster image cannot be measured or restyled.

## Several source reports: tentative figures from exploration

During exploration, the plots of a figure can come from several reports (`.qmd` or `.Rmd`), for example one per data type. They are assembled into **tentative figures** with the same drafting loop (Step 3). Final figures are always drawn by the figures document; exploration reports never render with `draft_mode <- FALSE`.

1. **Turning the toolkit on in a report.** A report takes part only if it contains the setup chunk of Step 1, the hidden chunk that loads the toolkit. In an exploration report, `draft_mode` stays `TRUE`, because exploration reports never make final figures. All reports live inside the paper project folder, and `execute-dir: project` makes each report run from that folder, so the same setup chunk finds the one shared `figure_assembly/` folder from every report. Set `embed-resources: true` in each report's header, as for the figures document: the plots of marked chunks are written into `figure_assembly/drafting/`, which later renders overwrite, so the HTML must hold its own copy. If a report saves every plot to a folder of its own (a report-wide `fig.path`, e.g. `fig.path = "./Figures/"` in `knitr::opts_chunk$set()`, in the report's header, or in `_quarto.yml`), every unmarked plot is also kept as a file in that folder at each render. The project checks report this at every render, and Claude offers to remove that setting when it adds the setup chunk; files already in the folder stay until you delete them.
2. **Marking a plot.** As in the figures document: a chunk is a figure plot when its label matches `figure_label_pattern` (e.g. `fig-3a-umap-by-genotype`) or has a row in `panel_specs.tsv`. knitr writes every plot to a file. For other chunks, that file sits in the temporary `<report>_files/` folder, which Quarto deletes after copying the image into the HTML (with `embed-resources: true`), so those plots exist only in the HTML report. For marked chunks, the toolkit writes the file into `figure_assembly/drafting/<Figure>/plots/`, where it stays. A marked plot appears in the report at its figure size and fonts.
3. **A chunk that draws several plots.** All plots of a marked chunk go into the figure as one panel. knitr writes one file per plot (`<label>-1.svg`, `<label>-2.svg`, …), but only the first file of a chunk is assembled, so the chunk must draw its plots as one combined plot, in R: each plot gets its placeholder, and the chunk ends with one combining call, e.g. `gridExtra::grid.arrange(grobs = plot_list, nrow = 1)` or `patchwork::wrap_plots(plot_list)` (the trial's Figure 3e chunk does this with three Euler plots). The combined plot is one panel with one box; its size and settings apply to all its plots alike, and the HTML report shows the combined plot. Such chunks are reported in the render log and by `check_project()`, and they stop a final render when their figure has a working canvas (see "Project checks"). Claude proposes the combining line; the plots themselves stay as they are.
4. **Unique plot ids.** knitr checks chunk labels only within one document. The project checks look for figure labels used by more than one chunk at every render, and Claude runs them before every "update Figure N" (see "Project checks"). A final render stops on such a label when its figure has a working canvas. While drafting, a duplicate is reported but does not stop the render: the report rendered last overwrites the other report's plot file in `drafting/`, and re-rendering that report restores it.
5. **Drafting.** Step 3 is unchanged, except that its item 1 means rendering every report that draws a plot of the figure. Do not cache plotting chunks (`cache: true`): a cached chunk may not be re-run when only `panel_specs.tsv` changed, so new settings would not be applied. Heavy computation chunks may be cached.
6. **Moving a panel** to another letter or figure: ask Claude, e.g. "move the UMAP to 4b". Claude first syncs the latest canvases (Step 3, item 4), then renames the chunk label, every reference to it (listed by `find_plot_id_references()`) and the table row, and shows the diff. The report is rendered, by you or by Claude when the project's rules allow it or you ask, and Claude builds new rounds of the figures involved. Annotations do not follow a plot to another figure: freehand content and moved elements stay in the old figure. Copy them across in Inkscape, or ask Claude for fresh copies of the moved elements in the new figure (it clears `moved_placed`).
7. **From tentative to final.** When a tentative figure is accepted, its plotting code is copied into the figures document with the labels unchanged, by you or, on request, verbatim by Claude. How the data reach the figures document is outside this protocol. Claude then renames the label of the copy left in the exploration report (e.g. `explored-fig-3a-umap-by-genotype`), so that only one document draws each plot id; that plot returns to the report's own size in its HTML. Then Claude compares the plots of the final figure with those of the tentative figure:
   - **Same plots** (same plot ids): the last settings of the tentative figure are used. Table rows and annotations carry over. Claude says so, builds the next round with the plots from the figures document, and lists annotations that may no longer fit (e.g. an arrow pointing at a cluster that moved).
   - **Plots added, removed or replaced:** Claude shows what changed and asks whether to keep the tentative settings for the plots that remain, or to start fresh. Starting fresh means that Claude empties the positions, sizes and settings in the figure's rows of `panel_specs.tsv` (which plots belong to the figure, and their panel letters, stay) and proposes a new first layout (Step 2). Every build copies `annotations/<Figure>_annotations.svg` into the new round, so Claude asks before renaming that file (e.g. to `<Figure>_annotations_tentative.svg`); once renamed, it is kept but no longer used.
8. **Leftovers.** The project checks list `panel_specs.tsv` rows whose plot id is the label of no chunk in any report that loads the toolkit (`leftover_row`, see "Project checks"), and Claude proposes deleting them. Rows with `plot_kind = external` are left out of this list: they have no chunk by design. A leftover figure whose latest canvas disagrees with the tables can stop the final render.

## Changing journal after a figure is finished

1. Set `journal` in `project_settings.tsv`.
2. Claude re-fits sizes and positions in the tables to the new page width, respecting the new font limits.
3. Render and rebuild. Panel letters follow the new preset, and R text is redrawn at the requested sizes. Freehand annotations stay where they are on the page and may need a nudge; Claude can restyle annotation text (font family and size).

## Journal presets

`presets/journal_presets.tsv` holds Nature, Nature Communications, Cell Press, Science, PNAS and eLife, checked on 2026-10-16. The `source_url` and `notes` columns say where each value comes from; values from archived pages and toolkit defaults are marked in `notes`. None of these journals lists SVG as an accepted format, so the deliverable is a vector PDF with embedded fonts and live text. `min_image_dpi` is the journal's minimum for photographs and other continuous-tone images at printed size, checked on 2026-10-19; text and lines stay vector in this toolkit, so the higher minimums that journals set for raster line art do not apply to them. `recommended_image_dpi` is filled only where a journal also asks for more (Nature: 450 dpi). Add or edit rows as needed.

## Known limitations (version 0.1.0)

- Tested with emulated knitr chunks, with a real Quarto render of `examples/example_figures.qmd`, and on a full research figures document (one figure finished, one in drafting).
- One figure plot per chunk.
- Base-R plots without a wrapper are sized and placed but not restyled.
- UpSetR font size is a multiplier (`upset_text_scale`), not points.
- `& figure_style()` on patchwork objects is untested.
- Text drawn by plots that are not ggplot2, pheatmap, eulerr or UpSetR is not restyled.
- Image resolution is measured for embedded PNG and JPEG images only; Inkscape stores imported TIFF files as PNG.
- A heatmap stored as a smooth image (e.g. a deepTools heatmap imported from a PDF) is checked like a photograph, so stretching it on purpose gives a "stretched image" warning.
- The project checks read reports as text, without running knitr. They do not see chunk labels made by code, chunks of child documents (`child =`), or chunks of included files that do not load the toolkit themselves.
