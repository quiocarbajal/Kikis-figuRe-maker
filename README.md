# Paper-ready figures from R, assembled in Inkscape

A small toolkit and protocol for iterative, AI-assisted assembly of multi-panel figures ready for publication:

- Every R plot is exported at its **final physical size** (mm), with **text in points** and **lines in points**, as real editable text (`svglite`).
- You arrange and annotate in **Inkscape**, and give instructions in plain language inside boxes on the canvas.
- The instructions become values in a table (`panel_specs.tsv`), and one placeholder per plot applies them. **Appearance only**: data, statistics, scales and mappings are never touched.
- Code, tables and an annotations file reproduce every figure. The journal deliverable is a vector PDF with embedded fonts and live text.

Inspired by [FigureFirst](https://github.com/FlyRanch/figurefirst) (Python) and [svgedit](https://github.com/DanielThedie/svgedit) (R).

## Contents

| Path | What it is |
|---|---|
| `PROTOCOL.md` | The step-by-step protocol (start here) |
| [`FLOWCHARTS.md`](FLOWCHARTS.md) | Flow charts of the protocol and a table of its safety checks, for an overview first |
| `GOTCHAS.md` | Places where the natural first approach fails, and what works instead |
| `R/assembly_*.R` | The toolkit code, copied into each paper project |
| `presets/journal_presets.tsv` | Figure specifications for Nature, Nature Communications, Cell Press, Science, PNAS, eLife |
| `templates/figure_assembly/` | Empty folder structure copied into each paper project |
| `skill/figure-assembly/SKILL.md` | Instructions for the AI assistant when it takes part (Agent Skills format); copied into each paper project with the code |
| `examples/example_figures.qmd` | A small example figures document with made-up data |
| `VERSION` | Toolkit version recorded in each project |

## Requirements

- R 4.1 or newer with `tidyverse` (ggplot2 4.0.0 or newer), `xml2`, `svglite` and `knitr`; optionally `pheatmap`, `eulerr` and `UpSetR`.
- Inkscape 1.x (tested with 1.4.4) for previews and PDF export.

## Quick start

1. Clone the repository:

   ```bash
   git clone https://github.com/quiocarbajal/Kikis-figuRe-maker.git
   ```

2. In R, install the toolkit into your paper project, where `<toolkit>` is the cloned folder's path:

   ```r
   for (toolkit_file in list.files("<toolkit>/R", pattern = "\\.R$", full.names = TRUE)) source(toolkit_file)
   install_figure_assembly("path/to/paper_project", "<toolkit>", journal_id = "nature_communications")
   ```

3. Then follow `PROTOCOL.md`, step 1, from item 2.

## Status

Version 0.1.0, first draft. Tested end to end with emulated knitr chunks (installation, round-0 layout, drafting rounds with edits made by Inkscape, sync, final render, journal SVG, PDF export and checks) and with a real Quarto render of the example document, and in use on a full research figures document (one figure finished, one in drafting).

## License

MIT. See `LICENSE`.
