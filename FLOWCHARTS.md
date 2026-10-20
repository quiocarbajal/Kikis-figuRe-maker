# Protocol flow charts

An overview of `PROTOCOL.md` and `SKILL.md`. `PROTOCOL.md` is the reference: where the two disagree, it is right. These charts are not kept in sync with the protocol automatically; they match it as of commit 1cb6ae3 (toolkit 0.1.0).

Colours: grey is a step, red is a check that can stop the work, amber is a check that only reports.

## 1. Overview

Four phases. The side paths can happen at any time while drafting.

```mermaid
flowchart TB
  classDef default fill:#f1efe8,stroke:#b4b2a9,color:#1f1e1c
  subgraph phases ["Phases"]
    direction LR
    setup["Setup<br>once per project"] --> round0["Round 0<br>first layout"] --> drafting["Drafting loop<br>repeat each round"] --> final["Final figure<br>journal PDF"]
  end
  subgraph side_paths ["Side paths while drafting"]
    direction TB
    external_panels["Panels not drawn by R"]
    source_reports["Several source reports"]
    moving_panel["Moving a panel"]
    changing_journal["Changing journal"]
  end
  phases -.- side_paths
  style phases fill:none,stroke:none
  style side_paths fill:none,stroke:#b4b2a9,stroke-dasharray:4 4
```

## 2. Setup and round 0

Once per paper project. You run the safety check by hand.

```mermaid
flowchart TD
  classDef default fill:#f1efe8,stroke:#b4b2a9,color:#1f1e1c
  classDef stop fill:#fcebeb,stroke:#a32d2d,color:#791f1f
  install["Install the toolkit<br>code and instructions, same version"] --> pointer["Point your assistant to SKILL.md<br>one line in AGENTS.md or CLAUDE.md"]
  pointer --> setup_chunk["Add the setup chunk<br>calls start_figure_assembly()"]
  setup_chunk --> placeholders["Insert placeholders<br>+ figure_style() and wrappers"]
  placeholders --> safety_check["Safety check, once<br>plots identical before and after"]:::stop
  safety_check --> round0["Round 0 layout<br>the assistant proposes sizes"]
```

## 3. Drafting loop

Repeats once per round. Checks during a draft render never stop it. Before syncing, the assistant reads the findings files in `drafting/project_checks/` (one per report) and runs `check_project()`. Positions and sizes come only from the boxes: a plot moved or resized on the canvas itself is reported and ignored.

```mermaid
flowchart TD
  classDef default fill:#f1efe8,stroke:#b4b2a9,color:#1f1e1c
  classDef stop fill:#fcebeb,stroke:#a32d2d,color:#791f1f
  classDef report fill:#faeeda,stroke:#854f0b,color:#633806
  render["Render in draft mode<br>plots go to drafting/plots/"] --> render_checks["Checks during the render<br>render log and findings file"]:::report
  render_checks --> build["Build the working canvas<br>round#lt;N#gt;_working.svg"]
  build --> inkscape["Draft in Inkscape<br>move boxes, edit notes"]
  inkscape --> ask["Ask: update Figure N<br>the assistant syncs the canvas"]
  ask --> sync_checks["Sync checks<br>boxes vs tables, appearance only"]:::stop
  sync_checks --> saved["Tables and annotations saved<br>after you approve"]
  saved -->|next round| render
```

## 4. Final figure

The three render-time stops apply only to figures that have a working canvas, and each stop is recorded in the figures document's findings file in `drafting/project_checks/`. The journal build also refuses to run while canvas and tables disagree.

```mermaid
flowchart TD
  classDef default fill:#f1efe8,stroke:#b4b2a9,color:#1f1e1c
  classDef stop fill:#fcebeb,stroke:#a32d2d,color:#791f1f
  classDef report fill:#faeeda,stroke:#854f0b,color:#633806
  render["Render in final mode<br>plots go to drafting/final_plots/"] --> duplicates["Duplicate figure labels?<br>stop if the figure has a canvas"]:::stop
  duplicates --> canvas_match["Canvas matches the tables?<br>stop, unless use_table is chosen"]:::stop
  canvas_match --> one_plot["Each chunk drew one plot?<br>stop at a chunk with several"]:::stop
  one_plot --> journal_build["Build the journal figure<br>refused if the canvas disagrees"]:::stop
  journal_build --> export["Export the vector PDF<br>fonts embedded, text kept live"]
  export --> figure_checks["Figure checks<br>fonts, lines, images, PDF fonts"]:::report
```

## 5. Safety checks in one place

"Enforced by" matters for public users: a rule enforced by the assistant holds only when an assistant has read `figure_assembly/SKILL.md`, and disappears when someone works without one.

| Check | Enforced by | When it runs | What happens on a problem |
|---|---|---|---|
| Before/after identity check | You, by hand | Once, at setup | You investigate before continuing. A plot that uses random numbers (Euler layouts, jitter, repelled labels, subsampling) differs between renders unless it is seeded. |
| Project checks: duplicate labels, multi-plot chunks, leftover rows, report-wide `fig.path`, stale plot files | Code | Start of every render; `check_project()` at any time | Draft: written to the render log and the report's findings file in `drafting/project_checks/`. Final: stops on a duplicate label in a figure with a canvas. |
| Plot count per chunk | Code (knitr hook) | After each figure chunk | Draft: render log and the report's findings file only. Final: stops at that chunk if its figure has a canvas. |
| Box-note parsing | Code | Each sync | Known setting names are applied exactly; unreadable values are reported as "NOT applied". |
| Appearance-only rule | Assistant | Each sync, for free-text notes | Refuses analysis changes and flags wording that contains values computed from the data. |
| Canvas vs tables | Code | Each sync, start of a final render, journal build | Positions and sizes come only from the boxes; a plot moved on the canvas itself is reported and ignored. The final render stops unless you choose `use_table`; the journal build is refused. The final-render result is recorded in the figures document's findings file. |
| `check_figure_svg()` | Code | After each build, and at the end | Reports font sizes, line widths, fonts, flowed text, linked images, image dpi, stretching and page size. |
| `check_pdf_fonts()` | Code | After the PDF export | Reports text turned into outlines and fonts that are not embedded. |

## 6. Panels not drawn by R

Micrographs, blots, deepTools PDFs and schematics. Their table rows never count as leftovers.

```mermaid
flowchart TD
  classDef default fill:#f1efe8,stroke:#b4b2a9,color:#1f1e1c
  classDef report fill:#faeeda,stroke:#854f0b,color:#633806
  declare["Declare the panel<br>the assistant adds an external- row"] --> canvas["Canvas: box and letter only<br>no setting lines in the note"]
  canvas --> import["Import the image<br>Embed, keep proportions"]
  import --> sync["Sync as usual<br>image saved with annotations"]
  sync --> image_checks["Image checks<br>resolution and stretching"]:::report
```

## 7. Several source reports: tentative to final

Exploration reports feed tentative figures; final figures always come from the figures document. At the last step, the assistant asks you instead of deciding when the plots changed.

```mermaid
flowchart TD
  classDef default fill:#f1efe8,stroke:#b4b2a9,color:#1f1e1c
  exploration["Exploration report<br>setup chunk, draft mode only"] --> tentative["Tentative figure<br>the usual drafting loop"]
  tentative --> accepted["Figure accepted<br>code copied, labels unchanged"]
  accepted --> rename["Rename the exploration copy<br>prefix explored-"]
  rename --> compare["Same plot ids as tentative?<br>the assistant compares both"]
  compare -->|yes| keep["Keep the settings<br>next round, check annotations"]
  compare -->|no| decide["You decide<br>keep settings or start fresh"]
```

## 8. Moving a panel, and changing journal

Two short side paths. Renaming a cached chunk discards its cache, so the next render recomputes it.

### Moving a panel

```mermaid
flowchart TD
  classDef default fill:#f1efe8,stroke:#b4b2a9,color:#1f1e1c
  sync_canvases["Sync every canvas<br>of each figure involved"] --> find_references["Find every reference<br>find_plot_id_references()"]
  find_references --> rename_label["Rename the chunk label<br>and every reference to it"]
  rename_label --> rename_row["Rename the table row<br>new figure and letter"]
  rename_row --> rebuild["Render and rebuild<br>annotations stay behind"]
```

### Changing journal

```mermaid
flowchart TD
  classDef default fill:#f1efe8,stroke:#b4b2a9,color:#1f1e1c
  set_journal["Set the journal<br>in project_settings.tsv"] --> refit["The assistant re-fits the tables<br>new width and font limits"]
  refit --> rebuild["Render and rebuild<br>nudge annotations by hand"]
```
