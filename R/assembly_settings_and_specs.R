# Figure assembly toolkit: settings, units, and specification tables
#
# Part of the paper-figure assembly toolkit. All files in this folder are
# loaded together with:
#   for (toolkit_file in list.files(file.path("figure_assembly", "R"), pattern = "\\.R$", full.names = TRUE)) source(toolkit_file)
#
# HARD RULE OF THIS TOOLKIT: it changes appearance only (sizes, fonts, line
# widths, legend placement, fixed point sizes). It never changes data,
# filtering, statistics, scales, axis limits, or aesthetic mappings.

library(tidyverse)
library(xml2)

# ---- Unit conversions --------------------------------------------------------

millimetres_per_inch <- 25.4
millimetres_per_point <- 25.4 / 72 # 1 pt (PostScript point) = 1/72 inch
millimetres_per_css_pixel <- 25.4 / 96 # SVG "px" and unit-less lengths = 1/96 inch

point_to_millimetre <- function(length_pt) length_pt * millimetres_per_point
millimetre_to_point <- function(length_mm) length_mm / millimetres_per_point
millimetre_to_inch <- function(length_mm) length_mm / millimetres_per_inch

# ggplot2 text drawn by layers (geom_text, geom_label) takes size in mm.
font_pt_to_geom_text_size <- function(font_pt) font_pt / ggplot2::.pt

# ggplot2 linewidth: 1 unit = .pt * 1/96 inch (about 0.75 mm, a historical
# ggplot2 convention). Converts a line width in pt to ggplot2 linewidth.
line_pt_to_ggplot_linewidth <- function(line_pt) line_pt * (96 / 72) / ggplot2::.pt

# ---- Specification table columns ---------------------------------------------

# Every column of panel_specs.tsv. Units are part of each column name.
panel_spec_columns <- c(
  "plot_id",               # full knitr chunk label of the plot
  "figure",                # e.g. Figure3, FigureS2
  "panel_letter",          # e.g. a, b (case is set by the journal preset)
  "plot_kind",             # ggplot, pheatmap, euler, upset, external (sets the box's setting names)
  "x_mm", "y_mm",          # top-left corner of the plot on the figure page
  "width_mm", "height_mm", # physical size of the whole plot
  "title_pt", "subtitle_pt", "caption_pt", "axis_title_pt", "axis_text_pt",
  "axis_text_face",        # plain, bold, italic, bold.italic
  "x_text_angle",          # rotation of x-axis tick labels in degrees (e.g. 45, 90)
  "legend_title_pt", "legend_text_pt", "legend_key_mm", "strip_text_pt",
  "label_text_pt",         # text drawn by layers: geom_text, geom_label, ggrepel
  "axis_line_pt",          # axis lines and ticks (only where the plot draws them)
  "grid_line_pt",          # panel grid lines (only where the plot draws them)
  "line_width_pt",         # lines drawn by layers (geom_line, geom_errorbar, ...)
  "point_size_ggplot",     # geom_point size in ggplot2's own size unit
  "mapped_size_factor",    # multiplies sizes mapped to data (e.g. DotPlot dots); relative sizes kept
  "alpha",                 # fixed transparency of layers (0-1)
  "legend_position",       # none, right, left, top, bottom, inside
  "legend_box_layout",     # vertical or horizontal: how several legends are arranged
  "heatmap_font_pt", "heatmap_row_font_pt", "heatmap_col_font_pt",
  "heatmap_cell_border",   # colour name, hex code, or NA for no border
  "euler_label_pt", "euler_quantity_pt",
  "upset_text_scale",      # UpSetR text.scale multiplier
  "title_text", "subtitle_text", "caption_text", # ggplot2 label wording; a typed \n = line break
  "x_title_text", "y_title_text",
  "letter_dx_mm", "letter_dy_mm", # panel-letter offset from the plot corner
  "move_to_annotations",   # elements drawn as editable annotations instead of in the plot:
                           # legend, strip labels, point labels (ggplot); set names (euler)
  "moved_placed",          # written by the canvas build: moved elements already placed once,
                           # never placed again (clear an entry to get a fresh copy)
  "notes"                  # latest plain-language instructions from the canvas
)

figure_layout_columns <- c("figure", "page_width_mm", "page_height_mm", "notes")

empty_panel_specs <- function() {
  as_tibble(set_names(rep(list(character()), length(panel_spec_columns)), panel_spec_columns))
}

empty_figure_layout <- function() {
  as_tibble(set_names(rep(list(character()), length(figure_layout_columns)), figure_layout_columns))
}

# ---- Reading and writing tables ----------------------------------------------

# All tables are read as text; numbers are converted only where they are used.
read_text_table <- function(table_path) {
  read_tsv(table_path, col_types = cols(.default = col_character()), na = c("", "NA"), progress = FALSE)
}

read_panel_specs <- function(panel_specs_path) {
  if (!file.exists(panel_specs_path)) {
    return(empty_panel_specs())
  }
  panel_specs <- read_text_table(panel_specs_path)
  missing_columns <- setdiff(panel_spec_columns, names(panel_specs))
  for (missing_column in missing_columns) panel_specs[[missing_column]] <- NA_character_
  duplicated_plot_ids <- unique(panel_specs$plot_id[duplicated(panel_specs$plot_id)])
  if (length(duplicated_plot_ids) > 0) {
    stop("panel_specs.tsv lists these plot_id values more than once: ",
         paste(duplicated_plot_ids, collapse = ", "), call. = FALSE)
  }
  panel_specs |> dplyr::select(all_of(panel_spec_columns), everything())
}

read_figure_layout <- function(figure_layout_path) {
  if (!file.exists(figure_layout_path)) {
    return(empty_figure_layout())
  }
  read_text_table(figure_layout_path)
}

write_panel_specs <- function(panel_specs, panel_specs_path) {
  write_tsv(panel_specs, panel_specs_path, na = "")
  invisible(panel_specs_path)
}

write_figure_layout <- function(figure_layout, figure_layout_path) {
  write_tsv(figure_layout, figure_layout_path, na = "")
  invisible(figure_layout_path)
}

# project_settings.tsv has two columns: setting, value.
read_project_settings <- function(project_settings_path) {
  settings_table <- read_text_table(project_settings_path)
  as.list(set_names(settings_table$value, settings_table$setting))
}

read_journal_presets <- function(journal_presets_path) {
  read_text_table(journal_presets_path)
}

select_journal_preset <- function(journal_presets, journal_id) {
  selected_preset <- dplyr::filter(journal_presets, .data$journal_id == .env$journal_id)
  if (nrow(selected_preset) != 1) {
    stop("Journal '", journal_id, "' is not in journal_presets.tsv. Available: ",
         paste(journal_presets$journal_id, collapse = ", "), call. = FALSE)
  }
  as.list(selected_preset)
}

# ---- One line width for every figure -----------------------------------------

# The project's line width in pt: project_settings line_width_pt if set,
# otherwise the journal preset's default_line_pt.
project_line_width_pt <- function(project_settings, journal_preset) {
  project_value <- suppressWarnings(as.numeric(project_settings$line_width_pt))
  if (length(project_value) == 1 && !is.na(project_value)) {
    return(project_value)
  }
  as.numeric(journal_preset$default_line_pt)
}

# Round 0: writes the project line width into axis_line_pt, grid_line_pt and
# line_width_pt of every ggplot2 row that has no value yet, so all figures
# share one line width. Values already in the table (set by the user) are
# kept. Lines a plot does not draw (e.g. no grid) stay absent.
apply_project_line_widths <- function(panel_specs, line_width_pt) {
  is_ggplot_row <- coalesce(na_if(panel_specs$plot_kind, ""), "ggplot") == "ggplot"
  for (column_name in c("axis_line_pt", "grid_line_pt", "line_width_pt")) {
    is_empty <- is.na(panel_specs[[column_name]]) | panel_specs[[column_name]] == ""
    panel_specs[[column_name]][is_ggplot_row & is_empty] <- format(line_width_pt)
  }
  panel_specs
}

# ---- Looking up one value ----------------------------------------------------

# Returns the value of `field_name` for `plot_id`, or `default` when the table
# has no row for the plot or the cell is empty. The type follows `default`:
# numeric default -> numeric value; otherwise text.
lookup_panel_spec <- function(panel_specs, plot_id, field_name, default) {
  if (is.null(plot_id) || !field_name %in% names(panel_specs)) {
    return(default)
  }
  matching_values <- panel_specs[[field_name]][panel_specs$plot_id == plot_id]
  if (length(matching_values) == 0 || is.na(matching_values[[1]]) || matching_values[[1]] == "") {
    return(default)
  }
  spec_value <- matching_values[[1]]
  if (is.numeric(default)) {
    numeric_value <- suppressWarnings(as.numeric(spec_value))
    if (is.na(numeric_value)) {
      stop("panel_specs.tsv: '", field_name, "' for '", plot_id, "' must be a number, found '",
           spec_value, "'.", call. = FALSE)
    }
    return(numeric_value)
  }
  if (identical(spec_value, "NA")) NA else spec_value
}

# ---- Which figure a plot belongs to ------------------------------------------

# Figure name from the panel_specs "figure" column, otherwise from the chunk
# label using the project's figure_label_pattern (first capture group is the
# figure number, e.g. "fig-3a-umap" -> "3" -> "Figure3"; "fig-s2b" -> "FigureS2").
# Returns NA when the plot is not part of any figure.
figure_for_plot <- function(plot_id, panel_specs, figure_label_pattern) {
  figure_from_table <- lookup_panel_spec(panel_specs, plot_id, "figure", default = NA_character_)
  if (!is.na(figure_from_table)) {
    return(figure_from_table)
  }
  if (is.null(figure_label_pattern) || is.na(figure_label_pattern) || figure_label_pattern == "") {
    return(NA_character_)
  }
  figure_number <- str_match(plot_id, regex(figure_label_pattern, ignore_case = TRUE))[, 2]
  if (is.na(figure_number)) NA_character_ else paste0("Figure", toupper(figure_number))
}
