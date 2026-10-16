# Figure assembly toolkit: reading canvases, building canvases, exporting
#
# A canvas is an Inkscape SVG page with four layers (bottom to top):
#   plots         - R plots, generated, locked (never edit by hand)
#   panel_letters - generated from panel_specs.tsv and the journal preset
#   annotations   - your freehand content (arrows, schematics, images, labels)
#   boxes         - one box per plot plus plain-language notes (working canvases only)
#
# Working canvases:  figure_assembly/drafting/<Figure>/round<N>_working.svg
# Journal figures:   <final_output_folder>/<Figure>_journal.svg and .pdf
# Your own canvas is never overwritten: every build writes a new file.

library(tidyverse) # loaded here too: toolkit files are sourced in alphabetical order
library(xml2)

figure_assembly_namespace <- "https://github.com/figure-assembly"
position_tolerance_mm <- 0.05

# ---- Box notes ---------------------------------------------------------------

# Setting names that can be written in a box note ("axis text: 7 pt") and the
# panel_specs.tsv column each one sets, per plot kind. A box shows only the
# settings that have a value; any other line is a request for Claude to read.
box_note_settings <- tribble(
  ~setting_name,        ~column_name,          ~value_type, ~plot_kind,
  "title",              "title_pt",            "pt",        "ggplot",
  "subtitle",           "subtitle_pt",         "pt",        "ggplot",
  "caption",            "caption_pt",          "pt",        "ggplot",
  "axis title",         "axis_title_pt",       "pt",        "ggplot",
  "axis text",          "axis_text_pt",        "pt",        "ggplot",
  "axis text style",    "axis_text_face",      "face",      "ggplot",
  "x axis text angle",  "x_text_angle",        "number",    "ggplot",
  "legend title",       "legend_title_pt",     "pt",        "ggplot",
  "legend text",        "legend_text_pt",      "pt",        "ggplot",
  "legend keys",        "legend_key_mm",       "mm",        "ggplot",
  "strip text",         "strip_text_pt",       "pt",        "ggplot",
  "labels",             "label_text_pt",       "pt",        "ggplot",
  "axis lines",         "axis_line_pt",        "pt",        "ggplot",
  "grid lines",         "grid_line_pt",        "pt",        "ggplot",
  "lines",              "line_width_pt",       "pt",        "ggplot",
  "points",             "point_size_ggplot",   "number",    "ggplot",
  "mapped point sizes", "mapped_size_factor",  "number",    "ggplot",
  "move to annotations", "move_to_annotations", "moved",    "ggplot",
  "transparency",       "alpha",               "number",    "ggplot",
  "legend",             "legend_position",     "legend",    "ggplot",
  "title text",         "title_text",          "text",      "ggplot",
  "subtitle text",      "subtitle_text",       "text",      "ggplot",
  "caption text",       "caption_text",        "text",      "ggplot",
  "x axis title text",  "x_title_text",        "text",      "ggplot",
  "y axis title text",  "y_title_text",        "text",      "ggplot",
  "heatmap text",       "heatmap_font_pt",     "pt",        "pheatmap",
  "row labels",         "heatmap_row_font_pt", "pt",        "pheatmap",
  "column labels",      "heatmap_col_font_pt", "pt",        "pheatmap",
  "cell borders",       "heatmap_cell_border", "colour",    "pheatmap",
  "title",              "title_pt",            "pt",        "euler",
  "labels",             "euler_label_pt",      "pt",        "euler",
  "numbers",            "euler_quantity_pt",   "pt",        "euler",
  "move to annotations", "move_to_annotations", "moved",    "euler",
  "text scale",         "upset_text_scale",    "number",    "upset"
)

plot_kind_of <- function(panel_row) {
  coalesce(na_if(panel_row$plot_kind, ""), "ggplot")
}

format_setting_value <- function(stored_value, value_type) {
  switch(value_type,
    "pt" = paste(stored_value, "pt"),
    "mm" = paste(stored_value, "mm"),
    "colour" = if (identical(stored_value, "NA")) "none" else stored_value,
    stored_value
  )
}

# Note lines for a box: the plot, every setting that has a value, then notes.
format_box_note_lines <- function(panel_row) {
  kind_settings <- dplyr::filter(box_note_settings, plot_kind == plot_kind_of(panel_row))
  setting_lines <- character()
  for (setting_index in seq_len(nrow(kind_settings))) {
    stored_value <- panel_row[[kind_settings$column_name[setting_index]]]
    if (!is.na(stored_value) && stored_value != "") {
      setting_lines <- c(setting_lines, paste0(kind_settings$setting_name[setting_index], ": ",
        format_setting_value(stored_value, kind_settings$value_type[setting_index])))
    }
  }
  note_text <- coalesce(panel_row$notes, "")
  c(paste("plot:", panel_row$plot_id), setting_lines, paste("notes:", note_text))
}

# Reads a value written in a note. Returns list(value, problem); value NA
# means "not set". "–" or an empty value clears a setting.
parse_setting_value <- function(written_value, value_type) {
  cleaned_value <- str_trim(written_value)
  if (cleaned_value %in% c("", "-", "–", "—")) {
    return(list(value = NA_character_, problem = NA_character_))
  }
  if (value_type %in% c("pt", "mm", "number")) {
    number_text <- str_trim(str_remove(cleaned_value, "(?i)\\s*(pt|mm)$"))
    if (is.na(suppressWarnings(as.numeric(number_text)))) {
      return(list(value = NA_character_, problem = paste0("not a number: '", cleaned_value, "'")))
    }
    return(list(value = format(as.numeric(number_text)), problem = NA_character_))
  }
  if (value_type == "legend") {
    legend_value <- tolower(cleaned_value)
    if (!legend_value %in% c("none", "right", "left", "top", "bottom", "inside")) {
      return(list(value = NA_character_, problem = paste0("legend must be none, right, left, top, bottom or inside: '",
                                                          cleaned_value, "'")))
    }
    return(list(value = legend_value, problem = NA_character_))
  }
  if (value_type == "colour" && tolower(cleaned_value) == "none") {
    return(list(value = "NA", problem = NA_character_))
  }
  if (value_type == "face") {
    face_value <- str_replace_all(tolower(cleaned_value), "[ -]", ".")
    if (!face_value %in% c("plain", "bold", "italic", "bold.italic")) {
      return(list(value = NA_character_, problem = paste0("style must be plain, bold, italic or bold italic: '",
                                                          cleaned_value, "'")))
    }
    return(list(value = face_value, problem = NA_character_))
  }
  if (value_type == "moved") {
    moved_values <- parse_moved_elements(cleaned_value)
    known_values <- unique(unlist(moved_element_names))
    unknown_values <- setdiff(moved_values, known_values)
    if (length(unknown_values) > 0) {
      return(list(value = NA_character_, problem = paste0("can move only ", paste(known_values, collapse = ", "),
                                                          "; not: ", paste(unknown_values, collapse = ", "))))
    }
    return(list(value = paste(moved_values, collapse = ", "), problem = NA_character_))
  }
  list(value = cleaned_value, problem = NA_character_)
}

# Splits a box note into recognised settings (applied by code) and free text
# (read by Claude). Lines written by toolkit version 0.1.0 starting with
# "round 0:" were layout explanations and are ignored.
parse_box_note <- function(note_text, plot_kind) {
  kind_settings <- dplyr::filter(box_note_settings, .data$plot_kind == .env$plot_kind)
  note_lines <- str_trim(str_split_1(coalesce(note_text, ""), "\n"))
  note_lines <- note_lines[note_lines != ""]
  settings <- tibble(column_name = character(), value = character(), problem = character(), line = character())
  free_text <- character()
  for (note_line in note_lines) {
    if (str_detect(note_line, "(?i)^(plot|round 0)\\s*:")) next
    line_parts <- str_match(note_line, "^([^:]+):\\s*(.*)$")
    setting_name <- tolower(str_squish(coalesce(line_parts[1, 2], "")))
    if (setting_name == "notes") {
      if (str_trim(line_parts[1, 3]) != "") free_text <- c(free_text, str_trim(line_parts[1, 3]))
      next
    }
    matched_setting <- dplyr::filter(kind_settings, setting_name == .env$setting_name)
    if (nrow(matched_setting) == 1) {
      parsed_value <- parse_setting_value(line_parts[1, 3], matched_setting$value_type)
      settings <- bind_rows(settings, tibble(column_name = matched_setting$column_name, value = parsed_value$value,
                                             problem = parsed_value$problem, line = note_line))
    } else {
      free_text <- c(free_text, note_line) # unknown names are requests for Claude
    }
  }
  list(settings = settings, free_text = paste(free_text, collapse = " | "))
}

# ---- Paths -------------------------------------------------------------------

figure_drafting_directory <- function(assembly_directory, figure_name) {
  file.path(assembly_directory, "drafting", figure_name)
}

list_working_canvases <- function(assembly_directory, figure_name) {
  canvas_paths <- list.files(figure_drafting_directory(assembly_directory, figure_name),
                             pattern = "^round[0-9]+_working\\.svg$", full.names = TRUE)
  round_numbers <- as.integer(str_match(basename(canvas_paths), "^round([0-9]+)_")[, 2])
  canvas_paths[order(round_numbers)]
}

latest_working_canvas <- function(assembly_directory, figure_name) {
  canvas_paths <- list_working_canvases(assembly_directory, figure_name)
  if (length(canvas_paths) == 0) NA_character_ else canvas_paths[[length(canvas_paths)]]
}

next_working_canvas_path <- function(assembly_directory, figure_name) {
  canvas_paths <- list_working_canvases(assembly_directory, figure_name)
  next_round <- if (length(canvas_paths) == 0) {
    0L
  } else {
    max(as.integer(str_match(basename(canvas_paths), "^round([0-9]+)_")[, 2])) + 1L
  }
  file.path(figure_drafting_directory(assembly_directory, figure_name),
            sprintf("round%d_working.svg", next_round))
}

# knitr names plot files <fig.path><chunk label>-<n>.svg; the first plot is used.
plot_file_path <- function(assembly_directory, figure_name, plot_id, draft_mode) {
  plot_folder <- if (draft_mode) "plots" else "final_plots"
  file.path(figure_drafting_directory(assembly_directory, figure_name), plot_folder, paste0(plot_id, "-1.svg"))
}

# ---- Reading a canvas --------------------------------------------------------

# Boxes and their notes from the "boxes" layer. Each text belongs to the
# smallest box that contains its anchor point. Generated boxes carry the id
# "box__<plot_id>"; boxes you draw yourself are matched through their note
# ("plot: <name>") by Claude or by you.
read_canvas_boxes <- function(canvas_path, boxes_layer_label = "boxes") {
  canvas_document <- read_xml(canvas_path)
  page_geometry <- read_svg_page_geometry(canvas_document)
  boxes_layer <- find_layer(canvas_document, boxes_layer_label)
  empty_result <- tibble(box_element_id = character(), plot_id = character(), x_mm = double(),
                         y_mm = double(), width_mm = double(), height_mm = double(), note_text = character())
  if (inherits(boxes_layer, "xml_missing")) {
    return(empty_result)
  }
  # Note cards (white background behind notes) are not boxes.
  rectangle_nodes <- xml_find_all(boxes_layer, ".//*[local-name()='rect'][not(starts-with(@id, 'notecard__'))]")
  boxes <- map(rectangle_nodes, \(rectangle_node) {
    rectangle_geometry <- transformed_rectangle_mm(
      as.numeric(coalesce(xml_attr(rectangle_node, "x"), "0")),
      as.numeric(coalesce(xml_attr(rectangle_node, "y"), "0")),
      as.numeric(xml_attr(rectangle_node, "width")),
      as.numeric(xml_attr(rectangle_node, "height")),
      cumulative_transform(rectangle_node), page_geometry$millimetres_per_user_unit
    )
    element_id <- coalesce(xml_attr(rectangle_node, "id"), NA_character_)
    rectangle_geometry |>
      mutate(box_element_id = element_id,
             plot_id = if_else(str_starts(element_id, "box__"), str_remove(element_id, "^box__"), NA_character_),
             .before = 1)
  }) |>
    list_rbind()
  if (nrow(boxes) == 0) {
    return(empty_result)
  }

  text_nodes <- xml_find_all(boxes_layer, ".//*[local-name()='text']")
  notes <- map(text_nodes, \(text_node) {
    anchor_point <- text_anchor_mm(text_node, page_geometry$millimetres_per_user_unit)
    tibble(anchor_x_mm = anchor_point[["x_mm"]], anchor_y_mm = anchor_point[["y_mm"]],
           note_text = paste(str_trim(read_text_lines(text_node)), collapse = "\n"))
  }) |>
    list_rbind()

  boxes$note_text <- NA_character_
  if (nrow(notes) > 0) {
    for (note_index in seq_len(nrow(notes))) {
      containing_boxes <- which(
        notes$anchor_x_mm[note_index] >= boxes$x_mm - position_tolerance_mm &
          notes$anchor_x_mm[note_index] <= boxes$x_mm + boxes$width_mm + position_tolerance_mm &
          notes$anchor_y_mm[note_index] >= boxes$y_mm - position_tolerance_mm &
          notes$anchor_y_mm[note_index] <= boxes$y_mm + boxes$height_mm + position_tolerance_mm
      )
      if (length(containing_boxes) == 0) {
        # Fall back to the box named in the note's "plot: <name>" line.
        named_plot <- str_match(notes$note_text[note_index], "(?im)^\\s*plot:\\s*(\\S+)")[, 2]
        containing_boxes <- which(!is.na(named_plot) & boxes$plot_id == named_plot)
      }
      if (length(containing_boxes) == 0) {
        warning("A note is not inside any box and names no known plot; it was ignored: \"",
                notes$note_text[note_index], "\"", call. = FALSE)
        next
      }
      smallest_box <- containing_boxes[which.min(boxes$width_mm[containing_boxes] * boxes$height_mm[containing_boxes])]
      boxes$note_text[smallest_box] <- paste(na.omit(c(boxes$note_text[smallest_box], notes$note_text[note_index])),
                                             collapse = "\n")
    }
  }
  # A "plot: <name>" line in the note names the plot for boxes you drew yourself.
  named_in_note <- str_match(coalesce(boxes$note_text, ""), "(?im)^\\s*plot:\\s*(\\S+)")[, 2]
  boxes$plot_id <- coalesce(boxes$plot_id, named_in_note)
  boxes
}

# Current page-millimetre position of each generated plot group (moves you
# made to a plot directly, instead of to its box, are detected here).
read_canvas_plot_positions <- function(canvas_path) {
  canvas_document <- read_xml(canvas_path)
  page_geometry <- read_svg_page_geometry(canvas_document)
  plot_groups <- xml_find_all(canvas_document, "//*[local-name()='g'][starts-with(@id, 'plot__')]")
  map(plot_groups, \(plot_group) {
    group_attributes <- xml_attrs(plot_group)
    transformed_rectangle_mm(0, 0, as.numeric(group_attributes[["local-width"]]),
                             as.numeric(group_attributes[["local-height"]]),
                             cumulative_transform(plot_group), page_geometry$millimetres_per_user_unit) |>
      mutate(plot_id = str_remove(group_attributes[["id"]], "^plot__"), .before = 1) |>
      mutate(placed_x_mm = as.numeric(group_attributes[["placed-x-mm"]]),
             placed_y_mm = as.numeric(group_attributes[["placed-y-mm"]]),
             placed_width_mm = as.numeric(group_attributes[["placed-width-mm"]]),
             placed_height_mm = as.numeric(group_attributes[["placed-height-mm"]]))
  }) |>
    list_rbind()
}

# Notes are compared after joining lines and dropping the "plot: <name>" line.
normalise_note <- function(note_text) {
  note_lines <- str_trim(str_split_1(coalesce(note_text, ""), "\n"))
  note_lines <- note_lines[note_lines != "" & !str_detect(note_lines, "(?i)^plot:")]
  paste(note_lines, collapse = " | ")
}

# Differences between a canvas and the tables. A change to a box wins over a
# change to its plot; if both moved to different places, the box is used and
# the conflict is reported.
compare_canvas_with_tables <- function(canvas_path, panel_specs, figure_layout, figure_name) {
  canvas_boxes <- read_canvas_boxes(canvas_path)
  plot_positions <- read_canvas_plot_positions(canvas_path)
  geometry_fields <- c("x_mm", "y_mm", "width_mm", "height_mm")

  matched_boxes <- dplyr::filter(canvas_boxes, !is.na(plot_id), plot_id %in% panel_specs$plot_id)
  unmatched_boxes <- dplyr::filter(canvas_boxes, is.na(plot_id) | !plot_id %in% panel_specs$plot_id)

  differences <- map(seq_len(nrow(matched_boxes)), \(box_index) {
    canvas_box <- matched_boxes[box_index, ]
    plot_position <- dplyr::filter(plot_positions, plot_id == canvas_box$plot_id)
    field_differences <- map(geometry_fields, \(field_name) {
      table_value <- lookup_panel_spec(panel_specs, canvas_box$plot_id, field_name, default = NA_real_)
      box_value <- canvas_box[[field_name]]
      plot_value <- if (nrow(plot_position) == 1) plot_position[[field_name]] else NA_real_
      placed_value <- if (nrow(plot_position) == 1) plot_position[[paste0("placed_", field_name)]] else NA_real_
      box_changed <- is.na(table_value) || abs(box_value - table_value) > position_tolerance_mm
      plot_changed <- !is.na(plot_value) && !is.na(placed_value) &&
        abs(plot_value - placed_value) > position_tolerance_mm &&
        (is.na(table_value) || abs(plot_value - table_value) > position_tolerance_mm)
      if (box_changed) {
        source_description <- if (plot_changed && abs(plot_value - box_value) > position_tolerance_mm) {
          "box (plot was also moved, to a different place)"
        } else {
          "box"
        }
        tibble(plot_id = canvas_box$plot_id, field = field_name, table_value = as.character(table_value),
               canvas_value = as.character(round(box_value, 2)), source = source_description)
      } else if (plot_changed) {
        tibble(plot_id = canvas_box$plot_id, field = field_name, table_value = as.character(table_value),
               canvas_value = as.character(round(plot_value, 2)), source = "plot")
      } else {
        NULL
      }
    }) |>
      list_rbind()
    panel_row <- dplyr::filter(panel_specs, plot_id == canvas_box$plot_id)
    parsed_note <- parse_box_note(canvas_box$note_text, plot_kind_of(panel_row))
    # A setting line changes the table only when it is present in the note;
    # removing a line by accident changes nothing (write "–" to clear a value).
    for (setting_index in seq_len(nrow(parsed_note$settings))) {
      setting_row <- parsed_note$settings[setting_index, ]
      table_value <- panel_row[[setting_row$column_name]]
      if (!is.na(setting_row$problem)) {
        field_differences <- bind_rows(field_differences, tibble(
          plot_id = canvas_box$plot_id, field = setting_row$column_name, table_value = table_value,
          canvas_value = NA_character_, source = paste0("box setting NOT applied, ", setting_row$problem)
        ))
        next
      }
      values_match <- identical(coalesce(table_value, ""), coalesce(setting_row$value, "")) ||
        isTRUE(abs(suppressWarnings(as.numeric(table_value)) - suppressWarnings(as.numeric(setting_row$value))) < 1e-9)
      if (!values_match) {
        field_differences <- bind_rows(field_differences, tibble(
          plot_id = canvas_box$plot_id, field = setting_row$column_name, table_value = table_value,
          canvas_value = setting_row$value, source = "box setting"
        ))
      }
    }
    table_note <- normalise_note(coalesce(panel_row$notes, ""))
    canvas_note <- normalise_note(str_replace_all(parsed_note$free_text, " \\| ", "\n"))
    if (canvas_note != table_note) {
      field_differences <- bind_rows(field_differences, tibble(
        plot_id = canvas_box$plot_id, field = "notes", table_value = table_note,
        canvas_value = canvas_note, source = "box note (for Claude to translate)"
      ))
    }
    field_differences
  }) |>
    list_rbind()
  if (nrow(differences) == 0) {
    differences <- tibble(plot_id = character(), field = character(), table_value = character(),
                          canvas_value = character(), source = character())
  }

  page_geometry <- read_svg_page_geometry(read_xml(canvas_path))
  table_page_width <- as.numeric(figure_layout$page_width_mm[figure_layout$figure == figure_name])
  table_page_height <- as.numeric(figure_layout$page_height_mm[figure_layout$figure == figure_name])
  page_differences <- tibble(
    figure = figure_name, field = c("page_width_mm", "page_height_mm"),
    table_value = c(table_page_width[1], table_page_height[1]),
    canvas_value = round(c(page_geometry$page_width_mm, page_geometry$page_height_mm), 2)
  ) |>
    dplyr::filter(is.na(table_value) | abs(canvas_value - table_value) > position_tolerance_mm)

  list(canvas_path = canvas_path, differences = differences,
       unmatched_boxes = unmatched_boxes, page_differences = page_differences)
}

has_canvas_differences <- function(canvas_comparison) {
  nrow(canvas_comparison$differences) > 0 || nrow(canvas_comparison$unmatched_boxes) > 0 ||
    nrow(canvas_comparison$page_differences) > 0
}

format_canvas_comparison <- function(canvas_comparison) {
  report_lines <- c(sprintf("Canvas and tables disagree (%s):", canvas_comparison$canvas_path))
  if (nrow(canvas_comparison$differences) > 0) {
    report_lines <- c(report_lines, sprintf("  %s  %s  table: %s  canvas: %s  (changed on the canvas: %s)",
      canvas_comparison$differences$plot_id, canvas_comparison$differences$field,
      canvas_comparison$differences$table_value, canvas_comparison$differences$canvas_value,
      canvas_comparison$differences$source))
  }
  if (nrow(canvas_comparison$page_differences) > 0) {
    report_lines <- c(report_lines, sprintf("  page  %s  table: %s  canvas: %s",
      canvas_comparison$page_differences$field, canvas_comparison$page_differences$table_value,
      canvas_comparison$page_differences$canvas_value))
  }
  if (nrow(canvas_comparison$unmatched_boxes) > 0) {
    report_lines <- c(report_lines, sprintf("  box not linked to a known plot: %s, note: \"%s\"",
      canvas_comparison$unmatched_boxes$box_element_id,
      str_replace_all(coalesce(canvas_comparison$unmatched_boxes$note_text, ""), "\n", " | ")))
  }
  paste(report_lines, collapse = "\n")
}

# Used by start_figure_assembly(draft_mode = FALSE): nothing is chosen silently.
stop_if_canvases_disagree_with_tables <- function(assembly_directory, panel_specs, figure_layout,
                                                  on_canvas_mismatch) {
  figure_names <- unique(na.omit(panel_specs$figure))
  for (figure_name in figure_names) {
    canvas_path <- latest_working_canvas(assembly_directory, figure_name)
    if (is.na(canvas_path)) {
      message(figure_name, ": no working canvas found; the tables are used as they are.")
      next
    }
    canvas_comparison <- compare_canvas_with_tables(
      canvas_path, dplyr::filter(panel_specs, figure == figure_name), figure_layout, figure_name
    )
    if (has_canvas_differences(canvas_comparison)) {
      comparison_report <- format_canvas_comparison(canvas_comparison)
      if (on_canvas_mismatch == "stop") {
        stop(comparison_report,
             "\nNothing was rendered. Choose one: update the tables from the canvas (ask Claude, or use ",
             "update_tables_from_canvas()), or keep the tables by calling ",
             "start_figure_assembly(draft_mode = FALSE, on_canvas_mismatch = \"use_table\").",
             call. = FALSE)
      }
      message(comparison_report, "\nUsing the tables, as requested (on_canvas_mismatch = \"use_table\").")
    }
  }
  invisible(TRUE)
}

# Applies the geometry and note differences of a canvas comparison to the
# tables and returns the updated tables (nothing is written to disk here).
update_tables_from_canvas <- function(canvas_comparison, panel_specs, figure_layout, figure_name) {
  applicable_differences <- dplyr::filter(canvas_comparison$differences, !str_starts(source, "box setting NOT applied"))
  for (difference_index in seq_len(nrow(applicable_differences))) {
    difference_row <- applicable_differences[difference_index, ]
    panel_specs[[difference_row$field]][panel_specs$plot_id == difference_row$plot_id] <- difference_row$canvas_value
  }
  for (page_index in seq_len(nrow(canvas_comparison$page_differences))) {
    page_row <- canvas_comparison$page_differences[page_index, ]
    if (!figure_name %in% figure_layout$figure) {
      figure_layout <- bind_rows(figure_layout, tibble(figure = figure_name))
    }
    figure_layout[[page_row$field]][figure_layout$figure == figure_name] <- as.character(page_row$canvas_value)
  }
  list(panel_specs = panel_specs, figure_layout = figure_layout)
}

# Copies the annotations layer of a canvas into the permanent annotations file.
extract_annotations_from_canvas <- function(canvas_path, annotations_path) {
  canvas_document <- read_xml(canvas_path)
  page_geometry <- read_svg_page_geometry(canvas_document)
  annotations_document <- new_canvas_document(page_geometry$page_width_mm, page_geometry$page_height_mm,
                                              layer_labels = "annotations")
  source_layer <- find_layer(canvas_document, "annotations")
  target_layer <- find_layer(annotations_document, "annotations")
  if (!inherits(source_layer, "xml_missing")) {
    copy_layer_contents(source_layer, target_layer)
  }
  # Empty text elements (no characters, e.g. a text frame created by an
  # accidental drag) are invisible; drop them so they do not fail the checks.
  text_nodes <- xml_find_all(target_layer, ".//*[local-name()='text']")
  empty_text_nodes <- text_nodes[str_trim(xml_text(text_nodes)) == ""]
  if (length(empty_text_nodes) > 0) {
    message("Removed ", length(empty_text_nodes), " empty text element(s) from the annotations: ",
            paste(coalesce(xml_attr(empty_text_nodes, "id"), "(no id)"), collapse = ", "))
    xml_remove(empty_text_nodes)
  }
  dir.create(dirname(annotations_path), recursive = TRUE, showWarnings = FALSE)
  write_xml(annotations_document, annotations_path, options = "as_xml")
  invisible(annotations_path)
}

# ---- Building a canvas -------------------------------------------------------

new_canvas_document <- function(page_width_mm, page_height_mm, layer_labels) {
  layer_markup <- paste0(sprintf(
    '<g inkscape:groupmode="layer" inkscape:label="%s" id="layer__%s"/>', layer_labels, layer_labels
  ), collapse = "\n  ")
  read_xml(sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
      xmlns:inkscape="%s" xmlns:sodipodi="%s" xmlns:figassembly="%s"
      width="%smm" height="%smm" viewBox="0 0 %s %s" version="1.1">
  <sodipodi:namedview id="namedview" inkscape:document-units="mm" units="mm"/>
  %s
</svg>',
    inkscape_namespace, sodipodi_namespace, figure_assembly_namespace,
    format(page_width_mm, nsmall = 2), format(page_height_mm, nsmall = 2),
    format(page_width_mm, nsmall = 2), format(page_height_mm, nsmall = 2), layer_markup
  ))
}

copy_layer_contents <- function(source_layer, target_layer) {
  source_transform <- xml_attr(source_layer, "transform")
  if (!is.na(source_transform)) xml_set_attr(target_layer, "transform", source_transform)
  for (child_node in xml_children(source_layer)) xml_add_child(target_layer, child_node)
  invisible(target_layer)
}

# svglite keeps default styling ("lines have no fill") in a <style> block that
# some programs ignore. Write those defaults onto every element instead.
inline_svglite_default_styles <- function(svglite_group) {
  shape_nodes <- xml_find_all(svglite_group, paste0(
    ".//*[local-name()='line' or local-name()='polyline' or local-name()='polygon' or ",
    "local-name()='path' or local-name()='rect' or local-name()='circle']"
  ))
  for (shape_node in shape_nodes) {
    if (identical(xml_attr(xml_parent(shape_node), "class"), "glyphgroup")) next
    style_text <- coalesce(xml_attr(shape_node, "style"), "")
    default_properties <- c("fill" = "none", "stroke" = "#000000", "stroke-linecap" = "round",
                            "stroke-linejoin" = "round", "stroke-miterlimit" = "10")
    for (property_name in names(default_properties)) {
      if (!str_detect(style_text, paste0("(^|;)\\s*", property_name, "\\s*:")) &&
          is.na(xml_attr(shape_node, property_name))) {
        style_text <- paste0(style_text, if (style_text != "" && !str_ends(str_trim(style_text), ";")) "; " else " ",
                             property_name, ": ", default_properties[[property_name]], ";")
      }
    }
    xml_set_attr(shape_node, "style", str_trim(style_text))
  }
  xml_remove(xml_find_all(svglite_group, ".//*[local-name()='style']"))
  invisible(svglite_group)
}

# Gives every id inside a plot a prefix so ids never clash between plots, and
# updates the references to them.
prefix_svg_ids <- function(svg_node, id_prefix) {
  nodes_with_id <- xml_find_all(svg_node, ".//*[@id]")
  original_ids <- xml_attr(nodes_with_id, "id")
  if (length(original_ids) == 0) {
    return(invisible(svg_node))
  }
  new_ids <- paste0(id_prefix, original_ids)
  xml_set_attr(nodes_with_id, "id", new_ids)
  for (descendant_node in xml_find_all(svg_node, ".//*")) {
    node_attributes <- xml_attrs(descendant_node)
    for (attribute_name in names(node_attributes)) {
      attribute_value <- node_attributes[[attribute_name]]
      if (!str_detect(attribute_value, fixed("#"))) next
      updated_value <- attribute_value
      for (id_index in seq_along(original_ids)) {
        updated_value <- str_replace_all(updated_value, fixed(paste0("url(#", original_ids[id_index], ")")),
                                         paste0("url(#", new_ids[id_index], ")"))
        if (updated_value == paste0("#", original_ids[id_index])) updated_value <- paste0("#", new_ids[id_index])
      }
      if (updated_value != attribute_value) {
        attribute_name_with_prefix <- if (attribute_name == "href" && !is.na(xml_attr(descendant_node, "xlink:href",
            ns = c(xlink = "http://www.w3.org/1999/xlink")))) "xlink:href" else attribute_name
        xml_set_attr(descendant_node, attribute_name_with_prefix, updated_value)
      }
    }
  }
  invisible(svg_node)
}

# Places one svglite plot file on the plots layer, at native size: the only
# scaling is the unit conversion from the plot's points to page millimetres.
# Inserts the content of an svglite file as one group at (x_mm, y_mm), at its
# native size (unit conversion only). Returns the group and the file's size.
insert_svglite_content <- function(parent_node, svg_path, group_id, group_label, x_mm, y_mm) {
  svg_document <- read_xml(svg_path)
  svg_geometry <- read_svg_page_geometry(svg_document)
  svglite_group <- xml_find_first(svg_document, "/*/*[local-name()='g'][@class='svglite']")
  if (inherits(svglite_group, "xml_missing")) {
    stop(svg_path, " was not written by svglite (no svglite group found).", call. = FALSE)
  }
  xml_remove(xml_find_all(svglite_group, "./*[local-name()='rect'][@width='100%']"))
  inline_svglite_default_styles(svglite_group)
  prefix_svg_ids(svglite_group, paste0(group_id, "__"))
  new_group <- xml_add_child(parent_node, "g")
  xml_set_attrs(new_group, c(
    "id" = group_id, "inkscape:label" = group_label,
    "transform" = sprintf("translate(%.4f,%.4f) scale(%.6f)", x_mm, y_mm, svg_geometry$millimetres_per_user_unit)
  ))
  xml_add_child(new_group, svglite_group)
  list(group = new_group, width_mm = svg_geometry$page_width_mm, height_mm = svg_geometry$page_height_mm)
}

# Elements of a plot moved to annotations (legend, facet labels, Euler set
# names) are added to the annotations layer once, inside the plot's box: the
# legend at the top right, words stacked at the top left. They are never added
# again while an element with the same id exists, so they become yours to move
# and edit.
add_moved_elements <- function(annotations_layer, panel_row, plot_folder_path) {
  moved_elements <- parse_moved_elements(panel_row$move_to_annotations)
  if (length(moved_elements) == 0) {
    return(invisible(annotations_layer))
  }
  existing_ids <- xml_attr(xml_find_all(annotations_layer, ".//*[@id]"), "id")
  box_x <- as.numeric(panel_row$x_mm)
  box_y <- as.numeric(panel_row$y_mm)
  box_width <- as.numeric(panel_row$width_mm)

  legend_id <- paste0("moved__", panel_row$plot_id, "__legend")
  legend_path <- file.path(plot_folder_path, paste0(panel_row$plot_id, "-moved-legend.svg"))
  if ("legend" %in% moved_elements && !legend_id %in% existing_ids && file.exists(legend_path)) {
    legend_size <- read_svg_page_geometry(read_xml(legend_path))
    insert_svglite_content(annotations_layer, legend_path, legend_id, paste("legend:", panel_row$plot_id),
                           box_x + box_width - legend_size$page_width_mm - 1, box_y + 1)
  }

  labels_path <- file.path(plot_folder_path, paste0(panel_row$plot_id, "-moved-labels.tsv"))
  label_id_prefix <- paste0("moved__", panel_row$plot_id, "__label__")
  if (file.exists(labels_path) && !any(str_starts(existing_ids, label_id_prefix))) {
    # Facet labels can be recorded twice (once per patchwork sub-plot), so repeats
    # are dropped; repeated Euler set names (e.g. "Genes" in three diagrams) are kept.
    moved_labels <- read_tsv(labels_path, col_types = cols(.default = col_character()))
    moved_labels <- moved_labels[!(moved_labels$label_kind == "strip label" & duplicated(moved_labels)), ]
    # Start below the box's note card, so the words are visible on the working canvas.
    note_card_height_mm <- length(format_box_note_lines(panel_row)) * point_to_millimetre(6) * 1.25 + 1.5
    box_y <- box_y + note_card_height_mm
    for (label_index in seq_len(nrow(moved_labels))) {
      label_row <- moved_labels[label_index, ]
      size_column <- if (label_row$label_kind == "set name") "euler_label_pt" else "strip_text_pt"
      label_pt <- coalesce(as.numeric(panel_row[[size_column]]), 6)
      label_size_mm <- point_to_millimetre(label_pt)
      add_text_lines(annotations_layer, label_row$label, box_x + 2,
                     box_y + 3 + label_size_mm + (label_index - 1) * label_size_mm * 1.6,
                     label_size_mm, label_size_mm * 1.25, "font-family:Arial;fill:#000000",
                     element_id = paste0(label_id_prefix, label_index))
    }
  }
  invisible(annotations_layer)
}

add_plot_to_canvas <- function(plots_layer, plot_svg_path, plot_id, x_mm, y_mm, expected_width_mm,
                               expected_height_mm) {
  plot_document <- read_xml(plot_svg_path)
  plot_geometry <- read_svg_page_geometry(plot_document)
  if (!is.na(expected_width_mm) &&
      (abs(plot_geometry$page_width_mm - expected_width_mm) > 0.1 ||
       abs(plot_geometry$page_height_mm - expected_height_mm) > 0.1)) {
    warning(sprintf(paste0("%s: the plot file is %.1f x %.1f mm but the table says %.1f x %.1f mm. ",
                           "It was placed at its real size (never stretched); re-render to update it."),
                    plot_id, plot_geometry$page_width_mm, plot_geometry$page_height_mm,
                    expected_width_mm, expected_height_mm), call. = FALSE)
  }
  svglite_group <- xml_find_first(plot_document, "/*/*[local-name()='g'][@class='svglite']")
  if (inherits(svglite_group, "xml_missing")) {
    stop(plot_svg_path, " was not written by svglite (no svglite group found). ",
         "Check that the chunk uses the svglite device.", call. = FALSE)
  }
  full_page_background <- xml_find_all(svglite_group, "./*[local-name()='rect'][@width='100%']")
  xml_remove(full_page_background)
  inline_svglite_default_styles(svglite_group)
  prefix_svg_ids(svglite_group, paste0(plot_id, "__"))

  view_box_values <- as.numeric(str_split_1(str_trim(xml_attr(xml_root(plot_document), "viewBox")), "[\\s,]+"))
  unit_conversion <- plot_geometry$millimetres_per_user_unit # canvas user unit = 1 mm
  plot_group <- xml_add_child(plots_layer, "g")
  xml_set_attrs(plot_group, c(
    "id" = paste0("plot__", plot_id),
    "inkscape:label" = plot_id,
    "transform" = sprintf("translate(%.4f,%.4f) scale(%.6f)", x_mm, y_mm, unit_conversion),
    "figassembly:local-width" = format(view_box_values[3]),
    "figassembly:local-height" = format(view_box_values[4]),
    # Where the plot was placed when built: a plot counts as moved by hand
    # only if it is no longer here.
    "figassembly:placed-x-mm" = format(x_mm),
    "figassembly:placed-y-mm" = format(y_mm),
    "figassembly:placed-width-mm" = format(plot_geometry$page_width_mm),
    "figassembly:placed-height-mm" = format(plot_geometry$page_height_mm)
  ))
  xml_add_child(plot_group, svglite_group)
  invisible(plot_group)
}

add_text_lines <- function(parent_node, text_lines, x_mm, first_baseline_mm, font_size_mm, line_spacing_mm,
                           style_text, element_id = NULL) {
  text_node <- xml_add_child(parent_node, "text")
  xml_set_attrs(text_node, c("x" = sprintf("%.3f", x_mm), "y" = sprintf("%.3f", first_baseline_mm),
                             "style" = sprintf("font-size:%.4fpx;line-height:%.3f;%s", font_size_mm,
                                               line_spacing_mm / font_size_mm, style_text),
                             # Keep typed spaces (Inkscape's own setting for text). Safe because the
                             # file is written without indentation between the lines.
                             "xml:space" = "preserve"))
  if (!is.null(element_id)) xml_set_attr(text_node, "id", element_id)
  for (line_index in seq_along(text_lines)) {
    line_node <- xml_add_child(text_node, "tspan", text_lines[[line_index]])
    xml_set_attrs(line_node, c(
      "sodipodi:role" = "line", "x" = sprintf("%.3f", x_mm),
      "y" = sprintf("%.3f", first_baseline_mm + (line_index - 1) * line_spacing_mm)
    ))
  }
  invisible(text_node)
}

format_panel_letter <- function(panel_letter, letter_case) {
  if (is.na(panel_letter) || panel_letter == "") {
    return(NA_character_)
  }
  if (identical(tolower(letter_case), "upper")) toupper(panel_letter) else tolower(panel_letter)
}

#' Build a canvas for one figure from the tables, the plot files, and the
#' annotations file.
#'
#' @param canvas_purpose "working" (with the boxes layer, draft plots, written
#'   to drafting/<Figure>/round<N>_working.svg) or "journal" (no boxes, final
#'   plots, written to <final_output_folder>/<Figure>_journal.svg).
#' @return Path of the written SVG.
build_figure_canvas <- function(figure_name, canvas_purpose = c("working", "journal"),
                                assembly_directory = "figure_assembly", output_path = NULL) {
  canvas_purpose <- match.arg(canvas_purpose)
  project_settings <- read_project_settings(file.path(assembly_directory, "settings", "project_settings.tsv"))
  journal_preset <- select_journal_preset(
    read_journal_presets(file.path(assembly_directory, "settings", "journal_presets.tsv")),
    project_settings$journal
  )
  panel_specs <- read_panel_specs(file.path(assembly_directory, "specs", "panel_specs.tsv"))
  figure_layout <- read_figure_layout(file.path(assembly_directory, "specs", "figure_layout.tsv"))
  figure_panels <- dplyr::filter(panel_specs, figure == figure_name)
  if (nrow(figure_panels) == 0) {
    stop("panel_specs.tsv has no rows for ", figure_name, ".", call. = FALSE)
  }
  figure_row <- dplyr::filter(figure_layout, figure == figure_name)
  if (nrow(figure_row) != 1) {
    stop("figure_layout.tsv needs exactly one row for ", figure_name, ".", call. = FALSE)
  }

  if (canvas_purpose == "journal") {
    canvas_path <- latest_working_canvas(assembly_directory, figure_name)
    if (!is.na(canvas_path)) {
      canvas_comparison <- compare_canvas_with_tables(canvas_path, figure_panels, figure_layout, figure_name)
      if (has_canvas_differences(canvas_comparison)) {
        stop(format_canvas_comparison(canvas_comparison),
             "\nThe journal figure was not built. Update the tables from the canvas first, or keep the ",
             "tables and build again after deciding.", call. = FALSE)
      }
    }
  }

  canvas_document <- new_canvas_document(
    as.numeric(figure_row$page_width_mm), as.numeric(figure_row$page_height_mm),
    layer_labels = c("plots", "panel_letters", "annotations", if (canvas_purpose == "working") "boxes")
  )
  plots_layer <- find_layer(canvas_document, "plots")
  xml_set_attr(plots_layer, "sodipodi:insensitive", "true") # locked: move the boxes, not the plots

  for (panel_index in seq_len(nrow(figure_panels))) {
    panel_row <- figure_panels[panel_index, ]
    if (plot_kind_of(panel_row) == "external") next # placed by hand on the annotations layer
    plot_svg_path <- plot_file_path(assembly_directory, figure_name, panel_row$plot_id,
                                    draft_mode = canvas_purpose == "working")
    if (!file.exists(plot_svg_path)) {
      warning(panel_row$plot_id, ": plot file not found (", plot_svg_path, "). Render the figures document ",
              "first; only its box is shown.", call. = FALSE)
      next
    }
    add_plot_to_canvas(plots_layer, plot_svg_path, panel_row$plot_id,
                       as.numeric(panel_row$x_mm), as.numeric(panel_row$y_mm),
                       as.numeric(panel_row$width_mm), as.numeric(panel_row$height_mm))
  }

  letters_layer <- find_layer(canvas_document, "panel_letters")
  letter_size_mm <- point_to_millimetre(as.numeric(journal_preset$panel_label_pt))
  letter_style <- sprintf("font-family:%s;font-weight:%s;fill:#000000",
                          str_split_1(coalesce(journal_preset$font_family, "Arial"), ",")[1],
                          if (isTRUE(as.logical(journal_preset$panel_label_bold))) "bold" else "normal")
  for (panel_index in seq_len(nrow(figure_panels))) {
    panel_row <- figure_panels[panel_index, ]
    letter_text <- format_panel_letter(panel_row$panel_letter, journal_preset$panel_label_case)
    if (is.na(letter_text)) next
    letter_offset_x <- coalesce(as.numeric(panel_row$letter_dx_mm), 0)
    letter_offset_y <- coalesce(as.numeric(panel_row$letter_dy_mm), 0)
    add_text_lines(letters_layer, letter_text,
                   as.numeric(panel_row$x_mm) + letter_offset_x,
                   as.numeric(panel_row$y_mm) + letter_offset_y + letter_size_mm * 0.75,
                   letter_size_mm, letter_size_mm, letter_style,
                   element_id = paste0("letter__", panel_row$plot_id))
  }

  annotations_path <- file.path(assembly_directory, "annotations", paste0(figure_name, "_annotations.svg"))
  annotations_layer <- find_layer(canvas_document, "annotations")
  if (file.exists(annotations_path)) {
    annotations_document <- read_xml(annotations_path)
    source_layer <- find_layer(annotations_document, "annotations")
    if (!inherits(source_layer, "xml_missing")) {
      copy_layer_contents(source_layer, annotations_layer)
    }
  }
  plot_folder_path <- file.path(figure_drafting_directory(assembly_directory, figure_name),
                                if (canvas_purpose == "working") "plots" else "final_plots")
  for (panel_index in seq_len(nrow(figure_panels))) {
    add_moved_elements(annotations_layer, figure_panels[panel_index, ], plot_folder_path)
  }

  if (canvas_purpose == "working") {
    boxes_layer <- find_layer(canvas_document, "boxes")
    note_font_mm <- point_to_millimetre(6)
    for (panel_index in seq_len(nrow(figure_panels))) {
      panel_row <- figure_panels[panel_index, ]
      # Box and note in one group, so they move together in Inkscape.
      box_group <- xml_add_child(boxes_layer, "g")
      xml_set_attrs(box_group, c("id" = paste0("boxgroup__", panel_row$plot_id),
                                 "inkscape:label" = paste("box and note:", panel_row$plot_id)))
      box_node <- xml_add_child(box_group, "rect")
      xml_set_attrs(box_node, c(
        "id" = paste0("box__", panel_row$plot_id), "inkscape:label" = panel_row$plot_id,
        "x" = panel_row$x_mm, "y" = panel_row$y_mm,
        "width" = panel_row$width_mm, "height" = panel_row$height_mm,
        "style" = "fill:#1f77b4;fill-opacity:0.10;stroke:none"
      ))
      note_lines <- format_box_note_lines(panel_row)
      note_line_spacing_mm <- note_font_mm * 1.25
      # White card behind the note so it stays readable over the plot, starting
      # 4 mm in from the left so the panel letter stays visible. Its size is
      # estimated from the text (about 0.55 em per character).
      card_left_mm <- as.numeric(panel_row$x_mm) + 4
      card_width_mm <- min(as.numeric(panel_row$width_mm) - 4.5,
                           max(nchar(note_lines)) * 0.55 * note_font_mm + 2)
      card_node <- xml_add_child(box_group, "rect")
      xml_set_attrs(card_node, c(
        "id" = paste0("notecard__", panel_row$plot_id),
        "x" = format(card_left_mm), "y" = format(as.numeric(panel_row$y_mm) + 0.5),
        "width" = format(round(card_width_mm, 2)),
        "height" = format(round(length(note_lines) * note_line_spacing_mm + 1.5, 2)),
        "style" = "fill:#ffffff;fill-opacity:0.95;stroke:#1f4e8c;stroke-width:0.2"
      ))
      add_text_lines(box_group, note_lines, card_left_mm + 1,
                     as.numeric(panel_row$y_mm) + 1 + note_font_mm, note_font_mm, note_line_spacing_mm,
                     "font-family:Arial;fill:#1f4e8c")
    }
  }

  if (is.null(output_path)) {
    output_path <- if (canvas_purpose == "working") {
      next_working_canvas_path(assembly_directory, figure_name)
    } else {
      file.path(project_settings$final_output_folder, paste0(figure_name, "_journal.svg"))
    }
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  # No indentation: whitespace between text lines would be read as text.
  write_xml(canvas_document, output_path, options = "as_xml")
  message("Written: ", output_path)
  invisible(output_path)
}

# ---- Round 0: a first layout ---------------------------------------------------

# Places panels left to right in the given order, starting a new row when the
# next panel would not fit the page width. Returns x/y positions and the page
# height needed. Sizes are decided beforehand (by Claude or by you).
layout_panels_in_rows <- function(plot_ids, widths_mm, heights_mm, page_width_mm,
                                  gap_mm = 4, margin_mm = 2) {
  x_positions <- numeric(length(plot_ids))
  y_positions <- numeric(length(plot_ids))
  cursor_x <- margin_mm
  cursor_y <- margin_mm
  row_height <- 0
  for (panel_index in seq_along(plot_ids)) {
    if (cursor_x > margin_mm && cursor_x + widths_mm[panel_index] > page_width_mm - margin_mm) {
      cursor_x <- margin_mm
      cursor_y <- cursor_y + row_height + gap_mm
      row_height <- 0
    }
    x_positions[panel_index] <- cursor_x
    y_positions[panel_index] <- cursor_y
    cursor_x <- cursor_x + widths_mm[panel_index] + gap_mm
    row_height <- max(row_height, heights_mm[panel_index])
  }
  list(
    panel_positions = tibble(plot_id = plot_ids, x_mm = round(x_positions, 2), y_mm = round(y_positions, 2),
                             width_mm = widths_mm, height_mm = heights_mm),
    page_height_mm = round(cursor_y + row_height + margin_mm, 2)
  )
}

# ---- Export ------------------------------------------------------------------

find_inkscape_executable <- function(project_settings = list()) {
  candidate_paths <- c(project_settings$inkscape_path, Sys.which("inkscape"),
                       "/Applications/Inkscape.app/Contents/MacOS/inkscape")
  candidate_paths <- candidate_paths[!is.na(candidate_paths) & candidate_paths != ""]
  existing_paths <- candidate_paths[file.exists(candidate_paths)]
  if (length(existing_paths) == 0) {
    stop("Inkscape was not found. Set inkscape_path in project_settings.tsv.", call. = FALSE)
  }
  existing_paths[[1]]
}

run_inkscape_export <- function(svg_path, output_path, export_type, extra_arguments = character(),
                                 inkscape_path = find_inkscape_executable()) {
  export_status <- system2(
    inkscape_path,
    c(shQuote(svg_path), paste0("--export-type=", export_type),
      paste0("--export-filename=", shQuote(output_path)), "--export-area-page", extra_arguments),
    env = c("LANG=en_US.UTF-8", "LC_ALL=en_US.UTF-8"), stdout = FALSE, stderr = FALSE
  )
  if (export_status != 0 || !file.exists(output_path)) {
    stop("Inkscape export failed for ", svg_path, call. = FALSE)
  }
  invisible(output_path)
}

# Journal deliverable: vector PDF, fonts embedded, text kept as text (not outlines).
export_figure_pdf <- function(svg_path, pdf_path = str_replace(svg_path, "\\.svg$", ".pdf"),
                              inkscape_path = find_inkscape_executable()) {
  run_inkscape_export(svg_path, pdf_path, "pdf", inkscape_path = inkscape_path)
}

# For journals that ask for EPS. EPS cannot store transparency.
export_figure_eps <- function(svg_path, eps_path = str_replace(svg_path, "\\.svg$", ".eps"),
                              inkscape_path = find_inkscape_executable()) {
  run_inkscape_export(svg_path, eps_path, "eps", inkscape_path = inkscape_path)
}

# Preview image for checking a canvas by eye (used by Claude). Written to a
# temporary file outside the project by default: the SVG itself is the preview
# for people.
render_canvas_preview <- function(svg_path, png_path = tempfile(pattern = "canvas_preview_", fileext = ".png"),
                                  dots_per_inch = 200, inkscape_path = find_inkscape_executable()) {
  run_inkscape_export(svg_path, png_path, "png",
                      extra_arguments = c(paste0("--export-dpi=", dots_per_inch), "--export-background=white",
                                          "--export-background-opacity=1"),
                      inkscape_path = inkscape_path)
}
