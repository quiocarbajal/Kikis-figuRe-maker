# Figure assembly toolkit: checks
#
# Measures what a reader of the final file will actually see: the effective
# font size of every piece of text and the effective width of every line,
# after all transforms (a plot shrunk to 73 % in Inkscape shows 7 pt text as
# 5.1 pt; this check reports 5.1 pt).

# CSS length (font-size, stroke-width) in the element's own user units.
css_length_to_user_units <- function(css_length) {
  length_parts <- str_match(str_trim(css_length), "^([-+0-9.eE]+)\\s*([a-zA-Z%]*)$")
  if (is.na(length_parts[1, 1])) {
    return(NA_real_)
  }
  length_value <- as.numeric(length_parts[1, 2])
  length_unit <- tolower(length_parts[1, 3])
  if (length_unit == "") length_unit <- "px"
  switch(length_unit,
    "px" = length_value,
    "pt" = length_value * 96 / 72, "pc" = length_value * 16,
    "mm" = length_value * 96 / 25.4, "cm" = length_value * 96 / 2.54, "in" = length_value * 96,
    NA_real_ # %, em and similar relative units are not resolved
  )
}

# One row per piece of text: content, font family, effective size in pt.
measure_text_sizes <- function(svg_document) {
  page_geometry <- read_svg_page_geometry(svg_document)
  text_nodes <- xml_find_all(svg_document, paste0(
    "//*[(local-name()='text' or local-name()='tspan') and text()[normalize-space()]]"
  ))
  map(text_nodes, \(text_node) {
    font_size_user_units <- css_length_to_user_units(coalesce(read_inherited_style_property(text_node, "font-size"), "16"))
    scale_summary <- transform_scale_summary(cumulative_transform(text_node))
    tibble(
      text_content = str_trunc(str_squish(paste(xml_text(xml_find_all(text_node, "./text()")), collapse = "")), 40),
      layer = coalesce(xml_attr(xml_find_first(text_node, "ancestor::*[@*[local-name()='groupmode']='layer'][1]"),
                                "label"), NA_character_),
      font_family = str_remove_all(coalesce(read_inherited_style_property(text_node, "font-family"), NA_character_), "[\"']"),
      effective_font_pt = round(font_size_user_units * scale_summary$uniform_scale *
                                  page_geometry$millimetres_per_user_unit / millimetres_per_point, 2),
      is_stretched = scale_summary$is_stretched
    )
  }) |>
    list_rbind()
}

# One row per stroked line or shape: effective line width in pt. Circles are
# left out: in plots they are data points, whose outline is not a "line" in
# the journals' sense.
measure_line_widths <- function(svg_document) {
  page_geometry <- read_svg_page_geometry(svg_document)
  shape_nodes <- xml_find_all(svg_document, paste0(
    "//*[local-name()='line' or local-name()='polyline' or local-name()='polygon' or ",
    "local-name()='path' or local-name()='rect' or local-name()='ellipse']"
  ))
  map(shape_nodes, \(shape_node) {
    stroke_colour <- coalesce(read_inherited_style_property(shape_node, "stroke"), "none")
    if (stroke_colour == "none") {
      return(NULL)
    }
    stroke_width_user_units <- css_length_to_user_units(coalesce(read_inherited_style_property(shape_node, "stroke-width"), "1"))
    if (is.na(stroke_width_user_units) || stroke_width_user_units == 0) {
      return(NULL)
    }
    scale_summary <- transform_scale_summary(cumulative_transform(shape_node))
    tibble(
      shape_type = xml_name(shape_node),
      layer = coalesce(xml_attr(xml_find_first(shape_node, "ancestor::*[@*[local-name()='groupmode']='layer'][1]"),
                                "label"), NA_character_),
      effective_line_pt = round(stroke_width_user_units * scale_summary$uniform_scale *
                                  page_geometry$millimetres_per_user_unit / millimetres_per_point, 3)
    )
  }) |>
    list_rbind()
}

#' Check a figure SVG against a journal preset
#'
#' @param ignore_layers Layers left out of the text and line checks (the boxes
#'   layer holds notes, not figure content).
#' @return A list: issues (one row per problem), text_sizes, line_widths.
check_figure_svg <- function(svg_path, journal_preset, ignore_layers = "boxes") {
  svg_document <- read_xml(svg_path)
  page_geometry <- read_svg_page_geometry(svg_document)
  text_sizes <- measure_text_sizes(svg_document) |> dplyr::filter(!layer %in% ignore_layers)
  line_widths <- measure_line_widths(svg_document) |> dplyr::filter(!layer %in% ignore_layers)
  issues <- tibble(severity = character(), check = character(), detail = character())
  add_issue <- function(issues, severity, check_name, detail) {
    bind_rows(issues, tibble(severity = severity, check = check_name, detail = detail))
  }

  minimum_font_pt <- as.numeric(journal_preset$min_font_pt)
  maximum_font_pt <- as.numeric(journal_preset$max_font_pt)
  if (!is.na(minimum_font_pt) && any(text_sizes$effective_font_pt < minimum_font_pt - 0.05, na.rm = TRUE)) {
    small_text <- dplyr::filter(text_sizes, effective_font_pt < minimum_font_pt - 0.05)
    issues <- add_issue(issues, "error", "font below journal minimum", sprintf(
      "%d text items below %.1f pt (smallest %.2f pt, e.g. \"%s\")", nrow(small_text), minimum_font_pt,
      min(small_text$effective_font_pt), small_text$text_content[which.min(small_text$effective_font_pt)]))
  }
  if (!is.na(maximum_font_pt) && any(text_sizes$effective_font_pt > maximum_font_pt + 0.05 &
                                     text_sizes$layer != "panel_letters", na.rm = TRUE)) {
    large_text <- dplyr::filter(text_sizes, effective_font_pt > maximum_font_pt + 0.05, layer != "panel_letters")
    issues <- add_issue(issues, "warning", "font above journal maximum", sprintf(
      "%d text items above %.1f pt (largest %.2f pt)", nrow(large_text), maximum_font_pt,
      max(large_text$effective_font_pt)))
  }
  if (any(text_sizes$is_stretched)) {
    issues <- add_issue(issues, "error", "stretched text", sprintf(
      "%d text items are stretched unevenly (a plot or text was resized non-proportionally)",
      sum(text_sizes$is_stretched)))
  }
  allowed_families <- str_trim(str_split_1(coalesce(journal_preset$font_family, ""), ","))
  used_families <- unique(na.omit(str_trim(str_split_i(text_sizes$font_family, ",", 1))))
  unexpected_families <- setdiff(used_families, allowed_families)
  if (length(allowed_families) > 0 && allowed_families[1] != "" && length(unexpected_families) > 0) {
    issues <- add_issue(issues, "warning", "font family", paste0(
      "Fonts not in the journal preset (", journal_preset$font_family, "): ", paste(unexpected_families, collapse = ", ")))
  }
  minimum_line_pt <- as.numeric(journal_preset$min_line_pt)
  if (!is.na(minimum_line_pt) && nrow(line_widths) > 0 && any(line_widths$effective_line_pt < minimum_line_pt - 0.005)) {
    thin_lines <- dplyr::filter(line_widths, effective_line_pt < minimum_line_pt - 0.005)
    issues <- add_issue(issues, "warning", "line below journal minimum", sprintf(
      "%d lines thinner than %.2f pt (thinnest %.3f pt)", nrow(thin_lines), minimum_line_pt,
      min(thin_lines$effective_line_pt)))
  }
  if (length(xml_find_all(svg_document, "//*[local-name()='flowRoot']")) > 0 ||
      any(str_detect(coalesce(xml_attr(xml_find_all(svg_document, "//*[local-name()='text']"), "style"), ""),
                     "shape-inside"))) {
    issues <- add_issue(issues, "error", "flowed text",
                        "Flowed text found (created by dragging with the Text tool); many programs cannot display it. Recreate it by clicking.")
  }
  image_references <- coalesce(xml_attr(xml_find_all(svg_document, "//*[local-name()='image']"), "href"), "")
  linked_images <- image_references[!str_starts(image_references, "data:")]
  if (length(linked_images) > 0) {
    issues <- add_issue(issues, "error", "linked image",
                        paste("Images linked to files instead of embedded:", paste(linked_images, collapse = ", ")))
  }
  permitted_widths <- as.numeric(na.omit(c(journal_preset$single_column_mm, journal_preset$one_and_half_column_mm,
                                           journal_preset$double_column_mm)))
  if (length(permitted_widths) > 0 && all(abs(page_geometry$page_width_mm - permitted_widths) > 0.5)) {
    issues <- add_issue(issues, "warning", "page width", sprintf(
      "Page is %.1f mm wide; journal widths are %s mm", page_geometry$page_width_mm,
      paste(permitted_widths, collapse = " / ")))
  }
  maximum_height_mm <- as.numeric(journal_preset$max_height_mm)
  if (!is.na(maximum_height_mm) && page_geometry$page_height_mm > maximum_height_mm + 0.5) {
    issues <- add_issue(issues, "error", "page height", sprintf(
      "Page is %.1f mm tall; journal maximum is %.0f mm", page_geometry$page_height_mm, maximum_height_mm))
  }
  list(issues = issues, text_sizes = text_sizes, line_widths = line_widths)
}

# Short per-layer summary of the effective font sizes.
summarise_text_sizes <- function(text_sizes) {
  text_sizes |>
    dplyr::summarise(text_items = n(), smallest_pt = min(effective_font_pt), largest_pt = max(effective_font_pt),
                     .by = c(layer, font_family))
}

#' Basic check that a PDF keeps its text as text and embeds its fonts
#'
#' Scans the PDF's font entries without extra software. It cannot inspect
#' compressed object streams; when in doubt, open the PDF in Acrobat
#' (File > Properties > Fonts) to confirm.
check_pdf_fonts <- function(pdf_path) {
  pdf_text <- rawToChar(readBin(pdf_path, "raw", file.size(pdf_path)) |> (\(raw_bytes) raw_bytes[raw_bytes != as.raw(0)])())
  font_names <- unique(str_match_all(pdf_text, "/BaseFont\\s*/([^\\s/>\\[]+)")[[1]][, 2])
  embedded_font_files <- str_count(pdf_text, "/FontFile[23]?\\b")
  tibble(
    pdf = pdf_path,
    fonts_found = paste(font_names, collapse = ", "),
    font_count = length(font_names),
    embedded_font_programs = embedded_font_files,
    text_is_text = length(font_names) > 0,
    all_fonts_embedded = length(font_names) > 0 && embedded_font_files >= length(font_names)
  )
}
