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
    # An SVG without stroked lines gives an empty table, not one without columns.
    list_rbind(ptype = tibble(shape_type = character(), layer = character(), effective_line_pt = numeric()))
}

# Pixel width and height from the header of a PNG or JPEG file; NA for other
# formats. svglite writes PNG, and Inkscape stores imported TIFF files as PNG.
read_raster_pixel_size <- function(image_bytes, image_format) {
  byte_values <- as.integer(image_bytes)
  if (identical(image_format, "image/png") && length(byte_values) >= 24) {
    return(c(width_px = sum(byte_values[17:20] * 256^(3:0)), height_px = sum(byte_values[21:24] * 256^(3:0))))
  }
  if (image_format %in% c("image/jpeg", "image/jpg")) {
    # Walk the JPEG segments to the first start-of-frame segment, which holds
    # the height and width.
    start_of_frame_codes <- c(0xC0:0xC3, 0xC5:0xC7, 0xC9:0xCB, 0xCD:0xCF)
    segment_start <- 3
    while (segment_start + 8 <= length(byte_values) && byte_values[segment_start] == 0xFF) {
      if (byte_values[segment_start + 1] %in% start_of_frame_codes) {
        return(c(width_px = byte_values[segment_start + 7] * 256 + byte_values[segment_start + 8],
                 height_px = byte_values[segment_start + 5] * 256 + byte_values[segment_start + 6]))
      }
      segment_start <- segment_start + 2 + byte_values[segment_start + 2] * 256 + byte_values[segment_start + 3]
    }
  }
  c(width_px = NA_real_, height_px = NA_real_)
}

# One row per image element: how it is stored, its pixel size, where and how
# large it is printed, and its effective resolution in dpi (image pixels per
# inch at the printed size, after all transforms). raster_role names images
# that are not pictures: "data cells" drawn with sharp pixels (ggplot2
# geom_raster, heatmaps from other programs) and one-pixel "gradient strip"s
# (colour-bar legends).
measure_raster_images <- function(svg_document) {
  page_geometry <- read_svg_page_geometry(svg_document)
  image_nodes <- xml_find_all(svg_document, "//*[local-name()='image'][not(ancestor::*[local-name()='defs'])]")
  if (length(image_nodes) == 0) {
    return(tibble(layer = character(), plot_id = character(), image_id = character(), storage = character(),
                  image_format = character(), width_px = numeric(), height_px = numeric(), x_mm = numeric(),
                  y_mm = numeric(), printed_width_mm = numeric(), printed_height_mm = numeric(),
                  effective_dpi = numeric(), is_stretched = logical(), raster_role = character()))
  }
  map(image_nodes, \(image_node) {
    image_reference <- coalesce(xml_attr(image_node, "href"), "")
    is_embedded <- str_starts(image_reference, "data:")
    image_format <- if (is_embedded) tolower(str_match(image_reference, "^data:([^;,]+)")[1, 2]) else NA_character_
    pixel_size <- c(width_px = NA_real_, height_px = NA_real_)
    if (is_embedded && str_detect(image_reference, "^data:[^,]*;base64,")) {
      encoded_image <- str_remove_all(str_remove(image_reference, "^data:[^,]*,"), "\\s")
      if (identical(image_format, "image/png")) encoded_image <- str_sub(encoded_image, 1, 64) # header only
      pixel_size <- read_raster_pixel_size(jsonlite::base64_dec(encoded_image), image_format)
    }
    # Element units per image pixel. With preserveAspectRatio="none" (written
    # by Inkscape and svglite) the image fills its width and height; otherwise
    # it keeps its proportions and fits inside them.
    element_width <- coalesce(css_length_to_user_units(xml_attr(image_node, "width")), pixel_size[["width_px"]])
    element_height <- coalesce(css_length_to_user_units(xml_attr(image_node, "height")), pixel_size[["height_px"]])
    units_per_pixel_x <- element_width / pixel_size[["width_px"]]
    units_per_pixel_y <- element_height / pixel_size[["height_px"]]
    if (!str_starts(coalesce(xml_attr(image_node, "preserveAspectRatio"), "xMidYMid"), "none")) {
      units_per_pixel_x <- units_per_pixel_y <- min(units_per_pixel_x, units_per_pixel_y)
    }
    image_transform <- cumulative_transform(image_node)
    millimetres_per_pixel_x <- units_per_pixel_x * sqrt(image_transform[1, 1]^2 + image_transform[2, 1]^2) *
      page_geometry$millimetres_per_user_unit
    millimetres_per_pixel_y <- units_per_pixel_y * sqrt(image_transform[1, 2]^2 + image_transform[2, 2]^2) *
      page_geometry$millimetres_per_user_unit
    printed_box <- transformed_rectangle_mm(coalesce(css_length_to_user_units(xml_attr(image_node, "x")), 0),
                                            coalesce(css_length_to_user_units(xml_attr(image_node, "y")), 0),
                                            element_width, element_height, image_transform,
                                            page_geometry$millimetres_per_user_unit)
    image_rendering <- tolower(coalesce(read_inherited_style_property(image_node, "image-rendering"), "auto"))
    plot_group <- xml_find_first(image_node, "ancestor::*[local-name()='g'][starts-with(@id, 'plot__')][1]")
    tibble(
      layer = coalesce(xml_attr(xml_find_first(image_node, "ancestor::*[@*[local-name()='groupmode']='layer'][1]"),
                                "label"), NA_character_),
      plot_id = str_remove(xml_attr(plot_group, "id"), "^plot__"),
      image_id = xml_attr(image_node, "id"),
      storage = if (is_embedded) "embedded" else "linked",
      image_format = image_format,
      width_px = pixel_size[["width_px"]],
      height_px = pixel_size[["height_px"]],
      x_mm = round(printed_box$x_mm, 1),
      y_mm = round(printed_box$y_mm, 1),
      printed_width_mm = round(pixel_size[["width_px"]] * millimetres_per_pixel_x, 2),
      printed_height_mm = round(pixel_size[["height_px"]] * millimetres_per_pixel_y, 2),
      effective_dpi = round(25.4 / max(millimetres_per_pixel_x, millimetres_per_pixel_y)),
      is_stretched = abs(millimetres_per_pixel_x / millimetres_per_pixel_y - 1) > 0.01,
      raster_role = case_when(
        identical(image_format, "image/svg+xml") ~ "vector image",
        image_rendering %in% c("pixelated", "optimizespeed", "crisp-edges") ~ "data cells",
        isTRUE(min(pixel_size) == 1) ~ "gradient strip",
        .default = "picture"
      )
    )
  }) |>
    list_rbind()
}

#' Check a figure SVG against a journal preset
#'
#' @param ignore_layers Layers left out of the text, line and image checks (the
#'   boxes layer holds notes, not figure content).
#' @return A list: issues (one row per problem), text_sizes, line_widths,
#'   raster_images.
check_figure_svg <- function(svg_path, journal_preset, ignore_layers = "boxes") {
  svg_document <- read_xml(svg_path)
  page_geometry <- read_svg_page_geometry(svg_document)
  text_sizes <- measure_text_sizes(svg_document) |> dplyr::filter(!layer %in% ignore_layers)
  line_widths <- measure_line_widths(svg_document) |> dplyr::filter(!layer %in% ignore_layers)
  raster_images <- measure_raster_images(svg_document) |> dplyr::filter(!layer %in% ignore_layers)
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
  # Resolution and stretching are checked for pictures only (see
  # measure_raster_images()); data cells and colour bars are drawn this way on purpose.
  pictures <- dplyr::filter(raster_images, storage == "embedded", raster_role == "picture")
  measured_pictures <- dplyr::filter(pictures, !is.na(effective_dpi))
  describe_images <- function(image_rows) {
    paste(sprintf("%s at x %.0f, y %.0f mm (layer %s%s): %.0f x %.0f px printed %.1f x %.1f mm, %.0f dpi",
                  coalesce(image_rows$image_id, "image"), image_rows$x_mm, image_rows$y_mm,
                  coalesce(image_rows$layer, "none"),
                  if_else(is.na(image_rows$plot_id), "", paste0(", plot ", image_rows$plot_id)),
                  image_rows$width_px, image_rows$height_px, image_rows$printed_width_mm,
                  image_rows$printed_height_mm, image_rows$effective_dpi), collapse = "; ")
  }
  if (nrow(measured_pictures) < nrow(pictures)) {
    issues <- add_issue(issues, "warning", "image resolution not measured", paste0(
      "Pixel size is read from PNG and JPEG images only; not measured: ",
      paste(unique(dplyr::filter(pictures, is.na(effective_dpi))$image_format), collapse = ", ")))
  }
  minimum_image_dpi <- as.numeric(journal_preset[["min_image_dpi"]] %||% NA)
  recommended_image_dpi <- as.numeric(journal_preset[["recommended_image_dpi"]] %||% NA)
  if (nrow(measured_pictures) > 0 && is.na(minimum_image_dpi)) {
    issues <- add_issue(issues, "warning", "image resolution not checked", paste(
      "The journal preset has no min_image_dpi (an older copy of journal_presets.tsv, or a value the journal",
      "does not state)."))
  }
  if (!is.na(minimum_image_dpi)) {
    low_resolution_pictures <- dplyr::filter(measured_pictures, effective_dpi < minimum_image_dpi)
    if (nrow(low_resolution_pictures) > 0) {
      issues <- add_issue(issues, "warning", "image below journal minimum", sprintf(
        "%d images below %.0f dpi at printed size: %s", nrow(low_resolution_pictures), minimum_image_dpi,
        describe_images(low_resolution_pictures)))
    }
    if (!is.na(recommended_image_dpi)) {
      below_recommended_pictures <- dplyr::filter(measured_pictures, effective_dpi >= minimum_image_dpi,
                                                  effective_dpi < recommended_image_dpi)
      if (nrow(below_recommended_pictures) > 0) {
        issues <- add_issue(issues, "warning", "image below journal recommendation", sprintf(
          "%d images meet the %.0f dpi minimum but not the %.0f dpi the journal also recommends (see the preset notes): %s",
          nrow(below_recommended_pictures), minimum_image_dpi, recommended_image_dpi,
          describe_images(below_recommended_pictures)))
      }
    }
  }
  stretched_pictures <- dplyr::filter(measured_pictures, is_stretched)
  if (nrow(stretched_pictures) > 0) {
    issues <- add_issue(issues, "warning", "stretched image", sprintf(
      paste("%d images are resized more in one direction than the other; fine when intended (e.g. a heatmap),",
            "but it distorts a photograph, micrograph or blot: %s"),
      nrow(stretched_pictures), describe_images(stretched_pictures)))
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
  list(issues = issues, text_sizes = text_sizes, line_widths = line_widths, raster_images = raster_images)
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
