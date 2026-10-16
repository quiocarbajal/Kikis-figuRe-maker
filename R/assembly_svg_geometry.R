# Figure assembly toolkit: SVG geometry
#
# Resolves the stacked transforms that Inkscape writes (layer translations,
# group matrices, element transforms) so that positions and sizes are always
# computed by code, never estimated by hand.

inkscape_namespace <- "http://www.inkscape.org/namespaces/inkscape"
sodipodi_namespace <- "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"

# ---- Lengths -----------------------------------------------------------------

# Converts an SVG length such as "183mm", "164.41pt" or "504" to millimetres.
# Unit-less lengths and "px" are CSS pixels (1/96 inch), as Inkscape reads them.
svg_length_to_millimetres <- function(svg_length) {
  length_parts <- str_match(str_trim(svg_length), "^([-+0-9.eE]+)\\s*([a-zA-Z%]*)$")
  if (is.na(length_parts[1, 1])) {
    stop("Cannot read the SVG length '", svg_length, "'.", call. = FALSE)
  }
  length_value <- as.numeric(length_parts[1, 2])
  length_unit <- tolower(length_parts[1, 3])
  if (length_unit == "") length_unit <- "px"
  millimetres_per_unit <- switch(length_unit,
    "mm" = 1, "cm" = 10, "in" = 25.4, "pt" = 25.4 / 72, "pc" = 25.4 / 6,
    "px" = 25.4 / 96,
    stop("Unsupported SVG length unit '", length_unit, "' in '", svg_length, "'.", call. = FALSE)
  )
  length_value * millimetres_per_unit
}

# Page size and the size of one user unit, from the root <svg> element.
read_svg_page_geometry <- function(svg_document) {
  svg_root <- xml_root(svg_document)
  page_width_mm <- svg_length_to_millimetres(xml_attr(svg_root, "width"))
  page_height_mm <- svg_length_to_millimetres(xml_attr(svg_root, "height"))
  view_box_text <- xml_attr(svg_root, "viewBox")
  if (is.na(view_box_text)) {
    millimetres_per_user_unit_x <- 25.4 / 96
    millimetres_per_user_unit_y <- 25.4 / 96
  } else {
    view_box_values <- as.numeric(str_split_1(str_trim(view_box_text), "[\\s,]+"))
    millimetres_per_user_unit_x <- page_width_mm / view_box_values[3]
    millimetres_per_user_unit_y <- page_height_mm / view_box_values[4]
  }
  if (abs(millimetres_per_user_unit_x / millimetres_per_user_unit_y - 1) > 0.001) {
    warning("The SVG page is scaled differently horizontally and vertically; positions may be distorted.",
            call. = FALSE)
  }
  list(
    page_width_mm = page_width_mm,
    page_height_mm = page_height_mm,
    millimetres_per_user_unit = millimetres_per_user_unit_x
  )
}

# ---- Transforms --------------------------------------------------------------

# Parses an SVG transform attribute into a 3 x 3 affine matrix.
parse_svg_transform <- function(transform_text) {
  combined_matrix <- diag(3)
  if (is.na(transform_text) || str_trim(transform_text) == "") {
    return(combined_matrix)
  }
  transform_operations <- str_match_all(transform_text, "([a-zA-Z]+)\\s*\\(([^)]*)\\)")[[1]]
  for (operation_index in seq_len(nrow(transform_operations))) {
    operation_name <- transform_operations[operation_index, 2]
    operation_numbers <- as.numeric(str_split_1(str_trim(transform_operations[operation_index, 3]), "[\\s,]+"))
    operation_matrix <- switch(operation_name,
      "matrix" = matrix(c(operation_numbers[1], operation_numbers[2], 0,
                          operation_numbers[3], operation_numbers[4], 0,
                          operation_numbers[5], operation_numbers[6], 1), nrow = 3),
      "translate" = matrix(c(1, 0, 0, 0, 1, 0,
                             operation_numbers[1], if (length(operation_numbers) > 1) operation_numbers[2] else 0, 1),
                           nrow = 3),
      "scale" = {
        scale_x <- operation_numbers[1]
        scale_y <- if (length(operation_numbers) > 1) operation_numbers[2] else scale_x
        matrix(c(scale_x, 0, 0, 0, scale_y, 0, 0, 0, 1), nrow = 3)
      },
      "rotate" = {
        angle_radians <- operation_numbers[1] * pi / 180
        rotation_matrix <- matrix(c(cos(angle_radians), sin(angle_radians), 0,
                                    -sin(angle_radians), cos(angle_radians), 0, 0, 0, 1), nrow = 3)
        if (length(operation_numbers) == 3) {
          to_centre <- matrix(c(1, 0, 0, 0, 1, 0, operation_numbers[2], operation_numbers[3], 1), nrow = 3)
          from_centre <- matrix(c(1, 0, 0, 0, 1, 0, -operation_numbers[2], -operation_numbers[3], 1), nrow = 3)
          to_centre %*% rotation_matrix %*% from_centre
        } else {
          rotation_matrix
        }
      },
      "skewX" = matrix(c(1, 0, 0, tan(operation_numbers[1] * pi / 180), 1, 0, 0, 0, 1), nrow = 3),
      "skewY" = matrix(c(1, tan(operation_numbers[1] * pi / 180), 0, 0, 1, 0, 0, 0, 1), nrow = 3),
      stop("Unsupported SVG transform '", operation_name, "'.", call. = FALSE)
    )
    combined_matrix <- combined_matrix %*% operation_matrix
  }
  combined_matrix
}

# Transform from an element's own coordinates to the page (root user units):
# product of every ancestor's transform, outermost first, then its own.
cumulative_transform <- function(svg_node, include_own_transform = TRUE) {
  ancestor_nodes <- rev(xml_parents(svg_node))
  combined_matrix <- diag(3)
  for (ancestor_node in ancestor_nodes) {
    combined_matrix <- combined_matrix %*% parse_svg_transform(xml_attr(ancestor_node, "transform"))
  }
  if (include_own_transform) {
    combined_matrix <- combined_matrix %*% parse_svg_transform(xml_attr(svg_node, "transform"))
  }
  combined_matrix
}

# Bounding box (in page millimetres) of a rectangle given in local coordinates.
transformed_rectangle_mm <- function(local_x, local_y, local_width, local_height,
                                     transform_matrix, millimetres_per_user_unit) {
  corner_points <- rbind(
    c(local_x, local_x + local_width, local_x, local_x + local_width),
    c(local_y, local_y, local_y + local_height, local_y + local_height),
    c(1, 1, 1, 1)
  )
  page_points <- transform_matrix %*% corner_points
  tibble(
    x_mm = min(page_points[1, ]) * millimetres_per_user_unit,
    y_mm = min(page_points[2, ]) * millimetres_per_user_unit,
    width_mm = (max(page_points[1, ]) - min(page_points[1, ])) * millimetres_per_user_unit,
    height_mm = (max(page_points[2, ]) - min(page_points[2, ])) * millimetres_per_user_unit
  )
}

# Uniform scale factor of a transform (geometric mean of the axis scales) and
# whether it stretches differently in the two directions.
transform_scale_summary <- function(transform_matrix) {
  scale_x <- sqrt(transform_matrix[1, 1]^2 + transform_matrix[2, 1]^2)
  scale_y <- sqrt(transform_matrix[1, 2]^2 + transform_matrix[2, 2]^2)
  list(
    uniform_scale = sqrt(scale_x * scale_y),
    is_stretched = abs(scale_x / scale_y - 1) > 0.01
  )
}

# ---- Finding things ----------------------------------------------------------

find_layer <- function(svg_document, layer_label) {
  xml_find_first(svg_document, sprintf(
    "//*[local-name()='g'][@*[local-name()='groupmode']='layer'][@*[local-name()='label']='%s']",
    layer_label
  ))
}

# Reads a CSS property from a style attribute, falling back to the plain
# attribute (e.g. style="font-size:2.47px" or font-size="2.47").
read_style_property <- function(svg_node, property_name) {
  style_text <- xml_attr(svg_node, "style")
  if (!is.na(style_text)) {
    property_match <- str_match(style_text, paste0("(?:^|;)\\s*", property_name, "\\s*:\\s*([^;]+)"))[1, 2]
    if (!is.na(property_match)) {
      return(str_trim(property_match))
    }
  }
  xml_attr(svg_node, property_name)
}

# Reads a property that SVG inherits from ancestors (font-size, stroke-width).
read_inherited_style_property <- function(svg_node, property_name) {
  for (candidate_node in c(list(svg_node), as.list(xml_parents(svg_node)))) {
    property_value <- read_style_property(candidate_node, property_name)
    if (!is.na(property_value)) {
      return(property_value)
    }
  }
  NA_character_
}

# Text lines of an Inkscape text element (one line per sodipodi:role="line" tspan).
read_text_lines <- function(text_node) {
  line_nodes <- xml_find_all(text_node, "./*[local-name()='tspan']")
  if (length(line_nodes) == 0) {
    return(xml_text(text_node))
  }
  xml_text(line_nodes)
}

# Anchor point (page mm) of a text element: its x/y, or its first tspan's x/y.
text_anchor_mm <- function(text_node, millimetres_per_user_unit) {
  anchor_x <- as.numeric(str_split_1(coalesce(xml_attr(text_node, "x"), ""), "[\\s,]+")[1])
  anchor_y <- as.numeric(str_split_1(coalesce(xml_attr(text_node, "y"), ""), "[\\s,]+")[1])
  anchor_node <- text_node
  if (is.na(anchor_x) || is.na(anchor_y)) {
    anchor_node <- xml_find_first(text_node, "./*[local-name()='tspan']")
    anchor_x <- as.numeric(str_split_1(coalesce(xml_attr(anchor_node, "x"), "0"), "[\\s,]+")[1])
    anchor_y <- as.numeric(str_split_1(coalesce(xml_attr(anchor_node, "y"), "0"), "[\\s,]+")[1])
  }
  page_point <- cumulative_transform(anchor_node) %*% c(anchor_x, anchor_y, 1)
  c(x_mm = page_point[1] * millimetres_per_user_unit, y_mm = page_point[2] * millimetres_per_user_unit)
}
