# Figure assembly toolkit: knitr integration and appearance placeholders
#
# start_figure_assembly() is called once at the top of the figures document.
# It returns the placeholder functions used in the plotting chunks:
#   ggplot2 plots: + figure_style()            (one placeholder, last line)
#   pheatmap:      figure_pheatmap(...)         (instead of pheatmap(...))
#   eulerr:        figure_euler_plot(fit, ...)  (instead of plot(fit, ...))
#   UpSetR:        figure_upset(...)            (instead of upset(...))
# Plot size and output folder need no placeholder: a knitr option hook sets
# them from panel_specs.tsv using the chunk label.
#
# Every placeholder changes only settings from a fixed, permitted list of
# appearance settings. Without a table entry, a placeholder changes nothing.

# Label of the chunk being rendered (NULL outside knitr).
current_chunk_label <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(NULL)
  }
  knitr::opts_current$get("label")
}

# ---- Theme part of figure_style() --------------------------------------------

# Theme elements each setting applies to. Specific elements (x/y, sides) are
# included because a plot's own theme may set them directly, and a specific
# element always wins over its general parent in ggplot2.
axis_sides <- function(prefix) {
  c(prefix, paste0(prefix, c(".x", ".y", ".x.top", ".x.bottom", ".y.left", ".y.right")))
}
theme_text_elements <- list(
  title_pt = "plot.title", subtitle_pt = "plot.subtitle", caption_pt = "plot.caption",
  axis_title_pt = axis_sides("axis.title"), axis_text_pt = axis_sides("axis.text"),
  legend_title_pt = "legend.title", legend_text_pt = "legend.text",
  strip_text_pt = c("strip.text", "strip.text.x", "strip.text.y", "strip.text.x.top", "strip.text.x.bottom",
                    "strip.text.y.left", "strip.text.y.right")
)
theme_line_elements <- list(
  axis_line_pt = c(axis_sides("axis.line"), axis_sides("axis.ticks")),
  grid_line_pt = c("panel.grid", "panel.grid.major", "panel.grid.minor", "panel.grid.major.x",
                   "panel.grid.major.y", "panel.grid.minor.x", "panel.grid.minor.y")
)

# TRUE when the element is drawn in this plot (not switched off with
# element_blank). Settings are only applied to drawn elements, so a table value
# can never make hidden text or lines appear.
element_is_drawn <- function(complete_plot_theme, element_name) {
  element <- tryCatch(calc_element(element_name, complete_plot_theme), error = \(error) NULL)
  !is.null(element) && !is_theme_element(element, "blank")
}

build_appearance_theme <- function(panel_specs, plot_id, font_family, plot) {
  complete_plot_theme <- complete_theme(plot$theme)
  theme_settings <- list()
  # Text properties are collected per element first (size, face, angle), then
  # turned into one element_text each, so settings do not overwrite each other.
  text_properties <- list()
  add_text_property <- function(element_names, property_values) {
    for (element_name in element_names) {
      if (element_is_drawn(complete_plot_theme, element_name)) {
        text_properties[[element_name]] <<- modifyList(text_properties[[element_name]] %||% list(), property_values)
      }
    }
  }
  for (spec_field in names(theme_text_elements)) {
    font_pt <- lookup_panel_spec(panel_specs, plot_id, spec_field, default = NA_real_)
    if (!is.na(font_pt)) add_text_property(theme_text_elements[[spec_field]], list(size = font_pt))
  }
  axis_text_face <- lookup_panel_spec(panel_specs, plot_id, "axis_text_face", default = NA_character_)
  if (!is.na(axis_text_face)) add_text_property(axis_sides("axis.text"), list(face = axis_text_face))
  x_text_angle <- lookup_panel_spec(panel_specs, plot_id, "x_text_angle", default = NA_real_)
  if (!is.na(x_text_angle) && x_text_angle != 0) {
    # Rotated labels end at their tick: right-aligned, and centred on the tick at 90 degrees.
    add_text_property(c("axis.text.x", "axis.text.x.bottom"),
                      list(angle = x_text_angle, hjust = 1, vjust = if (abs(x_text_angle) == 90) 0.5 else 1))
  }
  for (element_name in names(text_properties)) {
    theme_settings[[element_name]] <- do.call(element_text, text_properties[[element_name]])
  }
  for (spec_field in names(theme_line_elements)) {
    line_pt <- lookup_panel_spec(panel_specs, plot_id, spec_field, default = NA_real_)
    if (is.na(line_pt)) next
    for (element_name in theme_line_elements[[spec_field]]) {
      if (element_is_drawn(complete_plot_theme, element_name)) {
        theme_settings[[element_name]] <- element_line(linewidth = line_pt_to_ggplot_linewidth(line_pt))
      }
    }
  }
  # Legend key size: ggplot2 sizes keys from its base font (11 pt by default),
  # not from the legend text size, so small text needs smaller keys.
  legend_key_mm <- lookup_panel_spec(panel_specs, plot_id, "legend_key_mm", default = NA_real_)
  if (!is.na(legend_key_mm)) {
    theme_settings$legend.key.size <- unit(legend_key_mm, "mm")
  }
  legend_position <- lookup_panel_spec(panel_specs, plot_id, "legend_position", default = NA_character_)
  if (!is.na(legend_position)) {
    theme_settings$legend.position <- legend_position
  }
  # Several legends (e.g. dot size and colour) stacked or side by side.
  legend_box_layout <- lookup_panel_spec(panel_specs, plot_id, "legend_box_layout", default = NA_character_)
  if (!is.na(legend_box_layout)) {
    theme_settings$legend.box <- legend_box_layout
  }
  if (!is.null(font_family) && !is.na(font_family) && font_family != "") {
    theme_settings$text <- element_text(family = font_family)
  }
  do.call(theme, theme_settings)
}

# ---- Layer part of figure_style() --------------------------------------------

text_geom_classes <- c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel")

# Fixed layer settings the table may change. A setting is skipped for a layer
# when the layer (or the inherited plot mapping) maps it to data, so a table
# value can never override an aesthetic that encodes data.
apply_permitted_layer_settings <- function(plot, panel_specs, plot_id) {
  point_size_ggplot <- lookup_panel_spec(panel_specs, plot_id, "point_size_ggplot", default = NA_real_)
  label_text_pt <- lookup_panel_spec(panel_specs, plot_id, "label_text_pt", default = NA_real_)
  line_width_pt <- lookup_panel_spec(panel_specs, plot_id, "line_width_pt", default = NA_real_)
  fixed_alpha <- lookup_panel_spec(panel_specs, plot_id, "alpha", default = NA_real_)

  plot$layers <- map(plot$layers, \(original_layer) {
    mapped_aesthetics <- names(original_layer$mapping)
    if (isTRUE(original_layer$inherit.aes)) {
      mapped_aesthetics <- c(mapped_aesthetics, names(plot$mapping))
    }
    layer_aesthetics <- original_layer$geom$aesthetics()
    is_text_layer <- inherits(original_layer$geom, text_geom_classes)
    new_settings <- list()
    if (is_text_layer && !is.na(label_text_pt)) {
      new_settings$size <- font_pt_to_geom_text_size(label_text_pt)
    }
    if (!is_text_layer && !is.na(point_size_ggplot) && inherits(original_layer$geom, "GeomPoint")) {
      new_settings$size <- point_size_ggplot
    }
    if (!is.na(line_width_pt) && "linewidth" %in% layer_aesthetics) {
      new_settings$linewidth <- line_pt_to_ggplot_linewidth(line_width_pt)
    }
    if (!is.na(fixed_alpha) && "alpha" %in% layer_aesthetics) {
      new_settings$alpha <- fixed_alpha
    }
    new_settings <- new_settings[!names(new_settings) %in% mapped_aesthetics]
    if (length(new_settings) == 0) {
      return(original_layer)
    }
    # Copy the layer so the original plot object is not modified.
    restyled_layer <- ggproto(NULL, original_layer)
    restyled_layer$aes_params <- modifyList(original_layer$aes_params, new_settings)
    restyled_layer
  })
  plot
}

# ---- Knitr option hook -------------------------------------------------------

make_chunk_option_hook <- function(panel_specs, project_settings, assembly_directory, draft_mode, assembly_state) {
  function(chunk_options) {
    plot_id <- chunk_options$label
    figure_name <- figure_for_plot(plot_id, panel_specs, project_settings$figure_label_pattern)
    if (is.na(figure_name)) {
      return(chunk_options) # not part of any figure: chunk left exactly as written
    }
    plot_folder <- if (draft_mode) "plots" else "final_plots"
    chunk_options$fig.path <- paste0(file.path(assembly_directory, "drafting", figure_name, plot_folder), "/")
    # Remembered so elements moved to annotations are saved next to the plot file.
    assembly_state$output_prefixes[[plot_id]] <- paste0(chunk_options$fig.path, plot_id)
    assembly_state$moved_labels[[plot_id]] <- NULL
    chunk_options$dev <- "svglite"
    chunk_options$dev.args <- list(fix_text_size = FALSE, bg = "transparent")
    width_mm <- lookup_panel_spec(panel_specs, plot_id, "width_mm", default = NA_real_)
    height_mm <- lookup_panel_spec(panel_specs, plot_id, "height_mm", default = NA_real_)
    if (!is.na(width_mm)) chunk_options$fig.width <- millimetre_to_inch(width_mm)
    if (!is.na(height_mm)) {
      chunk_options$fig.height <- millimetre_to_inch(height_mm)
      chunk_options$fig.asp <- NULL
    }
    chunk_options
  }
}

# ---- Entry point -------------------------------------------------------------

#' Start figure assembly in a figures document
#'
#' @param draft_mode TRUE while drafting (plots go to drafting/<Figure>/plots/);
#'   FALSE for the final render (plots go to drafting/<Figure>/final_plots/ and
#'   each figure's latest canvas is compared with the tables first).
#' @param assembly_directory Folder created by install_figure_assembly().
#' @param on_canvas_mismatch What the final render does when a canvas and the
#'   tables disagree: "stop" (default) or "use_table".
#' @return A list of placeholder functions and the loaded tables.
start_figure_assembly <- function(draft_mode = TRUE,
                                  assembly_directory = "figure_assembly",
                                  on_canvas_mismatch = c("stop", "use_table")) {
  on_canvas_mismatch <- match.arg(on_canvas_mismatch)
  if (!requireNamespace("svglite", quietly = TRUE)) {
    stop("The svglite package is required: install.packages(\"svglite\")", call. = FALSE)
  }
  project_settings <- read_project_settings(file.path(assembly_directory, "settings", "project_settings.tsv"))
  panel_specs <- read_panel_specs(file.path(assembly_directory, "specs", "panel_specs.tsv"))
  figure_layout <- read_figure_layout(file.path(assembly_directory, "specs", "figure_layout.tsv"))

  if (!draft_mode) {
    stop_if_canvases_disagree_with_tables(assembly_directory, panel_specs, figure_layout, on_canvas_mismatch)
  }

  # Shared by the hook and the placeholders during one render.
  assembly_state <- new.env()
  assembly_state$output_prefixes <- list()
  assembly_state$moved_labels <- list()

  if (requireNamespace("knitr", quietly = TRUE)) {
    knitr::opts_hooks$set(fig.width = make_chunk_option_hook(
      panel_specs, project_settings, assembly_directory, draft_mode, assembly_state
    ))
  }

  font_family <- project_settings$font_family

  figure_style <- function(plot_id = current_chunk_label()) {
    structure(list(plot_id = plot_id, panel_specs = panel_specs, font_family = font_family,
                   assembly_state = assembly_state),
              class = "figure_style_request")
  }

  figure_pheatmap <- function(..., plot_id = current_chunk_label()) {
    pheatmap_arguments <- list(...)
    permitted_arguments <- c(
      heatmap_font_pt = "fontsize", heatmap_row_font_pt = "fontsize_row",
      heatmap_col_font_pt = "fontsize_col"
    )
    for (spec_field in names(permitted_arguments)) {
      font_pt <- lookup_panel_spec(panel_specs, plot_id, spec_field, default = NA_real_)
      if (!is.na(font_pt)) pheatmap_arguments[[permitted_arguments[[spec_field]]]] <- font_pt
    }
    cell_border <- lookup_panel_spec(panel_specs, plot_id, "heatmap_cell_border", default = NULL)
    if (!is.null(cell_border)) pheatmap_arguments$border_color <- cell_border
    do.call(pheatmap::pheatmap, pheatmap_arguments)
  }

  figure_euler_plot <- function(euler_fit, ..., plot_id = current_chunk_label()) {
    plot_arguments <- list(...)
    set_euler_fontsize <- function(current_setting, font_pt) {
      if (is.na(font_pt) || isFALSE(current_setting)) {
        return(current_setting)
      }
      if (is.null(current_setting) || isTRUE(current_setting)) {
        return(list(fontsize = font_pt))
      }
      modifyList(current_setting, list(fontsize = font_pt))
    }
    euler_label_pt <- lookup_panel_spec(panel_specs, plot_id, "euler_label_pt", default = NA_real_)
    euler_quantity_pt <- lookup_panel_spec(panel_specs, plot_id, "euler_quantity_pt", default = NA_real_)
    if (!is.na(euler_label_pt)) {
      plot_arguments$labels <- set_euler_fontsize(plot_arguments$labels, euler_label_pt)
    }
    # Set names moved to annotations: drawn without them, names recorded for the canvas.
    moved_elements <- parse_moved_elements(lookup_panel_spec(panel_specs, plot_id, "move_to_annotations",
                                                             default = NA_character_))
    if ("set names" %in% moved_elements && !isFALSE(plot_arguments$labels)) {
      record_moved_labels(assembly_state, plot_id, rownames(euler_fit$ellipses), "set name")
      plot_arguments$labels <- FALSE
    }
    # Quantities are shown only when the original call enables them.
    if (!is.na(euler_quantity_pt) && !is.null(plot_arguments$quantities)) {
      plot_arguments$quantities <- set_euler_fontsize(plot_arguments$quantities, euler_quantity_pt)
    }
    # Title (main): only resized, its wording stays as written in the call.
    # eulerr multiplies the title's fontsize by cex (1.5 by default), so cex is set to 1.
    euler_title_pt <- lookup_panel_spec(panel_specs, plot_id, "title_pt", default = NA_real_)
    if (!is.na(euler_title_pt) && !is.null(plot_arguments$main)) {
      plot_arguments$main <- if (is.list(plot_arguments$main)) {
        modifyList(plot_arguments$main, list(fontsize = euler_title_pt, cex = 1))
      } else {
        list(label = plot_arguments$main, fontsize = euler_title_pt, cex = 1)
      }
    }
    do.call(plot, c(list(euler_fit), plot_arguments))
  }

  figure_upset <- function(..., plot_id = current_chunk_label()) {
    upset_arguments <- list(...)
    upset_text_scale <- lookup_panel_spec(panel_specs, plot_id, "upset_text_scale", default = NA_real_)
    if (!is.na(upset_text_scale)) upset_arguments$text.scale <- upset_text_scale
    do.call(UpSetR::upset, upset_arguments)
  }

  list(
    figure_style = figure_style,
    figure_pheatmap = figure_pheatmap,
    figure_euler_plot = figure_euler_plot,
    figure_upset = figure_upset,
    panel_specs = panel_specs,
    figure_layout = figure_layout,
    project_settings = project_settings,
    draft_mode = draft_mode
  )
}

# ---- Moving elements to annotations --------------------------------------------

moved_element_names <- list(ggplot = c("legend", "strip labels", "point labels"), euler = c("set names"))

parse_moved_elements <- function(stored_value) {
  if (is.null(stored_value) || is.na(stored_value) || stored_value == "") {
    return(character())
  }
  str_trim(tolower(str_split_1(stored_value, ",")))
}

# Records words (facet labels, set names) that the canvas build will place as
# editable text annotations. Written next to the plot file; nothing is written
# outside a knitr render of a figure chunk.
record_moved_labels <- function(assembly_state, plot_id, labels, label_kind) {
  output_prefix <- assembly_state$output_prefixes[[plot_id]]
  if (is.null(output_prefix)) {
    return(invisible(FALSE))
  }
  assembly_state$moved_labels[[plot_id]] <- bind_rows(assembly_state$moved_labels[[plot_id]],
                                                      tibble(label = labels, label_kind = label_kind))
  dir.create(dirname(output_prefix), recursive = TRUE, showWarnings = FALSE)
  write_tsv(assembly_state$moved_labels[[plot_id]], paste0(output_prefix, "-moved-labels.tsv"))
  invisible(TRUE)
}

# Saves the plot's legend (keys and text, already restyled) as its own SVG.
save_plot_legend <- function(plot, legend_path) {
  plot_table <- ggplotGrob(plot)
  legend_grobs <- plot_table$grobs[str_starts(plot_table$layout$name, "guide-box")]
  legend_grobs <- legend_grobs[!vapply(legend_grobs, inherits, TRUE, "zeroGrob")]
  if (length(legend_grobs) == 0) {
    return(invisible(FALSE))
  }
  legend_grob <- legend_grobs[[1]]
  legend_width_mm <- grid::convertWidth(sum(legend_grob$widths), "mm", valueOnly = TRUE)
  legend_height_mm <- grid::convertHeight(sum(legend_grob$heights), "mm", valueOnly = TRUE)
  previous_device <- grDevices::dev.cur()
  svglite::svglite(legend_path, width = millimetre_to_inch(legend_width_mm),
                   height = millimetre_to_inch(legend_height_mm), fix_text_size = FALSE, bg = "transparent")
  grid::grid.draw(legend_grob)
  grDevices::dev.off()
  if (previous_device > 1) grDevices::dev.set(previous_device) # back to knitr's device
  invisible(TRUE)
}

# Facet (strip) labels as shown in the plot.
facet_strip_labels <- function(plot) {
  facet_layout <- ggplot_build(plot)$layout$layout
  layout_columns <- c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "AXIS_X", "AXIS_Y", "COORD")
  unique(as.character(unlist(facet_layout[setdiff(names(facet_layout), layout_columns)])))
}

# Multiplies sizes mapped to data (e.g. DotPlot dot size = % expressed). The
# mapping, legend values and relative sizes stay the same.
scale_mapped_sizes <- function(plot, size_factor) {
  size_scale_positions <- which(vapply(plot$scales$scales, \(scale) "size" %in% scale$aesthetics, TRUE))
  if (length(size_scale_positions) == 0) {
    return(plot)
  }
  plot$scales <- plot$scales$clone() # leave the original plot object untouched
  for (scale_position in size_scale_positions) {
    resized_scale <- plot$scales$scales[[scale_position]]$clone()
    original_palette <- resized_scale$palette
    resized_scale$palette <- function(values) original_palette(values) * size_factor
    plot$scales$scales[[scale_position]] <- resized_scale
  }
  plot
}

# `plot + figure_style()` lands here. Registered in the ggplot2 namespace so it
# works however this file was sourced.
add_figure_style_to_plot <- function(object, plot, ...) {
  restyled_plot <- apply_permitted_layer_settings(plot, object$panel_specs, object$plot_id)
  restyled_plot <- restyled_plot +
    build_appearance_theme(object$panel_specs, object$plot_id, object$font_family, restyled_plot)
  label_wording <- build_label_wording(object$panel_specs, object$plot_id)
  if (length(label_wording) > 0) restyled_plot <- restyled_plot + do.call(labs, label_wording)
  size_factor <- lookup_panel_spec(object$panel_specs, object$plot_id, "mapped_size_factor", default = NA_real_)
  if (!is.na(size_factor)) restyled_plot <- scale_mapped_sizes(restyled_plot, size_factor)

  moved_elements <- parse_moved_elements(lookup_panel_spec(object$panel_specs, object$plot_id,
                                                           "move_to_annotations", default = NA_character_))
  output_prefix <- object$assembly_state$output_prefixes[[object$plot_id]]
  if ("legend" %in% moved_elements) {
    if (!is.null(output_prefix)) save_plot_legend(restyled_plot, paste0(output_prefix, "-moved-legend.svg"))
    restyled_plot <- restyled_plot + theme(legend.position = "none")
  }
  if ("strip labels" %in% moved_elements) {
    record_moved_labels(object$assembly_state, object$plot_id, facet_strip_labels(restyled_plot), "strip label")
    # Every strip-text element, because specific ones (strip.text.x.top, ...) override the general one.
    hidden_strip_text <- set_names(rep(list(element_blank()), length(theme_text_elements$strip_text_pt)),
                                   theme_text_elements$strip_text_pt)
    restyled_plot <- restyled_plot + do.call(theme, hidden_strip_text)
  }
  if ("point labels" %in% moved_elements) {
    # Text-label layers (geom_text, geom_label, ggrepel; e.g. Seurat's cluster
    # names from label = TRUE) are dropped; their words are recorded. Points,
    # colours and every other layer stay as they are.
    is_text_layer <- vapply(restyled_plot$layers, \(plot_layer) inherits(plot_layer$geom, text_geom_classes), TRUE)
    if (any(is_text_layer)) {
      built_layers <- ggplot_build(restyled_plot)$data
      point_labels <- unlist(map(built_layers[is_text_layer], \(layer_data) as.character(layer_data$label)))
      record_moved_labels(object$assembly_state, object$plot_id, point_labels, "point label")
      restyled_plot$layers <- restyled_plot$layers[!is_text_layer]
    }
  }
  restyled_plot
}

# Label wording from the table (labs() changes label text only). A typed \n
# (backslash, n) in the table means a line break; "/" is kept as written.
build_label_wording <- function(panel_specs, plot_id) {
  wording_fields <- c(title_text = "title", subtitle_text = "subtitle", caption_text = "caption",
                      x_title_text = "x", y_title_text = "y")
  label_wording <- list()
  for (spec_field in names(wording_fields)) {
    wording <- lookup_panel_spec(panel_specs, plot_id, spec_field, default = NA_character_)
    if (!is.na(wording)) {
      label_wording[[wording_fields[[spec_field]]]] <- str_replace_all(wording, "\\s*\\\\n\\s*", "\n")
    }
  }
  label_wording
}
registerS3method("ggplot_add", "figure_style_request", add_figure_style_to_plot,
                 envir = asNamespace("ggplot2"))
