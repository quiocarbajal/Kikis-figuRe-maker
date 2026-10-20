# Figure assembly toolkit: project checks (reports, chunk labels, plot files)
#
# check_project() reads every report of the paper project that loads the
# toolkit, the tables, and the plot files in figure_assembly/drafting/, and
# lists what would make a figure wrong or is worth cleaning up. It only
# reports: it never deletes files, edits a report, or changes a table.
#
# start_figure_assembly() runs it at every render and writes the findings to
# the render log and to figure_assembly/drafting/project_checks.md, the record
# of the last render for people and AI assistants who did not see the log. A
# final render (draft_mode = FALSE) stops on a figure label used by more than
# one chunk, and at a figure chunk that draws several plots
# (make_plot_count_hook()), but only for figures that have a working canvas.
#
# Reports are read as text, without running knitr: chunk labels made by code
# and chunks in child documents are not seen (PROTOCOL.md, Known limitations).

library(tidyverse)

# Folders never searched for reports. Hidden folders (.git, .quarto) are left
# out too, and links to folders are not followed: they often point to raw data.
report_search_skipped_folders <- c("figure_assembly", "renv", "packrat", "node_modules")

# Most serious first. problem: the figure would be wrong; warning: worth
# acting on; note: clean-up only.
check_severity_levels <- c("problem", "warning", "note")

# Every check returns one row per finding, with these columns.
empty_check_findings <- function() {
  tibble(check_id = character(), severity = character(), location = character(),
         plot_id = character(), finding = character(), suggested_action = character())
}

# ---- Reading reports ---------------------------------------------------------

# Paths, relative to the project folder, of every .qmd and .Rmd file.
list_report_files <- function(project_directory) {
  search_folder <- function(relative_folder) {
    absolute_folder <- if (relative_folder == "") project_directory else file.path(project_directory, relative_folder)
    entry_names <- list.files(absolute_folder) # hidden files and folders are left out
    relative_paths <- if (relative_folder == "") entry_names else file.path(relative_folder, entry_names)
    absolute_paths <- file.path(project_directory, relative_paths)
    entry_is_folder <- dir.exists(absolute_paths)
    entry_is_link <- Sys.readlink(absolute_paths) != ""
    report_paths <- relative_paths[!entry_is_folder & str_detect(entry_names, regex("\\.(qmd|rmd)$", ignore_case = TRUE))]
    subfolder_paths <- relative_paths[entry_is_folder & !entry_is_link &
                                        !entry_names %in% report_search_skipped_folders]
    c(report_paths, list_c(map(subfolder_paths, search_folder)))
  }
  sort(search_folder(""))
}

# Label from a chunk header's options: {r fig-3a-umap, cache = TRUE} or
# {r, label = "fig-3a-umap"}. NA when the header has no label.
chunk_label_from_header <- function(header_options) {
  header_options <- str_trim(str_remove(coalesce(header_options, ""), "^\\s*,"))
  positional_label <- str_match(header_options, "^([^,=\\s]+)\\s*(,|$)")[, 2]
  if (!is.na(positional_label)) {
    return(str_remove_all(positional_label, "^['\"]|['\"]$"))
  }
  str_match(header_options, "(?:^|,)\\s*label\\s*=\\s*['\"]([^'\"]+)['\"]")[, 2]
}

# The parts of one report that the checks need: its R chunks (start line and
# label), the code lines inside R chunks, and the front matter lines.
read_report_structure <- function(report_lines) {
  line_count <- length(report_lines)
  front_matter_lines <- tibble(line_number = integer(), text = character())
  first_body_line <- 1L
  if (line_count > 0 && str_detect(report_lines[[1]], "^---\\s*$")) {
    closing_lines <- which(str_detect(report_lines, "^(---|\\.\\.\\.)\\s*$"))
    closing_lines <- closing_lines[closing_lines > 1]
    if (length(closing_lines) > 0) {
      front_matter_numbers <- seq_len(closing_lines[[1]] - 2L) + 1L
      front_matter_lines <- tibble(line_number = front_matter_numbers, text = report_lines[front_matter_numbers])
      first_body_line <- closing_lines[[1]] + 1L
    }
  }

  chunk_rows <- list()
  code_line_numbers <- integer()
  line_index <- first_body_line
  while (line_index <= line_count) {
    opening_fence <- str_match(report_lines[[line_index]], "^\\s*(`{3,})(.*)$")
    if (is.na(opening_fence[1, 1])) {
      line_index <- line_index + 1L
      next
    }
    # A block ends at a line of at least as many backticks as it opened with.
    closing_pattern <- paste0("^\\s*`{", nchar(opening_fence[1, 2]), ",}\\s*$")
    closing_index <- line_index + 1L
    while (closing_index <= line_count && !str_detect(report_lines[[closing_index]], closing_pattern)) {
      closing_index <- closing_index + 1L
    }
    r_chunk_header <- str_match(opening_fence[1, 3], "^\\s*\\{[rR]([ ,][^}]*)?\\}\\s*$")
    if (!is.na(r_chunk_header[1, 1])) {
      body_numbers <- seq_len(max(0L, min(closing_index, line_count + 1L) - line_index - 1L)) + line_index
      # Quarto options: "#|" lines at the top of the chunk.
      option_count <- match(FALSE, str_detect(report_lines[body_numbers], "^\\s*#\\|"), nomatch = length(body_numbers) + 1L) - 1L
      option_lines <- report_lines[body_numbers[seq_len(option_count)]]
      label_from_options <- str_match(option_lines, "^\\s*#\\|\\s*label\\s*:\\s*['\"]?([^'\"\\s]+)['\"]?\\s*$")[, 2]
      chunk_label <- coalesce(na.omit(label_from_options)[1], chunk_label_from_header(r_chunk_header[1, 2]))
      chunk_rows[[length(chunk_rows) + 1L]] <- tibble(chunk_line = line_index, label = chunk_label)
      code_line_numbers <- c(code_line_numbers, body_numbers[seq_along(body_numbers) > option_count])
    }
    line_index <- closing_index + 1L
  }

  list(
    chunks = bind_rows(tibble(chunk_line = integer(), label = character()), chunk_rows),
    code_lines = tibble(line_number = code_line_numbers, text = report_lines[code_line_numbers]),
    front_matter_lines = front_matter_lines
  )
}

# A report loads the toolkit when code in one of its R chunks calls
# start_figure_assembly() outside a comment.
report_loads_toolkit <- function(report_structure) {
  any(str_detect(report_structure$code_lines$text, "^[^#]*start_figure_assembly\\("))
}

# ---- The checks --------------------------------------------------------------

# A figure label used by more than one chunk: the plot file in drafting/ comes
# from whichever report was rendered last.
find_duplicate_figure_labels <- function(report_chunks) {
  report_chunks |>
    dplyr::filter(!is.na(figure)) |>
    dplyr::filter(n() > 1, .by = label) |>
    summarise(chunk_count = n(), chunk_locations = paste0(report, ":", chunk_line, collapse = ", "),
              .by = c(label, figure)) |>
    transmute(
      check_id = "duplicate_label", severity = "problem", location = chunk_locations, plot_id = label,
      finding = paste0("Figure label used by ", chunk_count, " chunks; the plot file in drafting/ comes ",
                       "from whichever report was rendered last."),
      suggested_action = paste0("Keep the label in one report. Rename the other chunk (e.g. prefix ",
                                "explored-) so that it no longer matches figure_label_pattern.")
    )
}

# knitr names plot files <label>-1.svg, <label>-2.svg, ... The chunk's latest
# render is the newest -1.svg in plots/ or final_plots/. Numbered files at least
# as new as that -1.svg come from the same render: the chunk draws several
# plots, and only -1.svg is assembled. All other numbered files are left from
# earlier renders and are not used.
find_plot_file_issues <- function(figure_plot_ids, assembly_directory) {
  plot_file_findings <- list()
  for (plot_index in seq_len(nrow(figure_plot_ids))) {
    plot_id <- figure_plot_ids$plot_id[[plot_index]]
    figure_folder <- figure_drafting_directory(assembly_directory, figure_plot_ids$figure[[plot_index]])
    plot_files <- map(file.path(figure_folder, c("plots", "final_plots")), \(folder_path) {
      file_names <- list.files(folder_path, pattern = paste0("^", str_escape(plot_id), "-[0-9]+\\.svg$"))
      tibble(folder_path = rep(folder_path, length(file_names)),
             plot_number = as.integer(str_match(file_names, "-([0-9]+)\\.svg$")[, 2]),
             modification_time = file.mtime(file.path(folder_path, file_names)))
    }) |> list_rbind()
    first_plot_files <- plot_files[plot_files$plot_number == 1L, ]
    if (nrow(first_plot_files) == 0 || all(plot_files$plot_number == 1L)) next
    latest_folder <- first_plot_files$folder_path[[which.max(first_plot_files$modification_time)]]
    for (current_folder in unique(plot_files$folder_path)) {
      folder_files <- plot_files[plot_files$folder_path == current_folder, ]
      first_plot_time <- folder_files$modification_time[folder_files$plot_number == 1L]
      extra_files <- folder_files[folder_files$plot_number > 1L, ]
      if (nrow(extra_files) == 0) next
      from_latest_render <- current_folder == latest_folder & length(first_plot_time) == 1 &
        extra_files$modification_time >= first_plot_time[1]
      if (any(from_latest_render)) {
        plot_file_findings[[length(plot_file_findings) + 1L]] <- tibble(
          check_id = "multi_plot_chunk", severity = "problem", location = current_folder, plot_id = plot_id,
          finding = paste0("The chunk drew ", 1L + sum(from_latest_render), " plots at its latest render (",
                           paste0("-", c(1L, sort(extra_files$plot_number[from_latest_render])), ".svg",
                                  collapse = ", "),
                           "); only -1.svg is assembled."),
          suggested_action = paste0("Combine the chunk's plots into one plot (PROTOCOL.md, \"Several source ",
                                    "reports\", item 3).")
        )
      }
      if (any(!from_latest_render)) {
        earlier_render_files <- paste0("-", sort(extra_files$plot_number[!from_latest_render]), ".svg", collapse = ", ")
        plot_file_findings[[length(plot_file_findings) + 1L]] <- tibble(
          check_id = "stale_plot_files", severity = "note", location = current_folder, plot_id = plot_id,
          finding = if (current_folder == latest_folder) {
            paste0(earlier_render_files, " older than -1.svg: left by an earlier render, not used.")
          } else {
            paste0(earlier_render_files, ": left by an earlier render (the chunk's latest plots are in ",
                   basename(latest_folder), "/), not used.")
          },
          suggested_action = "Delete these files if you like; drafting/ is disposable."
        )
      }
    }
  }
  bind_rows(empty_check_findings(), plot_file_findings)
}

# Table rows whose plot id is the label of no chunk in a report that loads the
# toolkit. Panels not drawn by R (plot_kind = external) have no chunk by design.
find_leftover_rows <- function(panel_specs, chunk_labels, panel_specs_location) {
  panel_specs |>
    dplyr::filter(!is.na(plot_id), coalesce(na_if(plot_kind, ""), "ggplot") != "external",
                  !plot_id %in% chunk_labels) |>
    transmute(
      check_id = "leftover_row", severity = "warning", location = panel_specs_location, plot_id,
      finding = "Row in panel_specs.tsv, but no chunk in a report that loads the toolkit has this label.",
      suggested_action = paste0("Delete the row if the plot was removed. If its chunk was renamed, rename ",
                                "the row instead, so that its position and settings carry over.")
    )
}

# A report-wide fig.path keeps every plot that is not part of a figure as a
# file in that folder, at every render. Looked for in R chunk code
# (knitr::opts_chunk$set), in the front matter, and in _quarto.yml or
# _metadata.yml next to the project or the report.
find_report_wide_fig_paths <- function(report_structures, project_directory) {
  fig_path_lines <- imap(report_structures, \(report_structure, report_path) {
    code_matches <- dplyr::filter(report_structure$code_lines,
                                  !str_detect(text, "^\\s*#"), str_detect(text, "fig\\.path\\s*="))
    front_matter_matches <- dplyr::filter(report_structure$front_matter_lines,
                                          !str_detect(text, "^\\s*#"), str_detect(text, "fig[.-]path\\s*:"))
    bind_rows(code_matches, front_matter_matches) |> mutate(file_path = report_path)
  }) |> list_rbind()
  report_paths <- names(report_structures) %||% character()
  project_file_paths <- unique(c("_quarto.yml", file.path(dirname(report_paths), "_metadata.yml")))
  project_file_paths <- str_remove(project_file_paths, "^\\./")
  project_file_lines <- map(project_file_paths[file.exists(file.path(project_directory, project_file_paths))],
                            \(project_file_path) {
    file_lines <- read_lines(file.path(project_directory, project_file_path))
    matching_numbers <- which(str_detect(file_lines, "^[^#]*fig[.-]path\\s*:"))
    tibble(line_number = matching_numbers, text = file_lines[matching_numbers], file_path = project_file_path)
  }) |> list_rbind()
  bind_rows(tibble(line_number = integer(), text = character(), file_path = character()),
            fig_path_lines, project_file_lines) |>
    transmute(
      check_id = "report_fig_path", severity = "warning", location = paste0(file_path, ":", line_number),
      plot_id = NA_character_,
      finding = paste0("Sets a report-wide fig.path (", str_remove(str_trim(text), ",$"), "): every plot that is not part of ",
                       "a figure is also kept as a file in that folder at each render."),
      suggested_action = paste0("Remove the setting unless those files are wanted. Files already in the ",
                                "folder stay until you delete them.")
    )
}

# ---- Entry points ------------------------------------------------------------

#' Check the reports, tables and plot files of a paper project
#'
#' @param project_directory The paper project's folder.
#' @param assembly_directory Its figure_assembly folder.
#' @param print_findings Print a readable list of the findings.
#' @return Invisibly, a table with one row per finding: check_id, severity
#'   (problem, warning, note), location, plot_id, finding, suggested_action.
check_project <- function(project_directory = ".",
                          assembly_directory = str_remove(file.path(project_directory, "figure_assembly"), "^\\./"),
                          print_findings = TRUE) {
  project_settings <- read_project_settings(file.path(assembly_directory, "settings", "project_settings.tsv"))
  panel_specs_path <- file.path(assembly_directory, "specs", "panel_specs.tsv")
  panel_specs <- read_panel_specs(panel_specs_path)

  report_paths <- list_report_files(project_directory)
  report_structures <- map(report_paths, \(report_path) read_report_structure(
    read_lines(file.path(project_directory, report_path))
  )) |> set_names(report_paths)
  toolkit_reports <- keep(report_structures, report_loads_toolkit)

  # unname(): bind_rows() would read a named list of tables as columns.
  report_chunks <- bind_rows(
    tibble(chunk_line = integer(), label = character(), report = character()),
    unname(imap(toolkit_reports, \(report_structure, report_path) mutate(report_structure$chunks, report = report_path)))
  ) |>
    mutate(figure = map_chr(label, \(chunk_label) {
      if (is.na(chunk_label)) NA_character_ else figure_for_plot(chunk_label, panel_specs, project_settings$figure_label_pattern)
    }))

  figure_plot_ids <- bind_rows(
    dplyr::filter(report_chunks, !is.na(figure)) |> dplyr::select(plot_id = label, figure),
    dplyr::filter(panel_specs, !is.na(figure), coalesce(na_if(plot_kind, ""), "ggplot") != "external") |>
      dplyr::select(plot_id, figure)
  ) |> dplyr::distinct()

  findings <- bind_rows(
    empty_check_findings(),
    find_duplicate_figure_labels(report_chunks),
    find_plot_file_issues(figure_plot_ids, assembly_directory),
    find_report_wide_fig_paths(toolkit_reports, project_directory)
  )
  if (length(toolkit_reports) == 0) {
    findings <- bind_rows(findings, tibble(
      check_id = "no_reports", severity = "warning", location = project_directory, plot_id = NA_character_,
      finding = "No .qmd or .Rmd file here calls start_figure_assembly(), so chunk labels were not checked.",
      suggested_action = "Run check_project() from the paper project folder (or pass project_directory)."
    ))
  } else {
    findings <- bind_rows(findings, find_leftover_rows(panel_specs, na.omit(report_chunks$label), panel_specs_path))
  }
  findings <- dplyr::arrange(findings, match(severity, check_severity_levels), check_id, location, plot_id)
  if (print_findings) cat(format_check_findings(findings), sep = "\n")
  invisible(findings)
}

# Readable lines for a findings table (printed by check_project(), and written
# to the render log by start_figure_assembly()). Without notes, the notes are
# only counted, so that clean-up items do not crowd the render log.
format_check_findings <- function(findings, include_notes = TRUE) {
  if (nrow(findings) == 0) {
    return("Figure assembly checks: nothing to report.")
  }
  severity_counts <- map_chr(check_severity_levels, \(severity_level) {
    level_count <- sum(findings$severity == severity_level)
    if (level_count == 0) NA_character_ else paste0(level_count, " ", severity_level, if (level_count > 1) "s")
  })
  listed_findings <- if (include_notes) findings else dplyr::filter(findings, severity != "note")
  finding_lines <- pmap(listed_findings, \(check_id, severity, location, plot_id, finding, suggested_action) c(
    paste0("- ", toupper(severity), " [", check_id, "] ", location, if (!is.na(plot_id)) paste0(" (", plot_id, ")")),
    paste0("  ", finding),
    paste0("  Action: ", suggested_action)
  )) |> list_c()
  unlisted_note_count <- nrow(findings) - nrow(listed_findings)
  c(paste0("Figure assembly checks: ", paste(na.omit(severity_counts), collapse = ", ")), finding_lines,
    if (unlisted_note_count > 0) {
      paste0("- ", unlisted_note_count, " clean-up note", if (unlisted_note_count > 1) "s",
             ": run check_project() to list ", if (unlisted_note_count > 1) "them." else "it.")
    })
}

#' Find the lines of every report that mention a plot id
#'
#' Useful before renaming a chunk: its label, cross-references (@label),
#' chunk options that name it (ref.label, dependson), and any other mention.
#'
#' @param plot_id A chunk label, e.g. "fig-3a-umap-by-genotype".
#' @param project_directory The paper project's folder.
#' @return A table with location (file:line), reference_kind and line_text.
find_plot_id_references <- function(plot_id, project_directory = ".") {
  # The id must not be part of a longer label: "fig-3a" does not match "fig-3a-umap".
  id_pattern <- paste0("(?<![A-Za-z0-9_.-])", str_escape(plot_id), "(?![A-Za-z0-9_-])")
  bind_rows(
    tibble(location = character(), line_text = character()),
    map(list_report_files(project_directory), \(report_path) {
      report_lines <- read_lines(file.path(project_directory, report_path))
      matching_numbers <- which(str_detect(report_lines, id_pattern))
      tibble(location = paste0(report_path, ":", matching_numbers), line_text = str_trim(report_lines[matching_numbers]))
    })
  ) |>
    mutate(
      header_label = map_chr(str_match(line_text, "^`{3,}\\s*\\{[rR]([ ,][^}]*)?\\}")[, 2], chunk_label_from_header),
      is_chunk_option = str_detect(line_text, "^`{3,}\\s*\\{[rR]") | str_detect(line_text, "^#\\|"),
      reference_kind = case_when(
        header_label %in% plot_id | str_detect(line_text, "^#\\|\\s*label\\s*:") ~ "chunk label",
        str_detect(line_text, paste0("@", str_escape(plot_id))) ~ "cross-reference",
        is_chunk_option ~ "chunk option (e.g. ref.label, dependson)",
        .default = "other mention"
      ),
      .before = line_text
    ) |>
    dplyr::select(location, reference_kind, line_text)
}

# ---- Findings file -----------------------------------------------------------

# The record of the last render: which report, which mode, what the checks
# found, and what happened during the render. Rewritten at every render (and
# only during a render, so running the setup chunk in the console does not
# replace it). The render log shows the same findings, but nobody may read it.
project_checks_file_path <- function(assembly_directory) {
  file.path(assembly_directory, "drafting", "project_checks.md")
}

# The report being rendered, relative to the project folder.
rendered_report_path <- function(project_directory) {
  input_path <- knitr::current_input(dir = TRUE)
  if (is.null(input_path)) {
    return("unknown")
  }
  str_remove(normalizePath(input_path, mustWork = FALSE),
             paste0("^", str_escape(normalizePath(project_directory)), "/"))
}

write_project_checks_file <- function(assembly_directory, project_directory, project_findings, draft_mode) {
  project_settings <- read_project_settings(file.path(assembly_directory, "settings", "project_settings.tsv"))
  file_lines <- c(
    "# Figure assembly checks at the last render", "",
    paste0("Written by start_figure_assembly() at every render and replaced at the next one. ",
           "Run check_project() for a fresh list."), "",
    paste0("- Report: ", rendered_report_path(project_directory)),
    paste0("- Mode: ", if (draft_mode) "draft (draft_mode = TRUE)" else "final (draft_mode = FALSE)"),
    paste0("- Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste0("- Toolkit version: ", coalesce(na_if(project_settings$toolkit_version %||% "", ""), "unknown")), "",
    "## At the start of the render", "",
    format_check_findings(project_findings), "",
    "## Events during the render", "",
    paste0("Canvas comparisons of a final render, figure chunks that drew more than one plot, and the reason ",
           "a final render stopped. A render that ended early for another reason (e.g. an error in the ",
           "analysis code) leaves no entry here."), ""
  )
  dir.create(dirname(project_checks_file_path(assembly_directory)), recursive = TRUE, showWarnings = FALSE)
  write_lines(file_lines, project_checks_file_path(assembly_directory))
}

# Adds an entry under "Events during the render" (only while knitr renders).
record_render_event <- function(assembly_directory, event_text) {
  if (!isTRUE(getOption("knitr.in.progress"))) {
    return(invisible(FALSE))
  }
  write_lines(paste0("- ", str_replace_all(str_trim(event_text), "\n", "\n  ")),
              project_checks_file_path(assembly_directory), append = TRUE)
  invisible(TRUE)
}

# ---- During a render ---------------------------------------------------------

# Figures whose drafting folder holds a working canvas: they are being
# assembled, so a final render stops for their problems.
figures_with_canvas <- function(assembly_directory, figure_names) {
  figure_names <- unique(na.omit(figure_names))
  figure_names[!is.na(map_chr(figure_names, \(figure_name) latest_working_canvas(assembly_directory, figure_name)))]
}

# knitr chunk hook for figure chunks. After a chunk, knitr knows how many plots
# it drew (fig.num); only the first is assembled. More than one is written to
# the render log, and stops a final render when the figure has a working canvas.
# The option's value is the chunk's figure name (set by make_chunk_option_hook()).
make_plot_count_hook <- function(assembly_directory, draft_mode) {
  function(before, options) {
    if (before || is.null(options$fig.num) || options$fig.num <= 1) {
      return(invisible(NULL))
    }
    plot_count_message <- paste0("Chunk '", options$label, "' drew ", options$fig.num, " plots; only the first ",
                                 "is assembled. Combine them into one plot (PROTOCOL.md, \"Several source ",
                                 "reports\", item 3).")
    figure_name <- options$figure_assembly_plot_count
    if (!draft_mode && length(figures_with_canvas(assembly_directory, figure_name)) > 0) {
      record_render_event(assembly_directory, paste0("Final render stopped at this chunk. ", plot_count_message))
      stop("Figure assembly, final render stopped: ", plot_count_message, call. = FALSE)
    }
    record_render_event(assembly_directory, plot_count_message)
    cat("Figure assembly: ", plot_count_message, "\n", file = stderr(), sep = "")
    invisible(NULL)
  }
}

# Run at the start of every render by start_figure_assembly(): writes the
# findings to the render log (messages from the hidden setup chunk would not
# reach it) and to the findings file, and stops a final render on duplicate
# labels of figures that have a working canvas.
run_project_checks_at_render <- function(assembly_directory, draft_mode) {
  project_directory <- dirname(normalizePath(assembly_directory))
  project_findings <- check_project(project_directory = project_directory,
                                    assembly_directory = assembly_directory, print_findings = FALSE)
  if (nrow(project_findings) > 0) {
    cat(format_check_findings(project_findings, include_notes = FALSE), sep = "\n", file = stderr())
  }
  if (isTRUE(getOption("knitr.in.progress"))) {
    write_project_checks_file(assembly_directory, project_directory, project_findings, draft_mode)
  }
  if (!draft_mode) {
    duplicate_findings <- dplyr::filter(project_findings, check_id == "duplicate_label")
    panel_specs <- read_panel_specs(file.path(assembly_directory, "specs", "panel_specs.tsv"))
    project_settings <- read_project_settings(file.path(assembly_directory, "settings", "project_settings.tsv"))
    duplicate_figures <- map_chr(duplicate_findings$plot_id, \(plot_id) {
      figure_for_plot(plot_id, panel_specs, project_settings$figure_label_pattern)
    })
    blocking_findings <- duplicate_findings[duplicate_figures %in% figures_with_canvas(assembly_directory, duplicate_figures), ]
    if (nrow(blocking_findings) > 0) {
      stop_reason <- paste0(
        "figure labels used by more than one chunk:\n",
        paste0("  ", blocking_findings$plot_id, " (", blocking_findings$location, ")", collapse = "\n"),
        "\nKeep each label in one report (PROTOCOL.md, \"Several source reports\", item 4)."
      )
      record_render_event(assembly_directory, paste0("Final render stopped before anything was drawn: ", stop_reason))
      stop("Figure assembly, final render stopped: ", stop_reason, call. = FALSE)
    }
  }
  invisible(project_findings)
}
