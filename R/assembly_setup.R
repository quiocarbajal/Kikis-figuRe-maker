# Figure assembly toolkit: setting up a paper project
#
# Copies the toolkit into <project>/figure_assembly/ so each paper is
# self-contained: it keeps working, unchanged, even if the toolkit changes
# later. Updating a paper to a newer toolkit is a deliberate, separate step.

#' Install the figure assembly folder into a paper project
#'
#' @param project_directory The paper project's folder.
#' @param toolkit_directory This toolkit's folder (a local clone of the
#'   GitHub repository).
#' @param journal_id A journal_id from presets/journal_presets.tsv.
#' @return The path of the created figure_assembly folder.
install_figure_assembly <- function(project_directory, toolkit_directory, journal_id = "nature_communications") {
  assembly_directory <- file.path(project_directory, "figure_assembly")
  if (dir.exists(assembly_directory)) {
    stop(assembly_directory, " already exists; nothing was changed. Remove or rename it first ",
         "if you really want a fresh installation.", call. = FALSE)
  }
  journal_presets <- read_journal_presets(file.path(toolkit_directory, "presets", "journal_presets.tsv"))
  select_journal_preset(journal_presets, journal_id) # stops if the journal is unknown

  template_directory <- file.path(toolkit_directory, "templates", "figure_assembly")
  dir.create(assembly_directory, recursive = TRUE)
  template_files <- list.files(template_directory, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  for (template_file in template_files) {
    dir.create(file.path(assembly_directory, dirname(template_file)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(template_directory, template_file), file.path(assembly_directory, template_file))
  }
  dir.create(file.path(assembly_directory, "R"), showWarnings = FALSE)
  toolkit_code_files <- list.files(file.path(toolkit_directory, "R"), pattern = "^assembly_.*\\.R$", full.names = TRUE)
  file.copy(toolkit_code_files, file.path(assembly_directory, "R"))
  file.copy(file.path(toolkit_directory, "presets", "journal_presets.tsv"),
            file.path(assembly_directory, "settings", "journal_presets.tsv"))

  settings_path <- file.path(assembly_directory, "settings", "project_settings.tsv")
  project_settings_table <- read_text_table(settings_path)
  toolkit_version <- str_trim(read_lines(file.path(toolkit_directory, "VERSION"), n_max = 1))
  project_settings_table$value[project_settings_table$setting == "journal"] <- journal_id
  project_settings_table$value[project_settings_table$setting == "toolkit_version"] <- toolkit_version
  write_tsv(project_settings_table, settings_path, na = "")

  message("Installed figure assembly toolkit ", toolkit_version, " into ", assembly_directory,
          "\nNext: add the setup chunk to your figures document (see PROTOCOL.md, step 1).")
  invisible(assembly_directory)
}
