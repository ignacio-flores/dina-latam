# Four entry points for an update; execution remains in the existing runner.
# The same text is printed by commands and kept on screen by terminal menus.

# Stata is normally configured once per update.  Keep this separate from
# dina_stata_status(): that helper intentionally describes the benchmark
# configuration for `dina doctor`, while the workspace must honour an active
# update's override before proposing a command to save.
dina_settings_stata_status <- function(root, session = dina_load_session(root = root)) {
  config <- dina_session_config(session, root, expand_env = FALSE)
  environment <- trimws(Sys.getenv("DINA_STATA_CMD", unset = ""))
  configured <- trimws(config$stata$command %||% "")
  configured <- if (identical(configured, "${DINA_STATA_CMD}")) "" else configured
  command <- if (nzchar(environment)) environment else configured
  source <- if (nzchar(environment)) "DINA_STATA_CMD" else if (nzchar(configured)) "this update's configuration" else "not configured"
  available <- nzchar(command) && dina_command_available(command)
  discovered <- if (!available) dina_discover_stata() else list(command = "", source = "none")
  proposal <- if (!nzchar(environment) && !available) discovered$command %||% "" else ""
  list(
    command = command,
    source = source,
    configured = nzchar(command),
    available = available,
    environment = environment,
    proposal = proposal,
    proposal_source = discovered$source %||% "none"
  )
}

dina_settings_stata_lines <- function(root, session = dina_load_session(root = root)) {
  stata <- dina_settings_stata_status(root, session)
  bold <- function(x) if (dina_cli_has("cli")) cli::style_bold(x) else x
  warning <- function(x) if (dina_cli_has("cli")) cli::col_yellow(x) else x
  if (isTRUE(stata$available)) {
    return(c(bold("  Stata"), paste("    Command:", stata$command),
      dina_cli_dim(paste("    Source:", stata$source))))
  }
  if (nzchar(stata$environment)) {
    return(c(bold("  Stata"), warning("    DINA_STATA_CMD is set but is not runnable."),
      dina_cli_dim(paste("    Configured command:", stata$environment)),
      dina_cli_dim("    Update DINA_STATA_CMD in your shell, then reopen Configuration.")))
  }
  if (nzchar(stata$proposal)) {
    state <- if (isTRUE(stata$configured)) "    The configured Stata command is not runnable." else "    Stata is not configured for this update."
    detected_label <- if (isTRUE(stata$configured)) "    Detected replacement:" else "    Detected:"
    configured_line <- if (isTRUE(stata$configured)) dina_cli_dim(paste("    Configured command:", stata$command)) else character()
    return(c(bold("  Stata"), warning(state), configured_line, paste(detected_label, stata$proposal),
      dina_cli_dim("    Choose ‘Use detected Stata’ below to save it after confirmation.")))
  }
  c(bold("  Stata"), warning("    No runnable Stata executable was found."),
    dina_cli_dim("    Install Stata or set DINA_STATA_CMD, then reopen Configuration."))
}

# The same text is printed by commands and kept on screen by terminal menus.
dina_settings_lines <- function(root, session = dina_load_session(root = root), full = FALSE,
                                check = dina_settings_check(root, session)) {
  cfg <- check$config
  base <- tryCatch(dina_config(root, expand_env = FALSE), error = function(e) list())
  bold <- function(x) if (dina_cli_has("cli")) cli::style_bold(x) else x
  accent <- function(x) if (dina_cli_has("cli")) cli::col_cyan(bold(x)) else x
  warning <- function(x) if (dina_cli_has("cli")) cli::col_yellow(x) else x
  wrap <- function(text, indent = 2L) strwrap(text, width = 78, indent = indent, exdent = indent)
  label <- function(x) if (length(x)) paste(unlist(x), collapse = ", ") else "Not set"
  pad <- function(text, width) paste0(text, strrep(" ", max(0L, width - nchar(text, type = "width"))))
  changed <- FALSE
  # Alignment communicates comparison even when terminal colors are disabled.
  pair <- function(name, old, new) {
    different <- !identical(old, new)
    changed <<- changed || different
    shown <- paste0(new, if (different) " *" else "")
    if (nchar(name) <= 20L && nchar(old) <= 20L && nchar(shown) <= 28L) {
      return(paste0("  ", pad(name, 20L), dina_cli_dim(pad(old, 22L)),
        if (different) accent(shown) else bold(shown)))
    }
    c(paste0("  ", name), dina_cli_dim(wrap(paste("Benchmark:", old), 4L)),
      if (different) accent(wrap(paste("This update:", shown), 4L)) else bold(wrap(paste("This update:", shown), 4L)))
  }
  years <- function(config) paste0(config$years$first %||% "?", "–", config$years$last %||% "?")
  lines <- c(bold(paste("Configuration", session$id %||% "Benchmark", sep = " · ")), "")
  if (length(check$errors)) lines <- c(lines, warning("! Settings need attention"),
    unlist(lapply(check$errors, wrap)), "")
  lines <- c(lines, dina_cli_dim(sprintf("  %-20s%-22s%s", "", "Benchmark", if (is.null(session)) "Effective" else "This update")),
    pair("Run years", years(base), years(cfg)),
    pair("Comparison through", label(base$export_validation$last_year), label(cfg$export_validation$last_year)))
  countries <- label(cfg$countries)
  lines <- c(lines, "", if (identical(countries, label(base$countries))) wrap(paste("Countries:", countries)) else pair("Countries", label(base$countries), countries))
  baseline <- cfg$export_validation$previous_update_file %||% ""
  old_baseline <- base$export_validation$previous_update_file %||% ""
  filename <- function(path) if (nzchar(path)) basename(path) else "Not selected"
  # Equal basenames in separate folders still need visibly distinct identities.
  same_name <- !identical(old_baseline, baseline) && identical(filename(old_baseline), filename(baseline))
  old_name <- if (same_name) old_baseline else filename(old_baseline)
  new_name <- if (same_name) baseline else filename(baseline)
  lines <- c(lines, "", bold("  Comparison baseline"))
  if (!is.null(session) && !identical(old_baseline, baseline)) {
    changed <- TRUE
    baseline_line <- function(title, name) {
      if (nchar(name, type = "width") <= 57L) paste0("    ", pad(title, 13L), name)
      else c(paste0("    ", title), wrap(name, 6L))
    }
    lines <- c(lines, dina_cli_dim(baseline_line("Benchmark:", old_name)), accent(baseline_line("This update:", paste(new_name, "*"))))
  } else lines <- c(lines, bold(wrap(new_name, 4L)))
  if (length(check$baseline)) {
    problems <- check$baseline
    missing <- startsWith(problems, "Baseline file is missing:")
    problems[missing] <- "Baseline file missing. Select an available file before export."
    lines <- c(lines, warning(unlist(lapply(paste("!", problems), wrap))),
      dina_cli_dim("    Affects final export; source inspection is available."))
  }
  validation <- dina_config_validation_state(root, session, check)
  lines <- c(lines, "", bold("  Validation"), paste("    Status:", validation$label),
    dina_cli_dim(wrap(paste("    ", validation$detail), 4L)),
    dina_cli_dim(wrap("Runtime readiness is checked during configuration validation and is required before every pipeline task.", 4L)))
  if (nzchar(validation$action %||% "")) {
    lines <- c(lines, dina_cli_dim(paste("    Next:", validation$action)))
  }
  lines <- c(lines, "", dina_settings_stata_lines(root, session))
  rows <- if (!is.null(session) && !length(check$errors)) dina_config_override_diff_rows(root, dina_config_override(session, root)) else data.frame()
  if (nrow(rows)) {
    other <- rows[rows$current != rows$proposed & !rows$key %in% c("years.first", "years.last", "countries",
      "export_validation.last_year", "export_validation.previous_update_file", "export_validation.previous_update_date"), , drop = FALSE]
    if (nrow(other)) {
      labels <- c(run.lang = "Language", run.units = "Run units", run.steps = "Run steps", run.debug = "Debug",
        run.bfm_replace = "Replace BFM", export_validation.unit = "Comparison unit", export_validation.steps = "Comparison steps")
      lines <- c(lines, "", bold("  Other changes"))
      for (i in seq_len(nrow(other))) {
        name <- if (other$key[i] %in% names(labels)) labels[[other$key[i]]] else gsub("[._]", " ", other$key[i])
        lines <- c(lines, pair(name, other$current[i], other$proposed[i]))
      }
    }
  }
  lines <- c(lines, "", if (changed) dina_cli_dim("  * Changed from benchmark"),
    dina_cli_dim(if (is.null(session)) "  No active update. Showing benchmark defaults."
      else "  Benchmark: config/dina.yml. Edits apply to this update only."))
  if (full) {
    lines <- c(lines, "", bold("Files and origins"),
      paste("Benchmark defaults:", dina_relative(dina_config_path(root), root)))
    if (!is.null(session)) lines <- c(lines, "This update's editable file:", paste0("  ", dina_relative(dina_session_config_override_path(session$id, root), root)))
    lines <- c(lines, "Effective settings = benchmark defaults + this update's overrides.",
      paste("Benchmark baseline:", if (nzchar(old_baseline)) old_baseline else "Not selected"),
      paste("Update baseline:", if (nzchar(baseline)) baseline else "Not selected"))
    if (nrow(rows)) for (i in which(rows$current != rows$proposed)) lines <- c(lines, paste(rows$key[i], ":", rows$current[i], "->", rows$proposed[i]))
    lines <- c(lines, "", "Effective YAML:", strsplit(yaml::as.yaml(cfg), "\n")[[1]])
  }
  lines
}

dina_settings_print <- function(root, session = dina_load_session(root = root), full = FALSE, validate_baseline = TRUE,
                                validate_runtime = FALSE, progress = NULL) {
  check <- dina_settings_check(root, session, validate_baseline = validate_baseline,
    validate_runtime = validate_runtime, progress = progress)
  if (isTRUE(validate_baseline)) {
    session <- dina_record_config_validation(root, session, check, progress = progress)
    dina_progress(progress, if (check$valid) "Configuration validation passed." else "Configuration validation needs attention.")
  }
  for (line in dina_settings_lines(root, session, full, check)) dina_cli_cat(line)
  invisible(check)
}

dina_settings_choose_baseline <- function(root, session, input = "stdin", is_terminal = isatty(stdin())) {
  cfg <- dina_session_config(session, root, expand_env = FALSE)
  if (!length(dina_baseline_check(cfg$export_validation$previous_update_file, root))) return(invisible(session))
  candidates <- dina_baseline_candidates(root)
  if (!length(candidates)) return(invisible(session))
  selected <- if (length(candidates) == 1L) candidates[[1]] else NULL
  if (is.null(selected) && is_terminal) selected <- dina_menu_select("Choose one comparison baseline",
    lapply(candidates, function(path) dina_menu_action(path, path)), input = input, is_terminal = is_terminal)
  if (is.null(selected) || identical(selected, "quit")) return(invisible(session))
  # Preserve comments and user formatting; replace only this scalar in the editor file.
  path <- dina_session_config_override_path(session$id, root)
  lines <- readLines(path, warn = FALSE)
  value <- trimws(yaml::as.yaml(list(previous_update_file = selected)))
  hit <- which(grepl("^  previous_update_file:", lines))
  if (length(hit) == 1L) lines[hit] <- paste0("  ", value) else {
    section <- which(grepl("^export_validation:", lines))
    if (length(section) == 1L) lines <- append(lines, paste0("  ", value), after = section) else lines <- c(lines, "export_validation:", paste0("  ", value))
  }
  date <- dina_update_extract_wid_update_date(selected)
  if (!nzchar(date)) date <- basename(selected)
  if (nzchar(date)) {
    hit <- which(grepl("^  previous_update_date:", lines))
    if (length(hit) == 1L) lines[hit] <- paste0("  previous_update_date: ", date)
  }
  writeLines(lines, path)
  session$config_override_hash <- dina_hash_file(path)
  dina_save_session(session, root)
  invisible(session)
}

dina_workspace_config <- function(root, input = "stdin", is_terminal = isatty(stdin())) {
  notice <- character()
  repeat {
    session <- dina_load_session(root = root)
    check <- dina_settings_check(root, session, validate_baseline = FALSE)
    context <- c(dina_settings_lines(root, session, check = check), notice)
    if (!is_terminal || is.null(session)) {
      for (line in context) dina_cli_cat(line)
      return(invisible(NULL))
    }
    stata <- dina_settings_stata_status(root, session)
    actions <- list(
      dina_menu_action("edit", "Open update file in editor", command = "dina update config edit",
        description = "Opens the update YAML in a graphical editor; save there, then validate here."),
      dina_menu_action("check", "Validate configuration and baseline", command = "dina update config check",
        description = "Checks the settings, baseline fields, and comparison keys. This may take a moment.")
    )
    if (!is.null(session) && nzchar(stata$proposal)) {
      actions <- c(actions, list(dina_menu_action("stata", "Use detected Stata", command = stata$proposal,
        description = "Shows the detected executable and asks before saving it for this update.")))
    }
    actions <- c(actions, list(
      dina_menu_action("full", "Details and file paths", command = "dina update config show --full"),
      dina_menu_action("back", "Back to workspace")))
    action <- dina_menu_select("Configuration actions", actions, prompt = "Review the settings above. q returns to the workspace.",
      input = input, is_terminal = is_terminal, context = context)
    if (is.null(action) || action %in% c("quit", "back")) return(invisible(NULL))
    notice <- character()
    if (action == "edit") {
      dina_update_config_edit(session, root)
      notice <- "Configuration file opened in your editor. Save it there, then validate configuration and baseline."
    } else if (action == "stata") {
      dina_cli_header("Configure Stata")
      dina_cli_cat(sprintf("Detected executable: %s", stata$proposal))
      dina_cli_cat(dina_cli_dim("This is saved only in the active update's configuration override. It does not run Stata."))
      confirmed <- dina_menu_confirm("Configure Stata", "Save this executable for the active update?",
        default = FALSE, input = input, is_terminal = is_terminal, preserve_screen = TRUE)
      if (confirmed) {
        dina_session_config_set(session, root, "stata.command", stata$proposal)
        notice <- paste("Stata configured for this update:", stata$proposal)
      } else {
        notice <- "Stata configuration was left unchanged."
      }
    } else if (action == "full") {
      dina_settings_print(root, session, full = TRUE, validate_baseline = FALSE)
      dina_cli_prompt_value("Press Enter to return to Configuration: ", input = input, is_terminal = is_terminal)
    } else {
      checked <- dina_cli_validate_configuration(root, session)
      notice <- if (checked$valid) character() else "Configuration needs attention; correct the issues shown above."
      dina_cli_prompt_value("Press Enter to return to Configuration: ", input = input, is_terminal = is_terminal)
    }
  }
}

dina_pipeline_snapshot <- function(root, session = dina_load_session(root = root), inspect_files = TRUE) {
  tasks <- dina_task_map(root)
  rows <- dina_review_bind(lapply(tasks, function(task) {
    files <- if (inspect_files) dina_task_status(task, root, session = NULL) else list(status = "Not inspected")
    run <- session$task_runs[[task$id]]
    data.frame(task = task$id, recorded_run = run$status %||% "No recorded run", ended = run$ended_at %||% "",
      files = files$status, stringsAsFactors = FALSE)
  }))
  runs <- session$task_runs %||% list()
  latest <- if (length(runs)) names(runs)[order(vapply(runs, function(run) run$ended_at %||% "", character(1)), decreasing = TRUE)[1L]] else NULL
  attention <- if (!inspect_files) NA_integer_ else if (nrow(rows)) sum(rows$recorded_run == "failed" | !rows$files %in% c("current", "inactive")) else 0L
  list(rows = rows, latest = latest, run = runs[[latest %||% ""]], attention = attention, failures = sum(rows$recorded_run == "failed"))
}

dina_pipeline_print <- function(root) {
  snapshot <- dina_pipeline_snapshot(root)
  dina_cli_header("Pipeline")
  if (is.null(snapshot$latest)) dina_cli_cat("No CLI run recorded in this update.") else {
    dina_cli_cat(sprintf("Last recorded run: %s — %s · %s", snapshot$latest, snapshot$run$status, snapshot$run$ended_at))
    dina_cli_cat(paste("Run logs:", file.path("output", "run_logs", snapshot$run$run_id)))
  }
  dina_cli_cat("File observations are separate from recorded execution outcomes.")
  dina_review_show_rows(snapshot$rows, limit = max(1L, nrow(snapshot$rows)))
  dina_cli_cat("Explain: dina run why TASK\nExecute: dina run TASK")
  invisible(snapshot)
}

dina_workspace_lines <- function(root, session = dina_load_session(root = root)) {
  check <- dina_settings_check(root, session, validate_baseline = FALSE); cfg <- check$config
  validation <- dina_config_validation_state(root, session, check)
  pipeline <- tryCatch(dina_pipeline_snapshot(root, session, inspect_files = FALSE), error = function(e) list(failures = NA, latest = NULL))
  results <- dina_results_inventory(root, verify = FALSE)
  families <- vapply(names(dina_review_families()), function(family) {
    state <- dina_review_family_status(root, family)
    paste0("    ", toupper(family), ": ", dina_review_state_label(state))
  }, character(1))
  c("DINA — Update workspace", "",
    "  Active update", paste0("    ", session$id %||% "none"), "",
    "Configuration", paste0("  Status: ", validation$label),
    "  Scope", paste0("    Countries: ", paste(unlist(cfg$countries), collapse = ", ")),
    sprintf("    Years: %s–%s", cfg$years$first %||% "?", cfg$years$last %||% "?"),
    paste0("  ", validation$detail), if (nzchar(validation$action %||% "")) paste0("  Next: ", validation$action) else NULL, "",
    "Sources", "  Review state", families, "",
    "Pipeline", sprintf("  Recorded failures: %s", pipeline$failures),
    paste("  Last recorded run:", if (is.null(pipeline$latest)) "none" else paste(pipeline$latest, pipeline$run$status)),
    "  File freshness: checked when you open Pipeline.", "",
    "Results", sprintf("  Final-series files: %s", length(results$series)),
    sprintf("  Comparison graphs: %s", nrow(results$graphs)),
    "  Artifact verification: Not inspected; open Results to verify freshness.")
}

dina_workspace_home <- function(root, input = "stdin", is_terminal = isatty(stdin())) {
  repeat {
    session <- dina_load_session(root = root)
    lines <- dina_workspace_lines(root, session)
    proposal <- tryCatch(dina_state_proposal(dina_dashboard_state_fast(session, root)), error = function(e) NULL)
    if (!is.null(proposal)) lines <- c(lines, "", "Next step", paste("  Command:", proposal$command), paste("  Why:", proposal$why))
    if (!is_terminal) {
      for (line in lines) dina_cli_cat(line)
      dina_cli_cat("\ndina update config show · dina sources · dina run list · dina results\nOther actions: dina commands")
      return(invisible(proposal))
    }
    actions <- list(
      dina_menu_action("config", "Configuration", command = "dina update config show"),
      dina_menu_action("sources", "Sources", command = "dina sources"),
      dina_menu_action("pipeline", "Pipeline", command = "dina run list"),
      dina_menu_action("results", "Results", command = "dina results"),
      dina_menu_action("commands", "Update actions and utilities", command = "dina commands"))
    selected <- dina_menu_select("Workspace", actions, input = input, is_terminal = is_terminal, context = lines)
    if (is.null(selected) || selected == "quit") return(invisible(NULL))
    tryCatch(switch(selected, config = dina_workspace_config(root, input, is_terminal), sources = dina_cmd_sources(root, character(), input, is_terminal),
      pipeline = dina_workspace_pipeline(root, input, is_terminal), results = dina_results_menu(root, input, is_terminal),
      commands = dina_command_browser(root, proposal, input, is_terminal)), error = function(e) dina_cli_warn(conditionMessage(e)))
  }
}

dina_workspace_pipeline <- function(root, input = "stdin", is_terminal = isatty(stdin())) {
  repeat {
    snapshot <- dina_pipeline_print(root)
    if (!is_terminal) return(invisible(snapshot))
    selected <- dina_menu_select("Explain a task", lapply(snapshot$rows$task, function(id) dina_menu_action(id, id, command = paste("dina run why", id))), input = input, is_terminal = is_terminal)
    if (is.null(selected) || selected == "quit") return(invisible(snapshot))
    dina_cmd_tasks(root, c("why", selected))
  }
}
