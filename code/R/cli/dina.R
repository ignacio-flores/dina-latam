#!/usr/bin/env Rscript

options(warn = 1)

source_file <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  prefix <- "--file="
  file <- sub(prefix, "", cmd[grepl(prefix, cmd, fixed = TRUE)][1])
  if (length(file) == 0 || is.na(file)) {
    return(file.path(getwd(), "code", "R", "cli", "dina.R"))
  }
  normalizePath(file, mustWork = TRUE)
}

script_dir <- dirname(source_file())
source(file.path(script_dir, "dina_lib.R"))
dina_cli_root <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)

dina_cli_has <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

dina_cli_cat <- function(...) {
  cat(paste0(...), "\n", sep = "")
}

dina_cli_header <- function(text) {
  if (dina_cli_has("cli")) cli::cli_h1(text) else dina_cli_cat("\n", text, "\n")
}

dina_cli_alert <- function(text) {
  if (dina_cli_has("cli")) cli::cli_alert_info(text) else dina_cli_cat("* ", text)
}

dina_cli_ok <- function(text) {
  if (dina_cli_has("cli")) cli::cli_alert_success(text) else dina_cli_cat("OK: ", text)
}

dina_cli_warn <- function(text) {
  if (dina_cli_has("cli")) cli::cli_alert_warning(text) else dina_cli_cat("WARN: ", text)
}

dina_cli_err <- function(text) {
  if (dina_cli_has("cli")) cli::cli_alert_danger(text) else dina_cli_cat("ERROR: ", text)
}

dina_usage <- function() {
  cat(
"Usage:
  dina [command]

Main commands:
  doctor                         Check packages, Stata, paths, notifications, data
  install [--yes] [--dry-run]     Install missing R dependencies interactively
  update start [YEAR]             Start an annual update session
  update resume|status            Recompute state and show next recommended action
  update checklist                Show update checklist
  update finalize [--force]       Freeze final outputs and manifests
  sources refresh|scan|review|diff|integrate
  tasks list|why TASK
  run [--task ID] [--stage STAGE] [--from ID] [--to ID] [--dry-run] [--force]
  config show|set KEY VALUE|edit|render [PATH]
  data check|pack|unpack ARCHIVE
  audit paths
  make export [PATH]
  notify test
  setup command

Plain `dina` opens the guided dashboard.
", sep = "")
}

dina_parse_flags <- function(args) {
  out <- list(positional = character())
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (grepl("^--", arg)) {
      key <- sub("^--", "", arg)
      if (grepl("=", key, fixed = TRUE)) {
        parts <- strsplit(key, "=", fixed = TRUE)[[1]]
        out[[parts[[1]]]] <- parts[[2]]
      } else if (i < length(args) && !grepl("^--", args[[i + 1L]])) {
        out[[key]] <- args[[i + 1L]]
        i <- i + 1L
      } else {
        out[[key]] <- TRUE
      }
    } else {
      out$positional <- c(out$positional, arg)
    }
    i <- i + 1L
  }
  out
}

dina_print_dashboard <- function(root = dina_repo_root()) {
  dina_cli_header("DINA-LatAm")
  session <- dina_load_session(root = root)
  state <- dina_session_state(session, root)
  if (is.null(session)) {
    dina_cli_alert("No active update session.")
  } else {
    dina_cli_alert(sprintf("Active update: %s", session$id))
    dina_cli_alert(sprintf("State: %s", state$state))
    dina_cli_alert(sprintf("Stale or blocked tasks: %s", state$stale_tasks %||% 0))
  }
  dina_cli_ok(sprintf("Recommended next action: %s", state$recommendation))
  cat("
Useful actions:
  1. dina doctor
  2. dina update start YEAR
  3. dina update resume
  4. dina sources scan
  5. dina tasks list
  6. dina run --dry-run
")
}

dina_cmd_doctor <- function(root) {
  result <- dina_doctor(root)
  dina_cli_header("Doctor")
  dina_cli_alert(sprintf("Repo root: %s", result$root))
  cat("\nR packages:\n")
  for (i in seq_len(nrow(result$packages))) {
    row <- result$packages[i, ]
    if (identical(unname(row$installed), TRUE)) dina_cli_ok(row$package) else dina_cli_err(paste("missing", row$package))
  }
  cat("\nStata:\n")
  if (result$stata$configured) {
    if (result$stata$available) dina_cli_ok(result$stata$command) else dina_cli_warn(sprintf("Configured but not found on PATH: %s", result$stata$command))
  } else {
    dina_cli_warn("Not configured. Set DINA_STATA_CMD or config/dina.yml stata.command.")
  }
  cat("\nPaths:\n")
  for (i in seq_len(nrow(result$paths))) {
    row <- result$paths[i, ]
    status <- sprintf("%s exists=%s writable=%s", row$path, row$exists, row$writable)
    if (identical(unname(row$exists), TRUE)) dina_cli_ok(status) else dina_cli_warn(status)
  }
  cat("\nPushover:\n")
  if (result$pushover$token_configured && result$pushover$user_configured) dina_cli_ok("Configured") else dina_cli_warn("Token/user not configured or notifications disabled")
  invisible(result)
}

dina_cmd_install <- function(root, args) {
  flags <- dina_parse_flags(args)
  dry_run <- isTRUE(flags[["dry-run"]])
  yes <- isTRUE(flags$yes)
  doctor <- dina_doctor(root)
  missing <- doctor$packages$package[!doctor$packages$installed]
  if (!length(missing)) {
    dina_cli_ok("All required R packages are installed.")
    return(invisible(missing))
  }
  dina_cli_warn(sprintf("Missing packages: %s", paste(missing, collapse = ", ")))
  if (dry_run) {
    return(invisible(missing))
  }
  if (!yes) {
    answer <- readline("Install missing packages now? [y/N] ")
    if (!tolower(answer) %in% c("y", "yes")) {
      dina_cli_warn("Installation skipped.")
      return(invisible(missing))
    }
  }
  install.packages(missing, dependencies = TRUE, repos = "https://cloud.r-project.org")
  invisible(missing)
}

dina_cmd_update <- function(root, args) {
  sub <- args[[1]] %||% "status"
  rest <- args[-1]
  if (identical(sub, "start")) {
    year <- rest[[1]] %||% format(Sys.Date(), "%Y")
    session <- dina_update_start(year = year, root = root)
    dina_cli_ok(sprintf("Started update session %s", session$id))
    dina_cli_alert(sprintf("Session directory: %s", dina_relative(dina_update_dir(session$id, root), root)))
  } else if (sub %in% c("resume", "status")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update. Run `dina update start YEAR`.", call. = FALSE)
    state <- dina_session_state(session, root)
    dina_cli_header(sprintf("Update %s", session$id))
    dina_cli_alert(sprintf("Status: %s", session$status))
    dina_cli_alert(sprintf("State: %s", state$state))
    dina_cli_ok(sprintf("Recommended next action: %s", state$recommendation))
  } else if (identical(sub, "checklist")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    dina_cli_header("Checklist")
    for (item in session$checklist) {
      dina_cli_cat(sprintf("[%s] %s - %s", item$status, item$id, item$label))
    }
  } else if (identical(sub, "finalize")) {
    flags <- dina_parse_flags(rest)
    session <- dina_load_session(root = root)
    result <- dina_finalize_update(session, root, force = isTRUE(flags$force))
    if (isTRUE(result$ok)) {
      dina_cli_ok(sprintf("Finalized update. Snapshot: %s", result$snapshot_dir))
    } else {
      dina_cli_err("Cannot finalize yet.")
      for (id in names(result$blockers)) {
        dina_cli_warn(sprintf("%s: %s", id, paste(result$blockers[[id]], collapse = "; ")))
      }
      quit(status = 1)
    }
  } else {
    stop("Unknown update command: ", sub, call. = FALSE)
  }
}

dina_cmd_sources <- function(root, args) {
  sub <- args[[1]] %||% "scan"
  session <- dina_load_session(root = root)
  if (identical(sub, "scan")) {
    flags <- dina_parse_flags(args[-1])
    scan <- dina_scan_sources(root, deep = isTRUE(flags$deep), hash = isTRUE(flags$hash) || isTRUE(flags$deep))
    dina_cli_header("Source Scan")
    for (x in scan) {
      dina_cli_cat(sprintf("%s [%s/%s]: years=%s files=%s", x$id, x$family, x$country, paste(x$detected_years, collapse = ","), length(x$files)))
    }
    if (!is.null(session)) {
      session$latest_source_scan <- scan
      session$updated_at <- dina_now()
      dina_save_session(session, root)
    }
  } else if (identical(sub, "diff")) {
    flags <- dina_parse_flags(args[-1])
    current <- dina_scan_sources(root, deep = isTRUE(flags$deep), hash = isTRUE(flags$hash) || isTRUE(flags$deep))
    previous <- if (!is.null(session)) session$source_scan else list()
    diff <- dina_classify_source_changes(current, previous)
    dina_cli_header("Source Diff")
    for (x in diff) {
      dina_cli_cat(sprintf("%s: %s current_years=%s previous_years=%s", x$id, paste(x$classes, collapse = ","), paste(x$current_years, collapse = ","), paste(x$previous_years, collapse = ",")))
    }
  } else if (identical(sub, "review")) {
    if (is.null(session)) stop("No active update.", call. = FALSE)
    staged <- list.files(file.path(dina_update_dir(session$id, root), "source_staging"), recursive = TRUE)
    dina_cli_header("Staged Sources")
    if (!length(staged)) dina_cli_warn("No staged downloads found.") else dina_cli_cat(paste(staged, collapse = "\n"))
  } else if (identical(sub, "refresh")) {
    if (is.null(session)) stop("No active update.", call. = FALSE)
    dina_cli_header("Source Refresh")
    flags <- dina_parse_flags(args[-1])
    source_ids <- if (!is.null(flags$source)) strsplit(flags$source, ",", fixed = TRUE)[[1]] else NULL
    results <- dina_sources_refresh(session, root, source_ids = source_ids, dry_run = isTRUE(flags[["dry-run"]]))
    for (result in results) {
      msg <- sprintf("%s: %s", result$id, result$status)
      if (!is.null(result$target)) msg <- paste(msg, "->", result$target)
      if (identical(result$status, "failed")) {
        dina_cli_err(paste(msg, result$error))
      } else if (identical(result$status, "staged")) {
        dina_cli_ok(msg)
      } else {
        dina_cli_alert(msg)
      }
    }
  } else if (identical(sub, "integrate")) {
    flags <- dina_parse_flags(args[-1])
    staged <- flags$staged %||% flags$file %||% NULL
    dest <- flags$to %||% NULL
    if (is.null(staged) || is.null(dest)) {
      dina_cli_warn("Usage: dina sources integrate --staged RELPATH --to input_data/... [--source ID] [--yes]")
      return(invisible(NULL))
    }
    decision <- dina_sources_integrate_file(
      session,
      staged_rel = staged,
      dest_rel = dest,
      source_id = flags$source %||% NULL,
      root = root,
      overwrite = isTRUE(flags$yes)
    )
    dina_cli_ok(sprintf("Integrated %s -> %s", decision$staged, decision$destination))
  } else {
    stop("Unknown sources command: ", sub, call. = FALSE)
  }
}

dina_cmd_tasks <- function(root, args) {
  sub <- args[[1]] %||% "list"
  if (identical(sub, "list")) {
    statuses <- dina_all_task_status(root)
    dina_cli_header("Tasks")
    for (x in statuses) {
      dina_cli_cat(sprintf("%-38s %-14s %s", x$id, x$stage, x$status))
    }
  } else if (identical(sub, "why")) {
    id <- args[[2]] %||% stop("Usage: dina tasks why TASK", call. = FALSE)
    tasks <- dina_task_map(root)
    if (is.null(tasks[[id]])) stop("Unknown task: ", id, call. = FALSE)
    status <- dina_task_status(tasks[[id]], root)
    dina_cli_header(sprintf("Why %s is %s", id, status$status))
    for (reason in status$reasons) dina_cli_alert(reason)
  } else {
    stop("Unknown tasks command: ", sub, call. = FALSE)
  }
}

dina_cmd_run <- function(root, args) {
  flags <- dina_parse_flags(args)
  tasks <- dina_select_tasks(
    root,
    task = flags$task %||% NULL,
    stage = flags$stage %||% NULL,
    from = flags$from %||% NULL,
    to = flags$to %||% NULL
  )
  dry_run <- isTRUE(flags[["dry-run"]]) || !isTRUE(flags$execute)
  for (task in tasks) {
    result <- dina_run_task(task, root, dry_run = dry_run, force = isTRUE(flags$force))
    dina_cli_cat(sprintf("%s: %s", result$task, result$status))
    if (!is.null(result$command)) dina_cli_cat(sprintf("  %s", paste(result$command, collapse = " ")))
  }
}

dina_cmd_config <- function(root, args) {
  sub <- args[[1]] %||% "show"
  if (identical(sub, "show")) {
    cat(paste(readLines(dina_config_path(root), warn = FALSE), collapse = "\n"), "\n")
  } else if (identical(sub, "set")) {
    key <- args[[2]] %||% stop("Usage: dina config set KEY VALUE", call. = FALSE)
    value <- args[[3]] %||% stop("Usage: dina config set KEY VALUE", call. = FALSE)
    config <- dina_read_yaml(dina_config_path(root))
    config <- dina_set_nested(config, key, value)
    dina_write_yaml(config, dina_config_path(root))
    dina_cli_ok(sprintf("Set %s", key))
  } else if (identical(sub, "render")) {
    path <- args[[2]] %||% dina_path("output", "run_logs", "config.do", root = root)
    dina_render_config_do(dina_config(root), if (grepl("^/", path)) path else file.path(root, path))
    dina_cli_ok(sprintf("Rendered %s", path))
  } else if (identical(sub, "edit")) {
    editor <- Sys.getenv("EDITOR", unset = "")
    if (!nzchar(editor)) stop("EDITOR is not set.", call. = FALSE)
    system2(editor, shQuote(dina_config_path(root)))
  } else {
    stop("Unknown config command: ", sub, call. = FALSE)
  }
}

dina_cmd_data <- function(root, args) {
  sub <- args[[1]] %||% "check"
  if (identical(sub, "check")) {
    checks <- dina_data_check(root)
    dina_cli_header("Data Check")
    for (i in seq_len(nrow(checks))) {
      if (checks$exists[i]) dina_cli_ok(checks$path[i]) else dina_cli_warn(paste("missing", checks$path[i]))
    }
  } else if (identical(sub, "pack")) {
    archive <- args[[2]] %||% NULL
    path <- dina_pack_data(root, archive)
    dina_cli_ok(sprintf("Packed %s", dina_relative(path, root)))
  } else if (identical(sub, "unpack")) {
    archive <- args[[2]] %||% stop("Usage: dina data unpack ARCHIVE", call. = FALSE)
    utils::untar(archive, exdir = root)
    dina_cli_ok(sprintf("Unpacked %s", archive))
  } else {
    stop("Unknown data command: ", sub, call. = FALSE)
  }
}

dina_cmd_audit <- function(root, args) {
  sub <- args[[1]] %||% "paths"
  if (!identical(sub, "paths")) stop("Unknown audit command: ", sub, call. = FALSE)
  hits <- dina_audit_paths(root)
  dina_cli_header("Path Audit")
  if (!length(hits)) {
    dina_cli_ok("No hardcoded path patterns found.")
  } else {
    for (file in names(hits)) {
      dina_cli_cat(file)
      for (i in seq_len(nrow(hits[[file]]))) {
        dina_cli_cat(sprintf("  %s: %s", hits[[file]]$line[i], trimws(hits[[file]]$text[i])))
      }
    }
  }
}

dina_cmd_make <- function(root, args) {
  sub <- args[[1]] %||% "export"
  if (!identical(sub, "export")) stop("Unknown make command: ", sub, call. = FALSE)
  path <- args[[2]] %||% "Makefile.dina"
  full <- if (grepl("^/", path)) path else file.path(root, path)
  dina_make_export(full, root)
  dina_cli_ok(sprintf("Wrote %s", dina_relative(full, root)))
}

dina_cmd_notify <- function(root, args) {
  sub <- args[[1]] %||% "test"
  if (!identical(sub, "test")) stop("Unknown notify command: ", sub, call. = FALSE)
  dina_notify_test(root)
  dina_cli_ok("Notification sent.")
}

dina_cmd_setup <- function(root, args) {
  sub <- args[[1]] %||% "command"
  if (!identical(sub, "command")) stop("Unknown setup command: ", sub, call. = FALSE)
  target_dir <- Sys.getenv("HOME")
  target <- file.path(target_dir, ".local", "bin", "dina")
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(root, "bin", "dina"), target, overwrite = TRUE)
  Sys.chmod(target, "0755")
  dina_cli_ok(sprintf("Installed command wrapper at %s", target))
}

dina_main <- function(args = commandArgs(trailingOnly = TRUE), root = dina_repo_root()) {
  if (!length(args)) {
    return(dina_print_dashboard(root))
  }
  cmd <- args[[1]]
  rest <- args[-1]
  if (cmd %in% c("-h", "--help", "help")) return(dina_usage())
  switch(
    cmd,
    doctor = dina_cmd_doctor(root),
    install = dina_cmd_install(root, rest),
    update = dina_cmd_update(root, rest),
    sources = dina_cmd_sources(root, rest),
    tasks = dina_cmd_tasks(root, rest),
    run = dina_cmd_run(root, rest),
    config = dina_cmd_config(root, rest),
    data = dina_cmd_data(root, rest),
    audit = dina_cmd_audit(root, rest),
    make = dina_cmd_make(root, rest),
    notify = dina_cmd_notify(root, rest),
    setup = dina_cmd_setup(root, rest),
    {
      dina_cli_err(sprintf("Unknown command: %s", cmd))
      dina_usage()
      quit(status = 1)
    }
  )
}

tryCatch(
  dina_main(root = dina_cli_root),
  error = function(e) {
    dina_cli_err(conditionMessage(e))
    quit(status = 1)
  }
)
