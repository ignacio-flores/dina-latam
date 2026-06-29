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

dina_arg <- function(args, i, default = NULL) {
  if (length(args) >= i) args[[i]] else default
}

dina_drop_leading_separator <- function(args) {
  if (length(args) && identical(args[[1]], "--")) args[-1] else args
}

dina_has_help_flag <- function(args) {
  separator <- match("--", args)
  if (!is.na(separator)) {
    args <- if (separator > 1L) args[seq_len(separator - 1L)] else character()
  }
  any(args %in% c("-h", "--help"))
}

dina_help_text <- function(topic = NULL) {
  topic <- topic %||% "main"
  switch(
    topic,
    main = "DINA-LatAm CLI

Usage:
  dina
  dina help [TOPIC]
  dina COMMAND [SUBCOMMAND] [OPTIONS]

Plain `dina` opens the guided dashboard and recommends the next action.
Use `dina help workflow` for the annual update recipe.
Use `dina help COMMAND` for command details.
After `dina setup command`, `dina` and `./bin/dina` are equivalent.

Command map

Annual update:
  `help workflow`                     [read-only] annual update recipe
  `update start [YEAR]`               [writes session] create active session
  `update resume|status|checklist`    [read-only] inspect active session
  `update finalize [--force]`         [writes session] freeze final records

Source data:
  `sources scan|diff|review`          [read-only] inspect source coverage
  `sources refresh [--dry-run]`       [writes session] stage downloads
  `sources integrate`                 [writes files] copy approved inputs

Pipeline:
  `tasks list|why TASK`               [read-only] inspect task freshness
  `run ... --dry-run`                 [read-only] preview selected scripts
  `run ... --execute`                 [writes files] run selected scripts

Setup and config:
  `doctor`                            [read-only] check local readiness
  `install`                           install missing R packages
  `config show|render`                [read-only/writes files] inspect or render
  `config set|edit`                   [writes config] modify `config/dina.yml`
  `data check|pack|unpack`            [read-only/writes archive/writes files]
  `notify init|test`                  configure or test Pushover
  `setup command`                     install the user-level `dina` wrapper

Maintenance:
  `audit paths`                       [read-only] report likely hardcoded paths
  `make export [PATH]`                [writes files] export task graph

Pipeline selectors
  `01a`                  one task, such as `01a-clean-macro-data`
  `01`                   whole numbered block for `dina run`
  `01a,02a`              multiple tasks for `dina run`
  `--from 03 --to 05`    range for `dina run`
  `tasks why` needs a unique selector, such as `01a` or a full task id.

Critical defaults
  `dina run` is dry-run unless `--execute` is present.
  `sources refresh` stages downloads; `sources integrate` copies into
  `input_data/`.
  `config render` writes a generated Stata config; it does not edit `_config.do`.
  `config set` and `config edit` modify `config/dina.yml`.

Notes:
  `--` is accepted as an optional separator for shell compatibility, but it is
  never required. For example, `dina help` and `dina -- help` both work.

Examples:
  dina help workflow
  dina update start YEAR
  dina sources scan --deep
  dina run 01a --dry-run
  dina run 01 --execute --notify
",
    workflow = "Usage:
  dina help workflow

What this page is:
  A recommended order for an annual DINA-LatAm update. It is a guide, not a new
  command. Use the command-specific help pages when you need exact options.

1. Check the machine and project state
  dina doctor
      Read-only preflight for R packages, Stata, paths, Pushover, and any
      active update pointer.

  dina data check
      Read-only check that configured primary data paths exist.

2. Start or resume the annual update
  dina update start [YEAR]
      Creates `output/updates/<update_id>` and makes it the active session.
      If YEAR is omitted, the current calendar year is used.

  dina update resume
      Recomputes the current state and recommends the next action.

  dina update status
      Same state summary as resume, without implying that work should continue.

3. Refresh and review source data
  dina sources scan [--deep] [--hash]
      Inspect configured source coverage. With an active update, records the
      latest scan in the session.

  dina sources refresh [--source ID] [--dry-run]
      Stage configured online downloads inside the active update session.
      It does not directly overwrite `input_data/`.

  dina sources diff [--deep] [--hash]
      Compare current source coverage with the active session baseline.

  dina sources review
      List staged downloads waiting for review.

  dina sources integrate --staged RELPATH --to input_data/... [--yes]
      Copy an approved staged source into its canonical `input_data/` location
      and record the decision.

4. Inspect the pipeline before running it
  dina tasks list
      Show task ids, aliases, stages, and freshness.

  dina tasks why TASK
      Explain why a task is current, stale, missing, or failed.

  dina run TASK --dry-run
      Preview commands without running Stata. Dry-run is the default.

Task selectors:
  01a                  One task.
  01                   Whole numbered block for `dina run`.
  01a,02a              Multiple tasks for `dina run`.
  full-task-id         Exact task id.
  --from 03 --to 05    Range from block 03 through block 05.
  `tasks why` needs one unique task selector, such as 01a or a full task id.

5. Execute the pipeline
  dina run TASK --execute
      Actually run selected scripts and write run logs under `output/run_logs/`.

  dina run --from TASK --to TASK --execute --notify
      Run a range and send Pushover status when notifications are configured.

6. Finalize the update
  dina update checklist
      Print the active update checklist.

  dina update finalize
      Freeze final outputs and checksums. It refuses missing, stale, or failed
      required tasks unless `--force` is supplied.

More help:
  dina help update     dina help sources     dina help run
  dina help tasks      dina help config      dina help data
",
    doctor = "Usage:
  dina doctor

What it does:
  Runs a read-only preflight check for the local machine. It reports missing R
  packages, whether Stata is configured and discoverable, write access for key
  project paths, Pushover configuration source, and the active update pointer.

What it changes:
  Nothing. This is safe to run before, during, or after an update.

Useful when:
  A server run fails early, a new computer is being configured, or you want to
  confirm whether notifications and Stata are wired correctly.

Examples:
  dina doctor
",
    install = "Usage:
  dina install [--yes] [--dry-run]

What it does:
  Looks at the R package list in `config/dina.yml` and installs packages that
  are missing from the current R library.

Options:
  --dry-run                       Only report what would be installed.
  --yes                           Install without asking for confirmation.

What it changes:
  Your R library, not project data. It does not install Stata packages yet.

Examples:
  dina install --dry-run
  dina install --yes
",
    update = "Usage:
  dina update start [YEAR]
  dina update resume
  dina update status
  dina update checklist
  dina update finalize [--force]

What it manages:
  Annual update sessions under `output/updates/<update_id>`. A session stores
  effective config, source scans, task run records, checklist state, and final
  manifests.

Subcommands:
  start [YEAR]                    Creates a new session and active pointer.
                                  If omitted, YEAR defaults to the current
                                  calendar year. Default id:
                                  YEAR-update-MM-DD.
  resume                          Recomputes reality and recommends the next
                                  action. It does not blindly continue a run.
  status                          Same state summary as resume, without implying
                                  that work should continue automatically.
  checklist                       Prints the update checklist stored in the
                                  active session.
  finalize [--force]              Freezes final outputs and checksums. Without
                                  --force it refuses missing, stale, or failed
                                  required tasks.

What it changes:
  `start`, source commands during a session, and `finalize` write session
  records. `resume`, `status`, and `checklist` are primarily inspection.

Examples:
  dina update start YEAR
  dina update resume
  dina update finalize
",
    sources = "Usage:
  dina sources refresh [--source ID] [--dry-run]
  dina sources scan [--deep] [--hash]
  dina sources review
  dina sources diff [--deep] [--hash]
  dina sources integrate --staged RELPATH --to input_data/... [--source ID] [--yes]

What it manages:
  Source files before they become canonical inputs. Downloads are staged inside
  the active update first; integration into `input_data/` is a separate step.

Subcommands:
  refresh                         Downloads configured online sources into the
                                  active session staging area. It never directly
                                  overwrites `input_data/`.
  scan                            Reads the source registry and detects local
                                  coverage from filenames and, with --deep,
                                  workbook metadata.
  review                          Lists staged files waiting for human review.
  diff                            Compares current scan results with the active
                                  session baseline and classifies changes.
  integrate                       Copies an approved staged file into its final
                                  destination and records the decision.

Options:
  --source ID                     Limit refresh to one source registry id.
  --deep                          Inspect workbook sheets when possible.
  --hash                          Compute file hashes during scan/diff.
  --dry-run                       For refresh, show planned downloads only.
  --yes                           For integrate, allow overwriting destination.

Gotcha:
  Source coverage is independent of update year. A 2026 update may discover
  newly available 2024 data or historical backfills.

Examples:
  dina sources refresh --dry-run
  dina sources refresh --source chl-pit-total
  dina sources scan --deep
  dina sources integrate --staged CHL/file.xlsx --to input_data/admin_data/CHL/file.xlsx --yes
",
    tasks = "Usage:
  dina tasks list
  dina tasks why TASK

What it does:
  Inspects the configured task graph without running scripts. `list` shows each
  task alias, full id, stage, and freshness status. `why` explains the reason a
  task is stale, missing outputs, missing inputs, current, or failed.

Task selectors:
  07d                             Unique short selector.
  07d-export-results-to-wid       Full task id.

Note:
  `tasks why` needs one unique task. Block selectors such as `07` are mainly for
  `dina run` and may be ambiguous here.

What it changes:
  Nothing. These are inspection commands.

Examples:
  dina tasks list
  dina tasks why 07d
  dina tasks why 01a-clean-macro-data
",
    run = "Usage:
  dina run [TASK ...] [OPTIONS]
  dina run --task TASK [OPTIONS]

What it does:
  Selects tasks from `config/pipeline.yml`, checks freshness, then either prints
  the commands that would run or executes them. Dry-run is the default.

Task selectors:
  01a                             One task, e.g. 01a-clean-macro-data.
  01                              Whole numbered block, e.g. all 01* tasks.
  01a,01b                         Multiple selected tasks.
  --task 01a,01b                  Same selection through the option form.
  --from 03 --to 05               Range from the first 03* task through the
                                  last 05* task.
  full-task-id                    Exact task id.

Options:
  --task TASK                     Selector or comma-separated selectors.
  --stage STAGE                   Restrict selected tasks to one stage.
  --from TASK                     Start at a task; NN starts at first task in
                                  block NN.
  --to TASK                       End at a task; NN ends at last task in block NN.
  --dry-run                       Print commands without executing. This is the
                                  default unless --execute is present.
  --execute                       Actually run scripts and write run logs.
  --force                         Run even when a task appears current.
  --notify                        Send a Pushover message at completion/failure.

What it changes:
  With --dry-run, nothing. With --execute, scripts may update data/output files
  and the CLI writes run logs under `output/run_logs/`.

Examples:
  dina run 01a --dry-run
  dina run 01 --dry-run
  dina run --from 03 --to 05 --execute
  dina run 07d --execute --notify
",
    config = "Usage:
  dina config show
  dina config set KEY VALUE
  dina config edit
  dina config render [PATH]

What it manages:
  `config/dina.yml`, the CLI's default project configuration. CLI runs can render
  a session-specific Stata config without changing `_config.do`, so manual Stata
  usage stays backward compatible.

Subcommands:
  show                            Prints the committed default YAML exactly as
                                  stored in `config/dina.yml`.
  set KEY VALUE                   Edits `config/dina.yml`. Nested keys use dots,
                                  e.g. `years.last`. Values are parsed as
                                  booleans, integers, or comma-separated vectors
                                  when they look like those types.
  edit                            Opens `config/dina.yml` in `$EDITOR`.
  render [PATH]                   Writes a Stata `config.do` from the effective
                                  CLI config. Default path:
                                  `output/run_logs/config.do`.

What it changes:
  `show` changes nothing. `set` and `edit` modify committed defaults.
  `render` writes only the rendered Stata config path; it does not edit
  `_config.do`.

Examples:
  dina config show
  dina config set years.last YEAR
  dina config set run.units ind,esn,pch
  dina config render output/run_logs/config.do
",
    data = "Usage:
  dina data check
  dina data pack [ARCHIVE]
  dina data unpack ARCHIVE

What it manages:
  Portable primary-data bundles for machines or servers where large data are not
  permanently stored.

Subcommands:
  check                           Reports whether configured primary paths exist.
  pack [ARCHIVE]                  Creates a `.tar.gz` archive from configured
                                  primary paths. If no archive is given, writes
                                  under `output/archives/`.
  unpack ARCHIVE                  Extracts an archive into the repo root.

What it changes:
  `check` changes nothing. `pack` writes an archive. `unpack` may create or
  overwrite data files depending on archive contents.

Examples:
  dina data check
  dina data pack output/archives/primary-data-YEAR.tar.gz
  dina data unpack output/archives/primary-data-YEAR.tar.gz
",
    audit = "Usage:
  dina audit paths

What it does:
  Searches code files for path patterns that often signal hardcoding, such as
  Dropbox paths, `setwd()`, or direct data folder literals.

What it changes:
  Nothing. It only reports matching files and lines.

Examples:
  dina audit paths
",
    make = "Usage:
  dina make export [PATH]

What it does:
  Exports the YAML task graph to a Makefile for transparency or server
  automation. The YAML config remains authoritative.

What it changes:
  Writes the requested Makefile path, defaulting to `Makefile.dina`.

Examples:
  dina make export
  dina make export /tmp/Makefile.dina
",
    notify = "Usage:
  dina notify init [--force]
  dina notify test

What it manages:
  Local Pushover notification setup for interactive and server runs.

Subcommands:
  init                            Creates ignored `config/pushover.local.R`
                                  with placeholder credentials. Use --force to
                                  overwrite the placeholder file.
  test                            Sends a test message using the local file or
                                  environment fallback.

Local config:
  config/pushover.local.R may contain:
    set_pushover_user(user = \"xxxxxx\")
    set_pushover_app(token = \"xxxxxx\")

Server fallback:
  PUSHOVER_USER_KEY and PUSHOVER_APP_TOKEN are also supported.

Examples:
  dina notify init
  dina notify test
",
    setup = "Usage:
  dina setup command

What it does:
  Copies the repo wrapper from `bin/dina` to `~/.local/bin/dina`.

What it changes:
  Creates or overwrites that user-level wrapper. It does not install R packages
  or edit project config.
",
    sprintf("Unknown help topic `%s`. Run `dina help` for available commands.\n", topic)
  )
}

dina_usage <- function(topic = NULL) {
  cat(dina_help_text(topic), sep = "")
}

dina_parse_flags <- function(args) {
  out <- list(positional = character())
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (identical(arg, "--")) {
      if (i < length(args)) {
        out$positional <- c(out$positional, args[(i + 1L):length(args)])
      }
      break
    } else if (grepl("^--", arg)) {
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

dina_dashboard_actions <- function() {
  year <- format(Sys.Date(), "%Y")
  list(
    list(label = "dina doctor", args = c("doctor")),
    list(label = sprintf("dina update start %s", year), args = c("update", "start", year)),
    list(label = "dina update resume", args = c("update", "resume")),
    list(label = "dina sources scan", args = c("sources", "scan")),
    list(label = "dina tasks list", args = c("tasks", "list")),
    list(label = "dina run --dry-run", args = c("run", "--dry-run"))
  )
}

dina_dashboard_prompt <- function(actions, root) {
  if (!isatty(stdin())) return(invisible(NULL))
  cat("\nChoose an action number to run, or press Enter to exit: ")
  answer <- trimws(readLines("stdin", n = 1L, warn = FALSE))
  if (!nzchar(answer)) return(invisible(NULL))
  choice <- suppressWarnings(as.integer(answer))
  if (is.na(choice) || choice < 1L || choice > length(actions)) {
    dina_cli_warn(sprintf("Unknown action `%s`.", answer))
    return(invisible(NULL))
  }
  selected <- actions[[choice]]
  dina_cli_alert(sprintf("Running `%s`", selected$label))
  dina_main(selected$args, root = root)
}

dina_print_dashboard <- function(root = dina_repo_root()) {
  dina_cli_header("DINA-LatAm")
  session <- dina_load_session(root = root)
  state <- dina_session_state(session, root)
  actions <- dina_dashboard_actions()
  recommendation <- gsub(
    "dina update start YEAR",
    actions[[2]]$label,
    state$recommendation,
    fixed = TRUE
  )
  if (is.null(session)) {
    dina_cli_alert("No active update session.")
  } else {
    dina_cli_alert(sprintf("Active update: %s", session$id))
    dina_cli_alert(sprintf("State: %s", state$state))
    dina_cli_alert(sprintf("Stale or blocked tasks: %s", state$stale_tasks %||% 0))
  }
  dina_cli_ok(sprintf("Recommended next action: %s", recommendation))
  cat("\nUseful actions:\n")
  for (i in seq_along(actions)) {
    cat(sprintf("  %s. %s\n", i, actions[[i]]$label))
  }
  dina_dashboard_prompt(actions, root)
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
  if (result$pushover$configured) {
    source_label <- switch(
      result$pushover$source,
      local_file = sprintf("local file %s", result$pushover$local_path),
      environment = "environment variables",
      config = "config/dina.yml",
      result$pushover$source
    )
    dina_cli_ok(sprintf("Configured via %s", source_label))
  } else {
    dina_cli_warn("Not configured. Run `dina notify init` or set PUSHOVER_APP_TOKEN and PUSHOVER_USER_KEY.")
  }
  if (!isTRUE(result$pushover$enabled)) {
    dina_cli_alert("notifications.pushover.enabled is false; explicit `dina notify test` and `--notify` still send when configured.")
  }
  invisible(result)
}

dina_cmd_install <- function(root, args) {
  args <- dina_drop_leading_separator(args)
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
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "status")
  rest <- args[-1]
  if (identical(sub, "start")) {
    year <- dina_arg(rest, 1L, format(Sys.Date(), "%Y"))
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
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "scan")
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
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "list")
  if (identical(sub, "list")) {
    statuses <- dina_all_task_status(root)
    dina_cli_header("Tasks")
    dina_cli_cat(sprintf("%-6s %-38s %-14s %s", "alias", "id", "stage", "status"))
    for (x in statuses) {
      dina_cli_cat(sprintf("%-6s %-38s %-14s %s", dina_task_short_id(x$id), x$id, x$stage, x$status))
    }
  } else if (identical(sub, "why")) {
    tasks <- dina_task_map(root)
    ids <- names(tasks)
    selector <- if (length(args) >= 2L) args[[2]] else stop("Usage: dina tasks why TASK", call. = FALSE)
    id <- dina_resolve_task_selector(selector, ids, mode = "single")
    if (is.null(tasks[[id]])) stop("Unknown task: ", id, call. = FALSE)
    status <- dina_task_status(tasks[[id]], root)
    dina_cli_header(sprintf("Why %s is %s", id, status$status))
    for (reason in status$reasons) dina_cli_alert(reason)
  } else {
    stop("Unknown tasks command: ", sub, call. = FALSE)
  }
}

dina_cmd_run <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  flags <- dina_parse_flags(args)
  task_selectors <- c(flags$task %||% character(), flags$positional %||% character())
  if (!length(dina_split_task_selectors(task_selectors))) {
    task_selectors <- NULL
  }
  tasks <- dina_select_tasks(
    root,
    task = task_selectors,
    stage = flags$stage %||% NULL,
    from = flags$from %||% NULL,
    to = flags$to %||% NULL
  )
  dry_run <- isTRUE(flags[["dry-run"]]) || !isTRUE(flags$execute)
  notify <- isTRUE(flags$notify)
  completed <- FALSE
  results <- list()
  if (notify) {
    on.exit({
      message <- if (completed) {
        summary <- paste(vapply(results, function(x) sprintf("%s=%s", x$task, x$status), character(1)), collapse = ", ")
        sprintf("DINA run finished: %s", summary)
      } else {
        "DINA run failed before completing. Check console output and run logs."
      }
      tryCatch(
        dina_notify(message, root = root),
        error = function(e) dina_cli_warn(sprintf("Could not send Pushover notification: %s", conditionMessage(e)))
      )
    }, add = TRUE)
  }
  for (task in tasks) {
    result <- dina_run_task(task, root, dry_run = dry_run, force = isTRUE(flags$force))
    results[[task$id]] <- result
    dina_cli_cat(sprintf("%s: %s", result$task, result$status))
    if (!is.null(result$command)) dina_cli_cat(sprintf("  %s", paste(result$command, collapse = " ")))
  }
  completed <- TRUE
}

dina_cmd_config <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "show")
  if (identical(sub, "show")) {
    cat(paste(readLines(dina_config_path(root), warn = FALSE), collapse = "\n"), "\n")
  } else if (identical(sub, "set")) {
    key <- dina_arg(args, 2L, NULL)
    value <- dina_arg(args, 3L, NULL)
    if (is.null(key) || is.null(value)) stop("Usage: dina config set KEY VALUE", call. = FALSE)
    config <- dina_read_yaml(dina_config_path(root))
    config <- dina_set_nested(config, key, value)
    dina_write_yaml(config, dina_config_path(root))
    dina_cli_ok(sprintf("Set %s", key))
  } else if (identical(sub, "render")) {
    path <- dina_arg(args, 2L, dina_path("output", "run_logs", "config.do", root = root))
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
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "check")
  if (identical(sub, "check")) {
    checks <- dina_data_check(root)
    dina_cli_header("Data Check")
    for (i in seq_len(nrow(checks))) {
      if (checks$exists[i]) dina_cli_ok(checks$path[i]) else dina_cli_warn(paste("missing", checks$path[i]))
    }
  } else if (identical(sub, "pack")) {
    archive <- dina_arg(args, 2L, NULL)
    path <- dina_pack_data(root, archive)
    dina_cli_ok(sprintf("Packed %s", dina_relative(path, root)))
  } else if (identical(sub, "unpack")) {
    archive <- dina_arg(args, 2L, NULL)
    if (is.null(archive)) stop("Usage: dina data unpack ARCHIVE", call. = FALSE)
    utils::untar(archive, exdir = root)
    dina_cli_ok(sprintf("Unpacked %s", archive))
  } else {
    stop("Unknown data command: ", sub, call. = FALSE)
  }
}

dina_cmd_audit <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "paths")
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
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "export")
  if (!identical(sub, "export")) stop("Unknown make command: ", sub, call. = FALSE)
  path <- dina_arg(args, 2L, "Makefile.dina")
  full <- if (grepl("^/", path)) path else file.path(root, path)
  dina_make_export(full, root)
  dina_cli_ok(sprintf("Wrote %s", dina_relative(full, root)))
}

dina_cmd_notify <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "test")
  if (identical(sub, "test")) {
    dina_notify_test(root)
    dina_cli_ok("Notification sent.")
  } else if (identical(sub, "init")) {
    flags <- dina_parse_flags(args[-1])
    result <- dina_notify_init(root, overwrite = isTRUE(flags$force))
    if (result$created) {
      dina_cli_ok(sprintf("Created %s", dina_relative(result$path, root)))
    } else {
      dina_cli_warn(sprintf("%s already exists. Pass --force to overwrite the placeholder template.", dina_relative(result$path, root)))
    }
  } else {
    stop("Unknown notify command: ", sub, call. = FALSE)
  }
}

dina_cmd_setup <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "command")
  if (!identical(sub, "command")) stop("Unknown setup command: ", sub, call. = FALSE)
  target_dir <- Sys.getenv("HOME")
  target <- file.path(target_dir, ".local", "bin", "dina")
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(root, "bin", "dina"), target, overwrite = TRUE)
  Sys.chmod(target, "0755")
  dina_cli_ok(sprintf("Installed command wrapper at %s", target))
}

dina_main <- function(args = commandArgs(trailingOnly = TRUE), root = dina_repo_root()) {
  args <- dina_drop_leading_separator(args)
  if (!length(args)) {
    return(dina_print_dashboard(root))
  }
  cmd <- args[[1]]
  rest <- args[-1]
  if (cmd %in% c("-h", "--help")) return(dina_usage())
  if (identical(cmd, "help")) return(dina_usage(dina_arg(dina_drop_leading_separator(rest), 1L, NULL)))
  if (dina_has_help_flag(rest)) return(dina_usage(cmd))
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
