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
  `update list`                       [read-only] list update sessions
  `update restart|delete`             [writes session] lifecycle controls
  `update finalize [--force]`         [writes session] freeze final records

Source data:
  `sources status|scan|diff|review`   [read-only] inspect source coverage
  `sources list|show ID`              [read-only] inspect source registry
  `sources refresh [--dry-run]`       [writes session] stage downloads
  `sources complete --status STATUS`  [writes session] record source decision
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
  `update start` records a source baseline with hashes by default.
  `sources status` uses hashes only when timestamps or sizes changed.
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
  dina sources status
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
      If YEAR is omitted, the current calendar year is used. It records a
      source baseline with hashes by default.

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

  dina sources status [--metadata-only] [--hash-all] [--deep]
      Compare local canonical source files with the update baseline. By default
      it reuses baseline hashes when size and timestamp are unchanged, and hashes
      only files whose cheap metadata changed.

  dina sources complete --status STATUS [--note TEXT]
      Record that source review is complete before moving to the pipeline.
      STATUS is one of: no-new-data, updated, manual, deferred.

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
  Stata is configured only when DINA has a runnable command for batch jobs;
  having the Stata app installed is not enough unless DINA can find it.

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
  dina update start [YEAR] [--yes] [--no-source-hash]
  dina update resume
  dina update status
  dina update checklist
  dina update list
  dina update restart [ID] [--yes]
  dina update delete [ID] [--yes]
  dina update finalize [--force]

What it manages:
  Annual update sessions under `output/updates/<update_id>`. A session stores
  effective config, source scans, task run records, checklist state, and final
  manifests.

Subcommands:
  start [YEAR]                    Creates a new session, active pointer, and
                                  hashed source baseline.
                                  If omitted, YEAR defaults to the current
                                  calendar year. Default id:
                                  YEAR-update-MM-DD. If an unfinished same-day
                                  session exists, creating YEAR-update-MM-DD-02
                                  requires confirmation.
  resume                          Recomputes reality and recommends the next
                                  action. It does not blindly continue a run.
  status                          Same state summary as resume, without implying
                                  that work should continue automatically.
  checklist                       Prints the update checklist stored in the
                                  active session.
  list                            Lists update sessions and marks the active
                                  one with `*`.
  restart [ID] [--yes]            Resets one update session from scratch using
                                  the same id. If ID is omitted, uses the active
                                  session. Without --yes, interactive terminals
                                  ask before resetting; scripts only preview.
  delete [ID] [--yes]             Deletes an update session. If ID is omitted,
                                  uses the active session. Without --yes,
                                  interactive terminals ask before deleting;
                                  scripts only preview.
  finalize [--force]              Freezes final outputs and checksums. Without
                                  --force it refuses missing, stale, or failed
                                  required tasks.

What it changes:
  `start`, source commands during a session, `restart`, `delete`, and `finalize`
  write session records or remove session files. `resume`, `status`,
  `checklist`, and `list` are primarily inspection.

Options:
  --no-source-hash                For start, record only file size/timestamp in
                                  the source baseline. The default computes
                                  source hashes for later comparison.

Examples:
  dina update start YEAR
  dina update start YEAR --yes
  dina update list
  dina update restart --yes
  dina update delete 2026-update-06-29 --yes
  dina update resume
  dina update finalize
",
    sources = "Usage:
  dina sources refresh [--source ID] [--dry-run]
  dina sources list [--family FAMILY] [--country ISO] [--method METHOD] [--urls]
  dina sources list country ISO [--urls]
  dina sources list family FAMILY [--urls]
  dina sources list method METHOD [--urls]
  dina sources show ID [--urls]
  dina sources methods
  dina sources status [--metadata-only] [--hash-all] [--deep]
  dina sources complete --status STATUS [--note TEXT]
  dina sources scan [--deep] [--hash]
  dina sources review
  dina sources diff [--deep] [--hash]
  dina sources integrate --staged RELPATH --to input_data/... [--source ID] [--yes]

What it manages:
  Source files before they become canonical inputs. Downloads are staged inside
  the active update first; integration into `input_data/` is a separate step.

  Subcommands:
  list                            Shows compact registry rows: id, family,
                                  country, method, path count, URL presence,
                                  downloader presence, and transformer presence.
  show ID                         Shows the full registry entry for one source,
                                  including URLs, canonical paths, scripts,
                                  checks, and notes.
  methods                         Explains source acquisition method labels.
  status                          Compares current canonical source files with
                                  the active update baseline. Default mode uses
                                  hashes only when size/timestamp changed.
  complete                        Records that source review is complete before
                                  pipeline tasks are recommended.
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
  --family FAMILY                 For list, keep one source family.
  --method METHOD                 For list, keep one acquisition method.
  --country ISO                   For list, keep one ISO country plus broad
                                  country sources.
  country ISO                     Friendly form of --country ISO for list.
  family FAMILY                   Friendly form of --family FAMILY for list.
  method METHOD                   Friendly form of --method METHOD for list.
  --urls                          Print source URLs in list/show output.
  --deep                          Inspect workbook sheets when possible.
  --hash                          Compute file hashes during scan/diff.
  --metadata-only                 For status, compare only paths, size, and
                                  timestamps; do not compute hashes.
  --hash-all                      For status, hash all source files.
  --status STATUS                 For complete: no-new-data, updated, manual,
                                  or deferred.
  --note TEXT                     Required when --status deferred.
  --dry-run                       For refresh, show planned downloads only.
  --yes                           For integrate, allow overwriting destination.

Gotcha:
  Source coverage is independent of update year. A 2026 update may discover
  newly available 2024 data or historical backfills.

Methods:
  url                             Direct URL fetchable by `sources refresh`.
  zip                             Direct archive URL fetchable by `sources refresh`.
  script                          Custom acquisition script exists.
  manual                          Human-curated input or URL index.
  wid                             Data currently acquired through Stata/WID calls.

Examples:
  dina sources list --urls
  dina sources list --country CHL --urls
  dina sources list country CHL
  dina sources list family admin-data
  dina sources list method manual
  dina sources methods
  dina sources show country-sna-index --urls
  dina sources status
  dina sources complete --status no-new-data
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
  task alias, full id, stage, language, and freshness status. `why` explains the
  reason a task is stale, missing outputs, missing inputs, current, or failed.
  Inactive tasks are registered for visibility but skipped by broad runs unless
  selected explicitly.

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

Stata:
  Stata tasks need a runnable command. If `dina doctor` finds Stata but says it
  is not configured, set DINA_STATA_CMD to the suggested executable path.

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

dina_cli_progress <- function(text) {
  dina_cli_cat(sprintf("  %s", text))
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

dina_source_counts_line <- function(counts) {
  counts <- counts[counts > 0L]
  if (!length(counts)) {
    return("none")
  }
  paste(sprintf("%s=%s", names(counts), as.integer(counts)), collapse = ", ")
}

dina_print_update_source_summary <- function(session) {
  review <- session$source_review %||% NULL
  baseline_at <- session$source_baseline$created_at %||% session$created_at %||% NA_character_
  hash_mode <- session$source_baseline$hash_mode %||% "none"
  dina_cli_alert(sprintf("Source baseline: %s (hash: %s)", baseline_at, hash_mode))
  refresh <- dina_latest_source_refresh_time(session)
  integration <- dina_latest_source_decision_time(session)
  if (!is.na(refresh) && nzchar(refresh)) {
    dina_cli_alert(sprintf("Last source refresh: %s", refresh))
  }
  if (!is.na(integration) && nzchar(integration)) {
    dina_cli_alert(sprintf("Last source integration: %s", integration))
  }
  if (!is.null(review)) {
    detail <- if (nzchar(review$note %||% "")) sprintf(" (%s)", review$note) else ""
    dina_cli_alert(sprintf("Source review: %s at %s%s", review$status %||% "", review$reviewed_at %||% "", detail))
  } else {
    dina_cli_warn("Source review: not recorded. Run `dina sources status` before running the pipeline.")
  }
}

dina_print_source_status <- function(status) {
  dina_cli_header("Source Status")
  dina_cli_alert(sprintf("Baseline: %s (hash: %s)", status$baseline_at, status$baseline_hash_mode))
  dina_cli_alert(sprintf("Current scan: %s (hash: %s)", status$scanned_at, status$scan_hash_mode))
  if (!is.na(status$last_recorded_scan_at) && nzchar(status$last_recorded_scan_at)) {
    dina_cli_alert(sprintf("Last recorded scan: %s", status$last_recorded_scan_at))
  }
  if (!is.na(status$last_refresh_at) && nzchar(status$last_refresh_at)) {
    dina_cli_alert(sprintf("Last refresh: %s", status$last_refresh_at))
  }
  if (!is.na(status$last_integration_at) && nzchar(status$last_integration_at)) {
    dina_cli_alert(sprintf("Last integration: %s", status$last_integration_at))
  }
  if (!is.null(status$review)) {
    dina_cli_alert(sprintf("Source review: %s at %s", status$review$status %||% "", status$review$reviewed_at %||% ""))
  } else {
    dina_cli_warn("Source review: not recorded.")
  }
  dina_cli_cat(sprintf("File status counts: %s", dina_source_counts_line(status$counts)))
  changed <- Filter(function(x) !identical(x$classes, "unchanged"), status$diff)
  if (!length(changed)) {
    dina_cli_ok("No source file changes detected against the baseline.")
  } else {
    dina_cli_cat("Changed sources:")
    for (item in changed) {
      dina_cli_cat(sprintf("  %s: %s", item$id, paste(item$classes, collapse = ",")))
    }
  }
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
    if (result$stata$available) {
      dina_cli_ok(sprintf("%s (%s)", result$stata$command, result$stata$source))
    } else {
      dina_cli_warn(sprintf("Configured via %s but not runnable: %s", result$stata$source, result$stata$command))
      if (isTRUE(result$stata$discovered)) {
        dina_cli_alert(sprintf("Discovered Stata via %s: %s", result$stata$discovered_source, result$stata$discovered_command))
        dina_cli_cat(sprintf("  %s", result$stata$suggestion))
      }
    }
  } else if (isTRUE(result$stata$discovered)) {
    dina_cli_warn("Installed but not configured for DINA.")
    dina_cli_alert(sprintf("Discovered Stata via %s: %s", result$stata$discovered_source, result$stata$discovered_command))
    dina_cli_cat(sprintf("  %s", result$stata$suggestion))
  } else {
    dina_cli_warn("Not configured and no Stata executable was discovered. Set DINA_STATA_CMD or config/dina.yml stata.command.")
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
    if (!dina_confirm_continue("Install missing packages now? [y/N] ")) {
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
    flags <- dina_parse_flags(rest)
    if (length(flags$positional) > 1L) {
      stop("Usage: dina update start [YEAR] [--yes] [--no-source-hash]", call. = FALSE)
    }
    year <- dina_arg(flags$positional, 1L, format(Sys.Date(), "%Y"))
    plan <- dina_update_start_plan(year, root)
    if (isTRUE(plan$requires_confirmation) && !isTRUE(flags$yes)) {
      if (isTRUE(plan$incomplete)) {
        dina_cli_warn(sprintf("Same-day update directory exists but manifest is missing: %s.", plan$default_id))
        dina_cli_alert(sprintf("Use `dina update restart %s` to rebuild the same update id.", plan$default_id))
      } else {
        dina_cli_warn(sprintf("Unfinished same-day update already exists: %s.", plan$default_id))
      }
      dina_cli_alert(sprintf("Starting now would create a separate session: %s.", plan$id))
      if (!dina_confirm_continue()) {
        dina_cli_alert("No changes made. Pass --yes for non-interactive creation of the suffixed session.")
        quit(status = if (isatty(stdin())) 0 else 1)
      }
    } else if (isTRUE(plan$finalized)) {
      dina_cli_warn(sprintf("Finalized same-day update already exists: %s. Creating %s.", plan$default_id, plan$id))
    } else if (isTRUE(plan$incomplete)) {
      dina_cli_warn(sprintf("Same-day update directory exists but manifest is missing: %s. Creating separate session %s.", plan$default_id, plan$id))
      dina_cli_alert(sprintf("Use `dina update restart %s` instead to rebuild the same update id.", plan$default_id))
    }
    dina_cli_header("Update Start")
    session <- dina_update_start(
      year = year,
      id = plan$id,
      root = root,
      source_hash = !isTRUE(flags[["no-source-hash"]]),
      progress = dina_cli_progress
    )
    dina_cli_ok(sprintf("Started update session %s", session$id))
    dina_cli_alert(sprintf("Session directory: %s", dina_relative(dina_update_dir(session$id, root), root)))
    dina_cli_alert(sprintf("Source baseline hash mode: %s", session$source_baseline$hash_mode %||% "none"))
    dina_cli_ok("Recommended next action: dina sources status")
  } else if (sub %in% c("resume", "status")) {
    active <- dina_current_update(root)
    session <- dina_load_session(root = root)
    if (is.null(session)) {
      if (!is.null(active) && dir.exists(dina_update_dir(active, root))) {
        dina_cli_header(sprintf("Update %s", active))
        dina_cli_warn("Active update directory exists, but manifest.json is missing.")
        dina_cli_alert(sprintf("Session directory: %s", dina_relative(dina_update_dir(active, root), root)))
        dina_cli_ok(sprintf("Recommended next action: dina update restart %s", active))
        return(invisible(NULL))
      }
      stop("No active update. Run `dina update start YEAR`.", call. = FALSE)
    }
    state <- dina_session_state(session, root)
    dina_cli_header(sprintf("Update %s", session$id))
    dina_cli_alert(sprintf("Status: %s", session$status))
    dina_cli_alert(sprintf("State: %s", state$state))
    dina_print_update_source_summary(session)
    dina_cli_ok(sprintf("Recommended next action: %s", state$recommendation))
  } else if (identical(sub, "checklist")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    dina_cli_header("Checklist")
    for (item in session$checklist) {
      dina_cli_cat(sprintf("[%s] %s - %s", item$status, item$id, item$label))
    }
  } else if (identical(sub, "list")) {
    updates <- dina_update_list(root)
    dina_cli_header("Update Sessions")
    if (!nrow(updates)) {
      dina_cli_warn("No update sessions found.")
      return(invisible(updates))
    }
    dina_cli_cat(sprintf("%-6s %-28s %-6s %-12s %-25s %-25s", "active", "id", "year", "status", "created", "updated"))
    for (i in seq_len(nrow(updates))) {
      dina_cli_cat(sprintf(
        "%-6s %-28s %-6s %-12s %-25s %-25s",
        updates$active[[i]], updates$id[[i]], updates$year[[i]], updates$status[[i]],
        updates$created_at[[i]], updates$updated_at[[i]]
      ))
    }
    invisible(updates)
  } else if (identical(sub, "delete")) {
    flags <- dina_parse_flags(rest)
    if (length(flags$positional) > 1L) {
      stop("Usage: dina update delete [ID] [--yes]", call. = FALSE)
    }
    result <- dina_update_delete(dina_arg(flags$positional, 1L, NULL), root = root, yes = isTRUE(flags$yes))
    if (isTRUE(result$dry_run)) {
      dina_cli_warn(sprintf("Would delete update %s at %s.", result$id, result$dir))
      if (isTRUE(result$active)) {
        dina_cli_warn("This is the active update; deletion would clear the active pointer.")
      }
      if (!dina_confirm_continue()) {
        dina_cli_alert("No changes made. Pass --yes for non-interactive deletion.")
        return(invisible(result))
      }
      result <- dina_update_delete(result$id, root = root, yes = TRUE)
      dina_cli_ok(sprintf("Deleted update %s.", result$id))
      if (isTRUE(result$active)) {
        dina_cli_alert("Cleared active update pointer.")
      }
    } else {
      dina_cli_ok(sprintf("Deleted update %s.", result$id))
      if (isTRUE(result$active)) {
        dina_cli_alert("Cleared active update pointer.")
      }
    }
  } else if (identical(sub, "restart")) {
    flags <- dina_parse_flags(rest)
    if (length(flags$positional) > 1L) {
      stop("Usage: dina update restart [ID] [--yes]", call. = FALSE)
    }
    update_id <- dina_arg(flags$positional, 1L, NULL)
    if (isTRUE(flags$yes)) {
      dina_cli_header("Update Restart")
    }
    result <- dina_update_restart(update_id, root = root, yes = isTRUE(flags$yes), progress = if (isTRUE(flags$yes)) dina_cli_progress else NULL)
    if (isTRUE(result$dry_run)) {
      dina_cli_warn(sprintf("Would reset update %s from scratch.", result$id))
      dina_cli_alert(sprintf("Year: %s", result$year))
      dina_cli_alert(sprintf("Current status: %s", result$current_status))
      dina_cli_alert(sprintf("Session directory: %s", result$dir))
      dina_cli_alert(sprintf(
        "Files to clear: %s staged, %s logs, %s snapshots.",
        result$staged_files,
        result$log_files,
        result$snapshot_files
      ))
      dina_cli_alert("Restart reuses the same update id; no suffixed session will be created.")
      if (!dina_confirm_continue()) {
        dina_cli_alert("No changes made. Pass --yes for non-interactive restart.")
        return(invisible(result))
      }
      dina_cli_header("Update Restart")
      result <- dina_update_restart(result$id, root = root, yes = TRUE, progress = dina_cli_progress)
      dina_cli_ok(sprintf("Restarted update %s from scratch.", result$id))
    } else {
      dina_cli_ok(sprintf("Restarted update %s from scratch.", result$id))
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

dina_source_list_filters <- function(root, flags) {
  family <- dina_source_resolve_family_filter(flags$family %||% NULL, root)
  method <- dina_source_resolve_method_filter(flags$method %||% NULL)
  country <- flags$country %||% NULL
  if (!is.null(country) && nzchar(country)) {
    country <- toupper(country)
  }
  list(
    family = family,
    family_label = dina_source_filter_label("family", flags$family %||% NULL, family),
    method = method,
    method_label = dina_source_filter_label("method", flags$method %||% NULL, method),
    country = country
  )
}

dina_print_source_list <- function(root, flags) {
  filters <- dina_source_list_filters(root, flags)
  registry <- dina_source_registry(root, family = filters$family, country = filters$country, method = filters$method)
  dina_cli_header("Source Registry")
  if (!is.null(filters$family_label)) {
    dina_cli_alert(sprintf("Filter: family %s", filters$family_label))
  }
  if (!is.null(filters$country) && nzchar(filters$country)) {
    dina_cli_alert(sprintf("Filter: country %s, including broad-country sources", filters$country))
  }
  if (!is.null(filters$method_label)) {
    dina_cli_alert(sprintf("Filter: method %s", filters$method_label))
  }
  if (!length(registry)) {
    dina_cli_warn("No sources matched.")
    return(invisible(registry))
  }
  dina_cli_cat(sprintf(
    "%-36s %-18s %-12s %-14s %5s %-4s %-10s %-11s",
    "id", "family", "country", "method", "paths", "urls", "downloader", "transformer"
  ))
  for (source in registry) {
    urls <- dina_source_urls(source)
    dina_cli_cat(sprintf(
      "%-36s %-18s %-12s %-14s %5s %-4s %-10s %-11s",
      source$id %||% "",
      source$family %||% "",
      dina_source_country_summary(source, root),
      source$method %||% "",
      length(dina_source_values(dina_source_field(source, "canonical"))),
      if (length(urls)) "yes" else "no",
      if (dina_source_has_value(dina_source_field(source, "downloader"))) "yes" else "no",
      if (dina_source_has_value(dina_source_field(source, "transformer"))) "yes" else "no"
    ))
    if (isTRUE(flags$urls) && length(urls)) {
      for (url in urls) dina_cli_cat(sprintf("  url: %s", url))
    }
  }
  invisible(registry)
}

dina_print_source_field <- function(label, values) {
  values <- dina_source_values(values)
  if (!length(values)) {
    return(invisible(NULL))
  }
  dina_cli_cat(sprintf("%s:", label))
  for (value in values) dina_cli_cat(sprintf("  - %s", value))
}

dina_source_list_usage <- function() {
  paste(
    "Usage: dina sources list [--family FAMILY] [--country ISO] [--method METHOD] [--urls]",
    "       dina sources list country ISO [--urls]",
    "       dina sources list family FAMILY [--urls]",
    "       dina sources list method METHOD [--urls]",
    sep = "\n"
  )
}

dina_parse_source_list_flags <- function(args) {
  flags <- dina_parse_flags(args)
  allowed <- c("positional", "family", "country", "method", "urls")
  friendly_filters <- c("country", "family", "method")
  positional <- flags$positional %||% character()
  extra <- setdiff(names(flags), allowed)

  if (length(positional)) {
    filter_name <- positional[[1]]
    if (!filter_name %in% friendly_filters) {
      stop("Unknown sources list filter: ", paste(positional, collapse = " "), "\n", dina_source_list_usage(), call. = FALSE)
    }
    if (!is.null(flags[[filter_name]])) {
      stop("Duplicate sources list filter: ", filter_name, "\n", dina_source_list_usage(), call. = FALSE)
    }
    if (length(positional) == 2L) {
      flags[[filter_name]] <- positional[[2]]
    } else if (length(positional) == 1L && length(extra) == 1L && isTRUE(flags[[extra[[1]]]])) {
      flags[[filter_name]] <- extra[[1]]
      flags[[extra[[1]]]] <- NULL
    } else {
      stop(dina_source_list_usage(), call. = FALSE)
    }
    flags$positional <- character()
  }

  extra <- setdiff(names(flags), allowed)
  if (length(extra)) {
    stop("Unknown sources list option: --", extra[[1]], "\n", dina_source_list_usage(), call. = FALSE)
  }
  if (isTRUE(flags$family)) {
    stop("Missing value for --family\n", dina_source_list_usage(), call. = FALSE)
  }
  if (isTRUE(flags$country)) {
    stop("Missing value for --country\n", dina_source_list_usage(), call. = FALSE)
  }
  if (isTRUE(flags$method)) {
    stop("Missing value for --method\n", dina_source_list_usage(), call. = FALSE)
  }
  flags
}

dina_print_source_methods <- function() {
  methods <- dina_source_method_glossary()
  dina_cli_header("Source Methods")
  dina_cli_cat(sprintf("%-8s %-7s %s", "method", "refresh", "meaning"))
  for (i in seq_len(nrow(methods))) {
    dina_cli_cat(sprintf("%-8s %-7s %s", methods$method[[i]], methods$refresh[[i]], methods$description[[i]]))
  }
  invisible(methods)
}

dina_print_source_show <- function(root, id, include_urls = FALSE) {
  source <- dina_source_by_id(id, root)
  dina_cli_header(sprintf("Source %s", source$id))
  dina_cli_cat(sprintf("family: %s", source$family %||% ""))
  dina_cli_cat(sprintf("country: %s", dina_source_country_summary(source, root)))
  coverage <- dina_source_country_values(source, root)
  if (length(coverage) > 1L) {
    dina_print_source_field("country coverage", coverage)
  }
  method <- source$method %||% ""
  method_description <- dina_source_method_description(method)
  if (nzchar(method_description)) {
    dina_cli_cat(sprintf("method: %s - %s", method, method_description))
  } else {
    dina_cli_cat(sprintf("method: %s", method))
  }
  urls <- dina_source_urls(source)
  if (length(urls)) {
    dina_cli_cat("urls:")
    for (url in urls) dina_cli_cat(sprintf("  - %s", url))
  } else if (isTRUE(include_urls)) {
    dina_cli_cat("urls: none")
  }
  dina_print_source_field("canonical", source$canonical %||% character())
  dina_print_source_field("staging_name", source$staging_name %||% character())
  dina_print_source_field("downloader", source$downloader %||% character())
  dina_print_source_field("transformer", source$transformer %||% character())
  dina_print_source_field("checks", source$checks %||% character())
  dina_print_source_field("notes", source$notes %||% character())
  invisible(source)
}

dina_cmd_sources <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "scan")
  if (identical(sub, "list")) {
    flags <- dina_parse_source_list_flags(args[-1])
    dina_print_source_list(root, flags)
  } else if (identical(sub, "show")) {
    flags <- dina_parse_flags(args[-1])
    id <- dina_arg(flags$positional, 1L, NULL)
    if (is.null(id)) stop("Usage: dina sources show ID [--urls]", call. = FALSE)
    dina_print_source_show(root, id, include_urls = isTRUE(flags$urls))
  } else if (identical(sub, "methods")) {
    dina_print_source_methods()
  } else if (identical(sub, "status")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(args[-1])
    hash_mode <- if (isTRUE(flags[["metadata-only"]])) {
      "none"
    } else if (isTRUE(flags[["hash-all"]])) {
      "all"
    } else {
      "changed"
    }
    status <- dina_sources_status(session, root, hash = hash_mode, deep = isTRUE(flags$deep))
    dina_print_source_status(status)
  } else if (identical(sub, "complete")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(args[-1])
    status <- flags$status %||% dina_arg(flags$positional, 1L, NULL)
    if (is.null(status)) {
      stop("Usage: dina sources complete --status STATUS [--note TEXT]", call. = FALSE)
    }
    review <- dina_sources_complete(session, root, status = status, note = flags$note %||% "")
    dina_cli_ok(sprintf("Recorded source review: %s", review$status))
    if (nzchar(review$note %||% "")) {
      dina_cli_alert(sprintf("Note: %s", review$note))
    }
  } else if (identical(sub, "scan")) {
    session <- dina_load_session(root = root)
    flags <- dina_parse_flags(args[-1])
    scan <- dina_scan_sources(root, deep = isTRUE(flags$deep), hash = isTRUE(flags$hash) || isTRUE(flags$deep))
    dina_cli_header("Source Scan")
    for (x in scan) {
      dina_cli_cat(sprintf("%s [%s/%s]: years=%s files=%s", x$id, x$family, x$country, paste(x$detected_years, collapse = ","), length(x$files)))
    }
    if (!is.null(session)) {
      session$latest_source_scan <- scan
      session$latest_source_scan_at <- dina_now()
      session$updated_at <- dina_now()
      dina_save_session(session, root)
    }
  } else if (identical(sub, "diff")) {
    session <- dina_load_session(root = root)
    flags <- dina_parse_flags(args[-1])
    previous <- if (!is.null(session)) session$source_scan else list()
    current <- dina_scan_sources(root, deep = isTRUE(flags$deep), hash = isTRUE(flags$hash) || isTRUE(flags$deep), previous = previous)
    diff <- dina_classify_source_changes(current, previous)
    dina_cli_header("Source Diff")
    for (x in diff) {
      dina_cli_cat(sprintf(
        "%s: %s current_years=%s previous_years=%s counts=%s",
        x$id,
        paste(x$classes, collapse = ","),
        paste(x$current_years, collapse = ","),
        paste(x$previous_years, collapse = ","),
        dina_source_counts_line(x$counts)
      ))
    }
  } else if (identical(sub, "review")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    staged <- list.files(file.path(dina_update_dir(session$id, root), "source_staging"), recursive = TRUE)
    dina_cli_header("Staged Sources")
    if (!length(staged)) dina_cli_warn("No staged downloads found.") else dina_cli_cat(paste(staged, collapse = "\n"))
  } else if (identical(sub, "refresh")) {
    session <- dina_load_session(root = root)
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
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
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
    dina_cli_cat(sprintf("%-6s %-38s %-14s %-8s %s", "alias", "id", "stage", "language", "status"))
    for (x in statuses) {
      dina_cli_cat(sprintf("%-6s %-38s %-14s %-8s %s", dina_task_short_id(x$id), x$id, x$stage, x$language, x$status))
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
  dina_main(root = dina_repo_root(dina_cli_root)),
  error = function(e) {
    dina_cli_err(conditionMessage(e))
    quit(status = 1)
  }
)
