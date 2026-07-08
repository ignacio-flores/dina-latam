#!/usr/bin/env Rscript

options(warn = 1)

source_file <- function() {
  override <- Sys.getenv("DINA_CLI_SOURCE_FILE", unset = "")
  if (nzchar(override)) {
    return(normalizePath(override, mustWork = TRUE))
  }
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

dina_cli_name <- function(text) {
  as.character(text)
}

dina_cli_dim <- function(text) {
  text <- as.character(text)
  if (dina_cli_has("cli")) cli::col_grey(text) else text
}

dina_cli_command <- function(text) {
  text <- as.character(text)
  if (dina_cli_has("cli")) cli::col_cyan(text) else text
}

dina_cli_cell <- function(value, width = NULL, dim = FALSE, align = "left") {
  text <- as.character(value %||% "")
  if (!is.null(width)) {
    fmt <- if (identical(align, "right")) sprintf("%%%ss", width) else sprintf("%%-%ss", width)
    text <- sprintf(fmt, text)
  }
  if (isTRUE(dim)) dina_cli_dim(text) else text
}

dina_cli_row <- function(values, widths = NULL, dim = FALSE, align = NULL, sep = " ") {
  n <- length(values)
  widths <- widths %||% rep(NA_integer_, n)
  dim <- rep(dim, length.out = n)
  align <- align %||% rep("left", n)
  cells <- vapply(seq_len(n), function(i) {
    width <- widths[[i]]
    if (is.na(width)) width <- NULL
    dina_cli_cell(values[[i]], width = width, dim = dim[[i]], align = align[[i]])
  }, character(1))
  paste(cells, collapse = sep)
}

dina_cli_key_value <- function(label, value, dim_value = TRUE) {
  sprintf("%s %s", label, if (isTRUE(dim_value)) dina_cli_dim(value) else value)
}

dina_cli_cat <- function(...) {
  cat(paste0(...), "\n", sep = "")
}

dina_cli_header <- function(text) {
  if (dina_cli_has("cli")) cli::cli_h1(text) else dina_cli_cat("\n", text, "\n")
}

dina_cli_alert <- function(text) {
  if (dina_cli_has("cli")) cli::cli_alert_info(dina_cli_dim(text)) else dina_cli_cat("* ", text)
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
  if (identical(topic, "bucket")) topic <- "buckets"
  switch(
    topic,
    main = "DINA-LatAm CLI

Usage:
  dina
  dina help [TOPIC]
  dina commands
  dina navigate
  dina COMMAND [SUBCOMMAND] [OPTIONS]

Plain `dina` opens the guided dashboard and recommends the next action.
Use `dina commands` or `dina navigate` to browse available commands.
Use `dina help workflow` for the annual update workflow guide.
Use `dina help COMMAND` for command details.
After `dina setup command`, `dina` and `./bin/dina` are equivalent.

Command map

Annual update:
  `help workflow`                     [read-only] annual update workflow guide
  `update start [YEAR]`               [writes session] create active session
  `update status|resume`              [read-only] inspect active session
  `update close [--dry-run]`          [writes session] closure report
  `update list`                       [read-only] list update sessions
  `update restart|delete`             [writes session] lifecycle controls
  `todo [check|uncheck|reset]`        [writes session] helper checklist

Source data:
  `sources list [SOURCETYPE]`         [read-only] compact source registry
  `sources list detail|guide`         [read-only] source detail/reminders
  `sources fetch [--dry-run]`         [read-only/writes inbox] fetch to _new
  `sources compare`                   [read-only] compare source baseline
  `sources explore SOURCETYPE`        [writes experiment] inspect _new files
  `sources include SOURCETYPE`        [writes experiment] stage/check inclusion
  `sources table SOURCETYPE TABLE`    [read-only] preview explore tables
  `sources status`                    [read-only] alias for sources compare
  `sources diff`                      [read-only] detailed source baseline diff
  `sources fields`                    [read-only] source option cheat sheet

Pipeline:
  `run list|why TASK`                 [read-only] inspect task freshness
  `run stale [--dry-run]`             [writes files] run stale/missing tasks
  `run TASK ...`                      [writes files] run selected scripts
  `run TASK ... --dry-run`            [read-only] preview selected scripts

Navigation:
  `commands|navigate`                 [interactive] browse available commands

Setup and config:
  `doctor`                            [read-only] check local readiness
  `install`                           install missing R packages
  `config show|check`                 [read-only] inspect benchmark config
  `update config show|edit`           [writes override] active update config
  `compress input`                    [writes zip] bundle input_data
  `notify init|test`                  configure or test Pushover
  `setup command`                     install the user-level `dina` wrapper

Maintenance:
  `maintain repo-status|repo-diff`    [read-only] compare repo baseline
  `maintain repo-restore`             [writes files] restore captured files
  `audit paths`                       [read-only] report likely hardcoded paths
  `make export [PATH]`                [writes files] export task graph

Pipeline selectors
  `01a`                  one task, such as `01a-clean-macro-data`
  `01`                   whole numbered block for `dina run`
  `01a,02a`              multiple tasks for `dina run`
  `--from 03 --to 05`    range for `dina run`
  `run why` needs a unique selector, such as `01a` or a full task id.

Critical defaults
  Update work is a lightweight workspace, not a blocking checklist.
  `dina run TASK` executes selected scripts. Use `--dry-run` to preview.
  `update start` records a source baseline with hashes by default.
  `update start` creates a suggested working config override.
  Working overrides do not change the benchmark automatically.
  `sources fetch` writes supported downloads directly to `input_data/_new`.
  `todo` is a helper list only; it never blocks sources, run, config, or close.

Notes:
  `--` is accepted as an optional separator for shell compatibility, but it is
  never required. For example, `dina help` and `dina -- help` both work.

Examples:
  dina
  dina commands
  dina help workflow
  dina update start YEAR
  dina sources list
  dina sources fetch --dry-run
  dina compress input --dry-run
  dina run 01a
  dina run 01a --dry-run
",
    workflow = "Usage:
  dina help workflow

What this page is:
  The annual update guide. The CLI is organized around sources, pipeline runs,
  and lightweight workspace maintenance. The todo list is only a helper.

1. Start or resume the workspace
  dina doctor
      Read-only preflight for R packages, Stata, paths, notifications, and
      the active update pointer.

  dina update start [YEAR]
      Creates `output/updates/<update_id>`, records a hashed source baseline,
      prepares source inbox buckets, writes a suggested working override,
      records a repo-state baseline, and makes the session active.

  dina update status
      Summarizes incoming sources, task freshness, unchecked todos, config
      override state, and repo changes.

  dina update config show
      Shows the benchmark `config/dina.yml`, the active update override if one
      exists, and the effective merged config.

  dina maintain repo-status
      Compares current code/config/docs with the session start baseline.

2. Work from sources
  dina sources list
      Shows compact rows with id, source type, country, method, URL hint,
      bucket, destination, transformer, and expected influence. Interactive
      terminals get a short dismissible follow-up menu.

  dina sources list detail ID --urls
  dina sources list guide ID --urls
  dina sources list SOURCETYPE
      Shows where a source comes from, where it lands, what transforms it, and
      what tasks likely consume it.

  dina sources fetch [ID|SOURCETYPE|--all] [--dry-run]
      Fetches supported sources directly into `input_data/_new` buckets.

  dina sources compare [--metadata-only] [--hash-all] [--deep]
      Compares configured source files against the active update baseline. This
      does not validate data or manage incoming files.

  dina sources explore SOURCETYPE [--dry-run]
      Inventories new files, likely extension years, overlap years,
      structure evidence, and broad include expectations under
      output/experiments. Implemented for sna.

  dina sources include SOURCETYPE --dry-run
      Uses the deterministic source include contract plus the latest
      exploration run to report all_good, check_following, or blocked. It does
      not replace pipeline tasks or run the pipeline. Implemented for sna and admin.

  dina sources table SOURCETYPE TABLE
      Previews explorer tables inline. Implemented for sna; variable
      expectations are summarized by default so the CLI does not dump thousands
      of rows.

  dina sources include SOURCETYPE --confirm --include-run RUN
      Promotes approved `_new` files from a clean staged include run only after
      writing a backup snapshot. It does not run the pipeline.

  dina sources include SOURCETYPE --restore CONFIRM_RUN
      Restores canonical source files from the backup snapshot written by
      confirm.

3. Run the pipeline
  dina run list
  dina run why TASK
  dina run TASK --dry-run
  dina run TASK
      `pipeline.yml` task ids remain the executable graph. `dina run TASK`
      executes by default; use `--dry-run` to preview.

Task selectors:
  01a                  One task.
  01                   Whole numbered block for `dina run`.
  01a,02a              Multiple tasks for `dina run`.
  full-task-id         Exact task id.
  --from 03 --to 05    Range from block 03 through block 05.
  `run why` needs one unique task selector, such as 01a or a full task id.

4. Keep a loose todo list
  dina todo
  dina todo check ID
  dina todo uncheck ID
  dina todo reset
      Tracks simple human reminders in the active update manifest. Todos never
      block pipeline runs, config, or closure.

5. Close with a report
  dina update close [--dry-run]
      Generates closure notes: changed sources, incoming source files, run
      status, output freshness, config diff, and repo diff. It reports state
      rather than enforcing completion.

More help:
  dina help workflow   dina help commands   dina help update
  dina help sources    dina help run        dina help todo
  dina help config     dina help maintain
",
    commands = "Usage:
  dina commands
  dina navigate

What it does:
  Opens the command navigator deliberately. The navigator groups documented
  CLI commands by workflow area, prompts for required placeholders, previews
  completed commands when needed, and can run the selected command.

What it changes:
  The navigator itself changes nothing. A selected command may be read-only or
  mutating; mutating commands still use their normal command behavior.

Non-interactive use:
  In scripts, these commands print the command themes and templates instead of
  opening an interactive menu.

Examples:
  dina commands
  dina navigate
",
    navigate = dina_help_text("commands"),
    menu = "Usage:
  dina menu
  dina menu commands

What it does:
  Compatibility aliases for the command navigator. Prefer `dina commands` or
  `dina navigate` in docs and scripts.

Examples:
  dina commands
  dina navigate
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
  dina update close [--dry-run]
  dina update list
  dina update restart [ID] [--yes] [--replace-repo-baseline] [--save-restart-checkpoint]
  dina update delete [ID] [--yes]

What it manages:
  Annual update workspaces under `output/updates/<update_id>`. A workspace
  stores the active pointer, config override path, source baseline, repo
  baseline, todo state, incoming bucket summary, source decisions, and run
  records. It reports state without enforcing blockers.

Subcommands:
  start [YEAR]                    Creates a new workspace, active pointer,
                                  source baseline, repo-state baseline, todo
                                  state, and central `input_data/_new` buckets.
                                  If omitted, YEAR defaults to the current
                                  calendar year. Default id:
                                  YEAR-update-MM-DD. If an unfinished same-day
                                  workspace exists, creating YEAR-update-MM-DD-02
                                  requires confirmation.
  resume                          Recomputes reality and recommends the next
                                  action. It does not blindly continue a run.
  status                          Same state summary as resume, without implying
                                  that work should continue automatically.
  close [--dry-run]               Generates closure notes: changed sources,
                                  incoming source files, run summary, output
                                  freshness, config diff, and repo diff. Without
                                  --dry-run, marks the workspace closed after
                                  report generation.
  list                            Lists update workspaces and marks the active
                                  one with `*`.
  restart [ID] [--yes]            Resets one workspace from scratch using the
                                  same id. If ID is omitted, uses the active
                                  workspace. It keeps `input_data/_new` buckets
                                  by default and can preserve, checkpoint, or
                                  replace the repo baseline.
  delete [ID] [--yes]             Deletes an update workspace. If ID is omitted,
                                  uses the active workspace. Without --yes,
                                  interactive terminals ask before deleting;
                                  scripts only preview.

What it changes:
  `start`, `close`, `restart`, and `delete` write or remove workspace records.
  Repo restore lives under `dina maintain repo-restore`.

Options:
  --no-source-hash                For start, record only file size/timestamp in
                                  the source baseline. The default computes
                                  source hashes for later comparison.
  --replace-repo-baseline         For restart, discard the original repo
                                  baseline and capture current repo state.
  --save-restart-checkpoint       For restart, keep the start baseline and also
                                  save current code/config/docs before reset.
  --yes                           For start/restart/delete, skip confirmation.
  --dry-run                       For close, print the closure report without
                                  changing workspace status.

Examples:
  dina update start YEAR
  dina update status
  dina update close --dry-run
  dina update close
  dina update list
  dina update restart --yes
  dina update delete 2026-update-06-29 --yes
  dina update resume
",
    sources = "Usage:
  dina sources list [SOURCETYPE] [--country ISO] [--urls] [--no-menu]
  dina sources list detail ID [--urls]
  dina sources list guide [ID|SOURCETYPE] [--urls]
  dina sources list workflow [SOURCETYPE] [--urls]
  dina sources list paths [SOURCETYPE] [--urls]
  dina sources list urls [SOURCETYPE|ID]
  dina sources fetch [ID|SOURCETYPE|--all] [--dry-run]
  dina sources compare [--metadata-only] [--hash-all] [--deep]
  dina sources explore SOURCETYPE [--dry-run] [--output-dir PATH] [--country ISO]
  dina sources include SOURCETYPE [--dry-run] [--exploration-run PATH] [--output-dir PATH]
  dina sources include SOURCETYPE --confirm --include-run RUN
  dina sources include SOURCETYPE --restore CONFIRM_RUN
  dina sources table SOURCETYPE TABLE [--run PATH] [--country ISO] [--limit N]
  dina sources status [--metadata-only] [--hash-all] [--deep]
  dina sources diff [--deep] [--hash]
  dina sources fields
  dina sources methods

What it manages:
  Source registry, incoming `_new` buckets, fetchers, and baseline comparison.
  Supported fetches write directly to `input_data/_new`. Public source types are
  sna, admin, admin-microdata, surveys, wid, and other. SNA and PIT admin have
  explore/include/table automation for now; they do not replace pipeline tasks.

Subcommands:
  list                            Default compact registry view. Columns: id,
                                  type, country, method, URL hint, bucket,
                                  destination, transformer, and influence.
                                  Interactive terminals show a short menu unless
                                  --no-menu is passed.
  list detail ID                  Sectioned single-source card: identity,
                                  acquisition, incoming bucket, destination,
                                  processing code, notes, usage, and URLs when
                                  requested.
  list guide                      Human-friendly reminder for where to get a
                                  source, where it lands, what code transforms
                                  it, and what task likely consumes it.
  list workflow|paths|urls        Discoverable list views. `--view` remains a
                                  compatibility spelling.
  fetch                           Runs configured fetchers or direct URL/ZIP
                                  downloads into primary `_new` buckets. If no
                                  safe target exists, prints manual guidance.
  compare                         Compares configured source files with the
                                  active update baseline. It does not validate
                                  data or manage incoming files.
  explore SOURCETYPE              Inventories incoming files for a source type.
                                  Currently implemented for sna and admin.
  include SOURCETYPE              Consumes an exploration run and checks the
                                  deterministic include contract. Currently
                                  implemented for sna and PIT admin. Dry-run is
                                  the default and stages files under the run.
  table SOURCETYPE                Previews explore output tables inline.
                                  Currently implemented for sna and admin.
  status                          Compatibility alias for compare.
  diff                            Compares current scan results with the active
                                  session baseline and classifies changes.
  fields                          Cheat sheet for filters, views, registry
                                  fields, and method labels.
  methods                         Explains acquisition method labels.

Options:
  --view VIEW                     Compatibility spelling for list views:
                                  compact, workflow, paths, all.
  --no-menu                       Suppress the interactive follow-up menu.
  --source ID                     Compatibility spelling for ID on fetch.
  --all                           For fetch, process every eligible matching
                                  source.
  SOURCETYPE                      Keep a public source type: sna, admin,
                                  admin-microdata, surveys, wid, other.
                                  Internal family names also work as
                                  compatibility selectors.
  --country ISO                   For list, keep one ISO country plus broad
                                  country sources.
  country ISO                     Friendly form of --country ISO for list.
  --urls                          Print or expand source URLs.
  --deep                          Inspect workbook sheets when possible.
  --hash                          Compute file hashes during scan/diff.
  --metadata-only                 For compare/status, compare only paths, size, and
                                  timestamps; do not compute hashes.
  --hash-all                      For compare/status, hash all source files.
  --dry-run                       For fetch, preview targets only. For explore,
                                  compute summaries without writing outputs.
                                  For include, perform the default no-promotion
                                  assessment.
  --confirm                       For include, promote approved `_new` files
                                  from a clean staged include run.
  --include-run RUN               Staged include run path or id required by
                                  --confirm.
  --restore CONFIRM_RUN           Restore canonical source files from a confirm
                                  backup snapshot.
  --apply                         Retired. Use --confirm.
  --run PATH                      For table, use a specific explore run.
  --limit N                       For table, limit preview rows.
  --exploration-run PATH          For include, use a specific explore output
                                  folder instead of the default latest folder.
  --output-dir PATH               For explore/include, write experiment outputs
                                  somewhere other than the default experiment
                                  folder.

Gotcha:
  Source coverage is independent of update year. A 2026 update may discover
  newly available 2024 data or historical backfills.
  URL presence does not mean automatic download. Manual, script, and WID sources
  can have URLs that you inspect, download from, or use for verification.
  `_new` folders are incoming source buckets. Pipeline scripts consume canonical
  paths only, never `_new` directly.

Methods:
  url                             Direct URL fetchable by `sources fetch`.
  zip                             Direct archive URL fetchable by `sources fetch`.
  script                          Custom acquisition script or fetcher exists.
  manual                          Human-curated input or URL index.
  wid                             Data currently acquired through Stata/WID calls.

Examples:
  dina sources list
  dina sources list sna
  dina sources list workflow
  dina sources list urls wid
  dina sources list country CHL --urls
  dina sources fields
  dina sources list guide chl-pit-total --urls
  dina sources list detail country-sna-bra --urls
  dina sources fetch --dry-run
  dina sources fetch chl-pit-total --dry-run
  dina sources compare
  dina sources explore sna
  dina sources explore admin
  dina sources include sna --dry-run
  dina sources include admin --dry-run
  dina sources table sna year_expectations
  dina sources table admin
  dina sources table admin year_expectations
",
    buckets = "Usage:
  dina buckets [detail|urls|uses|fetch] [OPTIONS]

Compatibility:
  `dina buckets` is a temporary alias for source-bucket views now owned by
  `dina sources`. Prefer `dina sources list`, `dina sources list guide`,
  `dina sources fetch`, and `dina sources compare`.
",
    tasks = "Usage:
  dina tasks list
  dina tasks why TASK

Compatibility:
  `dina tasks` is a temporary alias. Prefer `dina run list` and
  `dina run why TASK`.
",
    run = "Usage:
  dina run list
  dina run why TASK
  dina run stale [--dry-run]
  dina run TASK [OPTIONS]
  dina run --task TASK [OPTIONS]

What it does:
  Selects tasks from `config/pipeline.yml`, checks freshness, then executes
  them. `dina run TASK` executes by default. Use --dry-run to print commands
  without running scripts.

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
  --dry-run                       Print commands without executing.
  --force                         Run even when a task appears current.
  --notify                        Send a Pushover message at completion/failure.

Subcommands:
  list                            Shows task alias, id, stage, language, and
                                  freshness status.
  why TASK                        Explains why one task is stale, missing,
                                  current, inactive, or failed.
  stale [--dry-run]               Runs all stale, missing, never-run, or failed
                                  active tasks. Use --dry-run to preview.

Stata:
  Stata tasks need a runnable command. If `dina doctor` finds Stata but says it
  is not configured, set DINA_STATA_CMD to the suggested executable path.

What it changes:
  By default, selected scripts may update data/output files and the CLI writes
  run logs under `output/run_logs/`. With --dry-run, nothing is executed.

Examples:
  dina run list
  dina run why 01a
  dina run stale --dry-run
  dina run 01a
  dina run 01a --dry-run
  dina run 01
  dina run --from 03 --to 05
  dina run 07d --notify
",
    config = "Usage:
  dina config show
  dina config check

What it manages:
  `config/dina.yml`, the benchmark project configuration. These commands are
  read-only; update workspaces use a separate working override.

Subcommands:
  show                            Prints the committed default YAML exactly as
                                  stored in `config/dina.yml`.
  check                           Reports required top-level config keys and
                                  whether legacy runtime files are visible.

What it changes:
  Nothing for the documented commands.

Examples:
  dina config show
  dina config check

Active update overrides:
  dina update config show
  dina update config edit
      These commands inspect or edit the active update's working override.
      No benchmark config file is changed.
",
    todo = "Usage:
  dina todo
  dina todo check ID
  dina todo uncheck ID
  dina todo reset

What it manages:
  A short YAML-backed helper list from `config/todo.yml`. Checked state lives in
  the active update manifest. Todos are reminders only and never block sources,
  runs, config, or close.

Examples:
  dina todo
  dina todo check review-config
  dina todo uncheck review-config
  dina todo reset
",
    compress = "Usage:
  dina compress input [OPTIONS]

What it manages:
  Zip bundles for selected project folders. The first supported target is
  `input`, which writes a `.zip` archive containing `input_data/`.

Options:
  --dry-run                       Preview the archive plan without writing.
  --dropbox                       Use ~/Dropbox/DINA-LatAm as both source root
                                  and output root. Tests may override this with
                                  DINA_DROPBOX_ROOT.
  --all                           Include every configured input source type.
  --include SOURCETYPE            Include a source type otherwise excluded by
                                  default. Example: admin-microdata.
  --exclude SOURCETYPE            Exclude a source type from the zip.
  --output PATH                   Write to PATH. Relative paths are relative to
                                  the selected source root.

Defaults:
  Source root: current repo.
  Output zip: output/archives/input-data-YYYY-MM-DD.zip.
  Included root: input_data.
  Excluded source types: admin-microdata.

Source types:
  Uses the same public SOURCETYPE vocabulary as `dina sources list`, including
  admin-microdata for the heavy MEX and URY admin-data folders.

Examples:
  dina compress input --dry-run
  dina compress input --dropbox
  dina compress input --all
  dina compress input --include admin-microdata
  dina compress input --exclude admin-microdata
  dina compress input --output output/archives/input-data.zip
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
    maintain = "Usage:
  dina maintain repo-status [--baseline NAME]
  dina maintain repo-diff [--stat] [--patch] [--files] [--baseline NAME]
  dina maintain repo-restore [--dry-run] [--yes] [--baseline NAME]
  dina maintain doctor
  dina maintain audit paths

What it manages:
  Setup checks and repo-baseline maintenance for the active update workspace.
  The older `dina update repo-*` spellings remain as temporary aliases.

Examples:
  dina maintain repo-status
  dina maintain repo-diff --stat --files
  dina maintain repo-restore --dry-run
  dina maintain doctor
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
  Installs a user-level `~/.local/bin/dina` wrapper that points to this checkout.
  The wrapper contains the absolute path to this repo's `code/R/cli/dina.R`, so
  normal code changes in this checkout are picked up immediately. Rerun setup
  only if you move the checkout, clone another copy you want `dina` to use, or
  replace the wrapper.

What it changes:
  Creates or overwrites that user-level wrapper. It does not install R packages
  or edit project config.

Options:
  `./bin/dina` always runs from the current checkout.
  Installed `dina` runs the checkout captured by `dina setup command`.
  `DINA_REPO_ROOT=/path/to/repo dina ...` can override the project root used for
  config/data/session lookup, but the installed wrapper still loads CLI code
  from the checkout recorded at setup time.
",
    sprintf("Unknown help topic `%s`. Run `dina help` for available commands.\n", topic)
  )
}

dina_help_heading_line <- function(line) {
  trimmed <- trimws(line)
  if (!nzchar(trimmed)) {
    return(FALSE)
  }
  if (identical(trimmed, "DINA-LatAm CLI")) {
    return(TRUE)
  }
  if (grepl("^[0-9]+\\.\\s", trimmed)) {
    return(TRUE)
  }
  if (grepl("^[A-Za-z][A-Za-z0-9 /&_-]*:$", trimmed)) {
    return(TRUE)
  }
  trimmed %in% c("Command map", "Pipeline selectors", "Critical defaults")
}

dina_help_command_line <- function(line) {
  body <- sub("^\\s+", "", line)
  grepl("^(dina|\\./bin/dina)(\\s|$)", body)
}

dina_help_dim_aligned_suffix <- function(line) {
  lead <- regmatches(line, regexpr("^\\s*", line))
  body <- substring(line, nchar(lead) + 1L)
  if (!nzchar(body) || grepl("^(dina|\\./bin/dina)(\\s|$)", body)) {
    return(line)
  }
  if (startsWith(body, "`") && nchar(lead) <= 2L) {
    close <- regexpr("`", substring(body, 2L), fixed = TRUE)[[1]]
    if (close > 0L) {
      close <- close + 1L
      prefix <- paste0(lead, substr(body, 1L, close))
      suffix <- substr(body, close + 1L, nchar(body))
      if (nzchar(trimws(suffix))) {
        return(paste0(prefix, dina_cli_dim(suffix)))
      }
      return(line)
    }
  }
  split <- regexpr("[[:space:]]{2,}", body)
  if (split[[1]] < 0L) {
    return(NULL)
  }
  split_start <- split[[1]]
  split_width <- attr(split, "match.length")
  prefix <- paste0(lead, substr(body, 1L, split_start + split_width - 1L))
  suffix <- substr(body, split_start + split_width, nchar(body))
  if (!nzchar(trimws(suffix))) {
    return(line)
  }
  paste0(prefix, dina_cli_dim(suffix))
}

dina_help_style_line <- function(line) {
  if (!nzchar(line) || dina_help_heading_line(line) || dina_help_command_line(line)) {
    return(line)
  }
  aligned <- dina_help_dim_aligned_suffix(line)
  if (!is.null(aligned)) {
    return(aligned)
  }
  dina_cli_dim(line)
}

dina_usage <- function(topic = NULL) {
  lines <- strsplit(dina_help_text(topic), "\n", fixed = TRUE)[[1]]
  for (line in lines) {
    cat(dina_help_style_line(line), "\n", sep = "")
  }
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

dina_cli_prompt_value <- function(prompt, default = "", input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(default)
  }
  answer <- trimws(dina_read_prompt(prompt, input = input))
  if (nzchar(answer)) answer else default
}

dina_menu_action <- function(key, label, value = key, description = "", group = "", disabled = FALSE, help = "", hidden = FALSE, right = NULL, command = "") {
  list(
    key = key,
    label = label,
    value = value,
    description = description,
    group = group,
    disabled = isTRUE(disabled),
    help = help,
    hidden = isTRUE(hidden),
    right = right,
    command = command
  )
}

dina_menu_actions <- function(...) {
  actions <- list(...)
  if (length(actions) == 1L && is.list(actions[[1]]) && !is.null(actions[[1]][[1]])) {
    actions <- actions[[1]]
  }
  actions
}

dina_menu_normalize <- function(items) {
  lapply(items, function(item) {
    if (is.character(item)) {
      return(dina_menu_action(item, item))
    }
    item$key <- item$key %||% item$value %||% item$label
    item$label <- item$label %||% item$key
    item$value <- item$value %||% item$key
    item$description <- item$description %||% ""
    item$group <- item$group %||% ""
    item$disabled <- isTRUE(item$disabled)
    item$hidden <- isTRUE(item$hidden)
    item$help <- item$help %||% item$details %||% ""
    item$right <- item$right %||% NULL
    item$command <- item$command %||% ""
    item
  })
}

dina_menu_action_value <- function(action) {
  action$value %||% action$key
}

dina_menu_enabled_indices <- function(items) {
  which(
    !vapply(items, function(item) isTRUE(item$disabled), logical(1)) &
      !vapply(items, function(item) isTRUE(item$hidden), logical(1))
  )
}

dina_menu_visible_indices <- function(items) {
  which(!vapply(items, function(item) isTRUE(item$hidden), logical(1)))
}

dina_menu_first_enabled <- function(items) {
  enabled <- dina_menu_enabled_indices(items)
  if (length(enabled)) enabled[[1]] else NA_integer_
}

dina_menu_index_for_value <- function(items, value) {
  if (is.null(value)) {
    return(NA_integer_)
  }
  values <- vapply(items, function(item) {
    candidate <- dina_menu_action_value(item)
    if (length(candidate) == 1L) as.character(candidate) else as.character(item$key %||% item$label)
  }, character(1))
  idx <- which(values == as.character(value) & !vapply(items, function(item) isTRUE(item$disabled), logical(1)))
  if (length(idx)) idx[[1]] else NA_integer_
}

dina_menu_index_for_values <- function(items, values) {
  for (value in values) {
    idx <- dina_menu_index_for_value(items, value)
    if (!is.na(idx)) {
      return(idx)
    }
  }
  NA_integer_
}

dina_menu_back_index <- function(items) {
  dina_menu_index_for_values(items, c("back", "previous"))
}

dina_menu_next_index <- function(items) {
  dina_menu_index_for_value(items, "next")
}

dina_menu_terminal_available <- function(input = "stdin", is_terminal = isatty(stdin())) {
  isTRUE(is_terminal) &&
    identical(input, "stdin") &&
    !identical(.Platform$OS.type, "windows") &&
    nzchar(Sys.getenv("TERM", unset = "")) &&
    !identical(Sys.getenv("TERM", unset = ""), "dumb") &&
    file.exists("/dev/tty")
}

dina_menu_open_tty_binary <- function() {
  suppressWarnings(tryCatch(
    file("/dev/tty", open = "rb", raw = TRUE),
    error = function(e) NULL
  ))
}

dina_menu_tty_state <- function() {
  suppressWarnings(tryCatch(
    system("stty -g < /dev/tty", intern = TRUE, ignore.stderr = TRUE),
    error = function(e) character()
  ))
}

dina_menu_set_raw <- function() {
  invisible(system("stty -echo -icanon min 1 time 0 < /dev/tty", ignore.stdout = TRUE, ignore.stderr = TRUE))
}

dina_menu_set_raw_poll <- function() {
  invisible(system("stty -echo -icanon min 0 time 1 < /dev/tty", ignore.stdout = TRUE, ignore.stderr = TRUE))
}

dina_menu_restore_tty <- function(state) {
  if (length(state) && nzchar(state[[1]])) {
    invisible(system(sprintf("stty %s < /dev/tty", state[[1]]), ignore.stdout = TRUE, ignore.stderr = TRUE))
  }
}

dina_menu_read_key <- function(con) {
  key <- readChar(con, nchars = 1L, useBytes = TRUE)
  if (identical(key, "\033")) {
    rest <- readChar(con, nchars = 2L, useBytes = TRUE)
    return(paste0(key, rest))
  }
  key
}

dina_menu_control_help <- function(items = NULL) {
  parts <- c("Up/Down move", "Enter select", "type number + Enter to select")
  if (!is.null(items)) {
    has_back <- !is.na(dina_menu_back_index(items))
    has_next <- !is.na(dina_menu_next_index(items))
    has_right <- any(!vapply(items, function(item) is.null(item$right), logical(1)))
    if (has_right && has_back) {
      parts <- c(parts, "Left back", "Right open", "p/n + Enter fallback")
    } else if (has_right) {
      parts <- c(parts, "Right open", "n + Enter fallback")
    } else if (has_back && has_next) {
      parts <- c(parts, "Left/Right back/next", "p/n + Enter fallback")
    } else if (has_back) {
      parts <- c(parts, "Left back", "p + Enter fallback")
    } else if (has_next) {
      parts <- c(parts, "Right next", "n + Enter fallback")
    }
  } else {
    parts <- c(parts, "Left/Right back/next when available", "p/n + Enter fallback")
  }
  paste(c(parts, "q quits", "? help"), collapse = " - ")
}

dina_menu_help_lines <- function(items, selected = NULL) {
  indices <- if (is.null(selected)) dina_menu_visible_indices(items) else selected
  indices <- indices[!vapply(items[indices], function(item) isTRUE(item$hidden), logical(1))]
  lines <- character()
  for (i in indices) {
    item <- items[[i]]
    text <- item$help %||% ""
    if (!nzchar(text)) {
      text <- item$description %||% ""
    }
    if (!nzchar(text)) {
      next
    }
    lines <- c(lines, sprintf("  %s:", item$label))
    for (line in strsplit(text, "\n", fixed = TRUE)[[1]]) {
      if (nzchar(line)) {
        lines <- c(lines, dina_cli_dim(sprintf("    %s", line)))
      }
    }
  }
  if (!length(lines)) {
    lines <- dina_cli_dim("  No extra help is available for this choice.")
  }
  lines
}

dina_menu_lines <- function(title, items, selected = NULL, prompt = "Choose an action", help = FALSE, context = character()) {
  context <- as.character(context %||% character())
  lines <- if (length(context)) c(context, "", title) else c("", title)
  if (nzchar(prompt %||% "")) {
    prompt_lines <- strsplit(prompt, "\n", fixed = TRUE)[[1]]
    lines <- c(lines, vapply(prompt_lines, function(line) dina_cli_dim(sprintf("  %s", line)), character(1)))
  }
  current_group <- NULL
  visible <- dina_menu_visible_indices(items)
  for (display_i in seq_along(visible)) {
    i <- visible[[display_i]]
    item <- items[[i]]
    group <- item$group %||% ""
    if (nzchar(group) && !identical(group, current_group)) {
      lines <- c(lines, "", dina_cli_dim(sprintf("  %s", group)))
      current_group <- group
    }
    marker <- if (!is.null(selected) && identical(i, selected)) ">" else " "
    disabled <- if (isTRUE(item$disabled)) dina_cli_dim(" (unavailable)") else ""
    line <- sprintf("%s %2s. %s%s", marker, display_i, item$label, disabled)
    if (nzchar(item$command %||% "")) {
      line <- sprintf("%s -- %s", line, dina_cli_command(sprintf("$ %s", item$command)))
    }
    lines <- c(lines, line)
    if (nzchar(item$description %||% "")) {
      lines <- c(lines, dina_cli_dim(sprintf("      %s", item$description)))
    }
  }
  lines <- c(lines, "", dina_cli_dim(sprintf("  Keys: %s", dina_menu_control_help(items))))
  if (isTRUE(help)) {
    lines <- c(lines, "", dina_cli_dim("  Help:"), dina_menu_help_lines(items, selected))
    if (is.null(selected)) {
      lines <- c(lines, dina_cli_dim("  Press q to leave this menu without choosing a new action."))
    }
  }
  lines
}

dina_menu_default_is_quit <- function(default = NULL, allow_quit = TRUE) {
  isTRUE(allow_quit) &&
    length(default %||% character()) == 1L &&
    tolower(as.character(default)) %in% c("q", "quit")
}

dina_menu_select_numbered <- function(title, items, prompt = "Choose an action", default = NULL, allow_quit = TRUE, input = "stdin", is_terminal = isatty(stdin()), context = character()) {
  visible <- dina_menu_visible_indices(items)
  default_is_quit <- dina_menu_default_is_quit(default, allow_quit)
  selected_default <- dina_menu_index_for_value(items, default)
  if (!is.na(selected_default) && isTRUE(items[[selected_default]]$hidden)) {
    selected_default <- NA_integer_
  }
  if (is.na(selected_default) && !default_is_quit) {
    selected_default <- dina_menu_first_enabled(items)
  }
  repeat {
    for (line in dina_menu_lines(title, items, selected = NULL, prompt = prompt, context = context)) {
      dina_cli_cat(line)
    }
    default_visible <- match(selected_default, visible)
    default_label <- if (!is.na(default_visible)) as.character(default_visible) else if (default_is_quit || isTRUE(allow_quit)) "q" else ""
    answer <- tolower(trimws(dina_cli_prompt_value(sprintf("Selection [%s]: ", default_label), default = default_label, input = input, is_terminal = is_terminal)))
    if (answer %in% c("?", "help")) {
      dina_cli_cat(dina_menu_control_help(items))
      for (line in dina_menu_help_lines(items, selected = NULL)) {
        dina_cli_cat(line)
      }
      next
    }
    if (isTRUE(allow_quit) && answer %in% c("q", "quit", "exit")) {
      return("quit")
    }
    if (answer %in% c("p", "prev", "previous", "left", "l", "back")) {
      idx <- dina_menu_back_index(items)
      if (!is.na(idx)) {
        return(dina_menu_action_value(items[[idx]]))
      }
      next
    }
    if (answer %in% c("n", "next", "right", "r", "skip")) {
      if (!is.na(selected_default) && !is.null(items[[selected_default]]$right)) {
        return(items[[selected_default]]$right)
      }
      idx <- dina_menu_next_index(items)
      if (!is.na(idx)) {
        return(dina_menu_action_value(items[[idx]]))
      }
      next
    }
    choice <- suppressWarnings(as.integer(answer))
    if (is.na(choice) || choice < 1L || choice > length(visible)) {
      dina_cli_warn(sprintf("Unknown selection `%s`.", answer))
      next
    }
    item_index <- visible[[choice]]
    if (isTRUE(items[[item_index]]$disabled)) {
      dina_cli_warn(sprintf("Unavailable action: %s", items[[item_index]]$label))
      next
    }
    return(dina_menu_action_value(items[[item_index]]))
  }
}

dina_menu_select_raw <- function(title, items, prompt = "Choose an action", default = NULL, allow_quit = TRUE, context = character()) {
  state <- dina_menu_tty_state()
  if (!length(state)) {
    return(NULL)
  }
  con <- dina_menu_open_tty_binary()
  if (is.null(con)) {
    return(NULL)
  }
  on.exit(close(con), add = TRUE)
  dina_menu_set_raw()
  on.exit({
    dina_menu_restore_tty(state)
    cat("\n")
  }, add = TRUE)
  default_is_quit <- dina_menu_default_is_quit(default, allow_quit)
  selected <- if (isTRUE(default_is_quit)) NA_integer_ else dina_menu_index_for_value(items, default)
  enabled <- dina_menu_enabled_indices(items)
  if (!length(enabled)) {
    return(if (isTRUE(allow_quit)) "quit" else NULL)
  }
  if (is.na(selected)) {
    selected <- if (isTRUE(default_is_quit)) NA_integer_ else enabled[[1]]
  }
  show_help <- FALSE
  repeat {
    screen <- paste(
      dina_menu_lines(title, items, selected = if (is.na(selected)) NULL else selected, prompt = prompt, help = show_help, context = context),
      collapse = "\r\n"
    )
    cat("\033[2J\033[H", screen, "\r\n", sep = "")
    flush.console()
    key <- dina_menu_read_key(con)
    show_help <- FALSE
    if (identical(key, "\033[A")) {
      if (is.na(selected)) {
        selected <- enabled[[length(enabled)]]
      } else {
        current <- match(selected, enabled)
        selected <- enabled[[if (current <= 1L) length(enabled) else current - 1L]]
      }
    } else if (identical(key, "\033[B")) {
      if (is.na(selected)) {
        selected <- enabled[[1]]
      } else {
        current <- match(selected, enabled)
        selected <- enabled[[if (current >= length(enabled)) 1L else current + 1L]]
      }
    } else if (identical(key, "\033[D")) {
      idx <- dina_menu_back_index(items)
      if (!is.na(idx)) return(dina_menu_action_value(items[[idx]]))
    } else if (identical(key, "\033[C")) {
      if (!is.na(selected) && !is.null(items[[selected]]$right)) return(items[[selected]]$right)
      idx <- dina_menu_next_index(items)
      if (!is.na(idx)) return(dina_menu_action_value(items[[idx]]))
    } else if (identical(key, "\r") || identical(key, "\n")) {
      if (is.na(selected)) {
        if (isTRUE(default_is_quit) && isTRUE(allow_quit)) return("quit")
        next
      }
      return(dina_menu_action_value(items[[selected]]))
    } else if (isTRUE(allow_quit) && tolower(key) %in% c("q")) {
      return("quit")
    } else if (identical(key, "?")) {
      show_help <- TRUE
    } else if (grepl("^[0-9]$", key)) {
      choice <- as.integer(key)
      visible <- dina_menu_visible_indices(items)
      if (!is.na(choice) && choice >= 1L && choice <= length(visible)) {
        item_index <- visible[[choice]]
        if (!isTRUE(items[[item_index]]$disabled)) {
          return(dina_menu_action_value(items[[item_index]]))
        }
      }
    }
  }
}

dina_menu_select <- function(title, items, prompt = "Choose an action", default = NULL, allow_quit = TRUE, input = "stdin", is_terminal = isatty(stdin()), context = character()) {
  items <- dina_menu_normalize(items)
  if (!length(items)) {
    return(NULL)
  }
  if (!isTRUE(is_terminal)) {
    idx <- dina_menu_index_for_value(items, default)
    if (is.na(idx)) return(NULL)
    return(dina_menu_action_value(items[[idx]]))
  }
  if (dina_menu_terminal_available(input, is_terminal)) {
    raw <- dina_menu_select_raw(title, items, prompt = prompt, default = default, allow_quit = allow_quit, context = context)
    if (!is.null(raw)) {
      return(raw)
    }
  }
  dina_menu_select_numbered(title, items, prompt = prompt, default = default, allow_quit = allow_quit, input = input, is_terminal = is_terminal, context = context)
}

dina_menu_confirm <- function(title = "Confirm", prompt = "Continue?", default = FALSE, input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(isTRUE(default))
  }
  result <- dina_menu_select(
    title,
    list(
      dina_menu_action("yes", "Yes", value = TRUE),
      dina_menu_action("no", "No", value = FALSE)
    ),
    prompt = prompt,
    default = if (isTRUE(default)) TRUE else FALSE,
    allow_quit = TRUE,
    input = input,
    is_terminal = is_terminal
  )
  if (identical(result, "quit") || is.null(result)) {
    return(FALSE)
  }
  isTRUE(result)
}

dina_menu_text <- function(title = "Input", prompt, default = "", input = "stdin", is_terminal = isatty(stdin()), allow_quit = TRUE) {
  if (!isTRUE(is_terminal)) {
    return(list(value = default, quit = FALSE))
  }
  dina_cli_cat("")
  dina_cli_cat(title)
  suffix <- if (nzchar(default %||% "")) sprintf(" [%s]", default) else ""
  answer <- trimws(dina_read_prompt(sprintf("%s%s: ", prompt, suffix), input = input))
  if (isTRUE(allow_quit) && tolower(answer) %in% c("q", "quit", "exit")) {
    return(list(value = default, quit = TRUE))
  }
  list(value = if (nzchar(answer)) answer else default, quit = FALSE)
}

dina_default_editor <- function() {
  visual <- Sys.getenv("VISUAL", unset = "")
  if (nzchar(visual)) {
    return(visual)
  }
  editor <- Sys.getenv("EDITOR", unset = "")
  if (nzchar(editor)) {
    return(editor)
  }
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    return("open -t")
  }
  "vi"
}

dina_open_editor <- function(path, editor = dina_default_editor()) {
  editor <- trimws(editor %||% "")
  if (!nzchar(editor)) {
    editor <- dina_default_editor()
  }
  parts <- strsplit(editor, "[[:space:]]+")[[1]]
  command <- parts[[1]]
  args <- c(parts[-1], path)
  status <- system2(command, args)
  if (is.null(status)) {
    status <- 0L
  }
  as.integer(status)
}

dina_prepare_session_config_yaml <- function(session, root = dina_repo_root()) {
  if (is.null(session)) {
    stop("No active update.", call. = FALSE)
  }
  override_path <- dina_session_config_override_path(session$id, root)
  created <- !file.exists(override_path)
  if (isTRUE(created)) {
    dina_write_suggested_session_config_override(session, root = root, overwrite = TRUE)
    session$config_override <- dina_relative(override_path, root)
    session$config_override_hash <- dina_hash_file(override_path)
    session$updated_at <- dina_now()
    dina_save_session(session, root)
  }
  list(session = session, path = override_path, created = created)
}

dina_update_config_edit <- function(
    session,
    root = dina_repo_root(),
    editor = dina_default_editor(),
    open_editor = isatty(stdin()) || nzchar(Sys.getenv("EDITOR", unset = "")) || nzchar(Sys.getenv("VISUAL", unset = ""))) {
  prepared <- dina_prepare_session_config_yaml(session, root)
  session <- prepared$session
  path <- prepared$path
  dina_cli_header("Update Config Edit")
  dina_cli_alert(sprintf("Update YAML: %s", dina_relative(path, root)))
  if (isTRUE(prepared$created)) {
    dina_cli_alert("Created from annual update suggestions. The benchmark config is untouched.")
  }
  if (isTRUE(open_editor)) {
    status <- dina_open_editor(path, editor = editor)
    if (!identical(status, 0L)) {
      dina_cli_warn(sprintf("Editor exited with status %s.", status))
    }
  } else {
    dina_cli_alert("Run from an interactive terminal or set EDITOR to open the file automatically.")
  }
  session$config_override <- dina_relative(path, root)
  session$config_override_hash <- dina_hash_file(path)
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  dina_cli_ok(sprintf("Updated working override: %s", dina_relative(path, root)))
  dina_cli_alert("Review with `dina update config show`.")
  invisible(dina_load_session(root = root))
}

dina_print_repo_status <- function(comparison) {
  dina_cli_header("Update Repo Status")
  metadata <- comparison$metadata
  dina_cli_alert(sprintf("Baseline: %s (%s)", metadata$baseline %||% "", metadata$created_at %||% ""))
  dina_cli_alert(sprintf("Branch at baseline: %s", metadata$branch %||% "unknown"))
  dina_cli_alert(sprintf("HEAD at baseline: %s", metadata$head %||% "unknown"))
  counts <- comparison$counts
  dina_cli_cat(dina_cli_key_value(
    "Current vs baseline:",
    sprintf(
      "added=%s modified=%s deleted=%s unchanged=%s",
      counts[["added"]], counts[["modified"]], counts[["deleted"]], counts[["unchanged"]]
    )
  ))
  changed <- comparison$rows[comparison$rows$state != "unchanged", , drop = FALSE]
  if (!nrow(changed)) {
    dina_cli_ok("No captured repo files differ from the baseline.")
    return(invisible(comparison))
  }
  dina_cli_cat(sprintf("%-10s %s", "state", "path"))
  for (i in seq_len(nrow(changed))) {
    dina_cli_cat(dina_cli_row(
      list(changed$state[[i]], changed$path[[i]]),
      widths = c(10L, NA),
      dim = c(TRUE, FALSE)
    ))
  }
  invisible(comparison)
}

dina_repo_state_missing_result <- function(session, root = dina_repo_root(), baseline = "start", title = "Update Repo Status") {
  baselines <- dina_repo_state_baselines(session, root)
  dina_cli_header(title)
  dina_cli_warn(sprintf("No captured repo baseline named `%s` for update %s.", baseline, session$id %||% ""))
  if (length(baselines)) {
    dina_cli_alert(sprintf("Available repo baselines: %s", paste(baselines, collapse = ", ")))
    dina_cli_alert(sprintf("Try `dina update repo-status --baseline %s` or `dina update repo-diff --baseline %s`.", baselines[[1]], baselines[[1]]))
  } else {
    dina_cli_alert("Available repo baselines: none.")
    dina_cli_alert("Repo status and repo diff need a repo-state baseline captured by update start or restart.")
    dina_cli_alert("To create one for this session, review whether `dina update restart --replace-repo-baseline` is appropriate.")
  }
  invisible(list(missing = TRUE, baseline = baseline, baselines = baselines))
}

dina_repo_state_compare_for_cli <- function(session, root = dina_repo_root(), baseline = "start", title = "Update Repo Status") {
  if (is.null(dina_repo_state_metadata(session, root, baseline))) {
    return(dina_repo_state_missing_result(session, root = root, baseline = baseline, title = title))
  }
  dina_repo_state_compare(session, root, baseline)
}

dina_print_repo_diff <- function(session, root = dina_repo_root(), baseline = "start", stat = TRUE, patch = FALSE, files = FALSE, comparison = NULL) {
  comparison <- comparison %||% dina_repo_state_compare(session, root, baseline)
  if (isTRUE(stat)) {
    dina_print_repo_status(comparison)
  }
  changed <- comparison$rows[comparison$rows$state != "unchanged", , drop = FALSE]
  if (isTRUE(files)) {
    dina_cli_cat("")
    dina_cli_cat("Files:")
    if (!nrow(changed)) dina_cli_cat("  none")
    for (i in seq_len(nrow(changed))) {
      dina_cli_cat(sprintf(
        "  %s %s",
        dina_cli_dim(changed$state[[i]]),
        changed$path[[i]]
      ))
    }
  }
  if (isTRUE(patch)) {
    dina_cli_cat("")
    dina_cli_cat("Patch:")
    if (!nrow(changed)) {
      dina_cli_cat("  no patch")
    }
    for (i in seq_len(nrow(changed))) {
      rel <- changed$path[[i]]
      state <- changed$state[[i]]
      from <- file.path(dina_repo_state_files_dir(session, root, baseline), rel)
      to <- file.path(root, rel)
      dina_cli_cat(sprintf("diff --dina %s (%s)", rel, state))
      if (identical(state, "added")) {
        dina_cli_cat(sprintf("  added file not present in baseline: %s", rel))
      } else {
        target <- if (identical(state, "deleted")) "/dev/null" else to
        diff <- suppressWarnings(system2("diff", c("-u", from, target), stdout = TRUE, stderr = TRUE))
        if (!length(diff)) {
          dina_cli_cat("  binary or unchanged diff")
        } else {
          cat(paste(diff, collapse = "\n"), "\n", sep = "")
        }
      }
    }
  }
  invisible(comparison)
}

dina_print_repo_restore <- function(result) {
  title <- if (isTRUE(result$dry_run)) "Update Repo Restore Preview" else "Update Repo Restore"
  dina_cli_header(title)
  actions <- result$actions
  if (!length(actions)) {
    dina_cli_ok("No modified or deleted captured files need restore.")
  } else {
    for (action in actions) {
      dina_cli_cat(sprintf("%-14s %s", action$status %||% "", action$path %||% ""))
    }
  }
  if (length(result$added_not_removed %||% character())) {
    dina_cli_warn("Added files are reported but not removed automatically:")
    for (path in result$added_not_removed) {
      dina_cli_cat(sprintf("  %s", path))
    }
  }
  if (isTRUE(result$dry_run)) {
    dina_cli_alert("Preview only. Pass --yes to restore captured modified/deleted files.")
  }
  invisible(result)
}

dina_command_entry <- function(
    key,
    label,
    description = "",
    args = NULL,
    children = list(),
    prompts = list(),
    defaults = list(),
    mutating = FALSE,
    confirm = FALSE,
    help = "",
    next_step = "") {
  list(
    key = key,
    label = label,
    description = description,
    args = args,
    children = children,
    prompts = prompts,
    defaults = defaults,
    mutating = isTRUE(mutating),
    confirm = isTRUE(confirm),
    help = help,
    next_step = next_step
  )
}

dina_command_catalog <- function(year = format(Sys.Date(), "%Y")) {
  list(
    dina_command_entry(
      "annual-update",
      "Annual update",
      "Start, inspect, close, and maintain update workspaces.",
      children = list(
        dina_command_entry("workflow", "Workflow guide", "Read the update workflow guide.", args = c("help", "workflow"), next_step = "Use sources, run, and todo commands as needed."),
        dina_command_entry("update-start", "Start update", "Create the active update workspace.", args = c("update", "start", "{YEAR}"), defaults = list(YEAR = year), mutating = TRUE, help = "Creates output/updates/<update_id>, source baseline, todo state, inbox buckets, and the active pointer.", next_step = "Then run `dina sources list` or `dina todo`."),
        dina_command_entry("update-status", "Update status", "Inspect the active update state and recommendation.", args = c("update", "status"), next_step = "Run the recommended command or browse this menu."),
        dina_command_entry("todo", "Todo", "Show the non-blocking helper checklist.", args = c("todo")),
        dina_command_entry("update-list", "List updates", "List update sessions and the active pointer.", args = c("update", "list")),
        dina_command_entry("close", "Close update", "Generate closure report for sources, runs, todo, config, and repo changes.", args = c("update", "close"), mutating = TRUE)
      )
    ),
    dina_command_entry(
      "source-data",
      "Source data",
      "Inspect, fetch, and compare source data.",
      children = list(
        dina_command_entry("sources-list", "Source registry", "List compact source rows.", args = c("sources", "list")),
        dina_command_entry("sources-list-detail", "Source detail", "Inspect one source registry entry.", args = c("sources", "list", "detail", "{ID}"), prompts = list(ID = list(label = "Source id", example = "chl-pit-total")), help = "Use `dina sources list` first if you do not know the id."),
        dina_command_entry("sources-list-guide", "Source guide", "Show where sources come from and where they land.", args = c("sources", "list", "guide")),
        dina_command_entry("sources-fetch-dry-run", "Preview fetch", "Preview supported source fetches into _new buckets.", args = c("sources", "fetch", "--dry-run")),
        dina_command_entry("sources-fetch-source", "Preview source fetch", "Preview one supported source fetch.", args = c("sources", "fetch", "{ID}", "--dry-run"), prompts = list(ID = list(label = "Source id", example = "chl-pit-total"))),
        dina_command_entry("sources-explore-type", "Explore source type", "Inspect new files, likely years, structure evidence, and include expectations.", args = c("sources", "explore", "{SOURCETYPE}"), defaults = list(SOURCETYPE = "sna"), prompts = list(SOURCETYPE = list(label = "Source type", example = "sna or admin")), help = "Currently implemented for sna and PIT admin; writes experiment outputs under output/experiments/."),
        dina_command_entry("sources-include-type", "Include source type dry-run", "Stage incoming sources and run deterministic inclusion checks.", args = c("sources", "include", "{SOURCETYPE}", "--dry-run"), defaults = list(SOURCETYPE = "sna"), prompts = list(SOURCETYPE = list(label = "Source type", example = "sna or admin")), help = "Currently implemented for sna and PIT admin. No production files are changed."),
        dina_command_entry("sources-include-type-confirm", "Confirm source type", "Promote sources from a clean staged include run after writing backups.", args = c("sources", "include", "{SOURCETYPE}", "--confirm", "--include-run", "{RUN}"), defaults = list(SOURCETYPE = "sna"), prompts = list(SOURCETYPE = list(label = "Source type", example = "sna or admin"), RUN = list(label = "Include run", example = "output/experiments/*/runs/RUN")), mutating = TRUE, confirm = TRUE, help = "Currently implemented for sna and PIT admin. Does not run the pipeline. Use only after reviewing a clean include dry-run."),
        dina_command_entry("sources-include-type-restore", "Restore source type", "Restore canonical sources from a confirm backup snapshot.", args = c("sources", "include", "{SOURCETYPE}", "--restore", "{CONFIRM_RUN}"), defaults = list(SOURCETYPE = "sna"), prompts = list(SOURCETYPE = list(label = "Source type", example = "sna"), CONFIRM_RUN = list(label = "Confirm run", example = "output/experiments/country_sna_include/confirms/confirm-YYYYMMDD-HHMMSS")), mutating = TRUE, confirm = TRUE),
        dina_command_entry("sources-table-type", "Source type table preview", "Preview explore output tables inline.", args = c("sources", "table", "{SOURCETYPE}", "year_expectations"), defaults = list(SOURCETYPE = "sna"), prompts = list(SOURCETYPE = list(label = "Source type", example = "sna or admin")), help = "Currently implemented for sna and PIT admin. Use --run PATH, --country ISO, and --limit N for a narrower preview."),
        dina_command_entry(
          "source-registry-diagnostics",
          "Source diagnostics",
          "Inspect registry rows, methods, scans, baseline comparisons, diffs, and fetch previews.",
          children = list(
            dina_command_entry("sources-workflow", "Workflow view", "List acquisition, destinations, transformers, and task usage.", args = c("sources", "list", "workflow")),
            dina_command_entry("sources-paths", "Paths view", "List canonical, inbox, destination, and fetch target paths.", args = c("sources", "list", "paths")),
            dina_command_entry("sources-urls", "URLs view", "List source URL hints or expand URLs.", args = c("sources", "list", "urls")),
            dina_command_entry("sources-fields", "Source fields", "Explain source registry fields and views.", args = c("sources", "fields")),
            dina_command_entry("sources-methods", "Source methods", "Explain acquisition method labels.", args = c("sources", "methods")),
            dina_command_entry("sources-compare", "Source compare", "Compare configured source files with the active update baseline.", args = c("sources", "compare")),
            dina_command_entry("sources-scan", "Source scan", "Detect local source coverage from the registry.", args = c("sources", "scan")),
            dina_command_entry("sources-diff", "Source diff", "Compare current source scan with the active session baseline.", args = c("sources", "diff"))
          )
        )
      )
    ),
    dina_command_entry(
      "pipeline",
      "Pipeline",
      "Inspect task freshness and run pipeline tasks.",
      children = list(
        dina_command_entry("run-list", "List tasks", "Inspect task aliases, stages, language, and status.", args = c("run", "list")),
        dina_command_entry("run-why", "Why task", "Explain why one task is current, stale, blocked, or failed.", args = c("run", "why", "{TASK}"), prompts = list(TASK = list(label = "Task selector", example = "07d")), help = "Selectors include short ids like 07d and full task ids."),
        dina_command_entry("run-stale", "Run stale", "Run all stale, missing, never-run, or failed active tasks.", args = c("run", "stale", "--dry-run"), help = "Remove --dry-run after reviewing the preview."),
        dina_command_entry("run-dry", "Preview run", "Preview selected scripts without executing.", args = c("run", "{TASK}", "--dry-run"), prompts = list(TASK = list(label = "Task selector", example = "01a"))),
        dina_command_entry("run-task", "Run task", "Run selected scripts and write run logs.", args = c("run", "{TASK}"), prompts = list(TASK = list(label = "Task selector", example = "01a")), mutating = TRUE, help = "This can update data/output files.")
      )
    ),
    dina_command_entry(
      "setup-config",
      "Setup and config",
      "Check readiness, inspect config, and manage local setup.",
      children = list(
        dina_command_entry("doctor", "Run doctor", "Check local readiness without changing files.", args = c("doctor")),
        dina_command_entry("install-dry", "Preview package install", "Report missing R packages without installing.", args = c("install", "--dry-run")),
        dina_command_entry("config-show", "Show config", "Print config/dina.yml.", args = c("config", "show")),
        dina_command_entry("config-check", "Check config", "Report required config keys and legacy runtime file status.", args = c("config", "check")),
        dina_command_entry("update-config-show", "Show update config", "Show benchmark, working override, and effective active-update config.", args = c("update", "config", "show")),
        dina_command_entry("update-config-edit", "Edit update config", "Open the active update working override for manual editing.", args = c("update", "config", "edit"), mutating = TRUE),
        dina_command_entry("compress-input-preview", "Preview input zip", "Preview a zip bundle of input_data without heavy admin microdata.", args = c("compress", "input", "--dry-run")),
        dina_command_entry("compress-input-dropbox", "Dropbox input zip", "Zip Dropbox mirror input_data under ~/Dropbox/DINA-LatAm/output/archives.", args = c("compress", "input", "--dropbox"), mutating = TRUE),
        dina_command_entry("notify-init", "Initialize notifications", "Create local Pushover placeholder config.", args = c("notify", "init"), mutating = TRUE),
        dina_command_entry("notify-test", "Test notification", "Send a Pushover test message.", args = c("notify", "test"), mutating = TRUE),
        dina_command_entry("setup-command", "Install command wrapper", "Install the user-level dina wrapper.", args = c("setup", "command"), mutating = TRUE)
      )
    ),
    dina_command_entry(
      "maintenance",
      "Maintenance",
      "Repo baseline tools, path audit, and task graph helpers.",
      children = list(
        dina_command_entry("maintain-repo-status", "Repo status", "Compare current code/config/docs with the update baseline.", args = c("maintain", "repo-status")),
        dina_command_entry("maintain-repo-diff", "Repo diff", "Show changed files against the update baseline.", args = c("maintain", "repo-diff", "--stat", "--files")),
        dina_command_entry("maintain-repo-restore", "Repo restore", "Restore captured modified/deleted files from a baseline.", args = c("maintain", "repo-restore", "--dry-run"), help = "Add --yes after reviewing the preview.", mutating = TRUE),
        dina_command_entry("audit-paths", "Audit paths", "Report likely hardcoded paths.", args = c("audit", "paths")),
        dina_command_entry("make-export", "Export Makefile", "Write a Makefile view of the task graph.", args = c("make", "export", "{PATH}"), prompts = list(PATH = list(label = "Makefile path", example = "Makefile.dina")), mutating = TRUE)
      )
    )
  )
}

dina_command_flatten <- function(entries) {
  out <- list()
  for (entry in entries) {
    out[[length(out) + 1L]] <- entry
    children <- entry$children %||% list()
    if (length(children)) {
      out <- c(out, dina_command_flatten(children))
    }
  }
  out
}

dina_command_placeholder <- function(arg) {
  if (grepl("^\\{[A-Za-z0-9_]+\\}$", arg)) {
    return(sub("^\\{(.+)\\}$", "\\1", arg))
  }
  NULL
}

dina_command_quote_arg <- function(arg) {
  arg <- as.character(arg)
  if (grepl("[[:space:]]", arg)) shQuote(arg) else arg
}

dina_command_format <- function(args) {
  paste(c("dina", vapply(args, dina_command_quote_arg, character(1))), collapse = " ")
}

dina_command_template_args <- function(entry) {
  args <- entry$args %||% character()
  vapply(args, function(arg) {
    name <- dina_command_placeholder(arg)
    if (is.null(name)) {
      return(arg)
    }
    default <- entry$defaults[[name]] %||% ""
    if (nzchar(default)) default else sprintf("<%s>", name)
  }, character(1))
}

dina_command_template <- function(entry) {
  dina_command_format(dina_command_template_args(entry))
}

dina_command_find_by_command <- function(command, catalog = dina_command_catalog()) {
  command <- trimws(command %||% "")
  for (entry in dina_command_flatten(catalog)) {
    if (!length(entry$args %||% character())) {
      next
    }
    if (identical(dina_command_template(entry), command)) {
      return(entry)
    }
  }
  NULL
}

dina_command_prompt_for <- function(entry, name) {
  prompt <- entry$prompts[[name]] %||% list()
  label <- prompt$label %||% tolower(name)
  example <- prompt$example %||% ""
  if (nzchar(example)) {
    sprintf("%s (example: %s)", label, example)
  } else {
    label
  }
}

dina_command_collect_args <- function(entry, input = "stdin", is_terminal = isatty(stdin())) {
  args <- character()
  prompted <- FALSE
  for (arg in entry$args %||% character()) {
    name <- dina_command_placeholder(arg)
    if (is.null(name)) {
      args <- c(args, arg)
      next
    }
    default <- entry$defaults[[name]] %||% entry$prompts[[name]]$default %||% ""
    if (nzchar(default)) {
      args <- c(args, default)
      next
    }
    result <- dina_menu_text(
      title = "Complete Command",
      prompt = sprintf("%s (`.` back, `q` quit)", dina_command_prompt_for(entry, name)),
      default = default,
      input = input,
      is_terminal = is_terminal,
      allow_quit = TRUE
    )
    if (isTRUE(result$quit)) {
      return(list(status = "quit"))
    }
    value <- trimws(result$value %||% "")
    if (tolower(value) %in% c(".", "cancel")) {
      return(list(status = "back"))
    }
    if (!nzchar(value)) {
      dina_cli_warn(sprintf("Missing value for %s.", name))
      return(list(status = "back"))
    }
    prompted <- TRUE
    args <- c(args, value)
  }
  list(status = "ok", args = args, command = dina_command_format(args), prompted = prompted)
}

dina_command_confirm_run <- function(entry, resolved, input = "stdin", is_terminal = isatty(stdin())) {
  needs_confirmation <- isTRUE(entry$mutating) || isTRUE(entry$confirm) || isTRUE(resolved$prompted)
  if (!needs_confirmation) {
    return(TRUE)
  }
  dina_cli_cat("")
  dina_cli_cat("Proposed command:")
  dina_cli_cat(sprintf("  %s", resolved$command))
  if (nzchar(entry$description %||% "")) {
    dina_cli_alert(sprintf("What it does: %s", entry$description))
  }
  if (nzchar(entry$next_step %||% "")) {
    dina_cli_alert(sprintf("Next step: %s", entry$next_step))
  }
  dina_menu_confirm(
    title = "Run Command",
    prompt = "Run this command?",
    default = !isTRUE(entry$mutating),
    input = input,
    is_terminal = is_terminal
  )
}

dina_command_run_entry <- function(entry, root = dina_repo_root(), input = "stdin", is_terminal = isatty(stdin())) {
  resolved <- dina_command_collect_args(entry, input = input, is_terminal = is_terminal)
  if (is.null(resolved) || !identical(resolved$status, "ok")) {
    if (identical(resolved$status, "quit")) {
      dina_cli_alert("Command completion quit. No command run.")
      return(invisible(list(status = "quit")))
    }
    dina_cli_alert("No command run.")
    return(invisible(list(status = "back")))
  }
  if (!dina_command_confirm_run(entry, resolved, input = input, is_terminal = is_terminal)) {
    dina_cli_alert("No command run.")
    return(invisible(list(status = "back")))
  }
  dina_cli_alert(sprintf("Running `%s`", resolved$command))
  result <- dina_main(resolved$args, root = root)
  invisible(list(status = "ran", result = result))
}

dina_command_entry_from_proposal <- function(proposal, catalog = dina_command_catalog()) {
  command <- trimws(proposal$command %||% "")
  entry <- dina_command_find_by_command(command, catalog)
  if (is.null(entry)) {
    parts <- strsplit(command, "[[:space:]]+")[[1]]
    if (length(parts) && identical(parts[[1]], "dina")) {
      parts <- parts[-1]
    }
    entry <- dina_command_entry(
      "recommended-action",
      "Recommended action",
      proposal$comment %||% "Run the recommended DINA command.",
      args = parts,
      confirm = TRUE,
      help = "This command came from the active update recommendation. Review the preview before running.",
      next_step = proposal$next_step %||% ""
    )
  } else {
    entry$key <- "recommended-action"
    entry$label <- "Recommended action"
    entry$description <- proposal$comment %||% entry$description %||% ""
    entry$help <- entry$help %||% proposal$comment %||% ""
    entry$next_step <- proposal$next_step %||% entry$next_step %||% ""
  }
  entry
}

dina_command_browser_entries <- function(proposal = NULL, catalog = dina_command_catalog()) {
  entries <- catalog
  if (!is.null(proposal)) {
    entries <- c(list(dina_command_entry_from_proposal(proposal, catalog)), entries)
  }
  entries
}

dina_command_browser <- function(root = dina_repo_root(), proposal = NULL, input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(invisible(NULL))
  }
  stack <- list(list(title = "DINA Commands", entries = dina_command_browser_entries(proposal)))
  repeat {
    frame <- stack[[length(stack)]]
    entries <- frame$entries
    actions <- lapply(seq_along(entries), function(i) {
      entry <- entries[[i]]
      value <- list(type = "entry", entry = entry)
      open_value <- list(type = "open", entry = entry)
      children <- entry$children %||% list()
      has_children <- length(children) > 0L
      has_command <- length(entry$args %||% character()) > 0L
      dina_menu_action(
        key = entry$key %||% sprintf("entry-%s", i),
        label = entry$label,
        value = value,
        description = if (has_children) {
          sprintf("%s >", entry$description %||% "")
        } else {
          entry$description %||% ""
        },
        command = if (has_command) dina_command_template(entry) else "",
        help = entry$help %||% entry$description %||% "",
        right = if (has_children) open_value else NULL
      )
    })
    if (length(stack) > 1L) {
      actions[[length(actions) + 1L]] <- dina_menu_action("back", "Back", value = "back", description = "Return to the previous menu.")
    }
    selected <- dina_menu_select(
      title = frame$title,
      items = actions,
      prompt = "Browse command groups. Type a number, then press Enter to open or run. Type ? for more detail.",
      default = NULL,
      allow_quit = TRUE,
      input = input,
      is_terminal = is_terminal
    )
    if (is.null(selected) || identical(selected, "quit")) {
      return(invisible(NULL))
    }
    if (identical(selected, "back")) {
      if (length(stack) > 1L) {
        stack <- stack[-length(stack)]
      }
      next
    }
    entry <- selected$entry
    selected_type <- selected$type %||% "entry"
    if (identical(selected_type, "open") || (!length(entry$args %||% character()) && length(entry$children %||% list()))) {
      stack[[length(stack) + 1L]] <- list(title = entry$label, entries = entry$children)
      next
    }
    result <- dina_command_run_entry(entry, root = root, input = input, is_terminal = is_terminal)
    if (is.list(result) && identical(result$status, "back")) {
      next
    }
    return(invisible(result))
  }
}

dina_dashboard_expand_recommended_command <- function(command) {
  gsub(
    "dina update start YEAR",
    sprintf("dina update start %s", format(Sys.Date(), "%Y")),
    command %||% "",
    fixed = TRUE
  )
}

dina_dashboard_recommended_command <- function(recommendation) {
  recommendation <- recommendation %||% ""
  matches <- gregexpr("`[^`]+`", recommendation, perl = TRUE)[[1]]
  candidates <- character()
  if (matches[[1]] > 0L) {
    candidates <- regmatches(recommendation, list(matches))[[1]]
    candidates <- gsub("^`|`$", "", candidates)
  }
  commands <- candidates[grepl("^dina(\\s|$)", candidates)]
  if (!length(commands) && grepl("dina update start YEAR", recommendation, fixed = TRUE)) {
    commands <- "dina update start YEAR"
  }
  if (!length(commands)) {
    return(NULL)
  }
  dina_dashboard_expand_recommended_command(commands[[1]])
}

dina_dashboard_proposal <- function(recommendation) {
  if (is.list(recommendation)) {
    command <- dina_dashboard_expand_recommended_command(recommendation$command %||% "")
    if (!nzchar(command)) {
      command <- "dina help workflow"
    }
    entry <- dina_command_find_by_command(command)
    next_command <- dina_dashboard_expand_recommended_command(recommendation$next_command %||% "")
    expected_action <- recommendation$expected_action %||% recommendation$comment %||% entry$description %||% "Open the guided workflow for the next update step."
    next_note <- recommendation$next_note %||% recommendation$next_step %||% entry$next_step %||% ""
    return(list(
      command = command,
      why = recommendation$why %||% "",
      todo_id = recommendation$todo_id %||% "",
      todo_label = recommendation$todo_label %||% "",
      expected_action = expected_action,
      next_command = next_command,
      next_note = next_note,
      comment = expected_action,
      next_step = if (nzchar(next_command)) next_command else next_note
    ))
  }
  command <- dina_dashboard_recommended_command(recommendation)
  if (is.null(command)) {
    command <- "dina help workflow"
  }
  entry <- dina_command_find_by_command(command)
  list(
    command = command,
    comment = entry$description %||% "Open the guided workflow for the next update step.",
    next_step = entry$next_step %||% "Use `dina commands` to choose a related command."
  )
}

dina_state_proposal <- function(state) {
  dina_dashboard_proposal(state$proposal %||% state$recommendation %||% "")
}

dina_recommendation_todo_text <- function(proposal) {
  todo_id <- proposal$todo_id %||% ""
  todo_label <- proposal$todo_label %||% ""
  if (nzchar(todo_id) && nzchar(todo_label)) {
    return(sprintf("%s - %s", todo_id, todo_label))
  }
  if (nzchar(todo_id)) {
    return(todo_id)
  }
  todo_label
}

dina_recommendation_lines <- function(proposal, title = "Recommended") {
  lines <- c(sprintf("%s:", title), sprintf("  %s", dina_cli_command(proposal$command %||% "")))
  if (nzchar(proposal$why %||% "")) {
    lines <- c(lines, "Why:", dina_cli_dim(sprintf("  %s", proposal$why)))
  }
  todo <- dina_recommendation_todo_text(proposal)
  if (nzchar(todo)) {
    lines <- c(lines, "Todo:", dina_cli_dim(sprintf("  %s", todo)))
  }
  if (nzchar(proposal$expected_action %||% "")) {
    lines <- c(lines, "Expected action:", dina_cli_dim(sprintf("  %s", proposal$expected_action)))
  }
  if (nzchar(proposal$next_command %||% "")) {
    lines <- c(lines, "Next likely command:", sprintf("  %s", dina_cli_command(proposal$next_command)))
    if (nzchar(proposal$next_note %||% "")) {
      lines <- c(lines, dina_cli_dim(sprintf("  %s", proposal$next_note)))
    }
  } else if (nzchar(proposal$next_note %||% "")) {
    lines <- c(lines, "Next:", dina_cli_dim(sprintf("  %s", proposal$next_note)))
  }
  lines
}

dina_print_recommendation <- function(proposal, title = "Recommended") {
  for (line in dina_recommendation_lines(proposal, title = title)) {
    dina_cli_cat(line)
  }
  invisible(proposal)
}

dina_command_catalog_entry_lines <- function(entries, indent = 4L) {
  lines <- character()
  prefix <- strrep(" ", indent)
  for (entry in entries) {
    children <- entry$children %||% list()
    has_command <- length(entry$args %||% character()) > 0L
    if (has_command) {
      lines <- c(lines, sprintf("%s- %s", prefix, entry$label))
      lines <- c(lines, sprintf("%s  %s", prefix, dina_cli_command(sprintf("$ %s", dina_command_template(entry)))))
    } else {
      lines <- c(lines, sprintf("%s- %s:", prefix, entry$label))
    }
    if (length(children)) {
      lines <- c(lines, dina_command_catalog_entry_lines(children, indent = indent + 2L))
    }
  }
  lines
}

dina_command_catalog_lines <- function(catalog = dina_command_catalog(), proposal = NULL) {
  lines <- character()
  if (!is.null(proposal)) {
    lines <- c(lines, "", dina_recommendation_lines(proposal, title = "Recommended action"))
  }
  lines <- c(lines, "", "Command themes:")
  for (theme in catalog) {
    lines <- c(lines, sprintf("  %s:", theme$label))
    lines <- c(lines, dina_command_catalog_entry_lines(theme$children %||% list(), indent = 4L))
  }
  lines
}

dina_dashboard_git_status <- function(root = dina_repo_root()) {
  branch <- dina_git_capture(c("rev-parse", "--abbrev-ref", "HEAD"), root)
  status <- dina_git_capture(c("status", "--short"), root)
  if (!identical(branch$status, 0L) || !identical(status$status, 0L)) {
    return("not a Git checkout")
  }
  status_lines <- if (nzchar(status$output)) strsplit(status$output, "\n", fixed = TRUE)[[1]] else character()
  changed <- sum(nzchar(status_lines))
  if (changed > 0L) {
    sprintf("%s, %s changed file%s", trimws(branch$output), changed, if (changed == 1L) "" else "s")
  } else {
    sprintf("%s, clean", trimws(branch$output))
  }
}

dina_dashboard_project_name <- function(root = dina_repo_root()) {
  config <- tryCatch(dina_config(root), error = function(e) list())
  config$project$name %||% basename(normalizePath(root, mustWork = FALSE))
}

dina_dashboard_status_label <- function(state) {
  raw <- state$state %||% ""
  switch(
    raw,
    no_active_update = "no active update",
    failed = "task failed",
    pipeline_check_needed = "pipeline status not checked",
    sources_pending = "incoming source files present",
    sources_explored = "country-SNA explored",
    sources_include_ready = "country-SNA ready to confirm",
    sources_confirmed = "country-SNA confirmed",
    build_ready = "pipeline work pending",
    todo_pending = "todo items open",
    review_ready = "ready for final review",
    gsub("_", " ", raw, fixed = TRUE)
  )
}

dina_csv_key_value <- function(path) {
  if (!file.exists(path)) return(data.frame(key = character(), value = character(), stringsAsFactors = FALSE))
  utils::read.csv(path, stringsAsFactors = FALSE)
}

dina_manifest_value <- function(manifest, key) {
  if (!nrow(manifest) || !("key" %in% names(manifest)) || !("value" %in% names(manifest))) return("")
  hit <- manifest$key == key
  if (!any(hit)) "" else as.character(manifest$value[which(hit)[[1L]]])
}

dina_country_sna_explore_root <- function(root = dina_repo_root()) {
  file.path(root, "output", "experiments", "country_sna_explore")
}

dina_country_sna_include_root <- function(root = dina_repo_root()) {
  file.path(root, "output", "experiments", "country_sna_include")
}

dina_country_sna_inbox_signature <- function(root = dina_repo_root()) {
  rows <- dina_sources_country_sna_inbox_rows(root)
  if (!nrow(rows)) return(data.frame(rel = character(), size = numeric(), mtime = character(), stringsAsFactors = FALSE))
  rels <- if ("path" %in% names(rows)) rows$path else if ("inbox" %in% names(rows)) rows$inbox else character()
  files <- ifelse(grepl("^/", rels), rels, file.path(root, rels))
  files <- unique(files[file.exists(files)])
  info <- file.info(files)
  data.frame(
    rel = vapply(files, dina_relative, character(1), root = root),
    size = as.numeric(info$size),
    mtime = as.character(info$mtime),
    stringsAsFactors = FALSE
  )
}

dina_country_sna_matching_explore <- function(root = dina_repo_root()) {
  explore_root <- dina_country_sna_explore_root(root)
  manifest_path <- file.path(explore_root, "logs", "explore_manifest.csv")
  fingerprints_path <- file.path(explore_root, "tables", "source_fingerprints.csv")
  if (!file.exists(manifest_path) || !file.exists(fingerprints_path)) return(NULL)
  current <- dina_country_sna_inbox_signature(root)
  if (!nrow(current)) return(NULL)
  fingerprints <- utils::read.csv(fingerprints_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  fingerprints <- fingerprints[grepl("^input_data/_new/country_sna/", fingerprints$rel %||% ""), , drop = FALSE]
  if (!nrow(fingerprints)) return(NULL)
  key <- function(df) paste(df$rel, df$size, df$mtime, sep = "|")
  if (!all(key(current) %in% key(fingerprints))) return(NULL)
  manifest <- dina_csv_key_value(manifest_path)
  list(root = explore_root, manifest = manifest, run_id = dina_manifest_value(manifest, "run_id"))
}

dina_country_sna_include_runs <- function(root = dina_repo_root()) {
  runs_root <- file.path(dina_country_sna_include_root(root), "runs")
  if (!dir.exists(runs_root)) return(character())
  runs <- list.dirs(runs_root, full.names = TRUE, recursive = FALSE)
  runs[file.exists(file.path(runs, "logs", "include_manifest.csv"))]
}

dina_country_sna_latest_include_for_explore <- function(explore_root, root = dina_repo_root()) {
  runs <- dina_country_sna_include_runs(root)
  if (!length(runs)) return(NULL)
  matches <- lapply(runs, function(run) {
    manifest <- dina_csv_key_value(file.path(run, "logs", "include_manifest.csv"))
    prior <- normalizePath(dina_manifest_value(manifest, "exploration_run"), mustWork = FALSE)
    if (!identical(prior, normalizePath(explore_root, mustWork = FALSE))) return(NULL)
    list(root = run, manifest = manifest, status = dina_manifest_value(manifest, "status"), mtime = file.info(file.path(run, "logs", "include_manifest.csv"))$mtime)
  })
  matches <- Filter(Negate(is.null), matches)
  if (!length(matches)) return(NULL)
  matches[[order(vapply(matches, function(x) as.numeric(x$mtime), numeric(1)), decreasing = TRUE)[[1L]]]]
}

dina_country_sna_confirm_for_include <- function(include_run, root = dina_repo_root()) {
  confirms_root <- file.path(dina_country_sna_include_root(root), "confirms")
  if (!dir.exists(confirms_root)) return(NULL)
  confirms <- list.dirs(confirms_root, full.names = TRUE, recursive = FALSE)
  matches <- lapply(confirms, function(confirm) {
    manifest <- dina_csv_key_value(file.path(confirm, "logs", "confirm_manifest.csv"))
    if (!nrow(manifest)) return(NULL)
    prior <- normalizePath(dina_manifest_value(manifest, "include_run"), mustWork = FALSE)
    if (!identical(prior, normalizePath(include_run, mustWork = FALSE))) return(NULL)
    list(root = confirm, manifest = manifest, status = dina_manifest_value(manifest, "status"), mtime = file.info(file.path(confirm, "logs", "confirm_manifest.csv"))$mtime)
  })
  matches <- Filter(Negate(is.null), matches)
  if (!length(matches)) return(NULL)
  matches[[order(vapply(matches, function(x) as.numeric(x$mtime), numeric(1)), decreasing = TRUE)[[1L]]]]
}

dina_dashboard_state_fast <- function(session, root = dina_repo_root()) {
  if (is.null(session)) {
    return(dina_session_result(
      "no_active_update",
      dina_recommendation(
        command = "dina update start YEAR",
        why = "No active update workspace is available.",
        expected_action = "Create an active update workspace, source baseline, todo state, and inbox buckets.",
        next_command = "dina update status",
        next_note = "Inspect the new workspace and follow the concrete recommendation.",
        recommendation = "Start an update with `dina update start YEAR`."
      )
    ))
  }

  failed <- names(session$task_runs)[vapply(session$task_runs, function(x) identical(x$status, "failed"), logical(1))]
  if (length(failed)) {
    task <- failed[[length(failed)]]
    return(dina_session_result(
      "failed",
      dina_recommendation(
        command = sprintf("dina run why %s", task),
        why = sprintf("Task %s failed in this update session.", task),
        todo_id = "run-pipeline",
        todo_label = dina_todo_label(root, "run-pipeline"),
        expected_action = "Read the failure reason, then preview or rerun the task deliberately.",
        next_command = sprintf("dina run %s --dry-run", task),
        next_note = "If the preview looks right, rerun without --dry-run.",
        recommendation = sprintf("Inspect failed task with `dina run why %s`, then retry deliberately.", task)
      )
    ))
  }

  country_sna_inbox <- dina_sources_country_sna_inbox_rows(root)
  if (nrow(country_sna_inbox) > 0L) {
    explore <- dina_country_sna_matching_explore(root)
    if (!is.null(explore)) {
      include <- dina_country_sna_latest_include_for_explore(explore$root, root)
      if (!is.null(include) && identical(include$status, "all_good")) {
        confirm <- dina_country_sna_confirm_for_include(include$root, root)
        if (!is.null(confirm) && identical(confirm$status, "confirmed")) {
          return(dina_session_result(
            "sources_confirmed",
            dina_recommendation(
              command = "dina run 01b --dry-run",
              why = "Country-SNA incoming files were explored, staged, and confirmed with a backup snapshot.",
              todo_id = "country-sna-source-workflow",
              todo_label = "Explore country-SNA source changes",
              expected_action = "Preview the country-SNA pipeline step after confirmed source promotion.",
              next_command = "dina run 01b",
              next_note = "Run without --dry-run only after reviewing the preview.",
              recommendation = "Preview 01b with `dina run 01b --dry-run`."
            )
          ))
        }
        return(dina_session_result(
          "sources_include_ready",
          dina_recommendation(
            command = sprintf("dina sources include sna --confirm --include-run %s", include$root),
            why = "A matching country-SNA exploration run exists and the latest include dry-run is clean.",
            todo_id = "country-sna-source-workflow",
            todo_label = "Explore country-SNA source changes",
            expected_action = "Promote approved incoming source files after the staged run and backup guard.",
            next_command = "dina run 01b --dry-run",
            next_note = "Confirm does not run the pipeline; preview 01b afterward.",
            recommendation = "Confirm the clean staged include run."
          )
        ))
      }
      return(dina_session_result(
        "sources_explored",
        dina_recommendation(
          command = "dina sources include sna --dry-run",
          why = "A matching country-SNA exploration run exists for the current incoming files.",
          todo_id = "country-sna-source-workflow",
          todo_label = "Explore country-SNA source changes",
          expected_action = "Stage incoming sources and check deterministic include expectations without changing production files.",
          next_command = "dina sources include sna --confirm --include-run RUN",
          next_note = "Confirm is only available after an all_good include dry-run.",
          recommendation = "Run a staged include dry-run."
        )
      ))
    }
    return(dina_session_result(
      "sources_pending",
      dina_recommendation(
        command = "dina sources explore sna",
        why = sprintf("%s incoming country-SNA source file%s %s waiting in input_data/_new/country_sna.", nrow(country_sna_inbox), if (nrow(country_sna_inbox) == 1L) "" else "s", if (nrow(country_sna_inbox) == 1L) "is" else "are"),
        todo_id = "country-sna-source-workflow",
        todo_label = "Explore country-SNA source changes",
        expected_action = "Inventory new files, likely years, layout changes, and expected variables before attempting inclusion.",
        next_command = "dina sources include sna --dry-run",
        next_note = "Use include after reviewing the exploration output.",
        recommendation = "Explore incoming SNA files with `dina sources explore sna`."
      )
    ))
  }

  dina_session_result(
    "pipeline_check_needed",
    dina_recommendation(
      command = "dina run stale --dry-run",
      why = "Task freshness is checked when you run the preview so this dashboard can open quickly.",
      todo_id = "run-pipeline",
      todo_label = dina_todo_label(root, "run-pipeline"),
      expected_action = "Preview stale, missing, or never-run pipeline tasks before changing outputs.",
      next_command = "dina run stale",
      next_note = "Run without --dry-run after reviewing the preview.",
      recommendation = "Preview stale tasks with `dina run stale --dry-run`."
    ),
    stale_tasks = NA_integer_
  )
}

dina_dashboard_common_commands <- function(session, proposal = NULL) {
  proposed <- trimws(proposal$command %||% "")
  commands <- if (is.null(session)) {
    c(
      proposed,
      "dina doctor",
      "dina update list",
      "dina sources list",
      "dina run list"
    )
  } else {
    c(
      proposed,
      "dina doctor",
      "dina update status",
      "dina sources compare",
      "dina run list",
      "dina todo"
    )
  }
  commands <- commands[nzchar(commands)]
  commands[!duplicated(commands)][seq_len(min(5L, length(commands)))]
}

dina_dashboard_summary_lines <- function(root = dina_repo_root(), session = dina_load_session(root = root), state = dina_session_state(session, root), proposal = NULL) {
  proposal <- proposal %||% dina_state_proposal(state)
  lines <- c(
    "DINA-LatAm CLI",
    "",
    "Project status:",
    dina_cli_key_value("  Project:", dina_dashboard_project_name(root)),
    dina_cli_key_value("  Root:", normalizePath(root, mustWork = FALSE)),
    dina_cli_key_value("  Git:", dina_dashboard_git_status(root))
  )

  active <- dina_current_update(root)
  if (is.null(session)) {
    if (is.null(active)) {
      lines <- c(lines, "", dina_cli_key_value("Active update:", "none"), dina_cli_key_value("Status:", dina_dashboard_status_label(state)))
    } else {
      lines <- c(lines, "", dina_cli_key_value("Active update:", active), dina_cli_key_value("Status:", "active pointer exists, but manifest.json is missing"))
    }
  } else {
    year <- session$year %||% dina_update_year_from_id(session$id)
    lines <- c(lines, "", dina_cli_key_value("Active update:", sprintf("%s (%s)", year, session$id)), dina_cli_key_value("Status:", dina_dashboard_status_label(state)))
    if (nzchar(session$status %||% "")) {
      lines <- c(lines, dina_cli_key_value("Session status:", session$status))
    }
    override_path <- dina_session_config_override_path(session$id, root)
    effective <- tryCatch(dina_session_config(session, root, expand_env = FALSE), error = function(e) list())
    years <- effective$years %||% list()
    lines <- c(
      lines,
      dina_cli_key_value("Benchmark config:", "config/dina.yml"),
      dina_cli_key_value("Override:", if (file.exists(override_path)) "present" else "not created")
    )
    if (!is.null(years$first) && !is.null(years$last)) {
      lines <- c(lines, dina_cli_key_value("Effective years:", sprintf("%s-%s", years$first, years$last)))
    }
  }

  c(lines, "", dina_recommendation_lines(proposal, title = "Recommended"))
}

dina_dashboard_print_summary <- function(root = dina_repo_root(), session = dina_load_session(root = root), state = dina_session_state(session, root), proposal = NULL) {
  proposal <- proposal %||% dina_state_proposal(state)
  for (line in dina_dashboard_summary_lines(root, session = session, state = state, proposal = proposal)) {
    dina_cli_cat(line)
  }

  invisible(proposal)
}

dina_dashboard_command_args <- function(command, catalog = dina_command_catalog()) {
  entry <- dina_command_find_by_command(command, catalog)
  if (!is.null(entry)) {
    return(dina_command_template_args(entry))
  }
  parts <- strsplit(trimws(command %||% ""), "[[:space:]]+")[[1]]
  if (length(parts) && identical(parts[[1]], "dina")) {
    parts <- parts[-1]
  }
  parts[nzchar(parts)]
}

dina_dashboard_run_command <- function(command, root = dina_repo_root()) {
  args <- dina_dashboard_command_args(command)
  if (!length(args)) {
    dina_cli_warn("No command available to run.")
    return(invisible(NULL))
  }
  dina_cli_alert(sprintf("Running `%s`", dina_command_format(args)))
  dina_main(args, root = root)
}

dina_dashboard_run_proposal <- function(proposal, root = dina_repo_root()) {
  dina_dashboard_run_command(proposal$command %||% "", root = root)
}

dina_dashboard_action_menu <- function(root = dina_repo_root(), proposal = NULL, input = "stdin", is_terminal = isatty(stdin()), context = NULL, session = NULL, state = NULL) {
  if (!isTRUE(is_terminal)) {
    return(invisible(NULL))
  }
  if (is.null(proposal) || is.null(context)) {
    session <- session %||% dina_load_session(root = root)
    state <- state %||% dina_session_state(session, root)
    proposal <- proposal %||% dina_state_proposal(state)
  }
  if (is.null(context)) {
    context <- dina_dashboard_summary_lines(root, session = session, state = state, proposal = proposal)
  }
  actions <- list(
    dina_menu_action(
      "recommended",
      "Run recommended action",
      value = list(type = "recommended")
    ),
    dina_menu_action(
      "commands",
      "Commands menu",
      value = list(type = "navigator")
    )
  )
  selected <- dina_menu_select(
    "DINA Actions",
    actions,
    prompt = "",
    default = "recommended",
    allow_quit = TRUE,
    input = input,
    is_terminal = is_terminal,
    context = context
  )
  if (is.null(selected) || identical(selected, "quit")) {
    dina_cli_alert("No command run.")
    return(invisible(NULL))
  }
  if (identical(selected$type %||% "", "recommended")) {
    return(dina_dashboard_run_proposal(proposal, root = root))
  }
  if (identical(selected$type %||% "", "navigator")) {
    return(dina_command_browser(root, proposal = proposal, input = input, is_terminal = is_terminal))
  }
  invisible(NULL)
}

dina_dashboard_prompt <- function(root = dina_repo_root(), proposal = NULL, input = "stdin", is_terminal = isatty(stdin()), context = NULL, session = NULL, state = NULL) {
  dina_dashboard_action_menu(root, proposal = proposal, input = input, is_terminal = is_terminal, context = context, session = session, state = state)
}

dina_print_command_navigator <- function(root = dina_repo_root(), proposal = NULL, input = "stdin", is_terminal = isatty(stdin())) {
  if (is.null(proposal)) {
    session <- dina_load_session(root = root)
    state <- dina_session_state(session, root)
    proposal <- dina_state_proposal(state)
  }
  if (!isTRUE(is_terminal)) {
    dina_cli_header("DINA Command Navigator")
    for (line in dina_command_catalog_lines(proposal = proposal)) {
      cat(line, "\n", sep = "")
    }
    return(invisible(NULL))
  }
  dina_command_browser(root, proposal = proposal, input = input, is_terminal = is_terminal)
}

dina_print_dashboard <- function(root = dina_repo_root(), input = "stdin", is_terminal = isatty(stdin())) {
  session <- dina_load_session(root = root)
  state <- dina_dashboard_state_fast(session, root)
  proposal <- dina_state_proposal(state)
  context <- dina_dashboard_summary_lines(root, session = session, state = state, proposal = proposal)
  if (!isTRUE(is_terminal)) {
    for (line in context) {
      dina_cli_cat(line)
    }
    return(invisible(proposal))
  }
  dina_dashboard_prompt(root, proposal = proposal, input = input, is_terminal = is_terminal, context = context, session = session, state = state)
}

dina_source_counts_line <- function(counts) {
  counts <- counts[counts > 0L]
  if (!length(counts)) {
    return("none")
  }
  paste(sprintf("%s=%s", names(counts), as.integer(counts)), collapse = ", ")
}

dina_print_update_summary <- function(session, root = dina_repo_root()) {
  baseline_at <- session$source_baseline$created_at %||% session$created_at %||% NA_character_
  hash_mode <- session$source_baseline$hash_mode %||% "none"
  dina_cli_alert(sprintf("Source baseline: %s (hash: %s)", baseline_at, hash_mode))
  incoming <- dina_sources_inbox_rows(root)
  dina_cli_alert(sprintf("Incoming source files: %s", nrow(incoming)))
  if (nrow(incoming)) {
    country_sna <- incoming[incoming$family == "country_sna", , drop = FALSE]
    other <- incoming[incoming$family != "country_sna", , drop = FALSE]
    if (nrow(country_sna)) {
      dina_cli_alert("SNA incoming files can be explored with `dina sources explore sna`.")
    }
    if (nrow(other)) {
      dina_cli_alert("Other incoming files are informational only; no source validation/preparation workflow is implemented yet.")
    }
  }
  todos <- dina_todo_rows(session, root)
  open_todos <- if (nrow(todos)) sum(!todos$checked) else 0L
  dina_cli_alert(sprintf("Open todo items: %s", open_todos))
  task_runs <- session$task_runs %||% list()
  dina_cli_alert(sprintf("Recorded task runs: %s", length(task_runs)))
  override_path <- dina_session_config_override_path(session$id, root)
  effective <- tryCatch(dina_session_config(session, root, expand_env = FALSE), error = function(e) list())
  years <- effective$years %||% list()
  countries <- effective$countries %||% character()
  dina_cli_alert("Benchmark config path: config/dina.yml")
  dina_cli_alert(sprintf("Override status: %s", if (file.exists(override_path)) sprintf("present (%s)", dina_relative(override_path, root)) else "not created"))
  if (!is.null(years$first) && !is.null(years$last)) {
    dina_cli_alert(sprintf("Effective years: %s-%s", years$first, years$last))
  }
  if (length(countries)) {
    dina_cli_alert(sprintf("Effective countries: %s", paste(countries, collapse = ",")))
  }
  dina_cli_alert("Review command: dina config show")
  dina_cli_alert("Active-update override: dina update config show")
}

dina_print_todo_rows <- function(session = NULL, root = dina_repo_root()) {
  rows <- dina_todo_rows(session, root)
  dina_cli_header("Todo")
  if (!nrow(rows)) {
    dina_cli_warn("No todo items configured.")
    return(invisible(rows))
  }
  dina_cli_cat(sprintf("%-4s %-22s %s", "done", "id", "item"))
  for (i in seq_len(nrow(rows))) {
    dina_cli_cat(dina_cli_row(
      list(if (isTRUE(rows$checked[[i]])) "yes" else "no", rows$id[[i]], rows$label[[i]]),
      widths = c(4L, 22L, NA),
      dim = c(TRUE, FALSE, TRUE)
    ))
  }
  dina_cli_cat("")
  dina_cli_cat("Modify this list:")
  dina_cli_alert("Mark done: dina todo check ID")
  dina_cli_alert("Mark undone: dina todo uncheck ID")
  dina_cli_alert("Reset checked state: dina todo reset")
  dina_cli_alert("Edit default items/labels: config/todo.yml")
  dina_cli_alert("Checked state lives in the active update manifest.")
  invisible(rows)
}

dina_source_diff_report_rows <- function(source_status) {
  diff <- source_status$diff %||% list()
  if (!length(diff)) {
    return(data.frame(source_id = character(), classes = character(), counts = character(), stringsAsFactors = FALSE))
  }
  rows <- lapply(diff, function(item) {
    data.frame(
      source_id = item$id %||% "",
      classes = paste(item$classes %||% character(), collapse = ", "),
      counts = dina_source_counts_line(item$counts %||% dina_empty_source_status_counts()),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

dina_task_report_rows <- function(task_status) {
  if (!length(task_status)) {
    return(data.frame(id = character(), stage = character(), language = character(), status = character(), stringsAsFactors = FALSE))
  }
  rows <- lapply(task_status, function(status) {
    data.frame(
      id = status$id %||% "",
      stage = status$stage %||% "",
      language = status$language %||% "",
      status = status$status %||% "",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

dina_records_df <- function(records, columns) {
  if (!length(records)) {
    out <- as.data.frame(stats::setNames(rep(list(character()), length(columns)), columns), stringsAsFactors = FALSE)
    return(out)
  }
  rows <- lapply(records, function(record) {
    values <- lapply(columns, function(column) as.character(record[[column, exact = TRUE]] %||% ""))
    names(values) <- columns
    as.data.frame(values, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

dina_output_freshness_rows <- function(session, root = dina_repo_root()) {
  config <- dina_session_config(session, root, expand_env = FALSE)
  outputs <- dina_source_values(config$paths$final_outputs %||% character())
  if (!length(outputs)) {
    return(data.frame(path = character(), exists = logical(), files = integer(), latest = character(), stringsAsFactors = FALSE))
  }
  rows <- lapply(outputs, function(path) {
    full <- if (grepl("^/", path)) path else file.path(root, path)
    exists <- file.exists(full)
    files <- if (dir.exists(full)) dina_count_files(full) else if (exists) 1L else 0L
    latest <- ""
    if (exists) {
      candidates <- if (dir.exists(full)) {
        list.files(full, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      } else {
        full
      }
      candidates <- candidates[file.exists(candidates) & !dir.exists(candidates)]
      if (length(candidates)) {
        latest <- format(max(file.info(candidates)$mtime, na.rm = TRUE), "%Y-%m-%d %H:%M:%S")
      }
    }
    data.frame(path = path, exists = exists, files = files, latest = latest, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

dina_update_close_report <- function(session, root = dina_repo_root()) {
  source_status <- tryCatch(dina_sources_compare(session, root, hash = "changed"), error = function(e) NULL)
  task_status <- dina_all_task_status(root = root, session = session)
  incoming <- dina_sources_inbox_rows(root)
  repo_diff <- tryCatch(dina_repo_state_compare(session$id, root = root, baseline = "start"), error = function(e) NULL)
  list(
    id = session$id,
    source_status = source_status,
    sources = if (is.null(source_status)) data.frame(source_id = character(), classes = character(), counts = character(), stringsAsFactors = FALSE) else dina_source_diff_report_rows(source_status),
    incoming = incoming,
    incoming_files = nrow(incoming),
    task_status = task_status,
    tasks = dina_task_report_rows(task_status),
    task_runs = session$task_runs %||% list(),
    todo = dina_todo_rows(session, root),
    outputs = dina_output_freshness_rows(session, root),
    repo_diff = repo_diff,
    config_override = file.exists(dina_session_config_override_path(session$id, root))
  )
}

dina_print_update_close <- function(report) {
  dina_cli_header("Update Close Report")
  dina_cli_alert(sprintf("Update: %s", report$id))
  dina_cli_alert(sprintf("Incoming source files still in _new: %s", report$incoming_files))
  if (!is.null(report$source_status)) {
    dina_cli_cat(dina_cli_key_value("Source changes:", dina_source_counts_line(report$source_status$counts)))
  }
  task_counts <- table(vapply(report$task_status, function(x) x$status %||% "", character(1)))
  if (length(task_counts)) {
    dina_cli_cat(dina_cli_key_value("Task status:", paste(sprintf("%s=%s", names(task_counts), as.integer(task_counts)), collapse = ", ")))
  }
  if (nrow(report$outputs)) {
    fresh <- sum(report$outputs$exists)
    dina_cli_cat(dina_cli_key_value("Output freshness:", sprintf("%s/%s configured outputs present", fresh, nrow(report$outputs))))
  }
  if (nrow(report$todo)) {
    dina_cli_cat(dina_cli_key_value("Todo:", sprintf("%s/%s checked", sum(report$todo$checked), nrow(report$todo))))
  }
  if (!is.null(report$repo_diff)) {
    counts <- as.integer(report$repo_diff$counts)
    names(counts) <- names(report$repo_diff$counts)
    dina_cli_cat(dina_cli_key_value("Repo diff:", paste(sprintf("%s=%s", names(counts), counts), collapse = ", ")))
  }
  dina_cli_cat(dina_cli_key_value("Config override:", if (isTRUE(report$config_override)) "present" else "none"))
}

dina_print_source_compare <- function(status) {
  dina_cli_header("Source Compare")
  dina_cli_alert(sprintf("Baseline: %s (hash: %s)", status$baseline_at, status$baseline_hash_mode))
  dina_cli_alert(sprintf("Current scan: %s (hash: %s)", status$scanned_at, status$scan_hash_mode))
  if (!is.na(status$last_recorded_scan_at) && nzchar(status$last_recorded_scan_at)) {
    dina_cli_alert(sprintf("Last recorded scan: %s", status$last_recorded_scan_at))
  }
  dina_cli_alert("Compares configured source files against the update baseline; does not validate data or manage incoming files.")
  dina_cli_cat(dina_cli_key_value("File status counts:", dina_source_counts_line(status$counts)))
  changed <- Filter(function(x) !identical(x$classes, "unchanged"), status$diff)
  if (!length(changed)) {
    dina_cli_ok("No source file changes detected against the baseline.")
  } else {
    dina_cli_cat("Changed sources:")
    for (item in changed) {
      dina_cli_cat(sprintf("  %s: %s", item$id, dina_cli_dim(paste(item$classes, collapse = ","))))
    }
  }
}

dina_print_source_status <- function(status) {
  dina_print_source_compare(status)
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
        dina_cli_cat(sprintf("  %s", dina_cli_command(result$stata$suggestion)))
      }
    }
  } else if (isTRUE(result$stata$discovered)) {
    dina_cli_warn("Installed but not configured for DINA.")
    dina_cli_alert(sprintf("Discovered Stata via %s: %s", result$stata$discovered_source, result$stata$discovered_command))
    dina_cli_cat(sprintf("  %s", dina_cli_command(result$stata$suggestion)))
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
    if (!dina_menu_confirm("Install Packages", "Install missing packages now?", default = FALSE)) {
      dina_cli_warn("Installation skipped.")
      return(invisible(missing))
    }
  }
  install.packages(missing, dependencies = TRUE, repos = "https://cloud.r-project.org")
  invisible(missing)
}

dina_print_restart_preview <- function(result, root = dina_repo_root()) {
  dina_cli_warn(sprintf("Would reset update %s from scratch.", result$id))
  dina_cli_alert("Restart preview: no changes have been made yet.")
  dina_cli_alert(sprintf("Year: %s", result$year))
  dina_cli_alert(sprintf("Current status: %s", result$current_status))
  dina_cli_alert(sprintf("Session directory: %s", result$dir))
  dina_cli_alert(sprintf(
    "Files to clear: %s logs, %s snapshots.",
    result$log_files,
    result$snapshot_files
  ))
  dina_cli_alert("Source records will be replaced by a fresh source scan.")
  dina_cli_alert(dina_restart_source_inbox_note(result))
  dina_cli_alert("Repo history powers repo-status, repo-diff, and repo-restore.")
  if (length(result$repo_baselines %||% character())) {
    dina_cli_alert(sprintf("Saved repo history points: %s", paste(result$repo_baselines, collapse = ", ")))
  } else {
    dina_cli_alert("Saved repo history points: none")
  }
  dina_cli_alert("Default: restart normally, keep repo history, and save no checkpoint.")
  dina_cli_alert("Restart reuses the same update id; no suffixed session will be created.")
}

dina_restart_source_inbox_file_count <- function(result) {
  files <- as.integer(result$source_inbox$files %||% dina_source_inbox_bucket_file_count(result$source_inbox$buckets))
  if (is.na(files)) 0L else files
}

dina_restart_source_inbox_note <- function(result) {
  files <- dina_restart_source_inbox_file_count(result)
  if (files > 0L) {
    sprintf("Source inbox: %s incoming file(s) under input_data/_new will be kept.", files)
  } else {
    "Source inbox: empty; bucket folders will be kept or recreated."
  }
}

dina_restart_policy_label <- function(policy) {
  switch(
    policy %||% "preserve",
    preserve_checkpoint = "restart and save checkpoint",
    replace = "restart with current repo history",
    "restart normally"
  )
}

dina_restart_mode_choice <- function(result = NULL, default = "preserve", input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(default)
  }
  choice <- dina_menu_select(
    title = "Restart Options",
    items = list(
      dina_menu_action(
        "preserve",
        "Restart normally",
        value = "preserve",
        description = "Keep source inbox files and repo history; save no checkpoint.",
        help = "Keeps the existing start baseline used by repo-status, repo-diff, and repo-restore."
      ),
      dina_menu_action(
        "preserve_checkpoint",
        "Restart and save checkpoint",
        value = "preserve_checkpoint",
        description = "Save current code/config/docs first, then restart.",
        help = "Use this only when you may need to inspect or restore today's pre-restart repo state."
      ),
      dina_menu_action(
        "replace",
        "Restart with current repo history",
        value = "replace",
        description = "Make the current repo state the new comparison point.",
        help = "Replaces the start baseline. Source inbox files are still kept."
      ),
      dina_menu_action(
        "cancel",
        "Cancel restart",
        value = "cancel",
        description = "Leave the update session unchanged.",
        help = "Choose this if you want to move or clear source inbox files first."
      )
    ),
    prompt = "Choose one restart mode.",
    default = default,
    allow_quit = TRUE,
    input = input,
    is_terminal = TRUE
  )
  if (identical(choice, "cancel") || identical(choice, "quit") || is.null(choice)) {
    return(NULL)
  }
  choice
}

dina_restart_confirm_choice <- function(policy, input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(NULL)
  }
  choice <- dina_menu_select(
    title = "Confirm Restart",
    items = list(
      dina_menu_action(
        "restart",
        "Restart now",
        value = "restart",
        description = sprintf("Apply: %s.", dina_restart_policy_label(policy))
      ),
      dina_menu_action(
        "back",
        "Back",
        value = "back",
        description = "Return to restart options."
      ),
      dina_menu_action(
        "cancel",
        "Cancel restart",
        value = "cancel",
        description = "Leave the update session unchanged."
      )
    ),
    prompt = "No files are changed until you confirm here.",
    default = "restart",
    allow_quit = TRUE,
    input = input,
    is_terminal = TRUE
  )
  if (identical(choice, "quit") || is.null(choice)) {
    return("cancel")
  }
  choice
}

dina_restart_interactive_choice <- function(result, default = "preserve", input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(NULL)
  }
  repeat {
    policy <- dina_restart_mode_choice(result, default = default, input = input, is_terminal = TRUE)
    if (is.null(policy)) {
      return(NULL)
    }
    confirm <- dina_restart_confirm_choice(policy, input = input, is_terminal = TRUE)
    if (identical(confirm, "back")) {
      default <- policy
      next
    }
    if (identical(confirm, "restart")) {
      return(policy)
    }
    return(NULL)
  }
}

dina_print_restart_completion <- function(result, root = dina_repo_root()) {
  dina_cli_ok(sprintf("Restarted update %s from scratch.", result$id))
  dina_cli_alert(dina_restart_source_inbox_note(result))
  if (identical(result$repo_policy %||% "", "preserve_checkpoint")) {
    dina_cli_alert("Repo history: kept start baseline and saved a pre-restart checkpoint.")
  } else if (identical(result$repo_policy %||% "", "replace")) {
    dina_cli_alert("Repo history: current repo state is the new start baseline.")
  } else {
    dina_cli_alert("Repo history: kept start baseline; no pre-restart checkpoint saved.")
  }
}

dina_config_flatten <- function(x, prefix = "") {
  if (!is.list(x) || !length(x)) {
    return(list())
  }
  out <- list()
  for (name in names(x)) {
    if (is.null(name) || !nzchar(name)) {
      next
    }
    key <- if (nzchar(prefix)) paste(prefix, name, sep = ".") else name
    value <- x[[name]]
    if (is.list(value) && length(value) && !is.null(names(value))) {
      out <- c(out, dina_config_flatten(value, key))
    } else {
      out[[key]] <- value
    }
  }
  out
}

dina_config_value_at <- function(config, key) {
  current <- config
  for (part in strsplit(key %||% "", ".", fixed = TRUE)[[1]]) {
    if (!is.list(current) || is.null(current[[part]])) {
      return(list(exists = FALSE, value = NULL))
    }
    current <- current[[part]]
  }
  list(exists = TRUE, value = current)
}

dina_config_value_label <- function(value, exists = TRUE, width = 80L) {
  if (!isTRUE(exists)) {
    return("<unset>")
  }
  if (is.null(value) || !length(value)) {
    return("<empty>")
  }
  if (is.list(value)) {
    label <- paste(vapply(value, function(item) paste(as.character(item), collapse = ","), character(1)), collapse = ",")
  } else {
    label <- paste(as.character(value), collapse = ",")
  }
  if (!nzchar(label)) {
    label <- "<empty>"
  }
  dina_refresh_shorten(label, width)
}

dina_config_override_reason <- function(key) {
  switch(
    key,
    years.last = "next update year",
    export_validation.last_year = "next export validation year",
    export_validation.previous_update_file = "latest previous-series file",
    export_validation.previous_update_date = "inferred previous update date",
    "manual override"
  )
}

dina_config_override_diff_rows <- function(root = dina_repo_root(), override = list()) {
  flat <- dina_config_flatten(override)
  if (!length(flat)) {
    return(data.frame(key = character(), current = character(), proposed = character(), reason = character(), stringsAsFactors = FALSE))
  }
  base <- dina_config(root, expand_env = FALSE)
  rows <- lapply(names(flat), function(key) {
    current <- dina_config_value_at(base, key)
    proposed <- flat[[key]]
    data.frame(
      key = key,
      current = dina_config_value_label(current$value, exists = current$exists),
      proposed = dina_config_value_label(proposed, exists = TRUE),
      reason = dina_config_override_reason(key),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  rows[order(rows$key), , drop = FALSE]
}

dina_print_config_proposal <- function(session, root = dina_repo_root(), include_yaml = TRUE) {
  path <- dina_session_config_override_path(session$id, root)
  override <- if (file.exists(path)) dina_config_override(session, root) else list()
  dina_cli_header("Config Proposal")
  dina_cli_alert(sprintf("Override: %s", dina_relative(path, root)))
  dina_cli_alert(sprintf("Base config: %s", dina_relative(dina_config_path(root), root)))
  dina_cli_alert("Effective config: base config plus this active-update override.")
  rows <- dina_config_override_diff_rows(root, override)
  if (!nrow(rows)) {
    dina_cli_warn("No override values are proposed.")
  } else {
    dina_cli_cat(sprintf("%-42s %-80s %-80s %s", "key", "current", "proposed", "reason"))
    for (i in seq_len(nrow(rows))) {
      dina_cli_cat(dina_cli_row(
        list(rows$key[[i]], rows$current[[i]], rows$proposed[[i]], rows$reason[[i]]),
        widths = c(42L, 80L, 80L, NA),
        dim = c(FALSE, TRUE, FALSE, TRUE)
      ))
    }
  }
  if (isTRUE(include_yaml)) {
    dina_cli_cat("")
    dina_cli_cat("Override YAML:")
    if (!file.exists(path)) {
      dina_cli_warn("Override file is missing.")
    } else {
      lines <- readLines(path, warn = FALSE)
      if (!length(lines)) {
        dina_cli_cat("{}")
      } else {
        for (line in lines) {
          dina_cli_cat(line)
        }
      }
    }
  }
  invisible(path)
}

dina_print_update_workflow_reminder <- function() {
  dina_cli_header("Workflow Reminder")
  dina_cli_cat("1. Review the suggested update config override.")
  dina_cli_cat("2. Populate source buckets under input_data/_new as files become available.")
  dina_cli_cat("3. Inspect source guidance with `dina sources list` and its subcommands.")
  dina_cli_cat("4. Run source explore/include by source type when ready, starting with `dina sources explore sna`.")
  dina_cli_cat("5. Run pipeline tasks deliberately with `dina run TASK` or `dina run stale --dry-run`.")
  dina_cli_cat("6. Preview closure with `dina update close --dry-run` before closing.")
}

dina_print_update_config_override <- function(session, root = dina_repo_root()) {
  path <- dina_print_config_proposal(session, root, include_yaml = TRUE)
  dina_cli_alert("This file overrides config/dina.yml only for the active update.")
  dina_cli_alert("Review effective values with `dina update config show`.")
  invisible(path)
}

dina_review_update_config_override <- function(session, root = dina_repo_root(), input = "stdin", is_terminal = isatty(stdin())) {
  dina_print_update_config_override(session, root)
  if (!isTRUE(is_terminal)) {
    dina_cli_alert("Edit manually or run `dina update config edit` from an interactive terminal.")
    dina_cli_alert("Workflow reminder: `dina help workflow`.")
    return(invisible(session))
  }
  repeat {
    action <- dina_menu_select(
      title = "Config Override",
      items = list(
        dina_menu_action("continue", "Continue", value = "continue", description = "Keep the suggested override and continue."),
        dina_menu_action("edit", "Edit", value = "edit", description = "Open the override in the default editor."),
        dina_menu_action("workflow", "Workflow", value = "workflow", description = "Print a short update workflow reminder.")
      ),
      prompt = "Review the suggested override.",
      default = "continue",
      allow_quit = TRUE,
      input = input,
      is_terminal = is_terminal
    )
    if (is.null(action) || identical(action, "continue") || identical(action, "quit")) {
      return(invisible(session))
    }
    if (identical(action, "edit")) {
      session <- dina_update_config_edit(session, root = root)
      return(invisible(session))
    }
    if (identical(action, "workflow")) {
      dina_print_update_workflow_reminder()
      next
    }
  }
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
      if (!dina_menu_confirm("Update Start", "Continue?", default = FALSE)) {
        dina_cli_alert("No changes made. Pass --yes for non-interactive creation of the suffixed session.")
        quit(status = if (isatty(stdin())) 0 else 1)
      }
    } else if (isTRUE(plan$finalized)) {
      dina_cli_warn(sprintf("Closed same-day update already exists: %s. Creating %s.", plan$default_id, plan$id))
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
    dina_print_source_inbox_bucket_summary(session$source_inbox$buckets, folders = session$source_inbox$folders, root = root)
    session <- dina_review_update_config_override(session, root = root)
    dina_cli_ok("Recommended next action: dina sources list")
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
    dina_print_update_summary(session, root)
    dina_cli_cat("")
    dina_print_recommendation(dina_state_proposal(state))
  } else if (identical(sub, "roadmap")) {
    stop("`dina update roadmap` was removed. Use `dina todo`, `dina sources list`, and `dina run list`.", call. = FALSE)
  } else if (identical(sub, "gate")) {
    stop("`dina update gate` was removed. Use `dina todo` and source/run commands directly.", call. = FALSE)
  } else if (identical(sub, "mark")) {
    stop("`dina update mark` was removed. Use `dina todo check ID` for the helper checklist.", call. = FALSE)
  } else if (identical(sub, "unmark")) {
    stop("`dina update unmark` was removed. Use `dina todo uncheck ID` for the helper checklist.", call. = FALSE)
  } else if (identical(sub, "prefs")) {
    stop("`dina update prefs` was removed with the gate workflow.", call. = FALSE)
  } else if (identical(sub, "config")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    action <- dina_arg(rest, 1L, "show")
    if (identical(action, "show")) {
      dina_need("yaml")
      dina_cli_header("Update Config")
      dina_print_config_proposal(session, root, include_yaml = TRUE)
      dina_cli_cat("Effective config:")
      cat(yaml::as.yaml(dina_session_config(session, root, expand_env = FALSE)))
    } else if (identical(action, "set")) {
      stop("`dina update config set` is retired. Use `dina update config edit` to review or change the working override.", call. = FALSE)
    } else if (identical(action, "edit")) {
      dina_update_config_edit(session, root = root)
    } else {
      stop("Usage: dina update config show\n       dina update config edit", call. = FALSE)
    }
  } else if (identical(sub, "repo-status")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(rest)
    comparison <- dina_repo_state_compare_for_cli(session, root, baseline = flags$baseline %||% "start", title = "Update Repo Status")
    if (isTRUE(comparison$missing)) {
      return(invisible(comparison))
    }
    dina_print_repo_status(comparison)
  } else if (identical(sub, "repo-diff")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(rest)
    any_mode <- isTRUE(flags$stat) || isTRUE(flags$patch) || isTRUE(flags$files)
    comparison <- dina_repo_state_compare_for_cli(session, root, baseline = flags$baseline %||% "start", title = "Update Repo Diff")
    if (isTRUE(comparison$missing)) {
      return(invisible(comparison))
    }
    dina_print_repo_diff(
      session,
      root = root,
      baseline = flags$baseline %||% "start",
      stat = isTRUE(flags$stat) || !any_mode,
      patch = isTRUE(flags$patch),
      files = isTRUE(flags$files),
      comparison = comparison
    )
  } else if (identical(sub, "repo-restore")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(rest)
    result <- dina_repo_state_restore(session, root = root, baseline = flags$baseline %||% "start", yes = isTRUE(flags$yes))
    dina_print_repo_restore(result)
  } else if (identical(sub, "close")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(rest)
    report <- dina_update_close_report(session, root)
    dina_print_update_close(report)
    if (isTRUE(flags[["dry-run"]])) {
      dina_cli_ok("Dry-run only: workspace status was not changed.")
      return(invisible(report))
    }
    session$status <- "closed"
    session$closed_at <- dina_now()
    session$close_report <- list(
      incoming_files = report$incoming_files,
      task_status = as.list(table(report$tasks$status)),
      output_paths = nrow(report$outputs),
      config_override = report$config_override,
      closed_at = session$closed_at
    )
    session$updated_at <- dina_now()
    dina_save_session(session, root)
    dina_cli_ok("Update workspace closed.")
  } else if (identical(sub, "checklist")) {
    stop("Unknown update command: checklist. Use `dina todo`.", call. = FALSE)
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
      if (!dina_menu_confirm("Delete Update", "Delete this update session?", default = FALSE)) {
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
      stop("Usage: dina update restart [ID] [--yes] [--replace-repo-baseline] [--save-restart-checkpoint]", call. = FALSE)
    }
    if (isTRUE(flags[["replace-repo-baseline"]]) && isTRUE(flags[["preserve-repo-baseline"]])) {
      stop("Choose either --replace-repo-baseline or --preserve-repo-baseline, not both.", call. = FALSE)
    }
    if (isTRUE(flags[["replace-repo-baseline"]]) && isTRUE(flags[["save-restart-checkpoint"]])) {
      stop("Choose either --replace-repo-baseline or --save-restart-checkpoint, not both.", call. = FALSE)
    }
    repo_policy <- if (isTRUE(flags[["replace-repo-baseline"]])) {
      "replace"
    } else if (isTRUE(flags[["save-restart-checkpoint"]])) {
      "preserve_checkpoint"
    } else {
      "preserve"
    }
    update_id <- dina_arg(flags$positional, 1L, NULL)
    if (isTRUE(flags$yes)) {
      dina_cli_header("Update Restart")
    }
    result <- dina_update_restart(
      update_id,
      root = root,
      yes = isTRUE(flags$yes),
      repo_policy = repo_policy,
      progress = if (isTRUE(flags$yes)) dina_cli_progress else NULL
    )
    if (isTRUE(result$dry_run)) {
      dina_print_restart_preview(result, root = root)
      if (!isatty(stdin())) {
        dina_cli_alert("No changes made. Pass --yes for non-interactive restart.")
        return(invisible(result))
      }
      repo_policy <- dina_restart_interactive_choice(result, default = repo_policy)
      if (is.null(repo_policy)) {
        dina_cli_alert("No changes made. Restart cancelled before confirmation.")
        return(invisible(result))
      }
      dina_cli_header("Update Restart")
      result <- dina_update_restart(result$id, root = root, yes = TRUE, repo_policy = repo_policy, progress = dina_cli_progress)
      dina_print_restart_completion(result, root = root)
      if (!is.null(result$new_session)) {
        dina_review_update_config_override(result$new_session, root = root)
      }
    } else {
      dina_print_restart_completion(result, root = root)
      if (!is.null(result$new_session)) {
        dina_review_update_config_override(result$new_session, root = root)
      }
    }
  } else if (identical(sub, "finalize")) {
    stop("`dina update finalize` was replaced by `dina update close`.", call. = FALSE)
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
    family_label = dina_source_filter_label("source type", flags$family %||% NULL, family),
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
    dina_cli_alert(sprintf("Filter: source type %s", filters$family_label))
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
  view <- dina_source_view_value(flags$view %||% "compact")
  dina_print_source_registry_view(registry, root = root, view = view, include_urls = isTRUE(flags$urls))
  dina_print_source_registry_warnings(dina_source_registry_warnings(registry, root), limit = 8L)
  if (identical(view, "compact")) {
    dina_print_source_list_next_commands(registry, flags)
  }
  if (identical(view, "compact") && !isTRUE(flags[["no-menu"]])) {
    dina_source_list_actions_menu(registry, root = root, flags = flags)
  }
  invisible(registry)
}

dina_source_view_value <- function(view) {
  view <- tolower(trimws(view %||% "compact"))
  choices <- c("compact", "workflow", "paths", "all")
  if (!view %in% choices) {
    stop("Unknown source view: ", view, ". Use one of: ", paste(choices, collapse = ", "), call. = FALSE)
  }
  view
}

dina_source_one_line <- function(values, width = 24L, empty = "none") {
  values <- dina_source_values(values)
  if (!length(values)) {
    return(empty)
  }
  value <- values[[1L]]
  if (length(values) > 1L) {
    value <- sprintf("%s +%s", value, length(values) - 1L)
  }
  dina_refresh_shorten(value, width)
}

dina_source_script_label <- function(source, fields, width = 22L) {
  values <- unique(unlist(lapply(fields, function(field) dina_source_values(dina_source_field(source, field))), use.names = FALSE))
  if (!length(values)) {
    return("none")
  }
  label <- basename(values[[1L]])
  if (length(values) > 1L) {
    label <- sprintf("%s +%s", label, length(values) - 1L)
  }
  dina_refresh_shorten(label, width)
}

dina_source_note_label <- function(source, width = 36L) {
  notes <- dina_source_values(dina_source_field(source, "notes"))
  if (!length(notes)) {
    return("none")
  }
  dina_refresh_shorten(notes[[1L]], width)
}

dina_source_destination_label <- function(source, width = 28L) {
  destination <- unique(c(
    dina_source_values(dina_source_field(source, "destination")),
    dina_source_values(dina_source_field(source, "destinations"))
  ))
  dina_source_one_line(destination, width = width)
}

dina_source_fetch_label <- function(source, width = 24L) {
  fetcher <- dina_source_values(dina_source_field(source, "fetcher"))
  downloader <- dina_source_values(dina_source_field(source, "downloader"))
  if (length(fetcher)) {
    return(dina_refresh_shorten(basename(fetcher[[1L]]), width))
  }
  if ((source$method %||% "") %in% c("url", "zip") && nzchar(dina_source_direct_fetch_url(source))) {
    return("direct URL")
  }
  if (length(downloader)) {
    return(dina_refresh_shorten(basename(downloader[[1L]]), width))
  }
  source$method %||% "manual"
}

dina_source_url_label <- function(source, width = 18L) {
  urls <- dina_source_urls(source)
  if (!length(urls)) {
    return("none")
  }
  if (length(urls) == 1L) {
    return(dina_refresh_shorten(urls[[1L]], width))
  }
  sprintf("%s urls", length(urls))
}

dina_source_influence_label <- function(source, root = dina_repo_root(), width = 28L) {
  tasks <- dina_source_pipeline_users(source, root)
  if (length(tasks)) {
    label <- paste(vapply(tasks, dina_task_short_id, character(1)), collapse = ",")
    return(dina_refresh_shorten(label, width))
  }
  notes <- dina_source_values(dina_source_field(source, "notes"))
  if (length(notes)) {
    return(dina_refresh_shorten(notes[[1L]], width))
  }
  "not linked"
}

dina_print_source_registry_view <- function(registry, root = dina_repo_root(), view = "compact", include_urls = FALSE) {
  view <- dina_source_view_value(view)
  if (identical(view, "compact")) {
    dina_cli_cat(sprintf("%-28s %-8s %-11s %-8s %-18s %-22s %-26s %-18s %s", "id", "type", "country", "method", "urls", "bucket", "destination", "transformer", "influence"))
    for (source in registry) {
      dina_cli_cat(dina_cli_row(
        list(
          dina_refresh_shorten(source$id %||% "", 28L),
          dina_source_public_family(source),
          dina_source_country_summary(source, root),
          source$method %||% "",
          dina_source_url_label(source, 18L),
          dina_refresh_shorten(dina_source_inbox_bucket_rel(source), 22L),
          dina_source_destination_label(source, 26L),
          dina_source_script_label(source, "transformer", 18L),
          dina_source_influence_label(source, root, 34L)
        ),
        widths = c(28L, 8L, 11L, 8L, 18L, 22L, 26L, 18L, NA),
        dim = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
      ))
      if (isTRUE(include_urls)) {
        for (url in dina_source_urls(source)) dina_cli_cat(sprintf("  %s %s", dina_cli_dim("url:"), url))
      }
    }
  } else if (identical(view, "workflow")) {
    dina_cli_cat(sprintf("%-28s %-16s %-22s %-26s %-26s %-20s %s", "id", "fetch", "bucket", "destination", "transformer", "tasks", "notes"))
    for (source in registry) {
      tasks <- dina_source_pipeline_users(source, root)
      dina_cli_cat(dina_cli_row(
        list(
          dina_refresh_shorten(source$id %||% "", 28L),
          dina_source_fetch_label(source, 16L),
          dina_refresh_shorten(dina_source_inbox_bucket_rel(source), 22L),
          dina_source_destination_label(source, 26L),
          dina_source_script_label(source, "transformer", 26L),
          dina_refresh_shorten(if (length(tasks)) paste(tasks, collapse = ",") else "none", 20L),
          dina_source_note_label(source, 42L)
        ),
        widths = c(28L, 16L, 22L, 26L, 26L, 20L, NA),
        dim = c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
      ))
      if (isTRUE(include_urls)) {
        for (url in dina_source_urls(source)) dina_cli_cat(sprintf("  %s %s", dina_cli_dim("url:"), url))
      }
    }
  } else if (identical(view, "paths")) {
    for (source in registry) {
      dina_cli_cat(sprintf("%s %s", dina_cli_name(source$id %||% ""), dina_cli_dim(sprintf("[%s]", dina_source_public_family(source)))))
      dina_print_source_field("  canonical", source$canonical %||% character())
      dina_print_source_field("  inbox", source$inbox %||% character())
      dina_print_source_field("  destination", c(source$destination %||% character(), source$destinations %||% character()))
      dina_print_source_field("  fetch_target", source$fetch_target %||% character())
      if (isTRUE(include_urls)) dina_print_source_field("  urls", dina_source_urls(source))
    }
  } else {
    for (source in registry) {
      dina_print_source_show(root, source$id %||% "", include_urls = include_urls, view = "all")
      dina_cli_cat("")
    }
  }
  invisible(registry)
}

dina_source_list_filter_args <- function(flags) {
  args <- character()
  if (!is.null(flags$family) && !isTRUE(flags$family) && nzchar(flags$family)) {
    args <- c(args, flags$family)
  }
  if (!is.null(flags$country) && !isTRUE(flags$country) && nzchar(flags$country)) {
    args <- c(args, "--country", flags$country)
  }
  if (!is.null(flags$method) && !isTRUE(flags$method) && nzchar(flags$method)) {
    args <- c(args, "--method", flags$method)
  }
  args
}

dina_source_list_command <- function(args) {
  dina_command_format(c("sources", "list", args))
}

dina_print_source_list_next_commands <- function(registry, flags = list()) {
  if (!length(registry)) {
    return(invisible(NULL))
  }
  filter_args <- dina_source_list_filter_args(flags)
  first_id <- registry[[1]]$id %||% "SOURCE"
  dina_cli_cat("")
  dina_cli_cat("More source detail:")
  dina_cli_cat(sprintf("  %s", dina_cli_command(dina_source_list_command(c("detail", first_id, "--urls")))))
  dina_cli_cat(sprintf("  %s", dina_cli_command(dina_source_list_command(c("guide", filter_args, "--urls")))))
  dina_cli_cat(sprintf("  %s", dina_cli_command(dina_source_list_command(c("workflow", filter_args)))))
  dina_cli_cat(sprintf("  %s", dina_cli_command(dina_source_list_command(c("paths", filter_args)))))
  invisible(NULL)
}

dina_source_list_actions_menu <- function(registry, root = dina_repo_root(), flags = list(), input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(invisible(NULL))
  }
  type_label <- flags$family %||% "matched"
  choice <- dina_menu_select(
    title = "Source List Actions",
    items = list(
      dina_menu_action("detail", "Detail", value = "detail", description = "Open one full source card by id."),
      dina_menu_action("guide", "Guide", value = "guide", description = sprintf("Show acquisition, bucket, destination, and task guidance for %s sources.", type_label)),
      dina_menu_action("workflow", "Workflow", value = "workflow", description = "Reprint acquisition, destination, transformer, and task influence."),
      dina_menu_action("paths", "Paths", value = "paths", description = "Reprint canonical, inbox, destination, and fetch target paths."),
      dina_menu_action("urls", "URLs", value = "urls", description = "Expand source URLs for these matches."),
      dina_menu_action("quit", "Quit", value = "quit", description = "Dismiss this menu.")
    ),
    prompt = "Choose a follow-up action.",
    default = "quit",
    allow_quit = TRUE,
    input = input,
    is_terminal = is_terminal
  )
  if (is.null(choice) || identical(choice, "quit")) {
    return(invisible(NULL))
  }
  if (identical(choice, "detail")) {
    id_result <- dina_menu_text("Source Detail", "Source id", default = registry[[1]]$id %||% "", input = input, is_terminal = is_terminal)
    id <- trimws(id_result$value %||% "")
    if (!isTRUE(id_result$quit) && nzchar(id)) {
      return(dina_print_source_show(root, id, include_urls = isTRUE(flags$urls), view = "all"))
    }
    return(invisible(NULL))
  }
  if (identical(choice, "guide")) {
    return(dina_print_sources_guide(root, flags))
  }
  if (identical(choice, "urls")) {
    return(dina_print_source_registry_view(registry, root = root, view = "compact", include_urls = TRUE))
  }
  dina_print_source_registry_view(registry, root = root, view = choice, include_urls = isTRUE(flags$urls))
}

dina_print_source_registry_warnings <- function(rows, limit = 8L) {
  if (!is.data.frame(rows) || !nrow(rows)) {
    return(invisible(rows))
  }
  dina_cli_cat("")
  dina_cli_warn(sprintf("Source registry warnings: %s", nrow(rows)))
  shown <- utils::head(rows, limit)
  for (i in seq_len(nrow(shown))) {
    dina_cli_cat(dina_cli_dim(sprintf(
      "  %s [%s]: %s",
      shown$source_id[[i]],
      shown$field[[i]],
      shown$warning[[i]]
    )))
  }
  if (nrow(rows) > nrow(shown)) {
    dina_cli_cat(dina_cli_dim(sprintf("  ... %s more", nrow(rows) - nrow(shown))))
  }
  invisible(rows)
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
    "Usage: dina sources list [SOURCETYPE] [--country ISO] [--urls] [--no-menu]",
    "       dina sources list detail ID [--urls]",
    "       dina sources list guide [ID|SOURCETYPE] [--urls]",
    "       dina sources list workflow [SOURCETYPE] [--urls]",
    "       dina sources list paths [SOURCETYPE] [--urls]",
    "       dina sources list urls [SOURCETYPE|ID]",
    "       dina sources list country ISO [--urls]",
    sep = "\n"
  )
}

dina_source_token_resolves_family <- function(token, root = dina_repo_root()) {
  token <- token %||% ""
  if (!nzchar(token)) {
    return(FALSE)
  }
  tryCatch({
    dina_source_resolve_family_filter(token, root)
    TRUE
  }, error = function(e) FALSE)
}

dina_parse_source_list_flags <- function(args, root = dina_repo_root()) {
  flags <- dina_parse_flags(args)
  allowed <- c("positional", "family", "country", "method", "urls", "view", "no-menu")
  friendly_filters <- c("country", "family", "method")
  positional <- flags$positional %||% character()
  extra <- setdiff(names(flags), allowed)

  if (length(positional)) {
    filter_name <- positional[[1]]
    if (filter_name %in% friendly_filters) {
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
    } else if (length(positional) == 1L && dina_source_token_resolves_family(filter_name, root)) {
      if (!is.null(flags$family)) {
        stop("Duplicate sources list source type: ", filter_name, "\n", dina_source_list_usage(), call. = FALSE)
      }
      flags$family <- filter_name
    } else {
      stop("Unknown sources list source type or filter: ", paste(positional, collapse = " "), "\n", dina_source_list_usage(), call. = FALSE)
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
  if (isTRUE(flags$view)) {
    stop("Missing value for --view\n", dina_source_list_usage(), call. = FALSE)
  }
  if (!is.null(flags$view)) {
    flags$view <- dina_source_view_value(flags$view)
  }
  flags
}

dina_source_ids <- function(root = dina_repo_root()) {
  vapply(dina_sources(root)$sources, function(source) source$id %||% "", character(1))
}

dina_parse_source_guide_flags <- function(root, args) {
  flags <- dina_parse_flags(args)
  positional <- flags$positional %||% character()
  if (length(positional) == 1L) {
    candidate <- positional[[1]]
    if (!(candidate %in% dina_source_ids(root)) && dina_source_token_resolves_family(candidate, root)) {
      flags$family <- candidate
      flags$positional <- character()
    }
  }
  flags
}

dina_print_source_methods <- function() {
  methods <- dina_source_method_glossary()
  dina_cli_header("Source Methods")
  dina_cli_cat(sprintf("%-8s %-7s %s", "method", "refresh", "meaning"))
  for (i in seq_len(nrow(methods))) {
    dina_cli_cat(dina_cli_row(
      list(methods$method[[i]], methods$refresh[[i]], methods$description[[i]]),
      widths = c(8L, 7L, NA),
      dim = c(FALSE, TRUE, TRUE)
    ))
  }
  invisible(methods)
}

dina_print_source_show <- function(root, id, include_urls = FALSE, view = "all") {
  source <- dina_source_by_id(id, root)
  view <- dina_source_view_value(view)
  dina_cli_header(sprintf("Source %s", source$id))
  dina_cli_cat(dina_cli_key_value("source type:", dina_source_public_family(source)))
  dina_cli_cat(dina_cli_key_value("internal family:", source$family %||% ""))
  dina_cli_cat(dina_cli_key_value("country:", dina_source_country_summary(source, root)))
  coverage <- dina_source_country_values(source, root)
  if (length(coverage) > 1L) {
    dina_print_source_field("country coverage", coverage)
  }
  method <- source$method %||% ""
  method_description <- dina_source_method_description(method)
  if (nzchar(method_description)) {
    dina_cli_cat(sprintf("method: %s %s", method, dina_cli_dim(sprintf("- %s", method_description))))
  } else {
    dina_cli_cat(dina_cli_key_value("method:", method))
  }
  if (identical(view, "compact")) {
    dina_cli_cat(dina_cli_key_value("bucket:", dina_source_inbox_bucket_rel(source)))
    dina_cli_cat(dina_cli_key_value("destination:", dina_source_destination_label(source, 80L)))
    dina_cli_cat(dina_cli_key_value("transformer:", dina_source_script_label(source, "transformer", 80L)))
    dina_cli_cat(dina_cli_key_value("notes:", dina_source_note_label(source, 120L)))
    return(invisible(source))
  }
  if (identical(view, "workflow")) {
    dina_cli_cat(dina_cli_key_value("fetch:", dina_source_fetch_label(source, 80L)))
    dina_cli_cat(dina_cli_key_value("bucket:", dina_source_inbox_bucket_rel(source)))
    dina_print_source_field("destination", c(source$destination %||% character(), source$destinations %||% character()))
    dina_print_source_field("downloader", source$downloader %||% character())
    dina_print_source_field("fetcher", source$fetcher %||% character())
    dina_print_source_field("transformer", source$transformer %||% character())
    dina_print_source_field("used by tasks", dina_source_pipeline_users(source, root))
    dina_print_source_field("notes", source$notes %||% character())
    if (isTRUE(include_urls)) dina_print_source_field("urls", dina_source_urls(source))
    return(invisible(source))
  }
  if (identical(view, "paths")) {
    dina_print_source_field("canonical", source$canonical %||% character())
    dina_print_source_field("inbox", source$inbox %||% character())
    dina_print_source_field("legacy_inbox", source$legacy_inbox %||% source$legacy_inboxes %||% character())
    dina_print_source_field("destination", c(source$destination %||% character(), source$destinations %||% character()))
    dina_print_source_field("fetch_target", source$fetch_target %||% character())
    if (isTRUE(include_urls)) dina_print_source_field("urls", dina_source_urls(source))
    return(invisible(source))
  }
  urls <- dina_source_urls(source)
  if (length(urls)) {
    dina_cli_cat("urls:")
    for (url in urls) dina_cli_cat(sprintf("  - %s", url))
  } else if (isTRUE(include_urls)) {
    dina_cli_cat("urls: none")
  }
  dina_print_source_field("canonical", source$canonical %||% character())
  dina_print_source_field("inbox", source$inbox %||% character())
  dina_print_source_field("legacy_inbox", source$legacy_inbox %||% source$legacy_inboxes %||% character())
  dina_print_source_field("inbox_examples", source$inbox_examples %||% character())
  dina_print_source_field("destination", source$destination %||% character())
  dina_print_source_field("destinations", source$destinations %||% character())
  dina_print_source_field("downloader", source$downloader %||% character())
  dina_print_source_field("fetcher", source$fetcher %||% character())
  dina_print_source_field("fetch_target", source$fetch_target %||% character())
  dina_print_source_field("transformer", source$transformer %||% character())
  dina_print_source_field("used by tasks", dina_source_pipeline_users(source, root))
  dina_print_source_field("notes", source$notes %||% character())
  invisible(source)
}

dina_print_sources_guide <- function(root, flags) {
  source_id <- dina_arg(flags$positional %||% character(), 1L, NULL)
  registry <- if (!is.null(source_id) && nzchar(source_id)) {
    list(dina_source_by_id(source_id, root))
  } else {
    filters <- dina_source_list_filters(root, flags)
    dina_source_registry(root, family = filters$family, country = filters$country, method = filters$method)
  }
  dina_cli_header("Source Guide")
  if (!length(registry)) {
    dina_cli_warn("No sources matched.")
    return(invisible(registry))
  }
  for (source in registry) {
    dina_cli_cat(sprintf("%s %s", dina_cli_name(source$id %||% ""), dina_cli_dim(sprintf("[%s / %s]", dina_source_public_family(source), source$method %||% ""))))
    urls <- dina_source_urls(source)
    if (length(urls)) {
      shown <- if (isTRUE(flags$urls)) urls else urls[[1L]]
      for (url in shown) dina_cli_cat(sprintf("  %s %s", dina_cli_dim("get:"), dina_cli_dim(url)))
      if (!isTRUE(flags$urls) && length(urls) > 1L) {
        dina_cli_cat(dina_cli_dim(sprintf("  %s %s", "more:", sprintf("dina sources list detail %s --urls", source$id %||% ""))))
      }
    }
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("bucket:"), dina_source_inbox_bucket_rel(source)))
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("destination:"), dina_source_destination_label(source, 90L)))
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("transformer:"), dina_source_script_label(source, "transformer", 90L)))
    tasks <- dina_source_pipeline_users(source, root)
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("tasks:"), if (length(tasks)) paste(tasks, collapse = ", ") else "none"))
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("notes:"), dina_source_note_label(source, 110L)))
  }
  invisible(registry)
}

dina_print_source_fields <- function() {
  dina_cli_header("Source Fields")
  rows <- data.frame(
    field = c("id", "type", "country", "method", "url/urls", "inbox", "destination", "fetcher", "fetch_target", "transformer", "notes"),
    meaning = c(
      "Stable source identifier.",
      "Public source type used for filters; detail view also shows the internal family.",
      "Country or broad-country coverage.",
      "Acquisition mode: url, zip, script, manual, wid.",
      "Reference or direct-fetch URLs.",
      "Incoming input_data/_new patterns.",
      "Canonical input_data path for accepted files.",
      "Script run by dina sources fetch.",
      "Explicit input_data/_new output path for fetchers.",
      "Code that transforms the canonical source in the pipeline.",
      "Human update reminders and caveats."
    ),
    stringsAsFactors = FALSE
  )
  dina_cli_cat(sprintf("%-14s %s", "field", "meaning"))
  for (i in seq_len(nrow(rows))) {
    dina_cli_cat(dina_cli_row(list(rows$field[[i]], rows$meaning[[i]]), widths = c(14L, NA), dim = c(FALSE, TRUE)))
  }
  dina_cli_cat("")
  dina_cli_cat("Views:")
  dina_cli_cat("  compact   id, source type, country, method, urls, bucket, destination, transformer, influence")
  dina_cli_cat("  workflow  acquisition, bucket, destination, transformer, task usage")
  dina_cli_cat("  paths     canonical, inbox, destination, fetch target")
  dina_cli_cat("  all       full cards")
  dina_cli_cat("")
  dina_cli_cat("Filters:")
  dina_cli_cat("  SOURCETYPE        --country ISO   --method METHOD")
  dina_cli_cat("  country ISO       method METHOD")
  dina_cli_cat("")
  dina_cli_cat("Methods:")
  for (i in seq_len(nrow(dina_source_method_glossary()))) {
    method <- dina_source_method_glossary()[i, , drop = FALSE]
    dina_cli_cat(sprintf("  %-8s %s", method$method[[1]], method$description[[1]]))
  }
  dina_cli_cat("")
  dina_cli_cat("Remember:")
  dina_cli_cat("  dina sources list workflow")
  dina_cli_cat("  dina sources list paths")
  dina_cli_cat("  dina sources list detail SOURCE --urls")
  invisible(rows)
}

dina_refresh_shorten <- function(x, width = 28L) {
  x <- x %||% ""
  if (!nzchar(x) || nchar(x) <= width) {
    return(x)
  }
  paste0(substr(x, 1L, width - 3L), "...")
}

dina_source_inbox_bucket_df <- function(buckets) {
  if (is.data.frame(buckets)) {
    if (!("files" %in% names(buckets))) buckets$files <- 0L
    return(buckets)
  }
  if (!length(buckets)) {
    return(data.frame(bucket = character(), status = character(), files = integer(), stringsAsFactors = FALSE))
  }
  rows <- lapply(buckets, function(bucket) {
    data.frame(
      bucket = bucket$bucket %||% "",
      status = bucket$status %||% "",
      files = as.integer(bucket$files %||% 0L),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

dina_source_inbox_folder_df <- function(folders) {
  if (is.data.frame(folders)) {
    if (!("files" %in% names(folders))) folders$files <- 0L
    return(folders)
  }
  if (!length(folders)) {
    return(data.frame(folder = character(), bucket = character(), status = character(), files = integer(), stringsAsFactors = FALSE))
  }
  rows <- lapply(folders, function(folder) {
    data.frame(
      folder = folder$folder %||% "",
      bucket = folder$bucket %||% "",
      status = folder$status %||% "",
      files = as.integer(folder$files %||% 0L),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

dina_source_inbox_bucket_file_count <- function(buckets) {
  buckets <- dina_source_inbox_bucket_df(buckets)
  if (!nrow(buckets)) {
    return(0L)
  }
  sum(as.integer(buckets$files %||% 0L), na.rm = TRUE)
}

dina_print_source_inbox_next_steps <- function(missing = FALSE, url_hint = TRUE) {
  if (isTRUE(missing)) {
    dina_cli_alert("Create missing buckets: dina sources inbox init")
  }
  if (isTRUE(url_hint)) {
    dina_cli_alert("More detail: dina sources list guide")
    dina_cli_alert("URLs: dina sources list urls")
    dina_cli_alert("Workflow: dina sources list workflow")
    dina_cli_alert("Fetch supported public files: dina sources fetch --dry-run")
  }
  dina_cli_alert("Drop manual downloads into the matching bucket shown above.")
  dina_cli_alert("No source validation/preparation workflow is implemented yet.")
}

dina_source_inbox_expected_text <- function(row) {
  expected <- row$expected_files %||% row$examples %||% ""
  if (!nzchar(expected)) {
    expected <- "(configured inbox pattern)"
  }
  expected
}

dina_print_source_inbox_expected_by_bucket <- function(bucket_paths, root = dina_repo_root(), rows = NULL) {
  if (is.null(rows)) {
    rows <- dina_sources_inbox_guide_rows(root)
  }
  if (!nrow(rows)) {
    return(invisible(rows))
  }
  rows <- rows[rows$bucket %in% bucket_paths, , drop = FALSE]
  if (!nrow(rows)) {
    return(invisible(rows))
  }
  dina_cli_cat("")
  dina_cli_cat("Expected files by bucket:")
  for (bucket in unique(rows$bucket)) {
    dina_cli_cat(sprintf("  %s", dina_cli_name(bucket)))
    bucket_rows <- rows[rows$bucket == bucket, , drop = FALSE]
    for (i in seq_len(nrow(bucket_rows))) {
      urls <- if (bucket_rows$url_count[[i]] > 0L) sprintf(" [%s]", bucket_rows$url_refs[[i]]) else ""
      folder <- bucket_rows$folders[[i]]
      folder_note <- if (nzchar(folder) && !identical(folder, bucket)) sprintf(" -> %s", folder) else ""
      dina_cli_cat(sprintf(
        "    - %s: %s%s%s",
        dina_cli_name(bucket_rows$source_id[[i]]),
        dina_cli_dim(dina_refresh_shorten(dina_source_inbox_expected_text(bucket_rows[i, , drop = FALSE]), 82L)),
        dina_cli_dim(dina_refresh_shorten(folder_note, 45L)),
        dina_cli_dim(urls)
      ))
    }
  }
  invisible(rows)
}

dina_print_source_inbox_bucket_summary <- function(buckets, folders = NULL, title = "Source Inbox Buckets", guidance = TRUE, root = dina_repo_root(), expected = FALSE) {
  buckets <- dina_source_inbox_bucket_df(buckets)
  folders <- dina_source_inbox_folder_df(folders %||% list())
  dina_cli_header(title)
  if (!nrow(buckets)) {
    dina_cli_warn("No configured source inbox buckets.")
    return(invisible(buckets))
  }
  dina_cli_cat(sprintf("%-34s %-12s %s", "bucket", "status", "files"))
  for (i in seq_len(nrow(buckets))) {
    dina_cli_cat(sprintf(
      "%-34s %-12s %s",
      dina_cli_name(buckets$bucket[[i]]),
      dina_cli_dim(buckets$status[[i]]),
      dina_cli_dim(as.integer(buckets$files[[i]] %||% 0L))
    ))
  }
  if (nrow(folders)) {
    dina_cli_cat("")
    dina_cli_cat(sprintf("%-44s %-12s %s", "folder", "status", "files"))
    for (i in seq_len(nrow(folders))) {
      dina_cli_cat(sprintf(
        "%-44s %-12s %s",
        dina_cli_name(folders$folder[[i]]),
        dina_cli_dim(folders$status[[i]]),
        dina_cli_dim(as.integer(folders$files[[i]] %||% 0L))
      ))
    }
  }
  if (isTRUE(expected)) {
    dina_print_source_inbox_expected_by_bucket(buckets$bucket, root = root)
  }
  if (isTRUE(guidance)) {
    dina_print_source_inbox_next_steps(missing = any(buckets$status %in% c("missing", "would_create")))
  }
  invisible(buckets)
}

dina_bucket_rows_for_args <- function(root, flags = list()) {
  family <- flags$family %||% NULL
  rows <- dina_sources_inbox_guide_rows(root, family = family)
  positional <- flags$positional %||% character()
  selector <- if (length(positional)) positional[[1]] else ""
  if (!nzchar(selector)) {
    return(rows)
  }
  if (selector %in% rows$bucket) {
    return(rows[rows$bucket == selector, , drop = FALSE])
  }
  bucket_tail <- basename(rows$bucket)
  keep <- rows$family == selector | rows$source_id == selector | bucket_tail == selector
  rows[keep, , drop = FALSE]
}

dina_print_source_inbox_guide <- function(rows, root = dina_repo_root(), include_urls = FALSE) {
  dina_cli_header(if (isTRUE(include_urls)) "Source Bucket URLs" else "Source Bucket Details")
  if (!nrow(rows)) {
    dina_cli_warn("No source bucket details matched.")
    return(invisible(rows))
  }
  bucket_paths <- unique(rows$bucket)
  bucket_rows <- data.frame(
    bucket = bucket_paths,
    bucket_exists = vapply(bucket_paths, function(bucket) any(vapply(rows$bucket_exists[rows$bucket == bucket], isTRUE, logical(1))), logical(1)),
    folder_exists = vapply(bucket_paths, function(bucket) any(vapply(rows$folder_exists[rows$bucket == bucket], isTRUE, logical(1))), logical(1)),
    types = vapply(bucket_paths, function(bucket) paste(sort(unique(vapply(rows$family[rows$bucket == bucket], dina_source_public_family_for_internal, character(1)))), collapse = ", "), character(1)),
    stringsAsFactors = FALSE
  )
  dina_cli_cat(sprintf("%-32s %-8s %s", "bucket", "state", "types"))
  for (i in seq_len(nrow(bucket_rows))) {
    dina_cli_cat(sprintf(
      "%-32s %-8s %s",
      dina_cli_name(bucket_rows$bucket[[i]]),
      dina_cli_dim(if (isTRUE(bucket_rows$folder_exists[[i]])) "exists" else "missing"),
      dina_cli_dim(bucket_rows$types[[i]])
    ))
  }

  dina_cli_cat("")
  dina_print_source_inbox_expected_by_bucket(bucket_rows$bucket, root = root, rows = rows)

  dina_cli_cat("")
  dina_cli_cat(sprintf("%-24s %-14s %-30s %-30s %s", "source", "method", "expected files", "folder", "urls"))
  for (i in seq_len(nrow(rows))) {
    url_label <- if (isTRUE(include_urls)) {
      if (rows$url_count[[i]] > 0L) "expanded below" else "none"
    } else if (rows$url_count[[i]] == 1L) {
      dina_refresh_shorten(rows$primary_url[[i]], 24L)
    } else {
      rows$url_refs[[i]]
    }
    dina_cli_cat(sprintf(
      "%-24s %-14s %-30s %-30s %s",
      dina_cli_name(rows$source_id[[i]]),
      dina_cli_dim(rows$method[[i]]),
      dina_cli_dim(dina_refresh_shorten(rows$expected_files[[i]], 30L)),
      dina_cli_dim(dina_refresh_shorten(rows$folders[[i]], 30L)),
      dina_cli_dim(url_label)
    ))
    destination <- if (identical(rows$destination_status[[i]], "ready")) rows$destination[[i]] else rows$destination_status[[i]]
    if (nzchar(destination)) {
      dina_cli_cat(sprintf("  %s %s", dina_cli_dim("destination:"), dina_cli_dim(destination)))
    }
  }

  if (isTRUE(include_urls)) {
    dina_cli_cat("")
    dina_cli_cat("URLs:")
    for (id in rows$source_id) {
      source <- dina_source_by_id(id, root)
      entries <- dina_source_url_entries(source)
      if (!nrow(entries)) {
        next
      }
      dina_cli_cat(sprintf("  %s", dina_cli_name(id)))
      for (j in seq_len(nrow(entries))) {
        label <- entries$label[[j]]
        prefix <- if (nzchar(label)) sprintf("    - %s: ", label) else "    - "
        dina_cli_cat(dina_cli_dim(sprintf("%s%s", prefix, entries$url[[j]])))
      }
    }
  } else {
    dina_cli_alert("URLs: dina sources list urls")
  }
  dina_print_source_inbox_next_steps(
    missing = any(!vapply(bucket_rows$folder_exists, isTRUE, logical(1))),
    url_hint = FALSE
  )
  invisible(rows)
}

dina_print_source_inbox_init <- function(result, root = dina_repo_root()) {
  title <- if (isTRUE(result$dry_run)) "Source Inbox Init Preview" else "Source Inbox Init"
  dina_cli_header(title)
  buckets <- result$buckets
  folders <- result$folders %||% data.frame(folder = character(), bucket = character(), status = character(), files = integer(), stringsAsFactors = FALSE)
  if (nrow(buckets)) {
    dina_cli_cat(sprintf("%-34s %-12s %s", "bucket", "status", "files"))
    for (i in seq_len(nrow(buckets))) {
      dina_cli_cat(sprintf(
        "%-34s %-12s %s",
        dina_cli_name(buckets$bucket[[i]]),
        dina_cli_dim(buckets$status[[i]]),
        dina_cli_dim(as.integer(buckets$files[[i]] %||% 0L))
      ))
    }
  }
  if (nrow(folders)) {
    dina_cli_cat("")
    dina_cli_cat(sprintf("%-44s %-12s %s", "folder", "status", "files"))
    for (i in seq_len(nrow(folders))) {
      dina_cli_cat(sprintf(
        "%-44s %-12s %s",
        dina_cli_name(folders$folder[[i]]),
        dina_cli_dim(folders$status[[i]]),
        dina_cli_dim(as.integer(folders$files[[i]] %||% 0L))
      ))
    }
  }
  migrations <- result$migrations
  dina_cli_cat("")
  if (!nrow(migrations)) {
    dina_cli_alert("No legacy colocated `_new` files matched for copying.")
  } else {
    dina_cli_cat("Legacy colocated _new files:")
    dina_cli_cat(sprintf("%-24s %-34s %-34s %s", "source", "from", "to", "status"))
    for (i in seq_len(nrow(migrations))) {
      dina_cli_cat(sprintf(
        "%-24s %-34s %-34s %s",
        migrations$source_id[[i]],
        dina_refresh_shorten(migrations$from[[i]], 34L),
        dina_refresh_shorten(migrations$to[[i]], 34L),
        migrations$status[[i]]
      ))
    }
  }
  if (isTRUE(result$dry_run)) {
    dina_cli_ok("Dry-run only: no folders were created and no files were copied.")
  }
  dina_print_source_inbox_next_steps(missing = any(c(buckets$status, folders$status) %in% c("would_create")))
  invisible(result)
}

dina_print_bucket_uses <- function(rows) {
  dina_cli_header("Bucket Uses")
  if (!nrow(rows)) {
    dina_cli_warn("No bucket/source usage rows matched.")
    return(invisible(rows))
  }
  for (i in seq_len(nrow(rows))) {
    dina_cli_cat(sprintf("%s %s", dina_cli_name(rows$source_id[[i]]), dina_cli_dim(sprintf("[%s]", rows$family[[i]]))))
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("folder:"), dina_cli_dim(rows$folders[[i]])))
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("expected:"), dina_cli_dim(rows$expected_files[[i]])))
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("destination:"), dina_cli_dim(rows$destination[[i]])))
    dina_cli_cat(sprintf("  %s %s", dina_cli_dim("uses:"), dina_cli_dim(rows$users[[i]])))
  }
  invisible(rows)
}

dina_print_bucket_fetch_results <- function(rows, dry_run = FALSE, show_skipped = FALSE, title = NULL) {
  dina_cli_header(title %||% if (isTRUE(dry_run)) "Source Fetch Preview" else "Source Fetch")
  if (!nrow(rows)) {
    dina_cli_warn("No bucket fetch rows matched.")
    return(invisible(rows))
  }
  statuses <- as.character(rows$status)
  fetch_statuses <- c("would_fetch", "fetched", "already_present", "failed")
  fetch_rows <- rows[statuses %in% fetch_statuses, , drop = FALSE]
  skipped_rows <- rows[!(statuses %in% fetch_statuses), , drop = FALSE]
  skipped_count <- nrow(skipped_rows)
  if (!nrow(fetch_rows)) {
    dina_cli_warn("No automatic fetches matched.")
  }
  for (i in seq_len(nrow(fetch_rows))) {
    status <- fetch_rows$status[[i]]
    label <- switch(
      status,
      would_fetch = "would fetch",
      already_present = "already present",
      fetched = "fetched",
      failed = "failed",
      status
    )
    line <- sprintf("%s %s", dina_cli_name(fetch_rows$source_id[[i]]), dina_cli_dim(label))
    if (identical(status, "failed")) {
      dina_cli_warn(line)
    } else if (identical(status, "fetched")) {
      dina_cli_ok(line)
    } else {
      dina_cli_cat(line)
    }
    if (nzchar(fetch_rows$target[[i]] %||% "")) {
      dina_cli_cat(sprintf("  %s %s", dina_cli_dim("target:"), dina_cli_dim(fetch_rows$target[[i]])))
    }
    detail <- fetch_rows$detail[[i]] %||% ""
    show_detail <- identical(status, "failed") || identical(status, "fetched")
    if (isTRUE(show_detail) && nzchar(detail)) {
      dina_cli_cat(sprintf("  %s %s", dina_cli_dim("detail:"), dina_cli_dim(dina_refresh_shorten(detail, 96L))))
    }
  }
  if (skipped_count > 0L && (isTRUE(show_skipped) || !nrow(fetch_rows))) {
    for (i in seq_len(nrow(skipped_rows))) {
      status <- gsub("_", " ", skipped_rows$status[[i]] %||% "")
      dina_cli_cat(sprintf("%s %s", dina_cli_name(skipped_rows$source_id[[i]]), dina_cli_dim(status)))
      detail <- skipped_rows$detail[[i]] %||% ""
      if (nzchar(detail)) {
        dina_cli_cat(sprintf("  %s %s", dina_cli_dim("detail:"), dina_cli_dim(dina_refresh_shorten(detail, 96L))))
      }
    }
  } else if (skipped_count > 0L) {
    dina_cli_alert(sprintf(
      "Skipped %s source(s) without automatic fetches. Use `dina sources list guide` for manual instructions.",
      skipped_count
    ))
  }
  if (isTRUE(dry_run)) {
    dina_cli_ok("Dry-run only: no files were written.")
  }
  if (any(fetch_rows$status %in% c("fetched", "already_present", "would_fetch"))) {
    dina_cli_alert("Fetched files land in input_data/_new. No source validation/preparation workflow is implemented yet.")
  }
  invisible(rows)
}

dina_stop_retired_source_workflow <- function(command) {
  stop(
    sprintf(
      "`dina sources %s` is retired. The old source review/integration workflow has been removed. No replacement data validation/preparation process is implemented yet.",
      command
    ),
    call. = FALSE
  )
}

dina_cmd_buckets <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "list")
  if (!length(args) || startsWith(sub, "-")) {
    sub <- "list"
  }
  detail_aliases <- c("detail", "details", "show")
  url_aliases <- c("url", "urls")
  uses_aliases <- c("uses", "usage", "used-by")
  fetch_aliases <- c("fetch", "get")
  if (sub %in% c("list", "status", "summary")) {
    flag_args <- if (!length(args) || startsWith(dina_arg(args, 1L, ""), "-")) args else args[-1]
    flags <- dina_parse_flags(flag_args)
    result <- dina_sources_inbox_init(root, dry_run = TRUE, migrate = FALSE)
    if (!is.null(flags$family)) {
      rows <- dina_sources_inbox_guide_rows(root, family = flags$family)
      keep <- unique(rows$bucket)
      result$buckets <- result$buckets[result$buckets$bucket %in% keep, , drop = FALSE]
    }
    dina_print_source_inbox_bucket_summary(result$buckets, folders = result$folders, title = "Source Buckets", root = root, expected = FALSE)
  } else if (sub %in% detail_aliases) {
    flags <- dina_parse_flags(args[-1])
    rows <- dina_bucket_rows_for_args(root, flags)
    dina_print_source_inbox_guide(rows, root = root, include_urls = FALSE)
  } else if (sub %in% url_aliases) {
    flags <- dina_parse_flags(args[-1])
    rows <- dina_bucket_rows_for_args(root, flags)
    dina_print_source_inbox_guide(rows, root = root, include_urls = TRUE)
  } else if (sub %in% uses_aliases) {
    flags <- dina_parse_flags(args[-1])
    selector <- if (length(flags$positional %||% character())) flags$positional[[1L]] else NULL
    rows <- dina_buckets_uses(root, family = flags$family %||% NULL, selector = selector, source_id = flags$source %||% NULL)
    dina_print_bucket_uses(rows)
  } else if (sub %in% fetch_aliases) {
    flags <- dina_parse_flags(args[-1])
    selector <- if (length(flags$positional %||% character())) flags$positional[[1L]] else NULL
    rows <- dina_buckets_fetch(
      root,
      family = flags$family %||% NULL,
      selector = selector,
      source_id = flags$source %||% NULL,
      dry_run = isTRUE(flags[["dry-run"]])
    )
    dina_print_bucket_fetch_results(
      rows,
      dry_run = isTRUE(flags[["dry-run"]]),
      show_skipped = !is.null(selector) || !is.null(flags$source)
    )
  } else {
    flags <- dina_parse_flags(args)
    rows <- dina_bucket_rows_for_args(root, flags)
    if (nrow(rows) && length(flags$positional %||% character())) {
      dina_print_source_inbox_guide(rows, root = root, include_urls = FALSE)
    } else {
      stop("Usage: dina buckets [--family FAMILY]\n       dina buckets detail [--family FAMILY]\n       dina buckets urls [--family FAMILY]\n       dina buckets uses [--family FAMILY] [BUCKET|SOURCE]\n       dina buckets fetch [--family FAMILY] [--source ID] [--dry-run]", call. = FALSE)
    }
  }
}

dina_country_sna_year_label <- function(x) {
  years <- suppressWarnings(as.integer(unlist(strsplit(as.character(x %||% ""), ",", fixed = TRUE), use.names = FALSE)))
  years <- sort(unique(years[!is.na(years)]))
  if (!length(years)) return("-")
  if (length(years) <= 4L) return(paste(years, collapse = ","))
  consecutive <- identical(years, seq.int(min(years), max(years)))
  if (consecutive) {
    sprintf("%s-%s (%s)", min(years), max(years), length(years))
  } else {
    sprintf("%s...%s (%s)", min(years), max(years), length(years))
  }
}

dina_country_sna_explore_label <- function(x) {
  switch(
    x %||% "",
    extension_found = "extension",
    overlap_only = "overlap",
    no_overlap = "new only",
    no_new_years_detected = "no new",
    no_old_years_detected = "no baseline",
    structure_evidence_available = "evidence ok",
    structure_review_needed = "review structure",
    layout_adapter_required = "adapter needed",
    no_new_years = "no new",
    no_new_files = "no files",
    include_dry_run = "include dry-run",
    include_dry_run_no_overlap = "include dry-run",
    keep_current = "keep current",
    gsub("_", " ", x %||% "", fixed = TRUE)
  )
}

dina_country_sna_explore_status_notes <- function(structure, actions) {
  notes <- character()
  if (nrow(structure) && any(structure$structure_status == "layout_adapter_required", na.rm = TRUE)) {
    notes <- c(notes, "adapter needed: layout family detected, but no deterministic adapter exists yet.")
  }
  if (nrow(structure) && any(structure$structure_status == "structure_review_needed", na.rm = TRUE)) {
    notes <- c(notes, "review structure: files/years were found, but sheet/table evidence is weak and include may warn.")
  }
  if (nrow(actions) && any(actions$action == "keep_current", na.rm = TRUE)) {
    notes <- c(notes, "no new: no incoming years found; existing canonical source is kept.")
  }
  unique(notes)
}

dina_print_country_sna_explore <- function(result, dry_run = FALSE, input = "stdin", is_terminal = isatty(stdin())) {
  summary <- result$outputs$extension_summary
  structure <- result$outputs$structure_summary
  actions <- result$outputs$review_actions
  dina_cli_header("Country-SNA Explore")
  dina_cli_alert("Experimental source workflow: inventories files, years, and layout evidence; it does not replace 01b.")
  if (!nrow(summary)) {
    dina_cli_alert("No country-SNA explorer rows were produced.")
  } else {
    counts <- table(summary$status)
    count_labels <- vapply(names(counts), dina_country_sna_explore_label, character(1))
    count_text <- paste(sprintf("%s=%s", count_labels, as.integer(counts)), collapse = ", ")
    dina_cli_alert(sprintf("Summary: %s countr%s%s", nrow(summary), if (nrow(summary) == 1L) "y" else "ies", if (nzchar(count_text)) paste0(" (", count_text, ")") else ""))
    dina_cli_cat(dina_cli_row(c("ctry", "current", "new", "ext", "overlap", "result", "structure", "action"), widths = c(5, 14, 14, 12, 14, 12, 16, 15), dim = TRUE))
    for (i in seq_len(nrow(summary))) {
      country <- summary$country[[i]]
      structure_row <- structure[structure$country == country, , drop = FALSE]
      action_row <- actions[actions$country == country, , drop = FALSE]
      dina_cli_cat(dina_cli_row(
        c(
          country,
          dina_country_sna_year_label(summary$old_years[[i]]),
          dina_country_sna_year_label(summary$new_years[[i]]),
          dina_country_sna_year_label(summary$extension_years[[i]]),
          dina_country_sna_year_label(summary$overlap_years[[i]]),
          dina_country_sna_explore_label(summary$status[[i]]),
          dina_country_sna_explore_label(if (nrow(structure_row)) structure_row$structure_status[[1L]] else "-"),
          dina_country_sna_explore_label(if (nrow(action_row)) action_row$action[[1L]] else "-")
        ),
        widths = c(5, 14, 14, 12, 14, 12, 16, 15)
      ))
    }
    notes <- dina_country_sna_explore_status_notes(structure, actions)
    if (length(notes)) {
      dina_cli_cat("")
      dina_cli_cat("Status notes:")
      for (note in notes) dina_cli_alert(note)
    }
    next_commands <- unique(actions$next_command[nzchar(actions$next_command %||% "")])
    if (length(next_commands)) {
      dina_cli_alert(sprintf("Next likely command: %s", next_commands[[1L]]))
    }
  }
  if (isTRUE(dry_run)) {
    dina_cli_alert("Dry-run only: explorer outputs were not written.")
  } else {
    dina_cli_ok(sprintf("Explorer output: %s", result$paths$root))
    dina_cli_alert("Review tables inline with `dina sources table sna year_expectations`.")
    dina_cli_alert("Low-level table/role/value candidates are developer evidence, not review actions.")
    dina_country_sna_explore_tables_menu(result$paths$root, input = input, is_terminal = is_terminal)
  }
  invisible(result)
}

dina_country_sna_table_run <- function(root, run = NULL) {
  path <- run %||% file.path(root, "output", "experiments", "country_sna_explore")
  if (!grepl("^/", path)) path <- file.path(root, path)
  normalizePath(path, mustWork = FALSE)
}

dina_country_sna_table_file <- function(root, table, run = NULL) {
  table <- gsub("-", "_", table %||% "", fixed = TRUE)
  run <- dina_country_sna_table_run(root, run)
  file.path(run, "tables", paste0(table, ".csv"))
}

dina_country_sna_read_table <- function(root, table, run = NULL) {
  path <- dina_country_sna_table_file(root, table, run)
  if (!file.exists(path)) {
    stop(sprintf("Country-SNA explore table not found: %s", path), call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

dina_country_sna_variable_expectation_summary <- function(rows) {
  if (!nrow(rows)) return(rows)
  keys <- unique(rows[, intersect(c("country", "year_role", "expected_status", "structure_status"), names(rows)), drop = FALSE])
  out <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    hit <- rep(TRUE, nrow(rows))
    for (name in names(key)) {
      value <- key[[name]][[1L]]
      hit <- hit & if (is.na(value)) is.na(rows[[name]]) else rows[[name]] == value
    }
    part <- rows[hit, , drop = FALSE]
    data.frame(
      key,
      years = length(unique(part$year)),
      variables = length(unique(part$variable)),
      rows = nrow(part),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  do.call(rbind, out)
}

dina_print_data_frame_compact <- function(rows, limit = 20L) {
  limit <- suppressWarnings(as.integer(limit %||% 20L))
  if (is.na(limit) || limit < 1L) limit <- 20L
  if (!nrow(rows)) {
    dina_cli_alert("No rows.")
    return(invisible(rows))
  }
  shown <- utils::head(rows, limit)
  shown[] <- lapply(shown, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "-"
    ifelse(nchar(x) > 34L, paste0(substr(x, 1L, 31L), "..."), x)
  })
  widths <- vapply(names(shown), nchar, integer(1))
  for (name in names(shown)) {
    widths[[name]] <- max(widths[[name]], max(nchar(shown[[name]]), na.rm = TRUE), 4L)
  }
  widths <- pmin(widths, 34L)
  dina_cli_cat(dina_cli_row(names(shown), widths = widths, dim = TRUE))
  for (i in seq_len(nrow(shown))) {
    dina_cli_cat(dina_cli_row(as.list(shown[i, , drop = FALSE]), widths = widths))
  }
  if (nrow(rows) > nrow(shown)) {
    dina_cli_alert(sprintf("Showing %s of %s rows. Use --limit N for more.", nrow(shown), nrow(rows)))
  }
  invisible(rows)
}

dina_print_country_sna_table <- function(root, table, run = NULL, country = NULL, limit = 20L) {
  table <- gsub("-", "_", table %||% "", fixed = TRUE)
  rows <- dina_country_sna_read_table(root, table, run)
  if (!is.null(country) && "country" %in% names(rows)) {
    rows <- rows[toupper(rows$country) == toupper(country), , drop = FALSE]
  }
  display <- rows
  title <- table
  if (identical(table, "variable_expectations")) {
    display <- dina_country_sna_variable_expectation_summary(rows)
    title <- "variable_expectations summary"
  }
  dina_cli_header(sprintf("Country-SNA Table: %s", title))
  dina_cli_alert(sprintf("Run: %s", dina_country_sna_table_run(root, run)))
  dina_print_data_frame_compact(display, limit = limit)
  invisible(display)
}

dina_country_sna_explore_tables_menu <- function(run, input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) return(invisible(NULL))
  actions <- list(
    dina_menu_action("year_expectations", "Year expectations", value = "year_expectations"),
    dina_menu_action("variable_expectations", "Variable expectation summary", value = "variable_expectations"),
    dina_menu_action("extension_summary", "Extension summary", value = "extension_summary"),
    dina_menu_action("source_inventory", "Source inventory", value = "source_inventory"),
    dina_menu_action("structure_summary", "Structure summary", value = "structure_summary"),
    dina_menu_action("quit", "Quit", value = "quit")
  )
  selected <- dina_menu_select(
    "Explore Tables",
    actions,
    prompt = "",
    default = "quit",
    allow_quit = TRUE,
    input = input,
    is_terminal = is_terminal
  )
  if (is.null(selected) || identical(selected, "quit")) return(invisible(NULL))
  dina_print_country_sna_table(dina_repo_root(), selected, run = run, limit = 20L)
  invisible(selected)
}

dina_print_country_sna_include <- function(result, mode = "dry_run") {
  summary <- result$outputs$include_summary
  manifest <- result$outputs$include_manifest
  status <- if (nrow(manifest)) manifest$value[manifest$key == "status"][[1L]] else "check_following"
  dina_cli_header("Country-SNA Include")
  dina_cli_alert("Experimental source workflow: deterministic expectation checks only; pipeline outputs are not replaced.")
  dina_cli_cat(sprintf("Overall status: %s", status))
  if (!nrow(summary)) {
    dina_cli_alert("No explorer expectations were found. Run `dina sources explore sna` first.")
  } else {
    dina_cli_cat(dina_cli_row(c("country", "status", "expected", "found", "missing/warn", "blocked"), widths = c(8, 18, 10, 10, 14, 10), dim = TRUE))
    for (i in seq_len(nrow(summary))) {
      dina_cli_cat(dina_cli_row(
        c(
          summary$country[[i]],
          summary$status[[i]],
          summary$expected_values[[i]],
          summary$found_values[[i]],
          summary$warnings[[i]],
          summary$blocked[[i]]
        ),
        widths = c(8, 18, 10, 10, 14, 10)
      ))
    }
  }
  dina_cli_ok(sprintf("Include dry-run output: %s", result$paths$root))
  dina_cli_alert("No production files changed. Fix code/contract and rerun safely.")
  dina_cli_alert(sprintf("Confirm after a clean run: dina sources include sna --confirm --include-run %s", result$paths$root))
  dina_cli_alert("Safe retry: before confirm, edit code/contract and rerun explore/include. After confirm, restore first, then rerun.")
  dina_cli_alert("Use git for code/config rollback; source data rollback uses the confirm backup snapshot.")
  invisible(result)
}

dina_print_country_sna_confirm <- function(result) {
  report <- result$outputs$promote_report
  dina_cli_header("Country-SNA Include Confirm")
  if (nrow(report)) {
    dina_print_data_frame_compact(report, limit = 20L)
  }
  dina_cli_ok(sprintf("Confirm run: %s", result$paths$root))
  dina_cli_alert("No pipeline was run. Next likely command: dina run 01b --dry-run")
  dina_cli_alert(sprintf("Rollback: dina sources include sna --restore %s", result$paths$root))
  invisible(result)
}

dina_print_country_sna_restore <- function(result) {
  report <- result$outputs$restore_report
  dina_cli_header("Country-SNA Include Restore")
  dina_print_data_frame_compact(report, limit = 20L)
  dina_cli_ok(sprintf("Restore report: %s", result$paths$restore_report))
  dina_cli_alert("Manual next steps: inspect sources, edit code/contract if needed, then rerun explore/include/confirm.")
  invisible(result)
}

dina_admin_pit_label <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  gsub("_", " ", x, fixed = TRUE)
}

dina_admin_pit_year_label <- function(x, max_chars = 34L) {
  x <- trimws(as.character(x %||% ""))
  x[is.na(x) | !nzchar(x)] <- "-"
  years <- suppressWarnings(as.integer(unlist(strsplit(x[[1L]], "[^0-9]+"))))
  years <- sort(unique(years[!is.na(years) & years >= 1900L & years <= 2100L]))
  if (!length(years)) {
    label <- x[[1L]]
    if (nchar(label) > max_chars) label <- paste0(substr(label, 1L, max_chars - 3L), "...")
    return(label)
  }
  breaks <- c(TRUE, diff(years) != 1L)
  groups <- split(years, cumsum(breaks))
  spans <- vapply(groups, function(group) {
    if (length(group) == 1L) {
      as.character(group[[1L]])
    } else {
      sprintf("%s-%s", group[[1L]], group[[length(group)]])
    }
  }, character(1))
  label <- paste(spans, collapse = ",")
  if (length(years) > 1L) {
    label <- sprintf("%s (%sy)", label, length(years))
  }
  if (nchar(label) > max_chars) {
    label <- sprintf("%s...%s (%sy)", years[[1L]], years[[length(years)]], length(years))
  }
  label
}

dina_print_admin_pit_explore <- function(result, dry_run = FALSE) {
  summary <- result$outputs$extension_summary
  structure <- result$outputs$structure_summary
  actions <- result$outputs$review_actions
  manifest <- result$manifest
  status <- if (nrow(manifest)) manifest$value[manifest$key == "status"][[1L]] else "check_following"
  dina_cli_header("PIT Admin Explore")
  dina_cli_cat(sprintf("Overall status: %s", status))
  if (!nrow(summary)) {
    dina_cli_alert("No supported PIT admin source rows were found.")
  } else {
    dina_cli_cat(dina_cli_row(c("source", "ctry", "current", "incoming", "extension", "result"), widths = c(18, 5, 22, 22, 18, 16), dim = TRUE))
    for (i in seq_len(nrow(summary))) {
      structure_row <- structure[structure$source_id == summary$source_id[[i]] & structure$country == summary$country[[i]], , drop = FALSE]
      action_row <- actions[actions$source_id == summary$source_id[[i]] & actions$country == summary$country[[i]], , drop = FALSE]
      dina_cli_cat(dina_cli_row(
        c(
          summary$source_id[[i]],
          summary$country[[i]],
          dina_admin_pit_year_label(summary$old_years[[i]], max_chars = 22L),
          dina_admin_pit_year_label(summary$new_years[[i]], max_chars = 22L),
          dina_admin_pit_year_label(summary$extension_years[[i]], max_chars = 18L),
          dina_admin_pit_label(summary$status[[i]])
        ),
        widths = c(18, 5, 22, 22, 18, 16)
      ))
      dina_cli_cat(dina_cli_dim(sprintf(
        "  review: structure=%s; action=%s",
        dina_admin_pit_label(if (nrow(structure_row)) structure_row$structure_status[[1L]] else "-"),
        dina_admin_pit_label(if (nrow(action_row)) action_row$action[[1L]] else "-")
      )))
    }
  }
  deps <- result$outputs$aux_dependency_summary %||% data.frame()
  static <- result$outputs$static_dependency_report %||% data.frame()
  static_attention <- if (nrow(static)) static[static$severity %in% c("blocked", "warning"), , drop = FALSE] else data.frame()
  if (nrow(deps) || nrow(static_attention)) {
    dina_cli_cat("")
    dina_cli_cat("Dependency status:")
    dina_cli_cat(dina_cli_row(c("source", "country", "dependency", "class", "status", "next"), widths = c(18, 8, 18, 18, 28, 36), dim = TRUE))
    if (nrow(deps)) {
      for (i in seq_len(nrow(deps))) {
        dina_cli_cat(dina_cli_row(
          c(
            deps$source_id[[i]],
            deps$country[[i]],
            deps$dependency_id[[i]],
            deps$artifact_class[[i]],
            dina_admin_pit_label(deps$status[[i]]),
            dina_refresh_shorten(deps$next_command[[i]] %||% "", 36L)
          ),
          widths = c(18, 8, 18, 18, 28, 36)
        ))
      }
    }
    if (nrow(static_attention)) {
      static_keys <- unique(static_attention[, c("source_id", "country", "dependency_id", "artifact_class", "status", "next_command"), drop = FALSE])
      for (i in seq_len(nrow(static_keys))) {
        dina_cli_cat(dina_cli_row(
          c(
            static_keys$source_id[[i]],
            static_keys$country[[i]],
            dina_refresh_shorten(static_keys$dependency_id[[i]], 18L),
            static_keys$artifact_class[[i]],
            dina_admin_pit_label(static_keys$status[[i]]),
            dina_refresh_shorten(static_keys$next_command[[i]] %||% "", 36L)
          ),
          widths = c(18, 8, 18, 18, 28, 36)
        ))
      }
    }
  }
  if (isTRUE(dry_run)) {
    dina_cli_alert("Dry-run only: review tables were not written.")
  } else {
    dina_cli_ok(sprintf("Explore output: %s", result$paths$root))
    dina_cli_alert("List review tables with `dina sources table admin`; preview one with `dina sources table admin TABLE`.")
    dina_cli_alert("Common tables: extension_summary, year_expectations, structure_summary, source_inventory, aux_dependency_summary, dependency_actions.")
  }
  invisible(result)
}

dina_admin_pit_table_run <- function(root, run = NULL) {
  path <- run %||% file.path(root, "output", "experiments", "admin_pit_explore")
  normalizePath(if (grepl("^/", path)) path else file.path(root, path), mustWork = FALSE)
}

dina_admin_pit_table_catalog <- function() {
  data.frame(
    table = c(
      "extension_summary",
      "year_expectations",
      "structure_summary",
      "source_inventory",
      "review_actions",
      "unsupported_sources",
      "source_fingerprints",
      "explore_manifest",
      "include_summary",
      "include_detail",
      "staged_source_mappings",
      "static_dependency_report",
      "aux_dependency_report",
      "aux_validation_report",
      "aux_dependency_summary",
      "aux_dependency_detail",
      "dependency_actions",
      "cleaner_summary",
      "cleaner_outputs",
      "promotion_plan",
      "promotion_fingerprints",
      "include_manifest"
    ),
    contents = c(
      "old/new coverage, extension and overlap years",
      "candidate file-year rows used by include",
      "shallow country-specific structure checks",
      "matched old/new files, destinations, and parsed years",
      "review decision and next command per source",
      "admin-family sources outside PIT v1",
      "hashes and metadata for matched files",
      "explore run metadata and effective config status",
      "per-source include status",
      "per-year source staging checks",
      "raw source copy plan into staged repo",
      "carried-forward or missing static inputs",
      "staged, carried-forward, or missing aux inputs",
      "semantic aux validation, including Brazil minimum wage overlap checks",
      "compact aux dependency status by source",
      "explore-time aux dependency inventory",
      "suggested next command for blocked dependencies",
      "candidate cleaner status and reasons",
      "generated candidate clean output files",
      "artifacts eligible for confirm promotion",
      "hashes for staged promotion artifacts",
      "include run metadata and status"
    ),
    stringsAsFactors = FALSE
  )
}

dina_admin_pit_table_available <- function(root, run = NULL) {
  run <- dina_admin_pit_table_run(root, run)
  table_dir <- file.path(run, "tables")
  if (!dir.exists(table_dir)) return(character())
  tools::file_path_sans_ext(basename(Sys.glob(file.path(table_dir, "*.csv"))))
}

dina_print_admin_pit_tables <- function(root, run = NULL) {
  run_path <- dina_admin_pit_table_run(root, run)
  catalog <- dina_admin_pit_table_catalog()
  available <- dina_admin_pit_table_available(root, run)
  catalog$available <- ifelse(catalog$table %in% available, "yes", "no")
  dina_cli_header("PIT Admin Tables")
  dina_cli_alert(sprintf("Run: %s", run_path))
  if (!length(available)) {
    dina_cli_warn("No admin PIT tables found yet. Run `dina sources explore admin` first, or pass --run PATH.")
  }
  dina_print_data_frame_compact(catalog, limit = nrow(catalog))
  if (!is.null(run)) {
    dina_cli_alert(sprintf("Preview one table with `dina sources table admin TABLE --run %s`.", run_path))
  } else {
    dina_cli_alert("Preview one table with `dina sources table admin TABLE`.")
  }
  invisible(catalog)
}

dina_admin_pit_table_file <- function(root, table, run = NULL) {
  table <- gsub("-", "_", table %||% "", fixed = TRUE)
  run <- dina_admin_pit_table_run(root, run)
  candidates <- c(
    file.path(run, "tables", paste0(table, ".csv")),
    file.path(root, "output", "experiments", "admin_pit_include", "runs", basename(run), "tables", paste0(table, ".csv"))
  )
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop("Admin PIT table not found: ", table, call. = FALSE)
  }
  hit[[1L]]
}

dina_admin_pit_read_table <- function(root, table, run = NULL) {
  utils::read.csv(dina_admin_pit_table_file(root, table, run), stringsAsFactors = FALSE)
}

dina_print_admin_pit_table <- function(root, table, run = NULL, country = NULL, limit = 20L) {
  table <- gsub("-", "_", table %||% "", fixed = TRUE)
  rows <- dina_admin_pit_read_table(root, table, run)
  if (!is.null(country) && "country" %in% names(rows)) {
    rows <- rows[toupper(rows$country) == toupper(country), , drop = FALSE]
  }
  dina_cli_header(sprintf("PIT Admin Table: %s", table))
  dina_cli_alert(sprintf("Run: %s", dina_admin_pit_table_run(root, run)))
  dina_print_data_frame_compact(rows, limit = limit)
  invisible(rows)
}

dina_admin_pit_bind_rows <- function(rows) {
  rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, rows)
  if (!length(rows)) return(data.frame(stringsAsFactors = FALSE))
  cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(cols, names(x))
    for (col in missing) x[[col]] <- NA
    x[cols]
  })
  do.call(rbind, rows)
}

dina_admin_pit_blocker_rows <- function(result) {
  outputs <- result$outputs
  rows <- list()
  detail <- outputs$include_detail %||% data.frame()
  if (nrow(detail)) {
    blocked <- detail[grepl("^blocked", detail$status %||% ""), , drop = FALSE]
    if (nrow(blocked)) {
      keys <- unique(blocked[, c("source_id", "country", "status", "reason"), drop = FALSE])
      rows[[length(rows) + 1L]] <- data.frame(
        source = keys$source_id,
        country = keys$country,
        blocker = dina_admin_pit_label(keys$status),
        detail = keys$reason,
        stringsAsFactors = FALSE
      )
    }
  }
  static <- outputs$static_dependency_report %||% data.frame()
  if (nrow(static)) {
    blocked <- static[static$severity == "blocked", , drop = FALSE]
    if (nrow(blocked)) {
      rows[[length(rows) + 1L]] <- data.frame(
        source = blocked$source_id,
        country = blocked$country,
        blocker = "missing static input",
        detail = blocked$from_rel,
        stringsAsFactors = FALSE
      )
    }
  }
  aux <- outputs$aux_dependency_report %||% data.frame()
  if (nrow(aux)) {
    blocked <- aux[aux$severity == "blocked", , drop = FALSE]
    aux_validation <- outputs$aux_validation_report %||% data.frame()
    if (nrow(blocked) && nrow(aux_validation)) {
      validation_keys <- paste(
        aux_validation$source_id[aux_validation$severity == "blocked"],
        aux_validation$country[aux_validation$severity == "blocked"],
        aux_validation$dependency_id[aux_validation$severity == "blocked"],
        sep = "\r"
      )
      blocked_keys <- paste(blocked$source_id, blocked$country, blocked$dependency_id, sep = "\r")
      blocked <- blocked[!(blocked_keys %in% validation_keys), , drop = FALSE]
    }
    if (nrow(blocked)) {
      commands <- vapply(blocked$dependency_id, function(id) {
        if (identical(id, "bra-minwage")) "dina sources fetch bra-minwage" else ""
      }, character(1))
      detail <- ifelse(nzchar(commands), paste0(blocked$dependency_id, ": missing. Run: ", commands), paste0(blocked$dependency_id, ": not found in _new or canonical paths"))
      rows[[length(rows) + 1L]] <- data.frame(
        source = blocked$source_id,
        country = blocked$country,
        blocker = "missing aux input",
        detail = detail,
        stringsAsFactors = FALSE
      )
    }
  }
  aux_validation <- outputs$aux_validation_report %||% data.frame()
  if (nrow(aux_validation)) {
    blocked <- aux_validation[aux_validation$severity == "blocked", , drop = FALSE]
    if (nrow(blocked)) {
      detail <- blocked$detail
      detail[!nzchar(detail %||% "")] <- blocked$status[!nzchar(detail %||% "")]
      detail <- ifelse(nzchar(blocked$next_command %||% ""), paste0(blocked$dependency_id, ": missing. Run: ", blocked$next_command), detail)
      rows[[length(rows) + 1L]] <- data.frame(
        source = blocked$source_id,
        country = blocked$country,
        blocker = dina_admin_pit_label(blocked$status),
        detail = detail,
        stringsAsFactors = FALSE
      )
    }
  }
  cleaner <- outputs$cleaner_summary %||% data.frame()
  if (nrow(cleaner)) {
    blocked <- cleaner[cleaner$cleaner_status == "blocked", , drop = FALSE]
    blocked <- blocked[!grepl("^blocked_dependency", blocked$reason %||% "") & !((blocked$reason %||% "") %in% "source_checks_blocked"), , drop = FALSE]
    if (nrow(blocked)) {
      detail <- blocked$reason
      detail[!nzchar(detail %||% "")] <- sprintf("missing %s expected clean output(s)", blocked$missing_outputs[!nzchar(detail %||% "")])
      rows[[length(rows) + 1L]] <- data.frame(
        source = blocked$source_id,
        country = blocked$country,
        blocker = "cleaner blocked",
        detail = detail,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- dina_admin_pit_bind_rows(rows)
  if (!nrow(out)) return(out)
  unique(out)
}

dina_print_admin_pit_blockers <- function(result, limit = 10L) {
  blockers <- dina_admin_pit_blocker_rows(result)
  if (!nrow(blockers)) return(invisible(blockers))
  shown <- utils::head(blockers, limit)
  dina_cli_cat("")
  dina_cli_cat("Blocking items:")
  dina_cli_cat(dina_cli_row(c("source", "country", "blocker", "detail"), widths = c(18, 8, 26, 72), dim = TRUE))
  for (i in seq_len(nrow(shown))) {
    dina_cli_cat(dina_cli_row(
      c(
        shown$source[[i]],
        shown$country[[i]],
        shown$blocker[[i]],
        dina_refresh_shorten(shown$detail[[i]], 72L)
      ),
      widths = c(18, 8, 26, 72)
    ))
  }
  if (nrow(blockers) > nrow(shown)) {
    dina_cli_alert(sprintf("Showing %s of %s blocking items. Inspect static_dependency_report, aux_dependency_report, aux_validation_report, and cleaner_summary for the full list.", nrow(shown), nrow(blockers)))
  }
  invisible(blockers)
}

dina_print_admin_pit_include <- function(result) {
  summary <- result$outputs$include_summary
  cleaner_summary <- result$outputs$cleaner_summary %||% data.frame()
  manifest <- result$manifest
  status <- if (nrow(manifest)) manifest$value[manifest$key == "status"][[1L]] else "check_following"
  dina_cli_header("PIT Admin Include")
  dina_cli_cat(sprintf("Overall status: %s", status))
  if (!nrow(summary)) {
    dina_cli_alert("No explorer expectations were found. Run `dina sources explore admin` first.")
  } else {
    dina_cli_cat(dina_cli_row(c("source", "country", "status", "years", "staged", "clean", "warnings", "blocked"), widths = c(18, 8, 16, 8, 8, 8, 10, 10), dim = TRUE))
    for (i in seq_len(nrow(summary))) {
      dina_cli_cat(dina_cli_row(
        c(
          summary$source_id[[i]],
          summary$country[[i]],
          summary$status[[i]],
          summary$expected_years[[i]],
          summary$staged_sources[[i]],
          if ("clean_outputs" %in% names(summary)) summary$clean_outputs[[i]] else 0L,
          summary$warnings[[i]],
          summary$blocked[[i]]
        ),
        widths = c(18, 8, 16, 8, 8, 8, 10, 10)
      ))
    }
  }
  if (nrow(cleaner_summary)) {
    blocked <- cleaner_summary[cleaner_summary$cleaner_status == "blocked", , drop = FALSE]
    if (nrow(blocked)) {
      dina_cli_warn(sprintf("%s admin cleaner%s blocked. Inspect `dina sources table admin cleaner_summary --run %s`.", nrow(blocked), if (nrow(blocked) == 1L) "" else "s", result$paths$root))
      dina_cli_alert("Dependency defaults: primary PIT files come from _new; static inputs are carried from canonical paths; aux inputs prefer _new and fall back to canonical.")
      dina_print_admin_pit_blockers(result)
    } else {
      dina_cli_alert("Cleaner outputs were generated in the staged repo.")
    }
  }
  dina_cli_ok(sprintf("Include dry-run output: %s", result$paths$root))
  dina_cli_alert("No production files changed. Confirm only after reviewing a clean run.")
  dina_cli_alert(sprintf("List include tables with `dina sources table admin --run %s`.", result$paths$root))
  dina_cli_alert("Common include tables: include_summary, aux_dependency_summary, aux_validation_report, static_dependency_report, cleaner_summary.")
  dina_cli_alert(sprintf("Confirm after a clean run: dina sources include admin --confirm --include-run %s", result$paths$root))
  invisible(result)
}

dina_print_admin_pit_confirm <- function(result) {
  report <- result$outputs$promote_report
  dina_cli_header("PIT Admin Include Confirm")
  dina_print_data_frame_compact(report, limit = 20L)
  dina_cli_ok(sprintf("Confirm run: %s", result$paths$root))
  dina_cli_alert(sprintf("Rollback: dina sources include admin --restore %s", result$paths$root))
  invisible(result)
}

dina_print_admin_pit_restore <- function(result) {
  report <- result$outputs$restore_report
  dina_cli_header("PIT Admin Include Restore")
  dina_print_data_frame_compact(report, limit = 20L)
  dina_cli_ok(sprintf("Restore report: %s", result$paths$restore_report))
  invisible(result)
}

dina_source_workflow_family <- function(target = NULL, command = "explore") {
  target <- target %||% "sna"
  normalized <- dina_source_norm(target)
  if (normalized %in% c("sna", "country_sna")) {
    if (!identical(normalized, "sna")) {
      dina_cli_warn(sprintf("`%s` is deprecated here; use `sna`.", target))
    }
    return("sna")
  }
  if (identical(normalized, "admin")) {
    return("admin")
  }
  if (normalized %in% dina_source_norm(dina_source_public_families())) {
    stop(
      sprintf(
        "`dina sources %s %s` is not implemented yet. For now, populate the buckets and inspect them with `dina sources list %s`.",
        command,
        normalized,
        normalized
      ),
      call. = FALSE
    )
  }
  stop(
    sprintf(
      "Unknown source type: %s\nPublic source types: %s",
      target,
      paste(dina_source_public_families(), collapse = ", ")
    ),
    call. = FALSE
  )
}

dina_cmd_sources_list <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  action <- dina_arg(args, 1L, "")
  list_actions <- c("detail", "show", "guide", "workflow", "paths", "urls")
  if (!nzchar(action) || startsWith(action, "-") || !action %in% list_actions) {
    flags <- dina_parse_source_list_flags(args, root = root)
    return(dina_print_source_list(root, flags))
  }

  rest <- args[-1]
  if (action %in% c("detail", "show")) {
    flags <- dina_parse_flags(rest)
    id <- dina_arg(flags$positional, 1L, NULL)
    if (is.null(id)) {
      stop("Usage: dina sources list detail ID [--urls]", call. = FALSE)
    }
    return(dina_print_source_show(root, id, include_urls = isTRUE(flags$urls), view = flags$view %||% "all"))
  }

  if (identical(action, "guide")) {
    flags <- dina_parse_source_guide_flags(root, rest)
    return(dina_print_sources_guide(root, flags))
  }

  if (action %in% c("workflow", "paths")) {
    flags <- dina_parse_source_list_flags(rest, root = root)
    flags$view <- action
    return(dina_print_source_list(root, flags))
  }

  if (identical(action, "urls")) {
    candidate <- dina_arg(rest, 1L, "")
    ids <- vapply(dina_sources(root)$sources, function(source) source$id %||% "", character(1))
    if (nzchar(candidate) && !startsWith(candidate, "-") && candidate %in% ids) {
      flags <- dina_parse_flags(rest[-1])
      return(dina_print_source_show(root, candidate, include_urls = TRUE, view = flags$view %||% "all"))
    }
    if (nzchar(candidate) && !startsWith(candidate, "-")) {
      family_match <- tryCatch({
        dina_source_resolve_family_filter(candidate, root)
        TRUE
      }, error = function(e) FALSE)
      if (isTRUE(family_match)) {
        rest <- c("family", candidate, rest[-1])
      }
    }
    flags <- dina_parse_source_list_flags(rest, root = root)
    flags$urls <- TRUE
    return(dina_print_source_list(root, flags))
  }

  invisible(NULL)
}

dina_cmd_sources <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "list")
  if (identical(sub, "list")) {
    dina_cmd_sources_list(root, args[-1])
  } else if (identical(sub, "show")) {
    dina_cli_warn("`dina sources show` is deprecated; use `dina sources list detail ID`.")
    flags <- dina_parse_flags(args[-1])
    id <- dina_arg(flags$positional, 1L, NULL)
    if (is.null(id)) stop("Usage: dina sources list detail ID [--urls]", call. = FALSE)
    dina_print_source_show(root, id, include_urls = isTRUE(flags$urls), view = flags$view %||% "all")
  } else if (identical(sub, "guide")) {
    dina_cli_warn("`dina sources guide` is deprecated; use `dina sources list guide`.")
    flags <- dina_parse_flags(args[-1])
    dina_print_sources_guide(root, flags)
  } else if (identical(sub, "fields")) {
    dina_print_source_fields()
  } else if (identical(sub, "methods")) {
    dina_print_source_methods()
  } else if (identical(sub, "inbox")) {
    action <- dina_arg(args, 2L, "guide")
    flags <- dina_parse_flags(args[-c(1L, 2L)])
    if (identical(action, "guide")) {
      rows <- dina_sources_inbox_guide_rows(root, family = flags$family %||% NULL)
      dina_print_source_inbox_guide(rows, root = root, include_urls = isTRUE(flags$urls))
    } else if (identical(action, "init")) {
      result <- dina_sources_inbox_init(root, dry_run = isTRUE(flags[["dry-run"]]), migrate = TRUE)
      dina_print_source_inbox_init(result, root = root)
    } else {
      stop("Usage: dina sources inbox guide [--family FAMILY] [--urls]\n       dina sources inbox init [--dry-run]", call. = FALSE)
    }
  } else if (sub %in% c("compare", "status")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    if (identical(sub, "status")) {
      dina_cli_warn("`dina sources status` is deprecated; use `dina sources compare`.")
    }
    flags <- dina_parse_flags(args[-1])
    hash_mode <- if (isTRUE(flags[["metadata-only"]])) {
      "none"
    } else if (isTRUE(flags[["hash-all"]])) {
      "all"
    } else {
      "changed"
    }
    status <- dina_sources_compare(session, root, hash = hash_mode, deep = isTRUE(flags$deep))
    dina_print_source_compare(status)
  } else if (identical(sub, "table")) {
    flags <- dina_parse_flags(args[-1])
    target <- dina_arg(flags$positional, 1L, "sna")
    table <- dina_arg(flags$positional, 2L, NULL)
    family <- dina_source_workflow_family(target, command = "table")
    if (is.null(table)) {
      if (identical(family, "admin")) {
        dina_print_admin_pit_tables(root, run = flags$run %||% NULL)
        return(invisible(NULL))
      }
      stop("Usage: dina sources table SOURCETYPE TABLE [--run PATH] [--country ISO] [--limit N]", call. = FALSE)
    }
    if (identical(family, "admin")) {
      dina_print_admin_pit_table(
        root,
        table,
        run = flags$run %||% NULL,
        country = flags$country %||% NULL,
        limit = flags$limit %||% 20L
      )
    } else {
      dina_print_country_sna_table(
        root,
        table,
        run = flags$run %||% NULL,
        country = flags$country %||% NULL,
        limit = flags$limit %||% 20L
      )
    }
  } else if (sub %in% c("explore", "include")) {
    flags <- dina_parse_flags(args[-1])
    target <- dina_arg(flags$positional, 1L, "sna")
    family <- dina_source_workflow_family(target, command = sub)
    country <- flags$country %||% NULL
    if (!is.null(country)) country <- toupper(country)
    output_dir <- flags[["output-dir"]] %||% if (identical(family, "admin") && identical(sub, "explore")) {
      file.path(root, "output", "experiments", "admin_pit_explore")
    } else if (identical(family, "admin")) {
      file.path(root, "output", "experiments", "admin_pit_include")
    } else if (identical(sub, "explore")) {
      file.path(root, "output", "experiments", "country_sna_explore")
    } else {
      file.path(root, "output", "experiments", "country_sna_include")
    }
    if (!grepl("^/", output_dir)) {
      output_dir <- file.path(root, output_dir)
    }
    if (identical(family, "admin") && identical(sub, "explore")) {
      source(file.path(root, "code", "R", "source-diagnostics", "admin_pit_explorer.R"), local = TRUE)
      result <- run_admin_pit_explorer(root = root, output_dir = output_dir, countries = country, write_outputs = !isTRUE(flags[["dry-run"]]))
      dina_print_admin_pit_explore(result, dry_run = isTRUE(flags[["dry-run"]]))
    } else if (identical(family, "admin")) {
      source(file.path(root, "code", "R", "source-diagnostics", "admin_pit_include.R"), local = TRUE)
      if (isTRUE(flags$confirm)) {
        result <- admin_pit_include_confirm_sources(
          root = root,
          include_run = flags[["include-run"]] %||% NULL,
          output_dir = flags[["output-dir"]] %||% NULL
        )
        dina_print_admin_pit_confirm(result)
      } else if (!is.null(flags$restore)) {
        result <- admin_pit_include_restore_sources(root = root, confirm_run = flags$restore)
        dina_print_admin_pit_restore(result)
      } else {
        result <- run_admin_pit_include(
          root = root,
          output_dir = output_dir,
          exploration_run = flags[["exploration-run"]] %||% NULL,
          write_outputs = TRUE
        )
        dina_print_admin_pit_include(result)
      }
    } else if (identical(sub, "explore")) {
      source(file.path(root, "code", "R", "source-diagnostics", "country_sna_explorer.R"), local = TRUE)
      result <- run_country_sna_explorer(root = root, output_dir = output_dir, countries = country, write_outputs = !isTRUE(flags[["dry-run"]]))
      dina_print_country_sna_explore(result, dry_run = isTRUE(flags[["dry-run"]]))
    } else {
      if (isTRUE(flags$apply)) {
        stop("Use `--confirm`; promotion now requires a staged include run and backup snapshot.", call. = FALSE)
      }
      source(file.path(root, "code", "R", "source-diagnostics", "country_sna_include.R"), local = TRUE)
      if (isTRUE(flags$confirm)) {
        result <- country_sna_include_confirm_sources(
          root = root,
          include_run = flags[["include-run"]] %||% NULL,
          output_dir = flags[["output-dir"]] %||% NULL
        )
        dina_print_country_sna_confirm(result)
      } else if (!is.null(flags$restore)) {
        result <- country_sna_include_restore_sources(root = root, confirm_run = flags$restore)
        dina_print_country_sna_restore(result)
      } else {
        result <- run_country_sna_include(
          root = root,
          output_dir = output_dir,
          exploration_run = flags[["exploration-run"]] %||% NULL,
          write_outputs = TRUE
        )
        dina_print_country_sna_include(result)
      }
    }
  } else if (sub %in% c("diagnose", "diagnostic", "diagnostics")) {
    stop("`dina sources diagnose country-sna` was retired. Use `dina sources explore sna`.", call. = FALSE)
  } else if (identical(sub, "complete")) {
    stop("Unknown sources command: complete. Use `dina todo check ID` for helper checklist progress.", call. = FALSE)
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
    dina_stop_retired_source_workflow("review")
  } else if (sub %in% c("fetch", "refresh")) {
    flags <- dina_parse_flags(args[-1])
    selector <- dina_arg(flags$positional %||% character(), 1L, NULL)
    family <- flags$family %||% NULL
    source_id <- flags$source %||% NULL
    if (!is.null(selector) && nzchar(selector)) {
      if (is.null(source_id) && selector %in% dina_source_ids(root)) {
        source_id <- selector
        selector <- NULL
      } else if (!(selector %in% dina_source_ids(root)) && dina_source_token_resolves_family(selector, root)) {
        family <- selector
        selector <- NULL
      }
    }
    rows <- dina_buckets_fetch(
      root,
      family = family,
      selector = selector,
      source_id = source_id,
      dry_run = isTRUE(flags[["dry-run"]])
    )
    dina_print_bucket_fetch_results(
      rows,
      dry_run = isTRUE(flags[["dry-run"]]),
      show_skipped = !is.null(selector) || !is.null(flags$source),
      title = if (isTRUE(flags[["dry-run"]])) "Source Fetch Preview" else "Source Fetch"
    )
  } else if (identical(sub, "integrate")) {
    dina_stop_retired_source_workflow("integrate")
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
      dina_cli_cat(dina_cli_row(
        list(dina_task_short_id(x$id), x$id, x$stage, x$language, x$status),
        widths = c(6L, 38L, 14L, 8L, NA),
        dim = c(FALSE, FALSE, TRUE, TRUE, TRUE)
      ))
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

dina_cmd_todo <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "list")
  session <- dina_load_session(root = root)
  if (sub %in% c("list", "show")) {
    dina_print_todo_rows(session, root)
  } else if (identical(sub, "check")) {
    if (is.null(session)) stop("No active update. Run `dina update start YEAR`.", call. = FALSE)
    id <- dina_arg(args, 2L, NULL)
    session <- dina_update_todo_state(session, root = root, id = id, checked = TRUE)
    dina_cli_ok(sprintf("Checked todo %s", id))
    dina_print_todo_rows(session, root)
  } else if (identical(sub, "uncheck")) {
    if (is.null(session)) stop("No active update. Run `dina update start YEAR`.", call. = FALSE)
    id <- dina_arg(args, 2L, NULL)
    session <- dina_update_todo_state(session, root = root, id = id, checked = FALSE)
    dina_cli_ok(sprintf("Unchecked todo %s", id))
    dina_print_todo_rows(session, root)
  } else if (identical(sub, "reset")) {
    if (is.null(session)) stop("No active update. Run `dina update start YEAR`.", call. = FALSE)
    session <- dina_update_todo_state(session, root = root, reset = TRUE)
    dina_cli_ok("Todo state reset.")
    dina_print_todo_rows(session, root)
  } else {
    stop("Usage: dina todo\n       dina todo check ID\n       dina todo uncheck ID\n       dina todo reset", call. = FALSE)
  }
}

dina_cmd_run <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, NULL)
  if (identical(sub, "list")) {
    return(dina_cmd_tasks(root, c("list", args[-1])))
  }
  if (identical(sub, "why")) {
    return(dina_cmd_tasks(root, c("why", args[-1])))
  }
  if (identical(sub, "stale")) {
    flags <- dina_parse_flags(args[-1])
    statuses <- dina_all_task_status(root)
    stale_ids <- names(statuses)[vapply(statuses, function(x) x$status %in% c("missing_outputs", "stale", "upstream_stale", "missing_inputs", "never_run", "failed"), logical(1))]
    if (!length(stale_ids)) {
      dina_cli_ok("No stale or missing active tasks.")
      return(invisible(list()))
    }
    tasks <- dina_task_map(root)[stale_ids]
    results <- list()
    for (task in tasks) {
      result <- dina_run_task(task, root, dry_run = isTRUE(flags[["dry-run"]]), force = isTRUE(flags$force))
      results[[task$id]] <- result
      dina_cli_cat(sprintf("%s: %s", result$task, dina_cli_dim(result$status)))
      if (!is.null(result$command)) dina_cli_cat(sprintf("  %s", dina_cli_command(paste(result$command, collapse = " "))))
    }
    return(invisible(results))
  }
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
  dry_run <- isTRUE(flags[["dry-run"]])
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
    dina_cli_cat(sprintf("%s: %s", result$task, dina_cli_dim(result$status)))
    if (!is.null(result$command)) dina_cli_cat(sprintf("  %s", dina_cli_command(paste(result$command, collapse = " "))))
  }
  completed <- TRUE
}

dina_cmd_config <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "show")
  if (identical(sub, "show")) {
    cat(paste(readLines(dina_config_path(root), warn = FALSE), collapse = "\n"), "\n")
  } else if (identical(sub, "check")) {
    cfg <- dina_config(root, expand_env = FALSE)
    dina_cli_header("Config Check")
    dina_cli_cat(dina_cli_key_value("Config:", dina_relative(dina_config_path(root), root)))
    required <- c("project", "countries", "years", "run", "stata", "paths")
    missing <- required[!vapply(required, function(name) !is.null(cfg[[name]]), logical(1))]
    if (length(missing)) {
      dina_cli_warn(sprintf("Missing top-level config keys: %s", paste(missing, collapse = ", ")))
    } else {
      dina_cli_ok("Required top-level config keys are present.")
    }
    if (file.exists(file.path(root, "_config.do"))) {
      dina_cli_warn("_config.do exists but is no longer part of the CLI workflow.")
    } else {
      dina_cli_ok("_config.do is absent from the CLI workflow.")
    }
  } else if (identical(sub, "propose")) {
    stop("`dina config propose` is retired. Start or resume an update, then use `dina update config edit`.", call. = FALSE)
  } else if (identical(sub, "set")) {
    stop("`dina config set` is retired. Edit config/dina.yml manually, or use `dina update config edit` for an update override.", call. = FALSE)
  } else if (identical(sub, "stata")) {
    flags <- dina_parse_flags(args[-1])
    path <- flags$output %||% dina_arg(flags$positional, 1L, NULL)
    if (is.null(path) || !nzchar(path)) {
      stop("Usage: dina config stata --output PATH", call. = FALSE)
    }
    full <- if (grepl("^/", path)) path else file.path(root, path)
    dina_render_config_do(dina_config(root, expand_env = FALSE), full)
    dina_cli_ok(sprintf("Wrote explicit Stata runtime config %s", dina_relative(full, root)))
    dina_cli_alert(sprintf("Use it with: %s", dina_cli_command(sprintf("export DINA_CONFIG_DO=\"%s\"", full))))
  } else if (identical(sub, "render")) {
    stop("Unknown config command: render. Use `dina config stata --output PATH` for manual Stata export.", call. = FALSE)
  } else if (identical(sub, "edit")) {
    stop("`dina config edit` is retired. Edit config/dina.yml manually, or use `dina update config edit` for an update override.", call. = FALSE)
  } else {
    stop("Unknown config command: ", sub, call. = FALSE)
  }
}

dina_print_compress_input <- function(result) {
  title <- if (isTRUE(result$dry_run)) "Compress Input Preview" else "Compress Input"
  dina_cli_header(title)
  dina_cli_cat(dina_cli_key_value("Source root:", result$source_root %||% ""))
  dina_cli_cat(dina_cli_key_value("Included root:", result$included_root %||% ""))
  dina_cli_cat(dina_cli_key_value("Output zip:", result$output %||% ""))
  excluded_types <- result$excluded_types %||% character()
  dina_cli_cat(dina_cli_key_value("Excluded source types:", if (length(excluded_types)) paste(excluded_types, collapse = ", ") else "none"))
  excluded <- result$excluded_paths
  if (is.data.frame(excluded) && nrow(excluded)) {
    dina_cli_cat("")
    dina_cli_cat("Excluded paths:")
    for (i in seq_len(nrow(excluded))) {
      status <- if (isTRUE(excluded$exists[[i]])) "present" else "missing"
      dina_cli_cat(sprintf("  %-8s %s", status, excluded$path[[i]]))
    }
  }
  if (isTRUE(result$dry_run)) {
    dina_cli_alert("Dry-run only: no zip was written.")
  } else {
    dina_cli_ok(sprintf("Wrote %s", result$output %||% ""))
  }
  invisible(result)
}

dina_cmd_compress <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  target <- dina_arg(args, 1L, "input")
  if (!identical(target, "input")) {
    stop("Usage: dina compress input [--dry-run] [--dropbox] [--all] [--include SOURCETYPE] [--exclude SOURCETYPE] [--output PATH]", call. = FALSE)
  }
  flags <- dina_parse_flags(args[-1])
  if (length(flags$positional %||% character())) {
    stop("Usage: dina compress input [--dry-run] [--dropbox] [--all] [--include SOURCETYPE] [--exclude SOURCETYPE] [--output PATH]", call. = FALSE)
  }
  if (isTRUE(flags$output)) {
    stop("Missing value for --output.", call. = FALSE)
  }
  if (isTRUE(flags$include)) {
    stop("Missing value for --include.", call. = FALSE)
  }
  if (isTRUE(flags$exclude)) {
    stop("Missing value for --exclude.", call. = FALSE)
  }
  result <- dina_compress_input(
    root = root,
    dropbox = isTRUE(flags$dropbox),
    output = flags$output %||% NULL,
    all = isTRUE(flags$all),
    include = flags$include %||% character(),
    exclude = flags$exclude %||% character(),
    dry_run = isTRUE(flags[["dry-run"]])
  )
  dina_print_compress_input(result)
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

dina_cmd_maintain <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "status")
  if (identical(sub, "doctor")) {
    return(dina_cmd_doctor(root))
  }
  if (identical(sub, "audit")) {
    return(dina_cmd_audit(root, args[-1]))
  }
  if (identical(sub, "repo-status")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(args[-1])
    comparison <- dina_repo_state_compare_for_cli(session, root, baseline = flags$baseline %||% "start", title = "Maintain Repo Status")
    if (isTRUE(comparison$missing)) return(invisible(comparison))
    return(dina_print_repo_status(comparison))
  }
  if (identical(sub, "repo-diff")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(args[-1])
    any_mode <- isTRUE(flags$stat) || isTRUE(flags$patch) || isTRUE(flags$files)
    comparison <- dina_repo_state_compare_for_cli(session, root, baseline = flags$baseline %||% "start", title = "Maintain Repo Diff")
    if (isTRUE(comparison$missing)) return(invisible(comparison))
    return(dina_print_repo_diff(
      session,
      root = root,
      baseline = flags$baseline %||% "start",
      stat = isTRUE(flags$stat) || !any_mode,
      patch = isTRUE(flags$patch),
      files = isTRUE(flags$files),
      comparison = comparison
    ))
  }
  if (identical(sub, "repo-restore")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(args[-1])
    result <- dina_repo_state_restore(session, root = root, baseline = flags$baseline %||% "start", yes = isTRUE(flags$yes))
    return(dina_print_repo_restore(result))
  }
  stop("Usage: dina maintain doctor\n       dina maintain audit paths\n       dina maintain repo-status\n       dina maintain repo-diff [--stat] [--files]\n       dina maintain repo-restore [--yes]", call. = FALSE)
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
  cli_path <- normalizePath(file.path(dina_cli_root, "code", "R", "cli", "dina.R"), mustWork = TRUE)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    sprintf("exec Rscript %s \"$@\"", shQuote(cli_path))
  ), target)
  Sys.chmod(target, "0755")
  dina_cli_ok(sprintf("Installed command wrapper at %s", target))
}

dina_cmd_commands <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  if (length(args)) {
    stop("Usage: dina commands\n       dina navigate", call. = FALSE)
  }
  dina_print_command_navigator(root)
}

dina_cmd_menu <- function(root, args) {
  args <- dina_drop_leading_separator(args)
  sub <- dina_arg(args, 1L, "main")
  if (sub %in% c("main", "commands", "navigate")) {
    return(dina_print_command_navigator(root))
  }
  stop("Usage: dina menu\n       dina menu commands", call. = FALSE)
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
  if (dina_has_help_flag(rest)) return(dina_usage(if (identical(cmd, "bucket")) "buckets" else cmd))
  switch(
    cmd,
    doctor = dina_cmd_doctor(root),
    install = dina_cmd_install(root, rest),
    update = dina_cmd_update(root, rest),
    bucket = dina_cmd_buckets(root, rest),
    buckets = dina_cmd_buckets(root, rest),
    sources = dina_cmd_sources(root, rest),
    tasks = dina_cmd_tasks(root, rest),
    run = dina_cmd_run(root, rest),
    todo = dina_cmd_todo(root, rest),
    config = dina_cmd_config(root, rest),
    compress = dina_cmd_compress(root, rest),
    commands = dina_cmd_commands(root, rest),
    navigate = dina_cmd_commands(root, rest),
    menu = dina_cmd_menu(root, rest),
    audit = dina_cmd_audit(root, rest),
    make = dina_cmd_make(root, rest),
    maintain = dina_cmd_maintain(root, rest),
    notify = dina_cmd_notify(root, rest),
    setup = dina_cmd_setup(root, rest),
    {
      dina_cli_err(sprintf("Unknown command: %s", cmd))
      dina_usage()
      quit(status = 1)
    }
  )
}

if (!identical(Sys.getenv("DINA_CLI_SOURCE_ONLY", unset = ""), "1")) {
  tryCatch(
    dina_main(root = dina_repo_root(dina_cli_root)),
    error = function(e) {
      dina_cli_err(conditionMessage(e))
      quit(status = 1)
    }
  )
}
