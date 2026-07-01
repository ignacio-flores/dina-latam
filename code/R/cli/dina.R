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
  `update roadmap|gate [GATE]`        [read-only] inspect gate workflow
  `update mark|unmark GATE/CHECK`     [writes session] record gate progress
  `update prefs|config`               [read-only/writes session] session choices
  `update repo-status|repo-diff`      [read-only] compare with start baseline
  `update repo-restore`               [writes files] restore captured files
  `update resume|status`              [read-only] inspect active session
  `update list`                       [read-only] list update sessions
  `update restart|delete`             [writes session] lifecycle controls
  `update finalize [--force] [--yes]` [writes session/config] freeze records

Source data:
  `sources status|scan|diff|review`   [read-only] inspect source coverage
  `sources list|show ID`              [read-only] inspect source registry
  `sources inbox guide|init`          [read-only/writes dirs] manual inboxes
  `sources refresh [--dry-run]`       [read-only/writes session] fetch URL/ZIP
  `sources integrate --incoming`      [writes files] accept _new inbox files
  `sources integrate`                 [writes files] copy approved inputs

Pipeline:
  `tasks list|why TASK`               [read-only] inspect task freshness
  `run ... --dry-run`                 [read-only] preview selected scripts
  `run ... --execute`                 [writes files] run selected scripts

Setup and config:
  `doctor`                            [read-only] check local readiness
  `install`                           install missing R packages
  `config show|stata`                 [read-only/writes files] inspect/export
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
  Update progress is recorded with roadmap gate checks, not source commands.
  `dina run` is dry-run unless `--execute` is present.
  `update start` records a source baseline with hashes by default.
  `update start` uses benchmark `config/dina.yml` plus a working override.
  Working overrides do not change the benchmark until `update finalize --yes`.
  `sources status` uses hashes only when timestamps or sizes changed.
  Source URLs are cataloged even for manual downloads. URL presence does not
  mean automatic download: `sources refresh` fetches only `url`/`zip` methods.
  `sources refresh --dry-run` writes nothing. `sources integrate` copies
  approved files into `input_data/`.
  `config stata --output PATH` writes an explicit manual Stata runtime config.
  `config set` and `config edit` modify `config/dina.yml`.

Notes:
  `--` is accepted as an optional separator for shell compatibility, but it is
  never required. For example, `dina help` and `dina -- help` both work.

Examples:
  dina help workflow
  dina update start YEAR
  dina update roadmap
  dina update gate tax-admin
  dina run 01a --dry-run
  dina run 01 --execute --notify
",
    workflow = "Usage:
  dina help workflow

What this page is:
  The annual update guide. The executable progress model is the roadmap:
  each gate documents the source families, checks, task ids, and next commands.

1. Start with the roadmap
  dina doctor
      Read-only preflight for R packages, Stata, paths, notifications, and
      the active update pointer.

  dina update start [YEAR]
      Creates `output/updates/<update_id>`, records a hashed source baseline,
      prepares a working override location, records a repo-state baseline, and
      makes the session active.

  dina update roadmap
      Shows the ordered gate map and the next unfinished check.

2. Work gate by gate
  dina update gate parameters
  dina update gate macro-sna
  dina update gate surveys
  dina update gate tax-admin
  dina update gate tax-rates
  dina update gate spending
  dina update gate bfm
  dina update gate imputation
  dina update gate export-validation
      Show gate-specific sources, checks, task ids, old-reference notes, inbox
      files, and suggested commands.

  dina update mark GATE/CHECK --status done --note TEXT
      Records a human/intermediate decision in the active session.

  dina update unmark GATE/CHECK
      Clears a check that was marked too early.

  dina update prefs old-refs on
  dina update prefs old-refs off
      Shows or hides old-reference notes on gate pages.

  dina update repo-status
      Compares current code/config/docs with the session start baseline.

3. Use source tools inside gates
  dina sources list family admin-data --urls
  dina sources show ID --urls
      Inspect cataloged source URLs and local canonical paths.

  dina sources refresh [--source ID] [--dry-run] [--urls]
      Fetches only `url` and `zip` methods into update staging.

  dina sources review
      Shows staged files and configured `_new` inbox candidates.

  dina sources integrate --incoming --source ID [--yes]
      Previews or copies an approved `_new` inbox file/folder into its canonical
      `input_data/` destination.

  dina sources status [--metadata-only] [--hash-all] [--deep]
      Diagnostic comparison against the update baseline.

4. Run tasks when the gate says they are ready
  dina tasks list
  dina tasks why TASK
  dina run TASK --dry-run
  dina run TASK --execute
      `pipeline.yml` task ids remain the executable graph. Gates explain when
      and why to run them.

Task selectors:
  01a                  One task.
  01                   Whole numbered block for `dina run`.
  01a,02a              Multiple tasks for `dina run`.
  full-task-id         Exact task id.
  --from 03 --to 05    Range from block 03 through block 05.
  `tasks why` needs one unique task selector, such as 01a or a full task id.

5. Finalize
  dina update finalize
      Freezes final outputs and checksums. It refuses missing, stale, or failed
      required tasks unless `--force` is supplied.
      With `--yes`, also promotes the effective update config to benchmark
      `config/dina.yml`.

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
  dina update roadmap
  dina update gate [GATE]
  dina update mark GATE/CHECK --status STATUS [--note TEXT]
  dina update unmark GATE/CHECK
  dina update prefs [old-refs on|off]
  dina update config show|set
  dina update repo-status [--baseline NAME]
  dina update repo-diff [--stat] [--patch] [--files] [--baseline NAME]
  dina update repo-restore [--dry-run] [--yes] [--baseline NAME]
  dina update list
  dina update restart [ID] [--yes] [--replace-repo-baseline]
  dina update delete [ID] [--yes]
  dina update finalize [--force] [--yes]

What it manages:
  Annual update sessions under `output/updates/<update_id>`. A session stores
  a working config override when needed, repo-state baselines, source scans,
  gate records, task run records, and final manifests. Update progress is
  recorded through roadmap gate checks.

Subcommands:
  start [YEAR]                    Creates a new session, active pointer, and
                                  hashed source baseline. It uses benchmark
                                  `config/dina.yml` plus an optional working
                                  override and records a recoverable repo-state
                                  baseline.
                                  If omitted, YEAR defaults to the current
                                  calendar year. Default id:
                                  YEAR-update-MM-DD. If an unfinished same-day
                                  session exists, creating YEAR-update-MM-DD-02
                                  requires confirmation.
  resume                          Recomputes reality and recommends the next
                                  action. It does not blindly continue a run.
  status                          Same state summary as resume, without implying
                                  that work should continue automatically.
  roadmap                         Prints the ordered data-family gate map.
  gate [GATE]                     Prints detailed checks, source families,
                                  inbox files, task ids, and suggested commands
                                  for one gate. With no GATE, shows the next
                                  unfinished gate. `gate parameters` is guided
                                  in an interactive terminal and read-only in
                                  scripts.
  mark GATE/CHECK                 Records a gate check as done, deferred, or
                                  needs-code.
  unmark GATE/CHECK               Clears a previously recorded gate check.
  prefs                           Shows session preferences.
  prefs old-refs on|off           Shows or hides old-reference notes on gate
                                  pages.
  config show|set                 Inspects effective config or writes only the
                                  active session override. Benchmark config
                                  changes only through finalize promotion.
  repo-status                     Compares current repo files with a captured
                                  session baseline.
  repo-diff                       Shows stat, file, or patch-style differences
                                  against a captured baseline.
  repo-restore                    Restores captured modified/deleted files from
                                  a baseline. It never touches excluded data
                                  roots and requires --yes to mutate.
  list                            Lists update sessions and marks the active
                                  one with `*`.
  restart [ID] [--yes]            Resets one update session from scratch using
                                  the same id. If ID is omitted, uses the active
                                  session. Without --yes, interactive terminals
                                  ask before resetting. By default it preserves
                                  the original repo baseline and adds a restart
                                  snapshot; use --replace-repo-baseline to make
                                  current repo state the new start baseline.
  delete [ID] [--yes]             Deletes an update session. If ID is omitted,
                                  uses the active session. Without --yes,
                                  interactive terminals ask before deleting;
                                  scripts only preview.
  finalize [--force]              Freezes final outputs and checksums. Without
                                  --force it refuses missing, stale, or failed
                                  required tasks. With --yes, promotes the
                                  effective update config to benchmark config.

What it changes:
  `start`, source commands during a session, `restart`, `delete`, and `finalize`
  write session records or remove session files. `mark` and `unmark` write gate
  records. `update config set` writes only the working override. `repo-restore --yes`
  restores captured code/config/docs outside excluded data roots. `resume`,
  `status`, `roadmap`, `gate`, `repo-status`, `repo-diff`, and `list` inspect.

Options:
  --no-source-hash                For start, record only file size/timestamp in
                                  the source baseline. The default computes
                                  source hashes for later comparison.
  --old-refs / --no-old-refs      For start/restart, set Old-reference notes.
  --replace-repo-baseline         For restart, discard the original repo
                                  baseline and capture current repo state.
  --baseline NAME                 For repo commands, compare against `start` or
                                  a `restart-...` baseline.
  --status STATUS                 For mark: done, deferred, or needs-code.
  --note TEXT                     Explanation for a marked check. Required for
                                  deferred and needs-code.
  --yes                           For finalize, promote effective config to
                                  benchmark config after checks pass.

Examples:
  dina update start YEAR
  dina update start YEAR --yes
  dina update roadmap
  dina update gate tax-admin
  dina update mark tax-admin/raw-accepted --status done
  dina update unmark tax-admin/raw-accepted
  dina update prefs old-refs off
  dina update config set years.last YEAR
  dina update repo-status
  dina update repo-diff --stat --files
  dina update repo-restore --dry-run
  dina update list
  dina update restart --yes
  dina update delete 2026-update-06-29 --yes
  dina update resume
  dina update finalize --yes
",
    sources = "Usage:
  dina sources refresh [--source ID] [--dry-run] [--urls]
  dina sources stage --source ID --file PATH [--yes]
  dina sources stage --source ID --dir PATH [--yes]
  dina sources list [--family FAMILY] [--country ISO] [--method METHOD] [--urls]
  dina sources list country ISO [--urls]
  dina sources list family FAMILY [--urls]
  dina sources list method METHOD [--urls]
  dina sources show ID [--urls]
  dina sources methods
  dina sources inbox guide [--family FAMILY] [--urls]
  dina sources inbox init [--dry-run]
  dina sources status [--metadata-only] [--hash-all] [--deep]
  dina sources scan [--deep] [--hash]
  dina sources review
  dina sources diff [--deep] [--hash]
  dina sources integrate --incoming --source ID [--yes]
  dina sources integrate --incoming --all [--yes]
  dina sources integrate --all [--yes]
  dina sources integrate --source ID [--yes]
  dina sources integrate --staged RELPATH --to input_data/... [--source ID] [--yes]

What it manages:
  Source catalog inspection, URL/ZIP fetching, staged-file review, and copying
  approved inputs into canonical `input_data/` paths. Update progress itself is
  recorded with `dina update mark GATE/CHECK`.

  Subcommands:
  list                            Shows compact registry rows: id, family,
                                  country, method, path count, URL presence,
                                  downloader presence, and transformer presence.
  show ID                         Shows the full registry entry for one source,
                                  including URLs, canonical paths, scripts,
                                  checks, and notes.
  methods                         Explains source acquisition method labels.
  inbox guide                     Shows the central `input_data/_new` buckets,
                                  expected examples, canonical destinations,
                                  and source URL references.
  inbox init                      Creates missing central inbox buckets and can
                                  copy old colocated `_new` files into them.
  status                          Compares current canonical source files with
                                  the active update baseline. Default mode uses
                                  hashes only when size/timestamp changed.
  refresh                         Shows file state, URL availability, staging
                                  targets, and next actions. It fetches only
                                  URL/ZIP methods. --dry-run writes nothing.
  stage                           Copies a hand-downloaded file/folder into
                                  staging and records source metadata. Advanced
                                  utility; `_new` inboxes are preferred for
                                  routine manual admin updates.
  scan                            Reads the source registry and detects local
                                  coverage from filenames and, with --deep,
                                  workbook metadata.
  review                          Lists staged files, empty inbox buckets with
                                  expected examples, and incoming files with
                                  hashes and validation.
  diff                            Compares current scan results with the active
                                  session baseline and classifies changes.
  integrate                       Copies approved staged or incoming files into
                                  final destinations and records decisions.

  Options:
  --source ID                     Limit refresh to one source registry id.
                                  For integrate, bulk-integrate one source when
                                  it has an explicit destination.
  --all                           For integrate, preview/integrate all ready
                                  staged files with explicit destinations.
  --incoming                      For integrate, use configured `_new` inbox
                                  candidates instead of update staging.
  --file PATH                     For stage, copy one manual file into staging.
  --dir PATH                      For stage, copy one manual folder into staging.
  --family FAMILY                 For list, keep one source family.
  --method METHOD                 For list, keep one acquisition method.
  --country ISO                   For list, keep one ISO country plus broad
                                  country sources.
  country ISO                     Friendly form of --country ISO for list.
  family FAMILY                   Friendly form of --family FAMILY for list.
  method METHOD                   Friendly form of --method METHOD for list.
  --urls                          Print/expand source URLs in list/show/refresh
                                  output. Manual URLs are catalog targets, not
                                  automatic downloads.
  --deep                          Inspect workbook sheets when possible.
  --hash                          Compute file hashes during scan/diff.
  --metadata-only                 For status, compare only paths, size, and
                                  timestamps; do not compute hashes.
  --hash-all                      For status, hash all source files.
  --dry-run                       For refresh, show planned downloads only and
                                  write nothing.
                                  For inbox init, preview folders and copied
                                  legacy files only.
  --yes                           For stage/integrate, allow overwriting the
                                  staged/final destination.

Gotcha:
  Source coverage is independent of update year. A 2026 update may discover
  newly available 2024 data or historical backfills.
  URL presence does not mean automatic download. `manual`, `script`, and `wid`
  sources can have URLs that you inspect, download from, or use for verification.
  `_new` folders are treated as manual inboxes. Pipeline scripts consume
  canonical paths only, never `_new` directly.

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
  dina sources show col-admin-income --urls
  dina sources show surveys-cepal --urls
  dina sources inbox guide
  dina sources inbox guide --family admin_tax --urls
  dina sources inbox init --dry-run
  dina sources status
  dina sources refresh --dry-run --urls
  dina sources refresh --source chl-pit-total
  dina sources list method manual --urls
  dina sources review
  dina sources integrate --incoming --source chl-pit-total
  dina sources integrate --source chl-pit-total
  dina sources integrate --all --yes
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
  dina config stata --output PATH

What it manages:
  `config/dina.yml`, the current benchmark project configuration. Update
  sessions may add a working override; Stata runtime configs are temporary
  during `dina run`.

Subcommands:
  show                            Prints the committed default YAML exactly as
                                  stored in `config/dina.yml`.
  set KEY VALUE                   Edits `config/dina.yml`. Nested keys use dots,
                                  e.g. `years.last`. Values are parsed as
                                  booleans, integers, or comma-separated vectors
                                  when they look like those types.
  edit                            Opens `config/dina.yml` in `$EDITOR`.
  stata --output PATH             Writes an explicit Stata runtime config for
                                  manual Stata runs. Normal `dina run` uses a
                                  temporary file and does not keep it.

What it changes:
  `show` changes nothing. `set` and `edit` modify committed defaults.
  `stata --output` writes only the requested manual export path; it does not
  edit `_config.do`.

Examples:
  dina config show
  dina config set years.last YEAR
  dina config set run.units ind,esn,pch
  dina config stata --output /tmp/dina-config.do
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

dina_cli_old_refs_choice <- function(flags = list(), current = TRUE, input = "stdin", is_terminal = isatty(stdin())) {
  if (isTRUE(flags[["old-refs"]])) {
    return(TRUE)
  }
  if (isTRUE(flags[["no-old-refs"]])) {
    return(FALSE)
  }
  dina_cli_alert("Old-reference notes map the old update notes to the current gate workflow.")
  dina_cli_alert("Later: `dina update prefs old-refs on` or `dina update prefs old-refs off`.")
  if (!isTRUE(is_terminal)) {
    dina_cli_alert(sprintf("Old-reference notes: %s", if (isTRUE(current)) "on" else "off"))
    return(isTRUE(current))
  }
  prompt <- sprintf("Show Old-reference notes in gate pages? [%s] ", if (isTRUE(current)) "Y/n" else "y/N")
  dina_prompt_yes_no(prompt, default = isTRUE(current), input = input, is_terminal = TRUE)
}

dina_cli_prompt_value <- function(prompt, default = "", input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(default)
  }
  answer <- trimws(dina_read_prompt(prompt, input = input))
  if (nzchar(answer)) answer else default
}

dina_cli_config_value <- function(x) {
  values <- dina_source_values(x)
  if (!length(values)) {
    return("")
  }
  paste(values, collapse = ",")
}

dina_print_parameter_summary <- function(session, root = dina_repo_root()) {
  config <- dina_session_config(session, root, expand_env = FALSE)
  export <- dina_export_validation_config(config)
  suggested <- as.integer(config$years$last %||% as.integer(format(Sys.Date(), "%Y"))) + 1L
  override_path <- dina_session_config_override_path(session$id, root)
  dina_cli_cat("")
  dina_cli_cat("Session config:")
  dina_cli_alert("Benchmark: config/dina.yml")
  dina_cli_alert(sprintf(
    "Working override: %s (%s)",
    dina_relative(override_path, root),
    if (file.exists(override_path)) "present" else "not created yet"
  ))
  dina_cli_cat(sprintf("%-28s %s", "years.first", config$years$first %||% ""))
  dina_cli_cat(sprintf("%-28s %s", "years.last", config$years$last %||% ""))
  dina_cli_cat(sprintf("%-28s %s", "suggested next years.last", suggested))
  dina_cli_cat(sprintf("%-28s %s", "countries", dina_cli_config_value(config$countries %||% character())))
  dina_cli_cat(sprintf("%-28s %s", "run.units", dina_cli_config_value(config$run$units %||% character())))
  dina_cli_cat(sprintf("%-28s %s", "run.steps", dina_cli_config_value(config$run$steps %||% character())))
  dina_cli_cat(sprintf("%-28s %s", "export_validation.unit", export$unit))
  dina_cli_cat(sprintf("%-28s %s", "export_validation.steps", dina_cli_config_value(export$steps)))
  dina_cli_cat(sprintf("%-28s %s", "export_validation.last_year", export$last_year))
  dina_cli_cat(sprintf("%-28s %s", "previous_update_date", export$previous_update_date))
  dina_cli_cat(sprintf("%-28s %s", "previous_update_file", export$previous_update_file))
  dina_cli_alert("Session edits use `dina update config set KEY VALUE`; benchmark config is promoted only at finalize.")
}

dina_update_parameters_wizard <- function(session, root = dina_repo_root(), input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    dina_print_update_gate(session, root, "parameters")
    return(invisible(session))
  }
  config <- dina_session_config(session, root, expand_env = FALSE)
  current_last <- as.integer(config$years$last %||% format(Sys.Date(), "%Y"))
  suggested_last <- current_last + 1L

  dina_cli_header("Gate: parameters")
  dina_cli_alert("This gate edits only the active working override. The benchmark config is untouched until finalize promotion.")
  dina_print_parameter_summary(session, root)

  dina_cli_cat("")
  dina_cli_cat(sprintf("Choose output last year: 1=%s, 2=keep %s, 3=type another year.", suggested_last, current_last))
  year_choice <- dina_cli_prompt_value("Selection [1]: ", default = "1", input = input, is_terminal = TRUE)
  if (identical(year_choice, "2")) {
    selected_last <- current_last
  } else if (identical(year_choice, "3")) {
    selected_last <- suppressWarnings(as.integer(dina_cli_prompt_value("Enter years.last: ", default = as.character(current_last), input = input, is_terminal = TRUE)))
  } else {
    selected_last <- suppressWarnings(as.integer(year_choice))
    if (is.na(selected_last) || selected_last < 1900L) selected_last <- suggested_last
  }
  if (is.na(selected_last) || selected_last < 1900L) selected_last <- current_last
  if (!identical(selected_last, current_last)) {
    session <- dina_session_config_set(session, root = root, key = "years.last", value = as.character(selected_last))
    config <- dina_session_config(session, root, expand_env = FALSE)
  }

  current_countries <- dina_cli_config_value(config$countries %||% character())
  dina_cli_cat(sprintf("Countries: %s", current_countries))
  if (dina_prompt_yes_no("Edit country list? [y/N] ", default = FALSE, input = input, is_terminal = TRUE)) {
    countries <- dina_cli_prompt_value("Countries, comma-separated ISO3: ", default = current_countries, input = input, is_terminal = TRUE)
    config$countries <- trimws(strsplit(countries, ",", fixed = TRUE)[[1]])
    config$countries <- config$countries[nzchar(config$countries)]
    session <- dina_session_config_set(session, root = root, key = "countries", value = paste(config$countries, collapse = ","))
    config <- dina_session_config(session, root, expand_env = FALSE)
  }

  dina_cli_cat("")
  dina_cli_cat("Optional working override edits. Use KEY VALUE, for example `export_validation.last_year 2024`; blank continues.")
  repeat {
    edit <- dina_cli_prompt_value("Edit: ", default = "", input = input, is_terminal = TRUE)
    if (!nzchar(edit)) {
      break
    }
    parts <- strsplit(edit, "\\s+", perl = TRUE)[[1]]
    if (length(parts) < 2L) {
      dina_cli_warn("Expected KEY VALUE.")
      next
    }
    key <- parts[[1]]
    value <- paste(parts[-1], collapse = " ")
    session <- dina_session_config_set(session, root = root, key = key, value = value)
    config <- dina_session_config(session, root, expand_env = FALSE)
  }

  override_path <- dina_session_config_override_path(session$id, root)
  if (file.exists(override_path)) {
    dina_cli_ok(sprintf("Updated working override: %s", dina_relative(override_path, root)))
  } else {
    dina_cli_ok("No working override created; effective config is the benchmark config.")
  }

  checks <- c("year-scope", "export-settings", "config-rendered")
  for (check in checks) {
    target <- sprintf("parameters/%s", check)
    if (dina_prompt_yes_no(sprintf("Mark %s done? [Y/n] ", target), default = TRUE, input = input, is_terminal = TRUE)) {
      dina_update_mark_gate(session, root = root, target = target, status = "done")
      session <- dina_load_session(root = root)
    }
  }
  dina_cli_ok("Next action: dina update roadmap")
  invisible(session)
}

dina_print_repo_status <- function(comparison) {
  dina_cli_header("Update Repo Status")
  metadata <- comparison$metadata
  dina_cli_alert(sprintf("Baseline: %s (%s)", metadata$baseline %||% "", metadata$created_at %||% ""))
  dina_cli_alert(sprintf("Branch at baseline: %s", metadata$branch %||% "unknown"))
  dina_cli_alert(sprintf("HEAD at baseline: %s", metadata$head %||% "unknown"))
  counts <- comparison$counts
  dina_cli_cat(sprintf(
    "Current vs baseline: added=%s modified=%s deleted=%s unchanged=%s",
    counts[["added"]], counts[["modified"]], counts[["deleted"]], counts[["unchanged"]]
  ))
  changed <- comparison$rows[comparison$rows$state != "unchanged", , drop = FALSE]
  if (!nrow(changed)) {
    dina_cli_ok("No captured repo files differ from the baseline.")
    return(invisible(comparison))
  }
  dina_cli_cat(sprintf("%-10s %s", "state", "path"))
  for (i in seq_len(nrow(changed))) {
    dina_cli_cat(sprintf("%-10s %s", changed$state[[i]], changed$path[[i]]))
  }
  invisible(comparison)
}

dina_print_repo_diff <- function(session, root = dina_repo_root(), baseline = "start", stat = TRUE, patch = FALSE, files = FALSE) {
  comparison <- dina_repo_state_compare(session, root, baseline)
  if (isTRUE(stat)) {
    dina_print_repo_status(comparison)
  }
  changed <- comparison$rows[comparison$rows$state != "unchanged", , drop = FALSE]
  if (isTRUE(files)) {
    dina_cli_cat("")
    dina_cli_cat("Files:")
    if (!nrow(changed)) dina_cli_cat("  none")
    for (i in seq_len(nrow(changed))) {
      dina_cli_cat(sprintf("  %s %s", changed$state[[i]], changed$path[[i]]))
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

dina_dashboard_action_from_command <- function(command) {
  command <- trimws(command %||% "")
  command <- gsub(
    "dina update start YEAR",
    sprintf("dina update start %s", format(Sys.Date(), "%Y")),
    command,
    fixed = TRUE
  )
  if (!grepl("^dina(\\s|$)", command)) {
    return(NULL)
  }
  args <- strsplit(sub("^dina\\s*", "", command), "\\s+")[[1]]
  args <- args[nzchar(args)]
  list(label = command, args = args)
}

dina_dashboard_recommended_action <- function(recommendation) {
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
  dina_dashboard_action_from_command(commands[[1]])
}

dina_dashboard_actions <- function(recommendation = NULL) {
  year <- format(Sys.Date(), "%Y")
  actions <- list()
  recommended <- dina_dashboard_recommended_action(recommendation)
  if (!is.null(recommended)) {
    actions[[length(actions) + 1L]] <- recommended
  }
  standard <- list(
    list(label = "dina doctor", args = c("doctor")),
    list(label = sprintf("dina update start %s", year), args = c("update", "start", year)),
    list(label = "dina update roadmap", args = c("update", "roadmap")),
    list(label = "dina update gate", args = c("update", "gate")),
    list(label = "dina sources inbox guide", args = c("sources", "inbox", "guide")),
    list(label = "dina tasks list", args = c("tasks", "list")),
    list(label = "dina run --dry-run", args = c("run", "--dry-run"))
  )
  for (action in standard) {
    actions[[length(actions) + 1L]] <- action
  }
  labels <- vapply(actions, function(action) action$label, character(1))
  actions[!duplicated(labels)]
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
  recommendation <- gsub(
    "dina update start YEAR",
    sprintf("dina update start %s", format(Sys.Date(), "%Y")),
    state$recommendation,
    fixed = TRUE
  )
  actions <- dina_dashboard_actions(recommendation)
  if (is.null(session)) {
    dina_cli_alert("No active update session.")
  } else {
    dina_cli_alert(sprintf("Active update: %s", session$id))
    dina_cli_alert(sprintf("State: %s", state$state))
    stale_label <- if (is.na(state$stale_tasks %||% NA_integer_)) "not checked before gates complete" else as.character(state$stale_tasks %||% 0)
    dina_cli_alert(sprintf("Stale or blocked tasks: %s", stale_label))
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

dina_print_update_gate_summary <- function(session, root = dina_repo_root()) {
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
  next_gate <- dina_next_gate_status(session, root)
  if (is.null(next_gate)) {
    dina_cli_ok("Roadmap gates: complete or deferred.")
  } else {
    dina_cli_warn(sprintf("Next gate: %s (%s)", next_gate$label, next_gate$status))
    if (nzchar(next_gate$next_check_label %||% "")) {
      dina_cli_alert(sprintf("Next check: %s", next_gate$next_check_label))
    }
  }
}

dina_print_update_roadmap <- function(session, root = dina_repo_root()) {
  dina_cli_header("Update Roadmap")
  if (is.null(session)) {
    dina_cli_warn("No active update. Run `dina update start YEAR` first.")
  } else {
    dina_cli_alert(sprintf("Active update: %s", session$id))
  }
  statuses <- dina_roadmap_status(session, root)
  dina_cli_cat(sprintf("%-3s %-18s %-13s %-28s %s", "#", "gate", "status", "next check", "tasks"))
  for (i in seq_along(statuses)) {
    status <- statuses[[i]]
    gate <- status$gate
    tasks <- paste(dina_source_values(gate$tasks %||% character()), collapse = ",")
    next_check <- status$next_check %||% ""
    dina_cli_cat(sprintf(
      "%-3s %-18s %-13s %-28s %s",
      i,
      status$id,
      status$status,
      next_check,
      tasks
    ))
  }
  next_gate <- dina_next_gate_status(session, root)
  if (is.null(next_gate)) {
    dina_cli_ok("Next action: inspect tasks or finalize when outputs are ready.")
  } else {
    dina_cli_ok(sprintf("Next action: dina update gate %s", next_gate$id))
  }
  invisible(statuses)
}

dina_print_gate_field <- function(label, values) {
  values <- dina_source_values(values)
  if (!length(values)) {
    return(invisible(NULL))
  }
  dina_cli_cat(sprintf("%s:", label))
  for (value in values) {
    dina_cli_cat(sprintf("  - %s", value))
  }
}

dina_print_update_gate <- function(session, root = dina_repo_root(), gate_id = NULL) {
  if (is.null(gate_id) || !nzchar(gate_id)) {
    next_gate <- dina_next_gate_status(session, root)
    gate_id <- next_gate$id %||% "parameters"
  }
  gate <- dina_find_gate(gate_id, root)
  status <- dina_gate_status(gate, session)
  dina_cli_header(sprintf("Gate: %s", gate$id))
  dina_cli_alert(sprintf("Status: %s", status))
  if (nzchar(gate$label %||% "")) dina_cli_alert(gate$label)
  if (nzchar(gate$goal %||% "")) dina_cli_cat(gate$goal)
  dina_print_gate_field("Source families", gate$source_families %||% character())
  dina_print_gate_field("Tasks", gate$tasks %||% character())
  if (dina_session_show_old_refs(session)) {
    dina_print_gate_field("Old-reference notes", gate$old_refs %||% character())
  } else if (length(dina_source_values(gate$old_refs %||% character()))) {
    dina_cli_alert("Old-reference notes hidden. Run `dina update prefs old-refs on` to show them.")
  }
  if (identical(gate$id %||% "", "parameters") && !is.null(session)) {
    dina_print_parameter_summary(session, root)
  }

  dina_cli_cat("")
  dina_cli_cat("Checks:")
  for (check in gate$checks %||% list()) {
    record <- dina_gate_check_record(session, gate$id %||% "", check$id %||% "")
    check_status <- record$status %||% "pending"
    line <- sprintf("  [%s] %s - %s", check_status, check$id %||% "", check$label %||% "")
    dina_cli_cat(line)
    if (nzchar(record$note %||% "")) {
      dina_cli_cat(sprintf("      note: %s", record$note))
    }
  }

  inbox_families <- dina_source_values(gate$source_families %||% character())
  if (length(inbox_families)) {
    dina_cli_cat("")
    guide <- dina_sources_inbox_guide_rows(root)
    guide <- guide[guide$family %in% inbox_families, , drop = FALSE]
    dina_print_source_inbox_guide(guide, root = root)

    inbox <- dina_sources_inbox_rows(root)
    inbox <- inbox[inbox$family %in% inbox_families, , drop = FALSE]
    dina_print_source_inbox_rows(inbox)
  }

  dina_print_gate_field("Suggested commands", gate$commands %||% character())
  next_check <- dina_gate_next_check(gate, session)
  if (!is.null(next_check)) {
    dina_cli_ok(sprintf("To record this check: dina update mark %s/%s --status done", gate$id, next_check$id))
  }
  invisible(gate)
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
  dina_cli_alert("Source status is diagnostic; update progress is recorded with `dina update mark GATE/CHECK`.")
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
    show_old_refs <- dina_cli_old_refs_choice(flags, current = TRUE)
    session <- dina_update_start(
      year = year,
      id = plan$id,
      root = root,
      source_hash = !isTRUE(flags[["no-source-hash"]]),
      show_old_refs = show_old_refs,
      progress = dina_cli_progress
    )
    dina_cli_ok(sprintf("Started update session %s", session$id))
    dina_cli_alert(sprintf("Session directory: %s", dina_relative(dina_update_dir(session$id, root), root)))
    dina_cli_alert(sprintf("Source baseline hash mode: %s", session$source_baseline$hash_mode %||% "none"))
    dina_cli_ok("Recommended next action: dina update roadmap")
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
    dina_print_update_gate_summary(session, root)
    dina_cli_ok(sprintf("Recommended next action: %s", state$recommendation))
  } else if (identical(sub, "roadmap")) {
    session <- dina_load_session(root = root)
    dina_print_update_roadmap(session, root)
  } else if (identical(sub, "gate")) {
    session <- dina_load_session(root = root)
    gate_id <- dina_arg(rest, 1L, NULL)
    effective_gate <- gate_id
    if (is.null(effective_gate) || !nzchar(effective_gate)) {
      next_gate <- dina_next_gate_status(session, root)
      effective_gate <- next_gate$id %||% "parameters"
    }
    if (identical(effective_gate, "parameters") && !is.null(session) && isatty(stdin())) {
      dina_update_parameters_wizard(session, root)
    } else {
      dina_print_update_gate(session, root, gate_id)
    }
  } else if (identical(sub, "mark")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(rest)
    target <- dina_arg(flags$positional, 1L, NULL)
    status <- flags$status %||% dina_arg(flags$positional, 2L, NULL)
    if (is.null(target) || is.null(status)) {
      stop("Usage: dina update mark GATE/CHECK --status done|deferred|needs-code [--note TEXT]", call. = FALSE)
    }
    record <- dina_update_mark_gate(session, root = root, target = target, status = status, note = flags$note %||% "")
    dina_cli_ok(sprintf("Recorded %s/%s: %s", record$gate, record$check, record$status))
    if (nzchar(record$note %||% "")) dina_cli_alert(sprintf("Note: %s", record$note))
  } else if (identical(sub, "unmark")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    target <- dina_arg(rest, 1L, NULL)
    if (is.null(target)) stop("Usage: dina update unmark GATE/CHECK", call. = FALSE)
    result <- dina_update_unmark_gate(session, root = root, target = target)
    if (isTRUE(result$removed)) {
      dina_cli_ok(sprintf("Cleared %s/%s", result$gate, result$check))
    } else {
      dina_cli_warn(sprintf("No record existed for %s/%s", result$gate, result$check))
    }
  } else if (identical(sub, "prefs")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    topic <- dina_arg(rest, 1L, NULL)
    value <- dina_arg(rest, 2L, NULL)
    if (is.null(topic)) {
      dina_cli_header("Update Preferences")
      dina_cli_cat(sprintf("%-16s %s", "old-refs", if (dina_session_show_old_refs(session)) "on" else "off"))
      dina_cli_alert("Change with `dina update prefs old-refs on` or `dina update prefs old-refs off`.")
    } else if (identical(topic, "old-refs") && value %in% c("on", "off")) {
      session <- dina_update_set_old_refs(session, root, show = identical(value, "on"))
      dina_cli_ok(sprintf("Old-reference notes are now %s.", if (dina_session_show_old_refs(session)) "on" else "off"))
    } else {
      stop("Usage: dina update prefs\n       dina update prefs old-refs on|off", call. = FALSE)
    }
  } else if (identical(sub, "config")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    action <- dina_arg(rest, 1L, "show")
    if (identical(action, "show")) {
      dina_need("yaml")
      override_path <- dina_session_config_override_path(session$id, root)
      dina_cli_header("Update Config")
      dina_cli_cat("Benchmark config/dina.yml:")
      cat(yaml::as.yaml(dina_config(root, expand_env = FALSE)))
      dina_cli_cat("Working override:")
      if (file.exists(override_path)) {
        cat(yaml::as.yaml(dina_config_override(session, root)))
      } else {
        dina_cli_alert(sprintf("%s has not been created yet.", dina_relative(override_path, root)))
      }
      dina_cli_cat("Effective config:")
      cat(yaml::as.yaml(dina_session_config(session, root, expand_env = FALSE)))
    } else if (identical(action, "set")) {
      key <- dina_arg(rest, 2L, NULL)
      value <- if (length(rest) >= 3L) paste(rest[-c(1L, 2L)], collapse = " ") else NULL
      if (is.null(key) || is.null(value)) {
        stop("Usage: dina update config set KEY VALUE", call. = FALSE)
      }
      session <- dina_session_config_set(session, root = root, key = key, value = value)
      dina_cli_ok(sprintf("Set working override %s", key))
      dina_cli_alert(sprintf("Override: %s", session$config_override))
    } else {
      stop("Usage: dina update config show\n       dina update config set KEY VALUE", call. = FALSE)
    }
  } else if (identical(sub, "repo-status")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(rest)
    comparison <- dina_repo_state_compare(session, root, baseline = flags$baseline %||% "start")
    dina_print_repo_status(comparison)
  } else if (identical(sub, "repo-diff")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(rest)
    any_mode <- isTRUE(flags$stat) || isTRUE(flags$patch) || isTRUE(flags$files)
    dina_print_repo_diff(
      session,
      root = root,
      baseline = flags$baseline %||% "start",
      stat = isTRUE(flags$stat) || !any_mode,
      patch = isTRUE(flags$patch),
      files = isTRUE(flags$files)
    )
  } else if (identical(sub, "repo-restore")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(rest)
    result <- dina_repo_state_restore(session, root = root, baseline = flags$baseline %||% "start", yes = isTRUE(flags$yes))
    dina_print_repo_restore(result)
  } else if (identical(sub, "checklist")) {
    stop("Unknown update command: checklist. Use `dina update roadmap`.", call. = FALSE)
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
      stop("Usage: dina update restart [ID] [--yes] [--replace-repo-baseline]", call. = FALSE)
    }
    if (isTRUE(flags[["replace-repo-baseline"]]) && isTRUE(flags[["preserve-repo-baseline"]])) {
      stop("Choose either --replace-repo-baseline or --preserve-repo-baseline, not both.", call. = FALSE)
    }
    repo_policy <- if (isTRUE(flags[["replace-repo-baseline"]])) "replace" else "preserve"
    old_refs_flag <- if (isTRUE(flags[["old-refs"]])) TRUE else if (isTRUE(flags[["no-old-refs"]])) FALSE else NULL
    update_id <- dina_arg(flags$positional, 1L, NULL)
    if (isTRUE(flags$yes)) {
      dina_cli_header("Update Restart")
    }
    result <- dina_update_restart(
      update_id,
      root = root,
      yes = isTRUE(flags$yes),
      repo_policy = repo_policy,
      show_old_refs = old_refs_flag,
      progress = if (isTRUE(flags$yes)) dina_cli_progress else NULL
    )
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
      if (length(result$repo_baselines %||% character())) {
        dina_cli_alert(sprintf("Existing repo baselines: %s", paste(result$repo_baselines, collapse = ", ")))
      } else {
        dina_cli_alert("Existing repo baselines: none")
      }
      dina_cli_alert("Default repo policy: preserve original baseline and add a restart snapshot.")
      dina_cli_alert("Restart reuses the same update id; no suffixed session will be created.")
      if (!dina_confirm_continue()) {
        dina_cli_alert("No changes made. Pass --yes for non-interactive restart.")
        return(invisible(result))
      }
      preserve <- dina_prompt_yes_no("Preserve original repo baseline? [Y/n] ", default = TRUE)
      repo_policy <- if (isTRUE(preserve)) "preserve" else "replace"
      old_refs <- dina_cli_old_refs_choice(flags, current = dina_session_show_old_refs(dina_load_session(result$id, root)))
      dina_cli_header("Update Restart")
      result <- dina_update_restart(result$id, root = root, yes = TRUE, repo_policy = repo_policy, show_old_refs = old_refs, progress = dina_cli_progress)
      dina_cli_ok(sprintf("Restarted update %s from scratch.", result$id))
    } else {
      dina_cli_ok(sprintf("Restarted update %s from scratch.", result$id))
    }
  } else if (identical(sub, "finalize")) {
    flags <- dina_parse_flags(rest)
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    blockers <- dina_finalize_blockers(session, root)
    if (length(blockers) && !isTRUE(flags$force)) {
      result <- list(ok = FALSE, blockers = lapply(blockers, function(x) x$reasons))
    } else {
      promote <- isTRUE(flags$yes)
      if (!isTRUE(flags$yes)) {
        dina_cli_alert("Finalize can promote the effective update config back to config/dina.yml.")
        dina_cli_alert("Use `dina update finalize --yes` in scripts to promote without prompting.")
        promote <- dina_prompt_yes_no("Promote effective update config to benchmark? [y/N] ", default = FALSE)
      }
      result <- dina_finalize_update(session, root, force = isTRUE(flags$force), promote_config = promote)
    }
    if (isTRUE(result$ok)) {
      dina_cli_ok(sprintf("Finalized update. Snapshot: %s", result$snapshot_dir))
      dina_cli_alert(sprintf("Config promoted: %s", if (isTRUE(result$config_promoted)) "yes" else "no"))
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
  dina_print_source_field("inbox", source$inbox %||% character())
  dina_print_source_field("legacy_inbox", source$legacy_inbox %||% source$legacy_inboxes %||% character())
  dina_print_source_field("inbox_examples", source$inbox_examples %||% character())
  dina_print_source_field("destination", source$destination %||% character())
  dina_print_source_field("destinations", source$destinations %||% character())
  dina_print_source_field("staging_name", source$staging_name %||% character())
  dina_print_source_field("downloader", source$downloader %||% character())
  dina_print_source_field("transformer", source$transformer %||% character())
  dina_print_source_field("checks", source$checks %||% character())
  dina_print_source_field("notes", source$notes %||% character())
  invisible(source)
}

dina_refresh_shorten <- function(x, width = 28L) {
  x <- x %||% ""
  if (!nzchar(x) || nchar(x) <= width) {
    return(x)
  }
  paste0(substr(x, 1L, width - 3L), "...")
}

dina_refresh_path_label <- function(x) {
  values <- dina_source_values(x)
  if (!length(values)) {
    return("")
  }
  out <- basename(values[[1]])
  if (length(values) > 1L) {
    out <- sprintf("%s +%s", out, length(values) - 1L)
  }
  out
}

dina_refresh_canonical_label <- function(result) {
  latest <- result$canonical_latest %||% "none"
  sprintf("%s/%s latest:%s", result$canonical_found %||% 0L, result$canonical_patterns %||% 0L, latest)
}

dina_refresh_url_label <- function(result) {
  count <- result$url_count %||% length(result$urls %||% character())
  if (!count) {
    return("none")
  }
  if (count == 1L) {
    return("1 url [1]")
  }
  sprintf("%s urls [1-%s]", count, count)
}

dina_refresh_action <- function(result) {
  switch(
    result$status %||% "",
    will_fetch = "run refresh",
    already_staged = "review staged",
    staged = "review staged",
    failed = "check error",
    manual_needed = sprintf("stage: dina sources stage --source %s", result$id),
    script_needed = sprintf("run/review script, then stage --source %s", result$id),
    wid_pipeline = "visible online dependency",
    skipped = "review registry",
    "review"
  )
}

dina_print_refresh_group <- function(title, results, statuses, detail_label, detail_fn) {
  group <- results[vapply(results, function(result) result$status %in% statuses, logical(1))]
  if (!length(group)) {
    return(invisible(NULL))
  }
  dina_cli_cat("")
  dina_cli_cat(sprintf("%s:", title))
  dina_cli_cat(sprintf(
    "%-34s %-15s %-24s %-20s %-13s %s",
    "source", "status", detail_label, "canonical", "urls", "next"
  ))
  for (result in group) {
    detail <- detail_fn(result)
    dina_cli_cat(sprintf(
      "%-34s %-15s %-24s %-20s %-13s %s",
      dina_refresh_shorten(result$id, 34L),
      result$status %||% "",
      dina_refresh_shorten(detail, 24L),
      dina_refresh_shorten(dina_refresh_canonical_label(result), 20L),
      dina_refresh_url_label(result),
      dina_refresh_shorten(dina_refresh_action(result), 46L)
    ))
    if (!is.null(result$error)) {
      dina_cli_warn(sprintf("%s: %s", result$id, result$error))
    }
  }
}

dina_print_source_refresh_url_appendix <- function(results, include_urls = FALSE) {
  with_urls <- results[vapply(results, function(result) length(result$urls %||% character()) > 0L, logical(1))]
  if (!length(with_urls)) {
    return(invisible(NULL))
  }
  dina_cli_cat("")
  dina_cli_cat("URL appendix:")
  index <- 1L
  for (result in with_urls) {
    urls <- result$urls %||% character()
    shown <- if (isTRUE(include_urls)) urls else urls[[1]]
    for (url in shown) {
      dina_cli_cat(sprintf("  [%s] %s: %s", index, result$id, url))
      index <- index + 1L
    }
    if (!isTRUE(include_urls) && length(urls) > 1L) {
      dina_cli_cat(sprintf("      %s more URL(s): dina sources show %s --urls", length(urls) - 1L, result$id))
    }
  }
}

dina_print_source_refresh_results <- function(results, session = NULL, root = dina_repo_root(), dry_run = FALSE, include_urls = FALSE) {
  if (!is.null(session)) {
    dina_cli_alert(sprintf("Active update: %s", session$id %||% ""))
    dina_cli_alert(sprintf("Mode: %s", if (isTRUE(dry_run)) "dry-run (no folders, downloads, or records)" else "refresh"))
    dina_cli_alert(sprintf("Staging root: %s", dina_relative(dina_source_staging_root(session, root), root)))
    dina_cli_alert("Targets in the table are relative to the staging root.")
  }
  dina_print_refresh_group(
    "Fetchable now",
    results,
    c("will_fetch", "staged", "already_staged", "failed"),
    "target",
    function(result) result$target_rel %||% ""
  )
  dina_print_refresh_group(
    "Manual download/stage",
    results,
    "manual_needed",
    "target",
    function(result) result$target_rel %||% ""
  )
  dina_print_refresh_group(
    "Script acquisition",
    results,
    "script_needed",
    "downloader",
    function(result) dina_refresh_path_label(result$downloader)
  )
  dina_print_refresh_group(
    "Pipeline online dependency",
    results,
    "wid_pipeline",
    "transformer",
    function(result) dina_refresh_path_label(result$transformer)
  )
  dina_print_refresh_group(
    "Skipped",
    results,
    "skipped",
    "target",
    function(result) result$target_rel %||% ""
  )
  dina_print_source_refresh_url_appendix(results, include_urls = include_urls)
  if (isTRUE(dry_run)) {
    dina_cli_ok("Dry-run only: no folders, downloads, or session records were written.")
    dina_cli_alert("Run `dina sources refresh` to fetch URL/ZIP sources, or `dina sources stage --source ID --file PATH` for manual files.")
  } else {
    dina_cli_ok("Next: run `dina sources review`, then `dina sources integrate`.")
  }
}

dina_print_source_review_rows <- function(rows) {
  dina_cli_header("Staged Sources")
  if (!nrow(rows)) {
    dina_cli_warn("No staged downloads or manual source files found.")
    return(invisible(rows))
  }
  ready <- rows[rows$destination_status == "ready", , drop = FALSE]
  needs <- rows[rows$destination_status != "ready", , drop = FALSE]
  print_rows <- function(title, data) {
    if (!nrow(data)) {
      return(invisible(NULL))
    }
    dina_cli_cat("")
    dina_cli_cat(sprintf("%s:", title))
    dina_cli_cat(sprintf("%-28s %-8s %-38s %-20s %s", "source", "method", "staged", "destination_status", "action"))
    for (i in seq_len(nrow(data))) {
      dina_cli_cat(sprintf(
        "%-28s %-8s %-38s %-20s %s",
        data$source_id[[i]],
        data$method[[i]],
        data$staged_rel[[i]],
        data$destination_status[[i]],
        data$action[[i]]
      ))
      if (!is.na(data$destination[[i]]) && nzchar(data$destination[[i]])) {
        dina_cli_cat(sprintf("  destination: %s", data$destination[[i]]))
      }
      if (!is.na(data$sha256[[i]]) && nzchar(data$sha256[[i]])) {
        dina_cli_cat(sprintf("  sha256: %s", data$sha256[[i]]))
      }
    }
  }
  print_rows("Ready for bulk integration", ready)
  print_rows("Needs manual integration target", needs)
  invisible(rows)
}

dina_print_source_inbox_guide <- function(rows, root = dina_repo_root(), include_urls = FALSE) {
  dina_cli_header("Source Inbox Guide")
  if (!nrow(rows)) {
    dina_cli_warn("No source inbox guidance matched.")
    return(invisible(rows))
  }
  bucket_rows <- unique(rows[c("family", "bucket", "bucket_exists")])
  dina_cli_cat(sprintf("%-18s %-32s %s", "family", "bucket", "state"))
  for (i in seq_len(nrow(bucket_rows))) {
    dina_cli_cat(sprintf(
      "%-18s %-32s %s",
      bucket_rows$family[[i]],
      bucket_rows$bucket[[i]],
      if (isTRUE(bucket_rows$bucket_exists[[i]])) "exists" else "missing"
    ))
  }

  dina_cli_cat("")
  dina_cli_cat(sprintf("%-24s %-14s %-30s %-22s %s", "source", "method", "examples", "destination", "urls"))
  for (i in seq_len(nrow(rows))) {
    url_label <- if (isTRUE(include_urls)) {
      if (rows$url_count[[i]] > 0L) "expanded below" else "none"
    } else if (rows$url_count[[i]] == 1L) {
      dina_refresh_shorten(rows$primary_url[[i]], 24L)
    } else {
      rows$url_refs[[i]]
    }
    destination <- if (identical(rows$destination_status[[i]], "ready")) {
      rows$destination[[i]]
    } else {
      rows$destination_status[[i]]
    }
    dina_cli_cat(sprintf(
      "%-24s %-14s %-30s %-22s %s",
      rows$source_id[[i]],
      rows$method[[i]],
      dina_refresh_shorten(rows$examples[[i]], 30L),
      dina_refresh_shorten(destination, 22L),
      url_label
    ))
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
      dina_cli_cat(sprintf("  %s", id))
      for (j in seq_len(nrow(entries))) {
        label <- entries$label[[j]]
        prefix <- if (nzchar(label)) sprintf("    - %s: ", label) else "    - "
        dina_cli_cat(sprintf("%s%s", prefix, entries$url[[j]]))
      }
    }
  } else {
    dina_cli_alert("Use `dina sources inbox guide --urls` or `dina sources show ID --urls` for clickable URLs.")
  }
  invisible(rows)
}

dina_print_source_inbox_init <- function(result) {
  title <- if (isTRUE(result$dry_run)) "Source Inbox Init Preview" else "Source Inbox Init"
  dina_cli_header(title)
  buckets <- result$buckets
  if (nrow(buckets)) {
    dina_cli_cat(sprintf("%-34s %s", "bucket", "status"))
    for (i in seq_len(nrow(buckets))) {
      dina_cli_cat(sprintf("%-34s %s", buckets$bucket[[i]], buckets$status[[i]]))
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
  invisible(result)
}

dina_print_source_inbox_rows <- function(rows) {
  dina_cli_header("Incoming Source Inbox")
  if (!nrow(rows)) {
    dina_cli_warn("No incoming files matched configured source inbox patterns.")
    return(invisible(rows))
  }
  dina_cli_cat(sprintf("%-24s %-8s %-11s %-52s %s", "source", "kind", "validation", "inbox", "destination"))
  for (i in seq_len(nrow(rows))) {
    dina_cli_cat(sprintf(
      "%-24s %-8s %-11s %-52s %s",
      rows$source_id[[i]],
      rows$kind[[i]],
      rows$validation[[i]],
      dina_refresh_shorten(rows$inbox[[i]], 52L),
      rows$destination[[i]]
    ))
    if (!identical(rows$validation_detail[[i]], "ok")) {
      dina_cli_cat(sprintf("  validation detail: %s", rows$validation_detail[[i]]))
    }
    if (!is.na(rows$sha256[[i]]) && nzchar(rows$sha256[[i]])) {
      dina_cli_cat(sprintf("  sha256: %s", rows$sha256[[i]]))
    }
  }
  invisible(rows)
}

dina_print_bulk_integration_results <- function(results, executed = FALSE) {
  if (!length(results)) {
    dina_cli_warn("No staged sources matched.")
    return(invisible(results))
  }
  for (result in results) {
    if (identical(result$status, "integrated")) {
      dina_cli_ok(sprintf("Integrated %s -> %s", result$staged, result$destination))
    } else if (identical(result$status, "would_integrate")) {
      dina_cli_alert(sprintf("Would integrate %s -> %s", result$staged, result$destination))
    } else {
      dina_cli_warn(sprintf("Skipped %s: %s. Action: %s", result$staged %||% result$source_id, result$reason %||% result$status, result$action %||% "review"))
    }
  }
  if (!isTRUE(executed)) {
    dina_cli_alert("Preview only. Pass --yes to integrate ready staged files.")
  }
}

dina_print_incoming_integration_results <- function(results, executed = FALSE) {
  if (!length(results)) {
    dina_cli_warn("No incoming sources matched.")
    return(invisible(results))
  }
  for (result in results) {
    if (identical(result$status, "integrated")) {
      dina_cli_ok(sprintf("Integrated incoming %s -> %s", result$incoming, result$destination))
      if (isTRUE(result$replaces_existing)) {
        dina_cli_warn("Existing destination was replaced.")
      }
    } else if (identical(result$status, "would_integrate")) {
      dina_cli_alert(sprintf("Would integrate incoming %s -> %s", result$incoming, result$destination))
      if (nzchar(result$validation %||% "")) {
        dina_cli_alert(sprintf("Validation: %s", result$validation))
      }
      if (isTRUE(result$replaces_existing)) {
        dina_cli_warn("Destination already exists and would be replaced with --yes.")
      }
    } else {
      dina_cli_warn(sprintf("Skipped %s: %s", result$incoming %||% result$source_id, result$reason %||% result$status))
    }
  }
  if (!isTRUE(executed)) {
    dina_cli_alert("Preview only. Pass --yes to copy incoming files into canonical input paths.")
  }
  invisible(results)
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
  } else if (identical(sub, "inbox")) {
    action <- dina_arg(args, 2L, "guide")
    flags <- dina_parse_flags(args[-c(1L, 2L)])
    if (identical(action, "guide")) {
      rows <- dina_sources_inbox_guide_rows(root, family = flags$family %||% NULL)
      dina_print_source_inbox_guide(rows, root = root, include_urls = isTRUE(flags$urls))
    } else if (identical(action, "init")) {
      result <- dina_sources_inbox_init(root, dry_run = isTRUE(flags[["dry-run"]]), migrate = TRUE)
      dina_print_source_inbox_init(result)
    } else {
      stop("Usage: dina sources inbox guide [--family FAMILY] [--urls]\n       dina sources inbox init [--dry-run]", call. = FALSE)
    }
  } else if (identical(sub, "stage")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(args[-1])
    source_id <- flags$source %||% NULL
    input <- flags$file %||% flags$dir %||% NULL
    if (is.null(source_id) || is.null(input)) {
      stop("Usage: dina sources stage --source ID --file PATH [--yes]\n       dina sources stage --source ID --dir PATH [--yes]", call. = FALSE)
    }
    record <- dina_sources_stage_path(session, source_id = source_id, input_path = input, root = root, overwrite = isTRUE(flags$yes))
    dina_cli_ok(sprintf("Staged %s -> %s", record$source_id, record$staged))
    dina_cli_alert(sprintf("sha256: %s", record$sha256))
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
    stop("Unknown sources command: complete. Record update progress with `dina update mark GATE/CHECK`.", call. = FALSE)
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
    rows <- dina_sources_review_rows(session, root)
    dina_print_source_review_rows(rows)
    dina_print_source_inbox_guide(dina_sources_inbox_guide_rows(root), root = root)
    dina_print_source_inbox_rows(dina_sources_inbox_rows(root))
  } else if (identical(sub, "refresh")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    dina_cli_header("Source Refresh")
    flags <- dina_parse_flags(args[-1])
    source_ids <- if (!is.null(flags$source)) strsplit(flags$source, ",", fixed = TRUE)[[1]] else NULL
    results <- dina_sources_refresh(session, root, source_ids = source_ids, dry_run = isTRUE(flags[["dry-run"]]))
    dina_print_source_refresh_results(
      results,
      session = session,
      root = root,
      dry_run = isTRUE(flags[["dry-run"]]),
      include_urls = isTRUE(flags$urls)
    )
  } else if (identical(sub, "integrate")) {
    session <- dina_load_session(root = root)
    if (is.null(session)) stop("No active update.", call. = FALSE)
    flags <- dina_parse_flags(args[-1])
    if (isTRUE(flags$incoming)) {
      results <- dina_sources_integrate_incoming(
        session,
        root = root,
        source_id = flags$source %||% NULL,
        all = isTRUE(flags$all),
        overwrite = isTRUE(flags$yes)
      )
      dina_print_incoming_integration_results(results, executed = isTRUE(flags$yes))
      return(invisible(results))
    }
    staged <- flags$staged %||% flags$file %||% NULL
    dest <- flags$to %||% NULL
    if (is.null(staged) && is.null(dest) && (isTRUE(flags$all) || !is.null(flags$source))) {
      results <- dina_sources_integrate_bulk(
        session,
        root = root,
        source_id = flags$source %||% NULL,
        all = isTRUE(flags$all),
        overwrite = isTRUE(flags$yes)
      )
      dina_print_bulk_integration_results(results, executed = isTRUE(flags$yes))
      return(invisible(results))
    }
    if (is.null(staged) || is.null(dest)) {
      dina_cli_warn("Usage: dina sources integrate --staged RELPATH --to input_data/... [--source ID] [--yes]\n       dina sources integrate --source ID [--yes]\n       dina sources integrate --all [--yes]")
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
  } else if (identical(sub, "stata")) {
    flags <- dina_parse_flags(args[-1])
    path <- flags$output %||% dina_arg(flags$positional, 1L, NULL)
    if (is.null(path) || !nzchar(path)) {
      stop("Usage: dina config stata --output PATH", call. = FALSE)
    }
    full <- if (grepl("^/", path)) path else file.path(root, path)
    dina_render_config_do(dina_config(root, expand_env = FALSE), full)
    dina_cli_ok(sprintf("Wrote explicit Stata runtime config %s", dina_relative(full, root)))
    dina_cli_alert(sprintf("Use it with: export DINA_CONFIG_DO=\"%s\"", full))
  } else if (identical(sub, "render")) {
    stop("Unknown config command: render. Use `dina config stata --output PATH` for manual Stata export.", call. = FALSE)
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
