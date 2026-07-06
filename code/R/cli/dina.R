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
  dina menu commands
  dina COMMAND [SUBCOMMAND] [OPTIONS]

Plain `dina` opens the guided dashboard and recommends the next action.
Use `dina commands` or `dina navigate` to browse available commands.
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
  `buckets [detail|urls|uses|fetch]`   [read-only/writes inbox] source buckets
  `sources review`                    [read-only] inspect staged/_new files
  `sources integrate --incoming`      [writes files] accept _new inbox files
  `sources list|show ID`              [read-only] inspect source registry
  `sources status|scan|diff`          [read-only] inspect source coverage
  `sources refresh [--dry-run]`       [read-only/writes session] fetch URL/ZIP
  `sources inbox guide|init`          [read-only/writes dirs] compatibility
  `sources integrate`                 [writes files] copy approved inputs

Pipeline:
  `tasks list|why TASK`               [read-only] inspect task freshness
  `run ... --dry-run`                 [read-only] preview selected scripts
  `run ... --execute`                 [writes files] run selected scripts

Navigation:
  `commands|navigate`                 [interactive] browse available commands
  `menu commands`                     [interactive] open command navigator

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
  dina
  dina commands
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
      prepares source inbox buckets, prepares a working override location,
      records a repo-state baseline, and makes the session active.

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
  dina help workflow   dina help commands   dina help update
  dina help sources    dina help run        dina help tasks
  dina help config     dina help data
",
    commands = "Usage:
  dina commands
  dina navigate
  dina menu commands

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
  dina menu commands
",
    navigate = dina_help_text("commands"),
    menu = "Usage:
  dina menu
  dina menu commands

What it does:
  `dina menu` opens the compact dashboard menu. `dina menu commands` opens the
  full command navigator.

Examples:
  dina menu
  dina menu commands
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
  dina update config show|set|edit
  dina update repo-status [--baseline NAME]
  dina update repo-diff [--stat] [--patch] [--files] [--baseline NAME]
  dina update repo-restore [--dry-run] [--yes] [--baseline NAME]
  dina update list
  dina update restart [ID] [--yes] [--replace-repo-baseline] [--save-restart-checkpoint]
  dina update delete [ID] [--yes]
  dina update finalize [--force] [--yes]

What it manages:
  Annual update sessions under `output/updates/<update_id>`. A session stores
  a working config override when needed, repo-state baselines, source scans,
  gate records, task run records, and final manifests. Update progress is
  recorded through roadmap gate checks.

Subcommands:
  start [YEAR]                    Creates a new session, active pointer, and
                                  hashed source baseline. It prepares central
                                  `input_data/_new` buckets, uses benchmark
                                  `config/dina.yml` plus an optional working
                                  override, and records a recoverable repo-state
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
                                  unfinished gate. `gate parameters` opens the
                                  active update YAML in an interactive terminal
                                  and is read-only in scripts.
  mark GATE/CHECK                 Records a gate check as done, deferred, or
                                  needs-code.
  unmark GATE/CHECK               Clears a previously recorded gate check.
  prefs                           Shows session preferences.
  prefs old-refs on|off           Shows or hides old-reference notes on gate
                                  pages.
  config show|set|edit            Inspects, edits, or writes only the active
                                  session YAML override. Benchmark config
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
                                  ask before resetting. It keeps
                                  `input_data/_new` buckets by default. By
                                  default it keeps the original start baseline
                                  without saving a bulky checkpoint; use
                                  --save-restart-checkpoint to save one, or
                                  --replace-repo-baseline to make current repo
                                  state the new start baseline.
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
  records. `update config set` and `update config edit` write only the working
  override. `repo-restore --yes`
  restores captured code/config/docs outside excluded data roots. `resume`,
  `status`, `roadmap`, `gate`, `repo-status`, `repo-diff`, and `list` inspect.

Options:
  --no-source-hash                For start, record only file size/timestamp in
                                  the source baseline. The default computes
                                  source hashes for later comparison.
  --old-refs / --no-old-refs      For start/restart, set Old-reference notes.
  --replace-repo-baseline         For restart, discard the original repo
                                  baseline and capture current repo state.
  --save-restart-checkpoint       For restart, keep the start baseline and also
                                  save current code/config/docs before reset.
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
  dina update config edit
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
  dina buckets [--family FAMILY]
  dina buckets detail [--family FAMILY]
  dina buckets urls [--family FAMILY]
  dina buckets uses [--family FAMILY] [BUCKET|SOURCE]
  dina buckets fetch [--family FAMILY] [--source ID] [--dry-run]
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
  Preferred path: use `dina buckets` for central `input_data/_new` buckets,
  expected files, source URLs, code usage, and supported public fetchers.

  Subcommands:
  list                            Shows compact registry rows: id, family,
                                  country, method, path count, URL presence,
                                  downloader presence, and transformer presence.
  show ID                         Shows the full registry entry for one source,
                                  including URLs, canonical paths, scripts,
                                  checks, and notes.
  methods                         Explains source acquisition method labels.
  inbox guide                     Compatibility view for `dina buckets detail`;
                                  prefer bucket commands in guided workflows.
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
  Inbox buckets are created only for sources with explicit `inbox` or
  `legacy_inbox` patterns. URLs alone do not create buckets.
  `_new` folders are treated as manual inboxes. Pipeline scripts consume
  canonical paths only, never `_new` directly.

Methods:
  url                             Direct URL fetchable by `sources refresh`.
  zip                             Direct archive URL fetchable by `sources refresh`.
  script                          Custom acquisition script exists.
  manual                          Human-curated input or URL index.
  wid                             Data currently acquired through Stata/WID calls.

Examples:
  dina buckets
  dina buckets detail
  dina buckets urls
  dina buckets uses
  dina buckets fetch --dry-run
  dina buckets detail --family admin-data
  dina sources review
  dina sources integrate --incoming --source chl-pit-total
  dina sources list --urls
  dina sources list --country CHL --urls
  dina sources list country CHL
  dina sources list family admin-data
  dina sources list method manual
  dina sources methods
  dina sources show country-sna-bra --urls
  dina sources show col-pit-total --urls
  dina sources show surveys-cepal --urls
  dina sources inbox guide
  dina sources inbox guide --family admin_tax --urls
  dina sources inbox init --dry-run
  dina sources status
  dina sources refresh --dry-run --urls
  dina sources refresh --source chl-pit-total
  dina sources list method manual --urls
  dina sources integrate --source chl-pit-total
  dina sources integrate --all --yes
  dina sources scan --deep
  dina sources integrate --staged CHL/file.xlsx --to input_data/admin_data/CHL/file.xlsx --yes
",
    buckets = "Usage:
  dina buckets [--family FAMILY]
  dina bucket [--family FAMILY]
  dina buckets detail [--family FAMILY]
  dina buckets urls [--family FAMILY]
  dina buckets uses [--family FAMILY] [BUCKET|SOURCE]
  dina buckets fetch [--family FAMILY] [--source ID] [--dry-run]

What it manages:
  Compact source inbox bucket visibility. Default output shows only bucket path,
  status, and current incoming-file count. Details and URLs are opt-in.

Subcommands:
  buckets                         Shows bucket paths, status, and file counts.
  buckets detail                  Shows expected files and destinations.
  buckets urls                    Shows expected files, destinations, and URLs.
  buckets uses                    Shows code/tasks that use expected files.
  buckets fetch                   Populates supported buckets for review.

Options:
  --family FAMILY                 Limit to one source family or friendly group,
                                  such as admin-data.

Notes:
  Buckets are created only for explicit `inbox` or `legacy_inbox` patterns.
  URLs alone do not create buckets.
  Fetch writes only to `input_data/_new`; humans still review and integrate.
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
  result <- dina_menu_select(
    title = "Old-Reference Notes",
    items = list(
      dina_menu_action(
        "show",
        "Show old-reference notes",
        value = TRUE,
        description = "Display reminders from older manual update notes on gate pages.",
        help = "Useful while comparing old notes with the current gate workflow."
      ),
      dina_menu_action(
        "hide",
        "Hide old-reference notes",
        value = FALSE,
        description = "Keep gate pages focused on current checks and commands.",
        help = "Does not delete notes. Turn them back on with `dina update prefs old-refs on`."
      )
    ),
    prompt = "Show old-reference notes in gate pages?",
    default = isTRUE(current),
    allow_quit = TRUE,
    input = input,
    is_terminal = TRUE
  )
  if (identical(result, "quit") || is.null(result)) {
    return(isTRUE(current))
  }
  isTRUE(result)
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
  parts <- c("Up/Down move", "Enter select")
  if (!is.null(items)) {
    has_back <- !is.na(dina_menu_back_index(items))
    has_next <- !is.na(dina_menu_next_index(items))
    has_right <- any(!vapply(items, function(item) is.null(item$right), logical(1)))
    if (has_right && has_back) {
      parts <- c(parts, "Left back", "Right open")
    } else if (has_right) {
      parts <- c(parts, "Right open")
    } else if (has_back && has_next) {
      parts <- c(parts, "Left/Right back/next")
    } else if (has_back) {
      parts <- c(parts, "Left back")
    } else if (has_next) {
      parts <- c(parts, "Right next")
    }
  } else {
    parts <- c(parts, "Left/Right back/next when available")
  }
  paste(c(parts, "number selects", "q quits", "? help"), collapse = " - ")
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

dina_menu_lines <- function(title, items, selected = NULL, prompt = "Choose an action", help = FALSE) {
  lines <- c("", title)
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

dina_menu_select_numbered <- function(title, items, prompt = "Choose an action", default = NULL, allow_quit = TRUE, input = "stdin", is_terminal = isatty(stdin())) {
  visible <- dina_menu_visible_indices(items)
  selected_default <- dina_menu_index_for_value(items, default)
  if (!is.na(selected_default) && isTRUE(items[[selected_default]]$hidden)) {
    selected_default <- NA_integer_
  }
  if (is.na(selected_default)) {
    selected_default <- dina_menu_first_enabled(items)
  }
  repeat {
    for (line in dina_menu_lines(title, items, selected = NULL, prompt = prompt)) {
      dina_cli_cat(line)
    }
    default_visible <- match(selected_default, visible)
    default_label <- if (!is.na(default_visible)) as.character(default_visible) else if (isTRUE(allow_quit)) "q" else ""
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

dina_menu_select_raw <- function(title, items, prompt = "Choose an action", default = NULL, allow_quit = TRUE) {
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
  selected <- dina_menu_index_for_value(items, default)
  if (is.na(selected)) {
    selected <- dina_menu_first_enabled(items)
  }
  if (is.na(selected)) {
    return(if (isTRUE(allow_quit)) "quit" else NULL)
  }
  show_help <- FALSE
  enabled <- dina_menu_enabled_indices(items)
  repeat {
    screen <- paste(
      dina_menu_lines(title, items, selected = selected, prompt = prompt, help = show_help),
      collapse = "\r\n"
    )
    cat("\033[2J\033[H", screen, "\r\n", sep = "")
    flush.console()
    key <- dina_menu_read_key(con)
    show_help <- FALSE
    if (identical(key, "\033[A")) {
      current <- match(selected, enabled)
      selected <- enabled[[if (current <= 1L) length(enabled) else current - 1L]]
    } else if (identical(key, "\033[B")) {
      current <- match(selected, enabled)
      selected <- enabled[[if (current >= length(enabled)) 1L else current + 1L]]
    } else if (identical(key, "\033[D")) {
      idx <- dina_menu_back_index(items)
      if (!is.na(idx)) return(dina_menu_action_value(items[[idx]]))
    } else if (identical(key, "\033[C")) {
      if (!is.null(items[[selected]]$right)) return(items[[selected]]$right)
      idx <- dina_menu_next_index(items)
      if (!is.na(idx)) return(dina_menu_action_value(items[[idx]]))
    } else if (identical(key, "\r") || identical(key, "\n")) {
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

dina_menu_select <- function(title, items, prompt = "Choose an action", default = NULL, allow_quit = TRUE, input = "stdin", is_terminal = isatty(stdin())) {
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
    raw <- dina_menu_select_raw(title, items, prompt = prompt, default = default, allow_quit = allow_quit)
    if (!is.null(raw)) {
      return(raw)
    }
  }
  dina_menu_select_numbered(title, items, prompt = prompt, default = default, allow_quit = allow_quit, input = input, is_terminal = is_terminal)
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

dina_edit_read_key <- function(con) {
  repeat {
    key <- readChar(con, nchars = 1L, useBytes = TRUE)
    if (nzchar(key)) {
      break
    }
  }
  if (identical(key, "\033")) {
    rest <- paste0(
      readChar(con, nchars = 1L, useBytes = TRUE),
      readChar(con, nchars = 1L, useBytes = TRUE)
    )
    if (!nzchar(rest)) {
      return("\033")
    }
    return(paste0(key, rest))
  }
  key
}

dina_edit_render <- function(prompt, buffer, cursor) {
  text <- paste(buffer, collapse = "")
  cat("\r\033[2K", prompt, text, sep = "")
  tail <- length(buffer) - cursor
  if (tail > 0L) {
    cat(sprintf("\033[%sD", tail))
  }
  flush.console()
}

dina_menu_edit_text_raw <- function(title, prompt, current = "") {
  state <- dina_menu_tty_state()
  if (!length(state)) {
    return(NULL)
  }
  con <- dina_menu_open_tty_binary()
  if (is.null(con)) {
    return(NULL)
  }
  on.exit(close(con), add = TRUE)
  dina_menu_set_raw_poll()
  on.exit(dina_menu_restore_tty(state), add = TRUE)

  dina_cli_cat("")
  dina_cli_cat(title)
  dina_cli_cat(dina_cli_dim("Enter saves the shown value. Esc cancels this edit without changes."))
  buffer <- strsplit(current %||% "", "", fixed = TRUE)[[1]]
  if (length(buffer) == 1L && !nzchar(buffer)) {
    buffer <- character()
  }
  cursor <- length(buffer)
  full_prompt <- sprintf("%s: ", prompt)
  repeat {
    dina_edit_render(full_prompt, buffer, cursor)
    key <- dina_edit_read_key(con)
    if (identical(key, "\033")) {
      cat("\n")
      return(list(value = current, quit = FALSE, cancel = TRUE, changed = FALSE))
    }
    if (identical(key, "\r") || identical(key, "\n")) {
      cat("\n")
      value <- paste(buffer, collapse = "")
      return(list(value = value, quit = FALSE, cancel = FALSE, changed = !identical(value, current %||% "")))
    }
    if (identical(key, "\033[D")) {
      cursor <- max(0L, cursor - 1L)
      next
    }
    if (identical(key, "\033[C")) {
      cursor <- min(length(buffer), cursor + 1L)
      next
    }
    if (identical(key, "\001")) {
      cursor <- 0L
      next
    }
    if (identical(key, "\005")) {
      cursor <- length(buffer)
      next
    }
    if (identical(key, "\025")) {
      buffer <- character()
      cursor <- 0L
      next
    }
    if (key %in% c("\177", "\b")) {
      if (cursor > 0L) {
        buffer <- buffer[-cursor]
        cursor <- cursor - 1L
      }
      next
    }
    if (identical(key, "\033[3")) {
      invisible(readChar(con, nchars = 1L, useBytes = TRUE))
      if (cursor < length(buffer)) {
        buffer <- buffer[-(cursor + 1L)]
      }
      next
    }
    if (nchar(key, type = "bytes") == 1L && grepl("^[[:print:]]$", key)) {
      if (cursor == 0L) {
        buffer <- c(key, buffer)
      } else if (cursor >= length(buffer)) {
        buffer <- c(buffer, key)
      } else {
        buffer <- c(buffer[seq_len(cursor)], key, buffer[(cursor + 1L):length(buffer)])
      }
      cursor <- cursor + 1L
    }
  }
}

dina_menu_edit_text <- function(title = "Edit", prompt, current = "", input = "stdin", is_terminal = isatty(stdin()), allow_quit = TRUE) {
  current <- current %||% ""
  if (!isTRUE(is_terminal)) {
    return(list(value = current, quit = FALSE, cancel = FALSE, changed = FALSE))
  }
  dina_cli_cat("")
  dina_cli_cat(title)
  dina_cli_cat(dina_cli_dim("Type a replacement value. Enter keeps current. `.` cancels this edit. `q` exits the gate."))
  dina_cli_cat(sprintf("%s %s", dina_cli_dim("Current:"), current))
  answer <- trimws(dina_read_prompt(sprintf("%s: ", prompt), input = input))
  if (isTRUE(allow_quit) && tolower(answer) %in% c("q", "quit", "exit")) {
    return(list(value = current, quit = TRUE, cancel = FALSE, changed = FALSE))
  }
  if (tolower(answer) %in% c(".", "cancel")) {
    return(list(value = current, quit = FALSE, cancel = TRUE, changed = FALSE))
  }
  if (!nzchar(answer)) {
    return(list(value = current, quit = FALSE, cancel = FALSE, changed = FALSE))
  }
  list(value = answer, quit = FALSE, cancel = FALSE, changed = !identical(answer, current))
}

dina_cli_config_value <- function(x) {
  values <- dina_source_values(x)
  if (!length(values)) {
    return("")
  }
  paste(values, collapse = ",")
}

dina_parameter_value <- function(config, key) {
  parts <- strsplit(key, "\\.", fixed = FALSE)[[1]]
  value <- config
  for (part in parts) {
    if (is.null(value[[part, exact = TRUE]])) {
      return(NULL)
    }
    value <- value[[part, exact = TRUE]]
  }
  value
}

dina_parameter_display_value <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return("")
  }
  if (is.logical(value)) {
    return(ifelse(value, "true", "false"))
  }
  values <- dina_source_values(value)
  if (length(values)) {
    return(paste(values, collapse = ","))
  }
  paste(as.character(value), collapse = ",")
}

dina_parameter_sections <- function() {
  list(
    list(
      title = "Global parameters",
      params = list(
        list(key = "countries", note = "ISO3 countries included in update runs.", input = "comma-separated ISO3 codes"),
        list(key = "years.first", note = "First year used by pipeline loops and Stata globals.", input = "year"),
        list(key = "years.last", note = "Last observed year used by main pipeline loops.", input = "year"),
        list(key = "run.units", note = "Population/income units processed in run loops.", input = "comma-separated units"),
        list(key = "run.steps", note = "Income concepts processed in run loops.", input = "comma-separated steps")
      )
    ),
    list(
      title = "WID export parameters",
      params = list(
        list(key = "export_validation.unit", note = "Unit used by 07d WID export comparison.", input = "unit"),
        list(key = "export_validation.steps", note = "Steps included in final WID export validation.", input = "comma-separated steps"),
        list(key = "export_validation.last_year", note = "Last year inserted for 07d interpolation/extrapolation.", input = "year"),
        list(key = "export_validation.previous_update_date", note = "Date label for the prior WID series comparison.", input = "date label, e.g. 3Oct2024"),
        list(key = "export_validation.previous_update_file", note = "Prior WID-format .dta loaded by 07d.", input = "relative path to .dta")
      )
    )
  )
}

dina_all_parameter_specs <- function() {
  unlist(lapply(dina_parameter_sections(), function(section) section$params), recursive = FALSE)
}

dina_extract_wid_update_date <- function(path) {
  base <- basename(path %||% "")
  match <- regexpr("[0-9]{1,2}(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[0-9]{4}", base, ignore.case = TRUE)
  if (match < 0L) {
    return("")
  }
  raw <- regmatches(base, match)
  day <- sub("^([0-9]{1,2}).*$", "\\1", raw)
  month <- sub("^[0-9]{1,2}([A-Za-z]{3})[0-9]{4}$", "\\1", raw)
  year <- sub("^[0-9]{1,2}[A-Za-z]{3}([0-9]{4})$", "\\1", raw)
  paste0(day, toupper(substr(month, 1L, 1L)), tolower(substr(month, 2L, 3L)), year)
}

dina_previous_update_candidates <- function(root = dina_repo_root()) {
  source <- NULL
  for (candidate in dina_sources(root)$sources) {
    if (identical(candidate$id %||% "", "previous-series")) {
      source <- candidate
      break
    }
  }
  if (is.null(source)) {
    return(data.frame(path = character(), rel = character(), source = character(), mtime = as.POSIXct(character()), stringsAsFactors = FALSE))
  }
  collect <- function(patterns, source_name) {
    paths <- dina_expand_paths(patterns, root = root)
    paths <- paths[file.exists(paths) & !dir.exists(paths)]
    paths <- paths[tolower(tools::file_ext(paths)) == "dta"]
    if (!length(paths)) {
      return(data.frame(path = character(), rel = character(), source = character(), mtime = as.POSIXct(character()), stringsAsFactors = FALSE))
    }
    info <- file.info(paths)
    data.frame(
      path = normalizePath(paths, mustWork = FALSE),
      rel = dina_relative(paths, root),
      source = source_name,
      mtime = as.POSIXct(info$mtime, origin = "1970-01-01"),
      stringsAsFactors = FALSE
    )
  }
  rows <- rbind(
    collect(dina_source_values(source$canonical %||% character()), "canonical"),
    collect(dina_source_inbox_patterns(source), "inbox")
  )
  if (!nrow(rows)) {
    return(rows)
  }
  rows$rank <- ifelse(rows$source == "canonical", 0L, 1L)
  rows$mtime_rank <- -as.numeric(rows$mtime)
  rows <- rows[order(rows$mtime_rank, rows$rank), , drop = FALSE]
  rows[!duplicated(rows$rel), c("path", "rel", "source", "mtime"), drop = FALSE]
}

dina_parameter_suggestions <- function(config, root = dina_repo_root()) {
  suggestions <- list()
  current_last <- suppressWarnings(as.integer(dina_parameter_value(config, "years.last") %||% NA_integer_))
  if (!is.na(current_last)) {
    suggestions[["years.last"]] <- as.character(current_last + 1L)
  }
  export_last <- suppressWarnings(as.integer(dina_parameter_value(config, "export_validation.last_year") %||% NA_integer_))
  if (!is.na(export_last)) {
    suggestions[["export_validation.last_year"]] <- as.character(export_last + 1L)
  }
  previous <- dina_previous_update_candidates(root)
  if (nrow(previous)) {
    suggestions[["export_validation.previous_update_file"]] <- previous$rel[[1]]
    date <- dina_extract_wid_update_date(previous$rel[[1]])
    if (nzchar(date)) {
      suggestions[["export_validation.previous_update_date"]] <- date
    }
  }
  suggestions
}

dina_parameter_suggestion <- function(suggestions, key) {
  value <- suggestions[[key, exact = TRUE]]
  if (is.null(value) || length(value) == 0L) {
    return("")
  }
  dina_parameter_display_value(value)
}

dina_print_parameter_row <- function(spec, config, suggestions) {
  value <- dina_parameter_display_value(dina_parameter_value(config, spec$key))
  suggestion <- dina_parameter_suggestion(suggestions, spec$key)
  dina_cli_cat(sprintf("%-42s %s", spec$key, value))
  dina_cli_cat(sprintf("%-42s %s", "", dina_cli_dim(sprintf("# %s", spec$note))))
  if (nzchar(suggestion)) {
    dina_cli_cat(sprintf("%-42s %s", "", dina_cli_dim(sprintf("suggestion: %s", suggestion))))
  }
}

dina_print_parameter_summary <- function(session, root = dina_repo_root()) {
  config <- dina_session_config(session, root, expand_env = FALSE)
  invisible(dina_export_validation_config(config))
  suggestions <- dina_parameter_suggestions(config, root)
  override_path <- dina_session_config_override_path(session$id, root)
  dina_cli_cat("")
  dina_cli_cat("Session config:")
  dina_cli_alert("Benchmark: config/dina.yml")
  dina_cli_alert(sprintf(
    "Working override: %s (%s)",
    dina_relative(override_path, root),
    if (file.exists(override_path)) "present" else "not created yet"
  ))
  dina_cli_cat(sprintf("%-42s %s", "parameter", "value"))
  for (section in dina_parameter_sections()) {
    dina_cli_cat("")
    dina_cli_cat(section$title)
    for (spec in section$params) {
      dina_print_parameter_row(spec, config, suggestions)
    }
  }
  dina_cli_alert("Session edits use `dina update config set KEY VALUE`; benchmark config is promoted only at finalize.")
}

dina_parameter_move <- function(position, direction) {
  sections <- dina_parameter_sections()
  if (identical(direction, "next")) {
    if (position$param < length(sections[[position$section]]$params)) {
      position$param <- position$param + 1L
    } else if (position$section < length(sections)) {
      position$section <- position$section + 1L
      position$param <- 1L
    } else {
      position$done <- TRUE
    }
    return(position)
  }
  if (position$param > 1L) {
    position$param <- position$param - 1L
  } else if (position$section > 1L) {
    position$section <- position$section - 1L
    position$param <- length(sections[[position$section]]$params)
  }
  position
}

dina_parameter_actions <- function(has_suggestion = FALSE) {
  actions <- list()
  if (isTRUE(has_suggestion)) {
    actions[[length(actions) + 1L]] <- dina_menu_action("accept", "Accept suggestion", value = "accept", description = "Write the suggested value to the working override.")
  }
  actions[[length(actions) + 1L]] <- dina_menu_action("keep", "Keep", value = "keep", description = "Leave this value unchanged.")
  actions[[length(actions) + 1L]] <- dina_menu_action("edit", "Edit", value = "edit", description = "Edit the current value, then save or cancel.")
  actions[[length(actions) + 1L]] <- dina_menu_action("previous", "Previous", value = "previous", hidden = TRUE)
  actions[[length(actions) + 1L]] <- dina_menu_action("next", "Next", value = "next", hidden = TRUE)
  actions
}

dina_parameter_choose_action <- function(section_title, spec, current, suggestion, index, total, input = "stdin", is_terminal = isatty(stdin())) {
  prompt <- paste(c(
    sprintf("%s (%s/%s)", section_title, index, total),
    sprintf("%s: %s", spec$key, if (nzchar(current)) current else "(blank)"),
    sprintf("Reminder: %s", spec$note),
    if (nzchar(suggestion)) sprintf("Suggestion: %s", suggestion) else character()
  ), collapse = "\n  ")
  result <- dina_menu_select(
    title = "Parameter Action",
    items = dina_parameter_actions(nzchar(suggestion)),
    prompt = prompt,
    default = if (nzchar(suggestion)) "accept" else "keep",
    allow_quit = TRUE,
    input = input,
    is_terminal = is_terminal
  )
  if (is.null(result)) {
    return("keep")
  }
  result
}

dina_update_parameters_print_override_status <- function(session, root = dina_repo_root()) {
  override_path <- dina_session_config_override_path(session$id, root)
  if (file.exists(override_path)) {
    dina_cli_ok(sprintf("Updated working override: %s", dina_relative(override_path, root)))
  } else {
    dina_cli_ok("No working override created; effective config is the benchmark config.")
  }
}

dina_open_editor <- function(path, editor = Sys.getenv("EDITOR", unset = "vi")) {
  editor <- trimws(editor %||% "")
  if (!nzchar(editor)) {
    editor <- "vi"
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
    session <- dina_save_session_config_override(
      session,
      dina_session_config(session, root, expand_env = FALSE),
      root
    )
  }
  list(session = session, path = override_path, created = created)
}

dina_update_config_edit <- function(
    session,
    root = dina_repo_root(),
    editor = Sys.getenv("EDITOR", unset = "vi"),
    open_editor = isatty(stdin()) || nzchar(Sys.getenv("EDITOR", unset = ""))) {
  prepared <- dina_prepare_session_config_yaml(session, root)
  session <- prepared$session
  path <- prepared$path
  dina_cli_header("Update Config Edit")
  dina_cli_alert(sprintf("Update YAML: %s", dina_relative(path, root)))
  if (isTRUE(prepared$created)) {
    dina_cli_alert("Created from the active effective config so the file can be edited directly.")
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
  dina_update_parameters_print_override_status(session, root)
  dina_cli_alert("Review with `dina update config show`.")
  invisible(dina_load_session(root = root))
}

dina_update_parameters_wizard <- function(session, root = dina_repo_root(), input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    dina_print_update_gate(session, root, "parameters")
    dina_cli_alert("To edit parameters interactively, run `dina update config edit`.")
    return(invisible(session))
  }

  dina_cli_header("Gate: parameters")
  dina_cli_alert("Opening the active update YAML. The benchmark config is untouched until finalize promotion.")
  dina_print_parameter_summary(session, root)
  session <- dina_update_config_edit(session, root = root)
  dina_cli_ok("Next action: review the YAML, then mark parameters checks with `dina update mark parameters/CHECK --status done`.")
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
      "Start, inspect, mark, restart, and finalize update sessions.",
      children = list(
        dina_command_entry("workflow", "Annual update recipe", "Read the update workflow guide.", args = c("help", "workflow"), next_step = "Use the roadmap to work gate by gate."),
        dina_command_entry("update-start", "Start update", "Create the active annual update session.", args = c("update", "start", "{YEAR}"), defaults = list(YEAR = year), mutating = TRUE, help = "Creates output/updates/<update_id>, source baselines, inbox buckets, and the active pointer.", next_step = "Then run `dina update roadmap`."),
        dina_command_entry("update-roadmap", "Open roadmap", "Inspect ordered gates and the next unfinished check.", args = c("update", "roadmap"), next_step = "Open the next gate when ready."),
        dina_command_entry("update-gate", "Open next gate", "Open the next unfinished gate, opening the update YAML when parameters are next.", args = c("update", "gate"), next_step = "Complete the checks shown on the gate page."),
        dina_command_entry("update-parameters", "Review parameters", "Open the active update YAML for parameter review.", args = c("update", "gate", "parameters"), next_step = "Edit the YAML, then mark parameters checks explicitly."),
        dina_command_entry("update-status", "Update status", "Inspect the active update state and recommendation.", args = c("update", "status"), next_step = "Run the recommended command or browse this menu."),
        dina_command_entry("update-list", "List updates", "List update sessions and the active pointer.", args = c("update", "list")),
        dina_command_entry("repo-status", "Repo status", "Compare current code/config/docs with the update baseline.", args = c("update", "repo-status")),
        dina_command_entry("repo-diff", "Repo diff", "Show changed files against the update baseline.", args = c("update", "repo-diff", "--stat", "--files")),
        dina_command_entry("finalize", "Finalize update", "Freeze final records after checks pass.", args = c("update", "finalize"), mutating = TRUE, help = "Use --yes in a typed command when you also want to promote the effective config.")
      )
    ),
    dina_command_entry(
      "source-data",
      "Source data",
      "Inspect, fetch, review, and integrate source data.",
      children = list(
        dina_command_entry("buckets", "Buckets", "Show central _new bucket status.", args = c("buckets")),
        dina_command_entry("buckets-detail", "Bucket detail", "Show expected files and destinations.", args = c("buckets", "detail")),
        dina_command_entry("buckets-urls", "Bucket URLs", "Show bucket URLs for manual downloads.", args = c("buckets", "urls")),
        dina_command_entry("buckets-uses", "Bucket uses", "Show code/tasks that use expected files.", args = c("buckets", "uses")),
        dina_command_entry("buckets-fetch-dry-run", "Preview bucket fetch", "Preview supported public bucket fetches.", args = c("buckets", "fetch", "--dry-run")),
        dina_command_entry("buckets-fetch-source", "Preview bucket source fetch", "Preview one supported public bucket fetch.", args = c("buckets", "fetch", "--source", "{ID}", "--dry-run"), prompts = list(ID = list(label = "Source id", example = "chl-pit-total"))),
        dina_command_entry("sources-review", "Source review", "Show staged files and _new inbox candidates.", args = c("sources", "review")),
        dina_command_entry("integrate-incoming", "Integrate incoming source", "Preview or accept one _new inbox source.", args = c("sources", "integrate", "--incoming", "--source", "{ID}"), prompts = list(ID = list(label = "Source id", example = "chl-pit-total")), mutating = TRUE, help = "The typed command can add --yes after reviewing the preview."),
        dina_command_entry(
          "source-registry-diagnostics",
          "Source registry diagnostics",
          "Inspect registry rows, methods, scans, status, diffs, and URL/ZIP refresh previews.",
          children = list(
            dina_command_entry("sources-list", "Source registry", "List source registry rows.", args = c("sources", "list")),
            dina_command_entry("sources-show", "Show source", "Inspect one source registry entry.", args = c("sources", "show", "{ID}"), prompts = list(ID = list(label = "Source id", example = "chl-pit-total")), help = "Use `dina sources list` first if you do not know the id."),
            dina_command_entry("sources-methods", "Source methods", "Explain acquisition method labels.", args = c("sources", "methods")),
            dina_command_entry("sources-status", "Source status", "Compare sources with the active update baseline.", args = c("sources", "status")),
            dina_command_entry("sources-scan", "Source scan", "Detect local source coverage from the registry.", args = c("sources", "scan")),
            dina_command_entry("sources-diff", "Source diff", "Compare current source scan with the active session baseline.", args = c("sources", "diff")),
            dina_command_entry("refresh-dry-run", "Preview refresh", "Preview URL/ZIP fetches without writing.", args = c("sources", "refresh", "--dry-run", "--urls"))
          )
        ),
        dina_command_entry(
          "source-inbox-compat",
          "Advanced compatibility",
          "Older source-inbox commands; bucket commands are preferred for routine _new work.",
          children = list(
            dina_command_entry("inbox-guide", "Inbox guide", "Compatibility view for bucket detail.", args = c("sources", "inbox", "guide"), help = "`dina buckets detail` is the preferred guided command."),
            dina_command_entry("inbox-init", "Preview inbox init", "Preview missing inbox buckets without writing.", args = c("sources", "inbox", "init", "--dry-run"), help = "`dina buckets` and `dina buckets detail` are the preferred first checks.")
          )
        )
      )
    ),
    dina_command_entry(
      "pipeline",
      "Pipeline",
      "Inspect task freshness and run pipeline tasks.",
      children = list(
        dina_command_entry("tasks-list", "List tasks", "Inspect task aliases, stages, language, and status.", args = c("tasks", "list")),
        dina_command_entry("tasks-why", "Why task", "Explain why one task is current, stale, blocked, or failed.", args = c("tasks", "why", "{TASK}"), prompts = list(TASK = list(label = "Task selector", example = "07d")), help = "Selectors include short ids like 07d and full task ids."),
        dina_command_entry("run-dry", "Preview run", "Preview selected scripts without executing.", args = c("run", "{TASK}", "--dry-run"), prompts = list(TASK = list(label = "Task selector", example = "01a"))),
        dina_command_entry("run-execute", "Execute run", "Run selected scripts and write run logs.", args = c("run", "{TASK}", "--execute"), prompts = list(TASK = list(label = "Task selector", example = "01a")), mutating = TRUE, help = "This can update data/output files.")
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
        dina_command_entry("config-set", "Set config value", "Modify config/dina.yml.", args = c("config", "set", "{KEY}", "{VALUE}"), prompts = list(KEY = list(label = "Config key", example = "years.last"), VALUE = list(label = "Value", example = "2026")), mutating = TRUE),
        dina_command_entry("config-stata", "Export Stata config", "Write an explicit manual Stata runtime config.", args = c("config", "stata", "--output", "{PATH}"), prompts = list(PATH = list(label = "Output path", example = "/tmp/dina-config.do")), mutating = TRUE),
        dina_command_entry("data-check", "Data check", "Report whether configured primary paths exist.", args = c("data", "check")),
        dina_command_entry("notify-init", "Initialize notifications", "Create local Pushover placeholder config.", args = c("notify", "init"), mutating = TRUE),
        dina_command_entry("notify-test", "Test notification", "Send a Pushover test message.", args = c("notify", "test"), mutating = TRUE),
        dina_command_entry("setup-command", "Install command wrapper", "Install the user-level dina wrapper.", args = c("setup", "command"), mutating = TRUE)
      )
    ),
    dina_command_entry(
      "maintenance",
      "Maintenance",
      "Audit paths and export task graph helpers.",
      children = list(
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
      prompt = "Browse command groups, press Enter to open or run, and ? for more detail.",
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
  command <- commands[[1]]
  gsub(
    "dina update start YEAR",
    sprintf("dina update start %s", format(Sys.Date(), "%Y")),
    command,
    fixed = TRUE
  )
}

dina_dashboard_proposal <- function(recommendation) {
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

dina_command_catalog_entry_lines <- function(entries, indent = 4L) {
  lines <- character()
  prefix <- strrep(" ", indent)
  for (entry in entries) {
    children <- entry$children %||% list()
    has_command <- length(entry$args %||% character()) > 0L
    if (has_command) {
      lines <- c(lines, sprintf("%s- %s", prefix, entry$label))
      lines <- c(lines, sprintf("%s  $ %s", prefix, dina_command_template(entry)))
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
    lines <- c(lines, "", "Recommended action:")
    lines <- c(lines, sprintf("  $ %s", proposal$command))
    if (nzchar(proposal$comment %||% "")) {
      lines <- c(lines, sprintf("  %s", proposal$comment))
    }
    if (nzchar(proposal$next_step %||% "")) {
      lines <- c(lines, sprintf("  Next: %s", proposal$next_step))
    }
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
    sources_pending = "waiting for source review",
    gate_pending = "waiting for roadmap gate",
    `gate_in-progress` = "working through roadmap gate",
    `gate_needs-code` = "roadmap gate needs code",
    build_ready = "pipeline work pending",
    review_ready = "ready for final review",
    gsub("_", " ", raw, fixed = TRUE)
  )
}

dina_dashboard_common_commands <- function(session, proposal = NULL) {
  proposed <- trimws(proposal$command %||% "")
  commands <- if (is.null(session)) {
    c(
      proposed,
      "dina doctor",
      "dina update roadmap",
      "dina update list",
      "dina buckets",
      "dina tasks list"
    )
  } else {
    c(
      proposed,
      "dina doctor",
      "dina update status",
      "dina update roadmap",
      "dina sources review",
      "dina tasks list"
    )
  }
  commands <- commands[nzchar(commands)]
  commands[!duplicated(commands)][seq_len(min(5L, length(commands)))]
}

dina_dashboard_print_summary <- function(root = dina_repo_root(), session = dina_load_session(root = root), state = dina_session_state(session, root), proposal = NULL) {
  proposal <- proposal %||% dina_dashboard_proposal(state$recommendation)
  dina_cli_header("DINA-LatAm CLI")
  dina_cli_cat("")
  dina_cli_cat("Project status:")
  dina_cli_cat(sprintf("  Project: %s", dina_dashboard_project_name(root)))
  dina_cli_cat(sprintf("  Root: %s", normalizePath(root, mustWork = FALSE)))
  dina_cli_cat(sprintf("  Git: %s", dina_dashboard_git_status(root)))

  active <- dina_current_update(root)
  if (is.null(session)) {
    if (is.null(active)) {
      dina_cli_cat("")
      dina_cli_cat("Active update: none")
      dina_cli_cat(sprintf("Status: %s", dina_dashboard_status_label(state)))
    } else {
      dina_cli_cat("")
      dina_cli_cat(sprintf("Active update: %s", active))
      dina_cli_cat("Status: active pointer exists, but manifest.json is missing")
    }
  } else {
    year <- session$year %||% dina_update_year_from_id(session$id)
    dina_cli_cat("")
    dina_cli_cat(sprintf("Active update: %s (%s)", year, session$id))
    dina_cli_cat(sprintf("Status: %s", dina_dashboard_status_label(state)))
    if (nzchar(session$status %||% "")) {
      dina_cli_cat(sprintf("Session status: %s", session$status))
    }
  }

  dina_cli_cat("")
  dina_cli_cat("Next recommended action:")
  dina_cli_cat(sprintf("  %s", proposal$command))
  if (nzchar(proposal$comment %||% "")) {
    dina_cli_cat(sprintf("  %s", proposal$comment))
  }
  if (nzchar(proposal$next_step %||% "")) {
    dina_cli_cat(sprintf("  Next: %s", proposal$next_step))
  }

  dina_cli_cat("")
  dina_cli_cat("Common commands:")
  for (command in dina_dashboard_common_commands(session, proposal)) {
    dina_cli_cat(sprintf("  %s", command))
  }

  dina_cli_cat("")
  dina_cli_cat("Interactive options:")
  dina_cli_cat("  [Enter] Run the recommended action")
  dina_cli_cat("  [n] Navigate available commands")
  dina_cli_cat("  [m] Open main menu")
  dina_cli_cat("  [h] Help")
  dina_cli_cat("  [q] Quit")
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

dina_dashboard_main_menu <- function(root = dina_repo_root(), proposal = NULL, input = "stdin", is_terminal = isatty(stdin())) {
  session <- dina_load_session(root = root)
  state <- dina_session_state(session, root)
  proposal <- proposal %||% dina_dashboard_proposal(state$recommendation)
  commands <- dina_dashboard_common_commands(session, proposal)
  actions <- list(
    dina_menu_action(
      "recommended",
      "Run recommended action",
      value = list(type = "command", command = proposal$command),
      description = proposal$command,
      command = proposal$command
    )
  )
  for (i in seq_along(commands)) {
    command <- commands[[i]]
    if (identical(command, proposal$command)) {
      next
    }
    actions[[length(actions) + 1L]] <- dina_menu_action(
      sprintf("common-%s", i),
      command,
      value = list(type = "command", command = command),
      command = command
    )
  }
  actions[[length(actions) + 1L]] <- dina_menu_action(
    "commands",
    "Navigate available commands",
    value = list(type = "navigator"),
    description = "Open the full command navigator."
  )
  actions[[length(actions) + 1L]] <- dina_menu_action(
    "help",
    "Help",
    value = list(type = "command", command = "dina help"),
    command = "dina help"
  )
  selected <- dina_menu_select(
    "DINA Main Menu",
    actions,
    prompt = "Choose a workflow action.",
    default = NULL,
    allow_quit = TRUE,
    input = input,
    is_terminal = is_terminal
  )
  if (is.null(selected) || identical(selected, "quit")) {
    return(invisible(NULL))
  }
  if (identical(selected$type %||% "", "navigator")) {
    return(dina_command_browser(root, proposal = proposal, input = input, is_terminal = is_terminal))
  }
  dina_dashboard_run_command(selected$command %||% "", root = root)
}

dina_dashboard_prompt <- function(root = dina_repo_root(), proposal = NULL, input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(invisible(NULL))
  }
  answer <- tolower(trimws(dina_read_prompt("Choose [Enter/n/m/h/q]: ", input = input)))
  if (!nzchar(answer)) {
    return(dina_dashboard_run_proposal(proposal, root = root))
  }
  if (answer %in% c("n", "navigate", "commands")) {
    return(dina_command_browser(root, proposal = proposal, input = input, is_terminal = is_terminal))
  }
  if (answer %in% c("m", "menu")) {
    return(dina_dashboard_main_menu(root, proposal = proposal, input = input, is_terminal = is_terminal))
  }
  if (answer %in% c("h", "help", "?")) {
    return(dina_usage())
  }
  if (answer %in% c("q", "quit", "exit")) {
    dina_cli_alert("No command run.")
    return(invisible(NULL))
  }
  dina_cli_warn(sprintf("Unknown option: %s", answer))
  invisible(NULL)
}

dina_print_command_navigator <- function(root = dina_repo_root(), proposal = NULL, input = "stdin", is_terminal = isatty(stdin())) {
  if (is.null(proposal)) {
    session <- dina_load_session(root = root)
    state <- dina_session_state(session, root)
    proposal <- dina_dashboard_proposal(state$recommendation)
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
  state <- dina_session_state(session, root)
  recommendation <- gsub(
    "dina update start YEAR",
    sprintf("dina update start %s", format(Sys.Date(), "%Y")),
    state$recommendation,
    fixed = TRUE
  )
  proposal <- dina_dashboard_proposal(recommendation)
  dina_dashboard_print_summary(root, session = session, state = state, proposal = proposal)
  dina_dashboard_prompt(root, proposal = proposal, input = input, is_terminal = is_terminal)
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
    "Files to clear: %s staged, %s logs, %s snapshots.",
    result$staged_files,
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
    dina_print_source_inbox_bucket_summary(session$source_inbox$buckets, folders = session$source_inbox$folders, root = root)
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
    } else if (identical(action, "edit")) {
      dina_update_config_edit(session, root = root)
    } else {
      stop("Usage: dina update config show\n       dina update config set KEY VALUE\n       dina update config edit", call. = FALSE)
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
      result <- dina_update_restart(result$id, root = root, yes = TRUE, repo_policy = repo_policy, show_old_refs = old_refs_flag, progress = dina_cli_progress)
      dina_print_restart_completion(result, root = root)
    } else {
      dina_print_restart_completion(result, root = root)
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
        promote <- dina_menu_confirm("Finalize Update", "Promote effective update config to benchmark?", default = FALSE)
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
    dina_cli_alert("More detail: dina buckets detail")
    dina_cli_alert("URLs: dina buckets urls")
    dina_cli_alert("Uses: dina buckets uses")
    dina_cli_alert("Fetch supported public files: dina buckets fetch --dry-run")
  }
  dina_cli_alert("Drop manual downloads into the matching bucket shown above.")
  dina_cli_alert("Then run `dina sources review` and copy approved files with `dina sources integrate --incoming`.")
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
    families = vapply(bucket_paths, function(bucket) paste(sort(unique(rows$family[rows$bucket == bucket])), collapse = ", "), character(1)),
    stringsAsFactors = FALSE
  )
  dina_cli_cat(sprintf("%-32s %-8s %s", "bucket", "state", "families"))
  for (i in seq_len(nrow(bucket_rows))) {
    dina_cli_cat(sprintf(
      "%-32s %-8s %s",
      dina_cli_name(bucket_rows$bucket[[i]]),
      dina_cli_dim(if (isTRUE(bucket_rows$folder_exists[[i]])) "exists" else "missing"),
      dina_cli_dim(bucket_rows$families[[i]])
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
    dina_cli_alert("URLs: dina buckets urls")
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

dina_print_bucket_fetch_results <- function(rows, dry_run = FALSE, show_skipped = FALSE) {
  dina_cli_header(if (isTRUE(dry_run)) "Bucket Fetch Preview" else "Bucket Fetch")
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
      "Skipped %s source(s) without automatic fetches. Use `dina buckets detail` for manual bucket instructions.",
      skipped_count
    ))
  }
  if (isTRUE(dry_run)) {
    dina_cli_ok("Dry-run only: no files were written.")
  }
  if (any(fetch_rows$status %in% c("fetched", "already_present", "would_fetch"))) {
    dina_cli_alert("Next: dina sources review")
    dina_cli_alert("Then: dina sources integrate --incoming")
  }
  invisible(rows)
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
      dina_print_source_inbox_init(result, root = root)
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
  if (identical(sub, "main")) {
    session <- dina_load_session(root = root)
    state <- dina_session_state(session, root)
    proposal <- dina_dashboard_proposal(state$recommendation)
    if (!isatty(stdin())) {
      dina_dashboard_print_summary(root, session = session, state = state, proposal = proposal)
      return(invisible(NULL))
    }
    return(dina_dashboard_main_menu(root, proposal = proposal))
  }
  if (sub %in% c("commands", "navigate")) {
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
    config = dina_cmd_config(root, rest),
    data = dina_cmd_data(root, rest),
    commands = dina_cmd_commands(root, rest),
    navigate = dina_cmd_commands(root, rest),
    menu = dina_cmd_menu(root, rest),
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

if (!identical(Sys.getenv("DINA_CLI_SOURCE_ONLY", unset = ""), "1")) {
  tryCatch(
    dina_main(root = dina_repo_root(dina_cli_root)),
    error = function(e) {
      dina_cli_err(conditionMessage(e))
      quit(status = 1)
    }
  )
}
