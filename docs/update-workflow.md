# DINA Update Workflow

This project treats `config/dina.yml` as the benchmark configuration. An update
workspace can carry a working override at
`output/updates/<id>/config.override.yml`, and start/restart pre-populates that
file with suggested annual overrides:

```text
config/dina.yml + active update override
```

## Start Or Resume

```bash
dina update start 2026
dina update status
```

`dina update start` creates the active workspace, source baseline, repo
baseline, todo state, central incoming buckets under `input_data/_new`, and a
suggested `config.override.yml`. It does not create a source staging area.

Home shows recorded pipeline outcomes without scanning every task's input tree.
Open Pipeline or use `dina run list` to inspect file freshness; this can take longer
in large workspaces and does not run tasks.

Bare `dina` offers Configuration, Sources, Pipeline, and Results directly, with
brief statuses and one secondary suggestion. `dina update status` uses the same
status evidence. `dina commands` retains lifecycle actions and utilities.

## Sources

Work through one source family across countries, then move to the next:

```bash
dina sources
dina sources list surveys
dina sources list detail surveys-cepal --urls
dina sources explore surveys
dina sources table surveys
dina sources include surveys
```

The supported families are `sna`, `admin`, `surveys`, and `wid`. Other inputs
and heavy admin microdata retain their acquisition instructions in the registry
but do not have a family review/inclusion workflow.

Menus and typed commands use the same operations. The menu keeps the selected
family while you find sources, explore, inspect details, and include. Return to
family selection when ready to move on. The compact source list shows complete
IDs, families, countries and acquisition methods; source details contain URLs,
paths, transformers and task links.

### Explore

Acquire files using the registry instructions or `dina sources fetch SOURCE`.
Manual files go in the corresponding `input_data/_new/<family>` bucket.
WID fetching uses `dina sources explore wid --fetch`. In a terminal, Explore
also offers to fetch missing or stale WID candidates through the shared
confirmation control; `--no-fetch` suppresses that offer.

Explore combines the existing inventory and inclusion-assessment engines into
one saved review. It prepares candidates without changing accepted source files
or running the pipeline. The report shows:

A compact country summary precedes:

1. Accepted and candidate extracted coverage, added years, and lost coverage.
2. Numerical revisions, newly available values, and lost values, separately.
3. Blockers followed by warnings, grouped by country and cause, including ambiguous
   source destinations and extraction failures.

"None" means no years in that category; "No extracted data" means no extracted
coverage. A covered year need not have every expected variable. Empty cells on
both sides are classified using extraction expectations and coverage, rather
than counted as skipped checks. Expected absences are not problems. Missing
expected values retain their extraction reason; undetermined applicability is
stated explicitly. Percentages from a zero baseline display N/A with that reason.

Country SNA comparisons use the same extraction rules for accepted and proposed
values; other macro inputs retain their registry workflow.
WID comparisons match observations by declared keys, so revisions that cancel in
aggregate totals remain visible. Survey comparisons cover file changes, schema,
weights and age summaries; they do not compare every income value. Admin PIT
checks cover structure, dependencies, cleaner outputs and auxiliary-series
values; full PIT values are not checked. Admin year coverage comes from file
and structure evidence, not a comparison of all extracted PIT observations. Missing comparison coverage is shown as
not checked, never as unchanged or passed.

Existing numerical tolerances and inclusion conditions remain in force.
Ordinary revisions are evidence for your manual review; existing failed
validation rules still require resolution. No blanket override is introduced.

`dina sources table FAMILY` revisits the saved review. Its descriptive menu
provides readable choices. Typed aliases are `coverage`, `revisions`,
`added-values`, `missing-values`, `problems`, and `files`, for example:
`dina sources table sna revisions`. Internal tables such as `review_values`
remain available.
Existing named tables remain available for deeper inspection. `--run PATH`
selects an older review or a legacy table run. `--country ISO` filters the
report only; the saved review and inclusion cover the whole family.

### Include

`dina sources include FAMILY` shows the family decision and asks once before
accepting it. Scripts use `dina sources include FAMILY --confirm`.

The CLI remembers the exact review, so a run path is unnecessary. Review
metadata records scope, proposed destinations, input/configuration fingerprints,
and candidate/baseline fingerprints. Inputs and reviewed artifacts are checked
again immediately before writing. Changed evidence requires exploration again.
A failed exploration invalidates the previous current review. Later exploration
does not overwrite an earlier review's saved tables or candidates.

Inclusion retains the existing backup mechanism, prints the restore command,
and reports incomplete inclusion rather than declaring success. It does not
run the pipeline. Continue with the next family or inspect pipeline tasks when
the required sources are ready.

Survey reviews also prepare the derived `SurveyPop.dta` for inclusion alongside
survey sources. Admin PIT depends on that artifact and WID population inputs;
missing dependencies appear in the review. Survey availability follows the
recognized input sources, while downstream tasks apply the run's country/year
filters. Source coverage is independent of the annual update year.

### Compatibility and diagnostics

Explicit legacy forms remain available:

```bash
dina sources explore FAMILY --dry-run
dina sources include FAMILY --dry-run
dina sources include FAMILY --confirm --include-run RUN
dina sources include FAMILY --restore CONFIRM_RUN
```

The first remains an inventory-only check; the second remains a standalone
inclusion assessment. Neither accepts production sources. Plain `include FAMILY`
now accepts the saved Explore review after confirmation; callers needing the old
assessment must retain the explicit `--dry-run` form. Historical runs are not
renamed or migrated. `--exploration-run PATH` still selects inventory for an
explicit assessment. Selecting a new saved review with `--include-run` uses the
same acceptance guards as plain Include; a superseded review requires Explore
again.

`compare`, `fields`, `methods`, `scan`, `diff`, `inbox`, and extra registry views
remain available for diagnostics outside the routine family workflow.

## Compress Input Data

Use `dina compress input` when you need a portable zip of `input_data/`.
The default excludes the heavy `admin-microdata` source type, currently
`input_data/admin_data/MEX` and `input_data/admin_data/URY`.

```bash
dina compress input --dry-run
dina compress input --dropbox
```

`--dropbox` reads and writes under `~/Dropbox/DINA-LatAm`.

## Run

Pipeline tasks come from `config/pipeline.yml`; the CLI does not hardcode task
logic.

```bash
dina run list
dina run why 01a
dina run 01a --dry-run
dina run 01a
dina run stale --dry-run
```

`dina run list` separates recorded outcomes from file freshness and shows the
last recorded run and log location. `dina run why TASK` explains a task.

`dina run TASK` executes by default. Use `--dry-run` when you only want to see
the commands.

## Todo

The todo list is a loose helper from `config/todo.yml`. Checked state lives in
the active workspace manifest. This repo does not need default todo items right
now, so `config/todo.yml` can be empty.

```bash
dina todo
dina todo check ID
dina todo uncheck ID
dina todo reset
```

Todos never block runs, config checks, or closure.

## Config

The active update's commented `config.override.yml` is prefilled once with the
current countries and suggested annual year increments. Run years and export
comparison years serve different steps and remain separate. Resume preserves
your edits and comments.

```bash
dina update config show
dina update config edit
dina update config check
dina update config show --full
```

Configuration places benchmark and update values side by side before the action
menu. Changed values are emphasized and marked `*`, including without color.
Baseline filenames appear together, with an actionable warning when needed.
Full paths, setting keys, and effective YAML are under **Details and file paths**
(or `dina update config show --full`).

`config/dina.yml` supplies benchmark defaults; the active update's
`output/updates/UPDATE/config.override.yml` overrides just the settings it contains.
Editing changes the update file, so removing an override restores that setting's
benchmark default. The editor screen identifies the exact file being edited.

Use arrows and Enter (or the numbered choices) to navigate; `q` returns to the
workspace. Full settings stay visible until you press Enter to return.
`edit` shows instructions for the selected editor before opening it. DINA uses
`VISUAL`, then `EDITOR`; on macOS the fallback is the system's default text editor, elsewhere it is `vi`.
In `vi`, press `i` to edit, then Esc and `:wq` followed by Enter to save and return;
Esc and `:q!` followed by Enter discards changes. In nano, use Ctrl+O, Enter to save
and Ctrl+X to exit. In TextEdit, save with Cmd+S and quit with Cmd+Q to return.
The macOS `open` launcher, VS Code, Sublime Text, and TextMate wait before validation.
Other custom editor commands should include their own wait option if needed.

`check` validates YAML, supported settings, countries, year bounds, and the
baseline's required comparison fields. The same checks run after a successful
editor return. Invalid edits remain on disk for correction and are not reported
as validated. Cancelling without saving does not record a configuration edit.
`dina config show|check` inspects the benchmark independently of the update.

Use one compatible WID-format DTA file from `previous_series/` as the comparison
baseline. A valid explicit selection is preserved. Otherwise, the sole compatible
file is selected; multiple alternatives require a choice in interactive review or
an explicit `export_validation.previous_update_file` in the settings file. Files
are never combined or automatically chosen by modification time. Missing baseline
configuration needs attention before export but does not prevent source inspection.

`dina run` continues supplying temporary runtime configuration to Stata tasks.

## Final WID results

```bash
dina results
dina results show
dina results open t10
```

Results lists the existing Top 10%, Top 1%, Top 0.1%, Top 0.01%, Middle 40%, and
Bottom 50% comparison graphs. With several versions, select a full printed path.
The CLI opens the chosen PDF in the system viewer; listing never opens files.

New exports record the generation time, actual baseline and its fingerprint,
comparison settings, final-series files, and graph fingerprints alongside the
plots. Old graphs remain accessible with an unknown generation baseline. Changed
settings/baselines or altered/missing artifacts are distinguished from a matching
recorded comparison. An incomplete export does not publish successful metadata.
The export uses available observations within the configured comparison period;
a shorter baseline has a shorter line, without a fixed 2022 cutoff.

Changing the baseline does not regenerate existing graphs. Use the existing
export task (`dina run 07d`) when ready. Other output comparisons and standalone
graph regeneration are outside this workflow.

## Maintenance

Repo baselines live conceptually under maintenance:

```bash
dina maintain repo-status
dina maintain repo-diff --stat --files
dina maintain repo-restore --dry-run
```

The baseline tracks small code/config/document files, not data roots such as
`input_data`, `intermediary_data`, `output`, or `previous_series`. Restore is
conservative: it restores captured modified or deleted files, does not remove
added files automatically, and never touches excluded data roots.

## Restart And Close

```bash
dina update restart --yes
dina update close --dry-run
dina update close
```

`dina update restart` resets the same workspace id from scratch, keeps
`input_data/_new` buckets and incoming files, and refreshes source/run state.

`dina update close` generates closure notes: changed sources, incoming source
files, run summary, output freshness, config diff, and repo diff. It
marks the workspace closed only after the report is generated.
