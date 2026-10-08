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

## Configuration scope and pipeline authorization

Pipeline execution is authorized by a successful, current configuration
validation—not by source-review status. The two runnable scopes are separate:

```bash
# The active update: config/dina.yml plus its config.override.yml
dina update config check
dina run

# The benchmark, even while an update is active
dina config check
dina run --benchmark
```

Each check writes a scope-specific receipt containing the effective-config and
comparison-baseline fingerprints, timestamp, and result. A configuration or
baseline change makes only that scope's receipt out of date; rerun its explicit
check before a real task starts. Source reviews are shown in the run record as
advisory context and never reset, promote, or block a provisional run.

Every Stata task receives a generated runtime configuration through
`DINA_CONFIG_DO`; direct use of the old `_config.do` intentionally stops with
migration guidance. For a manual Stata session, export a currently validated
file first:

```bash
dina config stata --output /tmp/dina-benchmark.do
dina update config stata --output /tmp/dina-update.do
export DINA_CONFIG_DO=/tmp/dina-update.do
```

The configuration check verifies the chosen Stata executable, loads this same
bootstrap, confirms the scope settings and comparison baseline, and checks the
declared external ado dependencies. It does not execute a project pipeline task.

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
After a terminal-menu action prints an Explore, Include, or detail report, DINA
waits for Enter before redrawing a menu so the result remains visible.

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

`dina run TASK` executes by default. It requires a current successful
`dina update config check`; source-review status is advisory and does not block
a provisional update run. Use `--benchmark` to run the validated benchmark
configuration while an update is active. Use `--dry-run` when you only want to see
the commands.

Task `02d5-interpolate-admin-data` runs the installed `gpinter` R package on
each selected country's `_clean` Admin tabulations. It creates the
`gpinter_output/total-pos-COUNTRY.xlsx` workbooks (`total-pre-BRA.xlsx` for
Brazil) consumed by `02e-format-for-bfm`. The run preflight reports a missing
`_clean` workbook before starting tasks; the interpolation task fits and checks
all selected countries before replacing any `gpinter_output` workbook. The
generated manifest in `output/data_reports/admin_gpinter_manifest.json` records
the package version, source hashes (including Brazil's publisher averages for
2000, 2002, and 2006), output hashes, and covered years. Ecuador is excluded
until a current `_clean` workbook is available; Chile 2012 is excluded because
its bracket average is below its own threshold. Uruguay's 127-row files come
directly from its large Admin microdata. They need no gpinter interpolation;
`02e` reads them without rewriting them.

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
benchmark default. `edit` opens that exact file in a graphical editor and returns
to DINA immediately.

Use arrows and Enter (or the numbered choices) to navigate; `q` returns to the
workspace. Full settings stay visible until you press Enter to return.
`edit` never opens a terminal editor or waits for an editor to close. It prefers
the VS Code `code` launcher, then VS Code or the default text editor on macOS, and
the platform's graphical file opener elsewhere. Set `DINA_CONFIG_EDITOR` to use a
different graphical editor command. Save the file in that editor, return to DINA,
then choose **Validate configuration and baseline**.

`check` validates YAML, supported settings, countries, year bounds, and the
baseline's required comparison fields. A completed check records a validation
receipt in the active update, tied to the benchmark config, update override, and
baseline file fingerprint. The workspace reports `Validated`, `Not validated`,
`Validation out of date`, or `Needs attention` from that receipt without rereading
the DTA. Invalid edits remain on disk for correction and are not reported as
validated. Opening the editor alone never records a validation.
`dina config show|check` inspects the benchmark independently of the update.

Use one compatible WID-format DTA file from `input_data/_new/previous_series/` as the comparison
baseline. Choose it explicitly through `export_validation.previous_update_file` in
the settings file. Files are never combined or automatically chosen. Missing or
out-of-date configuration validation prevents only the final WID export task; it
does not prevent source inspection or earlier pipeline work.

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
`input_data`, `intermediary_data`, or `output`. Restore is
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
