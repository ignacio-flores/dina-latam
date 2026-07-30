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

`dina update status` reports current state: incoming sources, stale or failed
runs, unchecked todos, config override presence, and repo changes.

## Sources

Use `dina sources` to find inputs, stage incoming files in `_new`, review
supported source workflows, and promote accepted source files. Source commands
do not run the pipeline.

```bash
dina sources list
dina sources list sna
dina sources list detail SOURCE --urls
dina sources list guide SOURCE --urls
dina sources fetch SOURCE --dry-run
dina sources explore sna
dina sources table sna year_expectations
dina sources include sna --dry-run
dina sources include sna --confirm --include-run RUN
dina sources explore wid --fetch
dina sources include wid --dry-run
```

Work by command family:

- `list`: find source ids, URLs, expected `_new` buckets, canonical
  destinations, transformers, and likely task users.
- `fetch/place`: preview supported fetches with `--dry-run`; put manual inputs
  in the matching `input_data/_new/<bucket>` folder.
- `explore/review`: run `dina sources explore SOURCETYPE`; inspect review tables
  with `dina sources table SOURCETYPE [TABLE]`.
- `include`: run `dina sources include SOURCETYPE --dry-run`; confirm only a
  reviewed clean run with `--confirm --include-run RUN`. Use `--restore` for the
  confirm backup snapshot.

Support matrix:

| type | list | fetch/place | explore/review | include |
|---|---|---|---|---|
| `sna` | yes | yes | yes | yes |
| `admin` | yes | yes | PIT v1 | PIT v1 |
| `surveys` | yes | manual | yes | yes |
| `wid` | yes | `explore --fetch` | yes | yes |
| `admin-microdata` | yes | manual | no | no |
| `other` | yes | yes/manual | no | no |

Keep these source-type exceptions in mind:

- WID fetching belongs to `dina sources explore wid --fetch`; it writes incoming
  candidates to `input_data/_new/wid/`, then `include wid --dry-run` stages them
  for promotion.
- Survey include can generate `intermediary_data/population/SurveyPop.dta`; admin
  PIT workflows depend on that file and may ask you to run the surveys workflow
  first.
- Survey discovery validates every incoming `.dta`, including files outside the
  configured `SurveyPop` countries or years. It infers unambiguous country/year
  identities from filenames and parent folders, retries configured legacy Stata
  encodings, and normalizes accepted destinations to `CCC_YYYYN.dta`.
- `SurveyPop.dta` is not restricted by the pipeline country/year configuration.
  It includes every country with a recognized survey and uses a country-specific
  annual grid from that country's earliest to latest available source.
  Unobserved years inside the grid are interpolated; downstream pipeline steps
  apply their own country/year filters.
- Survey coverage distinguishes incoming, usable, and selected observed years,
  the resulting annual SurveyPop grid, and blocked country-years. A missing or
  stale `SurveyPop.dta` is `action_required`; only unusable or ambiguous sources
  make exploration `blocked`.
- Explore inventories overlaps but does not compare their contents. Run
  `dina sources include surveys --dry-run` to see whether incoming replacements
  differ from their canonical country-year files before confirmation.
- Treat the surveys workflow as two layers. The primary layer is ingestion and
  promotion of the survey source files; the secondary layer is rebuilding the
  derived `SurveyPop.dta` grid from the selected sources. `explore surveys` is a
  quick identity/readability/required-variable preflight for both layers.
  `include surveys --dry-run` is the full review package: it stages source
  destinations, compares overlaps, builds the candidate grid, and records the
  exact promotion plan. Neither command changes production files.
- Before confirming a survey include run, review these tables in order:
  `survey_source_comparison` for new and changed source contents,
  `staged_source_mappings` for incoming-to-canonical paths, `promotion_plan` for
  the exact files that confirmation will write, and `survey_pop_comparison` for
  the smaller derived-artifact change. Use `include_detail` and
  `survey_source_candidates` when a source is blocked or its identity is
  unclear. The include command prints the corresponding `dina sources table`
  commands with the run path as a reminder.
- Running `dina sources include surveys` currently performs the same safe staged
  dry-run as the documented `--dry-run` form; it also regenerates exploration
  data in memory when the saved exploration is missing or has an old schema.
  Production writes require the separate `--confirm --include-run RUN` command.
- Admin source workflow support is PIT-only v1. Non-PIT admin families remain
  registry/fetch inputs until a workflow is added.
- `compare`, `fields`, `methods`, `scan`, `diff`, and `inbox` are advanced
  checks for registry or baseline investigation, not the main updater path.

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

Inspect `config/dina.yml` through read-only commands. Review the active
workspace override with the proposed keys, override YAML, and full effective
merged config:

```bash
dina config show
dina config check
dina update config show
dina update config edit
```

Edit protocol: manually edit `output/updates/<id>/config.override.yml`, then
verify with `dina update config show`. Do not edit `config/dina.yml` for
active-update override changes. `dina update config edit --open` can launch the
default editor, but the normal command prints the path and protocol instead.

`dina run` creates a temporary Stata runtime config only while a Stata task is
running, points `DINA_CONFIG_DO` to it, and removes it afterward. R helpers read
the same YAML benchmark plus override.

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
