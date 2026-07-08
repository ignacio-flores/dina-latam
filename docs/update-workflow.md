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

Use `dina sources` as the home for the source registry, buckets, fetchers, and
baseline comparison.

```bash
dina sources list
dina sources list sna
dina sources list workflow
dina sources list urls wid
dina sources list detail SOURCE --urls
dina sources list guide SOURCE --urls
dina sources fetch SOURCE --dry-run
dina sources fetch wid --dry-run
dina sources compare
dina sources explore sna
dina sources table sna year_expectations
dina sources include sna --dry-run
dina sources include sna --confirm --include-run RUN
```

`dina sources list` is compact by default. In an interactive terminal it can
offer a dismissible follow-up menu for details, workflow view, paths, or URLs.
Use `--no-menu` in scripts.

Fetches write directly to `input_data/_new/<bucket>`.

The public `sna` source type has an active experimental source workflow. Use
`dina sources explore sna` to inspect `_new/country_sna` files,
available years, likely extensions, and structure evidence. Then use
`dina sources include sna --dry-run` to check the deterministic include contract
against the latest exploration run. Use `dina sources table sna
year_expectations` to preview explorer tables inline. Confirm is a separate
guarded promotion step that writes a backup snapshot first; restore uses that
snapshot if the source promotion needs to be undone. These commands do not
replace `01b` or run the pipeline.

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
the active workspace manifest.

```bash
dina todo
dina todo check review-config
dina todo uncheck review-config
dina todo reset
```

Todos never block runs, config checks, or closure.

## Config

Inspect `config/dina.yml` through read-only commands. Edit the active workspace
override with your default editor when an update is active:

```bash
dina config show
dina config check
dina update config show
dina update config edit
```

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
