# DINA Update Workflow

This project treats `config/dina.yml` as the benchmark configuration. An update
workspace can carry a working override at
`output/updates/<id>/config.override.yml`, but the CLI is intentionally
read/propose first:

```text
config/dina.yml + active update override
```

## Start Or Resume

```bash
dina update start 2026
dina update status
```

`dina update start` creates the active workspace, source baseline, repo
baseline, todo state, and central incoming buckets under `input_data/_new`.
It does not create a source staging area.

`dina update status` reports current state: incoming sources, stale or failed
runs, unchecked todos, config override presence, and repo changes.

## Sources

Use `dina sources` as the home for the source registry, buckets, fetchers,
review, and integration.

```bash
dina sources list
dina sources list --view workflow
dina sources show SOURCE --view all --urls
dina sources guide SOURCE --urls
dina sources fetch SOURCE --dry-run
dina sources review
dina sources integrate SOURCE
dina sources integrate SOURCE --yes
```

`dina sources list` is compact by default. In an interactive terminal it can
offer a dismissible follow-up menu for details, workflow view, paths, or URLs.
Use `--no-menu` in scripts.

Fetches write directly to `input_data/_new/<bucket>`. Human review still happens
before accepted files are copied into canonical `input_data/` destinations.

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
dina todo check review-sources
dina todo uncheck review-sources
dina todo reset
```

Todos never block source integration, runs, config checks, or closure.

## Config

Edit `config/dina.yml` or the active workspace override manually. CLI config
commands are mainly reminders and proposals:

```bash
dina config show
dina config check
dina config propose years.last 2024
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

`dina update close` generates closure notes: changed sources, incoming and
integrated files, run summary, output freshness, config diff, and repo diff. It
marks the workspace closed only after the report is generated.
