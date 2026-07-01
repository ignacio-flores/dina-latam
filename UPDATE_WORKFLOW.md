# DINA Update Workflow

This project treats `config/dina.yml` as the current benchmark configuration.
An update session stores only a working override at
`output/updates/<id>/config.override.yml`, and only after you actually edit a
setting. The effective config is always:

```text
config/dina.yml + active update override
```

## Working Config

Use the parameters gate first:

```bash
dina update gate parameters
```

In an interactive terminal this guides the year, country list, export-validation
settings, and optional `KEY VALUE` edits. In scripts, edit the working override
explicitly:

```bash
dina update config set years.last 2024
dina update config set export_validation.previous_update_date 3Oct2024
dina update config show
```

No session `config.do` is kept. `dina run` creates a temporary Stata globals file
only while a Stata task is running, points `DINA_CONFIG_DO` to it, and removes it
afterward. R helpers read the same YAML benchmark plus override.

## Manual Stata Runs

Root `_config.do` is now only a compatibility loader. It runs `DINA_CONFIG_DO`
when that environment variable is present and otherwise fails with instructions.
For a manual Stata run, export a runtime config explicitly:

```bash
dina config stata --output /tmp/dina-config.do
export DINA_CONFIG_DO="/tmp/dina-config.do"
```

## Old-Reference Notes

Old-reference notes map older manual update notes to the current gate model.
They are a session preference:

```bash
dina update prefs old-refs on
dina update prefs old-refs off
```

## Repo Baselines

The start baseline captures small code/config/document files outside the big
data roots. It intentionally excludes `input_data`, `intermediary_data`,
`output`, and `previous_series`.

```bash
dina update repo-status
dina update repo-diff --stat --files
dina update repo-restore --dry-run
```

Restoration is conservative: it restores captured modified or deleted files, it
does not remove added files automatically, and it never touches excluded data
roots.

## Restart And Finalize

`dina update restart` resets the same update id from scratch. By default it
preserves the original `start` repo baseline and records an additional
`restart-<timestamp>` snapshot before resetting.

`dina update finalize` freezes final records after task checks pass. With
`--yes`, it promotes the effective config back to `config/dina.yml`; it does not
write or regenerate root `_config.do`.
