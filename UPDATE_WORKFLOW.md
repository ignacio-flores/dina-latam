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

In an interactive terminal this walks through two sections:

- Global parameters: countries, years, run units, and run steps.
- WID export parameters: export unit, export steps, extrapolation last year,
  previous update date, and previous update file.

Each parameter shows a short reminder plus any inferred suggestion. Interactive
DINA menus support arrow navigation in a terminal: Up/Down moves, Left/Right
moves backward or forward when available, Enter selects, `?` shows help, and `q`
quits. Numbered choices remain available as the fallback: type a number and
press Enter. Quitting stops before check-mark prompts; edits already accepted in
the same run remain in the working override. The shared CLI interaction rules
live in `CLI_INTERACTION_GUIDE.md`.

Inspection pages such as `dina update roadmap`, `dina sources review`, and
`dina tasks list` stay readable and script-friendly. When an interactive choice
is needed, DINA shows a separate action menu instead of making every table
interactive.

The previous WID-format series can be placed directly in `previous_series/` or
dropped into `input_data/_new/validation/` for the gate to suggest the newest
`.dta` file and infer date labels like `3Oct2024` from the filename. Suggestions
are shown only; they are written only when explicitly accepted.

In scripts, edit the working override explicitly:

```bash
dina update config set years.last 2024
dina update config set export_validation.previous_update_date 3Oct2024
dina update config set export_validation.previous_update_file previous_series/dina_latam_3Oct2024.dta
dina update config show
```

No session `config.do` is kept. `dina run` creates a temporary Stata globals file
only while a Stata task is running, points `DINA_CONFIG_DO` to it, and removes it
afterward. R helpers read the same YAML benchmark plus override.

## Source Inbox Buckets

`dina update start` prepares central manual-download buckets under
`input_data/_new/<bucket>` and prints each bucket path with its status and file
count. Buckets are created only for sources with explicit `inbox` or
`legacy_inbox` patterns; URLs alone do not create buckets.

```bash
dina buckets
dina buckets detail
dina buckets urls
dina sources inbox guide
dina sources inbox init
dina sources review
dina sources integrate --incoming
```

Use `dina buckets detail` for expected files and destinations, and `dina buckets
urls` for source URLs. If any bucket is missing, `dina sources inbox init`
recreates it and can copy old colocated `_new` files into the central bucket
layout. Pipeline scripts do not read `_new` directly; approved files are copied
into canonical `input_data/` paths with `dina sources integrate --incoming`.

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

The start baseline is the comparison point for repo-status, repo-diff, and
repo-restore. It tracks small code/config/document files, not data roots:
`input_data`, `intermediary_data`, `output`, or `previous_series`.

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
keeps the original `start` baseline as the comparison point and saves the
current repo state as a pre-restart checkpoint. It clears session staging, logs,
snapshots, and source baseline records, then creates a fresh session baseline.
It keeps `input_data/_new` buckets and incoming files by default; if those
buckets contain files, interactive restart asks whether to keep them or cancel
before changing anything.

`dina update finalize` freezes final records after task checks pass. With
`--yes`, it promotes the effective config back to `config/dina.yml`; it does not
write or regenerate root `_config.do`.
