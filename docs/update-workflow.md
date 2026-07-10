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
dina sources compare
dina sources explore sna
dina sources table sna year_expectations
dina sources include sna --dry-run
dina sources include sna --confirm --include-run RUN
dina sources explore admin
dina sources table admin year_expectations
dina sources include admin --dry-run
dina sources include admin --confirm --include-run RUN
dina sources explore surveys
dina sources table surveys survey_pop_status
dina sources include surveys --dry-run
dina sources include surveys --confirm --include-run RUN
dina sources explore wid
dina sources explore wid --fetch
dina sources table wid overlap_summary
dina sources include wid --dry-run
dina sources include wid --confirm --include-run RUN
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

The public `admin` source type has a PIT-only v1 workflow for Chile
`chl-pit-total`, Brazil `bra-pit-total`, and Colombia `col-pit-total`. Use
`dina sources explore admin` to inspect incoming PIT files and shallow
country-specific structure evidence, then `dina sources include admin --dry-run`
to stage source/contract compatibility checks. Confirm and restore follow the
same guarded snapshot pattern as SNA. This v1 does not run admin cleaners during
include, and non-PIT admin families are reported as unsupported for this
workflow. Admin PIT cleaners require `intermediary_data/population/SurveyPop.dta`;
if admin explore/include reports it missing or stale, run
`dina sources explore surveys`, review with `dina sources table surveys`, then
`dina sources include surveys --dry-run` and confirm the clean surveys include
run before returning to admin.

The public `surveys` source type has a survey-source workflow for CEPAL
survey inputs. Use `dina sources explore surveys` to inspect canonical and
incoming survey files, filename variants such as `N1`, required `_fep`/`edad`
availability, new years, retroactive overlap candidates, country-year coverage,
and whether `SurveyPop.dta` is missing or stale. Use `dina sources include
surveys --dry-run` to stage approved raw survey files under normalized active
`COUNTRY_YEARN.dta` names, write light raw-source comparison tables, and build a
candidate `SurveyPop.dta`. Confirm only after review; the confirm step promotes
approved survey inputs and writes `intermediary_data/population/SurveyPop.dta`.

The public `wid` source type owns WID fetching for the active pipeline. Run
`dina sources explore wid` to review the local artifact inventory. If configured
WID artifacts are missing or stale, interactive explore offers to fetch them;
scripts can use `dina sources explore wid --fetch`. The fetch writes raw WID
extracts and derived `.dta` candidates into the flat incoming bucket
`input_data/_new/wid/`, then reruns exploration so review tables compare the
incoming candidates with any existing files. After reviewing a clean fetch, use
`dina sources include wid --dry-run` to stage the `_new/wid` candidates for
promotion, then confirm only after review. The canonical WID files live flat in
`input_data/wid/`, including
`population_total_adult_npopul.dta`, `macro_national_accounts_indicators.dta`,
`public_spending_gdp_shares.dta`, `prices_deflator_ppp_eur.dta`, and export
comparison/scaling artifacts. World Bank price inputs remain separate under
`input_data/prices_WB/`; any combined WID/WB price object is a pipeline
temporary, not a WID source artifact.

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
