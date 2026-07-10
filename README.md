# dina-latam

Data and code to build Distributional National Accounts (DINA) series for Latin America and the Caribbean for the inclusion in the [World Inequality Database](https://wid.world). The project organizes input data, processing code (mostly Stata and some R), and generates WID-formated output. It follows the DINA framework to reconcile micro sources (surveys and administrative records) with national accounts to produce macro-consistent distributional series across countries and years

---


## Repository Structure (main files and folders)

```
dina-latam/
├─ code/                    # Stata & R scripts to ingest, harmonize, benchmark, and export series
├─ docs/                    # CLI and update workflow notes
├─ input_data/              # External source data 
│  ├─ surveys_CEPAL/        # (ignored in Git) household surveys
│  ├─ sna_UNDATA/           # national accounts
│  └─ admin_data/           # country-specific admin microdata
├─ output/
│  └─ latest_wid_series/    # final WID-formatted TSVs/CSVs (created when running the code)
|  └─ figures/              # All figures shown in the paper, technical notes and more
|  └─ synthetic-microfiles/ # Distributive results, in light format, before harmonization
├─ config/                  # YAML config, source registry, pipeline graph, todo helper
└─ .gitignore               # Paths excluded from version control
```

---

## Quick Start

### 1. Prepare Input Data

Place your local copies of input data that are not tracked by Git for size and confidentiality issues:

```
input_data/
├─ surveys_CEPAL/               # Household surveys
├─ previous_series/             # DTA file in WID format (to compare in stata's 07d)
├─ admin_data/URY/microdata     # Admin microdata (if you want to run stata's 02b)
└─ admin_data/MEX/microdata     # Admin microdata (if you want to run stata's 02b)
```

### 2. Configure Run Options

Edit `config/dina.yml` manually to set:

| Parameter | Description | Example |
|------------|-------------|----------|
| `countries` | ISO3 country codes to process | `ARG, BRA, CHL, ...` |
| `years.first`, `years.last` | First and last year | `2000`, `2023` |
| `run.lang` | Output language | `eng` |
| `run.units` | Units (e.g., individuals, equal-split adults) | `ind, esn, pch` |
| `run.steps` | Processing steps | `natinc, pon` |
| Flags | Debug or overwrite options | `run.debug`, `run.bfm_replace` |

### 3. Run the Pipeline

```stata
do code/Stata/00.run-everything.do
```

### CLI Workflow

This repository also includes an experimental R-based project CLI:

```sh
./bin/dina
./bin/dina doctor
./bin/dina update start 2026
./bin/dina sources list
./bin/dina sources compare
./bin/dina sources explore sna
./bin/dina sources table sna year_expectations
./bin/dina sources include sna --dry-run
./bin/dina sources include sna --confirm --include-run RUN
./bin/dina sources explore wid --fetch
./bin/dina sources include wid --dry-run
./bin/dina run list
./bin/dina run 01a
./bin/dina update close --dry-run
./bin/dina help sources
```

The CLI keeps Stata's manual workflow available, but adds update workspaces,
source registry and `_new` bucket tools, task freshness checks, temporary Stata
runtime config files, run logs, closure reports, and archive helpers. Update
workspaces are stored under `output/updates/`.
Task selectors can use full task IDs, step aliases like `01a`, or whole-block
aliases like `01`.

Detailed CLI notes live in [`docs/update-workflow.md`](docs/update-workflow.md)
and [`docs/cli-interaction-guide.md`](docs/cli-interaction-guide.md).

For Pushover notifications, copy `config/pushover.local.R.example` to
`config/pushover.local.R` or run `./bin/dina notify init`, then replace the
placeholders. The local credentials file is ignored by Git; server runs can
instead use `PUSHOVER_USER_KEY` and `PUSHOVER_APP_TOKEN`.

When `dina run` executes a Stata task, the CLI writes a temporary runtime Stata
config, sets `DINA_CONFIG_DO` for that process, and removes the file after the
task finishes.
---

## Outputs

- **Final series:** `output/latest_wid_series/`
- **Intermediate data:** `intermediary_data/` (ignored)
- **Archived versions:** `previous_series/` (ignored)

Files include variables like income shares, thresholds, and average incomes for each percentile.  

---

## Citation

If you use these series, please cite:

> De Rosa, M., Flores, I., & Morgan, M. (2024). *More unequal or not as rich? Revisiting the Latin American exception.* *World Development*, 184, 106737.
---

## Contact

**Maintainer:** Ignacio Flores  
**GitHub:** [@ignacio-flores](https://github.com/ignacio-flores)  

---

## License

This project is licensed under the **MIT License**.  
See the [LICENSE](LICENSE) file for details.
