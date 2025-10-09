# dina-latam

Data and code to build **Distributional National Accounts (DINA)** series for **Latin America and the Caribbean** for inclusion in the **[World Inequality Database (WID)](https://wid.world/)**.

The project organizes input data, processing code (mostly Stata with some R), and generated WID-formatted outputs. It follows the DINA framework to reconcile micro sources (surveys and administrative records) with national accounts and produce consistent distributional series across countries and years.
---

## Table of Contents

- [Overview](#overview)
- [Repository Structure](#repository-structure)
- [Quick Start](#quick-start)
- [Configuration](#configuration)
- [Outputs](#outputs)
- [Citation](#citation)
- [Contact](#contact)
- [License](#license)

---

## Overview

This repository standardizes and publishes DINA-style series for Latin America & the Caribbean in the WID format.  
It harmonizes microdata (household surveys and administrative files where available), benchmarks to national accounts, and produces series for income concepts and units aligned with WID conventions.

> Steps and settings are defined in `_config.do`.
> Run everything (Stata and R) with a click in `code/Stata/00-run-everything.do`.

---

## Repository Structure (main folders)

```
dina-latam/
├─ code/                    # Stata & R scripts to ingest, harmonize, benchmark, and export series
├─ input_data/              # External source data 
│  ├─ surveys_CEPAL/        # (ignored in Git) household surveys
│  ├─ sna_UNDATA/           # national accounts
│  └─ admin_data/<CTR>/     # country-specific admin microdata
├─ output/
│  └─ latest_wid_series/    # final WID-formatted TSVs/CSVs (created when running the code)
|  └─ figures/              # All figures shown in the paper, technical notes and more
|  └─ synthetic-microfiles/ # Distributive results, in light format, before harmonization
├─ _config.do               # Global settings: countries, years, steps, language, flags
└─ .gitignore               # Paths excluded from version control
```

---

## Quick Start

### 1. Prepare Input Data

Place your local copies of input data (not tracked by Git for size and confidentiality issues):

```
input_data/
├─ surveys_CEPAL/               # Household surveys
├─ previous_series/             # DTA file in WID format (to compare in stata's 07d)
├─ admin_data/URY/microdata     # Admin microdata (if you want to run stata's 02b)
└─ admin_data/MEX/microdata     # Admin microdata (if you want to run stata's 02b)
```

### 2. Configure Run Options

Edit `_config.do` to set:

| Parameter | Description | Example |
|------------|-------------|----------|
| `all_countries` | ISO3 country codes to process | `"ARG" "BRA" "CHL" ...` |
| `first_y`, `last_y` | First and last year | `2000`, `2023` |
| `lang` | Output language | `eng` |
| `all_units` | Units (e.g., individuals, equal-split adults) | `"ind" "esn" "pch"` |
| `all_steps` | Processing steps | `"natinc" "pon"` |
| Flags | Debug or overwrite options | `debug`, `bfm_replace` |

### 3. Run the Pipeline

```stata
do code/Stata/00.run-everything.do
```
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
