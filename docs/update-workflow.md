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

The supported families are `sna`, `admin`, `surveys`, `wid`, and `dina`
(Finished DINA series).
Other inputs and heavy admin microdata retain their acquisition instructions in
the registry but do not have a family review/inclusion workflow.

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

WID Explore compares the incoming snapshot with accepted WID files and never
automatically restores an earlier inclusion. Explicit restore remains available.
The export-scaling request ends at `export_validation.last_year`, while other
WID artifacts retain the calculation window. Accepted publication sources add
country-specific historical requests where needed: BRA adds 1976–1989 to the
ordinary export-country request beginning in 1990. Coverage checks require
positive income means for each consumed country/year and positive exchange
rates for the export reference year. Historical fields not consumed by export
do not become spurious blockers. The WID backend also supports
`run_wid_include(..., source_ids = "wid-export-scaling")` for a recoverable
scaling-only inclusion, preserving other accepted macro inputs.

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

### Finished DINA series

```bash
dina sources explore dina
dina sources table dina
```

Place researcher-supplied distributions in `input_data/_new/dina/`. The registry
records the exact file, version, country/year coverage, concepts, units and
publication citation. The current Ecuador source is
`WIL_ECU_1990-2022_v2.dta`, attributed to Nabernegg, M. (2026), *Extreme Income
Inequality in Ecuador-Dollarization, Commodity Price Boom, and Citizen
Revolution*, for the supplied 1990–2022 observations. Later years are explicitly
attributed to DINA continuation or carry-forward of that source. No DOI is
registered for this reference.

Explore reports coverage and precise validation results in the existing concise
CLI/table format. It checks required fields, keys, finite values, percentile
coverage, ordering, distribution identities, Gini bounds and export-grid
compatibility for all supplied concepts and units. It distinguishes structural
failures, scientific discrepancies, format differences and unconfirmed
definitions, using tolerances for storage precision. For Ecuador, `nat/esn`
is the publication series. Currency, price basis and population definitions
remain unconfirmed until documented.

The consistency overview and `checks` table present a separate section for each
country, keeping its numerical findings and source-definition notes together.

Brazil's `MorganSouza2026-BR-wid.xlsx` follows a coarse-data path in the same
Explore/Include workflow. Its two sheets supply shares (`sptinc992j`) and group
averages (`aptinc992j`, labelled 2016 prices), covering 1926–2015 with different
percentile coverage and missing years. Explore describes these observations,
preserves blank cells separately from zero, and reports share totals, nested
top-share ordering and shares/averages consistency. It does not require Gini,
thresholds or a complete export grid from this source.

```bash
dina sources explore dina --country BRA
dina sources table dina coverage --country BRA
dina sources table dina values --country BRA --year 2015 --wid-code sptinc992j
dina sources include dina --dry-run
dina sources table dina changes --country BRA --year 2015
dina sources table dina prepared-coverage --country BRA
dina sources table dina harmonization --country BRA
```

BRA Include preserves the byte-identical workbook and writes
`input_data/dina/MorganSouza2026-BR-wid_prepared.dta` after acceptance. Like ECU,
this prepared file contains standard 127-bracket distributions. BRA uses
GPinter `shares_fit()` because thresholds are not supplied; ECU uses
`tabulation_fit()` with its reviewed thresholds and bracket averages.

BRA retains only observed full-population years from 1976 onward: 38 years in
1976–2015, excluding missing 1991 and 1994. Earlier top-1% or top-10%
observations remain in the original workbook and Explore tables, but never
enter the prepared distributions. No lower-population range is inferred from
these historical tails. Missing-year interpolation and temporal extrapolation
are disabled, including any extension beyond 2015.

Shares govern BRA preparation. Include derives non-overlapping brackets by
subtracting nested top shares (for example p90–p99 = p90–p100 minus p99–p100),
then proportionally normalizes all disjoint shares to sum to one. This preserves
their relative proportions. The changes table records both the subtraction
and every normalization adjustment; the impact table reports the original
share total and GPinter preservation error at the source boundaries. Failed
fits, invalid shares, reversed bracket means or failed consistency checks block
acceptance. No pooling or replacement of missing years is authorized for BRA.

The output uses an overall mean of one; supplied monetary averages and the
deflator remain in the preserved workbook and Explore evidence. GPinter
estimates the thresholds, finer bracket means and Gini. Gini is explicitly a
fitted statistic, since BRA supplies no Gini. Prepared coverage lists every
source year with its inclusion/exclusion status, source and normalized share
totals, population coverage and reason. The prepared file itself contains only
the retained years. Unconfirmed source definitions remain visible in the reports.

For both ECU and BRA, harmonization runs in Include, including `--dry-run`,
independently of the numbered pipeline. 07d automatically prepares and validates
the integration after 07c; a separate preview is optional. BRA uses its registered
policy: preserve all 38 accepted observed
years, leave 1991 and 1994 unfilled, continue from the 2015 anchor using internal
four-group changes, and carry the last calculation-year shape to the export
year. For the current update that means continuation through 2024 and carry-
forward into 2025. This publication policy does not change Include's prohibition
on filling missing source years. Country filters affect display only; source
Explore and Include cover the entire family.

The overview groups discrepancies in a compact table with affected cases,
small/larger finding counts, median and maximum gaps. For review navigation,
“small” means at most 0.1% of the reference value; share discrepancies must also
be at most 0.01 percentage points. These are display thresholds, not scientific
approval or a rule for automatic correction. Definition questions, export-grid
alignment and structural failures are listed separately.

```bash
dina sources table dina --step nat --unit esn
dina sources table dina small
dina sources table dina larger --check share_identity --year 1994
dina sources table dina decisions
```

`checks` lists the check IDs. Detail tables show the largest discrepancies first;
`--exact` gives full precision and the check explanation. Display filters do not
change validation scope or the source's overall status.

Exploration reads the supplied file independently of unfinished pipeline results.
Missing internal Ecuador estimates do not fail this source review. No source
values are repaired, no continuation or comparison preview is constructed, and
no accepted inputs or published results are replaced.

Prepare and inspect the inclusion candidate separately:

```bash
dina sources include dina --dry-run
dina sources table dina changes
dina sources table dina impact
dina sources include dina
```

The dry run saves a validated candidate and its reports without accepting it.
Include displays that candidate's summary and asks once before acceptance;
`--confirm` supplies the same confirmation in scripted use. `--include-run RUN`
selects the saved candidate explicitly. `--restore CONFIRM_RUN` restores the
previous accepted files from the inclusion backup. ECU retains all supplied
years, concepts and units; BRA follows its explicit full-population-year policy.
Display filters never narrow inclusion.

The ECU preparation engine applies the agreed rules to a separate copy:

| Issue | Preparation rule |
| --- | --- |
| Unequal bracket weights | Recover population mass as `share × overall mean / bracket mean`; check it against the supplied cumulative top means. |
| Storage precision | Normalize masses and recompute the overall mean from unchanged bracket means. Reject normalization beyond `1e-6`; report the corresponding share normalization. |
| Threshold errors | Move a threshold to the nearest feasible value between adjacent bracket means. Record substantive repairs separately from numerical boundary nudges. |
| Tiny reversed means | Pool adjacent bins by population weight, preserving income and population; reject pooling if any affected mean changes by more than **0.001%**. |
| Export grid | Fit local GPinter on recovered population boundaries and repaired thresholds, then derive a consistent standard **127-bracket** distribution. |
| Gini | Preserve the supplied Gini; report the fitted Gini separately. |

The interior threshold margin is the smaller of
`1e-7 × max(abs(current mean), 1)` and one quarter of the adjacent-mean gap.
Valid thresholds are preserved apart from these reported numerical nudges.
Invalid population weights, unexplained identity failures or unsuccessful fits
stop preparation; no broader automatic cleaning is used.

The concise summary distinguishes population-weight explanations, precision
adjustments, threshold repairs, pooling, grid conversion and remaining definition
warnings. `changes` records each affected country/year/concept/unit, original
bracket, field, before/after values, differences and reason. `impact` compares
means, bottom-50 and top-10/1/0.1 shares and supplied/fitted Gini, explicitly
identifying comparisons whose population boundaries changed. Regridding is not
reported as a direct income correction. Raw exploration findings remain
available. Explanatory notes are grey; headings, tables and results use the
normal terminal foreground.

Acceptance preserves a byte-identical original at
`input_data/dina/WIL_ECU_1990-2022_v2.dta` and writes the prepared distribution
separately to `input_data/dina/WIL_ECU_1990-2022_v2_prepared.dta`. The inbox file is
unchanged. A small inclusion record retains source identity, citation, version,
correction recipe, GPinter version and accepted candidate. Acceptance validates
the complete candidate, backs up prior accepted files and restores them if
publication fails. Reaccepting the same candidate performs no additional
transformation; source or recipe changes require a new candidate review.
Unconfirmed population and currency definitions remain visible. Acceptance does
not run the pipeline. The next 07d automatically integrates the included series.

#### Automatic export integration and optional continuation review

07d selects included finished sources and prepares their continuation from the
accepted distributions, registered policies and current internal/WID inputs.
Source inclusion is sufficient; there is no separate publication approval.
After 07c, an optional preview exposes the same calculations before running 07d:

```bash
dina sources include dina --publication --dry-run
dina sources table dina continuation
dina sources table dina continuation-groups
dina sources table dina continuation-changes --year 2024 --variant group_join
dina sources table dina continuation-impact
dina sources table dina continuation-problems
dina sources table dina continuation-scaling
```

Country, publication concept/unit, coverage, anchor year and citation come from
each registered source. No country is singled out in the integration code.
`--source ID` prepares selected registered sources; omission covers accepted
publication sources. Source exploration and inclusion remain independent of
these later pipeline inputs.

For grouped sources, publication observation years come from the checksum-
verified accepted prepared artifact, not the workbook's historical extent.
BRA's raw 1926–2015 coverage therefore cannot become a full-distribution
publication request. Its fitted Gini is labelled `gpinter_fitted`, with no
researcher-supplied Gini invented; ECU retains its supplied/fitted distinction.
Comparison curves break at missing years rather than drawing interpolated
observations across historical gaps.

Continuation uses four disjoint population groups: **p0–50, p50–90,
p90–99 and p99–100**. For each group, divide its current internal share of
normalized income by its internal share in the supplied anchor year. Apply that
single growth factor to all supplied anchor bracket means and thresholds in the
group, then normalize the resulting distribution and derive consistent shares.
The factor is common within each group; tiny individual-bracket anchor incomes
therefore cannot produce isolated extreme growth factors.

All supplied years are preserved. Years beyond the last internal estimate carry
forward that distribution. The review contains the raw four-group continuation
and a candidate with local repairs where group joins violate ordering. Repairs
restore ordered means while preserving **each group's target income**, followed
by feasible thresholds. The group's total is not redistributed to other groups.
`--adjustment group_join` is the default review candidate; `--adjustment none`
reviews the raw four-group result and cannot be accepted if checks fail.
The older `isotonic` option is not an alias: it used a different bracket-level
method, and a new candidate review is required.

The `continuation-groups` table reports internal anchor/current group shares,
growth factors, target/achieved shares and preservation checks. Changes distinguish
intended group proportional changes, local mean/threshold repairs, and remaining
source-definition warnings. The PDF distinguishes the original supplied file
(shares at its labelled population boundaries), the prepared source (shares at
exact population boundaries), internal estimates, and continuation before/after
join repair. Its second page shows prepared minus original shares in percentage
points. The continuation alternatives share the same prepared historical years;
their difference concerns continuation join repairs, not source preparation.
The latest comparison is also saved beside the regular update figures as
`output/figures/updates/finished-dina-COUNTRY-CONCEPT-UNIT-candidate.pdf`.
The immutable original stays with its review; this figure copy does not activate
the replacement or regenerate the regular export comparisons.
Supplied Gini is retained separately from the grouped Gini calculated for the
continuation. WID scaling is reported but applied only once during 07d. Missing
WID means are reported; they do not imply missing distribution shares, and no
monetary amounts are invented.

Run the regular export to integrate all included sources:

```bash
dina run 07d
```

`dina run 07d`, a pipeline range reaching 07d, and direct invocation of the 07d
do-file all use the same automatic integration. Newly included sources join
automatically. Re-inclusion, policy/code changes, internal estimates or WID
benchmark updates trigger fresh validation and calculations, reusing a matching
validated preview when available. An altered accepted file without a matching
inclusion receipt still fails validation.

The small integration record is a derived cache, not an authorization record.
The optional `dina sources include dina --publication` command prepares and
caches a validated candidate without another prompt; `--include-run RUN` selects
a saved candidate. Legacy `--confirm` is accepted but unnecessary.
`--restore CONFIRMATION` restores a prior cache; 07d then validates it against
current included sources and rebuilds if needed. Preview and cache operations
leave accepted sources, the internal 07c CSV and final exports unchanged.

A failed integration preserves the previous cache and exports. The runner's
dry run reports pending integration without changing either, including when 07c
is planned earlier in the range. Comparison metadata is recorded after refresh.

Historical publication years use the prepared distribution, while the original
supplied file stays unchanged for reference and comparison. Legitimate zero-income
internal brackets are allowed; the overall mean and each of the four continuation
groups' incomes must remain positive. All distributions stay normalized until
07d applies the common WID national-income mean once, including supplied series.
The BRA and ECU publication policies require complete WID income benchmarks.
If a mean remains absent after the extended import, the candidate is blocked
with the affected country/year; no benchmark is invented. Refreshing WID scaling
does not require replacing accepted population, macro, spending or price inputs.

07d inserts validated normalized distributions after its internal rectangularization
and before regional donor averages, so early supplied observations do not extend
other countries backwards. Included contributions bypass legacy automatic mean,
threshold and cumulative-statistic adjustments. Their bottom averages are scaled
once and their middle-group means use income-weighted differences. The existing
DOM-before-2012 exclusion applies only to internal contributions. Source origin,
citation, DOI, adjustment and donor attribution accompany publication; the
internal survey/admin `data_quality` classification is left unspecified for
supplied/continued observations and is 1 for carry-forward observations. The
separate `pub_origin` identifies all three cases without inventing a quality rank.
`pub_gini_method` and `pub_fitted_gini` preserve the distinction between fitted
and supplied source statistics. The historical national observations do not
extend regional calculations before their existing 2000 start or relax the
complete-donor-pool checks.

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

### Delivery workflow

Country runs are available for stages **03a–07d**. Omission of `--countries`
retains the full configured execution. Preparation stages 01–02 keep their
existing workflow and reject country selection. Country codes are validated
against the effective configuration; they do not change that configuration or
the donor universe.

```bash
dina run --from 03a --to 04c --countries ECU,MEX --dry-run
dina run --from 03a --to 04c --countries ECU,MEX --notify
dina run list --countries ECU,MEX
dina run why 06a --countries ECU,MEX
dina run --resume RUN_ID --notify
```

ECU/MEX are examples, not special selection rules. An explicit country run
recomputes its requested contributions even if a whole workbook appears current.
It preserves other countries' microdata, workbook sheets and summary rows.
03c rebuilds the complete observed panel; 05a rebuilds the complete macro comparison;
07b and 07d assemble complete availability and export products. Shared donor
preparation stays complete in 05b/05d. Before a run updates 05a, it saves the small
prepared donor tables; before 05b/05d it compares them and reports additional
affected countries without launching them.

The runner stages only changed outputs. It keeps canonical inputs in place and
publishes a successful task-country contribution together with its shared-table
updates. Failed contributions leave their previous published files intact.
Successful work is recorded in `output/run_logs/RUN_ID/country-run.json`.
Worker logs are kept in their dated `output/run_logs` directory: `.stata.log`
contains Stata output, while `.out.log` and `.err.log` capture process output.
The automatic Stata batch log is also created there, so runner commands do not
leave `.log` files in the repository root. Stata still executes task code from
the repository root, preserving relative input paths. Earlier root logs are
preserved in `output/run_logs/archive/20261024-root-logs.tar.gz`; the adjacent
cleanup manifest records their names and the superseded partial stages removed.
06a checkpoints are country × income step × statistical unit. 07c saves successful
country fits privately until it can assemble and validate the complete CSV.
An interrupted publication is rolled back or completed from its small rename journal.

`--resume RUN_ID` continues that same task list, scope and configuration, reusing
completed chunks. Do not combine resume with `--force`; start a new invocation to
recompute. This is recovery of a specific run, not automatic freshness inference
across future updates. Existing validated inputs and outputs require no migration,
repository-wide hashing, or new historical processing receipts.

Standalone entry points use the same dispatcher and publisher:

```stata
do code/Stata/06a-run-ineqstats.do "ECU,MEX"
```

```bash
Rscript code/R/07c_interpolate_ineqstats.R --countries ECU,MEX
```

Selection ends with that invocation. R rate generators also retain their explicit
comparison-output options. Dry runs report selected computation, completed chunks
to reuse, shared assembly, retained results, and missing prerequisites. They recognize
inputs produced earlier in the requested range; they never launch prerequisites.
`stale` remains an advisory task-level view based on the existing runner checks.

`--notify` retains script names, error excerpts and log locations. A dry run
checks the selected commands without starting calculations.

03c remains after 03a/03b and publishes its observed-theta panel only after its
own source and output checks. 03d uses that accepted panel for observed donors;
03e writes extrapolated merging points to separate `_extrap` sheets. The agreed
`noreplace`, PAVA boundary and merging-point precision fixes remain in place.
Approved historical Admin files and the current scientific outputs are retained.

**Scientific decision clarified on 2026-10-21:** replacing ARG corrections for
years with Admin data by extrapolated BFM corrections was intentional. Preserve
that choice and the resulting series; do not restore the earlier 03b outputs
merely because 03e overwrote their files. Apply the same preservation logic to
CRI, as requested. The diagnostic traced ARG 2007/2012 and CRI 2010 as examples,
not an exhaustive list of cases. Their calculation origin should be described
as an approved extrapolated correction, even where an existing index calls the
case observed. This decision does not change theta values, donor membership,
case selection or published results. Future execution changes must preserve the
chosen method and make its case selection and theta inputs explicit.

**Correction stability review, 2026-10-22:** 03a/03b now capture the original
observed correction in `output/bfm_summary/observed_donors/ISOYYYY.dta`, a small
127-fractile table. 03d reads that table, not corrected microdata that 03e or
later stages can overwrite. Missing tables name the required observed task;
they are never reconstructed from an extrapolated result. The observed panel
and donor table must agree on the merging point. Existing calculation formulas,
donor eligibility and BFM `noreplace` are retained.

`config/theta_extrapolation_policy.csv` explicitly retains the approved ARG
2000–2014 and CRI 2010–2016 replacements alongside ordinary extrapolation targets.
This controls which corrections are published, independently of which observed
years supply donors. Country selection still limits execution. Review runs can
set `DINA_REVIEW_ROOT` for generated donor/panel/adjuster inputs; these reads are
strictly isolated and never fall back to canonical products.

The November 7, 2025 code and generated workbooks used PER 2019/2020 as observed
donors. Their exclusion during the current update followed the absence of their
matching Admin inputs; it was not last year's eligibility rule. The user has now
approved restoring both validated historical `gpinter_PER_YEAR.xlsx` inputs from
`eb2fe44b`; they are registered as approved external inputs in `config/sources.yml`.
The existing generic eligibility and p77 trust rules apply to both years, with
no country-specific exclusion to remove. These files are already BFM-ready:
02e is not a prerequisite for their restoration.

Restoring inputs does not fabricate observed results or change existing exports.
The next selected PER run must start at 03a to rebuild observed corrections and
capture independent donor tables, followed by 03c to publish the reconciled panel
and 03d/03e to recalculate affected extrapolations. Historical corrected microdata
or old merging-point sheets are not substituted for that reconstruction.
Backward survey-provider revisions are accepted source updates and must not be
confused with this donor-selection restoration.

07c retains the narrow threshold repair within the agreed storage-precision
envelope. For larger threshold conflicts with ordered adjusted bracket means,
it now uses GPinter `shares_fit()` to reconstruct thresholds from the unchanged
population boundaries and bracket means. This applies to every country. The
fit must preserve income and population anchors at every supplied boundary;
it cannot change the scientific tail multipliers or redistribute income to make
the fit succeed. Existing conservative precision handling of tied thresholds
remains available.

Reversed adjusted means that cannot be handled by the existing precision rule
still fail the affected group and preserve the previous publication. Those
require a separate decision about the tail-income redistribution. Worker
reports distinguish `gpinter_precision_adjustments.csv` from
`gpinter_threshold_reconstruction.csv`; `gpinter_fitting_methods.csv` records
each group's method, status and failure reason.
Reports and bounded comparisons are under
`output/experiments/correction-stability-review/`. No production acceptance is
implied by generating these candidates.

**CRI delivery clarification, 2026-10-22 (accepted):** the remaining differences
from the November 2025 publication primarily reflect corrected tail processing,
not revised Costa Rican country-SNA workbooks. The existing top-0.1% and
top-0.01% multipliers remain 2.18 and 7.23, with the existing 2016/2021 exceptions.
Correcting `cRI` to `CRI` makes the top-0.01% rule effective as intended. The
approved GPinter route reconstructs conflicting thresholds while preserving
the retained population and income anchors, replacing the old cleaner's
clipping of bracket incomes. Where the approved tail adjustment requires
consolidation or a feasible top-0.01% target, the separate adjustment report
records that change. This acceptance does not change the BFM `noreplace` rule,
the approved 2010–2016 extrapolated replacements, or the backward carry of the
2010 distribution to 2000–2009. The WID benchmark has small revisions; the ten
consumed CRI country-SNA workbooks for 2012–2021 are unchanged from the previous
delivery. These distinctions should be retained in the delivery change report.

**ARG/PER delivery review, 2026-10-23:** ARG retains the approved common median
correction for every year from 2000 through 2014. Controlled comparisons identify
real survey revisions: the 2012 input now combines Q3 and Q4, while the previous
sample corresponds to Q4; the 2007 sample is unchanged in size but has revised
wage/mixed-income classifications and other-income values. Updated distributions
entering final fitting explain most of the top-1 difference in 22 of 23 comparable
years. Final cleaning/fitting is the larger contribution in 2002. The bounded
2007/2012 survey tests and remaining historical reconstruction limitations are
recorded in `output/experiments/arg-change-attribution-20261022/`; these comparisons
do not imply that all upstream differences can be assigned to raw surveys.

For PER, 01b now assembles the accepted 2007–2024 annual CEI tables through the
existing source contract, replacing its use of the frozen historical merged
workbook. Accepted source files remain unchanged. The 2019/2020 Admin donors have
been restored and recalculated; 2015/2021 are included for comparison before the
user decides their final treatment. Non-PER observed and pseudo-theta rows were
verified unchanged. Run `20261023-002017-countries-39425-file9a016479d42e` continues
the PER-only calculation through 07c. Its separate completion job prepares a
staged 07d export and full old/current/proposed comparison under
`output/experiments/per-completion-review/export/`; it does not activate the final
publication. `completion.json` in that review folder's parent records progress
or failure. Changed PER donor inputs also affect ARG, CHL, SLV and URY from 05b;
those additional country runs remain pending user approval.

The DOM macro repair on 2026-10-21 restored
`input_data/sna_country_data/DOM/cei.xlsx` byte-for-byte from the historical
`DOM/old/cei.xlsx`. A missing-file guard now stops 01b before it writes outputs;
source exploration reports absent accepted/incoming files as unavailable.
01b and 05a completed. The restored 2014–2016 composition reduces DOM's later
capital multiplier from 42.8155 to 25.5022 (historical: 25.6783). Non-DOM source
macro values are unchanged, but the existing donor calculations propagate the
repair into prepared 05b inputs for ARG, CHL, SLV and URY. CRI and 05d's prepared
tax/spending inputs are unaffected. The user subsequently approved downstream
processing from 05b for DOM, ARG, CHL, SLV and URY, preserving the accepted
BFM/theta choices above. That invocation is recorded under
`output/run_logs/20261021-163028-countries-88446-file1597e8ef13cd/`.
The completed checkpoint and before/after comparisons are recorded under
`output/run_logs/20261021-160309-countries-86204-file150bc43f1ff43/`.

For this delivery, the targeted completion begins at 03a for ECU and 03b for MEX:
their recovered Admin sources exist, while their corrected survey files are missing.
After their 03a–04c checkpoint, inspect the donor comparison before continuing
with 05a–07d. The separate production repair is 03d–04c for CRI/SLV (the 22
recorded extrapolation failures). The accepted theta changes also need downstream
propagation for ARG, BRA, CHL, COL, DOM, PER and URY from 04b, alongside CRI/SLV
after repair. These are known unfinished calculations, not a requirement to restart
at stage 01 or rebuild countries merely because tracking records are absent.

The separate preparation previews are:

```bash
dina run --from 03d --to 04c --countries CRI,SLV --dry-run
dina run --from 04b --to 04c --countries ARG,BRA,CHL,COL,DOM,PER,URY --dry-run
```

Review and complete the needed preparation checkpoints before the final 05a–07d
pass, so the expensive statistics work uses the intended corrected surveys.
The October 19 fixture checks and the three delivery previews are saved under
`output/run_logs/previews/20261019-*`. No production calculations were launched
to create those reports.

### Complete delivery sequence

Keep the existing source and preparation results from stages 01–02. Finish the
three preparation scopes before calculating the final statistics:

| Checkpoint | Scope | Purpose |
| --- | --- | --- |
| 03a–04c | ECU, MEX | Produce missing corrected surveys using the recovered Admin inputs. |
| 03d–04c | CRI, SLV | Apply the extrapolation repairs and rebuild the affected survey variables. |
| 04b–04c | ARG, BRA, CHL, COL, DOM, PER, URY | Propagate the accepted BFM/theta work into survey variables, reusing existing effective rates. |
| 05a–05d | All 11 configured countries | Rebuild macro comparisons and complete income/tax imputations. |
| 06a | All 11 configured countries | Compute statistics for the updated surveys, with resumable country/step/unit chunks. |
| 06b–07d | All 11 configured countries | Produce figures, synthetic files, availability reports, GPinter groups and final WID exports. |

The downstream scope is the union of the three unfinished preparation scopes.
It does not require repeating 01–02 or repeating observed BFM for the seven
countries in the third checkpoint. The current configuration calculates surveys
through 2024 and extrapolates the final export through 2025.

This shell block runs the six checkpoints sequentially and stops at the first
failure. The countries variable exists only inside the subshell:

```bash
(
  set -e
  delivery_countries=ARG,BRA,CHL,COL,CRI,DOM,ECU,MEX,PER,SLV,URY

  dina run --from 03a --to 04c --countries ECU,MEX --notify
  dina run --from 03d --to 04c --countries CRI,SLV --notify
  dina run --from 04b --to 04c --countries ARG,BRA,CHL,COL,DOM,PER,URY --notify
  dina run --from 05a --to 05d --countries "$delivery_countries" --notify
  dina run 06a --countries "$delivery_countries" --notify
  dina run --from 06b --to 07d --countries "$delivery_countries" --notify
)
```

If a checkpoint fails, correct that error and use its printed
`dina run --resume RUN_ID --notify` command. After it succeeds, run only the
subsequent checkpoints; do not paste the whole block again. Do not use `--force`
with resume or launch overlapping country runs in parallel.

After successful 07d publication, run `dina results show` and review the new
comparison figures with `dina results`. The dated deliverables are in
`output/latest_wid_series/`; the long, wide and Amory versions share the run date.
The supporting statistics, synthetic files, GPinter CSV and availability report
are updated by their preceding checkpoints. A successful script run still needs
the final scientific review of the comparison figures before handoff.

Task `02d5-interpolate-admin-data` runs the installed `gpinter` R package on
each configured country's applicable `_clean` Admin tabulations. It creates the
`gpinter_output/total-pos-COUNTRY.xlsx` workbooks (`total-pre-BRA.xlsx` for
Brazil), plus `total-pre-CHL.xlsx` for Chile's effective-rate calculation.
The post-tax workbooks are consumed by `02e-format-for-bfm`. The run preflight reports a missing
`_clean` workbook before starting tasks; the interpolation task fits and checks
all selected countries before replacing any `gpinter_output` workbook. The
generated manifest in `output/data_reports/admin_gpinter_manifest.json` records
the package version, source hashes (including Brazil's publisher averages for
2000, 2002, and 2006), output hashes, and covered years. Ecuador is excluded
until a current `_clean` workbook is available; Chile 2012 is excluded from
post-tax interpolation because its bracket average is below its own threshold,
but remains in pre-tax interpolation for the effective-rate calculation.
Uruguay's 127-row files come
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
Availability covers the export horizon as well as the calculation years. Years
beyond the calculation endpoint are classified as carry-forward, so their flat
extension remains visible in the comparisons. The export stops with a request to
rerun 07b if the availability report is missing a configured country-year.

Changing the baseline does not regenerate existing graphs. Use the existing
export task (`dina run 07d`) when ready. Other output comparisons and standalone
graph regeneration are outside this workflow, except for the presentation-only
supplied-series figures described below.

### Delivery reports and supplied-series figures

Keep the important delivery reports beside the comparison graphs in
`output/figures/updates/`. The completed 24 October 2026 report is
`dina-update-final-24Oct2026.pdf`; it covers all 26 published countries and the
accepted changes. Its production checks are summarized in
`delivery-validation-24Oct2026.json`. The previous report path under `output/pdf/`
remains available for existing references.

Generate single-page supplied-series comparison figures for a LaTeX document:

```bash
Rscript --vanilla code/R/plot-finished-dina-for-latex.R
# Optional: select any countries with an active supplied series.
Rscript --vanilla code/R/plot-finished-dina-for-latex.R ECU BRA
```

Omission includes all active supplied-series countries. The command reads the
accepted source and saved continuation, checks their current inputs, and writes
`finished-dina-COUNTRY-STEP-UNIT-latex.pdf` in the same updates folder. These
figures omit overall titles and notes, retain income-group panel labels, and
use the legend Original, Prepared, Internal estimates, Final series, and
Unadjusted continuation. Historical gaps and the final carry-forward remain
visible. This command only regenerates presentation files; it does not fit
distributions, activate candidates, or rerun the pipeline. The original review
figures and acceptance records retain their detailed notes.

Experimental cleanup should target superseded staging directories. Keep active
source candidates, confirmation/restore backups, source receipts, completed run
records, and the small diagnostic tables and scripts used by delivery reports.
Record any removed staging paths in the updates folder so an old candidate's
unavailable staging area is explicit.

### Holding a supplied series for scientific review

An included source can be retained for review while excluded from publication.
In its `finished_series.publication_policy` entry in `config/sources.yml`, set
`status: hold` and a brief `hold_reason`. The default status is `active`.
Exploration, source files and preparation evidence remain available. The next
07d export uses the country's internal estimates, including in regional
calculations. Explicit activation of a held source fails with its hold reason;
re-including its source does not lift the hold.

On 24 October 2026, `bra-morgan-souza-2026` was put on hold at the user's request.
Its review graphs are in `output/figures/updates/on-hold/BRA/`. Ecuador's supplied
series remains enabled. A future scientific decision must explicitly restore
Brazil's publication status before exporting the supplied series again.

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


**COL/ECU source handoff, 2026-10-23:** the accepted 2024 national-account releases are now selected by 01b, including their revised historical sheets. Ecuador retains its existing pre-2018 source splice; the new release uses CEI_2023 and CEI_2024p. Required sheets are checked before replacing macro output, so missing new-year inputs fail explicitly instead of silently retaining older coverage. The underlying component extraction and treatment of zero placeholder rows are unchanged. Bounded Stata checks matched 64 source components within existing float precision, and legacy/failure tests passed. The user approved downstream refresh for COL, ECU, ARG, CHL, SLV and URY; any additional donor recipients are reported before expanding execution.

**Export aggregate correction, 2026-10-23:** the user approved correcting inherited bottom and middle group aggregates in 07d. The bottom 20%, 50% and 90% now stop at their stated population boundaries; previously the internal-series aggregates included the following one-percent bracket. The middle 40% consequently covers p50-p90. Bottom-group monetary means receive the common WID national-income scaling, and the middle-group mean is calculated from the two groups' income totals and population masses. Detailed distributions, top shares and upstream calculations are unchanged by this export correction. Supplied-series aggregates already followed these definitions.

Historical bottom/middle aggregates are reconstructed from their detailed brackets only in the temporary comparison dataset. Archived publications remain intact, and comparisons require complete reference brackets. The diagnostic and bounded actual-Stata/independent checks are recorded under `output/experiments/final-completion-20261023`; the final production export must pass the same aggregate identities before delivery. This is a correction to exported aggregation and units, separate from changes in source data or distribution estimation.

### Final export consistency

07d runs the R consistency checker before regional averaging, after regional
assembly, and on the saved combined export. Internal and supplied distributions
therefore receive the same treatment, including their contribution to regional
estimates. All calculations and exported numbers retain double precision.

Positive flat segments receive a small population-weighted slope, preserving
their total income and population. The maximum target change in a bracket mean
is 0.0001%, reduced further when neighboring values constrain the correction.
Zero-income groups remain unchanged. Only repeated or incompatible thresholds
are moved; shares and cumulative statistics are then recomputed consistently.
These are export consistency corrections, separate from the scientific tail
adjustments already made in 07c.

Repair and audit use the same `1e-12` relative near-equality tolerance, without a
scale floor of one. This stricter delivery check also catches pairs just outside
the former `128 * double epsilon` criterion. Preparation accepts downward
rounding noise within `4096 * double epsilon` (about 9.1e-13 relative) and includes
neighboring nearly flat means
within `16384 * double epsilon` when smoothing a defective plateau. This avoids
leaving an almost-equal neighbor that squeezes the corrected spacing back into
numerical equality; already-valid increasing runs stay unchanged. The per-bracket
change remains bounded by 0.0001%; population and total income are preserved. Both repair validation
and the saved-file audit reject exact and numerically indistinguishable positive
repetitions. Larger decreases remain reported without discarding brackets.

Consistency findings do not stop the run. The terminal distinguishes a clean
result from one needing review and links to a short summary and a detail CSV in
`output/data_reports/consistency/DATE/`. The before/after review PDF is in
`output/figures/updates/consistency-DATE.pdf`; the regular figures also use the
corrected export. Headline-share changes above 0.01 percentage points are
highlighted for review, without requiring confirmation during execution.

The PDF review starts with a compact country table. It only draws comparisons
for headline shares changing by at least 0.1% relative to their previous value;
smaller changes stay in the detailed CSV. Qualifying regional trajectories are
grouped together, with identical curves drawn once. If no change reaches the
display filter, the PDF contains the table only. This presentation filter does
not change scientific results or the complete consistency diagnostics.

WID benchmark requirements cover all publication countries, including regional
recipients and supplied historical observations. Coverage completion preserves
accepted benchmarks and adds available missing information. A benchmark that
remains unavailable is reported; its monetary records remain explicitly missing
in the export, while available distribution shares remain usable. No benchmark
is invented or carried forward by the checker.

To audit a delivered file without changing it, use `dina results check [FILE]`.
Omitting the file selects the latest recorded main export. To regenerate after
completing benchmark coverage, use `dina run 07d --notify`; upstream statistics
and GPinter do not need to be repeated. The combined main export includes every
selected income concept. The redundant `_amory` subset is no longer generated;
historical files are retained.
