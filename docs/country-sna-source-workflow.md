# Country-SNA source workflow

This document describes the active experimental country-SNA source workflow.
The workflow is available in the CLI, but it does not replace `01b`, does not
run the production pipeline, and keeps generated artifacts under disposable
experiment folders.

## Commands

```bash
dina sources explore country-sna [--dry-run] [--output-dir PATH] [--country ISO]
dina sources include country-sna [--dry-run|--apply] [--exploration-run PATH] [--output-dir PATH]
```

`explore` reads `input_data/_new/country_sna`, inventories old and new source
files, detects likely years and broad layout changes, and writes expectations
for the include step. It focuses on source structure and coverage, not on
claiming that every candidate value has been adaptively extracted.

`include --dry-run` is the default inclusion assessment. It consumes the latest
exploration run, applies the deterministic country-SNA include contract, and
reports whether expected values were found, missing, revised, ambiguous, or
blocked by an adapter/layout issue.

`include --apply` is visible but guarded. It requires a clean dry-run manifest
for the same exploration run, promotes only approved `_new` source files whose
destinations are unambiguous, refuses overwrites with different content, and
never edits `01b` or pipeline outputs.

Pipeline execution remains explicit:

```bash
dina run 01b --dry-run
dina run 01b
```

## Output Folders

Generated artifacts stay in cleanup-friendly experiment roots:

- `output/experiments/country_sna_explore/`
- `output/experiments/country_sna_include/`

Deleting those folders removes the generated explorer/include outputs. The
only exception is the guarded source promotion performed by `include --apply`.

## Explorer Logic

The explorer answers the first-review questions:

- Which country-SNA files are present in `_new`?
- Which years appear to be available?
- Which years look like extensions beyond the old source coverage?
- Which years overlap and may contain retroactive revisions?
- Does the workbook family look comparable, shifted, or adapter-breaking?
- Which country-year-variable values should the include step expect?

The expected variables come from the deterministic include contract. Adaptive
table, role, and value candidates are still written as developer evidence, but
they are not surfaced as thousands of user review actions.

## Include Logic

The include step uses the deterministic contract to check explorer
expectations. Its top-level statuses are:

- `all_good`: expected values were found without blocking issues.
- `check_following`: one or more expected values are missing, revised, or
  ambiguous enough to review.
- `blocked`: the explorer indicates a layout family or adapter issue that the
  deterministic include contract cannot resolve.

Variable-level statuses include:

- `ok_value_found`
- `ok_contract_missing`
- `revision_detected`
- `warning_missing_expected_value`
- `warning_ambiguous_match`
- `blocked_adapter_required`

## Benchmark Logic

The current Stata-compatible extraction remains benchmark v0. The deterministic
include experiment has a parity report against `UNDATA-WID-Merged.dta`, and a
green parity report makes it a replacement candidate. It is not the active
benchmark provider until the replacement gate is explicitly accepted.

Future benchmarks should be accepted workflow snapshots rather than the old
hardcoded extractor. A snapshot should record source fingerprints, extraction
contract version, accepted output values, include run id, reviewer, review
date, notes, and any accepted exceptions.

## Replacement Gate

The deterministic include code can replace the hardcoded country-SNA extraction
only when the gate below passes and the evidence is committed as documentation
or test fixtures:

- Numeric parity has zero unexplained differences after the documented
  Stata-float tolerance.
- Missing-value parity has zero unexplained mismatches.
- Brazil special logic is reproduced and covered by a named expectation.
- Uruguay auxiliary logic is reproduced without writing `cei.xlsx` or any other
  canonical source file during extraction.
- Ecuador exclusions are reproduced and covered by a named expectation.
- Every country-specific exception is centralized, documented, and linked to a
  parity expectation.
- The parity report names all source files, output variables, years, countries,
  tolerances, and unresolved differences.

## Current Scope

This workflow is active, experimental, and country-SNA specific. Similar source
families should reuse the same shape later: an explorer that builds structural
expectations, an include step that applies a deterministic contract, and a
guarded apply path that never silently rewrites production data.
