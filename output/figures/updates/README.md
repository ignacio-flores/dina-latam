# Delivery figures and reports

## Current delivery: 24 October 2026

| File | Contents |
| --- | --- |
| `dina-update-final-24Oct2026.pdf` | All-country review: final comparisons, explanations, source coverage and validation. |
| `finished-dina-ECU-nat-esn-latex.pdf` | Ecuador comparison, ready for LaTeX; no overall title or notes. |
| `on-hold/BRA/finished-dina-BRA-nat-esn-latex.pdf` | Supplied Brazil series: review only, on hold pending a scientific decision. |
| `update-24Oct2026-sptinc992j-*.pdf` | Regular comparisons with the previous delivery, by income group. |
| `finished-dina-ECU-nat-esn-candidate.pdf` | Detailed Ecuador source comparison, including preparation differences and explanatory notes. |
| `delivery-validation-24Oct2026.json` | Completed run IDs, output coverage and passed scientific/presentation checks. |
| `cleanup-24Oct2026.json` | Removed redundant staging directories and the retained evidence supporting cleanup. |

The clean supplied-series graphs retain the original plotted values and
historical gaps. Their legends distinguish Original, Prepared, Internal
estimates, Final series, and Unadjusted continuation. Panel labels identify
income groups; the dotted vertical line marks the continuation anchor.

The supplied Brazil series is on hold. Current publication graphs and data use
Brazil's internal survey-and-tax estimates. Its supplied files and review
figures remain available; Ecuador's supplied series remains in the publication.

Regenerate these presentation PDFs from the accepted data at the repository root:

```bash
Rscript --vanilla code/R/plot-finished-dina-for-latex.R
```

Optional ISO3 arguments select active supplied-series countries. This command
does not run the production pipeline. The final data files are in
`../../latest_wid_series/`.
