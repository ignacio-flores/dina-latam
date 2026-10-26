// Rebuild legacy bottom/middle aggregates in memory for like-for-like plots.
// Never save over the archived publication. Retain its detailed and top rows.
preserve
tempfile dina_old_comparison dina_old_expected dina_old_means dina_old_shares dina_old_fixes
quietly save `dina_old_comparison'
quietly keep if substr(widcode, 1, 1) == "s" & ///
    inlist(p, "p0p20", "p0p50", "p0p90", "p50p90")
quietly keep country year widcode
quietly duplicates drop
quietly count
if r(N) == 0 {
    restore
    exit
}
quietly save `dina_old_expected'

quietly use `dina_old_comparison', clear
quietly keep if substr(widcode, 1, 1) == "a" & p == "p0p100"
quietly replace widcode = "s" + substr(widcode, 2, .)
quietly rename value dina_overall_mean
quietly keep country year widcode dina_overall_mean
isid country year widcode
quietly save `dina_old_means'

quietly use `dina_old_comparison', clear
quietly keep if substr(widcode, 1, 1) == "s"
quietly gen double dina_lo = real(substr(p, 2, strpos(substr(p, 2, .), "p") - 1))
quietly gen double dina_hi = real(substr(p, strpos(substr(p, 2, .), "p") + 2, .))
quietly keep if dina_lo >= 0 & dina_hi <= 90 & abs(dina_hi - dina_lo - 1) < 1e-8 & ///
    abs(dina_lo - round(dina_lo, 1)) < 1e-8
isid country year widcode dina_lo
quietly gen double dina_nbins = 1
quietly gen double dina_fixp0p20 = cond(dina_lo < 20, value, 0)
quietly gen double dina_fixp0p50 = cond(dina_lo < 50, value, 0)
quietly gen double dina_fixp0p90 = value
quietly gen double dina_fixp50p90 = cond(dina_lo >= 50, value, 0)
quietly gen byte dina_missing = missing(value)
quietly collapse (sum) dina_fix* dina_nbins dina_missing (min) dina_lo (max) dina_hi, by(country year widcode)
quietly merge 1:1 country year widcode using `dina_old_expected', nogen
quietly count if dina_nbins != 90 | dina_lo != 0 | dina_hi != 90 | dina_missing != 0
if r(N) > 0 {
    di as error "Previous-export comparisons require complete detailed p0-p90 brackets:"
    list country year widcode dina_nbins if dina_nbins != 90 | dina_lo != 0 | dina_hi != 90 | dina_missing != 0, noobs
    restore
    exit 459
}
quietly drop dina_nbins dina_lo dina_hi dina_missing
quietly reshape long dina_fix, i(country year widcode) j(p) string
quietly merge m:1 country year widcode using `dina_old_means', keep(master match) nogen
quietly save `dina_old_shares'
quietly replace dina_fix = dina_fix * dina_overall_mean / ///
    cond(p == "p0p20", 0.2, cond(p == "p0p50", 0.5, cond(p == "p0p90", 0.9, 0.4)))
quietly replace widcode = "a" + substr(widcode, 2, .)
quietly append using `dina_old_shares'
quietly keep country year widcode p dina_fix
isid country year widcode p
quietly save `dina_old_fixes'
restore
quietly merge 1:1 country year widcode p using `dina_old_fixes', keep(master match)
// A missing historical monetary benchmark must not retain a normalized mean.
quietly recast double value
quietly replace value = dina_fix if _merge == 3
quietly drop dina_fix _merge
