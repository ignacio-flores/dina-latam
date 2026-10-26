// Capture the existing 03d donor calculation once, while the observed
// correction has just been saved. 03e must never write this product.
capture program drop dina_observed_donor
program define dina_observed_donor
    syntax, COUNTRY(string) YEAR(integer) MPFILE(string) CORFILE(string) STAGE(string)
    if "${bfm_replace}" != "no" {
        di as error "Observed donor capture requires BFM noreplace."
        exit 459
    }
    local raw_weight "_fep"
    if "`country'" == "ARG" local raw_weight "n_fep"
    local cor_weight "_weight"
    local income "${y_postax_tot}"
    if "`country'" == "BRA" local income "${y_pretax_tot_bra}"
    if "`country'" == "CRI" local income "${y_pretax_tot}"
    if "`country'" == "PER" local income "${y_postax_tot_per}"
    if "`country'" == "MEX" local income "${y_postax_formal_wage}"
    if "`country'" == "ARG" local income "${y_postax_private_wage}"

    // Use the matching observed sheet, never a later extrapolated sheet.
    preserve
        quietly import excel "`mpfile'", sheet("`country'`year'") firstrow clear
        quietly summarize mpoint, meanonly
        local observed_mp = r(mean)
        assert !missing(`observed_mp') & `observed_mp' > 0 & `observed_mp' < 1
        quietly summarize p if antitonic <= 0.1, meanonly
        local unobs_cutoff = r(min)
    restore

    preserve
        // postbfm summarize leaves its statistics table in memory. Read the
        // freshly saved observed correction, preserving that caller state.
        quietly use "`corfile'", clear
        confirm numeric variable `income' `raw_weight' `cor_weight' _pid
        // Preserve the previous 03d arithmetic, including its weighted
        // collapse and nonlinear interpolation of unoccupied fractiles.
        tempvar freq F
        foreach type in raw cor {
            quietly summarize ``type'_weight', meanonly
            local poptot_`type' = r(sum)
        }
        local ratio = round(`poptot_cor' / `poptot_raw' * 100)
        if missing(`ratio') | `ratio' != 100 {
            di as error "Observed donor `country' `year': corrected population is `ratio'% of original."
            exit 459
        }
        sort `income' _pid
        quietly generate `freq' = `raw_weight' / `poptot_raw'
        quietly generate `F' = sum(`freq')
        capture drop ftile
        quietly egen ftile = cut(`F'), ///
            at(0(0.01)0.99 0.991(0.001)0.999 0.9991(0.0001)0.9999 ///
            0.99991(0.00001)0.99999 1)
        quietly replace ftile = 0.99999 if missing(ftile)
        quietly collapse (sum) `raw_weight' `cor_weight' ///
            [weight = `raw_weight'], by(ftile)
        quietly generate pseudo_theta = `raw_weight' / `cor_weight'
        if _N != 127 {
            tempfile occupied
            quietly save `occupied'
            clear
            quietly set obs 127
            quietly generate ftile = (_n - 1)/100 in 1/100
            quietly replace ftile = (99 + (_n - 100)/10)/100 in 101/109
            quietly replace ftile = (99.9 + (_n - 109)/100)/100 in 110/118
            quietly replace ftile = (99.99 + (_n - 118)/1000)/100 in 119/127
            quietly merge 1:1 ftile using `occupied', nogen
            quietly nl (pseudo_theta = {a = 1} * (1 - ftile)^-{b = 1})
            quietly predict predicted
            quietly replace pseudo_theta = predicted if missing(pseudo_theta)
            quietly drop predicted
        }
        quietly generate inter = 1 if missing(`raw_weight', `cor_weight')
        quietly rename (`raw_weight' `cor_weight') (raw_weight_sum corrected_weight_sum)
        quietly generate str3 country = "`country'"
        quietly generate year = `year'
        quietly generate mpoint = `observed_mp'
        quietly generate double unobs_cutoff = `unobs_cutoff'
        quietly generate double raw_population = `poptot_raw'
        quietly generate double corrected_population = `poptot_cor'
        quietly generate str32 income_variable = "`income'"
        quietly generate str32 raw_weight_variable = "`raw_weight'"
        quietly generate str3 capture_stage = "`stage'"
        quietly generate str24 source_method = "observed_bfm_noreplace"
        quietly generate recipe_version = 1
        assert _N == 127
        isid country year ftile
        assert !missing(ftile, pseudo_theta) & pseudo_theta > 0
        order country year ftile pseudo_theta mpoint unobs_cutoff
        sort ftile
        capture mkdir "${dina_output_root}/output"
        capture mkdir "${dina_output_root}/output/bfm_summary"
        capture mkdir "${dina_output_root}/output/bfm_summary/observed_donors"
        quietly save "${dina_output_root}/output/bfm_summary/observed_donors/`country'`year'.dta", replace
    restore
end
