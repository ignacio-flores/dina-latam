* DINA country-run entry; the worker writes only to its temporary output folder.
args dina_requested_countries
local dina_worker : environment DINA_COUNTRY_WORKER
if "`dina_worker'" != "07d-export-results-to-wid" {
    do "code/Stata/auxiliar/dina_country_task.do" "07d-export-results-to-wid" "`dina_requested_countries'"
    exit
}
local dina_output_env : environment DINA_OUTPUT_ROOT
local dina_countries_env : environment DINA_RUN_COUNTRIES
if `"`dina_output_env'"' == "" | trim("`dina_countries_env'") == "" {
    di as error "Missing country-run output folder or selected countries."
    exit 198
}
global dina_output_root `"`dina_output_env'"'
global dina_run_countries "`dina_countries_env'"

*Export data to WID database

//general settings
*ssc install gtools 
*ssc mipolate
*findit renvars 
clear all

//preliminary
global aux_part  ""preliminary"" 
do "code/Stata/auxiliar/aux_general.do"

// DINA supplies these export-validation settings from the effective YAML config.
// Fail loudly if the runtime config was not provided instead of using stale defaults.
foreach required in export_unit export_steps export_last_y previous_update {
	if `"${`required'}"' == "" {
		di as error "Missing required DINA config global: `required'"
		di as error "Run this task through dina run, or provide DINA_CONFIG_DO explicitly."
		exit 198
	}
}
local unit "$export_unit"
local steps: copy global export_steps
local ly = $export_last_y

// Capture the settings actually used by this export; publish only after success.
local date "$S_DATE"
local date = subinstr("`date'", " ", "", .)
cap mkdir "${dina_output_root}/output/figures"
cap mkdir "${dina_output_root}/output/figures/updates"
tempfile comparison_request
tempname comparison_handle
file open `comparison_handle' using "`comparison_request'", write text replace
file write `comparison_handle' `"$previous_update"' _n `"$export_unit"' _n `"$export_steps"' _n
file write `comparison_handle' "$export_last_y" _n `"$all_countries"' _n "$first_y" _n "$last_y" _n
file close `comparison_handle'

// Integrate all included finished sources using the registered continuation
// policies. Failed validation leaves the previous integration and exports intact.
local dina_finished_dir "${dina_output_root}/_finished_dina"
// Stata shell does not reliably propagate R's exit status. Remove only the
// temporary adapter files so a failed refresh cannot reuse an earlier overlay.
cap erase "`dina_finished_dir'/overlay.dta"
cap erase "`dina_finished_dir'/attribution.dta"
shell Rscript code/R/07d_prepare_finished_dina.R "`comparison_request'" "`dina_finished_dir'"
confirm file "`dina_finished_dir'/overlay.dta"
confirm file "`dina_finished_dir'/attribution.dta"
preserve
	quietly use "`dina_finished_dir'/overlay.dta", clear
	local dina_finished_count = _N
restore
local dina_finished_fields ""

// Record the validated integration consumed after the automatic refresh.
cap erase "${dina_output_root}/output/figures/updates/comparison-`date'.json"
cap erase "${dina_output_root}/output/figures/updates/comparison-`date'.pending.json"
shell Rscript code/R/export-comparison-metadata.R begin `date' "`comparison_request'"
confirm file "${dina_output_root}/output/figures/updates/comparison-`date'.pending.json"

//compare with previous update? 
qui use $previous_update, clear 
// Correct legacy aggregate labels only in this temporary comparison copy.
// The archived publication, detailed brackets and top shares stay unchanged.
do "code/Stata/auxiliar/dina_export_comparison_aggregates.do"
foreach optional in data_quality comment extrap_points interp_points {
	cap drop `optional'
}
tempfile tf_prev
qui save `tf_prev'


//create folders if necessary 
local dirpath "${dina_output_root}/output/figures/updates"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

//I.------------------------------------------------------


qui import excel ///
	"output/data_reports/latam_availability.xlsx", clear firstrow
// Availability must cover the export horizon, including carry-forward years.
// Otherwise the merge below would silently classify those years as no data.
isid country year
foreach c in $all_countries {
	forvalues y = $first_y/`ly' {
		quietly count if country == "`c'" & year == `y'
		if r(N) != 1 {
			di as error "Availability report is missing `c' `y'. Run dina run 07b,07d."
			exit 459
		}
	}
}
qui keep country year grade
tempfile tf_ava 
qui save `tf_ava'

//open longformat here and keep relevant info 
qui import delimited ///
	using "output/ineqstats/_gpinter_all_topcorrected.csv", clear 	

*qui import delimited step unit country year  ///
*	using "output/synthetic_microfiles/_smicrofile_long_detailed.csv", clear

//clean 	
qui gen keeper = .
foreach s in `steps' {
	qui replace keeper = 1 if step == "`s'" & unit == "`unit'"
}
qui drop if missing(keeper)		
qui replace p = round(p * 10^5)
qui do "code/Stata/auxiliar/aux_exclude_ctries.do"
qui drop if exclude == 1 
qui drop exclude 
qui rename (avg bckt_sh) (bracketavg s)
qui sort country year p
qui keep step country year p bracketavg s thr topsh topavg 
qui order step country year p bracketavg s thr topsh topavg 

// Every requested concept must exist for every configured source country.
// Never replace missing disposable estimates with the pretax distribution.
foreach concept in `steps' {
	if !inlist("`concept'", "nat", "pon", "pod") {
		di as error "Unsupported export income concept: `concept'"
		exit 198
	}
	foreach c in $all_countries {
		quietly count if step == "`concept'" & country == "`c'"
		if r(N) == 0 {
			di as error "Missing `concept'/`unit' GPinter results for `c'. Run its statistics and 07a-07c first."
			exit 459
		}
	}
}
// Monetary ratios use precisely the internally observed export cases, after
// the same scientific exclusions as the distributions above.
tempfile dina_income_observed
preserve
	keep if p == 0
	keep country year step
	isid country year step
	export delimited using "`dina_income_observed'", replace
restore

*normalize variables in lcu 
tempvar average 
bysort step country year: egen `average' = min(topavg)
foreach var in "bracketavg" "thr" "topavg" {
	qui gen `var'_norm = `var' / `average'
	if "`var'" != "bracketavg" qui replace `var' = `var'_norm 
}

*II. Extrapolate/Interpolate data points within country ------------------------

*create an obs with last year if necessary 
qui count if year == `ly'
if `r(N)' == 0 {
	local so = _N + 1 
	local fw : word 1 of `steps'
	qui set obs `so'
	qui replace country = "BRA" in `so'  
	qui replace year = `ly' in `so'  
	qui replace p = 0 in `so'  
	qui replace step = "`fw'" in `so'
}

*rectangularize 
qui fillin country year step p
qui replace _fillin = 1 if step == "`fw'" & country == "BRA" & year == `ly'

*interpolate within countries 
sort country p year 
foreach v in "thr_norm" "bracketavg_norm" {
	bysort step country p: ipolate `v' year, gen(ip_`v'1)
	bysort step country p: mipolate ip_`v'1 year, gen(ip_`v'2) forward epolate 
	bysort step country p: mipolate ip_`v'2 year, gen(ip_`v'3) backward epolate 
}

*fill values in 
qui rename _fillin imputed 
qui gen extrap = 1 if missing(ip_bracketavg_norm1) 
foreach v in bracketavg thr {
	qui replace `v'_norm = ip_`v'_norm3 if imputed == 1 
	qui replace `v' = `v'_norm if imputed == 1 
}
qui drop ip_*

// Overlay after fillin: an early supplied year must not backcast other countries.
// Remember legacy storage before the double-precision supplied rows are added.
local dina_legacy_floatvars ""
local dina_legacy_p_type : type p
foreach v in bracketavg bracketavg_norm s thr topavg topsh {
	local dina_storage : type `v'
	if "`dina_storage'" == "float" local dina_legacy_floatvars "`dina_legacy_floatvars' `v'"
}
if `dina_finished_count' > 0 {
	do "code/Stata/auxiliar/dina_finished_overlay.do" "`dina_finished_dir'/overlay.dta"
	quietly merge m:1 country year step using "`dina_finished_dir'/attribution.dta", keep(master match) nogen
	local dina_finished_fields "finished_dina bottomsh bottomavg pub_*"
}

*get bracket population share
qui gsort step country year -p
bysort step country year: gen bracketpop = p[_n-1] - p  
qui replace bracketpop = 1 if p == 99999

*define temporary variables 
tempvar fy1 sum_fy1 sum_pop1 auxi1 avg1 test1  auxi2 aux_bottomsh1 aux_bottomsh2

*compute top averages
qui gsort step country year -p	
bysort step country year: gen `fy1' = bracketavg * bracketpop
bysort step country year: gen `sum_fy1' = sum(`fy1')
bysort step country year: gen `sum_pop1' = sum(bracketpop)
bysort step country year: replace topavg = `sum_fy1' / `sum_pop1' ///
	if imputed == 1

*get general average
bysort step country year: gen `auxi1' = topavg if p == 0  
bysort step country year: egen `avg1' = total(`auxi1')

*get top shares 
qui replace topsh = topavg / `avg1' * (`sum_pop1' / 100000 ) if imputed == 1
	
*get bracket shares 
qui replace s = bracketavg / `avg1' * (bracketpop  / 100000) if imputed == 1
bysort step country year: egen `test1' = total(s)
drop if `test1' == 0 
sort step country year p

*global all_ctries "`ctries_norm' `ctries_exep'"
qui levelsof country, local(all_ctries) clean 
foreach x in "data" "extrap" "interp" {
	qui gen `x'_points = ""
}

*tag data points, extrapolations and interpolations
// A concept can have different observed years; never pool its coverage tags.
foreach concept in `steps' {
	foreach c in `all_ctries' {
		quietly levelsof year if country == "`c'" & step == "`concept'" & missing(extrap), local(data_years) clean separate(,)
		if "`data_years'" != "" {
			quietly replace data_points = "[`data_years']" if country == "`c'" & step == "`concept'"
			quietly summarize year if country == "`c'" & step == "`concept'" & missing(extrap)
			local data_min = r(min)
			local data_max = r(max)
			quietly count if country == "`c'" & step == "`concept'" & !missing(extrap)
			if r(N) > 0 {
				local retro ""
				if `data_min' > $first_y local retro "[$first_y, `data_min'] ; "
				quietly replace extrap_points = "`retro'[`data_max', `ly']" if country == "`c'" & step == "`concept'"
			}
			quietly levelsof year if country == "`c'" & step == "`concept'" & imputed == 1 & missing(extrap), local(interp_years) clean separate(,)
			if "`interp_years'" != "" quietly replace interp_points = "[`interp_years']" if country == "`c'" & step == "`concept'"
			display as result "`c' `concept': observed [`data_years']; interpolated [`interp_years']"
		}
	}
}

*harmonise country names  
qui kountry country, from(iso3c) to(iso2c) 
qui rename _ISO2C_ iso

*clean dataset 
qui keep step country year p bracketavg bracketavg_norm bracketpop s thr ///
	topsh topavg *_points iso `dina_finished_fields'
qui order step country year p bracketavg bracketavg_norm bracketpop s thr ///
	topsh topavg *_points iso
* save for later 
tempfile top bottom bottomavg main 
qui save `main' 

bysort step country year: egen counter = count(p)
assert counter == 127 

*III. Compute regional averages (yearly) ---------------------------------------

*keep square panel 
drop if (country == "CRI")
drop if !inrange(year, 2000, `ly') 

*list years missing 
forvalue x = 2000/`ly' {
	if `x' == 2000 local years_missing "`x'"
	else local years_missing "`years_missing',`x'"
}
local years_missing "[`years_missing']"

*loop over groups
foreach s in `steps' {
	di as result "`s'"
	foreach x in "all" "CUB" {
		di as result "`x'"
		preserve
			qui keep if step == "`s'" 
			if "`x'" == "CUB" drop if !inlist(country, "ECU", "URY", "ARG")
            // Carry the actual donor pool's supplied-source attribution.
            if `dina_finished_count' > 0 {
                    tempfile dina_donor_input
                    quietly save `dina_donor_input'
                    keep year step pub_source pub_citation pub_doi
                    drop if pub_source == ""
                    duplicates drop
                    sort year step pub_source
                    by year step: gen strL pub_donor_note = pub_source + ": " + pub_citation + " " + pub_doi
                    by year step: replace pub_donor_note = pub_donor_note[_n-1] + "; " + pub_donor_note if _n > 1
                    by year step: keep if _n == _N
                    keep year step pub_donor_note
                    tempfile dina_donor_notes
                    quietly save `dina_donor_notes'
                    quietly use `dina_donor_input', clear
            }
            local dina_expected ""
            if "`x'" == "CUB" local dina_expected "ECU URY ARG"
            else {
                foreach donor in $all_countries {
                    if "`donor'" != "CRI" local dina_expected "`dina_expected' `donor'"
                }
            }
            levelsof country if !missing(bracketavg_norm), local(dina_present) clean
            local dina_missing : list dina_expected - dina_present
            local dina_availability ""
            foreach donor in `dina_expected' {
                tempvar dina_available_`donor'
                quietly gen byte `dina_available_`donor'' = country == "`donor'" & !missing(bracketavg_norm)
                local dina_availability "`dina_availability' `dina_available_`donor''"
            }
			*collapse normalized bracket averages 
			tempvar check2 fy2 sum_fy2 sum_pop2 aux2 avg2  
			qui collapse (mean) bracketavg = bracketavg_norm ///
				thr bracketavg_norm ///
				(count) `check2' = bracketavg_norm ///
                (sum) `dina_availability' ///
				(firstnm) bracketpop step, by(year p)
			tab `check2'
			qui gen check2 = `check2'
            local dina_expected_count = cond("`x'" == "all", 10, 3)
            capture assert `check2' == `dina_expected_count'
            if _rc {
                di as error "Incomplete `x' donor pool for `s': expected `dina_expected_count' countries."
                di as error "Expected donors: `dina_expected'"
                di as error "Present donors: `dina_present'"
                di as error "Entirely missing donors: `dina_missing'"
                keep if `check2' != `dina_expected_count'
                generate str244 missing_countries = ""
                foreach donor in `dina_expected' {
                    quietly replace missing_countries = trim(missing_countries + " `donor'") if `dina_available_`donor'' == 0
                }
                di as error "First affected year/fractile rows (check2 is donor count):"
                local dina_show = min(_N, 20)
                list year p check2 missing_countries in 1/`dina_show', noobs
                exit 9
            }
            quietly drop `dina_availability'
            if `dina_finished_count' > 0 {
                quietly merge m:1 year step using `dina_donor_notes', keep(master match) nogen
            }

			*compute top averages
			qui gsort year -p	
			bysort year: gen `fy2' = bracketavg * bracketpop
			bysort year: gen `sum_fy2' = sum(`fy2')
			bysort year: gen `sum_pop2' = sum(bracketpop)
			bysort year: gen topavg = `sum_fy2' / `sum_pop2'

			*get general average
			bysort year: gen `aux2' = topavg if p == 0  
			bysort year: egen `avg2' = total(`aux2')

			*get top shares 
			qui gen topsh = topavg / `avg2' * (`sum_pop2' / 100000 )
				
			*get bracket shares 
			qui gen s = bracketavg / `avg2' * (bracketpop  / 100000)
			
			*save for later
			tempfile tf_reg_avg_`x'_`s' 
			qui save `tf_reg_avg_`x'_`s'' 
		restore
	}
}

*merge with population data 
preserve 
	qui use "input_data/wid/population_total_adult_npopul.dta", clear
	qui kountry country, from(other) stuck marker
	qui rename (_ISO3N_ country) (iso3n country_long)
	qui kountry iso3n, from(iso3n) to(iso3c) 
	qui rename _ISO3C_ country 
	qui kountry iso3n, from(iso3n) to(iso2c) 
	qui rename _ISO2C_ iso 
	qui keep country* iso year totalpop adultpop 
	qui replace country = "VEN" if strpos(country_long, "Venezuela")
	qui replace country = "BOL" if strpos(country_long, "Bolivia")
	drop if missing(country) | year < 2002
	tempfile tf_pop 
	qui save `tf_pop'
restore 
tempvar merge 
qui merge m:1 country year using `tf_pop', gen(`merge')
sort step country year p

*drop isos that are not actual countries (or to exclude from aggregate)
qui drop if inlist(country, "VIR", "ATG", "ABW", "BRB", "GLP", "GRD") 
qui drop if inlist(country, "GUF", "LCA", "MTQ", "PRI", "VCT", "CRI") 

*drop countries already present in main 
foreach c in `all_ctries' {
	qui drop if country == "`c'"
}

*impute values to missing countries 
qui levelsof country, local(missing_ctries) clean 
tempfile imputed_ctries 
local iter = 1 
foreach c in `missing_ctries' {
	foreach s in `steps' {
		preserve 
			if "`c'" == "CUB" qui use `tf_reg_avg_`c'_`s'', clear 
			else qui use `tf_reg_avg_all_`s'', clear 
			qui gen country = "`c'"
			qui gen data_points = "`years_missing'"
			qui gen data_quality = 0 
			if `iter' != 1 append using `imputed_ctries'
			if `iter' == 1 local iter = 0 
			qui save `imputed_ctries', replace 
		restore 
	}
	drop if country == "`c'"
}

*put everyting together 
qui use `main', clear 
qui append using `imputed_ctries'

// Accepted contributions already passed validation. Keep them out of the
// legacy +0.1% adjustments and derived-statistics reconstruction below.
if `dina_finished_count' > 0 {
	preserve
		keep if finished_dina == 1
		tempfile dina_finished_main
		quietly save `dina_finished_main'
	restore
	drop if finished_dina == 1
	drop finished_dina bottomsh bottomavg
	// Preserve the original calculation precision for internal and donor rows.
	foreach v of local dina_legacy_floatvars {
		quietly recast float `v', force
	}
}

//check income variables are non-decreasing 
local incomevars thr bracketavg_norm topavg 
foreach v in `incomevars' {
	di as result "checking `v' non-decreasing..."
	qui sort step country year p
	bysort step country year: gen `v'_check = (`v' - `v'[_n-1]) / `v'[_n-1] * 100
	qui replace `v'_check = 0 if missing(`v'_check) & p == 0 | `v'[_n-1] < 0
	assert `v'_check >= -0.01  
	qui drop `v'_check 
}

// Preserve equal incomes (point masses); repair actual decreases only.
foreach v in `incomevars' {
	*make room 
	sort step country year p
	foreach nv in `v'_adj lag_`v' check_`v' {
		cap drop `nv'
	}
	*spot an actual decrease
	qui gen `v'_adj = `v'
	qui gen lag_`v' = `v'_adj[_n - 1] if p > 0
	qui gen check_`v' = 1 if (lag_`v' > `v'_adj & `v'_adj != 0)
	qui count if check_`v' == 1 
	*retain the existing 0.1% repair for decreases, one by one
	local iter_`v' = 1 
	while `r(N)' > 0 & `iter_`v'' <= 50 {
		if `iter_`v'' == 1 {
			di as result " Repairing decreases in `v' ..." _continue
			cap drop lag_`v'
			bysort step country year: gen lag_`v' = `v'_adj[_n - 1] if p > 0
		} 
		di as text ", `iter_`v''" _continue
		qui {
			cap drop sumcheck_`v' 
			bysort step country year: gen sumcheck_`v' = sum(check_`v') if check_`v' == 1
			*change only first-in-line value (+0.1%)
			bysort step country year: replace `v'_adj = ///
				lag_`v' * 1.001 if sumcheck_`v' == 1
			cap drop check_`v'  
			*update lag var and re-estimate 
			cap drop lag_`v' 
			bysort step country year: gen lag_`v' = `v'_adj[_n - 1] if p > 0
			bysort step country year: gen check_`v' = 1 if `v'_adj < lag_`v' & ///
				`v'_adj != 0 & !missing(`v'_adj, lag_`v')
			*add one to counter 	
			local iter_`v' = `iter_`v'' + 1
			qui count if check_`v' == 1
		}
	}
	if ("`v'" == "topavg"){
		qui replace `v'_adj = 1 if p == 0 & missing(`v'_adj)
	}
	*clean 
	cap drop check_`v' 
	cap drop sumcheck_`v' 
	cap drop lag_`v'
	qui replace `v' = `v'_adj if !missing(`v'_adj) 
	qui drop `v'_adj 
	di as text ", done."
}

//enforce  thr-avg consistency 
qui replace thr = bracketavg_norm if bracketavg_norm < thr
qui drop check2 	

//check income variables are non-decreasing (again)
foreach v in thr bracketavg topavg {
	di as result "checking `v' non-decreasing..."
	qui sort step country year p
	bysort step country year: gen `v'_check = (`v' - `v'[_n-1]) / `v'[_n-1] * 100
	qui replace `v'_check = 0 if missing(`v'_check) & p == 0 | `v'[_n-1] < 0
	assert `v'_check >= -0.01  
	qui drop `v'_check 
}

// Check nondecreasing order; equality is valid for a population mass.
di as result "final nondecreasing-order check: "
foreach v in `incomevars' {
	di as result " `v': " _continue
	*spot an actual decrease
	qui gen lag_`v' = `v'[_n - 1] if p > 0
	qui gen check_`v' = 1 if (lag_`v' > `v' & `v' != 0 & !missing(lag_`v'))
	assert check_`v' != 1 
	di as text "done."
	qui drop lag_`v' 
	qui drop check_`v'
}

*renormalize bracketavg
qui gen a1 = bracketavg_norm * bracketpop 
bysort step country year: egen a2 = total(a1)
qui replace a2 = a2 / 100000
qui replace a2 = 1 / a2 
qui replace bracketavg_norm = bracketavg_norm * a2 
qui replace thr = thr * a2 
qui drop a1 a2 

*recompute s 
qui gen s2 = bracketavg_norm * (bracketpop / 100000)
bysort step country year: egen sums = total(s2)
qui replace sums = round(1/sums * 10^5)
assert sums == 100000
qui replace s = s2 
qui drop s2 sums 

*recompute topavg 
qui gsort step country year -p 
qui gen fy = bracketavg_norm * bracketpop 
bysort step country year: gen bp = sum(bracketpop)
bysort step country year: gen sum_fy = sum(fy)
qui gen topavg2 = sum_fy / bp
qui replace topavg2 = 1 if p == 0
qui replace topavg = topavg2
qui drop topavg2 sum_fy fy bp 

*recompute topsh 
bysort step country year: gen topsh2 = sum(s)
qui replace topsh2 = 1 if bracketavg == 0
qui replace topsh = topsh2 
qui drop topsh2

*raplace bracketavg
qui replace bracketavg = bracketavg_norm 

*compute bottom shares
// p is the lower boundary: the bottom aggregate excludes the current bin.
qui sort step country year p
by step country year: gen double bottomsh = sum(s) - s

*compute bottom averages
tempvar fy3 sum_fy3 sum_pop3 
qui sort step country year p
by step country year: gen double `fy3' = bracketavg * bracketpop
by step country year: gen double `sum_fy3' = sum(`fy3') - `fy3'
by step country year: gen double `sum_pop3' = sum(bracketpop) - bracketpop
by step country year: gen double bottomavg = `sum_fy3' / `sum_pop3'
qui replace bottomavg = 0 if p == 0

// Restore exactly the validated normalized distribution before WID scaling.
local dina_scale_floatvars ""
foreach v in bracketavg thr topavg {
	local dina_storage : type `v'
	if "`dina_storage'" == "float" local dina_scale_floatvars "`dina_scale_floatvars' `v'"
}
if `dina_finished_count' > 0 {
	quietly append using `dina_finished_main'
	drop finished_dina
}

*harmonise country names
qui kountry country, from(iso3c) to(iso2c) 
qui replace iso = _ISO2C_ if missing(iso)
qui drop _ISO2C_ 

*list iso codes 
qui levelsof iso, local(iso2_codes) clean
*qui drop if `merge' == 2 

*merge with national incomes from wid
preserve 
	qui use "input_data/wid/export_scaling_anninc_xrates.dta", clear
	qui levelsof iso, local(c_wid) clean 
	foreach x in "ppp" "mer" {
		qui gen `x'_eur_`ly' = . 
		foreach c in `c_wid' {
			qui sum `x'_eur if iso == "`c'" & year == `ly'
			qui replace `x'_eur_`ly' = r(mean) if iso == "`c'"
		}
	}
	tempfile tf_ni_wid 
	qui save `tf_ni_wid'
restore  

*scale to national income level
qui merge m:1 iso year using `tf_ni_wid'
qui drop if year < 2002 & missing(country)
qui sort step country year p
local dina_income_scale "anninc992i"
local dina_has_pod : list posof "pod" in steps
if `dina_has_pod' > 0 {
	// Keep the disposable aggregate relative to national income: normalization
	// concerns distribution shape, not an instruction to equate the two totals.
	tempfile dina_income_targets dina_income_ratios
	preserve
		keep if !missing(p)
		keep country year step
		duplicates drop
		export delimited using "`dina_income_targets'", replace
	restore
	shell Rscript code/R/export-income-means.R "output/synthetic_microfiles/_smicrofile_long_grouped.csv" "`dina_income_targets'" "`comparison_request'" "`dina_income_ratios'" "`dina_income_observed'"
	confirm file "`dina_income_ratios'"
	merge m:1 country year step using "`dina_income_ratios'", keep(master match) nogen
	assert !missing(income_mean_ratio) & income_mean_ratio > 0 if !missing(p)
	generate double income_mean = anninc992i * income_mean_ratio
	local dina_income_scale "income_mean"
}
foreach var in "bracketavg" "thr" "topavg" "bottomavg" {
	qui replace `var' = `var' * `dina_income_scale'
}
if `dina_finished_count' > 0 {
	// Appending doubles must not change rounding of previously float columns.
	foreach v of local dina_scale_floatvars {
		quietly replace `v' = float(`v') if pub_source == ""
	}
}

//create main folders 
local dirpath "${dina_output_root}/output/latest_wid_series"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

*prepare data 
qui drop if missing(p)
if `dina_finished_count' > 0 {
	preserve
		keep country year step pub_*
		duplicates drop
		isid country year step
		tempfile dina_final_attribution
		quietly save `dina_final_attribution'
	restore
}
qui replace p = p / 1000
if `dina_finished_count' > 0 & "`dina_legacy_p_type'" == "float" {
	quietly replace p = float(p) if pub_source == ""
}
qui save `main', replace 
qui save "${dina_output_root}/output/latest_wid_series/dina_latam_wide_`date'.dta", replace

//check if there are missing p at this step 
qui keep step year country p bracketavg s thr ///
	data_points extrap_points interp_points 
bys step year country (p) : gen p2 = p[_n+1]
qui replace p2 = 100 if p2 == .
qui gen perc = "p"+string(p)+"p"+string(p2)
qui drop p p2
qui rename perc p

// Reshape within each income concept so concept-specific coverage survives.
qui renvars bracketavg s thr, prefix(value)
qui greshape long value, i(step country year p) j(widcode) string
foreach concept in `steps' {
	local income_code = cond("`concept'" == "nat", "ptinc", cond("`concept'" == "pon", "diinc", "cainc"))
	qui replace widcode = "a`income_code'992j" if widcode == "bracketavg" & step == "`concept'"
	qui replace widcode = "s`income_code'992j" if widcode == "s" & step == "`concept'"
	qui replace widcode = "t`income_code'992j" if widcode == "thr" & step == "`concept'"
}

*Top
preserve
	use `main', clear
	keep step year country p topsh topavg data_points extrap_points
	gen perc = "p"+string(p)+"p100"
	drop p
	rename perc   p
	renvars topsh topavg, prefix(value)
	greshape long value, i(step country year p) j(widcode) string
	qui replace widcode = "sptinc992j" if widcode == "topsh" & step == "nat"
	qui replace widcode = "aptinc992j" if widcode == "topavg" & step == "nat"
	qui replace widcode = "sdiinc992j" if widcode == "topsh" & step == "pon"
	qui replace widcode = "adiinc992j" if widcode == "topavg" & step == "pon"
	qui replace widcode = "scainc992j" if widcode == "topsh" & step == "pod"
	qui replace widcode = "acainc992j" if widcode == "topavg" & step == "pod"
	tempfile top
	save `top'
restore

*Bottom
preserve
	qui use `main', clear
	local dina_pub_marker ""
	if `dina_finished_count' > 0 local dina_pub_marker "pub_source"
	qui keep ///
		step year country p bottomsh data_points extrap_points interp_points `dina_pub_marker'
	qui sort step year country p 	
	bys step year country (p) : gen p2 = p[_n+1]
	qui replace p2 = 100 if p2 == .
	qui gen perc = "p"+string(p)+"p"+string(p2)
	qui drop p p2
	qui rename perc    p
	qui keep if inlist(p, "p50p51", "p90p91", "p20p21")
	qui reshape wide bottomsh, i(step country year) j(p) string
	qui rename bottomshp50p51 valuep0p50
	qui rename bottomshp90p91 valuep0p90
	qui rename bottomshp20p21 valuep0p20
	qui bys step country year : gen double valuep50p90 = valuep0p90 - valuep0p50
	if `dina_finished_count' > 0 {
		drop pub_source
	}
	qui reshape long value, i(step country year) j(p) string
	qui gen widcode = "sptinc992j" if step == "nat"
	qui replace widcode = "sdiinc992j" if step == "pon"
	qui replace widcode = "scainc992j" if step == "pod"
	qui tempfile bottom
	save `bottom'	
restore

*Bottom avg 
preserve
	qui use `main', clear
	local dina_pub_marker ""
	if `dina_finished_count' > 0 local dina_pub_marker "pub_source"
	qui keep ///
		step year country p bottomavg data_points extrap_points interp_points `dina_pub_marker'
	qui sort step year country p 	
	bys step year country (p) : gen p2 = p[_n+1]
	qui replace p2 = 100 if p2 == .
	qui gen perc = "p"+string(p)+"p"+string(p2)
	qui drop p p2
	qui rename perc    p
	qui keep if (p == "p50p51" | p == "p90p91")
	qui reshape wide bottomavg, i(step country year) j(p) string
	qui rename bottomavgp50p51 valuep0p50
	qui rename bottomavgp90p91 valuep0p90
	bys step country year : gen double valuep50p90 = (0.9 * valuep0p90 - 0.5 * valuep0p50) / 0.4
	if `dina_finished_count' > 0 {
		drop pub_source
	}
	qui reshape long value, i(step country year) j(p) string
	qui gen  widcode = "aptinc992j" if step == "nat"
	qui replace  widcode = "adiinc992j" if step == "pon"
	qui replace widcode = "acainc992j" if step == "pod"
	tempfile bottomavg
	qui save `bottomavg'	
restore

append using `top'
append using `bottom'
append using `bottomavg'

sort country year p widcode
*duplicates tag country year p widcode, gen(dup)
duplicates drop country year p widcode, force
drop if year == .

*harmonise names 
qui kountry country, from(iso3c) to(iso2c)
qui rename _ISO2C_ iso

*tag current LCU and data quality here 
qui merge m:1 country year using `tf_ava', nogen
qui rename grade data_quality 
qui replace data_quality = 0 if missing(data_quality)
tab country data_quality 

qui gen comment = ///
	"Scaled to Net National inc. from WID.world (Constant LCU)" 
qui replace comment = "Disposable-income mean: internal disposable/national income ratio times WID national-income benchmark" if step == "pod"

if `dina_finished_count' > 0 {
	// Keep attribution keyed by the actual income concept.
	replace step = "nat" if strpos(widcode, "ptinc")
	replace step = "pon" if strpos(widcode, "diinc")
	replace step = "pod" if strpos(widcode, "cainc")
	quietly merge m:1 country year step using `dina_final_attribution', keep(master match) nogen
	// Source origin is separate from the internal survey/admin quality scale.
	replace data_quality = . if pub_source != ""
	replace data_quality = 1 if pub_origin == "carry_forward"
	replace comment = pub_citation + cond(pub_doi != "", " DOI: " + pub_doi, "") + ///
		"; source preparation: DINA consistency corrections and GPinter regridding; WID national-income scaling" if pub_source != ""
	replace comment = comment + "; prepared supplied observation" if pub_origin == "supplied"
	replace comment = comment + "; DINA continuation of prepared source (" + pub_adjustment + ")" if pub_origin == "continuation"
	replace comment = comment + "; carry-forward of last validated distribution (" + pub_adjustment + ")" if pub_origin == "carry_forward"
	replace comment = comment + "; proportional changes for p0-p50, p50-p90, p90-p99 and p99-p100; boundary correction preserves group incomes" ///
		if pub_source != "" & pub_adjustment == "group_join"
	replace comment = comment + "; regional donors include: " + pub_donor_note if pub_donor_note != ""
}

*clean and save in long format 
drop if missing(value)
qui drop step 

if `dina_finished_count' > 0 {
	qui drop if country == "DOM" & year < 2012 & pub_source == ""
}
else qui drop if country == "DOM" & year < 2012

*save 
preserve 
	qui keep if strpos(widcode, "ptinc")
	qui save "${dina_output_root}/output/latest_wid_series/dina_latam_`date'.dta" , replace
restore 

preserve 
	qui keep if strpos(widcode, "diinc")
	qui save "${dina_output_root}/output/latest_wid_series/dina_latam_`date'_amory.dta", replace
restore 

if `dina_has_pod' > 0 {
	preserve
		keep if strpos(widcode, "cainc")
		assert _N > 0
		isid country year p widcode
		save "${dina_output_root}/output/latest_wid_series/dina_latam_`date'_disposable.dta", replace
	restore
}

////////////////////////////////////////////////////////////////////////////////

//bring basic parameters for graphs
global aux_part " "graph_basics" "
qui do "code/Stata/auxiliar/aux_general.do"

*get date 

/*
*compare pretax postax 
// Baseline and updated lines use observed availability within the comparison period.
local ly = $export_last_y
graph twoway ///
	(line value year if widcode == "sdiinc992j", lcolor(blue)) ///
	(line value year if widcode == "sptinc992j", lcolor(red)) ///
	if p == "p90p100" & (data_quality != 0 & year <= `ly' ///
	/*| inlist(country, "BOL", "CUB")*/) ///
	, by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 10% share") ///
	$graph_scheme legend(label(1 "Post-tax") label(2 "Pretax")) ///
	xlab(2000(5)2025)
qui graph export "${dina_output_root}/output/figures/updates/update-`date'-posvspre_t10.pdf", replace

graph twoway ///
	(line value year if widcode == "sdiinc992j", lcolor(blue)) ///
	(line value year if widcode == "sptinc992j", lcolor(red)) ///
	if p == "p99p100" & (data_quality != 0 & year <= `ly' ///
	/*| inlist(country, "BOL", "CUB")*/) ///
	, by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 1% share") ///
	$graph_scheme legend(label(1 "Post-tax") label(2 "Pretax")) ///
	xlab(2000(5)2025)
qui graph export "${dina_output_root}/output/figures/updates/update-`date'-posvspre_t1.pdf", replace

graph twoway ///
	(line value year if widcode == "sdiinc992j", lcolor(blue)) ///
	(line value year if widcode == "sptinc992j", lcolor(red)) ///
	if p == "p0p50" & (data_quality != 0 & year <= `ly' ///
	/*| inlist(country, "BOL", "CUB")*/) ///
	, by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Bottom 50% share") ///
	$graph_scheme legend(label(1 "Post-tax") label(2 "Pretax")) ///
	xlab(2000(5)2025)
qui graph export "${dina_output_root}/output/figures/updates/update-`date'-posvspre_b50.pdf", replace
*/

*exclude some obs 
qui keep if strpos(widcode, "ptinc")

*compare with previous update 
qui rename (value data_points) (new_value new_data_points) 
qui merge 1:1 year iso p widcode using `tf_prev'
qui rename value old_value 

*include last year in graph 
cap drop var1 
qui gen var1 = . 
foreach c in $countries_tax {
	qui replace var1 = 1 if country == "`c'" & year == `ly'
}
qui gen diff_value = (new_value - old_value) / old_value * 100
*qui replace data_quality = 4 if country == "DOM"
	
//compare pretax 	
// Baseline and updated lines use observed availability within the comparison period.
local ly = $export_last_y

foreach xxx in "sptinc992j" /*"sdiinc992j"*/ {
	
	graph twoway (line new_value year if inrange(year, $first_y, `ly'), lcolor(red)) ///
		(line old_value year if inrange(year, $first_y, `ly'), lcolor(black*.5)) ///
		if p == "p99.99p100" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 0.01% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "${dina_output_root}/output/figures/updates/update-`date'-`xxx'-t001.pdf", replace
	
	graph twoway (line new_value year if inrange(year, $first_y, `ly'), lcolor(red)) ///
		(line old_value year if inrange(year, $first_y, `ly'), lcolor(black*.5)) ///
		if p == "p99.9p100" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 0.1% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "${dina_output_root}/output/figures/updates/update-`date'-`xxx'-t01.pdf", replace
	
	graph twoway (line new_value year if inrange(year, $first_y, `ly'), lcolor(red)) ///
		(line old_value year if inrange(year, $first_y, `ly'), lcolor(black*.5)) ///
		if p == "p99p100" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 1% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "${dina_output_root}/output/figures/updates/update-`date'-`xxx'-t1.pdf", replace

	graph twoway (line new_value year if inrange(year, $first_y, `ly'), lcolor(red)) ///
		(line old_value year if inrange(year, $first_y, `ly'), lcolor(black*.5)) ///
		if p == "p90p100" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 10% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "${dina_output_root}/output/figures/updates/update-`date'-`xxx'-t10.pdf", replace

	graph twoway (line new_value year if inrange(year, $first_y, `ly'), lcolor(red)) ///
		(line old_value year if inrange(year, $first_y, `ly'), lcolor(black*.5)) ///
		if p == "p50p90" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Middle 40% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "${dina_output_root}/output/figures/updates/update-`date'-`xxx'-m40.pdf", replace

	graph twoway (line new_value year if inrange(year, $first_y, `ly'), lcolor(red)) ///
		(line old_value year if inrange(year, $first_y, `ly'), lcolor(black*.5)) ///
		if p == "p0p50" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Bottom 50% share") ///
		ylabel(0(.1).3) ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "${dina_output_root}/output/figures/updates/update-`date'-`xxx'-b50.pdf", replace
	
}
preserve
	qui use "input_data/wid/export_comparison_sptinc_p0p20.dta", clear
	tempfile tf_wid 
	qui save `tf_wid'
restore 

qui drop _merge 
qui merge 1:1 country year widcode p using `tf_wid'

graph twoway (line new_value year if inrange(year, $first_y, `ly'), lcolor(red)) ///
		(line value_web year if inrange(year, $first_y, `ly'), lcolor(black*.5)) ///
		if p == "p0p20" & widcode == "sptinc992j" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Bottom 20% share") ///
		ylabel(0(.01).03) ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "${dina_output_root}/output/figures/updates/update-`date'-sptinc992j-b20.pdf", replace

// A failed or interrupted export never reaches publication of this metadata.
shell Rscript code/R/export-comparison-metadata.R complete `date' "`comparison_request'"
confirm file "${dina_output_root}/output/figures/updates/comparison-`date'.json"
