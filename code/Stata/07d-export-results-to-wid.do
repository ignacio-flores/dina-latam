*Export data to WID database 

//general settings
*ssc install gtools 
*ssc mipolate
*findit renvars 
clear all

//preliminary
global aux_part  ""preliminary"" 
do "code/Stata/auxiliar/aux_general.do"

//choose unit 
local unit esn
//choose step 
local steps nat pon
//choose last year 
local ly = 2024 

//compare with previous update? 
local prev_date 3Oct2024
global previous_update "previous_series/dina_latam_`prev_date'.dta"
qui use $previous_update, clear 
qui drop data_quality comment extrap_points interp_points
tempfile tf_prev
qui save `tf_prev'

local date "$S_DATE"
local date = subinstr("`date'", " ", "", .)

//create folders if necessary 
local dirpath "output/figures/updates"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

//I.------------------------------------------------------


qui import excel ///
	"output/data_reports/latam_availability.xlsx", clear firstrow
qui keep country year grade
tempfile tf_ava 
qui save `tf_ava'

//open longformat here and keep relevant info 
qui import delimited ///
	using "output/ineqstats/_gpinter_all.csv", clear 	
	
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

*normalize variables in lcu 
tempvar average 
bysort country year: egen `average' = min(topavg)
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
foreach c in `all_ctries' {
	//list data points
	qui levelsof year if country == "`c'" & missing(extrap), ///
		local(data_points_`c') clean separate(,)
	if ("`data_points_`c''" != "") {
		//tag them  
		qui replace data_points = "[`data_points_`c'']" if country == "`c'"
		//get most recent year for each country
		qui sum year if country == "`c'" & missing(extrap)
		local `c'_max_year = r(max)
		local `c'_min_year = r(min)
		*list extrapolated points 
		qui levelsof year if country == "`c'" & !missing(extrap), ///
			local(extrap_points_`c') clean separate(,)		
		if ("`extrap_points_`c''" != "") {
			local retrop_`c' ""
			if ``c'_min_year' != 2000 local retrop_`c' "[2000, ``c'_min_year'] ; "
			local extrap_points_`c' "``c'_max_year', `ly'"
			qui replace extrap_points = ///
				"`retrop_`c''[`extrap_points_`c'']" if country == "`c'"
		}
		*list interpolated years 
		qui levelsof year if country == "`c'" & imputed == 1 & ///
			missing(extrap), local(interp_points_`c') clean separate(,)
		if ("`interp_points_`c''" != "") {
			qui replace interp_points = "[`interp_points_`c'']" if country == "`c'"
		}	
		*report everything 
		di as result "`c' :" 
		di as text "data points :  `data_points_`c''"
		di as text "extrapolation: `extrap_points_`c''"		
		di as text "interpolated: `interp_points_`c''"		
	}	
}

*harmonise country names  
qui kountry country, from(iso3c) to(iso2c) 
qui rename _ISO2C_ iso

*clean dataset 
qui keep step country year p bracketavg bracketavg_norm bracketpop s thr ///
	topsh topavg *_points iso 
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
			*collapse normalized bracket averages 
			tempvar check2 fy2 sum_fy2 sum_pop2 aux2 avg2  
			qui collapse (mean) bracketavg = bracketavg_norm ///
				thr bracketavg_norm ///
				(count) `check2' = bracketavg_norm ///
				(firstnm) bracketpop step, by(year p)
			tab `check2'
			qui gen check2 = `check2'

			if "`x'" == "all" assert `check2' == 10
			if "`x'" == "CUB" assert `check2' == 3

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
	qui use "input_data/population/PopulationLatAm.dta", clear
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

//ensure income variables are strictly increasing 
foreach v in `incomevars' {
	*make room 
	sort step country year p
	foreach nv in `v'_adj lag_`v' check_`v' {
		cap drop `nv'
	}
	*spot monotonicity or worse 
	qui gen `v'_adj = `v'
	qui gen lag_`v' = `v'_adj[_n - 1] if p > 0
	qui gen check_`v' = 1 if (lag_`v' >= `v'_adj & `v'_adj != 0)
	qui count if check_`v' == 1 
	*loop over monotonic adding 0.1%, one by one 
	local iter_`v' = 1 
	while `r(N)' > 0 & `iter_`v'' <= 50 {
		if `iter_`v'' == 1 {
			di as result " Ensuring non-monotonicity in `v' ..." _continue
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
			bysort step country year: gen check_`v' = 1 if `v'_adj <= lag_`v' & ///
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

//check monotonic (again)
di as result "final non-monotonicity check: "
foreach v in `incomevars' {
	di as result " `v': " _continue
	*spot monotonicity or worse 
	qui gen lag_`v' = `v'[_n - 1] if p > 0
	qui gen check_`v' = 1 if (lag_`v' >= `v' & `v' != 0 & !missing(lag_`v'))
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
gsort step country year p
bysort step country year: gen bottomsh = sum(s) 

*compute bottom averages
tempvar fy3 sum_fy3 sum_pop3 
qui sort country year p	
bysort step country year: gen `fy3' = bracketavg * bracketpop
bysort step country year: gen `sum_fy3' = sum(`fy3')
bysort step country year: gen `sum_pop3' = sum(bracketpop)
bysort step country year: gen bottomavg = `sum_fy3' / `sum_pop3' 

*harmonise country names
qui kountry country, from(iso3c) to(iso2c) 
qui replace iso = _ISO2C_ if missing(iso)
qui drop _ISO2C_ 

*list iso codes 
qui levelsof iso, local(iso2_codes) clean
*qui drop if `merge' == 2 

*merge with national incomes from wid
preserve 
	wid, indicators(anninc xlceup xlceux) ///
		areas("`iso2_codes'") perc(p0p100) clear 
	keep country variable year value 
	reshape wide value, i(country year) j(variable) string
	qui rename value* *
	qui rename *999i *
	qui rename (country anninc xlceup xlceux) ///
		(iso nninc_lcu_constc ppp_eur mer_eur)
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
foreach var in "bracketavg" "thr" "topavg" {
	qui replace `var' = `var' * anninc992i
}

//create main folders 
local dirpath "output/latest_wid_series"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

*prepare data 
qui drop if missing(p)
qui replace p = p / 1000
qui save `main', replace 
qui save "output/latest_wid_series/dina_latam_wide_`date'.dta", replace

//check if there are missing p at this step 
qui keep step year country p bracketavg s thr ///
	data_points extrap_points interp_points 
bys step year country (p) : gen p2 = p[_n+1]
qui replace p2 = 100 if p2 == .
qui gen perc = "p"+string(p)+"p"+string(p2)
qui drop p p2
qui rename perc p

//rename 
qui reshape wide bracketavg s thr, i(country year p) j(step) string 
qui rename (bracketavgnat bracketavgpon snat spon thrnat thrpon) ///
	(aptinc992j adiinc992j sptinc992j sdiinc992j tptinc992j tdiinc992j)
qui renvars aptinc992j sptinc992j tptinc992j ///
	adiinc992j sdiinc992j tdiinc992j, prefix(value)
	
//put in long format 	
qui greshape long value, i(country year p) j(widcode) string

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
	tempfile top
	save `top'
restore

*Bottom
preserve
	qui use `main', clear
	qui keep ///
		step year country p bottomsh data_points extrap_points interp_points
	qui sort step year country p 	
	bys step year country (p) : gen p2 = p[_n+1]
	qui replace p2 = 100 if p2 == .
	qui gen perc = "p"+string(p)+"p"+string(p2)
	qui drop p p2
	qui rename perc    p
	qui keep if (p == "p50p51" | p == "p90p91")
	qui reshape wide bottomsh, i(step country year) j(p) string
	qui rename bottomshp50p51 valuep0p50
	qui rename bottomshp90p91 valuep0p90
	qui bys step country year : gen valuep50p90 = valuep0p90 - valuep0p50
	qui reshape long value, i(step country year) j(p) string
	qui gen widcode = "sptinc992j" if step == "nat"
	qui replace widcode = "sdiinc992j" if step == "pon"
	qui tempfile bottom
	save `bottom'	
restore

*Bottom avg 
preserve
	qui use `main', clear
	qui keep ///
		step year country p bottomavg data_points extrap_points interp_points 
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
	bys step country year : gen valuep50p90 = valuep0p90 - valuep0p50
	qui reshape long value, i(step country year) j(p) string
	qui gen  widcode = "aptinc992j" if step == "nat"
	qui replace  widcode = "adiinc992j" if step == "pon"
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

*clean and save in long format 
drop if missing(value)
qui drop step 

qui drop if country == "DOM" & year < 2012

*save 
preserve 
	qui keep if strpos(widcode, "ptinc")
	qui save "output/latest_wid_series/dina_latam_`date'.dta" , replace
restore 

preserve 
	qui keep if strpos(widcode, "diinc")
	qui save "output/latest_wid_series/dina_latam_`date'_amory.dta", replace
restore 

////////////////////////////////////////////////////////////////////////////////

//bring basic parameters for graphs
global aux_part " "graph_basics" "
qui do "code/Stata/auxiliar/aux_general.do"

*get date 
local date "$S_DATE"
local date = subinstr("`date'", " ", "", .)

*compare pretax postax 
local ly = 2022
graph twoway ///
	(line value year if widcode == "sdiinc992j", lcolor(blue)) ///
	(line value year if widcode == "sptinc992j", lcolor(red)) ///
	if p == "p90p100" & (data_quality != 0 & year <= `ly' ///
	/*| inlist(country, "BOL", "CUB")*/) ///
	, by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 10% share") ///
	$graph_scheme legend(label(1 "Post-tax") label(2 "Pretax")) ///
	xlab(2000(5)2025)
qui graph export "output/figures/updates/update-`date'-posvspre_t10.pdf", replace	

graph twoway ///
	(line value year if widcode == "sdiinc992j", lcolor(blue)) ///
	(line value year if widcode == "sptinc992j", lcolor(red)) ///
	if p == "p99p100" & (data_quality != 0 & year <= `ly' ///
	/*| inlist(country, "BOL", "CUB")*/) ///
	, by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 1% share") ///
	$graph_scheme legend(label(1 "Post-tax") label(2 "Pretax")) ///
	xlab(2000(5)2025)
qui graph export "output/figures/updates/update-`date'-posvspre_t1.pdf", replace	

graph twoway ///
	(line value year if widcode == "sdiinc992j", lcolor(blue)) ///
	(line value year if widcode == "sptinc992j", lcolor(red)) ///
	if p == "p0p50" & (data_quality != 0 & year <= `ly' ///
	/*| inlist(country, "BOL", "CUB")*/) ///
	, by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Bottom 50% share") ///
	$graph_scheme legend(label(1 "Post-tax") label(2 "Pretax")) ///
	xlab(2000(5)2025)
qui graph export "output/figures/updates/update-`date'-posvspre_b50.pdf", replace	


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
local ly = 2022

foreach xxx in "sptinc992j" /*"sdiinc992j"*/ {
	
	graph twoway (line new_value year, lcolor(red)) ///
		(line old_value year if year <= `ly', lcolor(black*.5)) ///
		if p == "p99.99p100" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 0.01% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "output/figures/updates/update-`date'-`xxx'-t001.pdf", replace 
	
	graph twoway (line new_value year, lcolor(red)) ///
		(line old_value year if year <= `ly', lcolor(black*.5)) ///
		if p == "p99.9p100" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 0.01% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "output/figures/updates/update-`date'-`xxx'-t01.pdf", replace 
	
	graph twoway (line new_value year, lcolor(red)) ///
		(line old_value year if year <= `ly', lcolor(black*.5)) ///
		if p == "p99p100" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 1% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "output/figures/updates/update-`date'-`xxx'-t1.pdf", replace 

	graph twoway (line new_value year, lcolor(red)) ///
		(line old_value year if year <= `ly', lcolor(black*.5)) ///
		if p == "p90p100" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Top 10% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "output/figures/updates/update-`date'-`xxx'-t10.pdf", replace 

	graph twoway (line new_value year, lcolor(red)) ///
		(line old_value year if year <= `ly', lcolor(black*.5)) ///
		if p == "p50p90" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Middle 40% share") ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "output/figures/updates/update-`date'-`xxx'-m40.pdf", replace 

	graph twoway (line new_value year, lcolor(red)) ///
		(line old_value year if year <= `ly', lcolor(black*.5)) ///
		if p == "p0p50" & widcode == "`xxx'" ///
		& (data_quality != 0 /*| inlist(country, "BOL", "CUB")*/) , ///
		by(country, note("")) /*xline(2020)*/ xtitle("") ytit("Bottom 50% share") ///
		ylabel(0(.1).3) ///
		$graph_scheme legend(label(1 "Updated") label(2 "Old"))
	qui graph export "output/figures/updates/update-`date'-`xxx'-b50.pdf", replace 
}


