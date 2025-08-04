////////////////////////////////////////////////////////////////////////////////
//
// 						Title: NATIONAL INCOME COMPOSITION 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Graph national income composition, plus total income 
//			declared in surveys, before and after correction.
//
////////////////////////////////////////////////////////////////////////////////

clear all 

//get list of paths 
global aux_part " "preliminary" " 
do "code/Do-files/auxiliar/aux_general.do"

//steps 
local steps " "raw" "bfm_norep_pre" " //"urb"
local steps1 " "raw" " // "urb"

*language 
local lang $lang

//1. COLLECT DATA --------------------------------------------------------------

tempvar aux1 aux2 

//get the data 
local iter = 0
tempfile tf_merge
foreach step in `steps' {
	
	*short name 
	local step3L = substr("`step'", 1, 3)
	
	//Import files 
	qui import excel "${summary}snacompare_`step'_totpop.xlsx", ///
		sheet("ni_comp") firstrow clear
		
	*adjust data for old currencies (old surveys to new currency)
	*wid national income is already in current lcu (1a takes care of it)
	global aux_part  ""old_currencies"" 
	qui do "code/Do-files/auxiliar/aux_general.do"
	foreach curr in $old_currencies {
		*identify country 
		local coun = substr("`curr'", 1, 3)
		*exceptions 
		if !inlist("`coun'", "URY"){
			*replace variable
			foreach x in pre pos {
				qui replace svy_to_ni_wid_`x' = ///
					svy_to_ni_wid_`x' * ${xr_`curr'} ///
					if year < ${yr_`curr'} & country == "`coun'"
			}
		}
	}
	
	*Harmonize special case (ECU)
	preserve 
		qui import excel $wb_xrates, sheet("ECU") clear firstrow
		qui destring year, replace 
		qui levelsof year, local(xr_yrs_ecu) clean 	
		foreach z in `xr_yrs_ecu' {
			qui sum rate if year == `z' 
			local xr_ecu_`z' = r(mean)
		}
	restore 
	foreach z in `xr_yrs_ecu' {
		foreach x in pre pos {
			qui replace svy_to_ni_wid_`x' = ///
				svy_to_ni_wid_`x' / `xr_ecu_`z'' ///
				if year == `z' & country == "ECU" & year < 2000
		}
	}
	
	*rename 	
	qui rename svy_to_ni* `step3L'_svy_to_ni*
	
	//check duplicates 
	tempvar tag`step'
	qui duplicates tag, gen(`tag`step'')
	cap qui assert `tag`step'' == 0 
	if _rc != 0 {
		di as text "duplicates (step `step') were found for: " _continue 
		qui levelsof country if `tag`step'' == 1, local("list") clean
		di as text "`list'"
		qui duplicates drop	
	}
			
	//merge
	if `iter' == 1 qui mer 1:1 country year using `tf_merge' , nogen
	qui save `tf_merge', replace
	local iter = 1 
}

*bring declared totals from tax 
preserve 
	qui import excel using "${taxpath}declared_tot.xlsx", clear firstrow
	keep country year declared_tot declared_tot_raw declared_tot_wag
	qui drop if missing(country, year)
	tempfile declared_tot
	qui save `declared_tot'	
restore 

*merge 
qui mer 1:1 country year using `declared_tot', nogen 

qui rename declared_tot tax_declared_tot 
qui rename declared_tot_raw tax_declared_tot_raw
qui rename declared_tot_wag tax_declared_tot_wag

* top 1% admin data for Colombia
qui gen tax_declared_tot_raw_t1 = .
qui replace tax_declared_tot_raw_t1 = tax_declared_tot_raw if country == "COL" & inrange(year,2000,2010)
qui replace tax_declared_tot_raw = . if country == "COL" & inrange(year,2000,2010)

* distinguish top 10% and total income for Ecuador
qui gen tax_declared_tot_raw_t10 = .
qui replace tax_declared_tot_raw_t10 = tax_declared_tot_raw if country == "ECU" ///
	& inrange(year,2008,2011)
qui replace tax_declared_tot_raw = . if country == "ECU" ///
	& inrange(year,2008,2011)

*label
if "`lang'" == "eng" { ///
	qui label var tax_declared_tot "Fiscal income"
	qui label var tax_declared_tot_raw "Fiscal income"
	qui label var tax_declared_tot_raw_t1 "Fiscal inc (top 1%)"
	qui label var tax_declared_tot_raw_t10 "Fiscal inc (top 10%)"
	qui label var tax_declared_tot_wag "Admin wages"
}
if "`lang'" == "esp" {
	qui label var tax_declared_tot_raw "Datos admin."
	qui label var tax_declared_tot_raw_t1 "Datos admin. (top 1%)"
	qui label var tax_declared_tot_raw_t10 "Datos admin. (top 10%)"
	qui label var tax_declared_tot_wag "Datos admin. (salarios)"
}
*merge undata to recover missing national income values
preserve 
	qui use "${sna_folder}UNDATA-WID-Merged.dta", clear
	keep country year TOT_B5g_wid
	sort country year
	drop if year==year[_n-1]
	tempfile un_wid_data
	qui save `un_wid_data'
restore

qui mer 1:1 country year using `un_wid_data', nogen update

qui replace tax_declared_tot = (tax_declared_tot / TOT_B5g_wid) * 100
qui replace tax_declared_tot_raw = (tax_declared_tot_raw / TOT_B5g_wid) * 100
qui replace tax_declared_tot_raw_t1 = (tax_declared_tot_raw_t1 / TOT_B5g_wid) * 100
qui replace tax_declared_tot_raw_t10 = (tax_declared_tot_raw_t10 / TOT_B5g_wid) * 100
qui replace tax_declared_tot_wag = (tax_declared_tot_wag / TOT_B5g_wid) * 100

*Chile 
qui replace tax_declared_tot_raw = tax_declared_tot if country == "CHL" 


//2. PREPARE FOR GRAPH --------------------------------------------------------- 

//bring basic parameters for graphs
global aux_part " "graph_basics" "
qui do "code/Do-files/auxiliar/aux_general.do"

//stack areas (institutional sectors)
local ni_vars "bpi_hh bpi_corp bpi_gg" 
qui genstack `ni_vars', gen(fp_)

//stack areas (composition of household sector)
foreach v in "wag" "ben" "mix" "cap" "imp" {
	rename pre_`v'_nac hh_`v'
	local hh_vars "`hh_vars' hh_`v'" 
}
qui genstack `hh_vars', gen(fp_)

//tag full-info cases 
qui gen aux1 = round(bpi_gg + bpi_corp + bpi_hh)
qui gen aux2 = 1 if aux1 >= 99 & aux1 <= 101

*tag extrapolated sna 
tempvar extsna_aux
qui egen `extsna_aux' = rowtotal(extsna_bpi*)
qui gen extsna_row = 1 if `extsna_aux' > 0 
qui replace extsna_row = 0 if missing(extsna_row)

*labels (IS variables)
if "`lang'" == "eng" {
	qui label var fp_bpi_hh "Household sector" 
	qui label var fp_bpi_corp "Corporations"
	qui label var fp_bpi_gg "General government"
	foreach x in pre pos {
		//cap label var urb_svy_to_ni_wid_`x' "Urban survey inc"
		qui label var raw_svy_to_ni_wid_`x' "Survey income"
		qui label var bfm_svy_to_ni_wid_`x' "Adj survey inc"
	}
}
if "`lang'" == "esp" {
	qui label var fp_bpi_hh "Hogares" 
	qui label var fp_bpi_corp "Corporaciones"
	qui label var fp_bpi_gg "Gobierno general"
	foreach x in pre pos {
		//cap label var urb_svy_to_ni_wid_`x' "Areas urbanas"
		qui label var raw_svy_to_ni_wid_`x' "Encuesta original"
		qui label var bfm_svy_to_ni_wid_`x' "Encuesta ajustada"
	}
}

*labels (HH variables)
qui la var fp_hh_wag "Wages"
qui la var fp_hh_ben "Benefits"
qui la var fp_hh_mix "Mixed Income"
qui la var fp_hh_cap "Capital Income"
qui la var fp_hh_imp "Imputed Rents"

// impute missing country/years
global imput_vars bfm_svy_to_ni raw_svy_to_ni 
qui do "code/Do-files/auxiliar/aux_fill_aver.do"
			
//loop over countries 
qui levelsof country, local(countries) clean
foreach x in pre pos {
	foreach c in `countries' {
	
		//details
		local c2 = strlower("`c'")
		local c2 = "c_`c2'"
		
		//prepare lines for graph
		foreach step in `steps' {
			
			*short name 
			local step3L = substr("`step'", 1, 3)
			
			*define lines for graph 
			qui count if !missing(`step3L'_svy_to_ni_wid_`x') ///
				& country == "`c'"
			if ("`step3L'" == "urb") local col "white"
			if ("`step3L'" == "raw") local col "gray*0.5"
			if ("`step3L'" == "bfm") local col "black"
			if (r(N) > 0) {
				local `c'_lines_`x' ``c'_lines_`x'' ///
				(connected `step3L'_svy_to_ni_wid_`x' year ///
				if !missing(raw_svy_to_ni_wid_`x'), ///
				lcolor(black) msymbol(O) mfcolor(`col') mcolor(black))
			}
		}

		*check if empty raw survey
		qui count if !missing(raw_svy_to_ni_wid_`x') & country == "`c'"
		if (r(N) > 0) {
			local graph_raw_`c'_`x' = 1 
			*undata 
			local `c'_rlines_`x' ``c'_rlines_`x'' ///
			(connected raw_svy_to_ni_wid_`x' year if !missing(raw_svy_to_ni_wid_`x'), ///
			lcolor(black) msymbol(O) mfcolor(black) mcolor(black))
			/*if "`c'" == "ARG" {
				*argentinian exception 
				local `c'_rlines_`x' ``c'_rlines_`x'' ///
				(connected urb_svy_to_ni_wid_`x' year if ///
				!missing(urb_svy_to_ni_wid_`x'), ///
				lcolor(black) msymbol(O) mfcolor(white) mcolor(black))
			} */
		}
		
		*check if empty admin 
		qui count if !missing(tax_declared_tot_raw) & country == "`c'"
		if (r(N) > 0) {
			local graph_tax_`c'_`x' = 1 
			*undata 
			local `c'_tlines_`x' ``c'_tlines_`x'' ///
			(connected tax_declared_tot_raw year if !missing(tax_declared_tot_raw), ///
			lcolor(black) msymbol(t) mfcolor(black) mcolor(black))
		}
		
		*check if empty admin top1
		qui count if !missing(tax_declared_tot_raw_t1) & country == "`c'"
		if (r(N) > 0) {
			local graph_taxt1_`c'_`x' = 1 
			*undata 
			local `c'_t1lines_`x' ``c'_t1lines_`x'' ///
			(connected tax_declared_tot_raw_t1 year if ///
			!missing(tax_declared_tot_raw_t1), ///
			lcolor(black) msymbol(x) mfcolor(black) mcolor(black))
		}
		
		*check if empty admin top10
		qui count if !missing(tax_declared_tot_raw_t10) & country == "`c'"
		if (r(N) > 0) {
			local graph_taxt10_`c'_`x' = 1 
			*undata 
			local `c'_t10lines_`x' ``c'_t10lines_`x'' ///
			(connected tax_declared_tot_raw_t10 year if ///
			!missing(tax_declared_tot_raw_t10), ///
			lcolor(black) msymbol(x) mfcolor(black) mcolor(black) lpattern(dash))
		}
		
		*check if empty admin wage
		qui count if !missing(tax_declared_tot_wag) & country == "`c'"
		if (r(N) > 0) {
			local graph_taxw_`c'_`x' = 1 
			*undata 
			local `c'_wlines_`x' ``c'_wlines_`x'' ///
			(connected tax_declared_tot_wag year if !missing(tax_declared_tot_wag), ///
			lcolor(black) msymbol(x) mfcolor(black) mcolor(black) ///
			lpattern(dot))
		
		}
		
		//chose to display areas or not 
		qui count if aux2 == 1 & country == "`c'"
			if (r(N) > 0) {
				*lines for first graph (IS)
				local `c'_areas_`x' (area fp_bpi_gg year if aux2 == 1 ///
				& extsna_row == 0, lwidth(none) color(sand*0.8)) ///
				(area fp_bpi_corp year if aux2 == 1 & extsna_row == 0 ///
				, lwidth(none) color(maroon*0.8)) (area fp_bpi_hh year ///
				if aux2 == 1 & extsna_row == 0, lwidth(none) ///
				color(edkblue*0.4))
				*lines for second graph (HH)
				foreach v in "imp"  "cap" "mix" "ben" "wag" {
					local `c'_areas2 ``c'_areas2' (area fp_hh_`v' year if aux2 == 1, lwidth(none) color(${c_`v'}))
				}
			} 
			
		//graphs 
		if ("`graph_raw_`c'_`x''" == "1" | "`graph_tax_`c'_`x''" == "1" /// 
		| "`graph_tax1_`c'_`x''" == "1" | "`graph_taxw_`c'_`x''" == "1" ///
		| "`graph_tax10_`c'_`x''" == "1" /*| "`graph_bfm_`c''" == "1"*/) {
			
			if "`lang'" == "esp" local ytit "% del Ingreso Nacional Bruto"
			if "`lang'" == "eng" local ytit "% of Gross National Income"
			
			if "`c'" == "DOM" {
				local firsty = 2012
				local midy2 = 2 
				local addcond & year >= `firsty'
			}
			else {
				local firsty = $first_y
				local midy2 = 5
				local addcond 
			}
			
			*Institutional Sectors in NI
			graph twoway ``c'_areas_`x'' ``c'_lines_`x'' ///
				if country == "`c'" `addcond' ///
				, ylabel(0(20)100, $ylab_opts) ///
				ytitle("") xtitle("") ///
				xlabel(`firsty'(`midy2')2020, $xlab_opts) $graph_scheme
			//save 
			qui graph export "${figs_cov}ni_`x'_`c'.pdf", replace 
			
			*Only raw
			graph twoway ``c'_areas_`x'' ``c'_rlines_`x'' ``c'_tlines_`x'' ///
				``c'_t1lines_`x'' ``c'_t10lines_`x'' ``c'_wlines_`x'' ///
				if country == "`c'" `addcond' ///
				/*& inrange(year, 2000, 2017)*/, ///
				ylabel(0(20)100, $ylab_opts) ///
				ytitle("`ytit'") xtitle("") ///
				xlabel(`firsty'(`midy2')2020, $xlab_opts) $graph_scheme
			//save 
			qui graph export "${figs_cov}ni_`x'_`c'_raw.pdf", replace
			
			*Only raw since 2000
			graph twoway ``c'_areas_`x'' ``c'_rlines_`x'' ``c'_tlines_`x'' ///
				``c'_t1lines_`x'' ``c'_t10lines_`x'' ``c'_wlines_`x'' ///
				if country == "`c'" `addcond' ///
				& inrange(year, 2000, ${last_y}), ///
				ylabel(0(20)100, $ylab_opts) ///
				ytitle("`ytit'") xtitle("") ///
				xlabel(2000(5)2020, $xlab_opts) $graph_scheme
			//save 
			qui graph export "${figs_cov}ni_`x'_`c'_raw_2000s.pdf", replace
			qui graph save "${figs_cov}ni_`x'_`c'_raw_2000s.gph", replace
			/*
			*Composition of Household Sector Income 
			graph twoway ``c'_areas2' ``c'_lines_`x'' ///
				(connected bpi_hh year, lcolor(black) msymbol(O) ///
				mfcolor(white) mcolor(black)) ///
				if country == "`c'" & extsna_row == 0 ///
				, ylabel(0(20)100, $ylab_opts) ///
				ytitle("`ytit'") xtitle("") ///
				xlabel(${first_y}(5)2020, $xlab_opts) $graph_scheme
			//save 
			qui graph export "${figs_cov}hh_`c'.pdf", replace 
			*/
		}
	}
}

//export excel "${figs_cov}output.xlsx", sheet("coverage", modify) firstrow(variables)
	
