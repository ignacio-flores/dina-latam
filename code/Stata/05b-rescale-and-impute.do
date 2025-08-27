////////////////////////////////////////////////////////////////////////////////
//
// 							Title: RESCALE AND IMPUTE 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
////////////////////////////////////////////////////////////////////////////////

clear all

//0. General settings ----------------------------------------------------------
local ext "_norep"

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Stata/auxiliar/aux_general.do"
local lang $lang 

//define macros 
if "${bfm_replace}" == "yes" local ext ""
if "${bfm_replace}" == "no" local ext "_norep"

// 1. Get scaling factors-------------------------------------------------------
display as result "Fetching scaling factors..."

//Import sheet
local pop "adults"
qui import excel "output/snacompare_summary/snacompare_bfm`ext'_pre_`pop'.xlsx", ///
	sheet("scal_sna") firstrow clear	

//estimate 'left over' income
qui gen lef_nac = . 
foreach x in norm exep {
	foreach v in ${ind_raw_`x'} {
		local s = substr("`v'", 5, 7)
		if "`s'" == "pre_pen" local s "pre_ben"
		local naclist_`x' `naclist_`x'' `s'_nac
	}
	qui gen extrap_`x' = 0 
	foreach v in ${ind_raw_`x'} {
		local s = substr("`v'", 5, 7)
		if "`s'" == "pre_pen" local s "pre_ben"
		qui replace extrap_`x' = 1 if extsna_`s' == 1
	}
	qui egen taux_`x' = rowtotal(`naclist_`x'') if extrap_`x' != 1 
	if "`x'" == "exep" qui replace lef_nac = taux_`x' if exception == 1 
	if "`x'" == "norm" qui replace lef_nac = taux_`x' if exception != 1 
}

qui replace lef_nac = (100 - lef_nac) / 100	

//fill gaps...
//impute missing country/years
global imput_vars lef_nac taux_exep 
qui do "code/Stata/auxiliar/aux_fill_aver.do"
	
//stock scaling factors in memory
local nc = 1 	
foreach var in $scal_list {
	//From ratio to scaling factor 
	qui gen `var'_scal = 1 / (`var' / 100)
	qui levelsof country if !missing(`var'), local(ctries) 
	foreach c in `ctries' {
		if `nc' == 1 {
			qui levelsof year if country == "`c'" ///
				& !missing(`var'), local(`c'_yrs)	
			di as result "`var' " "`c' ``c'_yrs'"		
		}
		foreach y in ``c'_yrs' {
			*get national income 
			if `nc' == 1 {
				qui sum TOT_B5g_wid if country == "`c'" & year == `y'
				local ni_wid_`c'_`y' = r(sum)
			}
			*save scaling factor
			qui sum `var'_scal if country == "`c'" & year == `y' 
			if "`r(mean)'" != "" local scaling_`var'_`c'_`y' = `r(mean)'
			di as result "scaling_`var'_`c'_`y': `scaling_`var'_`c'_`y''"
			*save total for checking purposes 
			qui sum `var'_nac if country == "`c'" & year == `y'
			if "`r(mean)'" != "" local nac_`var'_`c'_`y' = `r(mean)'
			*also save total and leftover 
			if `nc' == 1 {
				qui sum taux_exep if country == "`c'" & year == `y'
				if "`r(mean)'" != "" local tnac_`c'_`y' = `r(mean)'
				qui sum lef_nac if country == "`c'" & year == `y' 
				if "`r(mean)'" != "" local lef1_`c'_`y' = `r(mean)'
				*di as result "`c' `y' `lef1_`c'_`y''"
			}
		}	
	}
	local nc = 0 
}

// 2. Get undistributed profits-------------------------------------------------
display as result "Fetching data on undistributed profits..."

//Import macro data 
tempfile wid_data_adj
qui use iso year uprofits_hh_ni bpi_corp_tot TOT_B5g_U ///
	sh_cfc_hh_surplus sh_cfc_hh_mixed sh_cfc_corp sh_cfc_hh sh_cfc_total ///
	series sh_bpi_corp_hh sh_bpi_corp_for sh_bpi_corp_gg ///
	NFC_B5g_cei FC_B5g_cei NFC_r_D4_cei FC_r_D4_cei GG_r_D4_cei ///
	TOT_r_D4_cei ROW_r_D4_cei TOT_B5g_cei using ///
	"intermediary_data/national_accounts/UNDATA-WID-Merged.dta", clear	

//standardize names 
qui kountry iso, from(iso2c) to(iso3c) 
qui rename _ISO3C_ country 
qui drop iso 

//Drop too little detail or empty, while better options are available
qui drop if (country == "BRA" & inrange(year,2000,2009) & series == 1000)
qui drop if (country == "ARG" & year >= 2000 & series == 1100)
*drop if (country  == "CHL" & year >= 2000 & series == 200)
*drop if (country  == "CHL" & inrange(year,2000,2009) & series == 1000)
qui drop if (country == "MEX" & inrange(year,2000,2002) & series == 300)

//Keep most recent series by year
qui egen cei_test = rowtotal(*_cei) 
qui drop if missing(series) & cei_test == 0 
qui drop cei_test
qui egen country_year = concat(country year)
qui levelsof country, local(all_ctries)
foreach c in `all_ctries' {
	qui levelsof country_year if country == "`c'", local(ctry_yrs_`c')
	foreach cy in `ctry_yrs_`c'' {
		qui sum series if country_year=="`cy'"
		qui drop if country_year=="`cy'" & series!=r(max)
	}		
}

//check duplicates 
qui duplicates tag country year, gen(dup)
qui assert dup == 0 
qui drop dup country_year series 
qui save "`wid_data_adj'", replace

//bring national income composition 
				 
qui import excel ///
	"output/snacompare_summary/snacompare_bfm`ext'_pre_`pop'.xlsx", ///
	sheet("ni_comp") firstrow clear	
qui la var svy_to_ni "Total income declared in corrected survey, % of GNI"	
qui la var bpi_gg "Bal. of 1ry inc., Gen. Gov. (% of G. National Income)"	
qui la var bpi_hh "Bal. of 1ry inc., Households (% of G. National Income)"	
qui la var bpi_corp "Bal. of 1ry inc., Corporate Sector (% of GNI)"	

//check duplicates 
tempvar tag
qui duplicates tag, gen(tag)
cap qui assert tag == 0 
if _rc != 0 {
	di as text "duplicates found for: " _continue 
	levelsof country if tag == 1, local(list) clean
	di as text "`list'"
	qui duplicates drop	
}
qui drop tag

//merge 
qui merge 1:1 country year using "`wid_data_adj'", keep(3 1) nogen

//update GNI data 
preserve 
	tempfile tf_gni 
	qui use iso year TOT_B5g_wid using ///
		"intermediary_data/national_accounts/UNDATA-WID-Merged.dta", clear
	duplicates drop 	
	qui drop if missing(TOT_B5g_wid)
	qui kountry iso, from(iso2c) to(iso3c) 
	qui rename _ISO3C_ country 
	qui drop iso 
	qui save `tf_gni', replace 		
restore
qui merge 1:1 country year using `tf_gni', update keep(3 4) nogen
sort country year

//use cei to estimate undistributed profits 
qui replace sh_bpi_corp_gg = GG_r_D4_cei / TOT_r_D4_cei if ///
	missing(sh_bpi_corp_gg) & !missing(TOT_r_D4_cei)
	
qui replace sh_bpi_corp_for = ROW_r_D4_cei / TOT_r_D4_cei if ///
	missing(sh_bpi_corp_for) & !missing(TOT_r_D4_cei)
	
qui replace bpi_corp_tot = NFC_B5g_cei + FC_B5g_cei if ///
	missing(bpi_corp_tot) & !missing(NFC_B5g_cei)
	
bysort country: mipolate sh_cfc_corp year if series == "CEI" ///
	, gen(ipo_cfc) epolate forward
qui replace sh_cfc_corp = ipo_cfc if missing(sh_cfc_corp) & !missing(ipo_cfc)
qui drop ipo_cfc

*uprofits in lcu (cei)
qui gen bpi_corp_tot_net = bpi_corp_tot * (1 - sh_cfc_corp) 
qui gen bpi_corp_hh_net = ///
	bpi_corp_tot_net * (1 - sh_bpi_corp_for - sh_bpi_corp_gg) if ///
	!missing(bpi_corp_tot_net)
qui replace sh_bpi_corp_hh = bpi_corp_hh_net / bpi_corp_tot	if ///
	missing(sh_bpi_corp_hh) & !missing(bpi_corp_hh_net)
assert sh_bpi_corp_hh <= 1 if !missing(sh_bpi_corp_hh)
qui replace uprofits_hh_ni = (bpi_corp_hh_net / TOT_B5g_cei) * 100 ///
	if missing(uprofits_hh_ni) & !missing(bpi_corp_hh_net)

//UProfits of households as % of tot inc. in survey
qui gen uprofits_hh_svy = uprofits_hh_ni / svy_to_ni
qui gen uprofits_hh_svy_pct = uprofits_hh_svy * 100
qui label var uprofits_hh_svy ///
	"Net national private corp. undis. profits, % of tot survey inc. (decimal)"
qui label var uprofits_hh_svy_pct ///
	"Net national private corp. undis. profits, % of tot survey inc. (x100)"

//UProfits of households as % national income
qui gen uprofits_hh_ni_pct = uprofits_hh_ni
qui label var uprofits_hh_ni_pct ///
	"National private corp. undistr. profits, % of national inc. (decimal)"

//Prepare variables for consumption of fixed capital and stock in memory
qui gen cfc_corp = sh_cfc_corp * bpi_corp_tot
qui la var cfc_corp "Consumption of fixed capital of corps., LCU"
foreach var in ///
	"sh_cfc_hh_surplus" "sh_cfc_hh_mixed" "sh_cfc_hh" "cfc_corp" {
	foreach c in `ctries' {
		foreach y in ``c'_yrs' {
			qui sum `var' if country == "`c'" & year == `y'
			local `var'_`c'_`y' = r(max)
		}
	}	
}

//Check structure of undistributed profits
foreach v in "sh_bpi_corp_hh" "sh_bpi_corp_for" ///
	"sh_bpi_corp_gg" "sh_cfc_corp" {
	qui gen `v'2 = `v' * 100
}

//create main folders 
local dirpath "output/figures/uprofits"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

//Graph all countries except ARG
graph twoway (line sh_bpi_corp_hh2 year, lcolor(black) lwidth(medthick)) ///
	(line sh_cfc_corp2 year, lcolor(${c_col}) lwidth(medium)) ///
	(line sh_bpi_corp_for2 year, lcolor(${c_bra}) lwidth(medium)) ///
	(line sh_bpi_corp_gg2 year, lcolor(${c_chl}) lwidth(medium)) ///
	if !inlist(country, "ARG", "PRY", "BOL", "VEN", "GTM"), ///
	ylabel(, angle(0)) yline(100 0, lcolor(black) lpattern(dot)) ///
	by(country) ytitle("Share of Gross U. Profits (%)") ///
	legend(label(1 "Households (net)") label(2 "Depreciation") ///
	label(3 "Foreigners") label(4 "Gral. Gorvernment")) $graph_scheme 
qui graph export ///
	"output/figures/uprofits/Composition.pdf", replace 
		
//call basic graph settings 
global aux_part " "graph_basics" " 
qui do "code/Stata/auxiliar/aux_general.do"

//prepare main graph lines 
local iv = 1
foreach var in "uprofits_hh_ni_pct" "uprofits_hh_svy_pct" {
	qui levelsof country if !missing(`var') & ///
		!inlist(country, "BOL", "GTM", "HND", "NIC", "VEN"), ///
		local(ctries_`iv')
	if "`ctries_`iv''" != "" {
		local counter = 1 
		foreach c in `ctries_`iv'' {
			//details 
			local c2 = strlower("`c'")
			local c2 "c_`c2'"
			local c3 = strlower("`c'")
			//add line to graph 
			local addgraphlines_`iv' `addgraphlines_`iv'' ///
				(connected `var' year if country == "`c'", ///
				lcolor($`c2') lwidth(thick) msize(tiny) ///
				msymbol(o) mfcolor($`c2') mcolor($`c2')) ///
				/*(line `var' year if country == "`c'" & ///
				series == "CEI" & year != 2003, ///
				lcolor($`c2'*0.2) lwidth(medium))*/
			local legend_`iv' `legend_`iv'' ///
				label(`counter' "${lname_`c3'}")
			di as result "``legend_`iv'''"
			*add 1 to counter
			local counter = `counter' + 1
		}		
		if "`var'" == "uprofits_hh_ni_pct" {
			local ytitle "U. Prof., % of NI"
		} 
		if "`var'" == "uprofits_hh_svy_pct" {
			local ytitle "U. Prof., % of Survey"
		} 
	
		//graph and save 
		graph twoway `addgraphlines_`iv'' ///
			if !inlist(country, "ARG", "BOL", "VEN") ///
			, ylabel(0(5)30, $ylab_opts) ///
			xlabel(${first_y}(5)2020, $xlab_opts) ///
			xtitle("") ytitle("") $graph_scheme ///
			legend(off)
		qui graph export "output/figures/uprofits/`var'.pdf", replace
		
		//graph and save w/legend
		graph twoway `addgraphlines_`iv'' ///
			if !inlist(country, "ARG", "BOL", "VEN") ///
			, ylabel(0(5)30, $ylab_opts) ///
			xlabel(${first_y}(5)2020, $xlab_opts) ///
			xtitle("") ytitle("") $graph_scheme ///
			legend(`legend_`iv'')
		qui graph export "output/figures/uprofits/`var'_legend.pdf", replace

		//details 
		qui drop `var'
		local iv = `iv' + 1
	}
}

//dont take CHL cfc data into account 
qui replace sh_cfc_hh_surplus = . if country == "CHL"
qui replace sh_cfc_hh_mixed = . if country == "CHL"

//impute missing country/years
global imput_vars sh_bpi_corp_for sh_cfc_corp sh_cfc_hh_surplus /// 
	sh_cfc_hh_mixed sh_cfc_hh sh_cfc_total sh_bpi_corp_gg /// 
	sh_bpi_corp_hh uprofits_hh_ni uprofits_hh_svy sh_bpi_corp_hh2 /// 
	sh_bpi_corp_for2 sh_bpi_corp_gg2 sh_cfc_corp2
qui include "code/Stata/auxiliar/aux_fill_aver.do"

*convert wid totals to old currency 
*wid national income is already in current lcu (1a takes care of it)
global aux_part  ""old_currencies"" 
quietly do "code/Stata/auxiliar/aux_general.do"
foreach curr in $old_currencies {
	*identify country 
	local coun = substr("`curr'", 1, 3)
	*exceptions 
	if !inlist("`coun'", "URY"){
		*replace variable
		quietly replace TOT_B5g_wid = TOT_B5g_wid / ${xr_`curr'} ///
			if year < ${yr_`curr'} & country == "`coun'"
	}
}

*Harmonize special case (ECU)
preserve 
	quietly import excel "input_data/xrates_WB/wb-xrates.xls", ///
		sheet("ECU") clear firstrow
	qui destring year, replace 
	quietly levelsof year, local(xr_yrs_ecu) clean 	
	foreach z in `xr_yrs_ecu' {
		quietly sum rate if year == `z' 
		local xr_ecu_`z' = r(mean)
	}
restore 
foreach z in `xr_yrs_ecu' {
	qui replace TOT_B5g_wid = TOT_B5g_wid * `xr_ecu_`z'' ///
		if year == `z' & country == "ECU" & year < 2000
}					
	
//stock values in memory 	
foreach c in `ctries' {
	foreach y in ``c'_yrs' {
		//uprofits 
		qui sum uprofits_hh_svy if country == "`c'" & year == `y'
		local uprof_pct_`c'_`y' = r(max)
		//cfc variables and NI 
		foreach v in "sh_cfc_hh_surplus" "sh_cfc_hh_mix" ///
			"sh_cfc_hh" "TOT_B5g_U" "TOT_B5g_wid" "uprofits_hh_ni" {
			qui sum `v' if country == "`c'" & year == `y' 
			local `v'_`c'_`y' = r(max)
			*di as result "local `v' `c' `y' " ``v'_`c'_`y''
		}
	}
}	

// 3. Rescale and impute corrected surveys -------------------------------------
//loop over countries and years 
local z = 1 
global area " ${all_countries} "
foreach c in $area {
	forvalues y = ${first_y}/${last_y} {
		clear 
		
		if `z' == 1 {
			di as result "{hline 80}"
			display as result "Rescaling and imputing ..."
			di as result "{hline 80}"
			local z = 0 
		}
		
		*inform activity
		di as text "`c' `y': " _continue 
		
		//confirm corrected survey exists 
		local survey "intermediary_data/microdata/bfm`ext'_pre/`c'_`y'_bfm`ext'_pre.dta"
		capture confirm file "`survey'"
		
		//open it
		if _rc==0 /*& !inlist("`c'", "ARG")*/ {
			qui use "`survey'", clear
			capture drop *_sca
			capture drop __*
			
			*prepare list of exception countries 
			global ex_ct 
			local iter_exep = 1 
			foreach ct in ${exep_countries} {
				if `iter_exep' == 1 global ex_ct "`ct'"
				else global ex_ct "`ct'", "$ex_ct"
				local iter_exep = 0 
			}
			
			*choose list of variables adapted to each country (raw step only)
			foreach u in $unit_list {
				*chose list of incomes 
				if !inlist("`c'", "${ex_ct}") local clist_`u' "${`u'_raw_norm}" 
				if  inlist("`c'", "${ex_ct}") local clist_`u' "${`u'_raw_exep}"
				
				*list variables (add suffix if raw)
				foreach v in `clist_`u'' {
					local dec_`u'_`c'_`y' "`dec_`u'_`c'_`y'' `v'`suf'"
				}
				*di as result "dec_`u'_`c'_`y': `dec_`u'_`c'_`y''"
			}	
			
			
			*check existence of ind variables (others not created yet)
			foreach v in `dec_ind_`c'_`y'' {
				cap confirm variable `v', exact
				if _rc == 0 {
					sum `v', meanonly
					local m_`c'_`y'_`v' = r(mean) 
					if `m_`c'_`y'_`v'' == 0 {
						local misvar_`c'_`y' "`misvar_`c'_`y''`v'"
					} 
				}
				else di as text "  `v' not found"
			}
						
			//chose variables 
			local inc "${y_pretax_tot}"
			if ("`c'" == "BRA") local inc "${y_pretax_tot_bra}"
			if ("`c'" == "PER") local inc "${y_pretax_tot_per}"
			local weight_bfm "_weight" 
			local weight_raw "_fep" 
			local age "edad" 
			if ("`c'" == "ARG") local weight_raw "_fep"
			
			*work only if all variables exist
			if ("`misvar_`c'_`y''" == "" & ///
				!inlist("`TOT_B5g_wid_`c'_`y''", ".")) {	
				
				//rescale them
				foreach var in $scal_list {
					if ("`scaling_`var'_`c'_`y''" != "") {	
						cap drop ind_`var'_sca 
						qui gen ind_`var'_sca = 0 
 						qui replace ind_`var'_sca = ind_`var' * ///
							`scaling_`var'_`c'_`y'' * _factor if edad >= 20
						*di as result "scal `var': " `scaling_`var'_`c'_`y''
						qui sum ind_`var' [w=`weight_bfm']
						local agg_before = r(sum)

 						if "`var'" == "pre_mir" & inlist("`c'", "${ex_ct}") {
							cap drop ind_pre_imp_sca
							qui gen ind_pre_imp_sca = 0 
							qui replace ind_pre_imp_sca = ind_pre_imp * ///
							`scaling_`var'_`c'_`y'' * _factor if edad >= 20
							cap drop ind_pre_mix_sca
							qui gen ind_pre_mix_sca = 0 
							qui replace ind_pre_mix_sca = ind_pre_mix * ///
							`scaling_`var'_`c'_`y'' * _factor if edad >= 20
						}
						*check consistency of scaled aggregates 
						if ("`nac_`var'_`c'_`y''" != "") {
							qui sum ind_`var'_sca [w=`weight_bfm'] ///
								if edad >= 20, meanonly 
							local chktot_`var'_`c'_`y' = r(sum)
							local chkrat_`var'_`c'_`y' = ///
								`chktot_`var'_`c'_`y'' / ///
								(`ni_wid_`c'_`y'' * `nac_`var'_`c'_`y'' / 100)
							di as result "ratio `var' " ///
								`chkrat_`var'_`c'_`y'' * 100
							/*assert inrange(round( ///
								`chkrat_`var'_`c'_`y'' * 100), 97, 103)*/
						} 
						local scal_list_`c'_`y' /// 
							"`scal_list_`c'_`y'' `var'"		
					}
				}
					
				// Exclude social assistance transfers
				foreach var in "ind_pre_ass" "ind_pre_ass_sca " ///
					"ind_pre_pen_sca" "pre_pen_ratio" {
					cap drop `var'
					cap gen `var' = 0 
				}
				
				//split benefits into pension and non-pension
				//note: ind_pre_ben = ind_pre_pen + ind_pre_oth
				qui replace ind_pre_ass = ind_pre_oth if edad >= 20
				qui replace pre_pen_ratio = ind_pre_pen / ind_pre_ben 
				qui replace pre_pen_ratio = 0 if missing(pre_pen_ratio)
				*pensions 
				qui replace ind_pre_pen_sca = ///
					ind_pre_ben_sca * pre_pen_ratio if edad >= 20
				*other monetary benefits 
				qui gen ind_pre_mbe_sca = ///
					ind_pre_ben_sca * (1 - pre_pen_ratio) if edad >= 20
				
				*get total population
				qui sum `weight_raw' if `age' >= 20, meanonly 
				local adultpop= r(sum)
				qui sum `weight_raw' if `age' > 0, meanonly 
				local totpop = r(sum)
					
				*list total incomes for checks	
				global exception_ctries inlist("`c'", "BOL") //"ECU" "CHL"
				if ${exception_ctries} local lv ${ind_res_exep} ind_pre_mbe_sca
				else local lv ${ind_res_norm} ind_pre_mbe_sca
				
				*check rescaled values are equal to target 
				*note: this is only useful for extrapolated years 
				cap drop auxtot_res 
				qui egen auxtot_res = rowtotal(`lv')
				qui sum auxtot_res [w = `weight_bfm'], meanonly 
				local sum_aux_`c'_`y' = r(sum)
				local delt_`c'_`y' = ///
					(`tnac_`c'_`y'' / 100 * `TOT_B5g_wid_`c'_`y'') / `sum_aux_`c'_`y'' 
				
				/*
				di as result "listvar: " "`lv'"
				di as result "delt * 100:" `delt_`c'_`y'' * 100 
				di as result "tnac "  `tnac_`c'_`y''
				di as result "B5g " `TOT_B5g_wid_`c'_`y'' 
				di as result "sumaux " `sum_aux_`c'_`y'' 
				*/
				local whilecount = 0	
				cap assert round(`delt_`c'_`y'' * 100) == 100
				while _rc != 0 {	
					
					*avoid eternal loop 
					local whilecount = `whilecount' + 1
					if `whilecount' >= 10 {
						di as error "Target total income can't be reached"
						exit 6
					}
					
					*report adjustment 
					di as text "rescaled inc. not equal to target, " _continue 
					if `delt_`c'_`y'' > 1 di as text "upscaling " _continue
					if `delt_`c'_`y'' < 1 di as text "downscaling " _continue 
					di as text "incomes " _continue ///
						round((`delt_`c'_`y'' - 1) * 100, 0.01) " % "  
						
					*re-rescale each var independently	
					foreach var in `lv'  {
						qui replace `var' = `var' * `delt_`c'_`y''
					}
					
					*re-estimate rescaled total income 
					cap drop auxtot_res 
					qui egen auxtot_res = rowtotal(`lv')
					
					*check again
					qui sum auxtot_res [w = `weight_bfm'], meanonly 
					local delt_`c'_`y' = ///
						(`tnac_`c'_`y'' / 100 * `TOT_B5g_wid_`c'_`y'') / r(sum) 
					cap assert round(`delt_`c'_`y'' * 100) == 100
				}
				
				*report total household income after rescaling 
				qui sum auxtot_res [w=`weight_bfm'], meanonly 
				local newtot_`c'_`y' = r(sum) / `TOT_B5g_wid_`c'_`y'' * 100
				di as text "(household inc. set to: " _continue 
				di as text round(`newtot_`c'_`y'', 0.1) "% NI)"
				
				//4. Study Undistributed Profits ------------------------------
				
				//compute cumulative distributions (pretax income)
				sort `inc' _pid
				foreach w in raw bfm {
					foreach v in F freq ftile {
						cap drop `v'_`w'
						cap drop `v'_`w'_tot
						cap drop `v'_`w'_adu
					}
					qui	gen freq_`w'_tot  = `weight_`w'' / `totpop'
					qui	gen freq_`w'_adu  = `weight_`w'' / `adultpop' ///
						if `age' >= 20 
					qui	gen F_`w'_adu = sum(freq_`w'_tot )
					qui egen ftile_`w'_adu = cut(F_`w'_adu), at(0(0.05)0.949 ///
						0.95(0.04)0.981 0.99(0.01)1)
					qui replace ftile_`w'_adu = 0.99 if missing(ftile_`w'_adu)
				}
				
				*(we also need to impute % to other units [PENDING])
				if !inlist("`uprof_pct_`c'_`y''", "", ".") {
					
					//get LCU amount of undistributed profits 
					*qui sum `inc' [w = `weight_bfm'], meanonly
					*local up_lcu_`c'_`y' = r(sum) * `uprof_pct_`c'_`y''
					local up_lcu_`c'_`y' = ///
						`TOT_B5g_wid_`c'_`y'' * `uprofits_hh_ni_`c'_`y'' / 100
					
					//make room for capital ownership proxies
					forvalues n = 1/4 {
						cap drop ind_upr_`n'
						cap drop cap_own_`n' 
						cap drop aux_`n'
						cap drop uprofits_pct_`n'
						cap drop uprofits_pct_orig_`n'
						qui gen cap_own_`n' = 0 
						qui gen ind_upr_`n' = 0 	
					} 

					//define proxies
					qui replace ind_pre_imp = 0 if missing(ind_pre_imp)
					qui replace cap_own_1 = ind_pre_cap 
					qui replace cap_own_2 = `inc' if categ5_p == 1
					qui replace cap_own_2 = cap_own_2 + ind_pre_cap ///
						if categ5_p != 1
					qui replace cap_own_3 = `inc' if categ5_p == 1
					qui replace cap_own_4 = ind_pre_cap + ind_pre_imp
					
					//loop over proxies  
					forvalues n = 1/4 {
						
						//impute UP proportionally to proxies 
						qui sum cap_own_`n' [w = `weight_bfm'], meanonly 
						qui replace cap_own_`n' = cap_own_`n' / r(sum) 
						qui replace ind_upr_`n' = ///
							cap_own_`n' * `up_lcu_`c'_`y''
						qui la var ind_upr_`n' ///
							"Imp. of private und. profits based on proxy nº`n'"
						//study income composition in all cases	
						qui gen uprofits_pct_orig_`n' = ///
							ind_upr_`n' / `inc' * 100
						qui replace uprofits_pct_orig_`n' = 0 if `inc' == 0 
						//check consistency
						sum cap_own_`n' [w = `weight_bfm'], meanonly 
						
						//2nd check
						sort `inc' _pid
						cap drop check_`n'
						qui gen check_`n' = ///
							sum(cap_own_`n'[_n-1] * `weight_bfm'[_n-1])	
						qui replace cap_own_`n' = cap_own_`n' * 100 
					}
					
					//label proxies 
					qui la var ind_upr_1 "Imp. % to Divds. & withdrawals"
					qui la var ind_upr_2 "Imp. % to Employer's inc. + Divs."
					qui la var ind_upr_3 "Imp. % to Employer's inc."
					qui la var ind_upr_4 "Imp. % to IR + Emp's inc. + Divs."	
					
					*store 'leftover' income as % of NI (excl. uprofits)  
					local lef2_`c'_`y' = ///
						`lef1_`c'_`y'' - (`uprofits_hh_ni_`c'_`y'' / 100)
					
					/*
					local lefscal_`c'_`y' = ///
						`lef2_`c'_`y'' / (1 - `lef2_`c'_`y'')	
					di as result "uprofits " `uprofits_hh_ni_`c'_`y''	
					di as result "lefscal " `lefscal_`c'_`y''
					di as result "lef1 " `lef1_`c'_`y''
					di as result "lef2 " `lef2_`c'_`y''
					*/
					
					preserve 
						//auxiliary variables 
						qui gen own2_cum_aux = cap_own_2 * `weight_bfm'
						qui	gen own2_cum = sum(own2_cum_aux)
					
						//collapse 
						qui collapse (mean) uprofits_pct_orig_*  ///
							(sum) own2_cum_aux cap_own_*, by(ftile_bfm_adu)
						qui replace ftile_bfm_adu = ftile_bfm_adu * 100
											
						//label proxies 
						foreach var in "uprofits_pct_orig_" "cap_own_" {
							qui la var `var'1 "Divds. & withdrawals"
							qui la var `var'2 "Employer inc. + divds."
							qui la var `var'3 "Employer's inc."
							qui la var `var'4 "Imp Rents + nº2 "
			
						}
						
						//label vars and prepare graph lines
						forvalues n = 1/4 {
							if ("`n'" != "3") {
								//prepare lines for graph
								local graphlines_`c'_`y' ///
									`graphlines_`c'_`y'' ///
									(line uprofits_pct_`n' ftile_bfm_adu)	
								local graphlines2_`c'_`y' ///
									`graphlines2_`c'_`y'' ///
									(line uprofits_pct_orig_`n' ///
									ftile_bfm_adu if ftile_bfm_adu >= 50 ///
									| uprofits_pct_orig_`n' <= 200, ///
									lwidth(thick))	
							}					 	
						}
			
						//call graph parameters 
						global aux_part  ""graph_basics"" 
						qui do "code/Stata/auxiliar/aux_general.do"
						
						//create main folders 
						local dirpath "output/figures/uprofits/incidence"
						mata: st_numscalar("exists", direxists(st_local("dirpath")))
						if (scalar(exists) == 0) {
							mkdir "`dirpath'"
							display "Created directory: `dirpath'"
						}
						
						//create main folders 
						local dirpath "output/figures/uprofits/incidence-scal"
						mata: st_numscalar("exists", direxists(st_local("dirpath")))
						if (scalar(exists) == 0) {
							mkdir "`dirpath'"
							display "Created directory: `dirpath'"
						}

						//graph as incidence  	
						graph twoway (connected own2_cum_aux ftile_bfm_adu, ///
							mcolor(cranberry) lcolor(cranberry) ///
							mfcolor(white)), title("") ///
							yline(100, lcolor(red) lpattern(dot)) ///
							ytitle("") ///
							xtitle("") ///
							xlabel(0(10)100 , $xlab_opts) ///
							ylabel(0(20)100, $ylab_opts) ///
							$graph_scheme 
						qui graph export ///
							"output/figures/uprofits/incidence/`c'`y'.pdf", replace  
			
					restore 
				}
				
				//Deduct consumption of fixed capital to OS & MI
				if ("`sh_cfc_hh_surplus_`c'_`y''" != "") ///
					& !${exception_ctries} {
					qui gen yn_imp_sca = ///
						ind_pre_imp_sca * (1-`sh_cfc_hh_surplus_`c'_`y'')
					qui la var yn_imp_sca ///
						"Imputed Rents (scaled to net OS of HH)"
				} 
				if ("`sh_cfc_hh_mix_`c'_`y''" != "") ///
					& !${exception_ctries} {
					qui gen yn_mix_sca = ///
						ind_pre_mix_sca * (1-`sh_cfc_hh_mix_`c'_`y'')
					qui la var yn_mix_sca ///
						"Mixed Income (scaled to net Mixed Inc.)"
				} 
				if ("`sh_cfc_hh_`c'_`y''" != "") {
					qui gen yn_mir_sca = ind_pre_mir_sca * (1-`sh_cfc_hh_`c'_`y'')
					qui la var yn_mir_sca ///
						"MI + IR (scaled to corresponding in SNA)"
				} 
				local net_sca_vars "yn_imp_sca yn_mix_sca yn_mir_sca"
				
				// 5. Other defintions of income (units)-----------------------
				
				*create couple variables 
				cap drop spouse
				cap drop married
				qui gen spouse = (paren_ee <= 2)
				qui egen married = sum(spouse) if paren_ee<=2, by(id_hogar)
							
				*loop over variables 
				foreach v in $scal_list pre_pen pre_mbe {
					
					*skip exceptions 
					if ${exception_ctries} & ///
						inlist("`v'", "pre_imp", "pre_mix") {
						*don't do anything
					}
					
					*define income with other units than individual 
					else {
						foreach x in "svy" "sca" {
							if "`x'" == "svy" local sf ""
							if "`x'" == "sca" local sf "_sca"
							*compute sum of incomes by couple (narrow)
							cap drop esn_`v'`sf' 
							qui egen esn_`v'`sf'= sum(ind_`v'`sf' /married) ///
								if married <= 2, by(id_hogar)
							qui cap replace esn_`v'`sf' = ///
								ind_`v'`sf' if missing(married)
							qui cap la var esn_`v'`sf' ///
								"Eq-split narrow inc. `v'`sf'"
									
							*compute sum of incomes by adults (broad)
							cap drop esb_`v'`sf'
							qui egen esb_`v'`sf' = ///
								sum(ind_`v'`sf'/adults_house), by(id_hogar)
							qui la var esb_`v'`sf' ///
								"Eq-split broad inc. `v'`sf'"
							
							*compute sum of incomes by household 
							cap drop pch_`v'`sf'
							qui egen pch_`v'`sf' = ///
								sum(ind_`v'`sf' / hh_size), by(id_hogar)
							qui la var pch_`v'`sf' ///
								"Per capita hld. inc. `v'`sf'"
						}
					}
				}
				
				//Also add undistributed profits
				*narrow eq-split 
				cap drop esn_upr_2 
				qui egen esn_upr_2 = sum(ind_upr_2 /married) ///
					if married <= 2, by(id_hogar)
				qui cap replace esn_upr_2 = ind_upr_2 if missing(married)
				qui cap la var esn_upr2 "Eq-split narrow inc. uprofits 2"
				*broad eq-split
				cap drop esb_upr_2
				qui egen esb_upr_2 = ///
					sum(ind_upr_2/adults_house), by(id_hogar)
				qui la var esb_upr_2 "Eq-split broad inc. uprofits 2"
				*household per capita 
				cap drop pch_upr_2 
				qui egen pch_upr_2 = sum(ind_upr_2 / hh_size), by(id_hogar)
				qui la var pch_upr_2 "Per capita hld. uprofits 2"
				
				//impute the leftover proportionally to total incomes
				foreach u in $unit_list {
					
					if "`u'" != "act" {
						*decide which list of variables 
						if ${exception_ctries} local lv ${`u'_upr_exep}
						else local lv ${`u'_upr_norm} 
						
						*last definition of total income  
						cap drop auxtot_`u'
						qui egen auxtot_`u' = rowtotal(`lv')
						
						*generate factor income here 
						if ${exception_ctries} {
							local fac_vars `u'_pre_wag_sca ///
								`u'_pre_mir_sca `u'_upr_2 
						} 
						else{
							local fac_vars `u'_pre_wag_sca ///
								`u'_pre_mix_sca `u'_pre_imp_sca `u'_upr_2 
						} 
						cap drop `u'_pre_fac_sca 
						qui egen `u'_pre_fac_sca = rowtotal(`fac_vars')	
					}
				}
				
				//6. Study the incidence of scaling ---------------------------
				//call graph parameters 
				global aux_part  ""graph_basics"" 
				qui do "code/Stata/auxiliar/aux_general.do"
				
				*loop over units (pre-tax so far) 
				*to make it faster, we only graph the chosen unit 
				foreach u in $unit_list { 
					
					*gen corresponding total
					cap drop `u'_totaux
					qui egen `u'_totaux = rowtotal(`dec_`u'_`c'_`y'')
					
					*total or adult pop?
					if inlist("`u'", "pch") local pop "tot"
					else local pop "adu"
					
					*get cumulative distribution 
					local w "raw"
					qui sort `u'_totaux
					qui cap drop F_`w'_`u'
					qui	gen F_`w'_`u' = sum(freq_`w'_`pop')
					cap drop ftile_`w'_`u' 
					qui egen ftile_`w'_`u' = cut(F_`w'_`u'), ///
						at(0(0.02)0.981 0.99(0.01)1)
					*qui replace ftile_`w'_`u' = 0.99 if missing(ftile_`w'_`u')
					
					*prepare lines for graph 
					local i3n_`u'_`c'_`y' = 2
					*1st for total income 
					local totinc_l (line `u'_totaux_a ftile_`w'_`u', ///
						lcolor(black) lwidth(thin) lpattern(dash)) 
					local leg_i3_`u'_`c'_`y' label(1 "Equality line")
					qui cap drop `u'_totaux_a 
					foreach v in `u'_totaux `dec_`u'_`c'_`y'' {
						if "`u'" == "ind" local sh =  substr("`v'", 5, 3)
						else local sh =  substr("`v'", 9, 3)
						qui cap drop `v'_a
						qui sum `v' [w = `weight_raw'], meanonly
						qui gen `v'_a = (`v' / r(sum)) * `weight_raw' * 100
						qui replace `v'_a = sum(`v'_a)
						if !inlist("`v'", "`u'_totaux") {
							local il_`u'_`c'_`y' `il_`u'_`c'_`y'' ///
							(connected `v'_a ftile_`w'_`u', ///
							lcolor(${c_`sh'}) mcolor(${c_`sh'}) msize(tiny) ///
							lwidth(thick) mfcolor(${c_`sh'})) 
							local leg_i3_`u'_`c'_`y' `leg_i3_`u'_`c'_`y'' ///
								label(`i3n_`u'_`c'_`y'' "${lab_`sh'_`lang'}")
							local i3n_`u'_`c'_`y' = `i3n_`u'_`c'_`y'' + 1
						} 
					}		
					local leg_i3_`u'_`c'_`y' `leg_i3_`u'_`c'_`y'' ///
						label(`i3n_`u'_`c'_`y'' "Total inc.") 
			
					//collapse and graph 
					preserve  
						qui collapse (max) *_a, by(ftile_`w'_`u')
						qui replace ftile_`w'_`u' = ftile_`w'_`u' * 100 + 1
						
						//check consistency 
						foreach v in `dec_`u'_`c'_`y'' {
							cap drop `v'_t
							qui gen `v'_t = sum(`v'_a)
						}
							
						//graph as incidence of scaling  	
						graph twoway (function y=x, range(0 100) ///
							lcolor(black)) `il_`u'_`c'_`y'' ///
							`totinc_l', /*title("`c' - `y'")*/ ///
							ytitle("Cumulative income share") ///
							xtitle("Percentile") ///
							xlabel(0(10)100 , $xlab_opts) ///
							ylabel(0(20)100, $ylab_opts) ///
							$graph_scheme /*legend(`leg_i3_`u'_`c'_`y'') ///
							legend(ring(1) pos(3) col(1)) */ ///
							legend(off) ysize(5.5) xsize(5.5) ///
							aspectratio(1) 
						qui graph export ///
							"output/figures/uprofits/incidence-scal/`c'`y'_`u'.pdf", replace
					restore 
				}
			
				*save 
				qui save "`survey'", replace
				
				//report scaled variables 
				*display as text "`c' `y' scaled variables: " _continue
				global checkvars ///
					"pre_wag pre_ben pre_cap pre_imp pre_mix pre_mir"
				if ("`scal_list_`c'_`y''" == " $checkvars") ///
					local scal_list_`c'_`y' " all"
				*display as result "`scal_list_`c'_`y''"
				
			}
			else {
				di as text "skipped, " _continue 
				di as error "`misvar_`c'_`y'' " _continue
				di as text "empty or missing"
			}	
 		}
		*report if not found
		else {
			di as text "skipped, `survey' not found"
		}
	}
}

				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				

