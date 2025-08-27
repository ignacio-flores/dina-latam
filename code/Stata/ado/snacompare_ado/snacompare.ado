// This program compares aggregate incomes in memory to those in SNA 
// This program compares aggregate incomes in memory to those in SNA 
// for each country and year specified
// Authors: Mauricio De Rosa, Ignacio Flores, Marc Morgan (2019)

program snacompare , eclass 
	version 11 
	syntax using/ , [TIme(numlist max=2)  EXT(string) ///
		EXPortexcel(string) AUXiliary(string) SHOW ESPañol ///
		GRAPHEXTRAPolate] SVYPath(string) TYPe(string) ///
		AReas(string) WEIght(string) EDAD(numlist max=2)	
	
	*---------------------------------------------------------------------------
	*PART 0: Checks and export paths
	*---------------------------------------------------------------------------
	 
	//SNA data file exists
	confirm file "`using'" 
	 
	//Check time  
	if ("`time'" != "") {
		if (wordcount("`time'") == 1) {
			di as error "Option time() incorrectly specified:" ///
				" Must contain both the first and last values" ///
				" Default is 1990 and 2017 respectively."
			exit 198
		}
		local first_yr: word 1 of `time'
		local last_yr: word 2 of `time'
	}
	else {
		local first_yr = 1989
		local last_yr = 2017
		di as text "First period was automatically set to `first_yr'" ///
		" and last period to `last_yr'"
	}
	
	//save selected weight 
	local orig_weight "`weight'"
	
	//Paths to export graphs
	global figs_path "output/figures/snacompare"
	
	*---------------------------------------------------------------------------
	*PART 1: Bring SNA totals
	*---------------------------------------------------------------------------
	
	//Loop over years and countries 
	local  iter = 1
	forvalues yr = `first_yr'/`last_yr' {
		foreach c in `areas' {
			
			//Weights (Argentinian exception)
			local weight "`orig_weight'"
			if ("`c'" == "ARG" & "`type'" == "raw") local weight "n_fep"
			
			local iter = `iter' + 1
			global current_year = `yr'
			
			//Keep only the relevant row
			qui use `using', clear	
			qui keep if _ISO3C_ ==	"`c'" & year ==	`yr'
			
			//ignore some series (exceptions)
			global aux_part = 1 
			qui do `auxiliary'
			
			//Continue only if data exists
			cap assert _N == 0
			if _rc != 0 {
				
				//Keep most recent SNA series available	
				qui sum series 
				qui keep if series == r(max)			

				//Fill missing values and special cases
				foreach cod in "D4" "B2g" "B5g" "D5" {
					foreach x in "U" "R" {
						cap qui replace HH_`cod'_`x' ///
							= HH_NPISH_`cod'_`x' ///
							if missing(HH_`cod'_`x') & ///
							!missing(HH_NPISH_`cod'_`x')
						local x "1"	
						cap qui replace CORPS_`cod'_`x' ///
							= NFC_`cod'_`x' + FC_`cod'_`x' ///
							if missing(CORPS_`cod'_`x') & ///
							!missing(NFC_`cod'_`x', FC_`cod'_`x')
					}
				}
				
				//Get National income from wid and undata  
				local ni_`c'_`yr' = TOT_B5g_U[1]
				local ni2_`c'_`yr' = TOT_B5g_wid[1]
				local z_`c'_`yr' = `ni2_`c'_`yr'' / `ni_`c'_`yr''
				if inlist("`z_`c'_`yr''", ".", "", " ") local z_`c'_`yr' = 1 
						
				//Generate totals from SNA
				qui gen pre_wag_nac	= (TOT_D1_R - TOT_D61_U)
				qui gen pre_ssc_nac	= TOT_D61_U 
				qui gen pre_ben_nac	= TOT_D62_R 
				qui gen pre_mix_nac	= TOT_B3g_R 
				qui gen pre_cap_nac	= HH_D4_R 
				qui gen pre_cap_nac_u = HH_D4_U  
				qui gen pre_imp_nac	= HH_B2g_R 
				qui gen pre_kap_nac	= (pre_cap_nac + pre_imp_nac) 
				qui gen pre_mir_nac = (pre_imp_nac + pre_mix_nac)  
				qui gen pre_tax_nac = HH_D5_U 
				qui gen pre_tot_nac = HH_B5g_R 
				qui gen pre_gni_nac	= TOT_B5g_R 
				qui gen pre_dis_nac	= HH_B6g_R 
				qui gen pre_upc_nac = CORPS_B5g_1 
				qui gen pre_upg_nac = GG_B5g_R 
				qui gen pre_gni_wid	= TOT_B5g_wid
				
				//check if variables are empty  
				qui egen hhva_test = rowtotal(HH_*_R)
				qui levelsof hhva_test, local(loc_hhva_`c'_`yr') clean
				qui levelsof series, local(loc_series_`c'_`yr') clean 
				qui egen cei_test = rowtotal(*_cei)
				qui levelsof cei_test, local(loc_cei_test_`c'_`yr') clean 
				
				*di as result "loc_hhva_`c'_`yr': " `loc_hhva_`c'_`yr''
				*di as result "loc_cei_test_`c'_`yr': " `loc_cei_test_`c'_`yr''
				
				//if they are, collect info from CEI 
				if (`loc_hhva_`c'_`yr'' == 0 & ///
					`loc_cei_test_`c'_`yr'' != 0) {
						
					*redefine SNA variables 
					qui replace series = 0
					qui replace pre_wag_nac = D1_cei - D61_cei 
					qui replace pre_ssc_nac = D61_cei 
					qui replace pre_ben_nac = D62_cei 
					qui replace pre_mix_nac = B3g_cei 
					qui replace pre_cap_nac = D4_cei 
					qui replace pre_imp_nac = B2g_cei 
					qui replace pre_mir_nac = (pre_imp_nac + pre_mix_nac)
					qui replace pre_tax_nac = D5_cei 
					qui replace pre_tot_nac = B5g_cei 
					qui replace pre_upc_nac = NFC_B5g_cei + FC_B5g_cei 
					qui replace pre_upg_nac = GG_B5g_cei 
					
					//Get National income from wid and undata  
					qui replace pre_gni_nac = TOT_B5g_cei 
					local ni_`c'_`yr' = pre_gni_nac[1]
					local z_`c'_`yr' = `ni2_`c'_`yr'' / `ni_`c'_`yr''
					if inlist("`z_`c'_`yr''", ".", "", " ") local z_`c'_`yr' = 1 
				}
				
				*other variables 
				qui gen pre_P31G_nac = GG_P31_U 
				qui gen pre_P32G_nac = GG_P32_U 
				qui gen pre_D2G_nac  = GG_D2_R 
				qui gen pre_D3G_nac  = GG_D3_R 
				qui gen pre_D31G_nac = GG_D31_R 
				
				*Scale to wid level
				qui ds *_nac* 
				*di as result "varlist: `r(varlist)'"
				local nac_varlist "`r(varlist)'"
				foreach v in `nac_varlist' {
					qui replace `v' = `v' * `z_`c'_`yr''
				}
				
				//simplify some variables (exceptions) 
				global aux_part = 2 
				qui do `auxiliary'
				
				//la variables
				qui la var pre_wag_nac "CE less SSC (nat. acc)"
				qui la var pre_ben_nac "Total social benefits (nat. acc)"
				qui la var pre_mix_nac "Mixed income (nat. acc)"
				qui la var pre_cap_nac "Property inc, received by HH (nat. acc)"
				qui la var pre_imp_nac "Gross op. surpluss, of HH (nat. acc)" 
				qui la var pre_kap_nac "All CI of HH incl. IR (nat. acc)"
				qui la var pre_mir_nac "Imp rents plus mixed inc (nat. acc)" 
				qui la var pre_tot_nac "Total income of HH (nat. acc)"
				qui la var pre_gni_nac "Gross National Income (nat. acc)"	
				qui la var pre_gni_wid "Gross National Income (WID)"
				qui la var pre_P31G_nac "Ind. consumption expenditure (UN)"
				qui la var pre_P32G_nac "Col. consumption expenditure (UN)"
				qui la var pre_D2G_nac "Taxes on production and imports"
				qui la var pre_D3G_nac "Subsidies"
				qui la var pre_D31G_nac "Subsidies on products"
				
				//Set table for log
				*di as text "{hline 50}"
				di as text "05a: Comparing aggregates for " ///
					_continue 
				di as text "`c' - `yr'"
				*di as text "{hline 50}"
					
				if ("`show'" != ""){	
					di as text "{hline 50}"
					di as text "Composition of HH inc. in sna (% of NI)"
					di as text "{hline 50}"
				}
				
				//Store sna-values in memory
				qui ds *_nac* 
				local nac_varlist "`r(varlist)'"
				foreach var in `nac_varlist' pre_gni_wid {
					qui gen sh_ni_`var' = `var' / TOT_B5g_wid * 100
					qui gen va_ni_`var' = `var' 
					qui la var sh_ni_`var' "`var' (% of National Income)"
					qui la var va_ni_`var' "`var' (Current LCU)"
					
					*total for scaling 
					qui sum `var' 			
					local `var'_tot	= r(sum)
					
					qui sum sh_ni_`var'
					local loc_`var'_`c'_`yr' = r(sum)
					qui sum va_ni_`var'
					local loc2_`var'_`c'_`yr' = r(sum)

					
					if ("`show'" != ""){
						//di on screen 
						di as text "`var': " round(`loc_`var'_`c'_`yr'', 0.1) "%"
					}
				}
				
				*generate a check for scaled 
				qui gen res_check_nac = pre_wag_nac + pre_ben_nac ///
					+ pre_mir_nac + pre_cap_nac 
				qui sum res_check_nac, meanonly 
				local loc_check_nac = r(sum)
				
				//Store sna-series in memory
 				qui levelsof series, local(loc_series_`c'_`yr') ///
					clean 
				if strlen("`loc_series_`c'_`yr''") == 1 {
					local loc_sna_`c'_`yr' "CEI"
				}	
				if strlen("`loc_series_`c'_`yr''") == 2 {
					local loc_sna_`c'_`yr' "earlier than SNA93"
				}	
				if strlen("`loc_series_`c'_`yr''") == 3 {
					local loc_sna_`c'_`yr' "SNA93"
				}
				if strlen("`loc_series_`c'_`yr''") == 4 {
					local loc_sna_`c'_`yr' "SNA08"
				}
				
				*---------------------------------------------------------------
				*PART 2: Compare to microdata
				*---------------------------------------------------------------
				
				//define list of variables
				foreach x in pre pos {
					*re-write if necessary
					local variables_`x' ""
					foreach v in wag ben cap mix pre imp mir kap tot {
						local variables_`x' "`variables_`x'' `x'_`v'"
					}
				}	
				
				local survey ""
				if inlist("`type'", "raw", "bfm_pre", "bfm_norep_pre") {
					local survey "`svypath'bfm`ext'_pre/`c'_`yr'_bfm`ext'_pre.dta"
				}
							
				//Check existence of file
				cap confirm file "`survey'" 
					
				//continue if available 	
				if _rc==0 {
					
					*di as result "`survey' found"
					qui use "`survey'", clear
					cap drop __*
						
					//drop population *********************************
					if wordcount("`edad'") == 1 {
						local edlab "`edad'"
						qui drop if edad < `edad' 
					}
					if wordcount("`edad'") == 2 {
						forvalues w = 1/2 {
							local w`w': word `w' of `edad'
						}
						local edlab "`w1'_`w2'"
						qui drop if !inrange(edad, `w1', `w2')
					}
					
					if ("`show'" != ""){
						//cosmetics
						di as text "{hline 80}"
						di as text "Scal. factors by inc. type (svy to sna)"
						di as text "{hline 80}"
					}
					
					//record proportional alloc. variable by income source
					qui replace `weight' = round(`weight')
					foreach var in `variables_pre' `variables_pos' {
							
						*check if variable exists
						cap confirm variable ind_`var', exact 
						
						if _rc == 0 {
							
							*short name 
							local v = substr("`var'", 5, 3)
							local s = substr("`var'", 1, 3)
							
							*summarize survey total
							qui sum ind_`var' [w = `weight'] 
							local `var'_svy_tot = r(sum) 
							
							*scaling factor 
							local `var'_scal_svyna_`c'_`yr'	= ///
								`pre_`v'_nac_tot' / ``var'_svy_tot' 	
							local `var'_invscal_`c'_`yr'	= ///
								``var'_svy_tot'/ `pre_`v'_nac_tot' 
								
							*check if scaling factor works
							tempvar t_`v' 
							qui gen `t_`v'' = ///
								ind_`var' * ``var'_scal_svyna_`c'_`yr''
							qui sum `t_`v'' [w = `weight'], meanonly 
							local test1 = r(sum)
							if r(sum) != 0 {
								assert round(`test1' / `pre_`v'_nac_tot' * ///
									10000) == 10000
							}
								
							*prepare for di on log 	
							local `var'_invscal_`c'_`yr'_rd = ///
								round(``var'_invscal_`c'_`yr'', 0.001) * 100
							local `var'_scal_svyna_disp	= ///
									round(``var'_scal_svyna_`c'_`yr'', 0.01)	
							
							/*
							if ("`show'" != ""){
								//di scaling factors
								di as text "`var' -> ``var'_scal_svyna_disp'" ///
									_continue 
								di as text " --> Survey: ``var'_svy_tot' (LCU)" ///
									_continue 
								di as text " SNA: `pre_`v'_nac_tot'"
							}
							*/
							if ("`v'" == "tot") {
								local svy_tot_`c'_`yr'_`s' = ``var'_svy_tot'
							}	
						}
						else {
							di as text "`var' not included"
						}
					}
					*di as text "{hline 80}"
					*di as result "`c' - `yr' value (`type'): " `svy_tot_`c'_`yr'' / `ni_`c'_`yr'' * 100
				}
				else {
					di as text "  * survey not found"
				}
			}
			else {
				di as text "  * not found in SNA data"
			}
		}
	}	
	
	*---------------------------------------------------------------------------
	*PART 3: Summarize scaling factors 
	*---------------------------------------------------------------------------

	clear all
	tempvar aux1 aux2 aux3 
	local strvars "country series `aux1'"
	local compvars "`nac_varlist' svy_sh" //pre_upg_nac pre_tot_nac pre_upc_nac
	local numvars "`variables_pre' `variables_pos' `compvars' svy_to_ni_wid_pre svy_to_ni_wid_pos TOT_B5g_wid year wid_scal_ni"
	
	//Make room for info
	local setobs = `iter' - 1
	set obs `setobs'
	foreach var in `strvars' {
		qui gen `var' = ""
	}
	foreach var in `numvars' {
		qui gen `var' = . 
	}

	//Loop over countries 
	local iter = 1 
	local diff = `last_yr' - `first_yr' 
	foreach c in `areas' {
		local iter_plus_diff = `iter' + `diff'
		local year = `first_yr'
		forvalues n = `iter' / `iter_plus_diff' {
		
			//Fill basic variables 
			qui replace country = "`c'" in `n'
			qui replace year = `year' in `n'
			if !inlist("`z_`c'_`year''", ".", "", " ") qui replace wid_scal_ni = `z_`c'_`year'' in `n'
			
			//Fill scaling factors 
			foreach var in `variables_pre' `variables_pos' {
				if !inlist("``var'_scal_svyna_`c'_`year''", "", ".") {
					qui replace `var' = ``var'_invscal_`c'_`year'' * 100 ///
						if country == "`c'" & year == `year'
				}
			}
			
			//Fill NI composition vars 
			foreach var in `compvars' {
				if (strpos("`var'", "_nac") & "`loc_`var'_`c'_`year''" != "") {
					qui replace `var' = `loc_`var'_`c'_`year'' ///
					if country == "`c'" & year == `year'
				}
				//Fill survey to ni (un-data)
				if ("`var'" == "svy_sh" & ///
					"`svy_tot_`c'_`year'_pre'" != "" & ///
					"`ni_`c'_`year''" != "") {
					qui replace `var' = ///
						`svy_tot_`c'_`year'_pre' / `ni_`c'_`year'' * 100 ///
						if country == "`c'" & year == `year'
				}
				
				//Fill survey to ni (wid-data)
				foreach x in pre pos {
					if ("`var'" == "svy_sh" & ///
					"`svy_tot_`c'_`year'_`x''" != "" & ///
					"`ni2_`c'_`year''" != "") {
						qui replace svy_to_ni_wid_`x' = ///
							`svy_tot_`c'_`year'_`x'' / `ni2_`c'_`year'' * 100 ///
							if country == "`c'" & year == `year'
						qui replace TOT_B5g_wid = `ni2_`c'_`year'' ///
								if country == "`c'" & year == `year'
					}
				}
				qui replace `var' = . if `var' == 0 
			}
			
			//di sna info on screen
			if ("`loc_sna_`c'_`year''" != "") {
				qui replace series = ///
					"series nº `loc_series_`c'_`year'' - `loc_sna_`c'_`year''" ///
					if country == "`c'" & year == `year'
					if "`loc_sna_`c'_`year''" == "CEI" {
						qui replace series = "CEI" ///
							if country == "`c'" & year == `year'	
					} 
				//SNA-series short-version
				qui replace `aux1' = "`loc_series_`c'_`year''" ///
					if country == "`c'" & year == `year'
			}
			
			//Add one to iterations
			local iter = `iter' + 1
			local year = `year' + 1
		}
	}
	
	//cosmetics 
	qui ren pre_upg_nac bpi_gg
	qui ren pre_tot_nac bpi_hh 
	qui ren pre_upc_nac bpi_corp
	qui ren svy_sh svy_to_ni 
	qui order country year series

	// impute missing scaling factors
	global imput_vars pre_wag pre_ben pre_cap pre_mix ///
		pre_imp pre_mir pre_kap pre_tot bpi_gg bpi_hh bpi_corp 
	qui do "code/Stata/auxiliar/aux_fill_aver.do"
	
	//export national income composition 
	if ("`exportexcel'" != "") {
		preserve 
			qui egen `aux3' = rowtotal(bpi_* svy_*)
			qui replace series = "" if `aux3' == 0 
			qui cap drop __*
			*qui cap drop pre_* 
			qui format bpi_* svy_* %2.1f
			qui export excel using "`exportexcel'", firstrow(variables) ///
				sheet("ni_comp") sheetreplace keepcellfmt
		restore 
	}
	
	//empty sna-series for absent scaling factors 
	qui egen `aux2' = rowtotal(pre_*)
	qui replace series = "" if `aux2' == 0 
	qui replace `aux1' = "" if `aux2' == 0 
	
	//simplify some variables (exceptions) 
	global aux_part = 3 
	qui do `auxiliary'
	qui replace `aux1' = "200" if country == "CHL" & year == 2003
		
	//export scaling factors
	if ("`exportexcel'" != "") {
	
		//la variables
		la var pre_wag "Wages over comp. of employees less total SSC"
		*la var pre_pen "Pensions over social sec. Benefits"
		la var pre_ben "Pensions & other inc. over social Sec. benefits"
		la var pre_mix "Independent inc over Mixed income"
		la var pre_cap "Capital inc. over Property inc received by HH"
		la var pre_mir "Mixed Income plus Operating Surplus of HH"
		la var pre_imp "Imputed Rents over Op. Surplus of HH sector" 
		la var pre_kap "Capital inc. (incl. Imp. Rents) over OS & PI received"
		la var pre_tot "Total inc. over Balance of 1ry inc. (B5g)"

		preserve 		
			qui cap drop __* 
			*qui cap drop bpi_* svy_to_ni 
			qui export excel using "`exportexcel'", firstrow(variables) ///
				sheet("scal_sna") sheetreplace keepcellfmt 
		restore 	
	}
	
	*save info on extrapolated values 
	preserve
		keep country year extrap_sca
		qui save "output/snacompare_summary/extrap_sna_age`edlab'.dta", replace
	restore

	*get rid of extrapolated data 
	if "`GRAPHEXTRAPolate'" == "" {
		foreach v in $imput_vars {
			qui replace `v' = . if extsna_`v' == 1
		}
	}
	
	//find first year of each series 
	foreach c in `areas' {
		qui levelsof `aux1' if country == "`c'", local(listseries_`c') clean
		local n_of_series_`c' = wordcount("`listseries_`c''")
		
		//prepare lines to add to graph
		foreach s in `listseries_`c'' {

			qui sum year if country == "`c'" & `aux1' == "`s'"
			local minyr_`c'_`s' = `r(min)'
			qui levelsof series if country == "`c'" & `aux1' == "`s'", ///
				local(series_`c'_`s') clean		
			local add_line_`c' ///
				"`add_line_`c'' xline(`minyr_`c'_`s'', lcolor(black*0.2))"
			local add_text_`c' ///
				"`add_text_`c'' text(160 `minyr_`c'_`s'' "`series_`c'_`s''", orientation(vertical) placement(e) color(black*0.2) size(vsmall)) " 
		}
	}

	//Loop over variables 
	qui sort country year
	foreach var in `variables_pre' {
		
		//Call graph-settings
		global var "`var'"
		global aux_part = 4 
		
		qui do `auxiliary'
	
		//cosmetics 
		local maxy = 200
		local midy = 50
		*if ("`var'" == "pre_imp") local maxy = 250
				
		//graph all countries (exclude exceptions for pre_mix and pre_imp)
		local exclude_`var' & !inlist(country, "BOL")
		if inlist("`var'", "pre_imp", "pre_mix") local exclude_`var' & missing(exception)
		
		if "${lang}" == "esp" {
			local ylab1 "Encuesta sobre cuentas nacionales"
		} 
		else {
			local ylab1 "Survey / NA"
		}
		graph twoway $per_country_settings ///
			if year >= 2000 & !missing(`var') `exclude_`var'' ///
			/*& country != "ARG"*/, ///
			ytitle("`ylab1'") xtitle("") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(`midy')`maxy', $ylab_opts format(%2.0f)) ///
			xlabel(2000(5)2025, $xlab_opts) ///
			$graph_scheme legend(off)	
			
			
			local dirpath "$figs_path/variables"
			mata: st_numscalar("exists", direxists(st_local("dirpath")))
			if (scalar(exists) == 0) {
				mkdir "`dirpath'"
				display "Created directory: `dirpath'"
			}
			
			//Save		
			qui graph export "$figs_path/variables/`var'_age`edlab'.pdf", replace
			*qui graph save "$figs_path/variables/`var'_age`edlab'.gph", replace
			
		//also one graph with only exceptions 	
		if inlist("`var'", "pre_mir") {
			
			if "${lang}" == "esp" {
				local ylab1 "Encuesta sobre cuentas nacionales"
			} 
			else {
				local ylab1 "Survey / NA"
			}
		
			graph twoway $per_country_settings_exep ///
			if !missing(`var') & !missing(exception), ///
			ytitle("`ylab1'") xtitle("") ///
			yline(100, lpattern(dash) lcolor(black*0.5)) ///
			ylabel(0(`midy')`maxy', $ylab_opts format(%2.0f)) ///
			xlabel(`first_yr'(5)2025, $xlab_opts) ///
			$graph_scheme legend(off)
			
			//Save
			qui graph export "$figs_path/variables/`var'_exep.pdf", replace
			*qui graph save "$figs_path/variables/`var'_gph.pdf", replace
			
			//get one graph with a legend (exceptions)
			graph twoway $per_country_settings_exep , ///
				$legend_ctries_exep $graph_scheme
			
			//save	
			qui graph export ///
				"$figs_path/variables/legend_ctries_exep.pdf", replace			
		}	
	}

	//get one graph with a legend
	global var "pre_wag"
	graph twoway $per_country_settings , ///
		$legend_ctries $graph_scheme	
	
	//save		
	qui graph export "$figs_path/variables/legend_ctries.pdf", replace


	//Now graph all variables by country 
	foreach c in `areas' {
	
		//cosmetics 
		local limit = 200 
		local byn = 50
		if inlist("`c'", "BOL", "PER")  local limit = 300
		if "`c'" == "PRY" local limit = 1800
		if "`c'" == "PRY" local byn = 300
		
		*- CHANGE ON
		/*
		//graph all variables 
		graph twoway $per_variable_settings if country == "`c'" , ///
			ytitle("Survey / NA") xtitle("") ///
			yline(100, /*lpattern(dash)*/ lcolor(black*0.2)) ///
			yline(0, /*lpattern(dash)*/ lcolor(black*0.2)) ///
			ylabel(0(`byn')`limit', $ylab_opts_white format(%2.0f)) ///
			xlabel(2000(5)2025, $xlab_opts_white ) ///
			`add_line_`c'' `add_text_`c'' ///
			$graph_scheme $legend_vars /*legend(off)*/
		*/

		if "`GRAPHEXTRAPolate'" != "" local pvs $per_variable_settings $per_variable_settings_e
		else local pvs $per_variable_settings
		
		if "`c'" == "DOM" {
			local firsty = 2012 
			local xr = 2 
			local addcon & year >= `firsty'
		}
		else {
			local firsty = $first_y 
			local xr = 5 
			local addcon 
		}
		
		if "`español'" != "" {
			local ytit "Encuesta / Cuentas Nacionales"
		}
		else {
			local ytit "Survey / NA"
		}	
		
		//graph all variables  // 
		graph twoway `pvs' if country == "`c'" `addcon' , /// 
			ytitle("") xtitle("") ///
			yline(100, /*lpattern(dash)*/ lcolor(black*0.2)) ///
			yline(0, /*lpattern(dash)*/ lcolor(black*0.2)) ///
			ylabel(0(`byn')`limit', $ylab_opts_white format(%2.0f)) ///
			xlabel(`firsty'(`xr')2025, $xlab_opts_white ) ///
			`add_line_`c'' `add_text_`c'' ///
			$graph_scheme $legend_vars_esp  legend(off)
	
		*- CHANGE OFF
		
		local dirpath "$figs_path/countries"
			mata: st_numscalar("exists", direxists(st_local("dirpath")))
			if (scalar(exists) == 0) {
				mkdir "`dirpath'"
				display "Created directory: `dirpath'"
			}
		
		//Save
		qui graph export "$figs_path/countries/`c'_age`edlab'.pdf", replace
		
		
		//graph all variables  with legend// 
			graph twoway `pvs' if country == "URY" `addcon' , /// 
				ytitle("") xtitle("") ///
				yline(100, /*lpattern(dash)*/ lcolor(black*0.2)) ///
				yline(0, /*lpattern(dash)*/ lcolor(black*0.2)) ///
				ylabel(0(`byn')`limit', $ylab_opts_white format(%2.0f)) ///
				xlabel(`firsty'(`xr')2025, $xlab_opts_white ) ///
				`add_line_`c'' `add_text_`c'' ///
				$graph_scheme $legend_vars_esp  /*legend(off)*/ 
			
			*- CHANGE OFF
			
			//Save
			qui graph export "$figs_path/countries/legend.pdf", replace
	}
	
	//Graph variables by country (for exceptions)
	qui levelsof country if !missing(exception), local(exception_ctries)
	foreach c in `exception_ctries'{
	
		//cosmetics 
		local limit = 200 
		local byn = 50
		if inlist("`c'", "BOL")  local limit = 300
		
		if "`español'" != "" {
			local ytit "Encuesta / Cuentas Nacionales"
		}
		else {
			local ytit "Survey / NA"
			local lang ""
		}	
		
		if "`GRAPHEXTRAPolate'" != "" {
			local pvs2 $per_variable_settings_exep ///
				$per_variable_settings_exep_e
		} 
		else {
			local pvs2 $per_variable_settings_exep
		} 
			
		//graph all variables 
		graph twoway `pvs2' ///
			if country == "`c'"  & ///
			(!missing(exception) | year > 2016), ///
			ytitle("") xtitle("") ///
			yline(100, /*lpattern(dash)*/ lcolor(black*0.2)) ///
			yline(0, /*lpattern(dash)*/ lcolor(black*0.2)) ///
			ylabel(0(`byn')`limit', $ylab_opts_white format(%2.0f)) ///
			xlabel(${first_y}(5)2025, $xlab_opts_white ) ///
			`add_line_`c'' `add_text_`c'' ///
			$graph_scheme ${legend_vars_exep_esp}

	*CHANGE OFF
			
		//Save
		qui graph export "$figs_path/countries/`c'_age`edlab'.pdf", replace
			
	}
		
end 	

