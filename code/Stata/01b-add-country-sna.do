//Description:
//Get information from detailed national accounts (country by country)

clear all

//0. General settings ----------------------------------------------------------

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Stata/auxiliar/aux_general.do"

*prepare data for URY 
qui do "code/Stata/auxiliar/aux_sna_ury.do"
clear 

//define some variables of interest 
local pir "ratio_d43d44"
local tra "ratio_d75_d7"  
global list_ratios pir tra 

//first year
local first_yr = $first_y 
local last_yr= $last_y

local cellr_PER "A21:U40"
local cellr_BRA_bloque1 "A30:X56"
local cellr_BRA "A26:U53"
local cellr_CHL "E27:Q48" 
local cellr_COL "F49:T146"  
local cellr_MEX "A68:S153" 
local cellr_CRI "A27:AY69"
local cellr_URY "A1:H38"
local cellr_DOM "D10:Q42"

*select codes and hh sect 
global vars_PER K L P G M N O U S
global vars_CHL I J N E K L M Q P
global vars_COL J K O F L M N T R
global vars_MEX A1 A2 I H C E G S N
global vars_CRI Z AA AT G AD AM AS AY AW
global vars_BRA K L P G M N O U S
global vars_DOM J K O F L M N P Q

*list of codes to fetch 
local dcodes 11 1 44 43 4 5 61 62 752 75 7
local bcodes 2 3 5

//loop over countries with integrated accounts
foreach c in $ctries_cei {
	
	//define country folder and file 
	local folder_`c' "primary_data/sna_country_data/`c'"
	if "`c'" == "BRA" local file_`c' "`folder_`c''/contas_economicas_a_precos_correntes_2000a2021.xls"
	if inlist("`c'", "CHL", "PER") local file_`c' "`folder_`c''/CEI_merged"
	*if "`c'" == "COL" local file_`c' "`folder_`c''/cuentas-economicas-integradas-2019provisional"
	if "`c'" == "COL" local file_`c' "`folder_`c''/anex-CuentasNalANuales-CuentaEconomicasIntegradas-2023p"
	if "`c'" == "DOM" local file_`c' "`folder_`c''/cei"
	//Mexico, Costa Rica and Uruguay defined below

	//1. Brazil ----------------------------------------------------------------
	
	//import data 
	if "`c'" == "BRA" {
		qui import excel `file_`c'', clear cellrange(`cellr_`c'_bloque1') ///
			sheet("Familias")
		qui rename (A B) (code code_long)
		qui drop in 13
		local iter = 0 
		foreach j in `c(ALPHA)'{
			if "`j'" >= "C" {
				local name = 2000 + `iter'
				cap qui rename `j' v`name'
				cap qui label var v`name' "`name'"
				*adapt to lcu (million reais)
				cap replace v`name' = v`name' * 10^6
				if _rc == 0 local iter = `iter' + 1
			}
		}
		
		*get rid of D7 uses 
		qui replace code = "D.7u" in 25
		
		//save data in memory
		forvalues y = `first_yr'/`last_yr' {
			
			//save aggregates for household sector 
			foreach n in `dcodes' {
				cap qui levelsof v`y' if code == "D.`n'", ///
					local(hh_d`n'r_`c'_`y') clean 
				if "`hh_d`n'r_`c'_`y''" == "" local hh_d`n'r_`c'_`y' = 0	
			}
			
			*GOS/MI/BPI
			foreach j in `bcodes' {
				cap qui levelsof v`y' if code == "  B.`j'", ///
					local(hh_b`j'g_`c'_`y') clean
			}
			
			//save ratios 
			*property incomes 
			cap local ratio_d43_d4_`c'_`y' = ///
				`hh_d43r_`c'_`y'' / `hh_d4r_`c'_`y'' 
			cap local ratio_d44_d4_`c'_`y' = ///
				`hh_d44r_`c'_`y'' / `hh_d4r_`c'_`y''
			cap local ratio_d43_b5g_`c'_`y' = ///
				`hh_d43r_`c'_`y'' / `hh_b5g_`c'_`y'' 
			cap local ratio_d44_b5g_`c'_`y' = ///
				`hh_d44r_`c'_`y'' / `hh_b5g_`c'_`y'' 
			cap local ratio_d43d44_`c'_`y' = ///
				`ratio_d43_d4_`c'_`y'' + `ratio_d44_d4_`c'_`y''
			*transfers  
			cap local ratio_d75_d7_`c'_`y' = ///
				`hh_d75r_`c'_`y'' / `hh_d7r_`c'_`y'' 
			cap local ratio_d75_b5g_`c'_`y' = ///
				`hh_d75r_`c'_`y'' / `hh_b5g_`c'_`y''
			
			//display 
			*di as result "`c' `y' d44 : `ratio_d44_d4_`c'_`y''"
			*di as result "`c' `y' d43 : `ratio_d43_d4_`c'_`y''"
			*di as result "`c' `y' d43 + d44 : `ratio_d43d44_`c'_`y''"
		}	
	}

	//2. Chile, Colombia, Costa Rica, Ecuador and Mexico -----------------------
	if inlist("`c'", "BRA", "CHL", "PER", "COL", ///
		"ECU", "MEX", "CRI", "URY", "DOM") {
		
		*define magnitudes 
		local mag * 1
		if inlist("`c'", "CHL", "COL") local mag * 10^9 
		if inlist("`c'", "MEX", "CRI", "BRA", "URY", "DOM", "PER") {
			local mag * 10^6 
		} 
		if inlist("`c'", "ECU") local mag * 10^3 
		
		forvalues y = `first_yr' / `last_yr' {
			
			//define locals 
			local y_sheet "`y'"
			local sp ""
			local b2 "B.2"
			local b3 "B.3"
			local b5 "B.5"
	
			//exceptions 
			if "`c'" == "ECU" {
				local file_`c' "`folder_`c''/ceinivel2sd"
				local cellr_ECU "A26:BV68"
				global vars_ECU A B BQ J AU BL BP BV BT
				if `y' <= 2014 local y_sheet "D `y'"
				if `y' == 2015 local y_sheet "`y'"
				if `y' == 2016 local y_sheet "`y'sd"
				if `y' >= 2017 {
					local file_`c' "`folder_`c''/CEI2007-2020p"
					local cellr_ECU "A26:Y90"
					global vars_ECU A B S J P Q R X V
					local y_sheet "CEI_`y'p"
				}
				if `y' >= 2018 {
					local y_sheet "CEI_`y'"
					local file_`c' "`folder_`c''/mcs_cei_2018_2023p"
					local cellr_ECU "A38:BD128" 
					global vars_ECU A B AY W AK AT AX BD BB
				}
				if `y' >= 2021 {
					local y_sheet "CEI_`y'p"
				}
			}
			if inlist("`c'", "ECU", "MEX") local sp " "
			if inlist("`c'", "ECU", "MEX", "COL", "PER") local b5 "B.5b"
			if inlist("`c'", "CHL") local b2_b3 "B.2b/B.3b"
			
			*exceptions for MEX
			if "`c'" == "MEX" {
				local num = `y' - 2000
				local nam = "CSI_`num'"
				local file_`c' "`folder_`c''/`nam'"
				local y_sheet "Tabulado"
			}
			
			*exceptions for BRA
			if "`c'" == "BRA" {
				local file_`c' "`folder_`c''/single_year/CEI`y'"
				local y_sheet "CEI"
				local sp "   "
			}
			
			*exeptions for CRI
			if "`c'" == "CRI" {
				local y_sheet "CEI`y'"
				if `y' >= 2017 {
					local y_sheet "CUENTAS CORRIENTES"
					local cellr_CRI "A26:AU91"
					global vars_CRI B C AO AN E M AE AS AU  				                 
				}
				local file_`c' "`folder_`c''/Cuentas_Economicas_Integradas_`y'"
			}
			
			*exceptions for URY
			local firstr ""
			if "`c'" == "URY" {
				local y_sheet "`y'"
				local file_`c' "`folder_`c''/cei.xlsx"
				local firstr firstrow
			}
			
			//import data 
			di as result "`c'- `y'"	
			qui cap import excel `file_`c'' , clear ///
				cellrange(`cellr_`c'') sheet("`y_sheet'") `firstr'
				
			//if file and sheet exist...	
			if _rc == 0 {		
		
				//rename variables 
				if "`c'" == "MEX" qui split A, parse(-)
				if "`c'" != "URY" {
					qui rename (${vars_`c'}) ///
						(code code_long households_r households_u ///
						NFC_r FC_r GG_r TOT_r ROW_r)
				}
				*if ("`c'" == "ECU" & `y' == 2018) exit 1
	
				*drop if unnecesary	
				if "`c'" == "ECU" qui drop if inlist(code, "Código", "0", "")
				
				foreach v in households_r households_u NFC_r ///
					FC_r GG_r TOT_r  ROW_r {
					cap destring `v', replace 
					cap replace `v' = . if `v' == 0 
				}
					
				//save household aggregates in memory	
				if "`c'" != "BRA" {
					foreach n in `dcodes' {
						if !inlist("`n'", "61", "5", "43") {
							local var households_r
						} 
						else local var households_u 
						cap qui levelsof `var' ///
							if inlist(code, "D.`n'`sp'", ///
								"`sp'D.`n'", "D`n'", "D.`n'"), ///
								local(hh_d`n'r_`c'_`y') clean
						if "`hh_d`n'r_`c'_`y''" == "" {
							local hh_d`n'r_`c'_`y' = 0
						} 
						local hh_d`n'r_`c'_`y' = `hh_d`n'r_`c'_`y'' `mag'
					}
				}
				
				//save other sectors in memory	
				foreach n in 4 41 42 43 44 {
					foreach var in NFC_r FC_r GG_r TOT_r ROW_r {
						
						cap qui levelsof `var' ///
							if inlist(code, "D.`n'`sp'", ///
								"`sp'D.`n'", "D`n'", "D.`n'"), ///
								local(`var'_d`n'r_`c'_`y') clean
						if "``var'_d`n'r_`c'_`y''" == "" {
							local `var'_d`n'r_`c'_`y' = 0
						} 
						local `var'_d`n'r_`c'_`y' = ///
							``var'_d`n'r_`c'_`y'' `mag'	
					} 
				}
				
		
				*GOS/MI/BPI
				foreach j in `bcodes' {
					
					*collect household data
					cap qui levelsof households_r ///
						if inlist(code, "`b`j''`sp'", "B.`j'b", "B`j'b", ///
						"B.`j'b`sp'", "`sp'B.`j'", "B.`j'b  ") & ///
						!missing(households_r), local(hh_b`j'g_`c'_`y') clean						
					if inlist("`c'", "CHL", "ECU") & `j' == 2 {
						if ("`c'" == "ECU" & `y' < 2018) {
							cap qui levelsof households_r ///
							if inlist(code, "B.2b/B.3b", "B.2b  y B.3b", ///
							"B.2b  / B.3b") & !missing(households_r), ///
							local(hh_b`j'g_`c'_`y') clean
						}
						if "`c'" == "CHL" {
							cap qui levelsof households_r ///
							if inlist(code, "B.2b/B.3b", "B.2b  y B.3b", ///
							"B.2b  / B.3b") & !missing(households_r), ///
							local(hh_b`j'g_`c'_`y') clean
						}
					}
					if "`hh_b`j'g_`c'_`y''" == "" local hh_b`j'g_`c'_`y' = 0
					local hh_b`j'g_`c'_`y' = `hh_b`j'g_`c'_`y'' `mag'
					
					*collect bpi for other IS
					if `j' == 5 & !inlist("`c'", "URY") {
						
						foreach i in "NFC" "FC" "GG" "TOT" {
							
							qui levelsof `i'_r if ///
								inlist(code, "`b`j''`sp'", "B.`j'b", ///
								"B`j'b", "B.`j'b`sp'", "`sp'B.`j'") & ///
								!missing(`i'_r) & `i'_r != 0 , ///
								local(`i'_b`j'g_`c'_`y') clean
							
						if "``i'_b`j'g_`c'_`y''" == "" {
							
							local `i'_b`j'g_`c'_`y' = 0
							
						}	
						local `i'_b`j'g_`c'_`y' = ``i'_b`j'g_`c'_`y'' `mag'	
 						}
						
					}
				}
				
				//save ratios 
				*property incomes  	
				local ratio_d43_d4_`c'_`y' = ///
					`hh_d43r_`c'_`y'' / `hh_d4r_`c'_`y'' 
				local ratio_d44_d4_`c'_`y' = ///
					`hh_d44r_`c'_`y'' / `hh_d4r_`c'_`y''
				local ratio_d43d44_`c'_`y' = ///
					`ratio_d43_d4_`c'_`y'' + `ratio_d44_d4_`c'_`y''
				local ratio_d44_b5g_`c'_`y' = ///
					`hh_d44r_`c'_`y'' / `hh_b5g_`c'_`y'' 
				*transfers   	
				local ratio_d75_d7_`c'_`y' = ///
					`hh_d75r_`c'_`y'' / `hh_d7r_`c'_`y'' 
				local ratio_d75_b5g_`c'_`y' = ///
					`hh_d75r_`c'_`y'' / `hh_b5g_`c'_`y'' 
				*inform 
				*di as result "`c' `y' d44: `ratio_d44_d4_`c'_`y''"
				*di as result "`c' `y' d43: `ratio_d43_d4_`c'_`y''"
				*di as result "`c' `y' d43 + d44: `ratio_d43d44_`c'_`y''"
			}	
		}
	}
}	

* NOTE: INCLUDE COMPUTATION OF EMPLOYEE SSC FOR CRI

//3. build a summary dataset from scratch --------------------------------------

//call graph basics  
global aux_part " "graph_basics" " 
do "code/Stata/auxiliar/aux_general.do"

//make room for data 
qui clear 
local n_ctries = wordcount( `"${ctries_cei}"' )
local n_obs = (`last_yr' - `first_yr' + 1) * `n_ctries'
set obs `n_obs'
qui gen country = ""

*make room for variables 
foreach n in `dcodes' {
	qui gen D`n'_cei = . 
}
foreach n in `bcodes' {
	qui gen B`n'g_cei = .
	if `n' == 5{
		foreach i in "NFC" "FC" "GG" "TOT"{
			qui gen `i'_B`n'g_cei = . 
		}
	}
}
foreach n in 4 41 42 43 44 {
	foreach var in NFC_r FC_r GG_r TOT_r ROW_r {
		qui gen `var'_D`n'_cei = . 
	}
}	
foreach var in "year" "ratio_d43d44" ///
	"ratio_d43_d4" "ratio_d44_d4" "ratio_d44_b5g" ///
	"ratio_d75_d7" "ratio_d75_b5g" {
	qui gen `var' = .
}
order country year

//loop over countries and years  
local iter = 1 
foreach c in $ctries_cei {
	forvalues y = `first_yr' / `last_yr' {
		
		//fill values 
		qui replace country = "`c'" in `iter'
		qui replace year = `y' in `iter'
		
		//Fill variables 
		foreach n in `dcodes' {
			if !inlist("`hh_d`n'r_`c'_`y''", "", "0") {
				qui replace D`n'_cei = `hh_d`n'r_`c'_`y'' in `iter'
			} 
		}
		
		foreach j in `bcodes' {
			if !inlist("`hh_b`j'g_`c'_`y''", "", "0") {
				qui replace B`j'g_cei = `hh_b`j'g_`c'_`y'' in `iter'
			} 
			if `j' == 5 {
				foreach i in "NFC" "FC" "GG" "TOT" {
					if !inlist("``i'_b`j'g_`c'_`y''", "", "0") {
						qui replace `i'_B`j'g_cei = ``i'_b`j'g_`c'_`y'' ///
							in `iter'
					} 
				}
			}
		}
		
		foreach var in "ratio_d43d44" ///
			"ratio_d43_d4" "ratio_d44_d4" "ratio_d44_b5g" ///
			"ratio_d75_d7" "ratio_d75_b5g" {
			if "``var'_`c'_`y''" != "" {
				qui replace `var' = ``var'_`c'_`y'' in `iter' 
			} 
		}
		foreach n in 4 41 42 43 44 {
			foreach var in NFC_r FC_r GG_r TOT_r ROW_r { 
				if !inlist("``var'_d`n'r_`c'_`y''", "", "0") {
					qui replace `var'_D`n'_cei = ``var'_d`n'r_`c'_`y'' ///
						in `iter'
				}
			}
		}
		
		//add one to counter 
		local iter = `iter' + 1
	}
	
	//prepare lines for graph
	foreach var in $list_ratios {
		
		//define color 
		local c1 = strlower("`c'")
		local c2 "c_`c1'"
		//lines 
		local lines_`var' `lines_`var'' ///
			(connected `var' year if country == "`c'", ///
			lcolor($`c2') lwidth(thick) msize(tiny) msymbol(O) ///
			mfcolor($`c2') mcolor($`c2'))	
		//prepare tags 
		qui sum year if country == "`c'" & !missing(``var'')
		if !inlist("`c'", "CRI", "COL") local xtext_`c' = r(min)	
		else local xtext_`c' = r(max)	
		qui sum ``var'' if year == `xtext_`c'' & country == "`c'" ///
			& !missing(``var'')
		if r(mean) != . {
			local ytext = (1 - r(mean)) * 100
			if !inlist("`c'", "CRI", "COL") local text_loc_`var' "`text_loc_`var'' text(`ytext' `xtext_`c'' "`c'", orientation(horizontal) placement(nw) color($`c2') size(small)) "
			else local text_loc_`var' "`text_loc_`var'' text(`ytext' `xtext_`c'' "`c'", orientation(horizontal) placement(ne) color($`c2') size(small)) "
		}	
	}
}

//define labels 
local lab_pir "Matching Property Inc. (%)"
local lab_tra "Share of remittences in transfers, in % (D75/D7)"

//define ratios of interest 
foreach var in $list_ratios {
	
	local ylabs ""
	if "`var'" == "pir" local ylabs 50(10)100 
	
	*get proportional 
	if "`var'" == "pir" qui gen `var' = (1 - ``var'') * 100
	else qui gen `var' = ``var'' * 100

	//graph result 
	qui sum `var', meanonly 
	local avg = r(mean)
	local avg_tag = `avg' + 2
	graph twoway `lines_`var'' if !missing(`var') & `var' != 0  ///
		, legend(off) `text_loc_`var''  ///
		text(`avg_tag' 1996 "AVG", color(black * 0.3)) ///
		ytitle("`lab_`var''") xtitle("") ///
		yline(`avg', lcolor(black*0.3) lwidth(thick)) ///
		ylabels(`ylabs', $ylab_opts_white ) ///
		xlabels(1995(5)2025, $xlab_opts_white ) $graph_scheme
	qui graph export "output/figures/cei/`var'.pdf", replace 	
	qui graph export "output/figures/cei/`var'.png", replace 

}

*save to merge 
tempfile savetomerge 
qui rename country _ISO3C_ 
qui save `savetomerge', replace 

*open SNA file 
qui use "intermediary_data/national_accounts/UNDATA-WID-Merged.dta", clear 

*merge with SNA data
qui merge m:1 _ISO3C_ year using `savetomerge' , nogen update replace

//ignore aggregated data for ECU (OS + MI)
qui replace B2g_cei = . if country == "ECU" & year <= 2017
qui replace HH_B2g_R = . if country == "ECU" & year <= 2017

//save
qui save "intermediary_data/national_accounts/UNDATA-WID-Merged.dta" , replace  

