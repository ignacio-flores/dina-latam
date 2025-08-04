//BEGINNING OF SUB-CODE --------------------------------------------------------

//This auxiliary do-file intervenes in the program 'snacompare.ado', 
//to apply exceptions, preventing the use -by default- 
//of the most recent SNA series (ideally, we do not use the most recent
//series if they are less detailed than older versions)


//I. Exceptions  ----------------------------------------------------------

//1. Ignore some series
if $aux_part == 1 {
	//Too little detail or empty, while better options are available
	drop if (iso == "BR" & inrange(year, 2000, 2009)) & series == 1000
	drop if (iso == "BR") & series == 200
	drop if (iso == "AR" & year >= 2000) & series == 1100 
	drop if (iso == "AR" & year < 2004)
	*drop if (iso == "CL" & year >= 2000) & series == 200
	*drop if (iso == "CL" & year >= 2000 & year <= 2009) & series == 1000
	drop if (iso == "MX" & year >= 2000 & year <= 2002) & series == 300
	
	// Exclude Costa Rica SNA 2017
	local cei17 "D11_cei D1_cei D44_cei D43_cei D4_cei D5_cei D61_cei D62_cei D752_cei D75_cei D7_cei B2g_cei B3g_cei B5g_cei"
	foreach var in `cei17' {
		qui replace `var' = . if `var' != . & (iso == "CR") & year == 2017
	}
}

//2. Simplified/refined versions of some variables 
//2.1 National accounts 
if $aux_part == 2 {
	//Argentina (no inormation of social contributions)
	quietly replace pre_wag_nac = TOT_D1_R if iso == "AR" & year >= 2000 
	
	// Costa Rica's survey is net of employee contributions
	// but no info on contributions prior to 2012. Use 2012 info for 2000-2011
	quietly replace pre_wag_nac = TOT_D1_R * (1-0.27) if iso == "CR" ///
		& (inrange(year, 2000, 2011))
		
	// 2010-2018 surveys includes gross wages as well as net wages
	/* After 2012 take wages net only of employer contributions
	quietly replace pre_wag_nac = TOT_D1_R - (TOT_D61_U*0.6)  if iso == "CR" ///
		& year >= 2012 & year <=2014
	quietly replace pre_wag_nac = TOT_D1_R - (TOT_D61_U*0.58)  if iso == "CR" ///
		& year == 2015 
	quietly replace pre_wag_nac = TOT_D1_R - (TOT_D61_U*0.57)  if iso == "CR" ///
		& year >= 2016 
	*/
	
	//Chile, Bolivia and Ecuador report OS and MI in the same variable (OS)	
	quietly gen marker = 1 if missing(pre_mix_nac) & ///
		missing(pre_mir_nac) & !missing(pre_imp_nac) & ///
		inlist(iso, "BO", "CL") //"EC"
	quietly replace pre_mir_nac = pre_imp_nac if marker == 1 
	quietly replace pre_imp_nac = . if marker == 1 	 
}

//2.2 Surveys 
if $aux_part == 3 {
	//Chile, Bolivia and Ecuador report OS and MI in the same variable (OS)
	cap drop exception 
	qui gen exception = 1 ///
		if inlist(country, "BOL", "CHL") & !missing(series) // "ECU"
	qui la var exception ///
		"Tags when SNA reports Operating Surplus and Mixed Income together"		
	
	qui replace series = "series nº 200 - SNA93" ///
		if country == "CHL" & year == 2003	
		
	//Brazil does not have survey-imputed rents before 1996
	*quietly replace pre_mir = . ///
	*	if country == "BRA" & inrange(year, 1990, 1999)	
	
	
}

//II: Graphs -------------------------------------------------------------------

if $aux_part == 4 {

	//bring graph basic parameters
	global aux_part " "graph_basics" "
	quietly do "code/Do-files/auxiliar/aux_general.do"

	//per country settings
	global per_country_settings ///
		(line $var year if country == "ARG", ///
		lcolor($c_arg) lwidth(thick)) ///
		(line $var year if country == "BOL" , ///
		lcolor($c_bol) lwidth(thick)) ///
		(line $var year if country == "BRA" , ///
		lcolor($c_bra) lwidth(thick)) ///
		(line $var year if country == "CHL" , ///
		lcolor($c_chl) lwidth(thick)) ///
		(line $var year if country == "CRI" , ///
		lcolor($c_cri) lwidth(thick)) ///
		(line $var year if country == "COL", ///
		lcolor($c_col) lwidth(thick)) ///
		(line $var year if country == "ECU", ///
		lcolor($c_ecu) lwidth(thick)) ///
		(line $var year if country == "PER", ///
		lcolor($c_per) lwidth(thick)) ///
		(line $var year if country == "MEX", ///
		lcolor($c_mex) lwidth(thick)) ///
		(line $var year if country == "URY", ///
		lcolor($c_ury) lwidth(thick)) 	
		
	//per country settings (OS + MI exceptions)
	global per_country_settings_exep ///
		(line $var  year if country == "BOL", ///
		lcolor($c_bol) lwidth(thick)) ///
		(line $var year if country == "CHL", ///
		lcolor($c_chl) lwidth(thick)) ///
		(line $var  year if country == "ECU", ///
		lcolor($c_ecu) lwidth(thick)) 
			
		
	//per variable settings 
	global per_variable_settings (connected pre_wag year if !missing(pre_wag), ///
		lcolor($c_ury) msymbol(D) mfcolor($c_ury) mcolor($c_ury)) ///
		(connected pre_mix year if !missing(pre_mix), ///
		lcolor($c_col) msymbol(T) mfcolor($c_col) mcolor($c_col)) ///
		(connected pre_cap year if !missing(pre_cap), ///
		lcolor($c_chl) msymbol(T) mfcolor($c_chl) mcolor($c_chl)) ///
		(connected pre_ben year if !missing(pre_ben), ///
		lcolor($c_bra) msymbol(O) mfcolor($c_bra) mcolor($c_bra)) ///
		(connected pre_imp year if !missing(pre_imp), ///
		lcolor($c_per) msymbol(O) mfcolor($c_per) mcolor($c_per))

		*- CHANGE ON
	global per_variable_settings_e (scatter pre_wag year if extsna_pre_wag == 1, ///
		lcolor($c_ury) msymbol(D) mfcolor($c_ury) mcolor($c_ury)  mfcolor(${c_ury}*0.5)) ///
		(scatter pre_mix year if extsna_pre_mix == 1, ///
		lcolor($c_col) msymbol(T) mfcolor($c_col) mcolor($c_col)  mfcolor(${c_col}*0.5)) ///
		(scatter pre_cap year if extsna_pre_cap == 1, ///
		lcolor($c_chl) msymbol(T) mfcolor($c_chl) mcolor($c_chl)  mfcolor(${c_chl}*0.5)) ///
		(scatter pre_ben year if extsna_pre_ben == 1, ///
		lcolor($c_bra) msymbol(O) mfcolor($c_bra) mcolor($c_bra)  mfcolor(${c_bra}*0.5)) ///
		(scatter pre_imp year if extsna_pre_imp == 1, ///
		lcolor($c_per) msymbol(O) mfcolor($c_per) mcolor($c_per) mfcolor(${c_per}*0.5))
		*- CHANGE OFF
	
	//per variable settings (OS + MI exceptions)
	global per_variable_settings_exep ///
		(connected pre_wag year if !missing(pre_wag) /*& extsna_pre_wag != 1*/, ///
		lcolor($c_ury) msymbol(D) mfcolor($c_ury) mcolor($c_ury)) ///
		(connected pre_mir year if !missing(pre_mir) /*& extsna_pre_mir != 1*/, ///
		lcolor($c_ecu) msymbol(T) mfcolor($c_ecu) mcolor($c_ecu)) ///
		(connected pre_cap year if !missing(pre_cap) /*& extsna_pre_cap != 1*/, ///
		lcolor($c_chl) msymbol(T) mfcolor($c_chl) mcolor($c_chl)) ///
		(connected pre_ben year if !missing(pre_ben) /*& extsna_pre_ben != 1*/, ///
		lcolor($c_bra) msymbol(O) mfcolor($c_bra) mcolor($c_bra)) ///	
	
		*- CHANGE ON
	global per_variable_settings_exep_e ///
		(scatter pre_wag year if extsna_pre_wag == 1, ///
		lcolor($c_ury) msymbol(D) mfcolor($c_ury) mcolor($c_ury) mfcolor(${c_ury}*0.5)) ///
		(scatter pre_mir year if extsna_pre_mir == 1, ///
		lcolor($c_ecu) msymbol(T) mfcolor($c_ecu) mcolor($c_ecu) mfcolor(${c_ecu}*0.5)) ///
		(scatter pre_cap year if  extsna_pre_cap == 1, ///
		lcolor($c_chl) msymbol(T) mfcolor($c_chl) mcolor($c_chl) mfcolor(${c_chl}*0.5)) ///
		(scatter pre_ben year if  extsna_pre_ben == 1, ///
		lcolor($c_bra) msymbol(O) mfcolor($c_bra) mcolor($c_bra) mfcolor(${c_bra}*0.5)) ///	
		
		*- CHANGE OFF
		
	//legend countries (OS + MI exceptions)
	global legend_ctries_exep legend(label(1 "Bolivia") ///
		label(2 "Chile") label(3 "Ecuador")) 
			
	//legend variables 
	global legend_vars legend(label(1 "Wages") label(2 "Mixed Income") ///
		label(3 "Capital Income") label(4 "Social Benefits") ///
		label(5 "Imputed Rents") order(1 2 3 4 5))
		
	//legend variables (OS + MI exceptions)
	global legend_vars_exep legend(label(1 "Wages") ///
		label(2 "Mixed Inc. & Imp. Rents") ///
		label(3 "Capital Income") label(4 "Social Benefits") order(1 2 3 4)) 
		
		//legend variables 
	global legend_vars_esp legend(label(1 "Salarios") ///
		label(2 "Mixto") label(3 "Propriedad") ///
		label(4 "Transferencias") label(5 "Alquileres") ///
		order(1 2 3 4 5))
		
	//legend variables (OS + MI exceptions)
	global legend_vars_exep_esp legend(label(1 "Salarios") ///
		label(2 "Mixto y alquileres") ///
		label(3 "Propriedad") label(4 "Transferencias") ///
		order(1 2 3 4)) 	
		 
}	

//END OF SUB-CODE --------------------------------------------------------------
