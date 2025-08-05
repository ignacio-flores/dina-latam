//PRELIMINARY SETTINGS 
if $aux_part == "preliminary" {

	*DEFINE PARAMETERS*************************************************
	//mode 
	*global mode debug
	global mode full 
	
	//language
	*global lang " "esp" "
	global lang " "eng" "
	******************************************************************
	
	//first and last years for loops 
	if "${mode}" == "debug" {
		global all_countries " "ARG"  "
		global first_y = 2019
		global last_y = 2021
		/*
		global snapops totpop adults //active
		global unit_list " "ind" "pch" " // "act" "esn"
		global all_steps " "por" "psp" "raw" "bfm_norep_pre" "rescaled" "natinc" "pon" "pod"  " // "raw" "natinc" "psp" "rescaled" "pon" "pod"  "por" 
		global units_06a " "esn" "pch" "
		global steps_06b " "natinc" "psp" "
		global steps_06c raw bfm_norep_pre rescaled natinc
		global steps_06d natinc pon pod psp por
		*/
		global snapops totpop adults active 
 		global unit_list " "ind" "esn" "pch" "act" "  
		global all_steps ///
			"  "raw"  "natinc" "por"  "psp" "bfm_norep_pre" "rescaled" "pon" "pod" " 
		//	
		global units_06a " "esn" "pch" "	
		//"uprofits"
		global steps_dec ///
			" "raw" "bfm_norep_pre" "rescaled" "natinc" "pon" "pod" "psp"" //"uprofits"
		global steps_06b ///
			" "raw" "natinc" "bfm_norep_pre" "rescaled" "pon" "pod" " //"uprofits"  
		global steps_06c natinc raw rescaled bfm_norep_pre  //uprofits 
		global steps_06d natinc pon pod psp por 
	}
	
	//full mode 
	else {
		global all_countries  ///
		" "COL" "ARG" "PER" "URY" "CRI" "ECU" "CHL" "BRA" "SLV" "MEX" "DOM" " 
		global first_y = 2000
		global last_y = 2023
		global snapops totpop adults active 
 		global unit_list " "ind" "esn" "pch" "act" "  
		global all_steps ///
			"  "raw"  "natinc" "por"  "psp" "bfm_norep_pre" "rescaled" "pon" "pod" " 
		//	
		global units_06a " "ind" "esn" "pch" "act" "	
		//"uprofits"
		global steps_dec ///
			" "raw" "bfm_norep_pre" "rescaled" "natinc" "pon" "pod" "psp"" //"uprofits"
		global steps_06b ///
			" "raw" "natinc" "bfm_norep_pre" "rescaled" "pon" "pod" " //"uprofits"  
		global steps_06c natinc raw rescaled bfm_norep_pre  //uprofits 
		global steps_06d natinc pon pod psp por 
	}
	
	//total income variables for bfm 
	global y_postax_tot "ind_pos_tot" 
	global y_pretax_tot "ind_pre_tot" 
	global y_postax_tot_bra "ind_pos_totesn"
	global y_pretax_tot_bra "ind_pre_totesn"
	global y_postax_tot_per "ind_pos_totnb"
	global y_pretax_tot_per "ind_pre_totnb"
	global y_postax_tot_dom "ind_pos_totcor"

	//wage variables for bfm2stg
	global y_postax_wag "ind_pos_wag"
	global y_pretax_wag "ind_pre_wag"
	global y_pretax_wag_cri "ind_pre_wag_gross"
	global y_postax_formal_wage "ind_pos_fwag"
	global y_pretax_formal_wage "ind_pre_fwag"
	global y_pretax_formal_wage_cri "ind_pre_fwag_gross"
	global y_postax_private_wage "ind_pos_prwag"
	global y_postax_formal_private_wage_arg "ind_pos_prfwag"
	
	//wid variables (weird names)
	global inflation_wid inyixx
	global xppp_eur xlceup
	
	//get date
	local date "$S_DATE"
	local date = subinstr("`date'", " ", "", .)
	di as result "`date'"
	
	//paths
	global summary results/summary/
	global microfiles results/syn_microfiles/
	global efftaxes results/eff_taxes/
	global svypath Data/CEPAL/surveys/
	global taxpath Data/Tax-data/
	global auxpath code/Stata/auxiliar/
	global adofile code/Stata/ado_files/
	global all_thetas results/all_thetas.xlsx
	global pseudo_thetas results/pseudo_thetas.xlsx
	global mp_norep_pos results/bfm_norep_pos/merging_points.xlsx
	global mp_norep_pre results/bfm_norep_pre/merging_points.xlsx
	global w_adj ${taxpath}weight-adjusters/
	global sna_folder Data/national_accounts/
	global figs_cov figures/compare_steps/coverage/
	global figs_t figures/bfm_norep_pos/thetas_test/
	global figs_tail figures/upper_tail
	global figs_regi figures/region	
	global figs_decomp figures/decomposition
	global figs_gic figures/gic
	global figs_ratesca figures/eff_rates_sca
	global figs_expsca figures/exp_incidence_sca
	global inflation_data Data/infl_xrates_wid_wb.xlsx
	global export_wid Data/export_series/dina_latam_`date'.dta
	global export_wid_momo Data/export_series/dina_latam_`date'_amory.dta
	global export_wid_extrap Data/export_series/dina_latam_extrap_`date'.dta
	global export_wid_wide Data/export_series/dina_latam_wide_`date'.dta
	global wb_xrates Data/World_Bank/xrates/wb-xrates.xls 
	global sna_wid_merged Data/national_accounts/UNDATA-WID-Merged.dta
	global sna_wid_oecd Data/national_accounts/UNDATA-WID-OECD-Merged.dta
	global sna_wid_oecd_wb Data/national_accounts/UNDATA-WID-OECD-WB-Merged.dta
	global tax_comp Data/Tax-data/OECD-CIAT-CEPAL/
	global exports Data/Export_series/dina_latam_
	global popdata Data/Population/SurveyPop.dta	
	global ceq input_data/CEQ/
	//global tax_tots Data/national_accounts/OECD/tax-database.dta
	global govt_exp Data/expenditure_all_countries.dta
	
	//Paths to export graphs 
	global figs_path "figures/`type'/snacompare"
	
	//lists of countries 
	global areas_wid_latam  ///
		" "AR" "BO" "BR" "CL" "CO" "CR" "DO" "EC" "GT" "HN" "MX" "NI" "PA" "PE" "PY" "SV" "VE" "UY" "
	global extra_count " "BOL" "GTM" "HND" "NIC" "PAN" "VEN" "	
	global really_all_countries " ${all_countries} ${extra_count} " 		
	global ctries_cei " "PER" "DOM" "URY" "CRI" "BRA" "CHL" "COL" "ECU" "MEX" "    	 

	//list of countries with tax data
	global countries_tax " "DOM"  "BRA" "CHL" "ARG" "COL" "PER"  "URY" "MEX" "SLV" "CRI" "ECU" "
		
	//BFM 
	global countries_bfm_02a " "COL" "URY" "BRA" "DOM" "SLV" "ECU" "PER" "CHL" "
	global countries_2stage " "ARG" "CRI" "MEX" " // "CHL" 
	global t_limit = 10 
		
	//normal / exceptional countries, with respect to SNA aggregation 	
	global norm_countries " "MEX" "BRA" "COL" "DOM" "SLV" "PER" "URY" "CRI" "PRY" " //"ARG"
	global exep_countries " "CHL" "BOL" " // "ECU"  
	global scal_list "  "pre_ben" "pre_cap" "pre_imp" "pre_mix" "pre_mir" "pre_wag" "
	
	global oecd_taxes pit cit prl imo wea est otp gog goo oth
	
	// Define list of variables (components) for each step and unit 
	
	*list components of income 
	local normvarlist "wag pen cap imp mix"
	local exepvarlist "wag pen cap mir"
	
	*list variables for each step/unit
	global unit_list2 " "esn" "ind" "esb" "pch" "
	foreach uni in $unit_list2 {
		foreach g in "norm" "exep" { 
			*list for raw and bfm
			foreach s in "raw" "bfm" "res" {
				*loop over variables 
				foreach v in ``g'varlist' {	
					*define suffix  
					if inlist("`s'", "raw", "bfm") local sf ""
					if inlist("`s'", "res", "upr") local sf "_sca"
					*list variables 
					if "`uni'" == "ind" & "`v'" != "pen"  {			
						local act_`s'_`g' ///
							"`act_`s'_`g'' `uni'_pre_`v'`sf'"
						if "`s'" == "raw" {
							local act_por_`g' ///
								"`act_por_`g'' `uni'_pos_`v'`sf'"		
						}	
					}
					
					*add variable to the list 
					local `uni'_`s'_`g' "``uni'_`s'_`g'' `uni'_pre_`v'`sf'" 
					if "`s'" == "raw" {
						local `uni'_por_`g' "``uni'_por_`g'' `uni'_pos_`v'`sf'" 
					}
				} 
				
				*define global using local 
				global `uni'_`s'_`g' ``uni'_`s'_`g''
				di as result "`uni'_`s'_`g': ${`uni'_`s'_`g'}"
				if "`s'" == "raw" {
					global `uni'_por_`g' ``uni'_por_`g''
					di as result "`uni'_por_`g': ${`uni'_por_`g'}"
				}
				
				if "`uni'" == "ind" {
					*Define as a global 
					global act_`s'_`g' `act_`s'_`g''
					di as result "act_`s'_`g': ${act_`s'_`g'}"
					if "`s'" == "raw" {
						global act_por_`g' `act_por_`g''
						di as result "act_por_`g': ${act_por_`g'}"
					}
				}
				
				*Add uprofits and other incomes 
				if "`s'" == "res" {
					global `uni'_upr_`g' ${`uni'_`s'_`g'} `uni'_upr_2 
					global `uni'_nat_`g' ${`uni'_upr_`g'} ///
						`uni'_tax_indg_pre `uni'_pre_lef
					global `uni'_pod_`g' `uni'_pre_mbe_sca `uni'_pod_tot_wmbe	
					global `uni'_psp_`g' `uni'_pre_mbe_sca `uni'_psp_tot_wmbe	
					global `uni'_pon_`g' ${`uni'_pod_`g'} ///
						`uni'_pon_hea `uni'_pon_edu `uni'_pon_oex
					if "`uni'" == "ind" {
						global act_upr_`g' ${act_`s'_`g'} `uni'_upr_2 
						global act_nat_`g' ${act_upr_`g'} ///
							`uni'_tax_indg_pre `uni'_pre_lef
						global act_pod_`g' `uni'_pre_mbe_sca `uni'_pod_tot_wmbe
						global act_psp_`g' `uni'_pre_mbe_sca `uni'_psp_tot_wmbe
						global act_pon_`g' ${act_pod_`g'} ///
							`uni'_pon_hea `uni'_pon_edu `uni'_pon_oex
					}	
				} 
			}
		}
	}
	
	//create main folders 
	local dirpath "intermediary_data"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	
	//create main folders 
	local dirpath "intermediary_data/microdata"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	
	// Create directory if it doesnt exist 
	local dirpath "output"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	
	// Create directory if it doesnt exist 
	local dirpath "output/figures"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	
	
	//country-years for 2 stage bfm correction
	global years_ARG "2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015"
	global years_CRI "2010 2011 2012 2013 2014 2015 2016"
	global years_CHL "2000 2001 2002 2003 2004 " //2005 2006 2007 2008 2009
	global years_MEX " 2010 2012 2014"
	global years_SLV "2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017"
	
	*isos for latam in WID
	global isos_latam_wid (iso=="AG" | iso=="AI" | iso=="AR" | iso=="AW" | iso=="BB" ///
	| iso=="BL" | iso=="BO" | iso=="BR" | iso=="BS" | iso=="BQ" | iso=="BZ" ///
	| iso=="CL" | iso=="CO" | iso=="CR" | iso=="CU" | iso=="CW" | iso=="DM" ///
	| iso=="DO" | iso=="EC" | iso=="FK" | iso=="GD" | iso=="GP" | iso=="GT" ///
	| iso=="GY" | iso=="HN" | iso=="HT" | iso=="JM" | iso=="KN" | iso=="KY" ///
	| iso=="LC" | iso=="MF" | iso=="MQ" | iso=="MX" | iso=="NI" | iso=="PA" ///
	| iso=="PE" | iso=="PR" | iso=="PY" | iso=="SR" | iso=="SV" | iso=="SX" ///
	| iso=="TC" | iso=="TT" | iso=="VC" | iso=="VE" | iso=="VG" | iso=="VI" ///
	| iso=="UY")
	
	
	//country long names 
	global lname_ARG_eng "Argentina"
	global lname_BOL_eng "Bolivia"
	global lname_BRA_eng "Brazil" 
	global lname_CHL_eng "Chile" 
	global lname_COL_eng "Colombia" 
	global lname_CRI_eng "Costa Rica"
	global lname_ECU_eng "Ecuador"
	global lname_PRY_eng "Paraguay"
	global lname_PER_eng "Peru"
	global lname_MEX_eng "Mexico"
	global lname_URY_eng "Uruguay"
	global lname_SLV_eng "El Salvador"
	global lname_VEN_eng "Venezuela"
	global lname_DOM_eng "Dominican Republic"
	

}

//GRAPH SETTINGS 
if ($aux_part == "graph_basics") {

	//country colors
	global c_arg "eltblue"
	global c_bol "eltgreen"
	global c_bra "midgreen"
	global c_chl "cranberry"
	global c_col "gold"
	global c_cri "purple"
	global c_ecu "stone"
	global c_dom "black"
	global c_pry "lavender"
	global c_per "gs7"
	global c_mex "dkgreen"
	global c_ury "ebblue"
	global c_slv "maroon"
	global c_ven "red"
	global c_nic "orange"
	global c_gtm "gs15"
	global c_hnd "sienna"
	
	//variable colors 
	global c_kap "cranberry*0.3"
	global c_cap "cranberry"
	global c_wag "ebblue" 
	global c_ben "midgreen"
	global c_pen "midgreen*0.5"
	global c_mix "gold" 
	global c_mir "stone" 
	global c_imp "ebblue*.5" 
	global c_upr "cranberry*0.5"
	global c_lef "gray*.3"
	global c_indg "gray*0.7"
	global c_mbe "dkgreen*.8"
	global c_wmbe "gray*.5"
	
	//step colors 
	global c_urb "gs9"
	global c_raw "cranberry*.25"
	global c_bfm "cranberry*.5"
	global c_res "cranberry*.75"
	global c_upr2 "erose" // cranberry*0.5
	*global c_upr "dkgreen"
	global c_nat "cranberry"
	global c_pon "navy"
	global c_pod "navy*.75"
	global c_psp "navy*.5"
	global c_por "navy*.25"
	
	//colors for taxes
	*on personal income 
	global c_1000 "ebblue"
	global c_1100 "ebblue*.8"
	global c_1110 "ebblue*.8"
	global c_1120 "ebblue*.4"
	*on corporate income 
	global c_1200 "cranberry*.55"
	global c_1300 "cranberry*.4"
	*ssc
	global c_2000 "midgreen*.7"
	global c_2100 "dkgreen*.8"
	global c_2200 "dkgreen*.6"
	global c_2300 "dkgreen*.4"
	global c_2400 "dkgreen*.2"
	*payroll and workforce
	global c_3000 "midgreen*0.4"
	*property 
	global c_4000 "cranberry*.9"
	global c_4200 "cranberry*1"
	global c_4100 "cranberry*.85"
	global c_4300 "cranberry*.7"
	global c_4999 "cranberry*.25"
	global c_4400 "cranberry*.4"
	global c_4500 "cranberry*.2"
	global c_4600 "cranberry*.1"
	
	*goods and services 
	global c_5000 "black*.5"
	global c_5100 "black*.5"
	global c_5110 "black*.3"
	global c_5999 "black*.15"
	*other 
	global c_6000 "black*.5"

	//s1 scheme colors
	global color_1 	"dkgreen"
	global color_2 	"orange_red"
	global color_3 	"navy"
	global color_4 	"maroon"
	global color_5 	"teal"
	global color_6 	"orange"
	global color_7 	"magenta"
	global color_8 	"cyan"
	global color_9 	"ref"
	global color_10 "lime"	
	
	// decomposition colours
	global c_wit	"lavender"
	global c_bet	"sienna"
	global c_top1	"red"
	global c_bot99	"orange"
	
	
	//country long names 
	global lname_arg "Argentina"
	global lname_bol "Bolivia"
	global lname_bra "Brazil" 
	global lname_chl "Chile" 
	global lname_col "Colombia" 
	global lname_cri "Costa Rica"
	global lname_ecu "Ecuador"
	global lname_pry "Paraguay"
	global lname_per "Perú"
	global lname_mex "Mexico"
	global lname_ury "Uruguay"
	global lname_slv "El Salvador"
	global lname_ven "Venezuela"
	global lname_dom "República Dominicana"
	
	//group labels 
	global lname_t1  "Top 1%"
	global lname_t10 "Top 10%"
	global lname_m40 "Middle 40%"
	global lname_b50 "Bottom 50%"
 	
	//variable labels (english)
	global labcom_cap_eng "Property income"
	global labcom_mix_eng "Mixed income"
	global labcom_kap_eng "Capital & GOS"
	global labcom_imp_eng "Imputed Rents"
	global labcom_wag_eng "Wages"
	global labcom_ben_eng "Pensions & benefits"
	global labcom_pen_eng "Pensions"
	global labcom_mir_eng "Mixed & GOS"
	global labcom_upr_eng "Und. profits"
	global labcom_indg_eng "Taxes on prod."
	global labcom_lef_eng "Other"
	global labcom_mbe_eng "Monetary Benefits"
	global labcom_wmbe_eng "Post-tax Market inc."
	global labcom_hea_eng "Health gov. expenditure"
	global labcom_edu_eng "Education gov. expenditure"
	global labcom_oex_eng "Other gov. expenditure"

	//variable labels (español)
	global labcom_cap_esp "Capital"
	global labcom_mix_esp "Mixto"
	global labcom_imp_esp "Alquileres"
	global labcom_wag_esp "Salarios"
	global labcom_ben_esp "Transferencias"
	global labcom_pen_esp "Pensiones"
	global labcom_mir_esp "Mixto & alquil."
	global labcom_upr_esp "Gan. retenidas"
	global labcom_indg_esp "Impuestos indir."
	global labcom_lef_esp "Otros"
	
	//step labels (english)
	global lab_urb_eng "Urban areas"
	global lab_raw_eng "Raw Survey"
	global lab_bfm_eng "Corrected Survey"
	global lab_res_eng "Scaled Household Inc."
	global lab_upr_eng "Sca. + Undis. Profits"
	global lab_nat_eng "Pretax National Inc."
	global lab_pod_eng "Postax disposable"
	global lab_psp_eng "Postax spendable"
	global lab_pon_eng "Postax national"
	global lab_por_eng "Postax raw"
	
	//step labels (español)
	global lab_urb_esp "Areas urbanas"
	global lab_raw_esp "Encuesta CEPAL"
	global lab_bfm_esp "Encuesta + DGII"
	global lab_res_esp "Escalado sec. hogares"
	global lab_upr_esp "+ Util .retenidas"
	global lab_nat_esp "Ingreso Nacional"
	
	
	//labels for taxes (english)
	*on income
	global lab_1000_eng "On income"
	global lab_1100_eng "Personal inc."
	global lab_1110_eng "Pers. inc"
	global lab_1120_eng "Pers. cap. gains"
	global lab_1200_eng "Corporate inc."
	global lab_1300_eng "Other inc. tax"
	*ssc
	global lab_2000_eng "SSC"
	global lab_2100_eng "SSC: Employees"
	global lab_2200_eng "SSC: Employers"
	global lab_2300_eng "SSC: Self emp"
	global lab_2400_eng "SSC: Other"
	*payroll
	global lab_3000_eng "Payroll taxes"
	*property
	global lab_4000_eng "On property"
	global lab_4100_eng "Imov. property"
	global lab_4200_eng "Wealth tax"
	global lab_4300_eng "Estate & inherit"
	global lab_4999_eng "Other on property"
	global lab_4400_eng "Fin. transactions"
	global lab_4500_eng "Non-recurrent"
	global lab_4600_eng "Other on prop"
	*on goods and products 
	global lab_5000_eng "On production"
	*global lab_5100_eng "Sales taxes"
	global lab_5111_eng "VAT"
	global lab_5110_eng "Gral. sale taxes"
	global lab_5999_eng "Other on prod."
	*other 
	global lab_6000_eng "Other"
	global lab_7000_eng "Other"
	
	*more on taxes 
	global lab_tot "Total taxes"
	global lab_pit_tot "Total property and income taxes"
	global lab_pit_corp "Corporate property and income taxes"
	global lab_pit_hh "Households' property and income taxes"
	global lab_ssc "Social security contributions"
	global lab_indg "Indirect taxes on goods and services, gross of subsidies"
	
	*even more on taxes 		
	global labtax_pit "Pers. inc. tax"		
	global labtax_cit "Corp. inc. tax"		
	global labtax_prl "Payroll"		
	global labtax_imo "Immov. property"		
	global labtax_wea "Wealth tax"		
	global labtax_est "Estate, inherit. & gifts"		
	global labtax_otp "Other on property"		
	global labtax_gog "Gral. on goods & serv."		
	global labtax_goo "Other goods & serv."		
	global labtax_oth "Other taxes"
	
	*and even more 
	global c_wea "cranberry*.8"
	global c_cit "cranberry"
	global c_imo "cranberry*.2"
	global c_prl "ebblue*.5"
	global c_pit "ebblue*.8"
	global c_goo "black*.2"
	global c_gog "black*.3"
	global c_est "cranberry*.4"
	global c_otp "cranberry*.6"
	global c_oth "black*.1"
	
	*label etr 
	global etr_indg "Indirect"
	global etr_corp "On inc. & prop. (corp)"
	global etr_pih "On inc. & prop. (hlds)"
	global indg_c_etr "black*.3"
	global corp_c_etr "maroon*.9"
	global pih_c_etr "ebblue*0.8"
	
	*label exr 
	global exr_oex "Other in-kind"
	global exr_hea "Health"
	global exr_edu "Education"
	global oex_c_exr "black*.3"
	global hea_c_exr "maroon*.9"
	global edu_c_exr "ebblue*0.8"
	
	
	//axis label options 
	global ylab_opts labsize(medium) grid labels angle(horizontal)
	global xlab_opts labsize(medium) grid labels angle(horizontal)
	global xlab_opts labsize(medium) grid labels angle(45)

	
	//axis label options 
	global ylab_opts_white labsize(medium) angle(horizontal)
	*global xlab_opts_white labsize(medium) angle(horizontal)
	global xlab_opts_white labsize(medium) angle(45)
	
	//last bit of a graph
	global graph_scheme scheme(s1color) subtitle(,fcolor(white) ///
	lcolor(bluishgray)) graphregion(color(white)) ///
	plotregion(lcolor(bluishgray)) scale(1.2)
	
}

//GET LIST OF CASES WITH SURVEY-TAX DATA OVERLAP
if ($aux_part == "tax_svy_overlap") {
	preserve
		foreach x in "" "_2stage" { 
			foreach z in "pos" "pre" {	
				//bring list of overlaps 
				cap import excel "${mp_norep_`z'}", ///
					sheet("country_years`x'") firstrow clear		
				if !_rc {
					//save if it exists 
					quietly ds 
					quietly split `r(varlist)', parse(_) gen(v)	
					
					//exceptions 
					quietly drop if v1 == "CHL" & inlist(v2, "2000", "2003")
					quietly drop if v1 == "ECU" & inlist(v2, "2011")
					
					//list years 
					quietly levelsof v1, local(countries`x'_`z') clean
					foreach c in `countries`x'_`z'' {
						if ("`c'_years" != "") local `c'_first_list ``c'_years'
					
						quietly levelsof v2 if v1 == "`c'", local(`c'_years) clean
						if ("``c'_first_list'" != "") local `c'_years "``c'_years' ``c'_first_list'"
						global `c'_overlap_years ``c'_years'
						di as result "`c': ${`c'_overlap_years}"
					}
				}
			}
		}
	restore
	
	foreach c in `countries_pos' `countries_pre' `countries_2stage_pos' `countries_2stage_pre' {
		display as text "`c': ${`c'_years}"	
		local all_overlaps "`all_overlaps' "`c'" "
	}
	global overlap_countries " `all_overlaps' "
}

//EXCHANGE RATES TO GET LCU C URRENCIES 
if ($aux_part == "old_currencies") {
	
		*list all currencies to change 
		global old_currencies ARG_aus BRA_crr BRA_cru VEN_bol ///
			SLV_col URY_npe MEX_npe 
 		
		*arg (austral to peso)
		global xr_ARG_aus = 1/10000  
		global yr_ARG_aus = 1992
		
		*bra (cruzeiro real to real)
		global xr_BRA_crr = 1/2750
		global yr_BRA_crr = 1994 

		*bra (cruzeiro to cruzeiro real)
		global xr_BRA_cru = 1/1000
		global yr_BRA_cru = 1993 // originally 1993
		
		*ven (bolivar to bolivar fuerte)
		global xr_VEN_bol = 1/1000
		global yr_VEN_bol = 2008
		
		*slv (colones to dollar) 
		global xr_SLV_col = 1/8.75 
		global yr_SLV_col = 2001 
		
		*ury (nuevos pesos to peso uruguayo)
		global xr_URY_npe = 1/1000
		global yr_URY_npe = 1993 // originally 1993
		
		*mex (nuevos pesos to peso)
		global xr_MEX_npe = 1/1000
		global yr_MEX_npe = 1993
		
} 

//GET LIST OF CASES TO EXTRAPOLATE BFM CORRECTION 

if ($aux_part == "list_bfm_extrap") {

	//Get list of countries and years to adjust
	preserve 
		quietly import excel "${w_adj}index.xlsx", ///
			sheet("country_years_03e") firstrow clear
		quietly split country_year, parse(_) gen(v)	
		quietly levelsof v1, local(countries) clean
		global extrap_countries `countries'
		global extrap_countries `countries'
		foreach c in $extrap_countries {
			quietly levelsof v2 if v1 == "`c'", local(`c'_years) clean
			global `c'_extrap_years ``c'_years'
		}
	restore 
	
}

//SELECT COUNTRIES FOR WHICH WE USE AVERAGE SCALING FACTORS (& OTHER)

if ($aux_part == "aver_countries") {

	global fix_countries DOM CRI URY SLV ARG BRA CHL ECU PER MEX COL 

}
