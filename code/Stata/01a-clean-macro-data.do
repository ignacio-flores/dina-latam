//This do file uses SNA data scrapped from http://data.un.org/ (in section 1.1)
//Gov. Balance Sheets from http://data.imf.org/ (in section 1.3)
//using "R/download-raw-un-sna.R", which then was
//cleaned using "R/import-un-sna-data.R"

//0. PRELIMINARY ------------------------------------------------------------//

//General 
clear all
sysdir set PERSONAL "."
clear programs

//preliminary
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"

//Table names
local TOT 		"Table 4.1 Total Economy (S.1)"
local RoW 		"Table 4.2 Rest of the world (S.2)"
local NFC 		"Table 4.3 Non-financial Corporations (S.11)"
local FC 		"Table 4.4 Financial Corporations (S.12)"
local GG 		"Table 4.5 General Government (S.13)"
local HH 		"Table 4.6 Households (S.14)"
local NPISH 	"Table 4.7 Non-profit institutions serving households (S.15)"
local corps 	" Non-Financial and Financial Corporations (S.11 + S.12)"
local CORPS 	"Table 4.8 Combined Sectors`corps'"
local HH_NPISH 	"Table 4.9 Combined Sectors Households and NPISH (S.14 + S.15)"
local all_IS 	"TOT RoW NFC FC GG HH NPISH CORPS HH_NPISH"

//1. PREPARE AND CLEAN DATA -------------------------------------------------//

//1.1 UNDATA ----------------------------------------------------------------// 

local iter = 1
tempfile tf_merge1
foreach IS in `all_IS' {
	tempvar auxi1 auxi2
	qui use "input_data/sna_UNDATA/_clean/``IS''.dta", clear

	//Items & codes
	qui rename sna93_item_code i_code
	qui replace i_code = subinstr(i_code, ".", "",.) 
	qui replace i_code = subinstr(i_code, "*", "",.) 
	qui split sub_group, parse(-)
	qui gen sg2 = substr(sub_group2,2,1) 
	qui replace sg2 = "L" if strpos(sub_group2, "liabilities")
	qui replace sg2 = "A" if strpos(sub_group2, "assets")
	qui egen `auxi1' = concat(i_code sg2), punct(_)

	//Check for items with same code
	qui sort iso year series `auxi1'
	qui by iso year series `auxi1':  gen dup = cond(_N==1,0,_n)
	qui egen `auxi2' = concat(`auxi1' dup) if dup > 0
	qui replace `auxi2' = `auxi1' if dup == 0
	qui replace `auxi2' = subinstr(`auxi2', "-", "",.) 
	qui levelsof `auxi2', local(vars)
	
	//Get labels
	qui egen item_lab = concat(sub_group2 item), punct(", `IS' (UN-DATA): ")
	qui levelsof `auxi2', local(lab_items)
	foreach i in `lab_items' {
		qui levelsof item_lab if `auxi2' == "`i'", local(lab_item_`i') clean 
	}

	//Reshape
	qui keep iso year series `auxi2' value
	qui reshape wide value, i(iso year series) j(`auxi2') string
	qui rename value* `IS'_*
	foreach i in `lab_items' {
		qui label var `IS'_`i' "`lab_item_`i''"
	}
	
	//Save and merge
	if (`iter' == 0) {
		qui mer 1:m iso year series using "`tf_merge1'", nogenerate 
	}
	local iter = 0
	qui save `tf_merge1', replace 
}
qui sort iso series year
qui kountry iso, from(iso2c) to(iso3c) geo(undet)

//Net balance of primary incomes for various sectors
foreach IS in "FC" "NFC" "TOT" "CORPS" {
	local s1 "1"
	local s2 ""
	if ("`IS'" == "TOT") {
		local s1 "U"
		local s2 "U"
	}
	qui gen `IS'_B5n_`s1' = `IS'_B5g_`s1' - `IS'_K1_`s2'
}

//Keep only LATAM now	
qui keep if inlist(GEO, "Caribbean", "South America", "Central America")
qui egen ctry_srs = concat(iso series)
qui encode ctry_srs, gen(ctry_srs_n)
qui sort iso series year
qui xtset ctry_srs_n year 

//Fill missing values and special cases [NEW CODE]
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

//Save
tempfile tf_main 
qui save `tf_main', replace 
//qui save "Data/national_accounts/UNDATA-Merged.dta", replace

//1.1.1 wid.world data -------------------------------------------------------// 

//define varlist
global widvars mnninc mgdpro mnnfin mptfrr mptfrp inyixx npopul ///
	mccshn mccmhn mcfcco mconfc mptfhr mgsmhn mgsrhn mgmxhn mprgco agninc
clear

// Download net national income figures (constant local currency) 
qui wid, indicators(${widvars}) areas(${areas_wid_latam}) ages(999 992) clear
qui rename country iso 

//rename 
qui kountry iso, from(iso2c) to(iso3c) geo(undet)
drop if missing(_ISO3C_)
qui keep iso _ISO3C_ year variable value 
qui rename _ISO3C_ country 
qui order country year 	
	
//reshape 
reshape wide value, i(country year) j(variable) string	

//rename main variables 
qui rename value* *
qui rename (agninc992i agninc999i) (agninc_adults agninc_totpop)
qui rename *999i * //these are all macro variables (defined as 999 = total pop)
qui rename *992i *_adults
qui rename (mnninc mconfc inyixx) (TOT_B5n_wid TOT_K1_wid priceindex)
qui drop *99?f *99?m

//Rename other variables
qui rename (mgdpro mnnfin mptfrr mptfrp mccshn mccmhn mcfcco ///
	mptfhr mgsmhn mgsrhn mgmxhn mprgco) ///
		(gdp_wid nfi re_portf_inv_rec re_portf_inv_paid cfc_hh_surplus ///
		cfc_hh_mixed cfc_corp y_cap_tax_havens y_gos_gmix_hh ///
		y_gos_hh y_gmix_hh bpi_corp_wid)		
		

//compute current gross national income
qui gen TOT_B5g_wid = TOT_B5n_wid + TOT_K1_wid
foreach v in TOT_B5g_wid TOT_B5n_wid TOT_K1_wid gdp_wid {
	qui replace `v' = `v' * priceindex
}

//
qui egen cfc_hh = rowtotal(cfc_hh_surplus cfc_hh_mixed)
qui gen foreign_up_corp = re_portf_inv_rec - re_portf_inv_paid

//label variables 
qui label var gdp_wid "gross domestic product"
qui label var nfi "net foreign income"
qui label var TOT_B5g_wid "gross national income"
qui label var cfc_hh_mixed "personal depreciation on mixed income"
qui label var cfc_hh "consumption of fixed capital of households"
qui label var cfc_corp "consumption of fixed capital of corporations"
qui label var TOT_K1_wid "consumption of fixed capital of the total economy"
qui label var y_cap_tax_havens "capital income received from tax havens"
qui label var y_gos_hh "gross operating surplus of households"
qui label var y_gmix_hh "gross mixed income of households"
qui label var bpi_corp_wid "balance of primary incomes of corporations (wid)"
qui label var re_portf_inv_rec ///
	"reinvested earnings on foreign portfolio investment (received)"
qui label var re_portf_inv_paid ///
	"reinvested earnings on foreign portfolio investment (paid)"	
qui label var foreign_up_corp ///
	"net foreign reinvested earnings on portfolio investment"	
qui label var y_gos_gmix_hh ///
	"gross operating surplus and mixed income of households"
qui label var cfc_hh_surplus ///
	"personal depreciation on operating surplus"
	
//Express as shares of target total
qui gen sh_bpi_corp_for = foreign_up_corp / bpi_corp_wid
qui la var sh_bpi_corp_for ///
	"net foreign reinvested earnings on portfolio investment (% of Corp Und. Profits)" 
qui gen sh_cfc_corp = cfc_corp / bpi_corp_wid
qui la var sh_cfc_corp ///
	"consumption of fixed capital of corporations (% of Corp Und. Profits)"
qui gen sh_cfc_hh_surplus = cfc_hh_surplus / y_gos_hh
qui la var sh_cfc_hh_surplus ///
	"Depreciation on operating surplus, HH (% of gross value)"
qui gen sh_cfc_hh_mixed = cfc_hh_mixed / y_gmix_hh
qui la var sh_cfc_hh_mixed ///
	"Depreciation on mixed income (% of gross value)"
qui gen sh_cfc_hh = cfc_hh / y_gos_gmix_hh
qui la var sh_cfc_hh ///
	"Consumption of fixed capital of households (% of MI + OS_HH)"
****ESTO ES MUY IMPORTANTE. AQUI!
qui gen sh_cfc_total = TOT_K1_wid / TOT_B5g_wid
qui la var sh_cfc_total ///
	"Total Consumption of fixed capital (% of Gross National Income)"		

qui merge 1:m iso year using "`tf_main'", nogenerate
qui save `tf_main', replace
//qui save "Data/national_accounts/UNDATA-WID-Merged.dta", replace 

//1.1.2 add some other UN-data for comparison ---------------------------------// 

//Total gross corporate uprofits 
qui gen bpi_corp_tot = CORPS_B5g_1
qui la var bpi_corp_tot ///
	"Corporate undistributed profits, current LCU (undata)"
	
//Government share of corporte primary income
qui gen sh_bpi_corp_gg = GG_D4_R / TOT_D4_R
qui la var sh_bpi_corp_gg ///
	"Government share of corporate undistributed profits, current LCU"	
		
//net corporate profits 
qui gen bpi_corp_tot_net = bpi_corp_tot * (1 - sh_cfc_corp)
qui la var bpi_corp_tot_net ///
	"Corporate undistributed profits, net of depreciation, LCU"
	
//government share of corporate profits 
qui gen bpi_corp_gg = bpi_corp_tot_net * (sh_bpi_corp_gg)
qui la var bpi_corp_gg ///
	"Government undistributed profits, net of depreciation, LCU"
	
//government net property income
qui gen prop_inc_net_gg = GG_D4_R - GG_D4_U
qui la var prop_inc_net_gg ///
	"Government net property income"

//net private national corporate profits:
	//in LCC
	qui gen bpi_corp_hh_net = ///
		bpi_corp_tot_net * (1 - sh_bpi_corp_for - sh_bpi_corp_gg)
	qui la var bpi_corp_hh_net ///
		"Net National private corp. undistributed profits, LCU"
	//as % of uprofits
	qui gen sh_bpi_corp_hh = bpi_corp_hh_net / bpi_corp_tot
	qui la var sh_bpi_corp_hh ///
		"Net National private corp. UP, % of Total UP"	
	assert sh_bpi_corp_hh <= 1 if !missing(sh_bpi_corp_hh)		
	//as % of GNI
	qui gen uprofits_hh_ni = (bpi_corp_hh_net / TOT_B5g_U) * 100
	qui la var uprofits_hh_ni ///
		"Net national private corp. undistributed profits, % of GNI"
	

//harmonize country names
qui rename (_ISO3C_ GEO) (iso3c geo)
qui kountry iso, from(iso2c) to(iso3c) geo(undet)

//Save
qui save "intermediary_data/national_accounts/UNDATA-WID-Merged.dta", replace 

//1.2 Merge with CEDLAS (TEMPORARY CODE) -------------------------------------//
/*
	qui rename iso country_iso
	qui merge m:1 country_iso year using ///
		"Data/SEDLAC/CEDLAS data/tot_income_survey.dta" ///
		, nogenerate //keep(match master)
	qui rename country_iso iso	

	//Compare Disp. Inc. of Households in SEDLAS to SNA
	foreach gn in "g" "n" {
		//HH in SNA as % of NI
		qui gen HH_DispInc_`gn' = HH_B6g_U / TOT_B5`gn'_U * 100 
		qui replace HH_DispInc_`gn' = HH_NPISH_B6g_U / TOT_B5`gn'_U * 100 ///
			if  missing(HH_B6g_U) & !missing(HH_NPISH_B6g_U)
		//HH in Svy as % of National Income
		qui gen Svy_NI_`gn' = tot_inc_survey / TOT_B5`gn'_U * 100
	}
	
	// Auxiliary variables for map
	preserve	
		//General 
		foreach name in "tot_inc_survey" "TOT_B5g_U" "HH_B6g_U" "TOT_B5g_wid"{
			qui gen 	data_`name'=0
			qui replace data_`name'=1 if !missing(`name')
		}
		qui gen data_tax=0

			foreach yr of numlist 1997/2004 {
				qui replace data_tax=1 if iso=="AR" & year==`yr'
			}
			foreach yr of numlist  1996/1998 2002 2006/2016 {
				qui replace data_tax=1 if iso=="BR" & year==`yr'
			}
			foreach yr of numlist 1990/2017 {
				qui replace data_tax=1 if iso=="CL" & year==`yr'
			}
			foreach yr of numlist 1992/2016 {
				qui replace data_tax=1 if iso=="CO" & year ==`yr'
			}
			foreach yr of numlist 2012/2017 {
				qui replace data_tax=1 if iso=="CR" & year==`yr'
			}
			foreach yr of numlist 2009/2014 {
				qui replace data_tax=1 if iso=="MX" & year==`yr'
			}
			foreach yr of numlist 2009/2015 {
				qui replace data_tax=1 if iso=="UY" & year==`yr'
			}

		//keep latam 
		qui keep if inlist(GEO, "Caribbean", "South America", "Central America")
			
		//Keep most recent series by year
		qui egen country_year = concat(iso year)
		qui levelsof iso, local(all_ctries)
		foreach c in `all_ctries' {
			qui levelsof country_year if iso == "`c'", local(ctry_yrs_`c')
			foreach cy in `ctry_yrs_`c'' {
				qui sum series if country_year=="`cy'"
				qui drop if country_year=="`cy'" & series!=r(max)
			}		
		}
		qui save "Data/national_accounts/sna_svy_tax_aux.dta", replace 
	restore 
	
	//as Local Currency Units (LCU)
	qui gen HH_DispInc_LCU_g = HH_B6g_U  
	qui replace HH_DispInc_LCU_g = HH_NPISH_B6g_U ///
		if missing(HH_B6g_U  ) & !missing(HH_NPISH_B6g_U)	
	//as % of Household Income in SNA	
	qui gen Svy_HHDI_g = tot_inc_survey / HH_DispInc_LCU_g * 100

qui save `tf_main', replace 
*/

//1.3. BALANCE SHEETS -------------------------------------------------------//

//1.3.1 IMF Data (Government Finance Statistics)-----------------------------//

preserve
	//Import financial balance sheets of general gov
	import excel "input_data/balance_sheet/IMF/Balance_Sheet_Stock_GG.xlsx" ///
		, sheet("Integrated Balance Sheet (Stoc") ///
		cellrange(A2:W515) firstrow clear
		
	//Lists	
	local ordr "country year item A_stock L_stock"
	ds, not(varlabel)
	local aux_list `r(varlist)'
		
	//Rename some variables
	qui rename Stockpositionliabilities L_stock
	qui rename Stockpositionfinancialassets	A_stock

	//check all variables are in same unit (millions or billions)	
	tempvar aux1 
	qui egen `aux1' = concat(`aux_list')
	foreach unit in "M" "B" {
		tempvar aux_`unit'
		qui gen `aux_`unit'' = strpos(`aux1', "`unit'") > 0
	}
	qui count if (`aux_M' + `aux_B' > 1)
	if r(N) != 0 {
		display as text "Gov financial balance sheets: " ///
			"Some variables not expressed in same unit"
		exit 1 
	}
	else {
		display as text "Gov financial balance sheets: " ///
			"All countries have variables in same unit"
	}

	//Clean
	qui keep `ordr' 

	//Reshape
	qui encode item, gen(item_n)
	qui levelsof item_n, local(item_numbs) 
	foreach i in `item_numbs' {
		qui levelsof item if item_n == `i', local(item_n_`i') clean
	}
	qui drop item
	destring year, replace 
	label list item_n
	qui reshape wide A_stock L_stock, i(country year) j(item_n)
	
	//label new variables
	foreach i in `item_numbs' {
		qui label var A_stock`i' "Stock, Assets, `item_n_`i'', IMF Data"
		qui label var L_stock`i' "Stock, Liabilities, `item_n_`i'', IMF Data"
	}
	
	//Clean
	foreach v in "A" "L" {
		qui rename `v'_stock3 GG_equity_`v' 
		label var GG_equity_`v' " Gen. Gov.: Equity & investment fund shares (`v')"
 	}
	tempvar aux2 
	qui egen `aux2' = rowtotal (*stock*)
	qui drop if `aux2' == 0 

	//Display available info 
	qui levelsof country, local(ctries)
	local iter = 1 
	foreach c in `ctries'{
		if (`iter' == 1) {
			display as text "Countries with information on G. Gov in Balance Sheets:"
			local iter = 0 
		}
		qui sum year if country == "`c'"
		display as text "-- `c' (`r(min)'-`r(max)')"	
	} 
	
	//Prepare for merge 
	qui kountry country, from(other) stuck marker 
	qui count if MARKER != 1 
	if (r(N) != 0) {
		display as text "At least one country unknown in IMF data"
		exit 1
	}
	qui rename _ISO3N_ iso3n_var
	qui kountry iso3n_var, from(iso3n) to(iso2c)
	qui rename _ISO2C_ iso
	drop MARKER iso3n_var
	
	//Save
	tempfile tf_imfbs
	qui save `tf_imfbs', replace 
restore  

//Merge with main
merge m:1 iso year using `tf_imfbs', nogenerate 

//1.3.2 OECD ----------------------------------------------------------------//

preserve
	qui import excel ///
		"input_data/balance_sheet/OECD/balancesheets-br-col-mex.xls" ///
		, sheet("OECD.Stat export") cellrange(A4:I3018) firstrow case(upper) clear

	//variables in lowercase
	qui rename *, lower

	//Replace missing values in some strings
	qui replace sector = c if missing(sector)
	qui drop c 
	qui replace transaction = "Total" if missing(transaction)
	foreach v in "country" "year" "sector" "unit" "transaction_class" "c" {
		qui replace `v' = `v'[_n-1] if missing(`v') 
	}
	qui egen trans = concat(transaction transaction_class), punct(", ")

	//Rename Institutional Sectors (short name)
	qui gen sec = "TOT" if sector == "Total economy"
	qui replace sec = "ROW" if sector == "Rest of the world"
	qui replace sec = "GG" if sector == "  General government"
	qui replace sec = "FC" if sector == "  Financial corporations" 
	qui replace sec = "NFC" if sector == "  Non-financial corporations"
	qui replace sec = "HH" ///
		if sector == "  Households and non-profit institutions serving households"

	//Get labels for imputation after reshape
	qui encode trans, gen(trans_n)
	label list trans_n
	local n_trans `r(k)'
	forvalues n = 1(1)`n_trans' {
		qui levelsof trans if trans_n == `n', local(lab_trans_`n') clean
	}
	qui levelsof sec, local(sec_n)
	foreach s in `sec_n' {
		qui levelsof sector if sec == "`s'", local(lab_sec_`s') clean
	}

	//Check for items with same code
	qui egen sec_trans = concat (sec trans_n), punct(_)
	qui drop transaction transaction_class trans h sector sec trans_n
	sort country year unit sec_trans
	qui by country year unit sec_trans:  gen dup = cond(_N==1,0,_n)
	qui count if dup != 0 
	if (r(N) != 0) {
		display as text "values of variable sec_trans" ///
			" are not unique within country year unit"
		exit 1	
	}
	drop dup

	//Harmonize values 
	qui replace value = value * 1000 if strpos(unit, "Millions")
	qui replace unit = subinstr(unit, ", Millions", "",.) 

	//Reshape 
	qui reshape wide value, i(country year unit) j(sec_trans) string
	qui rename value* *
	foreach s in `sec_n' {
		forvalues n = 1(1)`n_trans' {
			cap confirm variable `s'_`n'
			if (!_rc) {
				qui label var `s'_`n' ///
				"`lab_sec_`s'', `lab_trans_`n'', OECD Balance Sheets"
			}
		}
	} 

	//Prepare for merge 
	qui destring year, replace 
	qui kountry country, from(other) stuck marker 
	qui count if MARKER != 1 
	if (r(N) != 0) {
		display as text "At least 1 Country in OECD data unknown"
		exit 1 
	}
	qui rename _ISO3N_ iso3numb 
	qui kountry iso3numb, from(iso3n) to(iso2c) 
	qui drop MARKER iso3numb unit
	qui rename _ISO2C_ iso
	tempfile tf_oecd 
	qui save `tf_oecd', replace  
restore 

//Merge 
qui merge m:1 iso year using `tf_oecd', nogenerate  

//Save 
//qui save "Data/national_accounts/sna-all-countries.dta", replace 

//1.4. SOCIAL CONTRIBUTIONS -------------------------------------------------//

preserve
	//General 
	qui import excel "input_data/social_security_oecd/ssc-LatAm.xls" ///
		, sheet("OECD.Stat export") cellrange(A4:F3504) ///
		firstrow case(upper) clear

	//variables in lowercase
	qui rename *, lower

	//Replace missing values in some strings
	qui replace tax= "Total" if missing(tax)
	foreach v in "country" "year" "tax" "unit" {
		qui replace `v' = `v'[_n-1] if missing(`v') 
	}

	//Rename tax variables
	qui gen sscs = "Total" if tax == "2000 Social security contributions"
	qui replace sscs = "Employees" if tax == "2100 Employees"
	qui replace sscs = "Emoloyers" if tax == "2200 Employers"
	qui replace sscs = "Selfemp_nonemp" if tax == "2300 Self-employed or non-employed"
	qui replace sscs = "Other" if tax== "2400 Unallocable between 2100, 2200 and 2300"

	//Get labels for imputation after reshape
	qui levelsof sscs, local(sscs_n)
	foreach s in `sscs_n' {
		 qui levelsof tax if sscs == "`s'", local(lab_sscs_`s') clean
	}
	
	//Check for items with same code
	qui drop tax unit e
	qui sort country year sscs
	qui by country year sscs:  gen dup = cond(_N==1,0,_n)
	qui count if dup != 0 
	if (r(N) != 0) {
		display as text "values of variable sscs" ///
			" are not unique within country year unit"
		exit 1	
	}
	qui drop dup

	//Harmonize values 
	qui replace value = "" if value == ".."
	qui destring value, replace
	qui replace value = value * 1000 

	//Reshape 
	qui reshape wide value, i(country year) j(sscs) string
	qui rename value* *
	foreach s in `sscs_n' {
		cap confirm variable `s'
		if (!_rc) {
			qui label var `s' ///
			"`lab_sscs_`s'', OECD Social Contributions"
		}
	}

	//Prepare for merge 
	qui destring year, replace 
	qui kountry country, from(other) stuck marker 
	qui count if MARKER != 1 
	if (r(N) != 0) {
		display as text "At least 1 country-name in OECD data unknown"
		exit 1 
	}
	qui rename _ISO3N_ isodrop
	qui kountry isodrop, from(iso3n) to(iso2c)
	qui drop isodrop
	qui rename _ISO2C_ iso
	tempfile tf_oecd1
	qui save `tf_oecd1', replace  
restore 

//Merge 
qui merge m:1 iso year using `tf_oecd1', nogenerate  

//Save 
tempfile last 
qui save `last'

// Harmonize country-names --------------------------------------------///	
qui import delimited using  ///
	"input_data/sna_UNDATA/iso/iso_fullnames.csv" ///
	, encoding(ISO-8859-1) clear varnames(1)	
split name, parse(",") gen(stub)
qui rename (code stub1) (iso iso_long)
drop stub2 name
qui merge 1:m iso using `last', keep(match) nogenerate

//cosmetics and save 	
order iso_long iso series year
sort iso series year 	
qui save "intermediary_data/national_accounts/sna-all-countries.dta", replace 	

