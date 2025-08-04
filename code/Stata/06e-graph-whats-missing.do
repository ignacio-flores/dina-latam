////////////////////////////////////////////////////////////////////////////////
//
// 						Title: GRAPH COMPOSITION OF MISSING HH INCOME
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Graph composition of missing survey income of the household sector 
//			for every country-year
//
////////////////////////////////////////////////////////////////////////////////
 
//General settings 
clear all 

//list variables 
local norm_list "pre_wag pre_ben pre_imp pre_mix pre_cap "
local exep_list "pre_wag pre_ben pre_mir pre_cap "
local lang $lang 
local pop adults 

//Load globals 
foreach x in preliminary graph_basics {
	global aux_part ""`x'""
	quietly do "code/Do-files/auxiliar/aux_general.do"
}   

local step "raw"

//Import scaling factors 
quietly import excel "${summary}snacompare_raw_`pop'.xlsx", sheet("scal_sna") ///
	firstrow clear
	
	
*drop extrapolated data
*quietly drop if missing(svy_to_ni) 
quietly drop if missing(svy_to_ni_wid_pre) 
foreach v in `norm_list' pre_mir {
	quietly replace `v' = . if extsna_`v' == 1
}

//compute missing part of HH income	
foreach v in `norm_list' "pre_mir" {
	if "`v'" == "pre_pen" local v "pre_ben"
	local sh =  substr("`v'", 5, 3)
	di as result "sh: `sh'" 
	quietly gen miss_`sh' = (1 - `v'/100) * `v'_nac
}


*get total 
local norm_list2 "cap wag ben imp mix"
*local exep_list2 "cap wag ben mir"
foreach v in `norm_list2' mir {
	foreach f in posi nega {
		if "`f'" == "posi" local sign > 
		if "`f'" == "nega" local sign < 
		qui gen `f'_`v' = 0 
		qui replace `f'_`v' = miss_`v' if miss_`v' `sign' 0 & !missing(miss_`v')
	}
}

*stack
foreach x in norm exep {
	local iter_`x' = 1 
	foreach f in posi nega {
		foreach v in ``x'_list' {
		if "`v'" == "pre_pen" local v "pre_ben"
		local sh =  substr("`v'", 5, 3)
		*di as result "v: `v', sh: `sh', labcom_`sh'_`lang': ${labcom_`sh'_`lang'}"
			*prepare lists for graph 
			local `f'_stacklist_`x' "`f'_`sh' ``f'_stacklist_`x''"
			local `f'_areas_`x' ``f'_areas_`x'' ///
				(bar `f'_f`x'_`f'_`sh' year, color(${c_`sh'}) lwidth(none) ///
				barwidth(.9) ) 
			if "`f'" == "posi" {
				local leg_`x' `leg_`x'' `iter_`x'' "${labcom_`sh'_`lang'}"
			} 
			local iter_`x' = `iter_`x'' + 1
		}
	}
	genstack `posi_stacklist_`x'', gen(posi_f`x'_)
	genstack `nega_stacklist_`x'', gen(nega_f`x'_)
}

*get total
foreach q in norm exep {
	foreach var in ``q'_list' {
		local sh =  substr("`var'", 5, 3)
		local `q'_rowt "``q'_rowt' miss_`sh'"
	}
	qui egen tot_`q' = rowtotal(``q'_rowt')
}

//one graph per country 
quietly levelsof country, local(ctries) clean
foreach c in `ctries' {
	quietly sum miss_cap if country == "`c'", meanonly  
	if r(mean) != 0 {
		local x "norm"
		if inlist("`c'", "CHL", "BOL", "ECU") local x "exep"
		local firsty 2000
		local midy 5
		if inlist("`c'", "DOM") {
			local firsty 2012
			local midy 2
		}
		if ("`lang'" == "esp") {
			local ylab1 "% del ingreso nacional"
		}	
		else {
			local ylab1 "Share of National Income (%)"
		}
		graph twoway ///
			`posi_areas_`x'' `nega_areas_`x'' ///
			(connected tot_`x' year, mfcolor(none) ///
			mcolor(black) lcolor(black) msymbol(x)) ///
			if country == "`c'" & !missing(miss_cap) ///
			, xtitle("") ytitle("`ylab1'") ///
			yline(0, lcolor(black) lpattern(dash)) ///
			ylabel(-10(10)50, $ylab_opts) title(/*"`c'"*/) ///
			xlabel(`firsty'(`midy')2020, $xlab_opts ) $graph_scheme ///
			legend(order(`leg_`x'' `iter_`x'' "Total")) 
		capture graph export "figures/raw/missing/`c'_`lang'.pdf", replace
		capture graph save "figures/raw/missing/`c'_`lang'.gph", replace
	}
}

//compute measurement gap and conceptual gap as % of national income
// Chile and Ecuador as exceptions

qui gen error_ni = (pre_wag_nac + pre_ben_nac + pre_mir_nac ///
	+ pre_cap_nac*0.9 - svy_to_ni_wid_pre) if inlist(country, "CHL", "ECU") 
	
qui replace error_ni = (pre_wag_nac + pre_ben_nac + pre_mix_nac ///
	+ pre_cap_nac*0.9 + pre_imp_nac - svy_to_ni_wid_pre) ///
		if !inlist(country, "CHL", "ECU") 
	
qui gen conc_diff_ni = (100-(pre_wag_nac + pre_ben_nac + ///
	pre_mir_nac + pre_cap_nac*0.9)) if inlist(country, "CHL", "ECU") 

qui replace conc_diff_ni = (100-(pre_wag_nac + pre_ben_nac + ///
	pre_mix_nac + pre_cap_nac*0.9 + pre_imp_nac)) ///
		if !inlist(country, "CHL", "ECU") 


*stack
local stackvars ///
	"svy_to_ni_wid_pre error_ni conc_diff_ni" 
genstack `stackvars', gen(sh_)
foreach var in `stackvars'{
	quietly replace sh_`var' = . if sh_`var' == 0 
}

//one graph per country 
quietly levelsof country, local(ctries) clean
foreach c in `ctries' {
	quietly sum sh_conc_diff_ni if country == "`c'", meanonly  
	if r(mean) != 0 {
		local firsty 2000
		local midy 5
		if inlist("`c'", "DOM") {
			local firsty 2012
			local midy 2
		}
	graph twoway ///
		(bar sh_conc_diff_ni year, color(cranberry*0.3) ///
		lwidth(none) barwidth(0.9)) (bar sh_error_ni year, ///
		color(cranberry*0.8) lwidth(none) barwidth(0.9)) ///
		(bar svy_to_ni_wid_pre year, fcolor(ebblue) lcolor(black) ///
		lwidth(vthin) barwidth(0.9)) ///
		if country == "`c'" & !missing(error_ni) ///
		, xtitle("") ytitle("Share of National Income (%)") ///
		ylabel(0(10)100, $ylab_opts) title(/*"`c'"*/) ///
		xlabel(`firsty'(`midy')2020, $xlab_opts ) $graph_scheme ///
		legend(label(1 "Conceptual gap") label(2 "Measurement gap") ///
			label(3 "Survey income share")) 
		qui graph export "figures/raw/missing/`c'_gap_decomp.pdf", replace
		qui graph save "figures/raw/missing/`c'_gap_decomp.gph", replace
		*capture graph export "figures/raw/missing/`c'_gap_decomp.png", replace	
	}
}
//one graph per country (español)
quietly levelsof country, local(ctries) clean
foreach c in `ctries' {
	quietly sum sh_conc_diff_ni if country == "`c'", meanonly  
	if r(mean) != 0 {
		local firsty 2000
		local midy 5
		if inlist("`c'", "DOM") {
			local firsty 2012
			local midy 2
		}
	graph twoway ///
		(bar sh_conc_diff_ni year, color(cranberry*0.3) ///
		lwidth(none) barwidth(0.9)) (bar sh_error_ni year, ///
		color(cranberry*0.8) lwidth(none) barwidth(0.9)) ///
		(bar svy_to_ni_wid_pre year, fcolor(ebblue) lcolor(black) ///
		lwidth(vthin) barwidth(0.9)) ///
		if country == "`c'" & !missing(error_ni) ///
		, xtitle("") ytitle("% del ingreso nacional bruto") ///
		ylabel(0(10)100, $ylab_opts) title(/*"`c'"*/) ///
		xlabel(`firsty'(`midy')2020, $xlab_opts ) $graph_scheme ///
		legend(label(1 "Brecha conceptual") label(2 "Brecha medición") ///
			label(3 "Ingreso de la encuesta")) 
		qui graph export "figures/raw/missing/`c'_gap_decomp_esp.pdf", replace
		qui graph save "figures/raw/missing/`c'_gap_decomp_esp.gph", replace
		*capture graph export "figures/raw/missing/`c'_gap_decomp.png", replace	
	}
}
//all countries in one graph (latest year)
keep conc_diff_ni error_ni svy_to_ni sh_* country year
tostring country year, replace
qui gen data_point = country + "" + year


keep if inlist(data_point, "MEX2014", "COL2016", "CHL2015", "ECU2016", ///
	"PER2016", "CRI2015", "BRA2015", "DOM2016") 
label def data_point 1 "MEX2014" 2 "COL2016" 3 "CHL2015" 4 "ECU2016" ///
	5 "PER2016" 6 "CRI2015" 7 "BRA2015" 8 "DOM2016"
encode data_point, gen(numeric) label(data_point)
sort numeric 

graph bar ///
	svy_to_ni error_ni conc_diff_ni, stack over(numeric, sort(svy_to_ni) ///
	relabel(/*1 "Mexico" 2 "Rep. Dominicana" ///
	3 "Colombia" 4 "Chile" 5 "Ecuador" 6 "Peru" 7 "Costa Rica" 8 "Brazil"*/) ///
	label(labsize(medsmall) grid labels angle(45))) ///
	bar(1,fcolor(ebblue) lcolor(black) lwidth(vthin)) ///
	bar(2,color(cranberry*0.8)) bar(3,color(cranberry*0.3)) ///
	ytitle("Share of National Income (%)") ///
	ylabel(0(10)100, $ylab_opts) title(/*"`c'"*/) ///
	$graph_scheme ///
	legend(label(1 "Survey income share") label(2 "Measurement gap") ///
		label(3 "Conceptual gap")) 
	capture graph export "figures/raw/missing/gap_decomp.pdf", replace
	capture graph save "figures/raw/missing/gap_decomp.gph", replace
	*capture graph export "figures/raw/missing/gap_decomp.png", replace	



	
