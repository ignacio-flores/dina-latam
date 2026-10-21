// Direct and runner execution share country selection and staged publication.
args dina_requested_countries
local dina_country_worker : environment DINA_COUNTRY_WORKER
if "`dina_country_worker'" != "05a-compare-sna-totals" {
    do "code/Stata/auxiliar/dina_country_task.do" "05a-compare-sna-totals" "`dina_requested_countries'"
    exit
}

////////////////////////////////////////////////////////////////////////////////
//
// 							Title: SCALING FACTORS 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Compares income aggregates to totals in national accounts 
// 			at every step of the procedure. The output is sent to an 
// 			excel file, which is then used as input for graphs in another 
//			dofile
//
////////////////////////////////////////////////////////////////////////////////

//General 
clear all
clear programs

//preliminary
global aux_part  ""preliminary"" 
quietly do "code/Stata/auxiliar/aux_general.do"

//define macros 
if "${bfm_replace}" == "yes" local ext ""
if "${bfm_replace}" == "no" local ext "_norep"

//find programs folder 
sysdir set PERSONAL "code/Stata/ado/snacompare_ado/." 

// Create directory if it doesnt exist 
local dirpath "${dina_output_root}/output/snacompare_summary"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

local dirpath "${dina_output_root}/output/figures/snacompare"
mata: st_numscalar("exists", direxists(st_local("dirpath")))
if (scalar(exists) == 0) {
	mkdir "`dirpath'"
	display "Created directory: `dirpath'"
}

//Run program 
foreach step in "raw" "bfm`ext'_pre" {  

	//define countries 
	if ("`step'" == "urb") global area ARG
	if ("`step'" != "urb") global area " ${all_countries} "

	//define weights
 	if inlist("`step'", "raw", "urb") local weight "_fep"
	if inlist("`step'", "bfm_norep_pre", "bfm_pre") local weight "_weight"
	
	*report step
	display as text "{hline 55}"
	di as result "Comparing micro-macro aggregates: `step'"
	display as text "{hline 55}"
	 
	foreach pop in $snapops {
		
		if "`pop'" == "adults" local age 20
		if "`pop'" == "totpop" local age 0
		if "`pop'" == "active" local age 20 65
		

        foreach c in $all_countries {
            forvalues y = $first_y/$last_y {
                local source "intermediary_data/microdata/`step'/`c'_`y'_`step'.dta"
                if "`step'" == "raw" local source "intermediary_data/microdata/raw/`c'/`c'_`y'_raw.dta"
                capture confirm file "`source'"
                if !_rc dina_coverage, country("`c'") year(`y') step("`step'") unit("`pop'") status("expected")
            }
        }

		//call program	
		snacompare using "intermediary_data/national_accounts/UNDATA-WID-Merged.dta", ///
			svypath("intermediary_data/microdata/") weight(`weight') ///
			time(${first_y} ${last_y}) edad(`age') ext(`ext') ///
			area(${area}) type(`step') /*show esp GRAPHEXTRAP*/ ///
			exportexcel("${dina_output_root}/output/snacompare_summary/snacompare_`step'_`pop'.xlsx") ///
			auxiliary("code/Stata/auxiliar/aux_snacompare.do")

        foreach c in $all_countries {
            forvalues y = $first_y/$last_y {
                local source "intermediary_data/microdata/`step'/`c'_`y'_`step'.dta"
                if "`step'" == "raw" local source "intermediary_data/microdata/raw/`c'/`c'_`y'_raw.dta"
                capture confirm file "`source'"
                if !_rc dina_coverage, country("`c'") year(`y') step("`step'") unit("`pop'") status("success")
            }
        }

	}
}
