* DINA country-run entry; the worker writes only to its temporary output folder.
args dina_requested_countries
local dina_worker : environment DINA_COUNTRY_WORKER
if "`dina_worker'" != "06a-run-ineqstats" {
    do "code/Stata/auxiliar/dina_country_task.do" "06a-run-ineqstats" "`dina_requested_countries'"
    exit
}
local dina_output_env : environment DINA_OUTPUT_ROOT
local dina_countries_env : environment DINA_RUN_COUNTRIES
if `"`dina_output_env'"' == "" | trim("`dina_countries_env'") == "" {
    di as error "Missing country-run output folder or selected countries."
    exit 198
}
global dina_output_root `"`dina_output_env'"'
global dina_run_countries "`dina_countries_env'"

////////////////////////////////////////////////////////////////////////////////
//
// 						Title: INEQUALITY STATISTICS - DATA 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Apply the 'ineqstats' program to various steps of our correction
//			and save results for analysis. It provides income shares, ginis, 
//			, theil, sginis and income composition for every country-year 
//
///////////////////////////////////////////////////////////////////////////////

clear all 

//General settings -------------------------------------------------------------

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Stata/auxiliar/aux_general.do"

//find program
sysdir set PERSONAL "code/Stata/ado/ineqstats_ado/." 

//Run ineqstats program --------------------------------------------------------

local run_steps : environment DINA_RUN_STEPS
if trim("`run_steps'") == "" local run_steps "$all_steps"
local run_units : environment DINA_RUN_UNITS
if trim("`run_units'") == "" local run_units "$units_06a"
foreach step in `run_steps' {
	// Keep the income-step exclusions already applied by 05b/05c/05d.
	// Raw and BFM-corrected income concepts retain their existing eligibility.
	local excluded_cases ""
	if inlist("`step'", "rescaled", "uprofits", "natinc", "pod", "pon", "psp") ///
		local excluded_cases "CRI2002 CRI2003 CRI2005"
	if inlist("`step'", "pod", "pon", "psp") ///
		local excluded_cases "`excluded_cases' ARG2016"

	local date "$S_DATE"
	local date = subinstr("`date'", " ", "", .)
	
	//get short name of step
	local s = substr("`step'", 1, 3)
	
	//normie locals
	local type "`step'"
	local weight "_weight"
	local bfm "" 
	
	//special-cases
	if inlist("`step'", "raw", "por") local weight "_fep"
	if ("`step'" == "raw") local ext "_svy_y" 
	else local ext ""
	
	*if inlist("`step'", "bfm${ext}_pre") local bfm_`step' "bfm"
	
	//define macros 
	if "${bfm_replace}" == "yes" {
		local bfm_`step' "bfm"
	} 	  	
	if "${bfm_replace}" == "no" {
		local bfm_`step' ""
	} 	  
	
	if inlist("`step'", "rescaled", "uprofits", ///
		"natinc", "pod", "pon", "psp") {
		local type "bfm${ext}_pre"
	}
	
	//create main folders 
	local dirpath "${dina_output_root}/output/ineqstats"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	//create main folders 
	local dirpath "${dina_output_root}/output/ineqstats/log"
	mata: st_numscalar("exists", direxists(st_local("dirpath")))
	if (scalar(exists) == 0) {
		mkdir "`dirpath'"
		display "Created directory: `dirpath'"
	}
	
	//statistical units
	global units "`run_units'"
	foreach unit in $units {

		cap log close
		log using "${dina_output_root}/output/ineqstats/log/`unit'_`step'_`date'.smcl", replace
		if inlist("`unit'", "ind", "esn") local age 20
		if inlist("`unit'", "pch") local age 0
		if inlist("`unit'", "act") local age 20 65
		
		//loop over country groups	
		di as result "step: `step'"
		
		//call program	
		 ineqstats summarize, `bfm_`step'' ///
			svypath("intermediary_data/microdata/bfm${ext}_pre") edad(`age') ///
			type(`type') time($first_y $last_y) area(${dina_run_countries}) ///
			export("${dina_output_root}/output/ineqstats/ineqstats_`step'_`unit'.xlsx") ///
			weight(`weight') dec(${`unit'_`s'_norm}) ///
			unit(`unit') coverage("${dina_output_root}/_coverage.csv") steplabel(`step') ///
			excludecases("`excluded_cases'") /*smoothtop*/
		log close 
	}
}	
