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
qui do "code/Do-files/auxiliar/aux_general.do"

//find program
sysdir set PERSONAL "$adofile/ineqstats_ado/." 

//Run ineqstats program --------------------------------------------------------

foreach step in $all_steps {

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
	if inlist("`step'", "bfm_norep_pre") local bfm_`step' "bfm"
	if inlist("`step'", "rescaled", "uprofits", ///
		"natinc", "pod", "pon", "psp") {
		local type "bfm_norep_pre"
	} 
	
	//statistical units
	global units " $units_06a "
	foreach unit in $units {

		cap log close
		log using "${summary}log/`unit'_`step'_`date'.smcl", replace 
		if inlist("`unit'", "ind", "esn") local age 20
		if inlist("`unit'", "pch") local age 0
		if inlist("`unit'", "act") local age 20 65
		
		//loop over country groups	
		di as result "step: `step'"
		//call program	
		 ineqstats summarize, `bfm_`step'' svypath($svypath) edad(`age') ///
			type(`type') time($first_y $last_y) area(${all_countries}) ///
			export("${summary}ineqstats_`step'_`unit'.xlsx") ///
			weight(`weight') dec(${`unit'_`s'_norm}) ///
			unit(`unit')  /*smoothtop*/
		log close 
	}
}	
