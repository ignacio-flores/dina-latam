
///////////////////////////////////////////////////////////////////////////////
//																			 //
//																			 //
//  MORE UNEQUAL OR NOT AS RICH? The Missing Half of Latin American Income	 //
//			          	De Rosa, Flores & Morgan (2022)						 //
//				    Goal: Runs every dofile in the project					 //
//																		     //
///////////////////////////////////////////////////////////////////////////////

//general settings 
macro drop _all 
clear all 

*ssc install gtools mipolate quandl xls2dta sgini  wid  kountry genstack egenmore ereplace jsonio 
// search dm88_1 (to download renvars) 
// net install github, from("https://haghish.github.io/github/")
// github install haghish/rcall
// ineqdeco

//list codes 
***********************************************************************
global do_codes1 " "01a" "01b" "01c" "01d" "01e" "01f" "01g" " 
global do_codes2 " "02a" "02b" "02c" "02d" "02e" " 
global do_codes3 " "03a" "03b" "03c" "03d" "03e" " 
global do_codes4 " "04a" "04b" "04d" " 
*global do_codes5   " "05a" "05b" "05c" "05d" "05e" "05f" "  
*global do_codes6  " "06a" "06b" "06c" " /*"06d" "06e"*/ 
*global do_codes7  " "07a" "07b" "07c" "07d" " 
global last_code = 3
***********************************************************************

//report and save start time 
local start_t "$S_DATE at $S_TIME"
di as result "Started running everything working `start_t'"

//prepare list of do-files 
forvalues n = 1/$last_code {

	//get do-files' name 
	foreach docode in ${do_codes`n'} { 
			
		local do_name : dir "code/Stata/." files "`docode'*.do" 
		local do_name = subinstr(`do_name', char(34), "", .)
		global doname_`docode' "`do_name'"
	}
}	

//loop over all files  
forvalues n = 1/$last_code {
	foreach docode in ${do_codes`n'} {
		
		*********************
		do code/Stata/${doname_`docode'}
		*********************
		
		//record time
		global do_endtime_`docode' " - ended $S_DATE at $S_TIME"
		
		//remember work plan
		di as result "{hline 70}" 
		di as result "list of files to run, started `start_t'"
		di as result "{hline 70}"
		forvalues x = 1/$last_code {
			di as result "Stage nº`x'"
			foreach docode2 in ${do_codes`x'} {
				di as text "  * " "${doname_`docode2'}" _continue
				di as text " ${do_endtime_`docode2'}"
			}
			if `x' == ${last_code} di as result "{hline 70}"	
		}
	}
}

