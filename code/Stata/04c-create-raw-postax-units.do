////////////////////////////////////////////////////////////////////////////////
//
// 							Title:  
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
////////////////////////////////////////////////////////////////////////////////

clear all

//0. General settings ----------------------------------------------------------

//define macros 
if "${bfm_replace}" == "yes" local ext ""
if "${bfm_replace}" == "no" local ext "_norep"

//get list of paths 
global aux_part " "preliminary" " 
qui do "code/Stata/auxiliar/aux_general.do"
local lang $lang 

global area " ${all_countries} "
local z = 1 
foreach c in $area {
	forvalues y = ${first_y}/${last_y} {
		clear 
		
		if `z' == 1 {
			di as result "{hline 80}"
			display as result ///
				"Creating raw post-tax variables for units ..."
			di as result "{hline 80}"
			local z = 0 
		}	
			
		*inform activity
		di as text "`c' `y': " _continue
		
		//confirm corrected survey exists 
		capture confirm file ///
			"intermediary_data/microdata/bfm`ext'_pre/`c'_`y'_bfm`ext'_pre.dta"
		
		//open it
		if _rc==0 {
			qui use ///
				"intermediary_data/microdata/bfm`ext'_pre/`c'_`y'_bfm`ext'_pre.dta", clear
			
			cap drop __*
			
			*loop over income items 
			foreach v in wag pen cap imp mix {
				
				*compute sum of incomes by couple (narrow)
				cap drop esn_pos_`v'
				qui egen esn_pos_`v' = sum(ind_pos_`v'/married) ///
					if married <= 2, by(id_hogar)
				qui replace esn_pos_`v' = ///
					ind_pos_`v' if missing(married)
				qui cap la var esn_pos_`v' ///
					"Eq-split narrow posttax inc. `v'"	
					
				*compute sum of incomes by adults (broad)
				cap drop esb_pos_`v'
				qui egen esb_pos_`v' = ///
					sum(ind_pos_`v'/adults_house), by(id_hogar)
				qui la var esb_pos_`v' ///
					"Eq-split broad `posttax inc. `v'"	
				
				*compute sum of incomes by household 
				cap drop pch_pos_`v'
				qui egen pch_pos_`v' = ///
					sum(ind_pos_`v' / hh_size), by(id_hogar)
				qui la var pch_pos_`v' ///
					"Per capita hld. posttax inc. `v'"
			}
			
			*save 
			qui save ///
				"intermediary_data/microdata/bfm`ext'_pre/`c'_`y'_bfm`ext'_pre.dta" ///
				, replace
			
			*report activity	
			di as result "done"	
		}
		else {
			di as text "intermediary_data/microdata/bfm`ext'_pre/`c'_`y'_bfm`ext'_pre.dta not found."
		}
	}
}	
