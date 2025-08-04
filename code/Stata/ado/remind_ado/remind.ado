// This will be the program that gives summarized info on data-availability in SNA

program remind, eclass
	version 11
	syntax, [REgion(string) STRUCture(string) SNA(string)] 

		//Test
		if "`region'" == "hola" {
			di "HOLA!"
		}
		
		//Display national accounts structure and codes
		if inlist("`structure'", "1968", "1993", "2008") {
			preserve 
				//Order
				local sy "`structure'"
				//use "Data/raw-data/un-national-accounts/Table 4.6 Households (S.14).dta", clear
				use "Data/raw-data/un-national-accounts/Table 4.1 Total Economy (S.1).dta", clear
				quietly split sub_group, parse(-)
				quietly rename sna93_item_code i_code
				quietly replace i_code = subinstr(i_code, ".", "",.) 
				//
				quietly levelsof sub_group1 if sna_system == `sy', local(sub_groups_`sy')
				local iter = 1
				foreach sg in `sub_groups_`sy'' {
					if (`iter' == 1) {
						display as text "{hline 75}"
						display as text "{bf: STRUCTURE AND CODES SNA`sy':}"
						display as text "{hline 75}"
					}
					//Display long name of item and code
					quietly levelsof item if sub_group1 == "`sg'", local(items_`iter'_`sy')
					display as text  "{bf: `sg' - (sub table `iter')}" 
					foreach it in `items_`iter'_`sy'' {
						quietly levelsof i_code if item == "`it'", local(icod) clean
						display as text "    - [`icod']: `it'"
					}
					local iter = `iter' + 1
				}
			restore
		}
		
		else if ("`structure'" != "") {
			di as error "Option structure(...) must be 1968, 1993 or 2008"
			exit 198
		}
end 		


/*
//Check for insconsistencies
local iter = 1
tempfile tf_1 
foreach sy in `sna_systems' {
	preserve
		collapse sna_system if sna_system == `sy', by (sub_group1 item)
		by sub_group1: gen sg_n = _n
		tostring sg_n, gen (sg_n_cd) 
		quietly egen sg_code = concat(sub_group1 sg_n_cd)
		quietly rename item item_`sy'
		if (`iter' == 0) {
			merge m:m sg_code using `tf_1', nogenerate
		}
		save `tf_1', replace
		local iter = 0
	restore	
}
quietly use `tf_1', clear
keep sg_code item_1968 item_1993 item_2008
order sg_code item_1968 item_1993 item_2008
*/
