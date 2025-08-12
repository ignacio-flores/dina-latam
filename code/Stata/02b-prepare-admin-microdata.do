///////////////////////////////////////////////////////////////////////////////
//				    Goal: Calls all dofiles preparing admin data	         //
/////////////////////////////////////////////////////////////////////////////// 

local list_noquotes : subinstr global all_countries `"""' "" , all

foreach dofile in "MEX-wages" "URY-gperc" { 
	local sub = substr("`dofile'", 1, 3)
	if strpos("`list_noquotes'", "`sub'") > 0 {
		//run
		di as result "(02b) Doing `dofile'.do at ($S_TIME)"
		quietly do "code/Stata/tax-data/`dofile'.do"
	}
}
