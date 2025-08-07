
//download data from wid 
wid, indicators(afiinc tfiinc inyixx) years() areas(CO) ///
	perc(p99p99.5 p99.5p99.9 p99.9p99.99 p99.99p100	p0p100 p99p100) clear
	


*identify percentiles 
gen p = 99 if percentile == "p99p99.5"
qui replace p = 99.5 if percentile == "p99.5p99.9"
qui replace p = 99.9 if percentile == "p99.9p99.99"
qui replace p = 99.99 if percentile == "p99.99p100"	

*save info for later 
qui levelsof year if variable == "inyixx999i", local(years_inyixx) clean 
foreach t in `years_inyixx' {
	qui levelsof value if variable == "inyixx999i" & year == `t', ///
		local(inyixx_`t') clean
}
qui levelsof year if variable == "afiinc992i" & percentile == "p99p100", ///
	local(years_top1avg) clean 
foreach t in `years_top1avg' {
	qui levelsof value if variable == "afiinc992i" & percentile == "p99p100" ///
	& year == `t', local(top1avg_`t') clean
}
qui drop if missing(p)

//reshape 
qui keep country year variable value p
reshape wide value, i(country year p) j(variable) string	
qui rename value*992i *
qui rename (tfiinc afiinc) (thr bracketavg) 

//fill info on price index and top average 
foreach x in inyixx top1avg {
	qui gen `x' = . 
	foreach t in `years_`x'' {
		qui replace `x' = ``x'_`t'' if year == `t'
	}
}

//get current values 
foreach v in thr bracketavg top1avg {
	qui replace `v' = `v' * inyixx
}
qui drop inyixx 


