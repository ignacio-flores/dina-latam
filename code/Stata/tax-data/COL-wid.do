*----------------------------------------------------------------------------*
*
* Importing data from WID.world using stata command "wid"				     *                                                                                  
*----------------------------------------------------------------------------*

clear all
local user "Data/Tax-data/COL"


forval year=1993/2010{
	foreach country in CO{
		foreach x in afiinc tfiinc {

		clear all
		wid, indicators(`x') years(`year') areas(`country') ///
			perc(p99p99.5 p99.5p99.9 p99.9p99.99 p99.99p100)
		drop percentile
		drop if variable=="`x'992j" 
		drop variable
		rename value `x'
		sort `x'

		gen double ftile = 99
		replace ftile = 99.5 if [_n]==2
		replace ftile = 99.9 if [_n]==3
		replace ftile = 99.99 if [_n]==4
		gen double p = ftile/100
		format p %10.0g 
		drop ftile

		save "`user'/wid/`x'`country'`year'.dta", replace
		
		}
	}
}
/*
forval year=1993/2010{
foreach country in CO{
foreach x in sfiinc {

clear all
wid, indicators (`x') years (`year') areas (`country') ///
	perc (p99p99.5 p99.5p99.9 p99.9p99.95 p99.95p99.99 p99.99p100)
drop percentile
drop if variable=="`x'992j" | variable=="`x'992t" 
drop variable
rename value `x'
gsort -`x'

gen double ftile = 99
replace ftile = 99.5 if [_n]==2
replace ftile = 99.9 if [_n]==3
replace ftile = 99.95 if [_n]==4
replace ftile = 99.99 if [_n]==5
gen double p = ftile/100
format p %10.0g 
drop ftile


save "`user'/wid/`x'`country'`year'.dta", replace
  }
  }
  }
*/ 
  
forval year=1993/2010{
	foreach country in CO{ 


		use "`user'/wid/tfiinc`country'`year'.dta"
		merge m:m p using "`user'/wid/afiinc`country'`year'.dta", nogenerate
		rename t* thr 
		rename afiinc bracketavg
		/*
		merge m:m p using "`user'/wid/bfiinc`country'`year'.dta", nogenerate
		rename bfiinc b

		merge m:m p using "`user'/wid/sfiinc`country'`year'.dta", nogenerate
		rename s* topsh
		*/
		save "`user'/wid/`country'`year'real.dta", replace
	 
	}
}

forval year=1993/2010{
	foreach country in CO{
		foreach x in afiinc inyixx {

		wid, indicators(`x') years(`year') areas(`country') perc(p0p100) clear
		keep country year value variable
		drop if variable=="afiinc992j" | variable=="afiinc992t" 
		gen double p=0 in 1
		ren value `x'
		drop variable

		save "`user'/wid/`x'`country'`year'.dta", replace
		}
	}
}

forval year=1993/2010{
	foreach country in CO{ 

	use "`user'/wid/afiinc`country'`year'.dta"
	merge m:m p using "`user'/wid/inyixx`country'`year'.dta", nogenerate
	rename a* average
	rename i* pindex

	merge 1:m p using "`user'/wid/`country'`year'real.dta", nogenerate 
	replace pindex=pindex[1] if pindex==.

	replace average=average*pindex
	replace thr=thr*pindex
	replace bracketavg=bracketavg*pindex
	replace country="COL" if country=="CO"
	replace average=average[1] if average==.
	drop age pop pindex
	//drop if p==0

	order year country average p thr bracketavg

	save "`user'/wid/`country'`year'nominal.dta", replace
	export excel using "`user'/gpinterinput_`country'.xlsx", ///
		sheet("`year'_WID", replace) firstrow(variables) keepcellfmt 
	}
}
