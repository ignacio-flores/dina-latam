//0. Define directory
clear all
capture cd "~/Dropbox/"

global open_wid "wid/W2ID/Latest_Updated_WID/wid-data.dta"
global save_jav "fiscal_shares"

qui use iso year p widcode value if strpos(widcode, "sfiinc") ///
	using $open_wid, clear 
qui kountry iso, from(iso2c) to(iso3c) geo(undet)	
quietly keep if inlist(GEO, "South America")
levelsof iso, clean

xtline value if p == "p99p100", i(iso) t(year) $graph_scheme overlay
keep iso year p value 
qui replace p = subinstr(p, ".", "_", .)
qui reshape wide value, i(iso year) j(p) string
qui rename value* * 

qui export excel "${save_jav}.xlsx" , replace firstrow(variables)
