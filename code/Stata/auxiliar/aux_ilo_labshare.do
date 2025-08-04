
* Auxiliary dofile: plots and creates labour shares dataset based on ILO data
clear all

global data "Data/national_accounts/ILO"
global results 	"figures/lab_share"

// ILO labor share data		
use "$data/ILO_lab_share.dta", clear


// plot for eight countries
qui cap drop country	
qui gen      country = "."
qui replace  country = "ARG" if ref_arealabel=="Argentina"
qui replace  country = "BRA" if ref_arealabel=="Brazil"
qui replace  country = "URY" if ref_arealabel=="Uruguay"
qui replace  country = "SLV" if ref_arealabel=="El Salvador"
qui replace  country = "COL" if ref_arealabel=="Colombia"
qui replace  country = "CHL" if ref_arealabel=="Chile"
qui replace  country = "ECU" if ref_arealabel=="Ecuador"
qui replace  country = "MEX" if ref_arealabel=="Mexico"
qui replace  country = "PER" if ref_arealabel=="Peru"

qui rename 	time 		year
qui rename 	obs_value	ad_lab_share 
qui keep 	country year ad_lab_share 
qui keep if country != "."

global countries ""ARG" "BRA" "URY" "SLV" "COL" "CHL" "ECU" "MEX" "PER""


qui foreach country in $countries {
	tempvar ls_`country'
	gen `ls_`country'' = ad_lab_share  if country=="`country'"
	label var `ls_`country'' "`country'"
}

global aux_part  ""graph_basics"" 
do "code/Do-files/auxiliar/aux_general.do"

qui twoway 	(connected `ls_ARG' year) ///
			(connected `ls_BRA' year) ///
			(connected `ls_URY' year) ///
			(connected `ls_SLV' year) ///
			(connected `ls_COL' year) ///
			(connected `ls_CHL' year) ///
			(connected `ls_ECU' year) ///
			(connected `ls_MEX' year) ///
			(connected `ls_PER' year) ///	
			, ytitle("Adjusted labour share") ///
			ylabel(20(10)80, $ylab_opts) ///
			xlabel(2004(1)2017, $xlab_opts) ///
			$graph_scheme ///	
			xtitle("Year")
			graph export "$results/ilo_lab_sh.pdf", replace

// save ILO labor share data		
save "$data/ILO_lab_share_latam.dta", replace
