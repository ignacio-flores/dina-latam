
* Auxiliary dofile: plots and creates labour shares dataset based on ILO data: adjusted and unadjusted for ARG, SLV and URY
clear all

global data "Data/national_accounts/ILO"
global results 	"figures/lab_share"



// ILO labor share data	
import excel "$data/labor_share_ILO.xlsx", sheet("Sheet2") cellrange(X6:AA48) firstrow	


// plot for eight countries


global countries ""ARG"  "URY" "SLV""


qui foreach country in $countries {
	tempvar als_`country' uls_`country'
	gen `als_`country'' = adjs_lab_share  if country=="`country'"
	label var `als_`country'' "unad. lab. sh. `country'"

	gen `uls_`country'' = unad_lab_share  if country=="`country'"
	label var `uls_`country'' "ad. lab. sh. `country'"
}

global aux_part  ""graph_basics"" 
do "code/Do-files/auxiliar/aux_general.do"

qui twoway 	(connected `als_ARG' year) ///
			(connected `als_SLV' year) ///
			(connected `als_URY' year) ///
			(connected `uls_ARG' year) ///
			(connected `uls_SLV' year) ///
			(connected `uls_URY' year) ///
			, ytitle("Adjust-unadjust lab. shares") ///
			ylabel(.2(.1).7, $ylab_opts) ///
			xlabel(2004(1)2017, $xlab_opts) ///
			$graph_scheme ///	
			xtitle("Year")
			graph export "$results/ilo_adunad_labsh.pdf", replace

// save ILO labor share data		
save "$data/ilo_adunad_labsh.dta", replace
