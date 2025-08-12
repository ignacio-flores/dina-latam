

clear all
global data "input_data/admin_data/DOM"
global results 	"output/figures/eff_tax_rates/"
	
//call graph parameters 
global aux_part  ""graph_basics"" 
do "code/Stata/auxiliar/aux_general.do"

local lang $lang 

forvalues year = 2012/2020 {

	qui import excel ///
		"input_data/admin_data/DOM/IR-1 consolidado (2012-2020).xlsx", /// 
		sheet("Año `year'") cellrange("C4:X32") clear
	
	//rename variables
	quietly rename (C D E F G H I J K L M N O P Q R S T U V W X) ///
		(declar thr totdec wage divs interest rent otherinc expens exemp ///
		taxdue sscont housasset agroasset stockasset cashasset liab ///
		men women capitalinc indepinc dependinc)
	
	*original tables contain an error, where n = 19 equals the sum of 
	*following brackets 
	qui drop in 19	
		
		
	//gen variables of interest
	quietly gen bracketavg = totdec/declar
	
	qui gen year=`year' in 1
	qui egen suminc=total(totdec)
	qui egen totaldeclar=total(declar)
	qui gen average=suminc/totaldeclar
	
	foreach inc in wage divs interest rent otherinc expens exemp ///
		taxdue sscont capitalinc indepinc dependinc ///
		housasset agroasset stockasset cashasset liab {
		qui gen sh_`inc' =  `inc' / totdec 
	}
	
	*qui gen auxchk = -sh_expens
	*qui egen check1 = rowtotal(sh_capitalinc sh_indepinc sh_dependinc auxchk) 
	
	//keep variables of interest
	qui order year average suminc declar thr bracketavg sh_*
	qui keep year average suminc declar thr bracketavg sh_*
	
	tempfile tab_`year'
	quietly save `tab_`year'', replace
	
	cap use "intermediary_data/microdata/raw/DOM_`year'_raw.dta", clear
	
	cap assert _N == 0
	if _rc != 0 {
		quietly sum _fep   
		local totalpop = r(sum)
	}	
	
	qui use `tab_`year'' , clear
	tempvar freq cumfreq 
	
	//Obtaining population totals, frequencies and cumulative frequencies
	quietly gen totalpop=`totalpop'
	qui gsort - bracketavg
	quietly gen `freq'=declar/totalpop
	quietly	gen `cumfreq' = sum(`freq')
	
	//percentiles
	quietly gen p = 1 - `cumfreq'
	qui sort bracketavg
	qui sort p
	
	qui gen country="DOM" in 1
	qui gen component = "pretax" in 1 
	qui replace average = suminc / totalpop
	qui replace totalpop = . if _n != 1 
	qui replace average = . if _n != 1 
	local lister year country component totalpop average p thr bracketavg
	qui keep `lister' sh_tax*
	qui order `lister'
	
	qui rename sh_taxdue eff_tax_rate 
	qui replace eff_tax_rate = . if thr == 0 
	
	*merge with gpinter file 
	preserve 
		qui import excel "$data/gpinter_output/total-pre-DOM.xlsx", ///
			sheet("pretax, DOM, `year'") firstrow clear	
		tempfile gpf_`year'
		qui keep p 
		qui save `gpf_`year''	
	restore
	qui merge 1:1 p using `gpf_`year''
	sort p 
	
	*interpolate 
	qui replace eff_tax_rate = 0 in 1 
	qui ipolate eff_tax_rate p, gen(ipol)
	qui mipolate ipol p, gen(ipol2) forward 
	
	*clean 
	qui drop ipol 
	qui keep if _merge != 1 
	qui rename (ipol2 p) (eff_tax_rate_ipol p_merge)
	qui keep p_merge  eff_tax_rate_ipol


	
	*prepare labels depending on language 
	if "`lang'" == "eng" {
		local ytit "Effective tax rate (% of pretax inc.)"
		local xtit "Fractiles"
	}
	if "`lang'" == "esp" {
		local ytit "Tributación efectiva (% ingreso bruto)"
		local xtit "Fractiles"
	}
	
	qui replace eff_tax_rate = eff_tax_rate * 100
	form p %15.2fc
	local lowlim = .9
	twoway (connected eff_tax_rate p) if p >= `lowlim' ,  ///
		ytitle("`ytit'") ///
		xtitle("`xtit'") ///
		yline(100, lpattern(dash) lcolor(black*0.5)) ///
		ylabel(0(5)30, $ylab_opts) ///
		xlabel(`lowlim'(0.01)1, $xlab_opts) ///
		$graph_scheme			
	graph export "$results/DOM_`year'.pdf", replace		

	form p %15.3fc
	twoway (connected eff_tax_rate p if p>=0.99), ///
		ytitle("`ytit'") ///
		xtitle("`xtit'") ///
		yline(100, lpattern(dash) lcolor(black*0.5)) ///
		ylabel(0(5)30, $ylab_opts) ///
		xlabel(0.99(0.001)1, $xlab_opts) ///
		$graph_scheme 
	graph export "$results/DOM_`year'_top1.pdf", replace
	
		*save 
	qui replace eff_tax_rate_ipol = eff_tax_rate_ipol / 100
	qui replace p_merge = p_merge*10000
	qui duplicates drop p_merge, force
	qui drop if p_merge > 9999
	qui format p_merge %9.0g
	save "$data/eff-tax-rate/DOM_effrates_`year'", replace
	
}	
