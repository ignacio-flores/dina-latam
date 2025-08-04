//---------------------------------------------------------------------------
//						IMPORT and CLEAN DATA from CEQ
//---------------------------------------------------------------------------

//preliminary
global aux_part  ""preliminary"" 
qui do "code/Stata/auxiliar/aux_general.do"

//I. Cleaning and descriptives  ..............................................

//I. 1 Clean new data ..........................................
local c "DOM"
local DOM_year 2007 
local DOM_cellr FR8:HG138
local sheet "E11.m+p FiscalInterventions"
local fname "DOM13WBR_PDI_E11_2011PPP_28Jul2020"

*chose variables without ssc 
qui import excel using ///
	"${ceq}E-sheets_LATAM_July18_2021/`c'/``c'_year'/PDI/`fname'.xlsx", ///
	sheet("`sheet'") cellrange(``c'_cellr') ///
	firstrow clear case(lower)
qui drop if _n < 30	
drop in 101 

*bulk renaming 
qui rename all* *
qui rename inkindhealthbenefits* inkheab*
qui rename inkindeducationbenefits* inkedub*
cap drop directtransfersinclcontri directtaxesandcontributio
qui rename (contributorypensionsidssper nccbonogaschoferespercapi ///
	directtaxespersonalincometax indirecttaxselectivopercapi ///
	indirecttaxitbispercapita) ///
	(contpeidss nccbonocho diretaxpit indtaxsele indtaxtbis)	
	
*detailed renaming 	
qui renvars, trim(10)
qui rename (contributo neteducat nethealtht) (allcontrib education health)
qui gen ftile = (_n-1) * 10^3
qui gen country = "`c'"
qui gen year = 2013
qui ds country year ftile, not 
foreach v in `r(varlist)' {
	qui rename `v' new_`v'
}
qui replace ftile = 100000 if ftile == 99000 & country == "DOM"

*save (and eventually append) 
tempfile tf_new 
qui save `tf_new' 

//I. 2 Clean aggregated data....................................

foreach approach in  "incidence" "concentration" {
	*choose excel sheet 
	if "`approach'" == "incidence" local sheet "16. Incidence by decile"
	if "`approach'" == "concentration" local sheet "19. Conc shares by decile"
	
	*For both including and not-including SSC
	foreach ssc_type in "no_ssc" "yes_ssc" {
		*choose cellrange 
		if "`ssc_type'" == "no_ssc" local cellr B7:AI568
		if "`ssc_type'" == "yes_ssc" local cellr B7:C568
		
		*chose variables without ssc 
		qui import excel using "${ceq}CEQSI_15May2020.xlsx", ///
			sheet("`sheet'") cellrange(`cellr') ///
			firstrow clear case(lower)
		qui renvars b c / country decile
	
		*small fixes 
		if "`approach'" == "incidence" {
			qui replace country = "Brazil (2008)" if country == "Brazil (2009)"
		}
		if "`approach'" == "concentration" {
			qui replace country = "Brazil (2008)" in 57/65
		}
		qui replace country = "Dominican Republic (2013)" ///
			if country == "Dominican Republic (2006)"
		
		*chose variable with ssc 
		if "`ssc_type'" == "yes_ssc" {
			qui gen id = _n
			preserve
				qui import excel using "${ceq}CEQSI_15May2020.xlsx", ///
					sheet("`sheet'") cellrange(AK7:BO568) ///
					firstrow clear case(lower)
				qui gen id = _n
				tempfile scc
				qui save `scc'
			restore
			qui merge 1:1 id using `scc', nogen
			qui drop id
		}
		
		*harmonise country names and years 
		qui gen year = substr(country, -5, 4)
		qui replace country = substr(country, 1, length(country) -7)
		qui kountry country, from(other) stuck marker
		qui rename _ISO3N_ iso3n_var
		qui kountry iso3n_var, from(iso3n) to(iso3c) geo(undet)
		qui rename _ISO3C_ ISO
		qui drop MARKER iso3n*
		qui keep if inlist(GEO, "South America", "Central America", "Caribbean")
		qui replace year = "2014" if ISO == "PRY"
		qui destring year, replace 
		
		*list countries 
		qui egen ctry_yr = concat(ISO year), punct("_")
		qui levelsof ctry_yr, local(ctryrs) clean 

		*label deciles 
		qui replace decile = "100" if decile == "Total"
		qui destring decile, replace
		qui label define decile 100 "Total"
		qui lab val decile decile

		*save 
		tempfile tf_`ssc_type'
		save `tf_`ssc_type'', replace
		
		//I. 2. Report availability .......................

		preserve 
			*collapse 
			qui drop decile
			qui ds country year ISO, not
			local vars `r(varlist)'
			qui collapse (firstnm) `vars', by(country year)

			* Generate marker of non-missing values per variable per year
			qui ds country year, not
			foreach v of varlist `vars'{
				qui gen x = "x" if !missing(`v')
				qui replace x="" if missing(`v')
				qui drop `v'
				qui ren x `v'
			}

			export excel "${cew}data_descript.xlsx", ///
				sheet("avail_`ssc_type'") sheetreplace first(varl)
		restore 
	}	

	
	*Interpolate to 127 percentiles 
	qui use `tf_no_ssc', clear 
	qui drop if decile == 100
	if "`approach'" == "incidence" qui gen ftile = (decile - .5) * 10^4
	if "`approach'" == "concentration" qui gen ftile = decile * 10^4
	
	qui save `tf_no_ssc', replace 

	// build 127 percentiles again from scratch
	local iter = 1 
	tempfile tf_scratch 
	foreach cy in `ctryrs' {
		*make room
		clear
		quietly set obs 127
		*define country and year 
		local iso = substr("`cy'", 1, 3)
		local t = substr("`cy'", 5, 4)
		*fill variables 
		qui gen ctry_yr = "`cy'"
		qui gen ISO = "`iso'"
		qui gen year = `t'
		quietly gen ftile = (_n - 1) * 10^3 in 1/100
		quietly replace ftile = (99 + (_n - 100)/10) * 10^3 in 101/109
		quietly replace ftile = (99.9 + (_n - 109)/100) * 10^3 in 110/118
		quietly replace ftile = (99.99 + (_n - 118)/1000) * 10^3 in 119/127	
		if `iter' == 0 qui append using `tf_scratch'
		qui save `tf_scratch', replace 
		local iter = 0 
	}

	*append clean cuts 	
	quietly append using `tf_no_ssc'
	quietly sort ISO ftile 
	qui order ISO ctry_yr year ftile 
	qui renvars, trim(10)
	quietly ds ctry_yr ftile, not
	local vars `r(varlist)' 
	foreach var in `vars' {
		local l_`var': var label `var'
	}
	qui collapse (firstnm) `vars', by(ctry_yr ftile) 
	qui rename (country ISO) (country_long country)
	if "`approach'" == "concentration" drop d 
	
	*bring more detailed data for DOM 
	qui merge 1:1 country year ftile using `tf_new', ///
		keepusing(new_indirectta new_allcontrib new_health new_education) ///
		nogen 
	
	*list variables to inter-extrapolate 
	quietly ds country ctry_yr year ftile country_long decile GEO, not 
	local vars `r(varlist)'
	
	*inter-extrapolate concentration 
	if "`approach'" == "concentration" {
		foreach var in `vars' {
			bysort ctry_yr: gen cum_`var' = sum(`var') if !missing(`var')
			qui levelsof ctry_yr if !missing(`var'), local(ctries) clean 
			foreach c in `ctries' {
				qui replace cum_`var' = 0 if ftile == 0 & ctry_yr == "`c'"
			}
			bysort ctry_yr: mipolate cum_`var' ftile, gen(cumlip_`var') linear
			bysort ctry_yr: mipolate cum_`var' ftile, gen(cumsip_`var') spline
			bysort ctry_yr: gen sh_li_`var' = cumlip_`var'[_n+1] - cumlip_`var'
			bysort ctry_yr: gen sh_si_`var' = cumsip_`var'[_n+1] - cumsip_`var'
			*labels 
			qui cap la variable `var' "`l_`var''"
			qui cap la variable cumlip_`var' ///
				"`l_`var'' (cumulative, linear interpolation)"
			qui cap la variable cumsip_`var' ///
				"`l_`var'' (cumulative, spline interpolation)"	
			qui cap la variable sh_li_`var' ///
				"`l_`var'' (fractile's share of total, linear interpolation)"
			qui cap la variable sh_si_`var' ///
				"`l_`var'' (fractile's share of total, spline interpolation)"	
		}
	}
	
	if "`approach'" == "incidence" {
		*inter-extrapolate eff rates 
		qui foreach var in `vars' {
			bysort ctry_yr: mipolate `var' ftile, gen(ip_`var')
			bysort ctry_yr: mipolate ip_`var' ftile, gen(epf_`var') forward 
			bysort ctry_yr: mipolate epf_`var' ftile, gen(epb_`var') backward 
			quietly drop `var' ip_`var' epf_`var'
			quietly rename epb_`var' `var'
		}
		qui drop country GEO 
	}
	
	//save
	
	if "`approach'" == "concentration" {
		/*
		preserve 
			qui merge 1:1 country year ftile using `tf_new'
			qui egen ftile2 = cut(ftile), at(0(1000)100000) 
			qui ds ctry_yr ftile ftile2 country year country_long ///
				GEO _merge new_marketinco, not
			qui collapse (firstnm) ctry_yr ftile country_long ///
				(sum) `r(varlist)', by(country year ftile2)
			foreach v in education health allcontrib indirectta {
				graph twoway (line sh_li_`v' ftile) ///
					(line sh_li_new_`v' ftile) ///
					if country == "DOM" & ftile!= 100000, ///
					legend(label(1 "interpolated") ///
					label(2 "detailed (new)")) ///
					ytitle("Share of total imputed to percentile") ///
					title("`v'")
				exit 1	
			}
		restore
		*/
	}
	
	foreach i in si li {
		*qui replace sh_`i'_indirectta = sh_`i'_new_indirectta ///
		*	if country == "DOM"
	}
	qui drop new_* 
	cap drop *_new_*
	qui save "${ceq}`approach'/no_ssc.dta", replace 

	* II. Create graphs per country.................................................

	//use aux for country colors
	global aux_part  ""graph_basics"" 
	qui do "code/Do-files/auxiliar/aux_general.do"

	//
	qui replace ftile = ftile / 10^5

	//II. 1 panel graphs..............................

	*qui renvars, trim(16)
	local indicators directtaxe indirectta vat allcontrib ///
		conditiona directtran disposable indirectsu education health

	foreach ind in `indicators'{
		
		local varlabel : var label `ind'
		
		*graph lorenz 
		if "`approach'" == "concentration" {
			graph twoway ///
				(scatter cum_`ind' ftile, mfcolor(none) msize(small)) ///
				(line cumlip_`ind' ftile, lwidth(thick)) ///
				(line cumsip_`ind' ftile) ///
				(function y = x, range(0 1) lcolor(black)) ///
				if !missing(cum_`ind') ///
				, by(country year) aspectratio(1) $graph_scheme ///
				yt("`varlabel'") xt("ftile")  ///
				legend(label(1 "CEQ estimate") ///
				label(2 "linear interpolation") label(3 "Spline interp.") ///
				label(4 "45º line"))
		}
		if "`approach'" == "incidence" {
			graph twoway (scatter `ind' ftile, mfcolor(none)) ///
				if !missing(decile), by(country year) ///
				yt("`varlabel'") xt("ftile") $graph_scheme ///
				legend(label(1 "CEQ") label(2 "Linear interp.")) 
		}
		qui graph export ///
			"figures/ceq/`approach'/panel/no_ssc_`ind'.pdf", replace
	}

	//II. 2. bunched Graphs.............................

	**loop over variables 
	foreach ind in `indicators' {
		
		//loop over countries  
		qui levelsof ctry_yr if !missing(`ind'), local(ctriys) clean 
		foreach cy in `ctriys' {
		
			//details
			local iso = lower(substr("`cy'", 1, 3)) 
			local c "c_`iso'"
			
			if "`approach'" == "concentration" {
				local l_`ind'_`approach' `l_`ind'_`approach'' ///
				(line cumlip_`ind' ftile if ctry_yr == "`cy'", ///
				lcolor($`c') lwidth(thick))	
			}
			if "`approach'" == "incidence" {
				local l_`ind'_`approach' `l_`ind'_`approach'' ///
				(line `ind' ftile if ctry_yr == "`cy'", ///
				lcolor($`c') lwidth(thick))	
			}
		}
			
		//Define title of y-axis 
		local varlabel : var label `ind'
		local ytit "`varlabel'"
		
		//Graph and save without legend
		graph twoway `l_`ind'_`approach'' , ytitle("`ytit'") xtitle("") ///
			$graph_scheme legend(off) 
		qui graph export ///
			"figures/ceq/`approach'/bunched/no_scc_`ind'.pdf", replace	
		
	}	
}

