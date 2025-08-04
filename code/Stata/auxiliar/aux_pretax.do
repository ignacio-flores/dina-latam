
* Auxiliary dofile: creates fractiles in survey to prepare merge

if $section == "fractiles" {
	*qui keep if edad > 19 & edad != .
	cap drop fractiles 
	cap drop ftile
	sort ind_${prefix}_tot
	qui gen fractiles = 0
	recast float fractiles
					
	qui cap replace _fep=round(_fep) 

	cap drop aux
	qui cap xtile aux = ind_${prefix}_tot [fw=_fep], n(100)
	qui cap replace fractiles = aux / 100
					
	cap drop aux
	qui cap xtile aux = ind_${prefix}_tot [fw=_fep] ///	
		if fractiles == 1, n(10)
	qui cap replace fractiles = 0.99 + ((aux) / 1000) ///
		if fractiles == 1
						
	cap drop aux
	qui cap xtile aux = ind_${prefix}_tot [fw=_fep] ///	
		if fractiles == 1, n(10)
	qui cap replace fractiles = 0.999 + ((aux) / 10000) ///
		if fractiles == 1

	cap drop aux
	qui cap xtile aux = ind_${prefix}_tot [fw=_fep] ///	
		if fractiles == 1, n(10)
	qui cap replace fractiles = 0.9999 + ((aux) / 100000) ///
		if fractiles == 1
					
	cap drop p_merge
	qui gen p_merge = round(fractiles, .00001)
	
	qui replace p_merge = round(p_merge*10000)
	qui drop if p_merge > 9999
}


* adjustment to remove fill missings fror tax eff. tax rates
if $section == "adjustment" {
	sort ind_${prefix}_tot
	cap drop nonmiss last last_rate
	qui gen nonmiss = !missing(ind_${prefix}_tot)
	qui replace nonmiss = sum(nonmiss)
	qui gen sort = _n
	qui sum  nonmis, meanonly
	qui bys nonmis (sort) : gen byte last = _n == 1 if nonmis == r(max)
	qui sum eff_tax_rate_ipol if last == 1
	local last_rate = r(sum) 
	qui cap replace eff_tax_rate_ipol = `last_rate' if eff_tax_rate_ipol == .
	cap drop last nonmiss sort
}
