*Smooth criminal 
clear all

//set directories
capture cd "C:/Users/Usuario/Dropbox/LATAM-WIL/"
capture cd "~/Dropbox (Personal)/DINA-LatAm/"
capture cd "~/Dropbox/DINA-LatAm/"

//preliminary
global aux_part  ""preliminary"" 
do "code/Do-files/auxiliar/aux_general.do"

*locals 
local c "BRA"
local t = 2009
local weight "_weight" 
local income "${y_pretax_tot_bra}"
local p0 = .9

*open dataset 
quietly use "${svypath}`c'/bfm_norep_pre/`c'_`t'_bfm_norep_pre.dta", clear 

*generate cdf of full distribution 
tempvar freq_full freq_t10 auxinc
quietly sum	`weight', meanonly
local poptot = r(sum)
sort `income'
quietly	gen `freq_full' = `weight' / `poptot'
quietly	gen F = sum(`freq_full'[_n-1])	

*get top 10% average 
quietly sum `income' [w=`weight'] if F >= `p0' 
local a = r(mean)
di as result "a: " round(`a')

*get ranks within top 10% 
quietly sum `weight' if F >= `p0', meanonly 
local popt10 = r(sum)
quietly gen `freq_t10' = `weight' / `popt10' 
quietly gen F_t10 = 1 - sum(`freq_t10'[_n-1]) if F >= `p0'
quietly gen `auxinc' = F_t10 * `income' 

*get b 
quietly sum `auxinc' [w= `weight'], meanonly
local b = r(mean) 
di as result "b: " round(`b')

*get mu (thr) 
quietly sum `income' if F >= `p0', meanonly 
local mu = r(min) 
di as result "mu: " round(`mu') 

*get xi and sigma 
local xi = (`a' - 4*`b' + `mu' ) / (`a' - 2*`b')
local sigma = (`a' - `mu') * (2*`b' - `mu') / (`a' - 2*`b')

di as result "xi: `xi'"
di as result "sigma: `sigma'"

*smooth income 
quietly gen p1 = F 
quietly gen p2 = F[_n+1]
quietly gen smooth_income = `mu' + `sigma'/(p2 - p1)*(-((-1 + `p0')/(-1 + p1))^`xi' - ((-1 + `p0') / (-1 + p2))^`xi'*(-1 + p2) + p2 - p2*`xi' + p1*(-1 + ((-1 + `p0') / (-1 + p1))^`xi' + `xi'))/((-1 + `xi')*`xi') if _n != _N
quietly replace smooth_income = (`mu' *(-1 + `xi')*`xi' - `sigma'*(-1 + ((-1 + `p0')/(-1 + /*p3*/ p1))^`xi' + `xi'))/((-1 + `xi')*`xi') if _n == _N

*quietly gen smooth_income = `mu' + `sigma' / `xi' * (((1 - F)/(1 - `p0'))^(-`xi') - 1)

*check the average is still the same 
quietly sum smooth_income [w=`weight'] if F >= `p0' , meanonly 
local new_mean = r(mean) 
di as result "new mean: `new_mean'"

gen test = smooth_income / ypre_toteqsn_svy_y 
graph twoway (line test F) if F >= .9
exit 1
*new F 
quietly gen F_new = 1-(1+`xi'*((`income'-`mu')/`sigma'))^(-1/`xi') ///
	if F >= `p0'
