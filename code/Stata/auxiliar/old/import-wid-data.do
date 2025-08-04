///////////////////////////////////////////////////////////////////////////////
//																			 //
//																			 //
//	 MORE UNEQUAL OR NOT AS RICH? The Missing Half of Latin American Income	 //
//			          	De Rosa, Flores & Morgan (2020)						 //
//				    Goal: import additional macro data from wid				 //
//																		     //
///////////////////////////////////////////////////////////////////////////////

//General 
clear all
sysdir set PERSONAL "."
clear programs

//Upload new wid-data
use "~/Dropbox/WID/W2ID/Country-Updates/National_Accounts/Update_2020/wid-data.dta", ///
clear

//Keep Latin American countries
/*
global ctry_condition (iso=="AG" | iso=="AI" | iso=="AR" | iso=="AW" | iso=="BB" ///
	| iso=="BL" | iso=="BO" | iso=="BR" | iso=="BS" | iso=="BQ" | iso=="BZ" ///
	| iso=="CL" | iso=="CO" | iso=="CR" | iso=="CU" | iso=="CW" | iso=="DM" ///
	| iso=="DO" | iso=="EC" | iso=="FK" | iso=="GD" | iso=="GP" | iso=="GT" ///
	| iso=="GY" | iso=="HN" | iso=="HT" | iso=="JM" | iso=="KN" | iso=="KY" ///
	| iso=="LC" | iso=="MF" | iso=="MQ" | iso=="MX" | iso=="NI" | iso=="PA" ///
	| iso=="PE" | iso=="PR" | iso=="PY" | iso=="SR" | iso=="SV" | iso=="SX" ///
	| iso=="TC" | iso=="TT" | iso=="VC" | iso=="VE" | iso=="VG" | iso=="VI" ///
	| iso=="UY")

//Keep relevant variables 
qui var condition (widcode=="mgdpro999i" | widcode=="mnnfin999i" ///
	| widcode=="mnninc999i" | widcode =="mptfrr999i" | widcode=="mptfrp999i" ///
	| widcode=="mccshn999i" | widcode=="mccmhn999i" | widcode=="mcfcco999i" ///
	| widcode=="mconfc999i" | widcode=="mptfhr999i" | widcode=="mgsmhn999i" ///
	| widcode=="mgsrhn999i" | widcode=="mgmxhn999i" | widcode=="mprgco999i")
*/	
	
drop p

// Transform the dataset
reshape wide value, i(iso year) j(widcode) string

//Rename and label variables
qui rename valuemgdpro999i gdp
qui rename valuemnnfin999i nfi
qui rename valuemnninc999i nninc
qui rename valuemptfrr999i re_portf_inv_rec
qui rename valuemptfrp999i re_portf_inv_paid
qui rename valuemccshn999i cfc_hh_surplus
qui rename valuemccmhn999i cfc_hh_mixed
qui rename valuemcfcco999i cfc_corp
qui rename valuemconfc999i cfc_total
qui rename valuemptfhr999i y_cap_tax_havens
qui rename valuemgsmhn999i y_gos_gmix_hh
qui rename valuemgsrhn999i y_gos_hh
qui rename valuemgmxhn999i y_gmix_hh
qui rename valuemprgco999i bpi_corp_wid

qui egen gninc = rowtotal(nninc cfc_total)
qui egen cfc_hh = rowtotal(cfc_hh_surplus cfc_hh_mixed)
qui gen foreign_up_corp = re_portf_inv_rec - re_portf_inv_paid

qui label var gdp "gross domestic product"
qui label var nfi "net foreign income"
qui label var nninc "net national income"
qui label var gninc "gross national income"
qui label var re_portf_inv_rec "reinvested earnings on foreign portfolio investment (received)"
qui label var re_portf_inv_paid "reinvested earnings on foreign portfolio investment (paid)"
qui label var foreign_up_corp "net foreign reinvested earnings on portfolio investment"
qui label var cfc_hh_surplus "personal depreciation on operating surplus"
qui label var cfc_hh_mixed "personal depreciation on mixed income"
qui label var cfc_hh "consumption of fixed capital of households"
qui label var cfc_corp "consumption of fixed capital of corporations"
qui label var cfc_total "consumption of fixed capital of the total economy"
qui label var y_cap_tax_havens "capital income received from tax havens"
qui label var y_gos_gmix_hh "gross operating surplus and mixed income of households"
qui label var y_gos_hh "gross operating surplus of households"
qui label var y_gmix_hh "gross mixed income of households"
qui label var bpi_corp_wid "balance of primary incomes of corporations (wid)"

//Express as shares of target total
qui gen sh_bpi_corp_for = foreign_up_corp / bpi_corp_wid
qui la var sh_bpi_corp_for ///
	"net foreign reinvested earnings on portfolio investment (% of Corp Und. Profits)" 
qui gen sh_cfc_corp = cfc_corp / bpi_corp_wid
qui la var sh_cfc_corp ///
	"consumption of fixed capital of corporations (% of Corp Und. Profits)"
qui gen sh_cfc_hh_surplus = cfc_hh_surplus / y_gos_hh
qui la var sh_cfc_hh_surplus ///
	"Depreciation on operating surplus, HH (% of gross value)"
qui gen sh_cfc_hh_mixed = cfc_hh_mixed / y_gmix_hh
qui la var sh_cfc_hh_mixed ///
	"Depreciation on mixed income (% of gross value)"
qui gen sh_cfc_hh = cfc_hh / y_gos_gmix_hh
qui la var sh_cfc_hh ///
	"Consumption of fixed capital of households (% of MI + OS_HH)"
qui gen sh_cfc_total = cfc_total / gninc
qui la var sh_cfc_total ///
	"Total Consumption of fixed capital (% of Gross National Income)"

qui save "Data/national_accounts/new-wid-data.dta", replace 


