

* Alternative total income definitions------------------------------------------ 


* label for main income variables-----------------------------------------------
* labor
gen lab_net	= ss_ing_brutos - ss_ing_exentos - ss_ing_no_acumulables
gen lab_rev	= ss_ing_brutos
gen lab_inc	= ss_ing_brutos
label var lab_net "Labour net income"
label var lab_rev "Labour revenue"
label var lab_inc "Labour income"

* rents
gen ren_net	= ar_ing_acumulables
gen ren_rev	= ar_ing_brutos
gen ren_inc	= ar_ing_brutos - 0.5*ar_ded_autorizadas - ar_ded_impuesto_local - ar_ded_predial
label var ren_net "Rent net income"
label var ren_rev "Rent revenue"
label var ren_inc "Rent income"

* interests
gen int_net	= in_real_financiero + in_real_no_financiero + in_real_seguros
gen int_rev	= in_nominal_financiero + in_nominal_no_financiero + in_nominal_seguros // - in_perdida_financiero - in_perdida_no_financiero
gen int_inc	= in_nominal_financiero + in_nominal_no_financiero + in_nominal_seguros // - in_perdida_financiero - in_perdida_no_financiero
label var int_net "Interests net income"
label var int_rev "Interests revenue"
label var int_inc "Interests income"

* prices
gen pri_net	= pr_acumulables
gen pri_rev	= pr_obtenidos
gen pri_inc	= pr_obtenidos
label var pri_net "Prices net income"
label var pri_rev "Prices revenue"
label var pri_inc "Prices income"

* dividends
gen div_net	= div_dividendos_acumulables
gen div_rev	= div_dividendos
gen div_inc	= div_dividendos_acumulables
label var div_net "Dividends net income"
label var div_rev "Dividends revenue"
label var div_inc "Dividends income"

* other incomes		
gen oth_net	= ot_ing_acumulables
gen oth_rev	= ot_ing_brutos
gen oth_inc	= ot_ing_brutos - ot_ded_autorizadas
label var oth_net "Oth. inc. net income"
label var oth_rev "Oth. inc. revenue"
label var oth_inc "Oth. inc. income"

* prof. incomes		
gen pro_net	= ap_utilidad_acumulable
gen pro_rev	= ap_ing_brutos
gen pro_inc	= ap_ing_brutos - 0.5*(ap_ded_autorizadas) - ap_ing_exentos // own definition, not clear
label var pro_net "Prof. inc. net income"
label var pro_rev "Prof. inc. revenue"
label var pro_inc "Prof. inc. income"

if `t' == 2014 {
}
else{
* int. regime		
	gen inr_net	= ri_utilidad_acumulable
	gen inr_rev	= ri_ing_brutos
	gen inr_inc	= ri_ing_brutos - 0.5*(ri_ded_otras) - ri_ded_inversiones - ri_ded_impuesto_local - ri_ptu 
	label var inr_net "Int. Regime net income"
	label var inr_rev "Int. Regime revenue"
	label var inr_inc "Int. Regime income"
}


* act. entrep.
gen aen_net	= ae_utilidad_acumulable
gen aen_rev	= ae_total_de_ing
gen aen_inc	= ae_total_de_ing - ae_ptu - 0.5*(ae_ded_total)  // own definition, not clear
label var aen_net "Ent. act. net income"
label var aen_rev "Ent. act. Regime revenue"
label var aen_inc "Ent. act. Regime income"

* label for main income variables-----------------------------------------------
if `t' == 2014 {
	gen net_income	= lab_net + ren_net + int_net + pri_net + div_net + oth_net + pro_net + aen_net
	gen revenue		= lab_rev + ren_rev + int_rev + pri_rev + div_rev + oth_rev + pro_rev + aen_rev
	gen income		= lab_inc + ren_inc + int_inc + pri_inc + div_inc + oth_inc + pro_inc + aen_inc 
}
else {
	gen net_income	= lab_net + ren_net + int_net + pri_net + div_net + oth_net + pro_net + inr_net + aen_net
	gen revenue		= lab_rev + ren_rev + int_rev + pri_rev + div_rev + oth_rev + pro_rev + inr_rev + aen_rev
	gen income		= lab_inc + ren_inc + int_inc + pri_inc + div_inc + oth_inc + pro_inc + inr_inc + aen_inc 
}

* variables---------------------------------------------------------------------
cap drop income_0 
cap drop income_1 
cap drop income_2 
cap drop income_3 

gen income_0 = 	de_total_de_ing_acumulables
label var income_0 "Default total income"

gen income_1 = 	net_income				
label var income_1 "Net income"
				
gen income_2 = 	revenue				
label var income_2 "Revenue"

gen income_3 = 	income				
label var income_3 "Income"



