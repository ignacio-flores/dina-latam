	foreach var in "ingreso_indep_irae" "irae_indep" "ingreso_catiiop" ///
					"irpf_catiiop" "deduc_catiiop" "ing_K_tot_posttax" ///
					"ingresos_iass" "iass" "y_k_alt3" "ing_socios_final" "imp_K_tot" {
 
		replace `var' = 0 if missing(`var') | `var' < 0 
	}

	*Creation of income variables (net of taxes and SS cont. by employees)
	*a) labour income
	gen ind_inc=0
	replace ind_inc=ingreso_indep_irae - irae_indep
	gen lab_inc=0
	replace lab_inc=ind_inc + ingreso_catiiop - irpf_catiiop - deduc_catiiop if fuente==44 | fuente==1144 // dependent workers 
	*b) mixed income
	gen mix_inc=0
	replace mix_inc=ind_inc + ingreso_catiiop - irpf_catiiop - deduc_catiiop if fuente==1146 | fuente==4446 // independent workers
	replace mix_inc=ind_inc if (fuente==0 | missing(fuente)) // independent workers
	*c) capital income
	*gen cap_inc=ing_K_tot_posttax // already posttax
	gen cap_inc= y_k_alt3 + ing_socios_final - imp_K_tot 
	*d) pension income
	gen pen_inc=ingresos_iass - iass 
	*e) total income net income
	gen pre_tot_inc=0
	replace pre_tot_inc=lab_inc + mix_inc + cap_inc + pen_inc 
	
	gen tot_inc = pre_tot_inc // for older versions

	*other variables
	rename nii_retenido_muestra	id
	rename edad					age
	rename sexo					sex

	label var pre_tot_inc	"Total pretax personal income"
	label var ind_inc 		"Indepentent income"
	label var mix_inc 		"mixed income"
	label var cap_inc 		"Total capital income"
	label var pen_inc 		"Pensions"
	label var lab_inc 		"Labour income"
	label var id		 	"Id"
	label var age 			"Age"
	label var sex 			"Sex"
	
	*Taxes paid & employee social security contributions
	gen tot_tax		= 0
	replace tot_tax	= irae_indep + irpf_catiiop + iass + imp_K_tot // total taxes paid
	label var tot_tax "Total taxes paid"
	
	gen ss_cont  	= 0
	replace ss_cont = deduc_catiiop
	label var ss_cont "Employee social security contributions"
	
	gen e_tax_rate 		= 0
	replace e_tax_rate	= tot_tax / pre_tot_inc
	label var e_tax_rate "Effective tax rate"
	
	gen pos_tot_inc 	= 0
	replace pos_tot_inc = pre_tot_inc - tot_tax
	label var pos_tot_inc "Total postax personal income"
	
	gen e_ss_rate 		= 0
	replace e_ss_rate	= ss_cont / pre_tot_inc
	label var e_ss_rate "Effective social security rate"

	*keep important variables
	keep pre_tot_inc pos_tot_inc tot_inc ind_inc mix_inc cap_inc pen_inc lab_inc id age sex tot_tax ss_cont e_tax_rate e_ss_rate y_k_alt3 ing_socios_final imp_K_tot fuente

	*Clean data base
	replace pre_tot_inc = 0 if missing(pre_tot_inc)
	replace pos_tot_inc = 0 if missing(pos_tot_inc) | pos_tot_inc < 0
	keep if pre_tot_inc>0 // I drop all zero incomes (missing population added later on)
	drop if age<20
