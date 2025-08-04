*drop useless stuff 
qui cap drop __*

//harmonize missing and NA codes for sociodemo vars
foreach v in categ5_p tamest_ee {
	if "`v'" == "categ5_p" local z = 6 
	if "`v'" == "tamest_ee" local z = 3 
	qui replace `v' = `z' if missing(`v')
	qui replace `v' = `z' if inlist(`v', -1, 99, 9)
	qui replace `v' = `z' if `v' == 0 & ///
		inlist("${c}", "SLV", "COL")
}

//0. DEFINE LABELS 

*define ocupation labels 
cap label drop _all 
qui la var categ5_p "Occupation"
qui la define codeocup ///
	1 "Employer" 2 "Employee" 3 "Domestic worker" ///
	4 "Self-employed" 5 "Non remunerated" 6 "No category"
qui la values categ5_p codeocup 

*define public or private sector, CEPAL labels
qui la var sector_ee "Private/Public Sector (CEPAL)"
qui la define codesector1 1 "Public sector" 2 "Private sector"  
qui la values sector_ee codesector1 

*define public or private sector, proxy labels 
label define codesector2 1 "Agriculture" 2 "Mining" ///
	3 "Manufacture" 4 "Energy" 5 "Construction" ///
	6 "Commerce and services" 7 "Transp. or com." ///
	8 "Finance" 9 "Public admin., education, health or soc. serv."
qui la values ramar_ee codesector2 

*define firm size labels (original)
qui la var tamest_ee "Firm size"
qui la define codefirmsize ///
	1 "5 people or less" 2 "More than 5 people" 3 "NA"	
qui la values tamest_ee codefirmsize

*re-define firm size, separating self-employed 
cap drop t1_cat 
qui gen t1_cat = . 
qui replace t1_cat = 1 if categ5_p == 4 
qui replace t1_cat = 2 if tamest_ee == 1 & inlist(categ5_p, 1, 2, 3) 
qui replace t1_cat = 3 if tamest_ee == 2 & inlist(categ5_p, 1, 2, 3) 
qui replace t1_cat = 4 if tamest_ee == 3 & inlist(categ5_p, 1, 2, 3) 
qui replace t1_cat = 4 if tamest_ee == -1 & inlist(categ5_p, 1, 2, 3) 
qui la define cod_t1_proxy ///
	1 "Self-employed" 2 "1-4" 3 "5 or more" 4 "NA"
qui la values t1_cat cod_t1_proxy	

*define identifier for table 2 
cap drop t2_cat 
qui gen t2_cat = . 
qui replace t2_cat = 1 if t1_cat == 1 
qui replace t2_cat = 2 if t1_cat == 2 & inlist(categ5_p, 2, 3)	
qui replace t2_cat = 3 if t1_cat == 2 & inlist(categ5_p, 1)
qui replace t2_cat = 4 if t1_cat == 3 & inlist(categ5_p, 2, 3)
qui replace t2_cat = 5 if t1_cat == 3 & inlist(categ5_p, 1)
qui la define cod_t2_proxy ///
	1 "Self-employed" 2 "1-4 Employee" 3 "1-4 Employer" ///
	4 "5 or more Employee" 5 "5 or more Employer"
qui la values t2_cat cod_t2_proxy	

*cut sample 
global conditions ///
	edad >= 20 & /// is an adult 
	!missing(inc) & inc > 0 & /// has positive income 
	!missing(t1_cat) & t1_cat != 4 //  has a firm size (or self-employed)
	
cap drop validobs_cepal
cap drop validobs_proxy	
qui gen validobs_cepal = 1 if $conditions & sector_ee != 2 
qui gen validobs_proxy= 1 if $conditions & !inlist(ramar_ee, -1, 9, 99)  // does not 
