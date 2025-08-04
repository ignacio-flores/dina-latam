//look for updates here: 
//https://population.un.org/wpp/Download/Standard/Population/

clear

//preliminary 
qui import excel "Data/Population/PopulationAge.xlsx", ///
	sheet("Data") firstrow

//clean 
qui drop Sex Note
qui rename Location country
foreach total of varlist ///
	E F G H I J K L M N O P Q R S T U V ///
	W X Y Z AA AB AC AD AE AF AG AH AI /*AJ*/ { 
	//when updating, add a column in the list above  
   local lab: variable label `total'
   local lab2 = "pop`lab'"
   qui rename `total' `lab2'
}

//sum age groups 
qui gen adults=0
qui replace adults=1 if !inlist(Age, "0-4", "5-9", "10-14", "15-19")
forvalues t = 1990/2020 /*$last_y*/ {
	qui egen totalpop`t' = sum(pop`t'), by(country)
	qui egen adultpop`t' = sum(pop`t') if adults==1, by(country)
	qui drop pop`t'
}

//clean variables 
drop if inlist(Age, "0-4", "5-9", "10-14", "15-19")
drop Age
drop adults
gen region = country if totalpop1990==0
replace region=region[_n-1] if region == ""
drop if totalpop1990==0
keep if country!=country[_n-1]

//reshape and make pretty 
reshape long totalpop adultpop, i(country) j(year)
order country region year totalpop adultpop
lab var region "region in Latin America & Caribbean"
lab var totalpop "Total Population"
lab var adultpop "Adult Population (20+)"
replace totalpop = totalpop * 1000
replace adultpop = adultpop * 1000

//save 
save "Data/Population/PopulationLatAm.dta", replace
