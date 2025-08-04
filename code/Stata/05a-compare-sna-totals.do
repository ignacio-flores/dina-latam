////////////////////////////////////////////////////////////////////////////////
//
// 							Title: SCALING FACTORS 
// 			 Authors: Mauricio DE ROSA, Ignacio FLORES, Marc MORGAN 
// 									Year: 2020
//
// 			Description:
// 			Compares income aggregates to totals in national accounts 
// 			at every step of the procedure. The output is sent to an 
// 			excel file, which is then used as input for graphs in another 
//			dofile
//
////////////////////////////////////////////////////////////////////////////////

//General 
clear all
clear programs

//preliminary
global aux_part  ""preliminary"" 
quietly do "code/Do-files/auxiliar/aux_general.do"

//find programs folder 
sysdir set PERSONAL "${adofile}snacompare_ado/." 

//Run program 
foreach step in /*"urb"*/ "raw" "bfm_norep_pre" {  

	//define countries 
	if ("`step'" == "urb") global area ARG
	if ("`step'" != "urb") global area " ${really_all_countries} "

	//define weights
 	if inlist("`step'", "raw", "urb") local weight "_fep"
	if ("`step'" == "bfm_norep_pre") local weight "_weight"
	
	*report step
	display as text "{hline 55}"
	di as result "Comparing micro-macro aggregates: `step'"
	display as text "{hline 55}"
	 
	foreach pop in $snapops {
		
		if "`pop'" == "adults" local age 20
		if "`pop'" == "totpop" local age 0
		if "`pop'" == "active" local age 20 65
		
		//call program	
		snacompare using "${sna_folder}UNDATA-WID-Merged.dta", ///
			svypath("${svypath}") weight(`weight') ///
			time(${first_y} ${last_y}) edad(`age') ///
			area(${area}) type(`step') /*show esp GRAPHEXTRAP*/ ///
			exportexcel("${summary}snacompare_`step'_`pop'.xlsx") ///
			auxiliary("${auxpath}aux_snacompare.do") 	
	}
}
