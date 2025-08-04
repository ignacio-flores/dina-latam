///////////////////////////////////////////////////////////////////////////////
//																			 //
//																			 //
//				INCOME INEQUALITY IN LATIN AMERICA REVISITED				 //
//			     	Authors: DE ROSA, FLORES & MORGAN 						 //
//																			 //
//																		     //
///////////////////////////////////////////////////////////////////////////////

//This do file uses SNA data scrapped from http://data.un.org/ (in section 1.1)
//Gov. Balance Sheets from http://data.imf.org/ (in section 1.3)
//using "R/download-raw-un-sna.R", which then was
//cleaned using "R/import-un-sna-data.R"

//Required programs: 
//ssc install kountry
//ssc install wid

//0. PRELIMINARY ------------------------------------------------------------//

//General 
sysdir set PERSONAL "."
clear programs

//Table names
local TOT "Table 4.1 Total Economy (S.1)"
local RoW "Table 4.2 Rest of the world (S.2)"
local NFC "Table 4.3 Non-financial Corporations (S.11)"
local FC "Table 4.4 Financial Corporations (S.12)"
local GG "Table 4.5 General Government (S.13)"
local HH "Table 4.6 Households (S.14)"
local NPISH "Table 4.7 Non-profit institutions serving households (S.15)"
local corps " Non-Financial and Financial Corporations (S.11 + S.12)"
local CORPS "Table 4.8 Combined Sectors`corps'"
local HH_NPISH "Table 4.9 Combined Sectors Households and NPISH (S.14 + S.15)"
local all_IS "TOT RoW NFC FC GG HH NPISH CORPS HH_NPISH"

//1. PREPARE AND CLEAN DATA -------------------------------------------------//

//1.1 UNDATA ----------------------------------------------------------------// 

local iter = 1
tempfile tf_merge1
foreach IS in `all_IS' {
	tempvar auxi1 auxi2
	use "Data/raw-data/un-national-accounts/``IS''.dta", clear
	saveold "Data/raw-data/un-national-accounts/``IS''.dta", replace version(13)
	}
	
