

gen exclude = 0
replace exclude = 1 if ///
	((country == "CRI" & year < 2010) /*| country == "DOM"*/ | ///
	country == "MEX" & year == 2000) // | (country == "CRI" & year == 2021)
	

global excl_ccyy " "CRI2000", "CRI2001", "CRI2002", "CRI2003", "CRI2004", "CRI2005", "CRI2006", "CRI2007", "CRI2008", "CRI2009", "CRI2021" "

//"DOM2000", "DOM2001", "DOM2002", "DOM2003", "DOM2004", "DOM2005", "DOM2006", "DOM2007", "DOM2008", "DOM2009", "DOM2010", "DOM2011", "DOM2012", "DOM2013", "DOM2014", "DOM2015", "DOM2016", "DOM2017", "DOM2018", "DOM2019", "DOM2020", "DOM2021"



