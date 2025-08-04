*********************
gen exclude=0
replace exclude=1 if (	 (country=="CHL" & year==2001) ///
						|(country=="CHL" & year==2002) ///
						|(country=="CHL" & year==2004) ///
						|(country=="CHL" & year==2005) ///
						|(country=="CHL" & year==2007) ///
						|(country=="CHL" & year==2008) ///
						|(country=="CHL" & year==2010) ///
						|(country=="CHL" & year==2012) ///
						|(country=="CHL" & year==2014) ///
						|(country=="CHL" & year==2016) ///
						|(country=="MEX" & year==2001) ///
						|(country=="MEX" & year==2003) ///
						|(country=="MEX" & year==2005) ///
						|(country=="MEX" & year==2007) ///
						|(country=="MEX" & year==2009) ///
						|(country=="MEX" & year==2011) ///
						|(country=="MEX" & year==2013) ///
						|(country=="MEX" & year==2015) ///
						|(country=="MEX" & year==2017) ///
						|(country=="MEX" & year==2018))
drop if exclude==1
*********************	
