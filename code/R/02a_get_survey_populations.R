
#setup 
setwd("~/Dropbox/DINA-LatAm/")
source("code/R/00_libraries.R")
libraries(stdlibs)

#load local functions 
source("code/R/functions/get_svy_totpop.R")

# I. Get total populations from surveys ----------------------------------------------------------

countries <- c("ARG", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "MEX", "PER", "SLV", "URY")

#get population data from surveys 
popdata <- map_dfr(countries, 
   ~get_svy_totpop(
     c = .x,
     years = 2000:2022
   )
)

#inter/extra-polate population linearly 
popdata %<>% 
  group_by(country) %>% 
  mutate(
    totpop_i      = approx(x = year, y = totpop, xout = year, method = "linear")$y, 
    pct_adults_i  = approx(x = year, y = pct_adults, xout = year, method = "linear")$y, 
    totpop_ie     = approxExtrap(x = year, y = totpop_i, xout = year, method = "linear")$y, 
    pct_adults_ie = approxExtrap(x = year, y = pct_adults_i, xout = year, method = "linear")$y
  )

#write .dta file 
write_dta(popdata, "Data/Population/SurveyPop.dta")