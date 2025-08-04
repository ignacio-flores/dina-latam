get_svy_totpop <- function(c, years) {
  
  binder <- NULL
  
  #loop over years 
  for (y in years) {
    
    #fill info only if file exists
    dta_file <- glue("Data/CEPAL/surveys/", c , "/raw/", c, "_", y, "_raw.dta")
    
    if (file.exists(dta_file)==TRUE) {
      
      #summarize population 
      survey <- read_dta(dta_file, col_select = c("_fep", "edad")) 
      survey %<>% 
        clean_names() %>% 
        mutate(adult = ifelse(edad>19, 1, 0)) %>% 
        group_by(adult) %>% 
        summarise(fep = sum(fep)) %>% 
        select(c("fep")) 
      survey <- as_tibble(t(survey)) 
      colnames(survey) <- c("nonadults", "adults")
      survey %<>% 
        mutate(totpop = nonadults + adults, 
               pct_adults=adults/totpop*100, 
               country = c, 
               year = y)
    }
    #or leave empty space 
    else {
      survey <- tibble(country = c, year = y , totpop = NA, pct_adults=NA, adults = NA, nonadults = NA )
    }
    #bind all
    binder %<>% 
      bind_rows(survey) %>% 
      dplyr::select(country, year, totpop, pct_adults, adults, nonadults)
  }
  return(binder)
}