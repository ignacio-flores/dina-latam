#clean (and download) tax data  

#setup 
source("code/R/00_libraries.R")
source("code/R/functions/clean_bra_2007plus.R")
libraries(stdlibs)
mode <- "local" #update 

#bring total pop 
popdata <- read_dta("Data/Population/SurveyPop.dta")

# I. BRAZIL------------------------

#arrange estimates before 2007 
bra_file <- "Data/Tax-data/BRA/Raw tabulations/"
bra_tabs_2000_06 <- NULL
for(t in 2000:2007) {
  excel_file <- file.path(bra_file, glue("ptot_", t, ".xlsx"))
  if(file.exists(excel_file)){
    content <- read_excel(excel_file)
    bra_tabs_2000_06 %<>% bind_rows(content)
  } 
}
bra_tabs_2000_06 %<>% rename(popsize = `population`) %>% 
  mutate(component = "pretax")

#download more recent (post 2007)? 
if (mode == "update") {
  source("code/R/functions/bra_admin_downloader.R")
}

#clean admin 2007-present 
bra_tabs <- map_dfr(
  2007:2022,
  ~clean_bra_2007plus(
    t = .x,
    fld = "Data/Tax-data/BRA/Raw tabulations/"
  )
)

#download info on minwages 
wiki_minwage <- "Data/Tax-data/BRA/Raw tabulations/wiki_minwage.csv"
if (mode == "update") {
  source("code/R/functions/bra_minwage_downloader.R")
} 
bra_minwag <- read_csv(wiki_minwage, show_col_types = F)

#merge minwages and tax data  
bra_tabs %<>% full_join(bra_minwag) %>% 
  mutate(thr = thr_minwag * minwage * 12, component = "pretax") %>% 
  filter(!is.na(country)) %>% left_join(popdata) %>% 
  arrange(year, desc(thr)) %>% group_by(year) %>% 
  mutate(freq = n/totpop_ie, p = 1-cumsum(freq), bracketavg = (inc)/n) %>%
  rename(popsize = `totpop_ie`) %>% group_by(year) %>% 
  arrange(year, p) %>% bind_rows(bra_tabs_2000_06)

#save averages for later 
bra_avgs <- summarise(bra_tabs, 
  renta=sum(inc), 
  popsize=mean(popsize)) %>% 
  mutate(average = renta/popsize)
bra_tab_years <- bra_avgs$year
bra_avgs <- bra_avgs$average

#Order as gpinter input
xlsx_file <- "Data/Tax-Data/BRA/gpinter_input/total-pre-BRA.xlsx"
if(file.exists(xlsx_file)) file.remove(xlsx_file)
for(x in 1:length(bra_tab_years)) {
  exptab <- select(ungroup(bra_tabs), year, country, component, popsize, average, p ,thr, bracketavg) %>% 
    filter(year == bra_tab_years[x]) %>% 
    select(year, country, component, popsize, average, p ,thr, bracketavg)
  if(x > 3) {
    exptab %<>% mutate(average=bra_avgs[x]) 
  }
  for(y in 1:length(exptab$p)) {
    if(y>1) {
      exptab$year[y] <- NA
      exptab$country[y] <- NA
      exptab$component[y] <- NA
      exptab$popsize[y] <- NA
      exptab$average[y] <- NA
    } 
  }
  write.xlsx2(exptab, xlsx_file, 
    sheetName=glue(bra_tab_years[x]),
    append=TRUE)
}

#Apply gpinter (manually)


