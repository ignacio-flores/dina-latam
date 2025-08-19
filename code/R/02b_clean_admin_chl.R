#clean (and download) tax data  

mode <- "local"  

#load packages
suppressMessages(suppressPackageStartupMessages({
  required_packages <- c("openxlsx","readxl", "xlsx", "ggplot2", "haven", "magrittr", "dplyr", "readr", "janitor", "glue","tidyr", "rvest", "stringr")
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  for (pkg in required_packages) {
    if (!suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}))

#read config file 
lines <- readLines("_config.do")
config <- list()
for (line in lines) {
  if (grepl("^\\s*global\\s+", line)) {
    parts <- strcapture("^\\s*global\\s+([a-zA-Z0-9_]+)\\s+\"?(.+?)\"?$", line, proto = list(name = "", value = ""))
    config[[parts$name]] <- parts$value
  }
}
config$last_year <- as.integer(config$last_y)


#bring total pop 
popdata <- read_dta("intermediary_data/population/SurveyPop.dta")

# II.2 Chile.............
last_y = 2023 
#xlrang <- "A8:K141"

if (mode == "update") {
  #download personal income tax (PIT) data and clean 
  web <- "https://www.sii.cl/sobre_el_sii/estadisticas/personas_naturales"
  tfile <- tempfile()
  download.file(
    file.path(web, paste0("PUB_Total.xlsb")), 
    tfile
  )
}
if (mode == "local") {
  tfile <- paste0("input_data/admin_data/CHL/PUB_Total_", last_y, ".xlsx")
}

#clean data 
raw_tabs <- openxlsx::read.xlsx(tfile, startRow = 8, cols = 0:11, sheet = "Datos") %>% 
  clean_names() %>%
  select(ano_comercial, tramo_de_rentas, n_de_personas_3, renta_determinada_millones_de_pesos_3, impuesto_determinado_millones_de_pesos_3) %>%
  rename(year = `ano_comercial`, 
         tramo = `tramo_de_rentas`, 
         personas = `n_de_personas_3`, 
         renta = `renta_determinada_millones_de_pesos_3`, 
         impuesto = `impuesto_determinado_millones_de_pesos_3`) %>%
  separate(tramo, into = c("a", "tramo"), sep = "-") %>% 
  separate(tramo, into = c("tramo_uta", "b"), sep = "a") %>%
  mutate(tramo_uta = str_replace_all(tramo_uta, " Más de ", "")) %>% 
  mutate(tramo_uta = str_replace_all(tramo_uta, ",", ".")) %>% 
  select(c("year", "tramo_uta", "personas", "renta", "impuesto")) %>% 
  mutate(tramo_uta = str_replace_all(tramo_uta, "UTA [:punct:]T", ""))

#download UTA (Unidad Tributaria Anual) values 
uta = NULL
web <- "https://www.sii.cl"
print("Listing UTA (Dec.)")
for(t in 2005:last_y) {
  print(t)
  if(t < 2013) webit <- "pagina/valores"
  else webit <- "valores_y_fechas"
  content <- read_html(file.path(web, webit, glue("utm/utm{t}.htm#")))
  tables <- content %>% html_table(fill = TRUE, dec = ",")
  uta_table <- tables[[1]] %>% 
    clean_names() 
  uta_table <- uta_table[str_detect(uta_table[, paste0("x", t), drop = TRUE], "Diciembre"), ] %>% 
    rename(uta = 3) %>% select(3) %>% mutate(year = t) 
  uta %<>% bind_rows(uta_table)
}

#merge PIT, UTA and population data
chl_tabs <- full_join(raw_tabs, uta, by = "year") %>% 
  mutate(uta = str_replace_all(uta, coll("."), ""), 
         tramo_uta = parse_number(tramo_uta), uta = parse_number(uta)) %>% 
  mutate(thr = tramo_uta*uta, eff_tax = impuesto/renta*100, country = "CHL")  %>% 
  left_join(popdata) %>% 
  arrange(year, desc(thr)) %>% 
  group_by(year) %>% 
  mutate(freq = personas/totpop_ie, p = 1-cumsum(freq), cum = cumsum(freq)) %>% 
  arrange(year, thr) 

#Prepare tabulation from 1999 to adjust for deductions 
chl_pop1999 <- read_dta("input_data/population/PopulationLatAm.dta")
chl_pop1999 %<>% clean_names() %>% 
  filter(str_detect(country, "Chile"), year == 1999) %>% 
  select(totalpop) %>% 
  mutate(country="CHL")
chl_tab1999 <- read_excel("input_data/admin_data/CHL/tab_gc_1991_2000.xls", 
                          sheet = "Global AT2000", range = cell_rows(3:73), col_names = TRUE)
chl_tab1999 %<>% 
  clean_names() %>% 
  rename(
    piso = `piso_tramo_en_pesos`, 
    techo = `techo_tramo_en_pesos`) %>% 
  select(numero, x1, techo, piso, sum_c158_n, sum_c158, sum_c170, sum_c170_n, sum_c165, sum_c166, sum_c169) %>% 
  mutate(x1 = str_replace_all(x1, "más de ", ""), x1 = parse_number(x1)) %>% 
  rename(tramo = `x1`) %>% 
  mutate(country = "CHL") %>% 
  left_join(chl_pop1999) %>% 
  arrange(desc(tramo), 
          desc(sum_c170_n)) %>% 
  mutate(freq = numero / totalpop, 
         p = 1-cumsum(freq))

#Ensure minimal size of brackets 
minbrack <- 0.0005
check <- sum(chl_tab1999$freq < minbrack)
while(check > 0) {
  chl_tab1999 %<>% 
    mutate(queue = if_else(freq < minbrack , 1, 0), 
           queue = if_else(queue == 1, cumsum(queue), 0), 
           bracket = row_number(), 
           newbracket = if_else(freq < minbrack, lead(bracket, 1), integer(1)), 
           newbracket = if_else(bracket == length(bracket) & freq < minbrack, lag(bracket, 1), newbracket),
           bracket = if_else(queue == 1 & freq < minbrack, newbracket, bracket)) %>% 
    group_by(bracket) %>% 
    summarise(
      tramo=min(tramo), 
      sum_c170_n=sum(sum_c170_n), 
      sum_c158_n=sum(sum_c158_n),
      sum_c165=sum(sum_c165),
      sum_c166=sum(sum_c166),
      sum_c169=sum(sum_c169),
      numero=sum(numero),
      techo=max(techo), 
      piso=min(piso), 
      p=min(p), 
      freq = sum(freq))  
  check <- sum(chl_tab1999$freq < minbrack)
  print(check)
}

#compute deduction rates from 1999 
chl_tab1999 %<>% 
  mutate(factor = (sum_c158_n+sum_c165+sum_c166+sum_c169)/sum_c170_n, 
         factor_old = sum_c158_n/sum_c170_n) %>% 
  filter(techo != 0 & piso != 0) %>% 
  select(p, factor)

#plot deductions 
ggplot(chl_tab1999, aes(p, factor)) + geom_line() + geom_point() + theme_bw()

#assume constancy beyond support 
add_to_1999 <- tribble(
  ~p, ~factor,
  0, chl_tab1999$factor[length(chl_tab1999$p)], 
  1, chl_tab1999$factor[1],
)
chl_tab1999 %<>% bind_rows(add_to_1999) %>% 
  arrange(p)
#cap value 
cap1999 <- chl_tab1999$p[length(chl_tab1999$p)-1]

#apply adjustment to recent data 
mean_factors <- NULL
for(t in 2005:last_y) {
  reduced_tab <- filter(ungroup(chl_tabs), year == t) %>% 
    mutate(factor = 0) %>% 
    select(year, p)
  reduced_vec <- reduced_tab$p
  for(x in 1:length(reduced_vec)){
    #current and lead values 
    pe <- reduced_vec[x]
    pe_lead <- reduced_vec[x+1]
    if(!is.na(pe)){
      if(is.na(pe_lead)) pe_lead <- 1 
      #get average for corresponding group in 1999 
      if(pe < chl_tab1999$p[2]) reduced_1999 <- filter(chl_tab1999, p <= pe) #used to be if (x == 1) reduced_1999 <- filter(chl_tab1999, p <= pe) 
      if(pe > cap1999) reduced_1999 <- filter(chl_tab1999, p >= pe) #used to be if (x == length(reduced_vec)) reduced_1999 <- filter(chl_tab1999, p >= pe)
      if(pe >= chl_tab1999$p[2] & pe <= cap1999) reduced_1999 <- filter(chl_tab1999, p >= pe & p < pe_lead) #if(x > 1 & x < length(reduced_vec)) reduced_1999 <- filter(chl_tab1999, p >= pe & p < pe_lead)
      #define factor
      reduced_tab$factor[x] <- mean(reduced_1999$factor)
    }
  }
  #append years 
  mean_factors <- bind_rows(reduced_tab, mean_factors) %>% 
    arrange(year, p)
}
#add info to tabulated data
chl_tabs %<>% full_join(mean_factors, by=c("year", "p"))

#graph effective tax rates 
chl_tabs %<>% mutate(
  renta_adj_pre=renta*factor,
  renta_adj_pos=renta_adj_pre-impuesto,
  eff_tax_adj_pre=impuesto/renta_adj_pre*100,
  eff_tax_adj_pos=impuesto/renta_adj_pos*100,
  component_pre = "pretax",
  component_pos = "postax",
  bracketavg_pre=renta_adj_pre/personas*10^6,
  bracketavg_pos=renta_adj_pos/personas*10^6) %>% 
  rename(popsize=`totpop_ie`) 

#estimate averages for later
chl_avgs <- summarise(chl_tabs, 
                      renta_pre=sum(renta_adj_pre),
                      renta_pos=sum(renta_adj_pos),
                      popsize=mean(popsize)) %>% 
  mutate(average_pre = renta_pre/popsize * 10^6,
         average_pos = renta_pos/popsize * 10^6)
chl_tab_years <- chl_avgs$year
chl_avgs_pre <- chl_avgs$average_pre
chl_avgs_pos <- chl_avgs$average_pos

#Adjustment from Fairfield & Jorratt (2016) - Potential use 
fj_a6 <- tribble(
  ~year,  ~top1, ~top01, ~top001, 
  "2005", 1.010,  1.016,   1.045,
  "2009", 1.009,  1.034,   1.058,
  "mean", 1.010,  1.025,   1.051,
)

#Order as gpinter input (pretax)
folder_path <- "input_data/admin_data/CHL/_clean"
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
  message("Folder created: ", folder_path)
} 
xlsx_file <- "input_data/admin_data/CHL/_clean/total-pre-CHL.xlsx"
if(file.exists(xlsx_file)) file.remove(xlsx_file)
for(x in 1:length(chl_tab_years)) {
  exptab <- select(ungroup(chl_tabs), year, country, component_pre, popsize, p ,thr, bracketavg_pre) %>% 
    filter(year == chl_tab_years[x]) %>% 
    mutate(average=chl_avgs_pre[x]) %>% 
    rename(bracketavg = `bracketavg_pre`, component = `component_pre`) %>% 
    select(year, country, component, popsize, average, p ,thr, bracketavg)
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
              sheetName=glue(chl_tab_years[x]),
              append=TRUE)
}

#Order as gpinter input (postax)
xlsx_file <- "input_data/admin_data/CHL/_clean/total-pos-CHL.xlsx"
if(file.exists(xlsx_file)) file.remove(xlsx_file)
for(x in 1:length(chl_tab_years)) {
  exptab <- select(ungroup(chl_tabs), year, country, component_pos, popsize, p ,thr, bracketavg_pos) %>% 
    filter(year == chl_tab_years[x]) %>% 
    mutate(average=chl_avgs_pos[x]) %>% 
    rename(bracketavg = `bracketavg_pos`, component = `component_pos`) %>% 
    select(year, country, component, popsize, average, p ,thr, bracketavg)
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
              sheetName=glue(chl_tab_years[x]),
              append=TRUE)
}

#Apply gpinter manually


