#2023 version 
content <- read_html(file.path("https://es.wikipedia.org/wiki/Anexo:Salario_m%C3%ADnimo_en_Brasil"))
tables <- content %>% html_table(fill = TRUE, dec = ",", header=TRUE)
bra_minwag <- tables[[3]] %>% 
  clean_names() %>% 
  mutate(valor_r = gsub("[^0-9.-]", "", valor_r)) %>% 
  separate(vigencia, into=c("dia", "mes", "year"), sep = " de ") %>% 
  rename(minwage = `valor_r`) %>% 
  select(year, minwage) %>% 
  mutate(year = as.numeric(year), minwage = as.numeric(minwage))
write_csv(bra_minwag, wiki_minwage)