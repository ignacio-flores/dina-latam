#locate folder
bra_file <- "input_data/admin_data/BRA/downloads"
if (!dir.exists(bra_file)) {
  dir.create(bra_file, recursive = TRUE)
  message("Folder created: ", bra_file)
} 

#download PIT data from 2007-2013
web <- "https://www.gov.br/receitafederal/pt-br/acesso-a-informacao/dados-abertos"
for(n in 2007:2013) {
  excel_file <- file.path(bra_file, glue("gn-irpf-ac", n, ".xlsx"))
  if(file.exists(excel_file)) file.remove(excel_file)
  download.file(glue(web, 
                     "/receitadata/estudos-e-tributarios-e-aduaneiros/estudos-e-estatisticas/",
                     "11-08-2014-grandes-numeros-dirpf/", "gn-irpf-ac-", n, ".xlsx"), 
                excel_file)  
}

#Also PIT data from 2014-2019
filenames_bra_14_19 <- list(
  "gn_irpf_ac2014.xlsx", 
  "tabelas-gn-irpf-ac-2015-excel.xlsx",
  "estudo-gn-irpf-ac-2016-excel.xlsx",
  "relatorio-gn-irpf-ac-2017-excel.xlsx",
  "tabelas-gn-irpf-ac-2018-so-tabelas.xlsx",
  "tabelas-irpf-ac2019-publicacao.xlsx")
for(n in 1:length(filenames_bra_14_19)){
  year = 2013 + n
  filename <- unlist(filenames_bra_14_19[n])
  excel_file <- file.path(bra_file, glue("gn-irpf-ac", year, ".xlsx"))
  if(file.exists(excel_file)) file.remove(excel_file)
  download.file(glue(web, 
                     "/receitadata/estudos-e-tributarios-e-aduaneiros/estudos-e-estatisticas/",
                     "11-08-2014-grandes-numeros-dirpf/", filename), 
                excel_file)
}