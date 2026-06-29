#download all coutnry data SNA for LATAM 
library(readr) 
library(haven)
library(glue)
library(dplyr)
library(XML)
library(httr)
library(rvest)
library(janitor)
library(stringr)
library(stringi)
library(tidyr)
library(magrittr)
library(readxlsb)
library(ggplot2)
library(Hmisc)
library(readxl)
library(xlsx)
setwd("~/Dropbox/DINA-LatAm/")

# I. Download data from National Accounts -----------------------------------------------------------

sna_folder <- "Data/national_accounts/country-data/"

#Brasil (find sector familias)

#Chile (merge the newest sheets manually with the old file)
web <- "https://si3.bcentral.cl/estadisticas/Principal1/Informes/"
filename <- "CEI_anuario_2013-2019.xls"
download.file(file.path(web, "AnuarioCCNN/excel2020/", filename), glue(sna_folder, "CHL/", filename))

#Colombia (name of file can change)
web <- "https://www.dane.gov.co/files"
filename <- "cuentas-economicas-integradas-2019provisional.xls"
download.file(file.path(web, "investigaciones/boletines/pib/cuentas-nal-anuales", 
                        filename), glue(sna_folder, "COL/", filename))  
#Costa Rica 
web <- "https://www.bccr.fi.cr/indicadores-economicos/"
for(t in 2012:2017) {
  if(t<2017) file <- glue("DocCuentasNacionalesProyecto/documentoscnaestadisticas/Cuentas_Economicas_Integradas_{t}.xlsx")
  else file <- glue("DocCuentasNacionales2017/CEI{t}.xlsx")
  download.file(file.path(web, file), glue(sna_folder, "CRI/", "Cuentas_Economicas_Integradas_{t}.xlsx"))
}

#Ecuador 
web <- "https://contenido.bce.fin.ec/documentos/PublicacionesNotas/Catalogo/CuentasNacionales/"
download.file(file.path(web, "CEI2007-2019p.xlsx"), glue(sna_folder, "ECU/", "CEI2007-2019p.xlsx"))
download.file(file.path(web, "ceinivel2sd.xlsx"), glue(sna_folder, "ECU/", "ceinivel2sd.xlsx"))

#Mexico (check files and move manually to the parent file)
web <- "https://www.inegi.org.mx/contenidos/temas/economia/"
zip_file <- tempfile()
zip_dir <- glue(sna_folder, "MEX/")
download.file(file.path(web, "cn/si/tabulados/ori/tabulados_si.zip"), zip_file)
unzip(zip_file, exdir= zip_dir)