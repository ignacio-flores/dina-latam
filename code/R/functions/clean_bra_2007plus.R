clean_bra_2007plus <- function(t, fld) {
  
  #choose settings
  if(t <= 2013) {
    sheetname <- "P14_P15_T9"
    rangecoor <- "C11:U22"
  }
  if(t>=2014) {
    sheetname <- "T9_AC2014"
    rangecoor <- "C11:U28"
  } 
  if(t>=2015) sheetname <- "T9"
  if(t>=2016){
    sheetname <- "faixa SM RTT+RTE+RTI"
    rangecoor <- "C9:V26"
  }
  if(t>=2017) rangecoor <- "B8:U26"
  if(t>=2019) rangecoor <- "C11:V28"
  if(t==2020) sheetname <- "Tab9_Fx Rend Total"
  if(t>=2021) {
    sheetname <- "Tab8"
    rangecoor <- "B7:BE959"
  } 
  if (t>=2022) {
    rangecoor <- "A2:BD954"
  }
  
  print(paste0("BRA", t, ". sheetname:", sheetname, ". rangecoor: ", rangecoor))
  
  #clean data 
  excel_file <- paste0(fld, "gn-irpf-ac", t, ".xlsx")
  content <- read_excel(excel_file, sheet = sheetname , range = rangecoor, col_types = c("text"))
  content %<>% clean_names() 
  
  if (t <= 2020) {
    content %<>% 
      dplyr::select(x1, x2, x3, x4, x5, livro_caixa) %>% 
      rename(faixa_in_min_wage = `x1`, n = `x2`) %>% 
      filter(faixa_in_min_wage != "Total") %>% 
      mutate(faixa_in_min_wage = str_replace_all(faixa_in_min_wage, "1/2", "0.5"), 
             faixa_in_min_wage = str_replace_all(faixa_in_min_wage, "Até 0.5", "0")) %>% 
      separate(faixa_in_min_wage, into=c("thr_minwag", "resto"), sep = " a ") %>% 
      mutate(thr_minwag = gsub("[^0-9.-]", "", thr_minwag), year = t, country = "BRA") %>% 
      mutate(thr_minwag=as.numeric(thr_minwag), n=as.numeric(n), x3=as.numeric(x3), x4=as.numeric(x4),
             x5=as.numeric(x5), livro_caixa=as.numeric(livro_caixa)) %>% 
      mutate(inc = (x3 + x4 + x5 - livro_caixa) * 10^6) %>% 
      dplyr::select(country, year, thr_minwag, n, inc) %>% 
      filter(!is.na(thr_minwag) & !is.na(inc))
  } 
  if (t >= 2021) {
    if (t == 2021) {
      content %<>% 
        dplyr::select(x1, x2, x3, x4, x5, x13, x14, livro_caixa) %>%
        rename(tipo = `x1`, uf = `x2`,
               faixa_in_min_wage = `x3`, n = `x4`)
    }
    if (t >= 2022) {
      content %<>% 
        dplyr::select(tipo_formulario, uf, faixa_de_rendim_tributavel_mais_trib_exclusiva_mais_isentos_em_sal_minimos, 
                      qtde_contribuintes, rendimento_tributavel_total, rend_sujeitos_a_tribut_exclusiva, 
                      rend_isentos_e_nao_tributaveis, deducao_livro_caixa) %>%
        rename(tipo = tipo_formulario, faixa_in_min_wage = faixa_de_rendim_tributavel_mais_trib_exclusiva_mais_isentos_em_sal_minimos, 
               n = `qtde_contribuintes`, x5 = `rendimento_tributavel_total`, 
               x13 = `rend_sujeitos_a_tribut_exclusiva`, x14 = `rend_isentos_e_nao_tributaveis`,
               livro_caixa = `deducao_livro_caixa`)
    }
    content %<>% 
      filter(faixa_in_min_wage != "Total") %>% 
      mutate(faixa_in_min_wage = str_replace_all(faixa_in_min_wage, "1/2", "0.5"), 
             faixa_in_min_wage = str_replace_all(faixa_in_min_wage, "Até 0.5", "0")) %>%
      mutate(across(c(n, x5, x13, x14, livro_caixa), as.numeric)) %>%
      select(-c("tipo", "uf")) %>% group_by(faixa_in_min_wage) %>% 
      summarise_all(sum, na.rm=T) %>%
      separate(faixa_in_min_wage, into=c("thr_minwag", "resto"), sep = " a ") %>% 
      mutate(thr_minwag = gsub("[^0-9.-]", "", thr_minwag), year = t, country = "BRA") %>% 
      mutate(thr_minwag=as.numeric(thr_minwag)) %>% ungroup() %>%
      mutate(inc = (x5 + x13 + x14 - livro_caixa))
    if (t == 2021) {
      content %<>% mutate(inc = inc * 10^6)
    }
    
    content %<>% dplyr::select(country, year, thr_minwag, n, inc)
  }
  
  return(content)
}
  
  