# Isolated candidate PIT admin cleaners for the experimental include workflow.
#
# These functions intentionally duplicate/refactor legacy CHL/BRA cleaner logic
# into an experimental namespace. They do not modify or replace the legacy
# pipeline scripts.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

admin_pit_candidate_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

admin_pit_candidate_load <- function(packages) {
  invisible(lapply(packages, admin_pit_candidate_need))
}

admin_pit_candidate_read_config <- function(
  repo_root = getwd(),
  config_path = Sys.getenv("DINA_CONFIG_YML", unset = file.path(repo_root, "config", "dina.yml")),
  override_path = Sys.getenv("DINA_CONFIG_OVERRIDE_YML", unset = "")
) {
  admin_pit_candidate_need("yaml")
  base <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
  merge_config <- function(x, y) {
    if (!is.list(x) || !is.list(y)) return(y)
    out <- x
    for (name in names(y)) {
      if (!is.null(out[[name]]) && is.list(out[[name]]) && is.list(y[[name]])) {
        out[[name]] <- merge_config(out[[name]], y[[name]])
      } else {
        out[[name]] <- y[[name]]
      }
    }
    out
  }
  if (nzchar(override_path) && file.exists(override_path)) {
    base <- merge_config(base, yaml::read_yaml(override_path))
  }
  base$first_y <- base$years$first
  base$last_y <- base$years$last
  base
}

admin_pit_candidate_read_workbook <- function(path, sheet, range = NULL, col_names = TRUE, col_types = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "xlsb")) {
    admin_pit_candidate_need("readxlsb")
    return(readxlsb::read_xlsb(path, sheet = sheet, range = range, col_names = col_names))
  }
  admin_pit_candidate_need("readxl")
  readxl::read_excel(path, sheet = sheet, range = range, col_names = col_names, col_types = col_types)
}

admin_pit_candidate_workbook_years <- function(path, sheet = "Datos", range = "A8:A5000") {
  years <- suppressWarnings(as.integer(unlist(admin_pit_candidate_read_workbook(path, sheet, range, col_names = TRUE), use.names = FALSE)))
  sort(unique(years[!is.na(years) & years >= 1900L & years <= 2099L]))
}

admin_pit_candidate_select_chl_pit <- function(input_root, target_year) {
  files <- c(
    Sys.glob(file.path(input_root, "input_data", "admin_data", "CHL", "PUB_Total_*.xlsb")),
    Sys.glob(file.path(input_root, "input_data", "admin_data", "CHL", "PUB_Total_*.xlsx"))
  )
  files <- unique(files[file.exists(files)])
  if (!length(files)) {
    stop("No Chile PIT PUB_Total workbook found in staged admin data.", call. = FALSE)
  }
  coverage <- do.call(rbind, lapply(files, function(path) {
    years <- tryCatch(admin_pit_candidate_workbook_years(path), error = function(e) integer())
    data.frame(
      path = path,
      first_year = if (length(years)) min(years) else NA_integer_,
      last_year = if (length(years)) max(years) else NA_integer_,
      covers_target = target_year %in% years,
      stringsAsFactors = FALSE
    )
  }))
  hits <- coverage[coverage$covers_target %in% TRUE, , drop = FALSE]
  if (!nrow(hits)) {
    stop(sprintf("Chile PIT workbook coverage does not include target year %s.", target_year), call. = FALSE)
  }
  hits <- hits[order(hits$last_year, decreasing = TRUE), , drop = FALSE]
  hits$path[[1L]]
}

admin_pit_candidate_write_sheeted_workbook <- function(path, tables, sheet_names) {
  admin_pit_candidate_need("openxlsx")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) file.remove(path)
  wb <- openxlsx::createWorkbook()
  for (i in seq_along(tables)) {
    sheet <- as.character(sheet_names[[i]])
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, tables[[i]])
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

admin_pit_candidate_read_chl_uta <- function(input_root, years) {
  admin_pit_candidate_load(c("readr", "dplyr", "stringr", "rvest", "janitor", "glue"))
  materialized <- file.path(input_root, "input_data", "admin_data", "CHL", "uta_december.csv")
  if (file.exists(materialized)) {
    uta <- readr::read_csv(materialized, show_col_types = FALSE)
    names(uta) <- tolower(names(uta))
    if (!all(c("year", "uta") %in% names(uta))) {
      stop("Materialized Chile UTA input must contain year and uta columns: ", materialized, call. = FALSE)
    }
    return(uta[uta$year %in% years, c("uta", "year")])
  }
  uta <- NULL
  web <- "https://www.sii.cl"
  for (year in years) {
    webit <- if (year < 2013) "pagina/valores" else "valores_y_fechas"
    content <- rvest::read_html(file.path(web, webit, glue::glue("utm/utm{year}.htm#")))
    tables <- rvest::html_table(content, fill = TRUE, dec = ",")
    uta_table <- janitor::clean_names(tables[[1]])
    uta_table <- uta_table[stringr::str_detect(uta_table[, paste0("x", year), drop = TRUE], "Diciembre"), ]
    uta_table <- dplyr::rename(uta_table, uta = 3)
    uta_table <- dplyr::select(uta_table, 3)
    uta_table$year <- year
    uta <- dplyr::bind_rows(uta, uta_table)
  }
  uta
}

admin_pit_candidate_clean_chl <- function(
  repo_root = getwd(),
  input_root = repo_root,
  output_root = input_root,
  config_path = Sys.getenv("DINA_CONFIG_YML", unset = file.path(repo_root, "config", "dina.yml")),
  override_path = Sys.getenv("DINA_CONFIG_OVERRIDE_YML", unset = "")
) {
  admin_pit_candidate_load(c("dplyr", "tidyr", "readr", "readxl", "haven", "janitor", "stringr", "glue", "openxlsx"))
  config <- admin_pit_candidate_read_config(repo_root, config_path, override_path)
  last_y <- as.integer(config$years$last)
  tfile <- admin_pit_candidate_select_chl_pit(input_root, last_y)
  coverage_years <- admin_pit_candidate_workbook_years(tfile)
  target_years <- coverage_years[coverage_years <= last_y]

  popdata <- haven::read_dta(file.path(repo_root, "intermediary_data", "population", "SurveyPop.dta"))
  raw_tabs <- admin_pit_candidate_read_workbook(tfile, sheet = "Datos", range = "A8:K5000", col_names = TRUE)
  raw_tabs <- raw_tabs |>
    janitor::clean_names() |>
    dplyr::select(ano_comercial, tramo_de_rentas, n_de_personas_3, renta_determinada_millones_de_pesos_3, impuesto_determinado_millones_de_pesos_3) |>
    dplyr::rename(
      year = ano_comercial,
      tramo = tramo_de_rentas,
      personas = n_de_personas_3,
      renta = renta_determinada_millones_de_pesos_3,
      impuesto = impuesto_determinado_millones_de_pesos_3
    ) |>
    tidyr::separate(tramo, into = c("a", "tramo"), sep = "-", fill = "right", extra = "merge") |>
    tidyr::separate(tramo, into = c("tramo_uta", "b"), sep = "a", fill = "right", extra = "merge") |>
    dplyr::mutate(
      tramo_uta = stringr::str_replace_all(tramo_uta, " Más de ", ""),
      tramo_uta = stringr::str_replace_all(tramo_uta, ",", "."),
      tramo_uta = stringr::str_replace_all(tramo_uta, "UTA [:punct:]T", "")
    ) |>
    dplyr::select(year, tramo_uta, personas, renta, impuesto) |>
    dplyr::filter(year %in% target_years)

  uta <- admin_pit_candidate_read_chl_uta(input_root, target_years[target_years >= 2005])
  chl_tabs <- dplyr::full_join(raw_tabs, uta, by = "year") |>
    dplyr::mutate(
      uta = stringr::str_replace_all(as.character(uta), stringr::coll("."), ""),
      tramo_uta = readr::parse_number(as.character(tramo_uta)),
      uta = readr::parse_number(as.character(uta)),
      thr = tramo_uta * uta,
      eff_tax = impuesto / renta * 100,
      country = "CHL"
    ) |>
    dplyr::left_join(popdata, by = c("country", "year")) |>
    dplyr::arrange(year, dplyr::desc(thr)) |>
    dplyr::group_by(year) |>
    dplyr::mutate(freq = personas / totpop_ie, p = 1 - cumsum(freq), cum = cumsum(freq)) |>
    dplyr::arrange(year, thr)

  chl_pop1999 <- haven::read_dta(file.path(repo_root, "input_data", "population", "PopulationLatAm.dta"))
  chl_pop1999 <- chl_pop1999 |>
    janitor::clean_names() |>
    dplyr::filter(stringr::str_detect(country, "Chile"), year == 1999) |>
    dplyr::select(totalpop) |>
    dplyr::mutate(country = "CHL")
  chl_tab1999 <- readxl::read_excel(
    file.path(input_root, "input_data", "admin_data", "CHL", "tab_gc_1991_2000.xls"),
    sheet = "Global AT2000",
    range = readxl::cell_rows(3:73),
    col_names = TRUE
  )
  chl_tab1999 <- chl_tab1999 |>
    janitor::clean_names() |>
    dplyr::rename(piso = piso_tramo_en_pesos, techo = techo_tramo_en_pesos) |>
    dplyr::select(numero, x1, techo, piso, sum_c158_n, sum_c158, sum_c170, sum_c170_n, sum_c165, sum_c166, sum_c169) |>
    dplyr::mutate(x1 = stringr::str_replace_all(x1, "mas de |más de ", ""), x1 = readr::parse_number(x1)) |>
    dplyr::rename(tramo = x1) |>
    dplyr::mutate(country = "CHL") |>
    dplyr::left_join(chl_pop1999, by = "country") |>
    dplyr::arrange(dplyr::desc(tramo), dplyr::desc(sum_c170_n)) |>
    dplyr::mutate(freq = numero / totalpop, p = 1 - cumsum(freq))

  minbrack <- 0.0005
  check <- sum(chl_tab1999$freq < minbrack, na.rm = TRUE)
  while (check > 0) {
    chl_tab1999 <- chl_tab1999 |>
      dplyr::mutate(
        queue = dplyr::if_else(freq < minbrack, 1, 0),
        queue = dplyr::if_else(queue == 1, cumsum(queue), 0),
        bracket = dplyr::row_number(),
        newbracket = dplyr::if_else(freq < minbrack, dplyr::lead(bracket, 1), integer(1)),
        newbracket = dplyr::if_else(bracket == length(bracket) & freq < minbrack, dplyr::lag(bracket, 1), newbracket),
        bracket = dplyr::if_else(queue == 1 & freq < minbrack, newbracket, bracket)
      ) |>
      dplyr::group_by(bracket) |>
      dplyr::summarise(
        tramo = min(tramo),
        sum_c170_n = sum(sum_c170_n),
        sum_c158_n = sum(sum_c158_n),
        sum_c165 = sum(sum_c165),
        sum_c166 = sum(sum_c166),
        sum_c169 = sum(sum_c169),
        numero = sum(numero),
        techo = max(techo),
        piso = min(piso),
        p = min(p),
        freq = sum(freq),
        .groups = "drop"
      )
    check <- sum(chl_tab1999$freq < minbrack, na.rm = TRUE)
  }

  chl_tab1999 <- chl_tab1999 |>
    dplyr::mutate(factor = (sum_c158_n + sum_c165 + sum_c166 + sum_c169) / sum_c170_n) |>
    dplyr::filter(techo != 0 & piso != 0) |>
    dplyr::select(p, factor)
  add_to_1999 <- data.frame(p = c(0, 1), factor = c(chl_tab1999$factor[length(chl_tab1999$p)], chl_tab1999$factor[1]))
  chl_tab1999 <- dplyr::bind_rows(chl_tab1999, add_to_1999) |>
    dplyr::arrange(p)
  cap1999 <- chl_tab1999$p[length(chl_tab1999$p) - 1]

  mean_factors <- NULL
  for (year in target_years[target_years >= 2005]) {
    reduced_tab <- dplyr::filter(dplyr::ungroup(chl_tabs), year == !!year) |>
      dplyr::mutate(factor = 0) |>
      dplyr::select(year, p)
    reduced_vec <- reduced_tab$p
    for (i in seq_along(reduced_vec)) {
      pe <- reduced_vec[[i]]
      pe_lead <- reduced_vec[i + 1L]
      if (!is.na(pe)) {
        if (is.na(pe_lead)) pe_lead <- 1
        if (pe < chl_tab1999$p[2]) reduced_1999 <- dplyr::filter(chl_tab1999, p <= pe)
        if (pe > cap1999) reduced_1999 <- dplyr::filter(chl_tab1999, p >= pe)
        if (pe >= chl_tab1999$p[2] & pe <= cap1999) reduced_1999 <- dplyr::filter(chl_tab1999, p >= pe & p < pe_lead)
        reduced_tab$factor[[i]] <- mean(reduced_1999$factor)
      }
    }
    mean_factors <- dplyr::bind_rows(reduced_tab, mean_factors) |>
      dplyr::arrange(year, p)
  }

  chl_tabs <- dplyr::full_join(chl_tabs, mean_factors, by = c("year", "p")) |>
    dplyr::mutate(
      renta_adj_pre = renta * factor,
      renta_adj_pos = renta_adj_pre - impuesto,
      component_pre = "pretax",
      component_pos = "postax",
      bracketavg_pre = renta_adj_pre / personas * 10^6,
      bracketavg_pos = renta_adj_pos / personas * 10^6
    ) |>
    dplyr::rename(popsize = totpop_ie)

  chl_avgs <- chl_tabs |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      renta_pre = sum(renta_adj_pre, na.rm = TRUE),
      renta_pos = sum(renta_adj_pos, na.rm = TRUE),
      popsize = mean(popsize, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(average_pre = renta_pre / popsize * 10^6, average_pos = renta_pos / popsize * 10^6)
  chl_tab_years <- chl_avgs$year
  pre_tables <- list()
  pos_tables <- list()
  for (i in seq_along(chl_tab_years)) {
    year <- chl_tab_years[[i]]
    pre <- dplyr::filter(dplyr::ungroup(chl_tabs), year == !!year) |>
      dplyr::mutate(average = chl_avgs$average_pre[[i]]) |>
      dplyr::transmute(year, country, component = component_pre, popsize, average, p, thr, bracketavg = bracketavg_pre)
    pos <- dplyr::filter(dplyr::ungroup(chl_tabs), year == !!year) |>
      dplyr::mutate(average = chl_avgs$average_pos[[i]]) |>
      dplyr::transmute(year, country, component = component_pos, popsize, average, p, thr, bracketavg = bracketavg_pos)
    if (nrow(pre) > 1L) pre[2:nrow(pre), c("year", "country", "component", "popsize", "average")] <- NA
    if (nrow(pos) > 1L) pos[2:nrow(pos), c("year", "country", "component", "popsize", "average")] <- NA
    pre_tables[[length(pre_tables) + 1L]] <- pre
    pos_tables[[length(pos_tables) + 1L]] <- pos
  }
  out_dir <- file.path(output_root, "input_data", "admin_data", "CHL", "_clean")
  pre_path <- file.path(out_dir, "total-pre-CHL.xlsx")
  pos_path <- file.path(out_dir, "total-pos-CHL.xlsx")
  admin_pit_candidate_write_sheeted_workbook(pre_path, pre_tables, chl_tab_years)
  admin_pit_candidate_write_sheeted_workbook(pos_path, pos_tables, chl_tab_years)
  data.frame(country = "CHL", output = c(pre_path, pos_path), first_year = min(chl_tab_years), last_year = max(chl_tab_years), stringsAsFactors = FALSE)
}

admin_pit_candidate_clean_bra_2007plus <- function(t, fld) {
  admin_pit_candidate_load(c("dplyr", "tidyr", "readxl", "janitor", "stringr"))
  if (t <= 2013) {
    sheetname <- "P14_P15_T9"
    rangecoor <- "C11:U22"
  }
  if (t >= 2014) {
    sheetname <- "T9_AC2014"
    rangecoor <- "C11:U28"
  }
  if (t >= 2015) sheetname <- "T9"
  if (t >= 2016) {
    sheetname <- "faixa SM RTT+RTE+RTI"
    rangecoor <- "C9:V26"
  }
  if (t >= 2017) rangecoor <- "B8:U26"
  if (t >= 2019) rangecoor <- "C11:V28"
  if (t == 2020) sheetname <- "Tab9_Fx Rend Total"
  if (t >= 2021) {
    sheetname <- "Tab8"
    rangecoor <- "B7:BE959"
  }
  if (t >= 2022) {
    rangecoor <- "A2:BD954"
  }

  excel_file <- file.path(fld, paste0("gn-irpf-ac", t, ".xlsx"))
  content <- readxl::read_excel(excel_file, sheet = sheetname, range = rangecoor, col_types = c("text"))
  content <- janitor::clean_names(content)

  if (t <= 2020) {
    content <- content |>
      dplyr::select(x1, x2, x3, x4, x5, livro_caixa) |>
      dplyr::rename(faixa_in_min_wage = x1, n = x2) |>
      dplyr::filter(faixa_in_min_wage != "Total") |>
      dplyr::mutate(
        faixa_in_min_wage = stringr::str_replace_all(faixa_in_min_wage, "1/2", "0.5"),
        faixa_in_min_wage = stringr::str_replace_all(faixa_in_min_wage, "Até 0.5", "0")
      ) |>
      tidyr::separate(faixa_in_min_wage, into = c("thr_minwag", "resto"), sep = " a ") |>
      dplyr::mutate(thr_minwag = gsub("[^0-9.-]", "", thr_minwag), year = t, country = "BRA") |>
      dplyr::mutate(
        thr_minwag = as.numeric(thr_minwag), n = as.numeric(n), x3 = as.numeric(x3), x4 = as.numeric(x4),
        x5 = as.numeric(x5), livro_caixa = as.numeric(livro_caixa)
      ) |>
      dplyr::mutate(inc = (x3 + x4 + x5 - livro_caixa) * 10^6) |>
      dplyr::select(country, year, thr_minwag, n, inc) |>
      dplyr::filter(!is.na(thr_minwag) & !is.na(inc))
  }
  if (t >= 2021) {
    if (t == 2021) {
      content <- content |>
        dplyr::select(x1, x2, x3, x4, x5, x13, x14, livro_caixa) |>
        dplyr::rename(tipo = x1, uf = x2, faixa_in_min_wage = x3, n = x4)
    }
    if (t >= 2022) {
      content <- content |>
        dplyr::select(
          tipo_formulario, uf, faixa_de_rendim_tributavel_mais_trib_exclusiva_mais_isentos_em_sal_minimos,
          qtde_contribuintes, rendimento_tributavel_total, rend_sujeitos_a_tribut_exclusiva,
          rend_isentos_e_nao_tributaveis, deducao_livro_caixa
        ) |>
        dplyr::rename(
          tipo = tipo_formulario, faixa_in_min_wage = faixa_de_rendim_tributavel_mais_trib_exclusiva_mais_isentos_em_sal_minimos,
          n = qtde_contribuintes, x5 = rendimento_tributavel_total,
          x13 = rend_sujeitos_a_tribut_exclusiva, x14 = rend_isentos_e_nao_tributaveis,
          livro_caixa = deducao_livro_caixa
        )
    }
    content <- content |>
      dplyr::filter(faixa_in_min_wage != "Total") |>
      dplyr::mutate(
        faixa_in_min_wage = stringr::str_replace_all(faixa_in_min_wage, "1/2", "0.5"),
        faixa_in_min_wage = stringr::str_replace_all(faixa_in_min_wage, "Até 0.5", "0")
      ) |>
      dplyr::mutate(dplyr::across(c(n, x5, x13, x14, livro_caixa), as.numeric)) |>
      dplyr::select(-c("tipo", "uf")) |>
      dplyr::group_by(faixa_in_min_wage) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
      tidyr::separate(faixa_in_min_wage, into = c("thr_minwag", "resto"), sep = " a ") |>
      dplyr::mutate(thr_minwag = gsub("[^0-9.-]", "", thr_minwag), year = t, country = "BRA") |>
      dplyr::mutate(thr_minwag = as.numeric(thr_minwag)) |>
      dplyr::ungroup() |>
      dplyr::mutate(inc = (x5 + x13 + x14 - livro_caixa))
    if (t == 2021) {
      content <- dplyr::mutate(content, inc = inc * 10^6)
    }
    content <- dplyr::select(content, country, year, thr_minwag, n, inc)
  }
  content
}

admin_pit_candidate_clean_bra <- function(
  repo_root = getwd(),
  input_root = repo_root,
  output_root = input_root,
  config_path = Sys.getenv("DINA_CONFIG_YML", unset = file.path(repo_root, "config", "dina.yml")),
  override_path = Sys.getenv("DINA_CONFIG_OVERRIDE_YML", unset = "")
) {
  admin_pit_candidate_load(c("dplyr", "tidyr", "readr", "readxl", "haven", "janitor", "stringr", "glue", "purrr", "openxlsx"))
  config <- admin_pit_candidate_read_config(repo_root, config_path, override_path)
  last_y <- as.integer(config$years$last)
  popdata <- haven::read_dta(file.path(repo_root, "intermediary_data", "population", "SurveyPop.dta"))
  bra_file <- file.path(input_root, "input_data", "admin_data", "BRA")
  bra_tabs_2000_06 <- NULL
  for (year in 2000:2007) {
    excel_file <- file.path(bra_file, glue::glue("ptot_{year}.xlsx"))
    if (file.exists(excel_file)) {
      content <- readxl::read_excel(excel_file)
      bra_tabs_2000_06 <- dplyr::bind_rows(bra_tabs_2000_06, content)
    }
  }
  if (!is.null(bra_tabs_2000_06) && nrow(bra_tabs_2000_06)) {
    bra_tabs_2000_06 <- dplyr::rename(bra_tabs_2000_06, popsize = population) |>
      dplyr::mutate(component = "pretax")
  }
  for (year in 2007:last_y) {
    expected <- file.path(bra_file, glue::glue("gn-irpf-ac{year}.xlsx"))
    if (!file.exists(expected)) {
      stop("Missing Brazil PIT workbook for cleaner year ", year, ": ", expected, call. = FALSE)
    }
  }
  bra_tabs <- purrr::map_dfr(2007:last_y, ~ admin_pit_candidate_clean_bra_2007plus(t = .x, fld = bra_file))
  wiki_minwage <- file.path(bra_file, "downloads", "wiki_minwage.csv")
  if (!file.exists(wiki_minwage)) {
    stop("Missing Brazil minimum wage auxiliary input: ", wiki_minwage, call. = FALSE)
  }
  bra_minwag <- readr::read_csv(wiki_minwage, show_col_types = FALSE)
  bra_tabs <- dplyr::full_join(bra_tabs, bra_minwag, by = "year") |>
    dplyr::mutate(thr = thr_minwag * minwage * 12, component = "pretax") |>
    dplyr::filter(!is.na(country)) |>
    dplyr::left_join(popdata, by = c("country", "year")) |>
    dplyr::arrange(year, dplyr::desc(thr)) |>
    dplyr::group_by(year) |>
    dplyr::mutate(freq = n / totpop_ie, p = 1 - cumsum(freq), bracketavg = inc / n) |>
    dplyr::rename(popsize = totpop_ie) |>
    dplyr::arrange(year, p) |>
    dplyr::bind_rows(bra_tabs_2000_06)
  bra_avgs <- bra_tabs |>
    dplyr::group_by(year) |>
    dplyr::summarise(renta = sum(inc, na.rm = TRUE), popsize = mean(popsize, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(average = renta / popsize)
  bra_tab_years <- bra_avgs$year
  tables <- list()
  for (i in seq_along(bra_tab_years)) {
    year <- bra_tab_years[[i]]
    exptab <- dplyr::filter(dplyr::ungroup(bra_tabs), year == !!year) |>
      dplyr::select(year, country, component, popsize, p, thr, bracketavg) |>
      dplyr::mutate(average = bra_avgs$average[[i]]) |>
      dplyr::select(year, country, component, popsize, average, p, thr, bracketavg)
    if (nrow(exptab) > 1L) exptab[2:nrow(exptab), c("year", "country", "component", "popsize", "average")] <- NA
    tables[[length(tables) + 1L]] <- exptab
  }
  out_dir <- file.path(output_root, "input_data", "admin_data", "BRA", "_clean")
  out_path <- file.path(out_dir, "total-pre-BRA.xlsx")
  admin_pit_candidate_write_sheeted_workbook(out_path, tables, bra_tab_years)
  data.frame(country = "BRA", output = out_path, first_year = min(bra_tab_years), last_year = max(bra_tab_years), stringsAsFactors = FALSE)
}
