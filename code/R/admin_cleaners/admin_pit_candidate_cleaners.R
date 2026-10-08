# Shared PIT Admin cleaners used by Explore and Include.
#
# They produce the reviewed interpolation-ready handoff. Pipeline task 02d
# validates the included handoff; it deliberately does not clean sources again.

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
  readxl::read_excel(path, sheet = sheet, range = range, col_names = col_names, col_types = col_types, .name_repair = "minimal")
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

admin_pit_candidate_read_chl_uta <- function(input_root, years, repo_root = getwd()) {
  # Candidate input_root is deliberately a disposable staged data tree.  Code
  # remains at repo_root, so never depend on getwd() to find this helper.
  helper_paths <- unique(c(
    file.path(repo_root, "code", "R", "source-helpers", "chl_uta.R"),
    file.path(input_root, "code", "R", "source-helpers", "chl_uta.R")
  ))
  helper_paths <- helper_paths[file.exists(helper_paths)]
  if (!length(helper_paths)) {
    stop("Missing Chile UTA helper code/R/source-helpers/chl_uta.R.", call. = FALSE)
  }
  source(helper_paths[[1L]], local = TRUE)
  uta <- chl_uta_load(input_root = input_root, years = years, allow_fetch = FALSE)
  uta[, c("uta", "year")]
}

# Chile's workbook has repeated triplets for different taxpayer populations.
# Do not infer the desired triplet from janitor's generated suffixes: those
# depend on how many repeated headers a particular release contains.  The
# publisher labels the required one "Consolidado", so resolve it from that
# declared label and fail clearly if a future layout no longer supplies it.
admin_pit_candidate_chl_consolidated_columns <- function(layout, raw_names) {
  label_columns <- which(vapply(layout, function(column) {
    any(grepl("\\bconsolidado\\b", as.character(column), ignore.case = TRUE))
  }, logical(1)))
  if (length(label_columns) != 1L) {
    stop("Chile PIT layout must contain exactly one 'Consolidado' column group.", call. = FALSE)
  }
  start <- label_columns[[1L]]
  positions <- start + 0:2
  if (max(positions) > length(raw_names)) {
    stop("Chile PIT 'Consolidado' group does not contain people, income and tax columns.", call. = FALSE)
  }
  names <- janitor::make_clean_names(raw_names[positions])
  expected <- c("^n.*personas", "^renta.*determinada", "^impuesto.*determinado")
  if (!all(vapply(seq_along(expected), function(i) grepl(expected[[i]], names[[i]]), logical(1)))) {
    stop("Chile PIT 'Consolidado' columns do not match the published people, income and tax layout.", call. = FALSE)
  }
  positions
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

  popdata <- haven::read_dta(file.path(input_root, "intermediary_data", "population", "SurveyPop.dta"))
  layout <- admin_pit_candidate_read_workbook(tfile, sheet = "Datos", range = "A1:ZZ8", col_names = FALSE)
  raw_tabs <- admin_pit_candidate_read_workbook(tfile, sheet = "Datos", range = "A8:ZZ5000", col_names = TRUE)
  consolidated <- admin_pit_candidate_chl_consolidated_columns(layout, names(raw_tabs))
  raw_tabs <- janitor::clean_names(raw_tabs)
  raw_tabs <- raw_tabs |>
    dplyr::select(1L, 2L, dplyr::all_of(names(raw_tabs)[consolidated]))
  names(raw_tabs) <- c("year", "tramo", "personas", "renta", "impuesto")
  raw_tabs <- raw_tabs |>
    tidyr::separate(tramo, into = c("a", "tramo"), sep = "-", fill = "right", extra = "merge") |>
    tidyr::separate(tramo, into = c("tramo_uta", "b"), sep = "a", fill = "right", extra = "merge") |>
    dplyr::mutate(
      tramo_uta = stringr::str_replace_all(tramo_uta, " Más de ", ""),
      tramo_uta = stringr::str_replace_all(tramo_uta, ",", "."),
      tramo_uta = stringr::str_replace_all(tramo_uta, "UTA [:punct:]T", "")
    ) |>
    dplyr::select(year, tramo_uta, personas, renta, impuesto) |>
    dplyr::filter(year %in% target_years)

  uta <- admin_pit_candidate_read_chl_uta(input_root, target_years[target_years >= 2005], repo_root = repo_root)
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

  chl_pop1999 <- haven::read_dta(file.path(input_root, "input_data", "wid", "population_total_adult_npopul.dta"))
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
      dplyr::select(year, p, factor)
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
  popdata <- haven::read_dta(file.path(input_root, "intermediary_data", "population", "SurveyPop.dta"))
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

# Colombia releases the F-210 workbooks inside publisher packaging folders.
# Resolve the documented filename for each expected year recursively: the
# folder name is evidence only and is never used as a proxy for coverage.
admin_pit_candidate_col_contract <- function(repo_root) {
  admin_pit_candidate_need("yaml")
  explorer <- yaml::read_yaml(file.path(repo_root, "config", "admin_pit_explorer.yml"))
  contract <- explorer$source_discovery$structure_checks$`col-pit` %||% list()
  required <- c("first_year", "expected_file_template", "index_offset")
  if (!all(required %in% names(contract))) stop("Colombia PIT source contract is incomplete.", call. = FALSE)
  contract
}

admin_pit_candidate_resolve_col_pit <- function(input_root, years, expected_file_template, index_offset) {
  base <- file.path(input_root, "input_data", "admin_data", "COL")
  if (!dir.exists(base)) stop("Colombia PIT source package is missing.", call. = FALSE)
  years <- sort(unique(as.integer(years)))
  expected <- vapply(years, function(year) {
    out <- gsub("\\{index\\}", as.character(year - as.integer(index_offset)), expected_file_template)
    gsub("\\{year\\}", as.character(year), out)
  }, character(1))
  files <- list.files(base, recursive = TRUE, full.names = TRUE, pattern = "_F-210\\.xlsx$", ignore.case = FALSE)
  # The same F-210 filenames also occur in other publisher packages.  The
  # declared incoming source is the *income natural persons* package; do not
  # accidentally resolve wealth or .dta companion releases.
  files <- files[grepl("(^|/)1_Cuantiles_Ingreso_Bruto_Naturales_[0-9]{4}-[0-9]{4}(/|$)", normalizePath(files, winslash = "/", mustWork = FALSE), perl = TRUE)]
  package_dirs <- unique(dirname(files[basename(files) == expected[[1L]]]))
  package_dirs <- package_dirs[vapply(package_dirs, function(dir) all(file.exists(file.path(dir, expected))), logical(1))]
  if (!length(package_dirs)) {
    missing <- expected[!vapply(expected, function(name) any(basename(files) == name), logical(1))]
    stop("Colombia PIT source package is missing the documented F-210 file(s): ", paste(missing %||% expected, collapse = ", "), call. = FALSE)
  }
  # A newer release may coexist with an older retained one.  Select the one
  # whose declared package endpoint is newest; equally current candidates are
  # genuinely ambiguous and must be resolved in source review.
  endpoint <- suppressWarnings(as.integer(sub(".*_([0-9]{4})-([0-9]{4})$", "\\2", basename(package_dirs))))
  best <- package_dirs[endpoint == max(endpoint, na.rm = TRUE)]
  if (length(best) != 1L) stop("Colombia PIT source package has multiple equally current folders with the documented F-210 files.", call. = FALSE)
  data.frame(year = years, path = normalizePath(file.path(best[[1L]], expected), winslash = "/", mustWork = FALSE), basename = expected, stringsAsFactors = FALSE)
}

admin_pit_candidate_col_header <- function(path) {
  sheets <- readxl::excel_sheets(path)
  preferred <- sheets[tolower(trimws(sheets)) == "ag cuantiles ingreso bruto"]
  for (sheet in c(preferred, setdiff(sheets, preferred))) {
    preview <- tryCatch(admin_pit_candidate_read_workbook(path, sheet, range = "A1:ZZ30", col_names = FALSE), error = function(e) NULL)
    if (is.null(preview) || !nrow(preview)) next
    for (row in seq_len(nrow(preview))) {
      labels <- janitor::make_clean_names(as.character(unlist(preview[row, ], use.names = FALSE)))
      has_cases <- any(labels == "numero_de_casos")
      has_quantile <- any(labels == "cuantil")
      has_tax <- any(labels == "impuesto_neto_de_renta")
      has_income <- any(labels %in% c("total_ingresos_recibidos_por_con", "total_ingresos_brutos_1"))
      if (has_cases && has_quantile && has_tax && has_income) return(list(sheet = sheet, header_row = row))
    }
  }
  stop("Colombia PIT workbook has no recognized F-210 quantile table header.", call. = FALSE)
}

admin_pit_candidate_col_read <- function(path) {
  header <- admin_pit_candidate_col_header(path)
  data <- admin_pit_candidate_read_workbook(path, header$sheet, range = paste0("A", header$header_row, ":ZZ5000"), col_names = TRUE)
  names(data) <- janitor::make_clean_names(names(data))
  choose <- function(candidates, label) {
    hit <- which(names(data) %in% candidates)
    if (length(hit) != 1L) stop("Colombia PIT table must contain exactly one ", label, " column.", call. = FALSE)
    hit[[1L]]
  }
  cols <- c(
    n = choose("numero_de_casos", "number-of-cases"),
    quantile = choose("cuantil", "quantile"),
    income = choose(c("total_ingresos_recibidos_por_con", "total_ingresos_brutos_1"), "total-income"),
    tax = choose("impuesto_neto_de_renta", "net-tax")
  )
  out <- data[, cols, drop = FALSE]
  names(out) <- names(cols)
  out$n <- suppressWarnings(as.numeric(out$n))
  out$income <- suppressWarnings(as.numeric(out$income))
  out$tax <- suppressWarnings(as.numeric(out$tax))
  out[is.finite(out$n) & out$n > 0 & is.finite(out$income), c("n", "income", "tax"), drop = FALSE]
}

admin_pit_candidate_col_breaks <- function() {
  unique(c(0, seq(0.01, 0.99, 0.01), seq(0.991, 0.999, 0.001), seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001), 1))
}

admin_pit_candidate_clean_col <- function(
  repo_root = getwd(), input_root = repo_root, output_root = input_root,
  config_path = Sys.getenv("DINA_CONFIG_YML", unset = file.path(repo_root, "config", "dina.yml")),
  override_path = Sys.getenv("DINA_CONFIG_OVERRIDE_YML", unset = "")
) {
  admin_pit_candidate_load(c("readxl", "haven", "janitor", "openxlsx"))
  config <- admin_pit_candidate_read_config(repo_root, config_path, override_path)
  last_year <- as.integer(config$years$last)
  contract <- admin_pit_candidate_col_contract(repo_root)
  first_year <- as.integer(contract$first_year)
  resolved <- admin_pit_candidate_resolve_col_pit(
    input_root, seq.int(first_year, last_year),
    expected_file_template = as.character(contract$expected_file_template),
    index_offset = as.integer(contract$index_offset)
  )
  pop <- haven::read_dta(file.path(input_root, "intermediary_data", "population", "SurveyPop.dta"))
  tables <- lapply(seq_len(nrow(resolved)), function(i) {
    year <- resolved$year[[i]]
    total_pop <- suppressWarnings(as.numeric(pop$totpop_ie[pop$country == "COL" & pop$year == year][[1L]]))
    if (!is.finite(total_pop) || total_pop <= 0) stop("Colombia PIT cleaner needs SurveyPop total population for ", year, ".", call. = FALSE)
    raw <- admin_pit_candidate_col_read(resolved$path[[i]])
    pre <- raw$income * 1e6 / raw$n
    pos <- pre - raw$tax * 1e6 / raw$n
    keep <- is.finite(pos) & pos >= 0
    raw <- raw[keep, , drop = FALSE]
    pos <- pos[keep]
    order_desc <- order(pos, decreasing = TRUE)
    weight <- raw$n[order_desc]
    value <- pos[order_desc]
    cumulative <- cumsum(weight) / total_pop
    p <- 1 - cumulative
    topavg <- cumsum(value * weight) / cumsum(weight)
    group <- cut(p, breaks = admin_pit_candidate_col_breaks(), include.lowest = TRUE, right = FALSE, labels = FALSE)
    split_index <- split(seq_along(group), group)
    rows <- lapply(split_index, function(index) {
      ix <- as.integer(index)
      data.frame(
        year = year, country = "COL", component = "posttax", popsize = total_pop,
        average = sum(value * weight, na.rm = TRUE) / total_pop,
        p = min(p[ix], na.rm = TRUE), thr = min(value[ix], na.rm = TRUE),
        bracketavg = stats::weighted.mean(value[ix], weight[ix]), topavg = min(topavg[ix], na.rm = TRUE),
        eff_tax_rate = NA_real_, stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    out <- out[out$thr >= 1e6 & is.finite(out$bracketavg) & out$bracketavg != out$thr, , drop = FALSE]
    out <- out[order(out$thr), , drop = FALSE]
    if (nrow(out) > 1L) out[2:nrow(out), c("year", "country", "component", "popsize", "average")] <- NA
    out
  })
  path <- file.path(output_root, "input_data", "admin_data", "COL", "_clean", "total-pos-COL.xlsx")
  admin_pit_candidate_write_sheeted_workbook(path, tables, resolved$year)
  data.frame(country = "COL", output = path, first_year = min(resolved$year), last_year = max(resolved$year), stringsAsFactors = FALSE)
}
