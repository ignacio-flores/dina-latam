# Country-SNA explorer.
#
# This file powers the experimental `dina sources explore country-sna` command.
# It stays outside the production pipeline: the explorer inventories files,
# years, sheets, and broad layout evidence, then emits expectations for the
# deterministic include workflow. It does not validate economic meaning and it
# does not trust adaptive value guesses as user-facing findings.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

country_sna_explorer_repo_root <- function(start = getwd()) {
  env_root <- Sys.getenv("DINA_REPO_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, mustWork = FALSE))
  }
  current <- normalizePath(start, mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, "_config.do")) || dir.exists(file.path(current, ".git"))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find DINA repo root from ", start, call. = FALSE)
    }
    current <- parent
  }
}

country_sna_explorer_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

country_sna_explorer_has <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

country_sna_explorer_path <- function(path, root) {
  if (is.na(path) || !nzchar(path)) return(NA_character_)
  if (grepl("^/", path)) path else file.path(root, path)
}

country_sna_explorer_read_yaml <- function(path) {
  country_sna_explorer_need("yaml")
  yaml::read_yaml(path)
}

country_sna_explorer_read_contract <- function(
  root = country_sna_explorer_repo_root(),
  contract_path = file.path(root, "config", "country_sna_explorer.yml")
) {
  contract <- country_sna_explorer_read_yaml(contract_path)
  if (is.null(contract$economic_contract) || is.null(contract$source_discovery)) {
    stop("Invalid country-SNA explorer contract: missing economic_contract or source_discovery.", call. = FALSE)
  }
  contract
}

country_sna_explorer_years <- function(contract, root) {
  years <- contract$years %||% list()
  if (!is.null(years$first) && !is.null(years$last)) {
    return(seq.int(as.integer(years$first), as.integer(years$last)))
  }
  config_path <- country_sna_explorer_path(years$from_config %||% "config/dina.yml", root)
  config <- country_sna_explorer_read_yaml(config_path)
  seq.int(as.integer(config$years$first), as.integer(config$years$last))
}

country_sna_explorer_country_rules <- function(contract) {
  contract$source_discovery$country_rules %||% contract$source_discovery$countries %||% list()
}

country_sna_explorer_project_countries <- function(contract, root) {
  rules <- country_sna_explorer_country_rules(contract)
  config_path <- country_sna_explorer_path((contract$years %||% list())$from_config %||% "config/dina.yml", root)
  if (file.exists(config_path)) {
    config <- country_sna_explorer_read_yaml(config_path)
    configured <- toupper(as.character(config$countries %||% character()))
    return(configured[configured %in% names(rules)])
  }
  # Test fixtures may not carry a full project config. In that case, retain
  # the legacy fixture field if present; production contracts use config/dina.yml.
  fallback <- toupper(as.character(contract$economic_contract$countries %||% names(rules)))
  fallback[fallback %in% names(rules)]
}

country_sna_explorer_output_root <- function(root, contract, output_dir = NULL) {
  country_sna_explorer_path(output_dir %||% contract$output_root, root)
}

country_sna_explorer_output_paths <- function(root, contract, output_dir = NULL) {
  out <- country_sna_explorer_output_root(root, contract, output_dir)
  list(
    root = out,
    tables = file.path(out, "tables"),
    figures = file.path(out, "figures"),
    workbooks = file.path(out, "workbooks"),
    logs = file.path(out, "logs")
  )
}

country_sna_explorer_run_id <- function(prefix = "explore") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

country_sna_explorer_relative_path <- function(path, root) {
  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

country_sna_explorer_file_fingerprint <- function(path, root = country_sna_explorer_repo_root()) {
  exists <- file.exists(path)
  info <- if (exists) file.info(path) else data.frame(size = NA_real_, mtime = as.POSIXct(NA))
  sha <- NA_character_
  status <- if (exists) "ok" else "missing"
  if (exists && country_sna_explorer_has("digest")) {
    sha <- digest::digest(file = path, algo = "sha256")
  } else if (exists) {
    status <- "digest_missing"
  }
  data.frame(
    path = path,
    rel = country_sna_explorer_relative_path(path, root),
    exists = exists,
    size = suppressWarnings(as.numeric(info$size[[1L]])),
    mtime = as.character(info$mtime[[1L]]),
    sha256 = sha,
    status = status,
    stringsAsFactors = FALSE
  )
}

country_sna_explorer_bind <- function(...) {
  parts <- list(...)
  if (length(parts) == 1L && is.list(parts[[1L]]) && !is.data.frame(parts[[1L]])) {
    parts <- parts[[1L]]
  }
  parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, parts)
  if (!length(parts)) return(data.frame(stringsAsFactors = FALSE))
  cols <- unique(unlist(lapply(parts, names), use.names = FALSE))
  parts <- lapply(parts, function(x) {
    missing <- setdiff(cols, names(x))
    for (col in missing) x[[col]] <- NA
    x[cols]
  })
  do.call(rbind, parts)
}

country_sna_explorer_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9.]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

country_sna_explorer_normalize_code <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- toupper(trimws(x))
  x <- gsub("\\s+", "", x)
  x <- gsub("Y", "/", x)
  x <- gsub("^D([0-9])", "D.\\1", x)
  x <- gsub("^B([0-9])", "B.\\1", x)
  x <- gsub("B\\.2B/B\\.3B", "B.2B/B.3B", x, fixed = TRUE)
  x
}

country_sna_explorer_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub(",", "", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", "", x)
  suppressWarnings(as.numeric(x))
}

country_sna_explorer_parse_years <- function(x) {
  matches <- gregexpr("(19|20)[0-9]{2}", basename(as.character(x)), perl = TRUE)
  years <- unique(as.integer(unlist(regmatches(basename(as.character(x)), matches))))
  years[!is.na(years)]
}

country_sna_explorer_parse_year_list <- function(x) {
  x <- paste(as.character(x %||% ""), collapse = ",")
  if (!nzchar(trimws(x))) return(integer())
  years <- suppressWarnings(as.integer(unlist(strsplit(x, "[^0-9]+"))))
  years[!is.na(years) & years >= 1900L & years <= 2099L]
}

country_sna_explorer_expand_year_span <- function(years, max_span = 35L) {
  years <- sort(unique(as.integer(years[!is.na(years)])))
  if (length(years) < 2L) return(years)
  span <- max(years) - min(years)
  if (span > 0L && span <= as.integer(max_span)) {
    return(seq.int(min(years), max(years)))
  }
  years
}

country_sna_explorer_glob <- function(pattern, root) {
  sort(Sys.glob(country_sna_explorer_path(pattern, root)))
}

country_sna_explorer_col_letter <- function(index) {
  vapply(as.integer(index), function(i) {
    out <- character()
    while (i > 0L) {
      r <- (i - 1L) %% 26L
      out <- c(LETTERS[[r + 1L]], out)
      i <- (i - 1L) %/% 26L
    }
    paste(out, collapse = "")
  }, character(1))
}

country_sna_explorer_account_aliases <- function(contract, account = NULL) {
  aliases <- contract$economic_contract$code_aliases
  if (!is.null(account)) {
    return(country_sna_explorer_normalize_code(unlist(aliases[[account]] %||% account, use.names = FALSE)))
  }
  unique(country_sna_explorer_normalize_code(unlist(aliases, use.names = FALSE)))
}

country_sna_explorer_primitive_specs <- function(contract) {
  contract$economic_contract$variables$primitives %||% list()
}

country_sna_explorer_pattern_for_year <- function(pattern, year) {
  pattern <- gsub("\\{year\\}", as.character(year), pattern)
  gsub("\\{index\\}", as.character(year), pattern)
}

country_sna_explorer_choose_file <- function(paths) {
  if (!length(paths)) return(NA_character_)
  years <- lapply(paths, country_sna_explorer_parse_years)
  max_year <- vapply(years, function(x) if (length(x)) max(x) else -Inf, numeric(1))
  paths[order(max_year, paths, decreasing = TRUE)][[1L]]
}

country_sna_explorer_source_row <- function(
  country, source_set, adapter_family, selector, year, file, files = NA_character_,
  status = "matched", matched_by = selector, expected_index = NA_integer_,
  index_consistency = NA_character_, notes = NA_character_
) {
  data.frame(
    country = country,
    source_set = source_set,
    adapter_family = adapter_family,
    selector = selector,
    year = as.integer(year),
    file = file %||% NA_character_,
    files = files %||% NA_character_,
    status = status,
    matched_by = matched_by,
    expected_index = as.integer(expected_index),
    index_consistency = index_consistency %||% NA_character_,
    notes = notes %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

country_sna_explorer_parse_mex_index <- function(index_file) {
  if (!file.exists(index_file)) return(data.frame(stringsAsFactors = FALSE))
  lines <- readLines(index_file, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  rows <- lapply(lines, function(line) {
    pieces <- strsplit(line, "\\|", fixed = FALSE)[[1]]
    if (length(pieces) >= 4L) {
      file <- pieces[[1L]]
      content <- paste(pieces[-1L], collapse = " ")
    } else {
      file <- regmatches(line, regexpr("CSI_[0-9]+\\.xlsx", line, ignore.case = TRUE))
      if (!length(file) || is.na(file)) file <- NA_character_
      content <- line
    }
    years <- country_sna_explorer_parse_years(content)
    data.frame(
      file = file,
      content = content,
      content_norm = country_sna_explorer_text(content),
      years = paste(years, collapse = ","),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_resolve_indexed_year_file <- function(spec, year, root) {
  folder <- country_sna_explorer_path(spec$folder, root)
  index_file <- country_sna_explorer_path(spec$index_file, root)
  index <- country_sna_explorer_parse_mex_index(index_file)
  expected_index <- as.integer(year) - as.integer(spec$fallback_index_offset %||% 2000L)
  title <- country_sna_explorer_text(spec$title_contains %||% "")
  unit <- country_sna_explorer_text(spec$unit_contains %||% "")
  # Mexico changed the apparent CSI file numbering in ways that make
  # `year - 2000` too brittle as the primary rule. The explorer therefore
  # trusts `indice_archivos.txt` first and records whether the old arithmetic
  # convention still agrees, leaving a visible audit trail when it does not.
  if (nrow(index)) {
    hit <- grepl(as.character(year), index$years, fixed = TRUE) |
      grepl(as.character(year), index$content, fixed = TRUE)
    if (nzchar(title)) hit <- hit & grepl(title, index$content_norm, fixed = TRUE)
    if (nzchar(unit)) hit <- hit & grepl(unit, index$content_norm, fixed = TRUE)
    candidates <- index[hit & !is.na(index$file), , drop = FALSE]
    if (nrow(candidates)) {
      selected <- candidates$file[[1L]]
      file <- file.path(folder, basename(selected))
      actual_index <- as.integer(sub("^CSI_([0-9]+)\\.xlsx$", "\\1", basename(file), ignore.case = TRUE))
      return(list(
        file = file,
        matched_by = "index_metadata",
        expected_index = expected_index,
        index_consistency = if (identical(actual_index, expected_index)) "matches_year_minus_2000" else "differs_from_year_minus_2000",
        status = if (file.exists(file)) "matched" else "missing_indexed_file"
      ))
    }
  }
  fallback <- country_sna_explorer_pattern_for_year(spec$fallback_pattern, expected_index)
  files <- country_sna_explorer_glob(fallback, root)
  file <- country_sna_explorer_choose_file(files)
  list(
    file = file,
    matched_by = "fallback_year_minus_2000",
    expected_index = expected_index,
    index_consistency = "fallback_rule_used",
    status = if (!is.na(file) && file.exists(file)) "matched" else "no_file"
  )
}

country_sna_explorer_resolve_sources <- function(country, source_set, rule, years, root) {
  spec <- rule[[source_set]]
  adapter <- rule$adapter_family %||% "unknown"
  if (is.null(spec)) {
    return(country_sna_explorer_source_row(country, source_set, adapter, "none", NA_integer_, NA_character_, status = "no_selector"))
  }
  type <- spec$type %||% "unknown"
  # Source matching is intentionally pattern/year based rather than tied to one
  # official release filename. The selector types below are the documented
  # matching contract: stems catch .xls/.xlsx variants, patterns catch renamed
  # releases, year patterns catch annual files, and bundles describe countries
  # whose CEI information arrives as several sector workbooks.
  if (identical(type, "single_stem")) {
    stem <- country_sna_explorer_path(spec$stem, root)
    paths <- sort(Sys.glob(paste0(stem, ".xls*")))
    if (!length(paths) && file.exists(stem)) paths <- stem
    if (!length(paths)) {
      return(country_sna_explorer_source_row(country, source_set, adapter, type, NA_integer_, NA_character_, status = "no_file"))
    }
    status <- if (length(paths) > 1L) "ambiguous_stem" else "matched"
    return(country_sna_explorer_bind(lapply(paths, function(path) {
      country_sna_explorer_source_row(country, source_set, adapter, type, NA_integer_, path, status = status)
    })))
  }
  if (identical(type, "single_pattern")) {
    paths <- country_sna_explorer_glob(spec$pattern, root)
    if (!length(paths)) {
      return(country_sna_explorer_source_row(country, source_set, adapter, type, NA_integer_, NA_character_, status = "no_file"))
    }
    return(country_sna_explorer_bind(lapply(paths, function(path) {
      parsed <- country_sna_explorer_parse_years(path)
      country_sna_explorer_source_row(
        country, source_set, adapter, type,
        if (length(parsed) == 1L) parsed[[1L]] else NA_integer_,
        path,
        status = "matched"
      )
    })))
  }
  if (identical(type, "year_pattern")) {
    return(country_sna_explorer_bind(lapply(years, function(year) {
      paths <- country_sna_explorer_glob(country_sna_explorer_pattern_for_year(spec$pattern, year), root)
      file <- country_sna_explorer_choose_file(paths)
      country_sna_explorer_source_row(
        country, source_set, adapter, type, year, file,
        status = if (!is.na(file) && file.exists(file)) "matched" else "no_file",
        notes = if (length(paths) > 1L) "multiple_files_for_year" else NA_character_
      )
    })))
  }
  if (identical(type, "indexed_year_file")) {
    return(country_sna_explorer_bind(lapply(years, function(year) {
      resolved <- country_sna_explorer_resolve_indexed_year_file(spec, year, root)
      country_sna_explorer_source_row(
        country, source_set, adapter, type, year, resolved$file,
        status = resolved$status,
        matched_by = resolved$matched_by,
        expected_index = resolved$expected_index,
        index_consistency = resolved$index_consistency
      )
    })))
  }
  if (identical(type, "sector_file_bundle")) {
    folder <- country_sna_explorer_path(spec$folder, root)
    return(country_sna_explorer_bind(lapply(years, function(year) {
      pattern <- country_sna_explorer_pattern_for_year(spec$pattern, year)
      files <- sort(Sys.glob(file.path(folder, pattern)))
      country_sna_explorer_source_row(
        country, source_set, adapter, type, year, if (length(files)) files[[1L]] else NA_character_,
        files = paste(files, collapse = "|"),
        status = if (length(files)) "matched" else "no_file",
        notes = sprintf("bundle_file_count=%s", length(files))
      )
    })))
  }
  country_sna_explorer_source_row(country, source_set, adapter, type, NA_integer_, NA_character_, status = "unsupported_selector")
}

country_sna_explorer_sheet_score <- function(sheet, target_year = NA_integer_, contract = NULL, adapter_family = "rectangular_workbook") {
  sheet_norm <- country_sna_explorer_text(sheet)
  years <- country_sna_explorer_parse_years(sheet)
  score <- 0
  reason <- character()
  if (!is.na(target_year) && length(years) && target_year %in% years) {
    score <- score + 0.75
    reason <- c(reason, "sheet_year_matches")
  } else if (!is.na(target_year) && grepl(as.character(target_year), sheet, fixed = TRUE)) {
    score <- score + 0.60
    reason <- c(reason, "sheet_contains_year")
  }
  keywords <- contract$layout_detection$sheet_keywords %||% c("CEI", "cuentas corrientes", "tabulado")
  keyword_norm <- country_sna_explorer_text(keywords)
  if (any(nzchar(keyword_norm) & vapply(keyword_norm, grepl, logical(1), x = sheet_norm, fixed = TRUE))) {
    score <- score + 0.20
    reason <- c(reason, "sheet_keyword")
  }
  if (identical(adapter_family, "account_sheet_workbook")) {
    account_keywords <- country_sna_explorer_text(contract$layout_detection$account_sheet_keywords %||% character())
    if (any(nzchar(account_keywords) & vapply(account_keywords, grepl, logical(1), x = sheet_norm, fixed = TRUE))) {
      score <- max(score, 0.80)
      reason <- c(reason, "account_sheet_keyword")
    }
  }
  if (identical(sheet_norm, "tabulado")) {
    score <- max(score, 0.75)
    reason <- c(reason, "fixed_tabulado_sheet")
  }
  if (!length(reason)) reason <- "no_year_or_keyword_match"
  list(score = min(score, 1), reason = paste(unique(reason), collapse = ";"))
}

country_sna_explorer_sheet_inventory <- function(source_inventory, contract) {
  country_sna_explorer_need("readxl")
  rows <- list()
  for (i in seq_len(nrow(source_inventory))) {
    src <- source_inventory[i, , drop = FALSE]
    if (is.na(src$file) || !file.exists(src$file) || !src$adapter_family %in% c("rectangular_workbook", "account_sheet_workbook")) {
      next
    }
    sheets <- tryCatch(readxl::excel_sheets(src$file), error = function(e) character())
    if (!length(sheets)) next
    scored <- lapply(sheets, function(sheet) {
      years <- country_sna_explorer_parse_years(sheet)
      target_year <- if (!is.na(src$year)) src$year else if (length(years) == 1L) years[[1L]] else NA_integer_
      score <- country_sna_explorer_sheet_score(sheet, target_year, contract, src$adapter_family)
      data.frame(
        country = src$country,
        source_set = src$source_set,
        adapter_family = src$adapter_family,
        year = as.integer(target_year),
        file = src$file,
        sheet = sheet,
        parsed_years = paste(years, collapse = ","),
        sheet_score = score$score,
        evidence = score$reason,
        status = if (score$score >= 0.70) "likely_sheet" else if (score$score >= 0.40) "possible_sheet" else "unlikely_sheet",
        stringsAsFactors = FALSE
      )
    })
    rows[[length(rows) + 1L]] <- country_sna_explorer_bind(scored)
  }
  country_sna_explorer_bind(rows)
}

country_sna_explorer_read_sheet_grid <- function(path, sheet, contract) {
  country_sna_explorer_need("readxl")
  max_rows <- as.integer(contract$layout_detection$max_rows %||% 220L)
  max_cols <- as.integer(contract$layout_detection$max_cols %||% 90L)
  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal",
    guess_max = max_rows
  ))
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (nrow(raw) > max_rows) raw <- raw[seq_len(max_rows), , drop = FALSE]
  if (ncol(raw) > max_cols) raw <- raw[, seq_len(max_cols), drop = FALSE]
  raw
}

country_sna_explorer_detect_tables <- function(grid, context, contract) {
  if (!nrow(grid) || !ncol(grid)) return(data.frame(stringsAsFactors = FALSE))
  aliases <- country_sna_explorer_account_aliases(contract)
  target_accounts <- names(contract$economic_contract$code_aliases)
  candidates <- list()
  for (col in seq_len(ncol(grid))) {
    codes <- country_sna_explorer_normalize_code(grid[[col]])
    hit <- which(codes %in% aliases)
    if (!length(hit)) next
    diversity <- length(unique(codes[hit]))
    score <- min(1, 0.75 * (diversity / max(1, min(length(target_accounts), 10))) + 0.25 * min(1, length(hit) / 12))
    status <- if (score >= (contract$layout_detection$table_score_high %||% 0.70)) {
      "accepted_high_confidence"
    } else if (score >= (contract$layout_detection$table_score_low %||% 0.40)) {
      "accepted_low_confidence"
    } else {
      "ambiguous_table"
    }
    candidates[[length(candidates) + 1L]] <- data.frame(
      country = context$country,
      source_set = context$source_set,
      adapter_family = context$adapter_family,
      year = as.integer(context$year),
      file = context$file,
      sheet = context$sheet,
      table_id = sprintf("%s:%s:%s:%s", context$country, context$source_set, context$year %||% "NA", col),
      code_col = col,
      code_col_letter = country_sna_explorer_col_letter(col),
      row_start = min(hit),
      row_end = max(hit),
      account_hit_count = length(hit),
      account_diversity = diversity,
      table_score = score,
      status = status,
      evidence = sprintf("account_code_column=%s;hits=%s;distinct=%s", country_sna_explorer_col_letter(col), length(hit), diversity),
      stringsAsFactors = FALSE
    )
  }
  out <- country_sna_explorer_bind(candidates)
  if (!nrow(out)) return(out)
  out[order(-out$table_score, out$code_col), , drop = FALSE]
}

country_sna_explorer_label_score <- function(text, labels) {
  text <- country_sna_explorer_text(text)
  labels <- country_sna_explorer_text(labels)
  labels <- labels[nzchar(labels)]
  if (!nzchar(text) || !length(labels)) return(0)
  direct <- any(vapply(labels, grepl, logical(1), x = text, fixed = TRUE))
  if (direct) return(1)
  if (!country_sna_explorer_has("stringdist")) return(0)
  tokens <- unique(strsplit(text, " ", fixed = TRUE)[[1]])
  tokens <- tokens[nchar(tokens) >= 4L]
  if (!length(tokens)) return(0)
  label_tokens <- unique(unlist(strsplit(labels, " ", fixed = TRUE), use.names = FALSE))
  label_tokens <- label_tokens[nchar(label_tokens) >= 4L]
  if (!length(label_tokens)) return(0)
  distances <- stringdist::stringdistmatrix(tokens, label_tokens, method = "jw")
  if (any(distances <= 0.12)) 0.65 else 0
}

country_sna_explorer_header_text <- function(grid, row_start, col, contract) {
  before <- as.integer(contract$layout_detection$header_rows_before_table %||% 12L)
  rows <- seq.int(max(1L, row_start - before), max(1L, row_start - 1L))
  same_col <- paste(unlist(grid[rows, col, drop = FALSE], use.names = FALSE), collapse = " ")
  if (nzchar(country_sna_explorer_text(same_col))) {
    return(same_col)
  }
  cols <- seq.int(max(1L, col - 1L), min(ncol(grid), col + 1L))
  paste(unlist(grid[rows, cols, drop = FALSE], use.names = FALSE), collapse = " ")
}

country_sna_explorer_detect_roles <- function(grid, table, contract) {
  roles <- contract$economic_contract$roles
  sector_labels <- contract$layout_detection$sector_labels
  direction_labels <- contract$layout_detection$direction_labels
  table_rows <- seq.int(max(1L, table$row_start), min(nrow(grid), table$row_end))
  rows <- list()
  for (role in names(roles)) {
    spec <- roles[[role]]
    for (col in seq_len(ncol(grid))) {
      if (identical(as.integer(col), as.integer(table$code_col))) next
      header <- country_sna_explorer_header_text(grid, table$row_start, col, contract)
      sector_score <- country_sna_explorer_label_score(header, sector_labels[[spec$sector]] %||% spec$sector)
      direction_score <- country_sna_explorer_label_score(header, direction_labels[[spec$direction]] %||% spec$direction)
      if (direction_score == 0 && sector_score > 0 && identical(spec$direction, "resources")) {
        direction_score <- 0.35
      }
      nums <- country_sna_explorer_num(grid[table_rows, col, drop = TRUE])
      numeric_score <- mean(!is.na(nums))
      score <- 0.45 * sector_score + 0.35 * direction_score + 0.20 * numeric_score
      status <- if (score >= (contract$layout_detection$role_score_high %||% 0.70)) {
        "accepted_high_confidence"
      } else if (score >= (contract$layout_detection$role_score_low %||% 0.40)) {
        "accepted_low_confidence"
      } else {
        "missing_role"
      }
      rows[[length(rows) + 1L]] <- data.frame(
        country = table$country,
        source_set = table$source_set,
        adapter_family = table$adapter_family,
        year = as.integer(table$year),
        file = table$file,
        sheet = table$sheet,
        table_id = table$table_id,
        role = role,
        role_col = col,
        role_col_letter = country_sna_explorer_col_letter(col),
        role_score = score,
        sector_score = sector_score,
        direction_score = direction_score,
        numeric_score = numeric_score,
        status = status,
        evidence = country_sna_explorer_text(header),
        stringsAsFactors = FALSE
      )
    }
  }
  country_sna_explorer_bind(rows)
}

country_sna_explorer_best_role <- function(role_candidates, role) {
  part <- role_candidates[role_candidates$role == role, , drop = FALSE]
  if (!nrow(part)) return(NULL)
  part <- part[order(-part$role_score, part$role_col), , drop = FALSE]
  top <- part[1L, , drop = FALSE]
  if (top$status[[1L]] == "missing_role") return(NULL)
  if (nrow(part) > 1L && part$role_score[[2L]] >= top$role_score[[1L]] - 0.03 && top$status[[1L]] == "accepted_high_confidence") {
    top$status <- "ambiguous_role"
  }
  top
}

country_sna_explorer_value_resolution <- function(status) {
  if (status %in% c("accepted_high_confidence", "accepted_low_confidence")) {
    return(c(status_group = "accepted", resolution_stage = "accepted"))
  }
  stage <- switch(
    status,
    layout_family_unhandled = "layout",
    ambiguous_table = "table",
    missing_account = "account",
    missing_role = "role",
    ambiguous_role = "role",
    non_numeric = "value",
    duplicate_conflict = "value",
    zero_treated_missing = "value",
    "unknown"
  )
  c(status_group = "unresolved", resolution_stage = stage)
}

country_sna_explorer_role_resolution_detail <- function(role_candidates, role) {
  part <- role_candidates[role_candidates$role == role, , drop = FALSE]
  if (!nrow(part)) return("no_role_candidates")
  part <- part[order(-part$role_score, part$role_col), , drop = FALSE]
  top <- part[1L, , drop = FALSE]
  sprintf(
    "best_role_status=%s;best_role_col=%s;best_role_score=%.3f",
    top$status[[1L]],
    top$role_col_letter[[1L]] %||% NA_character_,
    top$role_score[[1L]]
  )
}

country_sna_explorer_value_candidates <- function(grid, table, role_candidates, contract) {
  variables <- country_sna_explorer_primitive_specs(contract)
  codes <- country_sna_explorer_normalize_code(grid[[table$code_col]])
  multiplier <- contract$economic_contract$units[[table$country]] %||% 1
  zero_as_missing <- isTRUE(contract$economic_contract$zero_as_missing)
  rows <- list()
  for (spec in variables) {
    aliases <- country_sna_explorer_account_aliases(contract, spec$account)
    hit <- which(codes %in% aliases)
    role <- country_sna_explorer_best_role(role_candidates, spec$role)
    status <- NA_character_
    reason <- character()
    value_raw <- NA_real_
    value_standardized <- NA_real_
    row_evidence <- NA_integer_
    col_evidence <- NA_integer_
    role_score <- NA_real_
    if (!nrow(table) || table$status == "ambiguous_table") {
      status <- "ambiguous_table"
      reason <- c(reason, sprintf("table_status=%s", table$status %||% "missing_table"))
    } else if (!length(hit)) {
      status <- "missing_account"
      reason <- c(reason, sprintf("account_code_not_found_in_table=%s", table$table_id %||% NA_character_))
    } else if (is.null(role)) {
      status <- "missing_role"
      reason <- c(reason, country_sna_explorer_role_resolution_detail(role_candidates, spec$role))
    } else if (identical(role$status[[1L]], "ambiguous_role")) {
      status <- "ambiguous_role"
      reason <- c(reason, country_sna_explorer_role_resolution_detail(role_candidates, spec$role))
    } else {
      values <- country_sna_explorer_num(grid[hit, role$role_col[[1L]], drop = TRUE])
      row_evidence <- hit[[1L]]
      col_evidence <- role$role_col[[1L]]
      role_score <- role$role_score[[1L]]
      non_missing <- values[!is.na(values)]
      if (!length(non_missing)) {
        status <- "non_numeric"
        reason <- c(reason, "matched_cell_not_numeric")
      } else if (length(unique(non_missing)) > 1L) {
        status <- "duplicate_conflict"
        reason <- c(reason, "duplicate_account_values_conflict")
      } else {
        value_raw <- unique(non_missing)[[1L]]
        if (isTRUE(zero_as_missing) && isTRUE(value_raw == 0)) {
          status <- "zero_treated_missing"
          reason <- c(reason, "zero_source_value")
        } else {
          high <- table$status == "accepted_high_confidence" && role$status[[1L]] == "accepted_high_confidence"
          status <- if (high) "accepted_high_confidence" else "accepted_low_confidence"
          value_standardized <- value_raw * multiplier
          reason <- c(reason, if (length(non_missing) > 1L) "duplicate_identical" else "unique_match")
        }
      }
    }
    resolution <- country_sna_explorer_value_resolution(status)
    rows[[length(rows) + 1L]] <- data.frame(
      country = table$country,
      source_set = table$source_set,
      adapter_family = table$adapter_family,
      year = as.integer(table$year),
      variable = spec$name,
      account_code = spec$account,
      sector_role = spec$role,
      file = table$file,
      sheet = table$sheet,
      table_id = table$table_id,
      value_raw = value_raw,
      value_standardized = value_standardized,
      row_evidence = row_evidence,
      column_evidence = col_evidence,
      table_score = table$table_score,
      role_score = role_score,
      status = status,
      status_group = unname(resolution[["status_group"]]),
      resolution_stage = unname(resolution[["resolution_stage"]]),
      reason = paste(reason, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }
  country_sna_explorer_bind(rows)
}

country_sna_explorer_audit_rectangular <- function(source_inventory, sheet_inventory, contract) {
  table_rows <- list()
  role_rows <- list()
  value_rows <- list()
  selected <- sheet_inventory[sheet_inventory$status %in% c("likely_sheet", "possible_sheet"), , drop = FALSE]
  if (!nrow(selected)) {
    return(list(tables = data.frame(stringsAsFactors = FALSE), roles = data.frame(stringsAsFactors = FALSE), values = data.frame(stringsAsFactors = FALSE)))
  }
  selected <- selected[order(selected$country, selected$source_set, selected$file, selected$year, -selected$sheet_score), , drop = FALSE]
  key <- paste(selected$file, selected$source_set, selected$year)
  selected <- selected[!duplicated(key), , drop = FALSE]
  for (i in seq_len(nrow(selected))) {
    ctx <- selected[i, , drop = FALSE]
    grid <- tryCatch(country_sna_explorer_read_sheet_grid(ctx$file, ctx$sheet, contract), error = function(e) NULL)
    if (is.null(grid)) next
    tables <- country_sna_explorer_detect_tables(grid, ctx, contract)
    if (!nrow(tables)) next
    tables <- tables[seq_len(min(3L, nrow(tables))), , drop = FALSE]
    table_rows[[length(table_rows) + 1L]] <- tables
    best_table <- tables[1L, , drop = FALSE]
    roles <- country_sna_explorer_detect_roles(grid, best_table, contract)
    role_rows[[length(role_rows) + 1L]] <- roles
    values <- country_sna_explorer_value_candidates(grid, best_table, roles, contract)
    value_rows[[length(value_rows) + 1L]] <- values
  }
  list(
    tables = country_sna_explorer_bind(table_rows),
    roles = country_sna_explorer_bind(role_rows),
    values = country_sna_explorer_bind(value_rows)
  )
}

country_sna_explorer_sector_from_file <- function(file, rule) {
  sector_codes <- rule$sector_codes %||% list()
  for (sector in names(sector_codes)) {
    if (grepl(sector_codes[[sector]], basename(file), fixed = TRUE)) return(sector)
  }
  NA_character_
}

country_sna_explorer_audit_sector_bundle <- function(source_inventory, rule, contract) {
  table_rows <- list()
  role_rows <- list()
  value_rows <- list()
  sources <- source_inventory[source_inventory$status == "matched" & source_inventory$adapter_family == "sector_file_bundle", , drop = FALSE]
  for (i in seq_len(nrow(sources))) {
    files <- strsplit(sources$files[[i]], "\\|", fixed = FALSE)[[1]]
    files <- files[nzchar(files)]
    for (file in files) {
      if (!file.exists(file)) next
      sheets <- tryCatch(readxl::excel_sheets(file), error = function(e) character())
      if (!length(sheets)) next
      grid <- tryCatch(country_sna_explorer_read_sheet_grid(file, sheets[[1L]], contract), error = function(e) NULL)
      if (is.null(grid)) next
      ctx <- data.frame(
        country = sources$country[[i]],
        source_set = sources$source_set[[i]],
        adapter_family = sources$adapter_family[[i]],
        year = sources$year[[i]],
        file = file,
        sheet = sheets[[1L]],
        stringsAsFactors = FALSE
      )
      tables <- country_sna_explorer_detect_tables(grid, ctx, contract)
      if (!nrow(tables)) next
      table <- tables[1L, , drop = FALSE]
      table_rows[[length(table_rows) + 1L]] <- table
      sector <- country_sna_explorer_sector_from_file(file, rule)
      value_col <- NA_integer_
      if (ncol(grid) > 1L) {
        table_rows_seq <- seq.int(table$row_start, table$row_end)
        numeric_density <- vapply(seq_len(ncol(grid)), function(col) {
          if (col == table$code_col) return(0)
          mean(!is.na(country_sna_explorer_num(grid[table_rows_seq, col, drop = TRUE])))
        }, numeric(1))
        value_col <- which.max(numeric_density)
      }
      roles <- contract$economic_contract$roles
      for (role in names(roles)) {
        if (!identical(roles[[role]]$sector, sector)) next
        role_rows[[length(role_rows) + 1L]] <- data.frame(
          country = table$country,
          source_set = table$source_set,
          adapter_family = table$adapter_family,
          year = as.integer(table$year),
          file = file,
          sheet = table$sheet,
          table_id = table$table_id,
          role = role,
          role_col = value_col,
          role_col_letter = country_sna_explorer_col_letter(value_col),
          role_score = if (!is.na(value_col)) 0.65 else 0,
          sector_score = 1,
          direction_score = 0.35,
          numeric_score = if (!is.na(value_col)) 1 else 0,
          status = if (!is.na(value_col)) "accepted_low_confidence" else "missing_role",
          evidence = sprintf("sector file bundle: %s", sector),
          stringsAsFactors = FALSE
        )
      }
      roles_df <- country_sna_explorer_bind(role_rows)
      values <- country_sna_explorer_value_candidates(grid, table, roles_df[roles_df$table_id == table$table_id, , drop = FALSE], contract)
      value_rows[[length(value_rows) + 1L]] <- values
    }
  }
  list(
    tables = country_sna_explorer_bind(table_rows),
    roles = country_sna_explorer_bind(role_rows),
    values = country_sna_explorer_bind(value_rows)
  )
}

country_sna_explorer_account_sheet_values <- function(source_inventory, sheet_inventory, contract) {
  sources <- source_inventory[source_inventory$adapter_family == "account_sheet_workbook" & source_inventory$status != "no_file", , drop = FALSE]
  if (!nrow(sources)) return(data.frame(stringsAsFactors = FALSE))
  variables <- country_sna_explorer_primitive_specs(contract)
  country_sna_explorer_bind(lapply(seq_len(nrow(sources)), function(i) {
    data.frame(
      country = sources$country[[i]],
      source_set = sources$source_set[[i]],
      adapter_family = sources$adapter_family[[i]],
      year = as.integer(sources$year[[i]]),
      variable = vapply(variables, `[[`, character(1), "name"),
      account_code = vapply(variables, `[[`, character(1), "account"),
      sector_role = vapply(variables, `[[`, character(1), "role"),
      file = sources$file[[i]],
      sheet = NA_character_,
      table_id = NA_character_,
      value_raw = NA_real_,
      value_standardized = NA_real_,
      row_evidence = NA_integer_,
      column_evidence = NA_integer_,
      table_score = NA_real_,
      role_score = NA_real_,
      status = "layout_family_unhandled",
      status_group = "unresolved",
      resolution_stage = "layout",
      reason = "account_sheet_workbook_requires_dedicated_adapter",
      stringsAsFactors = FALSE
    )
  }))
}

country_sna_explorer_source_match_summary <- function(source_inventory) {
  if (!nrow(source_inventory)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(source_inventory[c("country", "source_set", "adapter_family", "selector")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    part <- source_inventory[
      source_inventory$country == key$country &
        source_inventory$source_set == key$source_set &
        source_inventory$adapter_family == key$adapter_family &
        source_inventory$selector == key$selector,
      ,
      drop = FALSE
    ]
    matched <- part[part$status %in% c("matched", "ambiguous_stem"), , drop = FALSE]
    parsed_years <- sort(unique(c(
      part$year[!is.na(part$year)],
      unlist(lapply(part$file[!is.na(part$file)], country_sna_explorer_parse_years), use.names = FALSE)
    )))
    data.frame(
      country = key$country,
      source_set = key$source_set,
      adapter_family = key$adapter_family,
      selector = key$selector,
      selector_records = nrow(part),
      matched_records = nrow(matched),
      unmatched_records = sum(part$status %in% c("no_file", "missing_indexed_file"), na.rm = TRUE),
      matched_files = length(unique(na.omit(matched$file))),
      first_matched_year = if (length(parsed_years)) min(parsed_years) else NA_integer_,
      last_matched_year = if (length(parsed_years)) max(parsed_years) else NA_integer_,
      statuses = paste(sort(unique(part$status)), collapse = ";"),
      matched_by = paste(sort(unique(na.omit(part$matched_by))), collapse = ";"),
      index_consistency = paste(sort(unique(na.omit(part$index_consistency))), collapse = ";"),
      notes = paste(sort(unique(na.omit(part$notes))), collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_extractability_summary <- function(source_inventory, value_candidates) {
  if (!nrow(source_inventory)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(source_inventory[c("country", "source_set", "adapter_family", "year")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    part <- value_candidates[
      value_candidates$country == key$country &
        value_candidates$source_set == key$source_set &
        value_candidates$adapter_family == key$adapter_family &
        (is.na(key$year) | is.na(value_candidates$year) | value_candidates$year == key$year),
      ,
      drop = FALSE
    ]
    accepted_high <- sum(part$status == "accepted_high_confidence", na.rm = TRUE)
    accepted_low <- sum(part$status == "accepted_low_confidence", na.rm = TRUE)
    unresolved <- sum(!part$status %in% c("accepted_high_confidence", "accepted_low_confidence"), na.rm = TRUE)
    unresolved_table <- sum(part$resolution_stage == "table", na.rm = TRUE)
    unresolved_role <- sum(part$resolution_stage == "role", na.rm = TRUE)
    unresolved_account <- sum(part$resolution_stage == "account", na.rm = TRUE)
    unresolved_value <- sum(part$resolution_stage == "value", na.rm = TRUE)
    unresolved_layout <- sum(part$resolution_stage == "layout", na.rm = TRUE)
    status <- if (!nrow(part)) {
      "no_value_candidates"
    } else if (accepted_high > 0 && unresolved == 0) {
      "all_high_confidence"
    } else if ((accepted_high + accepted_low) > 0) {
      "partial_or_low_confidence"
    } else {
      "unresolved"
    }
    data.frame(
      country = key$country,
      source_set = key$source_set,
      adapter_family = key$adapter_family,
      year = as.integer(key$year),
      accepted_high = accepted_high,
      accepted_low = accepted_low,
      unresolved = unresolved,
      unresolved_table = unresolved_table,
      unresolved_role = unresolved_role,
      unresolved_account = unresolved_account,
      unresolved_value = unresolved_value,
      unresolved_layout = unresolved_layout,
      status = status,
      unresolved_statuses = paste(sort(unique(part$status[part$status_group == "unresolved"])), collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_available_years <- function(source_inventory, sheet_inventory, contract = NULL) {
  if (!nrow(source_inventory)) return(data.frame(stringsAsFactors = FALSE))
  span_max <- as.integer(contract$source_discovery$filename_year_span_max %||% 35L)
  year_rows <- list()
  add_year_rows <- function(country, source_set, years, evidence) {
    years <- sort(unique(as.integer(years[!is.na(years)])))
    if (!length(years)) return(NULL)
    data.frame(
      country = country,
      source_set = source_set,
      year = years,
      evidence = evidence,
      stringsAsFactors = FALSE
    )
  }
  for (i in seq_len(nrow(source_inventory))) {
    src <- source_inventory[i, , drop = FALSE]
    if (identical(src$status[[1L]], "no_file")) next
    years <- src$year
    if (is.na(years)) {
      years <- country_sna_explorer_parse_years(src$file)
      evidence <- if (length(years) >= 2L) "source_filename_span" else "source_filename_year"
      years <- country_sna_explorer_expand_year_span(years, span_max)
    } else {
      evidence <- "source_selector_year"
    }
    row <- add_year_rows(src$country, src$source_set, years, evidence)
    if (!is.null(row)) year_rows[[length(year_rows) + 1L]] <- row
  }
  if (nrow(sheet_inventory)) {
    sheets <- sheet_inventory[sheet_inventory$status %in% c("likely_sheet", "possible_sheet"), , drop = FALSE]
    for (i in seq_len(nrow(sheets))) {
      sheet <- sheets[i, , drop = FALSE]
      years <- country_sna_explorer_parse_year_list(sheet$parsed_years)
      if (!length(years) && !is.na(sheet$year)) years <- sheet$year
      row <- add_year_rows(sheet$country, sheet$source_set, years, paste0("sheet_", sheet$status))
      if (!is.null(row)) year_rows[[length(year_rows) + 1L]] <- row
    }
  }
  years_long <- unique(country_sna_explorer_bind(year_rows))
  keys <- unique(source_inventory[c("country", "source_set")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    src <- source_inventory[source_inventory$country == key$country & source_inventory$source_set == key$source_set, , drop = FALSE]
    sheets <- sheet_inventory[sheet_inventory$country == key$country & sheet_inventory$source_set == key$source_set, , drop = FALSE]
    years <- years_long$year[years_long$country == key$country & years_long$source_set == key$source_set]
    years <- sort(unique(years[!is.na(years)]))
    evidence <- years_long$evidence[years_long$country == key$country & years_long$source_set == key$source_set]
    data.frame(
      country = key$country,
      source_set = key$source_set,
      year_count = length(years),
      first_year = if (length(years)) min(years) else NA_integer_,
      last_year = if (length(years)) max(years) else NA_integer_,
      years = paste(years, collapse = ","),
      matched_files = length(unique(na.omit(src$file[src$status != "no_file"]))),
      likely_sheets = sum(sheets$status == "likely_sheet", na.rm = TRUE),
      possible_sheets = sum(sheets$status == "possible_sheet", na.rm = TRUE),
      source_statuses = paste(sort(unique(src$status)), collapse = ";"),
      year_evidence = paste(sort(unique(evidence)), collapse = ";"),
      selector_types = paste(sort(unique(src$selector)), collapse = ";"),
      matched_by = paste(sort(unique(na.omit(src$matched_by))), collapse = ";"),
      index_consistency = paste(sort(unique(na.omit(src$index_consistency))), collapse = ";"),
      unmatched_selectors = sum(src$status %in% c("no_file", "missing_indexed_file"), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_extension_summary <- function(available_years) {
  if (!nrow(available_years)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(sort(unique(available_years$country)), function(country) {
    old <- available_years[available_years$country == country & available_years$source_set == "old", , drop = FALSE]
    new <- available_years[available_years$country == country & available_years$source_set == "new", , drop = FALSE]
    old_years <- country_sna_explorer_parse_year_list(old$years)
    new_years <- country_sna_explorer_parse_year_list(new$years)
    overlap <- intersect(old_years, new_years)
    extensions <- setdiff(new_years, old_years)
    missing_in_new <- setdiff(old_years, new_years)
    status <- if (!length(new_years)) {
      "no_new_years_detected"
    } else if (!length(old_years)) {
      "no_old_years_detected"
    } else if (!length(overlap)) {
      "no_overlap"
    } else if (length(extensions)) {
      "extension_found"
    } else {
      "overlap_only"
    }
    data.frame(
      country = country,
      old_years = paste(sort(old_years), collapse = ","),
      new_years = paste(sort(new_years), collapse = ","),
      overlap_years = paste(sort(overlap), collapse = ","),
      extension_years = paste(sort(extensions), collapse = ","),
      missing_in_new_years = paste(sort(missing_in_new), collapse = ","),
      status = status,
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_read_include_contract <- function(root, include_contract_path = NULL, fallback_contract = NULL) {
  include_contract_path <- include_contract_path %||% file.path(root, "config", "country_sna_include.yml")
  if (!file.exists(include_contract_path)) {
    return(list(
      variables = fallback_contract$economic_contract$variables %||% list(primitives = list(), derived = list()),
      post_merge_exclusions = fallback_contract$post_merge_exclusions %||% list()
    ))
  }
  country_sna_explorer_read_yaml(include_contract_path)
}

country_sna_explorer_include_variables <- function(include_contract) {
  primitives <- include_contract$variables$primitives %||% list()
  derived <- include_contract$variables$derived %||% list()
  primitive_rows <- lapply(primitives, function(spec) {
    data.frame(
      variable = spec$name %||% NA_character_,
      variable_type = "primitive",
      account_code = spec$account %||% NA_character_,
      sector_role = spec$role %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  derived_rows <- lapply(derived, function(spec) {
    data.frame(
      variable = spec$name %||% NA_character_,
      variable_type = "derived",
      account_code = NA_character_,
      sector_role = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(c(primitive_rows, derived_rows))
}

country_sna_explorer_exclusion_applies <- function(rule, country, year, variable) {
  min_year <- if (is.null(rule$years$min)) -Inf else as.integer(rule$years$min)
  max_year <- if (is.null(rule$years$max)) Inf else as.integer(rule$years$max)
  identical(rule$country, country) &&
    year >= min_year &&
    year <= max_year &&
    variable %in% as.character(rule$variables %||% character())
}

country_sna_explorer_expected_variable_status <- function(include_contract, country, year, variable) {
  exclusions <- include_contract$post_merge_exclusions %||% list()
  hit <- vapply(exclusions, country_sna_explorer_exclusion_applies, logical(1),
    country = country, year = year, variable = variable
  )
  if (any(hit)) "expected_contract_missing" else "expected_value"
}

country_sna_explorer_structure_summary <- function(extension_summary, source_inventory, sheet_inventory, value_candidates) {
  countries <- sort(unique(c(extension_summary$country, source_inventory$country)))
  if (!length(countries)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(countries, function(country) {
    ext <- extension_summary[extension_summary$country == country, , drop = FALSE]
    src <- source_inventory[source_inventory$country == country & source_inventory$source_set == "new", , drop = FALSE]
    sheets <- sheet_inventory[sheet_inventory$country == country & sheet_inventory$source_set == "new", , drop = FALSE]
    values <- value_candidates[value_candidates$country == country & value_candidates$source_set == "new", , drop = FALSE]
    matched_files <- length(unique(na.omit(src$file[src$status != "no_file"])))
    likely_sheets <- sum(sheets$status == "likely_sheet", na.rm = TRUE)
    possible_sheets <- sum(sheets$status == "possible_sheet", na.rm = TRUE)
    layout_unhandled <- sum(values$resolution_stage == "layout", na.rm = TRUE)
    accepted <- sum(values$status_group == "accepted", na.rm = TRUE)

    structure_status <- if (!nrow(ext) || identical(ext$status[[1L]], "no_new_years_detected")) {
      "no_new_years"
    } else if (layout_unhandled > 0L) {
      "layout_adapter_required"
    } else if (matched_files > 0L && (likely_sheets + possible_sheets) > 0L) {
      "structure_evidence_available"
    } else if (matched_files > 0L) {
      "structure_review_needed"
    } else {
      "no_new_files"
    }

    data.frame(
      country = country,
      extension_status = if (nrow(ext)) ext$status[[1L]] else NA_character_,
      structure_status = structure_status,
      matched_new_files = matched_files,
      likely_new_sheets = likely_sheets,
      possible_new_sheets = possible_sheets,
      developer_accepted_values = accepted,
      developer_unresolved_values = sum(values$status_group == "unresolved", na.rm = TRUE),
      note = switch(
        structure_status,
        no_new_years = "No new source years were detected.",
        layout_adapter_required = "Detected layout evidence needs a dedicated adapter before include can extract values.",
        structure_evidence_available = "Source and sheet evidence are available for include expectation checks.",
        structure_review_needed = "New files exist, but sheet/table evidence is too weak for a clean explorer classification.",
        no_new_files = "No matched new files were found.",
        "Inspect developer evidence tables."
      ),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_year_expectations <- function(extension_summary, structure_summary) {
  rows <- list()
  for (i in seq_len(nrow(extension_summary))) {
    ext <- extension_summary[i, , drop = FALSE]
    structure <- structure_summary[structure_summary$country == ext$country, , drop = FALSE]
    structure_status <- if (nrow(structure)) structure$structure_status[[1L]] else "structure_review_needed"
    old_years <- country_sna_explorer_parse_year_list(ext$old_years)
    new_years <- country_sna_explorer_parse_year_list(ext$new_years)
    overlap <- intersect(old_years, new_years)
    extensions <- setdiff(new_years, old_years)

    add_rows <- function(years, year_role, expected_action) {
      if (!length(years)) return(NULL)
      data.frame(
        country = ext$country,
        year = as.integer(sort(years)),
        year_role = year_role,
        extension_status = ext$status,
        structure_status = structure_status,
        expected_action = expected_action,
        include_scope = !structure_status %in% c("no_new_years", "no_new_files"),
        stringsAsFactors = FALSE
      )
    }

    if (!length(new_years)) {
      rows[[length(rows) + 1L]] <- data.frame(
        country = ext$country,
        year = NA_integer_,
        year_role = "no_new_years",
        extension_status = ext$status,
        structure_status = structure_status,
        expected_action = "no_new_years",
        include_scope = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }

    action_for <- function(default) {
      if (identical(structure_status, "layout_adapter_required")) "layout_adapter_required"
      else if (identical(structure_status, "structure_review_needed")) "structure_review_needed"
      else default
    }
    rows[[length(rows) + 1L]] <- add_rows(overlap, "overlap", action_for("overlap_confirm"))
    rows[[length(rows) + 1L]] <- add_rows(extensions, "extension", action_for("extension_expect_values"))
  }
  country_sna_explorer_bind(rows)
}

country_sna_explorer_variable_expectations <- function(year_expectations, include_contract) {
  scoped <- year_expectations[!is.na(year_expectations$include_scope) & year_expectations$include_scope, , drop = FALSE]
  if (!nrow(scoped)) return(data.frame(stringsAsFactors = FALSE))
  variables <- country_sna_explorer_include_variables(include_contract)
  if (!nrow(variables)) return(data.frame(stringsAsFactors = FALSE))

  # The explorer deliberately does not infer economic variables from workbook
  # layouts. It expands broad year expectations with the deterministic include
  # contract, so the include command can later warn about missing expected
  # values without trusting adaptive value guesses.
  rows <- lapply(seq_len(nrow(scoped)), function(i) {
    year_row <- scoped[i, , drop = FALSE]
    part <- variables
    part$country <- year_row$country
    part$year <- as.integer(year_row$year)
    part$year_role <- year_row$year_role
    part$expected_action <- year_row$expected_action
    part$structure_status <- year_row$structure_status
    part$expected_status <- vapply(part$variable, function(variable) {
      if (identical(year_row$expected_action[[1L]], "layout_adapter_required")) {
        return("blocked_adapter_required")
      }
      country_sna_explorer_expected_variable_status(include_contract, year_row$country[[1L]], year_row$year[[1L]], variable)
    }, character(1))
    part[, c(
      "country", "year", "year_role", "variable", "variable_type", "account_code",
      "sector_role", "expected_action", "structure_status", "expected_status"
    ), drop = FALSE]
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_value_status_summary <- function(value_candidates) {
  if (!nrow(value_candidates)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(value_candidates[c("country", "source_set", "year", "status", "status_group", "resolution_stage")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    part <- value_candidates[
      value_candidates$country == key$country &
        value_candidates$source_set == key$source_set &
        value_candidates$year == key$year &
        value_candidates$status == key$status &
        value_candidates$status_group == key$status_group &
        value_candidates$resolution_stage == key$resolution_stage,
      ,
      drop = FALSE
    ]
    data.frame(
      country = key$country,
      source_set = key$source_set,
      year = as.integer(key$year),
      status = key$status,
      status_group = key$status_group,
      resolution_stage = key$resolution_stage,
      values = nrow(part),
      variables = length(unique(part$variable)),
      example_reasons = paste(utils::head(sort(unique(part$reason)), 3L), collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_value_map <- function(value_candidates) {
  accepted <- value_candidates[value_candidates$status %in% c("accepted_high_confidence", "accepted_low_confidence"), , drop = FALSE]
  if (!nrow(accepted)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(accepted[c("country", "source_set", "year", "variable", "account_code", "sector_role")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    part <- accepted[
      accepted$country == key$country &
        accepted$source_set == key$source_set &
        accepted$year == key$year &
        accepted$variable == key$variable &
        accepted$account_code == key$account_code &
        accepted$sector_role == key$sector_role,
      ,
      drop = FALSE
    ]
    values <- unique(part$value_standardized[!is.na(part$value_standardized)])
    status <- if (!length(values)) {
      "accepted_missing_value"
    } else if (length(values) > 1L) {
      "accepted_duplicate_conflict"
    } else if (any(part$status == "accepted_high_confidence", na.rm = TRUE)) {
      "accepted_high_confidence"
    } else {
      "accepted_low_confidence"
    }
    data.frame(
      country = key$country,
      source_set = key$source_set,
      year = as.integer(key$year),
      variable = key$variable,
      account_code = key$account_code,
      sector_role = key$sector_role,
      value = if (length(values) == 1L) values[[1L]] else NA_real_,
      status = status,
      candidate_rows = nrow(part),
      evidence = paste(unique(part$reason), collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_overlap_revision_detail <- function(value_candidates, contract) {
  value_map <- country_sna_explorer_value_map(value_candidates)
  if (!nrow(value_map)) return(data.frame(stringsAsFactors = FALSE))
  threshold <- as.numeric(contract$thresholds$anecdotal_pct %||% contract$economic_contract$anecdotal_pct %||% 1)
  keys <- unique(value_map[c("country", "year", "variable", "account_code", "sector_role")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    part <- value_map[
      value_map$country == key$country &
        value_map$year == key$year &
        value_map$variable == key$variable &
        value_map$account_code == key$account_code &
        value_map$sector_role == key$sector_role,
      ,
      drop = FALSE
    ]
    old <- part[part$source_set == "old", , drop = FALSE]
    new <- part[part$source_set == "new", , drop = FALSE]
    old_value <- if (nrow(old)) old$value[[1L]] else NA_real_
    new_value <- if (nrow(new)) new$value[[1L]] else NA_real_
    ratio <- if (!is.na(old_value) && old_value != 0 && !is.na(new_value)) new_value / old_value else NA_real_
    pct_diff <- if (!is.na(ratio)) (ratio - 1) * 100 else NA_real_
    status <- if (!nrow(old)) {
      "no_old_candidate"
    } else if (!nrow(new)) {
      "no_new_candidate"
    } else if (old$status[[1L]] == "accepted_duplicate_conflict") {
      "old_duplicate_conflict"
    } else if (new$status[[1L]] == "accepted_duplicate_conflict") {
      "new_duplicate_conflict"
    } else if (is.na(old_value) && is.na(new_value)) {
      "both_missing"
    } else if (is.na(old_value)) {
      "old_missing"
    } else if (is.na(new_value)) {
      "new_missing"
    } else if (old_value == 0) {
      "old_zero"
    } else if (abs(pct_diff) == 0) {
      "unchanged"
    } else if (abs(pct_diff) < threshold) {
      "anecdotal_change"
    } else {
      "substantive_change"
    }
    data.frame(
      country = key$country,
      year = as.integer(key$year),
      variable = key$variable,
      account_code = key$account_code,
      sector_role = key$sector_role,
      old_value = old_value,
      new_value = new_value,
      ratio_new_old = ratio,
      pct_diff = pct_diff,
      old_status = if (nrow(old)) old$status[[1L]] else NA_character_,
      new_status = if (nrow(new)) new$status[[1L]] else NA_character_,
      status = status,
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_overlap_revision_summary <- function(revision_detail) {
  if (!nrow(revision_detail)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(revision_detail[c("country", "year")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    part <- revision_detail[revision_detail$country == key$country & revision_detail$year == key$year, , drop = FALSE]
    compared <- part$status %in% c("unchanged", "anecdotal_change", "substantive_change")
    data.frame(
      country = key$country,
      year = as.integer(key$year),
      compared_values = sum(compared, na.rm = TRUE),
      unchanged = sum(part$status == "unchanged", na.rm = TRUE),
      anecdotal_changes = sum(part$status == "anecdotal_change", na.rm = TRUE),
      substantive_changes = sum(part$status == "substantive_change", na.rm = TRUE),
      unresolved = sum(!compared, na.rm = TRUE),
      no_old_candidate = sum(part$status == "no_old_candidate", na.rm = TRUE),
      no_new_candidate = sum(part$status == "no_new_candidate", na.rm = TRUE),
      missing_values = sum(part$status %in% c("both_missing", "old_missing", "new_missing", "old_zero"), na.rm = TRUE),
      median_ratio_new_old = if (any(compared, na.rm = TRUE)) stats::median(part$ratio_new_old[compared], na.rm = TRUE) else NA_real_,
      max_abs_pct_diff = if (any(compared, na.rm = TRUE)) max(abs(part$pct_diff[compared]), na.rm = TRUE) else NA_real_,
      unresolved_statuses = paste(sort(unique(part$status[!compared])), collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_review_actions <- function(extension_summary, structure_summary) {
  countries <- sort(unique(c(extension_summary$country, structure_summary$country)))
  if (!length(countries)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(countries, function(country) {
    ext <- extension_summary[extension_summary$country == country, , drop = FALSE]
    structure <- structure_summary[structure_summary$country == country, , drop = FALSE]
    structure_status <- if (nrow(structure)) structure$structure_status[[1L]] else "structure_review_needed"
    action <- if (!nrow(ext) || identical(ext$status[[1L]], "no_new_years_detected")) {
      "keep_current"
    } else if (identical(structure_status, "layout_adapter_required")) {
      "layout_adapter_required"
    } else if (identical(ext$status[[1L]], "no_overlap")) {
      "include_dry_run_no_overlap"
    } else if (identical(structure_status, "structure_review_needed")) {
      "structure_review_needed"
    } else {
      "include_dry_run"
    }
    data.frame(
      country = country,
      action = action,
      extension_status = if (nrow(ext)) ext$status[[1L]] else NA_character_,
      structure_status = structure_status,
      matched_new_files = if (nrow(structure)) structure$matched_new_files[[1L]] else 0L,
      likely_new_sheets = if (nrow(structure)) structure$likely_new_sheets[[1L]] else 0L,
      next_command = if (action %in% c("include_dry_run", "include_dry_run_no_overlap", "structure_review_needed")) {
        "dina sources include country-sna --dry-run"
      } else {
        ""
      },
      note = switch(
        action,
        keep_current = "No new source years were detected; keep the current canonical source.",
        layout_adapter_required = "Detected layout evidence needs a dedicated adapter before include can extract values.",
        include_dry_run_no_overlap = "New source years exist without old/new overlap; include should check expected extension values.",
        structure_review_needed = "New files exist, but explorer evidence is weak; include dry-run should surface missing expectations.",
        include_dry_run = "Explorer found year coverage that should be checked by include dry-run.",
        "Inspect explorer outputs."
      ),
      stringsAsFactors = FALSE
    )
  })
  country_sna_explorer_bind(rows)
}

country_sna_explorer_audit <- function(contract, root, years = country_sna_explorer_years(contract, root), countries = NULL, include_contract = NULL) {
  rules <- country_sna_explorer_country_rules(contract)
  countries <- countries %||% country_sna_explorer_project_countries(contract, root)
  source_rows <- list()
  for (country in countries) {
    rule <- rules[[country]]
    if (is.null(rule)) next
    source_rows[[length(source_rows) + 1L]] <- country_sna_explorer_resolve_sources(country, "old", rule, years, root)
    source_rows[[length(source_rows) + 1L]] <- country_sna_explorer_resolve_sources(country, "new", rule, years, root)
  }
  source_inventory <- country_sna_explorer_bind(source_rows)
  sheet_inventory <- country_sna_explorer_sheet_inventory(source_inventory, contract)
  rectangular <- country_sna_explorer_audit_rectangular(source_inventory, sheet_inventory, contract)
  sector <- list(tables = data.frame(stringsAsFactors = FALSE), roles = data.frame(stringsAsFactors = FALSE), values = data.frame(stringsAsFactors = FALSE))
  for (country in countries) {
    rule <- rules[[country]]
    if (!is.null(rule) && identical(rule$adapter_family, "sector_file_bundle")) {
      sector <- country_sna_explorer_audit_sector_bundle(source_inventory[source_inventory$country == country, , drop = FALSE], rule, contract)
    }
  }
  account_values <- country_sna_explorer_account_sheet_values(source_inventory, sheet_inventory, contract)
  table_candidates <- country_sna_explorer_bind(rectangular$tables, sector$tables)
  role_candidates <- country_sna_explorer_bind(rectangular$roles, sector$roles)
  value_candidates <- country_sna_explorer_bind(rectangular$values, sector$values, account_values)
  source_match_summary <- country_sna_explorer_source_match_summary(source_inventory)
  extractability_summary <- country_sna_explorer_extractability_summary(source_inventory, value_candidates)
  available_years <- country_sna_explorer_available_years(source_inventory, sheet_inventory, contract)
  extension_summary <- country_sna_explorer_extension_summary(available_years)
  include_contract <- include_contract %||% country_sna_explorer_read_include_contract(root, fallback_contract = contract)
  structure_summary <- country_sna_explorer_structure_summary(extension_summary, source_inventory, sheet_inventory, value_candidates)
  year_expectations <- country_sna_explorer_year_expectations(extension_summary, structure_summary)
  variable_expectations <- country_sna_explorer_variable_expectations(year_expectations, include_contract)
  value_status_summary <- country_sna_explorer_value_status_summary(value_candidates)
  overlap_revision_detail <- country_sna_explorer_overlap_revision_detail(value_candidates, contract)
  overlap_revision_summary <- country_sna_explorer_overlap_revision_summary(overlap_revision_detail)
  review_actions <- country_sna_explorer_review_actions(extension_summary, structure_summary)
  list(
    source_inventory = source_inventory,
    sheet_inventory = sheet_inventory,
    table_candidates = table_candidates,
    role_candidates = role_candidates,
    value_candidates = value_candidates,
    source_match_summary = source_match_summary,
    extractability_summary = extractability_summary,
    available_years = available_years,
    extension_summary = extension_summary,
    structure_summary = structure_summary,
    year_expectations = year_expectations,
    variable_expectations = variable_expectations,
    overlap_revision_summary = overlap_revision_summary,
    overlap_revision_detail = overlap_revision_detail,
    value_status_summary = value_status_summary,
    review_actions = review_actions
  )
}

country_sna_explorer_write_workbook <- function(outputs, paths) {
  if (!country_sna_explorer_has("openxlsx")) {
    return(list(path = NA_character_, status = "openxlsx_missing"))
  }
  path <- file.path(paths$workbooks, "country_sna_explorer.xlsx")
  wb <- openxlsx::createWorkbook()
  for (name in names(outputs)) {
    openxlsx::addWorksheet(wb, substr(name, 1L, 31L))
    openxlsx::writeData(wb, substr(name, 1L, 31L), outputs[[name]])
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  list(path = path, status = "written")
}

country_sna_explorer_write_figures <- function(outputs, paths) {
  if (!country_sna_explorer_has("ggplot2")) {
    return(data.frame(figure = NA_character_, path = NA_character_, status = "ggplot2_missing", data_rows = 0L, stringsAsFactors = FALSE))
  }
  dir.create(paths$figures, recursive = TRUE, showWarnings = FALSE)
  rows <- list()

  available <- outputs$available_years
  if (is.data.frame(available) && nrow(available)) {
    coverage_rows <- list()
    for (i in seq_len(nrow(available))) {
      years <- country_sna_explorer_parse_year_list(available$years[[i]])
      if (!length(years)) next
      coverage_rows[[length(coverage_rows) + 1L]] <- data.frame(
        country = available$country[[i]],
        source_set = available$source_set[[i]],
        year = years,
        stringsAsFactors = FALSE
      )
    }
    coverage <- country_sna_explorer_bind(coverage_rows)
    if (nrow(coverage)) {
      plot <- ggplot2::ggplot(coverage, ggplot2::aes(x = year, y = country, fill = source_set)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.2) +
        ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "bottom")
      png <- file.path(paths$figures, "country_sna_year_coverage.png")
      pdf <- file.path(paths$figures, "country_sna_year_coverage.pdf")
      ggplot2::ggsave(png, plot, width = 8, height = 4.5, dpi = 160)
      ggplot2::ggsave(pdf, plot, width = 8, height = 4.5)
      rows[["coverage"]] <- data.frame(
        figure = "year_coverage",
        path = paste(c(png, pdf), collapse = ";"),
        status = "written",
        data_rows = nrow(coverage),
        stringsAsFactors = FALSE
      )
    }
  }

  revisions <- outputs$overlap_revision_detail
  if (is.data.frame(revisions) && nrow(revisions)) {
    comparable <- revisions[!is.na(revisions$ratio_new_old), , drop = FALSE]
    if (nrow(comparable)) {
      plot <- ggplot2::ggplot(comparable, ggplot2::aes(x = year, y = ratio_new_old, color = status, group = variable)) +
        ggplot2::geom_hline(yintercept = 1, color = "grey70", linewidth = 0.3) +
        ggplot2::geom_point(size = 1.2, alpha = 0.8) +
        ggplot2::facet_wrap(stats::as.formula("~ country")) +
        ggplot2::labs(x = NULL, y = "new / old", color = NULL) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(legend.position = "bottom")
      png <- file.path(paths$figures, "country_sna_overlap_revisions.png")
      pdf <- file.path(paths$figures, "country_sna_overlap_revisions.pdf")
      ggplot2::ggsave(png, plot, width = 9, height = 5, dpi = 160)
      ggplot2::ggsave(pdf, plot, width = 9, height = 5)
      rows[["revisions"]] <- data.frame(
        figure = "overlap_revisions",
        path = paste(c(png, pdf), collapse = ";"),
        status = "written",
        data_rows = nrow(comparable),
        stringsAsFactors = FALSE
      )
    }
  }

  extractability <- outputs$extractability_summary
  if (is.data.frame(extractability) && nrow(extractability)) {
    plot <- ggplot2::ggplot(extractability, ggplot2::aes(x = country, fill = status)) +
      ggplot2::geom_bar() +
      ggplot2::facet_wrap(stats::as.formula("~ source_set")) +
      ggplot2::labs(x = NULL, y = "country-year records", fill = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")
    png <- file.path(paths$figures, "country_sna_extractability.png")
    pdf <- file.path(paths$figures, "country_sna_extractability.pdf")
    ggplot2::ggsave(png, plot, width = 8, height = 4.5, dpi = 160)
    ggplot2::ggsave(pdf, plot, width = 8, height = 4.5)
    rows[["extractability"]] <- data.frame(
      figure = "extractability",
      path = paste(c(png, pdf), collapse = ";"),
      status = "written",
      data_rows = nrow(extractability),
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) {
    return(data.frame(figure = NA_character_, path = NA_character_, status = "no_figure_data", data_rows = 0L, stringsAsFactors = FALSE))
  }
  country_sna_explorer_bind(rows)
}

country_sna_explorer_write_outputs <- function(outputs, root, contract, output_dir = NULL, run_id = NULL) {
  paths <- country_sna_explorer_output_paths(root, contract, output_dir)
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
  run_id <- run_id %||% country_sna_explorer_run_id("explore")
  source_files <- if (is.data.frame(outputs$source_inventory) && nrow(outputs$source_inventory)) {
    files <- as.character(outputs$source_inventory$file %||% character())
    unique(files[!is.na(files) & nzchar(files) & file.exists(files)])
  } else {
    character()
  }
  source_fingerprints <- country_sna_explorer_bind(lapply(source_files, country_sna_explorer_file_fingerprint, root = root))
  outputs$source_fingerprints <- source_fingerprints
  csv_paths <- list()
  for (name in names(outputs)) {
    csv_path <- file.path(paths$tables, paste0(name, ".csv"))
    utils::write.csv(outputs[[name]], csv_path, row.names = FALSE, na = "")
    csv_paths[[name]] <- csv_path
  }
  workbook <- country_sna_explorer_write_workbook(outputs, paths)
  figures <- country_sna_explorer_write_figures(outputs, paths)
  table_list <- paste(sort(names(csv_paths)), collapse = ",")
  source_fingerprint_status <- if (nrow(source_fingerprints)) {
    paste(sort(unique(source_fingerprints$status)), collapse = ";")
  } else {
    "none"
  }
  metadata <- data.frame(
    key = c("run_id", "run_at", "output_root", "source_fingerprint_status", "tables", "figure_statuses", "review_actions"),
    value = c(
      run_id,
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      paths$root,
      source_fingerprint_status,
      table_list,
      paste(unique(figures$status), collapse = ";"),
      if (nrow(outputs$review_actions)) paste(sort(unique(outputs$review_actions$action)), collapse = ";") else ""
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(metadata, file.path(paths$logs, "run_metadata.csv"), row.names = FALSE)
  utils::write.csv(metadata, file.path(paths$logs, "explore_manifest.csv"), row.names = FALSE)
  utils::write.csv(figures, file.path(paths$logs, "figures.csv"), row.names = FALSE, na = "")
  list(paths = paths, csv = csv_paths, workbook = workbook, figures = figures)
}

run_country_sna_explorer <- function(
  root = country_sna_explorer_repo_root(),
  contract_path = file.path(root, "config", "country_sna_explorer.yml"),
  include_contract_path = file.path(root, "config", "country_sna_include.yml"),
  output_dir = NULL,
  years = NULL,
  countries = NULL,
  write_outputs = TRUE
) {
  contract <- country_sna_explorer_read_contract(root, contract_path)
  include_contract <- country_sna_explorer_read_include_contract(root, include_contract_path, fallback_contract = contract)
  years <- years %||% country_sna_explorer_years(contract, root)
  outputs <- country_sna_explorer_audit(contract, root, years = years, countries = countries, include_contract = include_contract)
  run_id <- country_sna_explorer_run_id("explore")
  written <- if (isTRUE(write_outputs)) {
    country_sna_explorer_write_outputs(outputs, root, contract, output_dir, run_id = run_id)
  } else {
    list(paths = country_sna_explorer_output_paths(root, contract, output_dir))
  }
  list(contract = contract, years = years, outputs = outputs, written = written, paths = written$paths, run_id = run_id)
}
