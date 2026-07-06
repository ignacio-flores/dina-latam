# Country-SNA current-extractor diagnostic
#
# This file is intentionally independent from the production Stata pipeline. It
# reads old and `_new` source workbooks, mimics the values currently extracted by
# code/Stata/01b-add-country-sna.do, and writes diagnostic tables. It does not
# validate, integrate, move, or rewrite source data.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

country_sna_repo_root <- function(start = getwd()) {
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

country_sna_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

country_sna_read_config <- function(root = country_sna_repo_root()) {
  country_sna_need("yaml")
  path <- file.path(root, "config", "dina.yml")
  if (!file.exists(path)) {
    stop("Missing config file: ", path, call. = FALSE)
  }
  yaml::read_yaml(path)
}

# Centralized extraction contract.
#
# This is the only place where country-specific knowledge should live in v1:
# file patterns, sheet rules, cell ranges, and role columns. The rest of the
# code consumes these rules generically. That makes future source-format changes
# mostly a rule-table edit, not a rewrite of the diagnostic engine.
country_sna_rules <- function() {
  # These are the D/B transaction codes currently touched by 01b. The diagnostic
  # deliberately ignores the rest of each workbook.
  common_d_codes <- c(11L, 1L, 44L, 43L, 4L, 5L, 61L, 62L, 752L, 75L, 7L)
  sector_d_codes <- c(4L, 41L, 42L, 43L, 44L)

  list(
    CHL = list(
      layout_family = "year_sheets",
      compare = TRUE,
      old = list(type = "single_stem", stem = "input_data/sna_country_data/CHL/CEI_merged"),
      new = list(type = "single_pattern", pattern = "input_data/_new/country_sna/CHL/CEI_anuario_*.xls*"),
      sheet = list(type = "year_variants"),
      cell_range = "E27:Q48",
      # Roles mirror the normalized column names created in Stata:
      # households_r/u are resources/uses; NFC/FC/GG/TOT/ROW are institutional
      # sectors. The letters are workbook columns within the full sheet, not
      # positions inside the imported range.
      columns = c(
        code = "I", code_long = "J", households_r = "N", households_u = "E",
        NFC_r = "K", FC_r = "L", GG_r = "M", TOT_r = "Q", ROW_r = "P"
      ),
      d_codes = common_d_codes,
      sector_d_codes = sector_d_codes,
      b5_alias = c("B.5", "B5", "B.5b", "B5b"),
      b2_special = TRUE,
      notes = "Production code reads CEI_merged by stem; warn when .xls and .xlsx both exist."
    ),
    COL = list(
      layout_family = "year_sheets",
      compare = TRUE,
      old = list(type = "single_pattern", pattern = "input_data/sna_country_data/COL/*CuentasEconomicasIntegradas*.xls*"),
      new = list(type = "single_pattern", pattern = "input_data/_new/country_sna/COL/*CuentasEconomicasIntegradas*.xls*"),
      sheet = list(type = "year_variants"),
      cell_range = "F49:T146",
      columns = c(
        code = "J", code_long = "K", households_r = "O", households_u = "F",
        NFC_r = "L", FC_r = "M", GG_r = "N", TOT_r = "T", ROW_r = "R"
      ),
      d_codes = common_d_codes,
      sector_d_codes = sector_d_codes,
      b5_alias = c("B.5b", "B5b", "B.5", "B5")
    ),
    CRI = list(
      layout_family = "year_sheets",
      compare = TRUE,
      old = list(type = "year_pattern", pattern = "input_data/sna_country_data/CRI/Cuentas_Economicas_Integradas_{year}.xls*"),
      new = list(type = "year_pattern", pattern = "input_data/_new/country_sna/CRI/CEI{year}.xls*"),
      sheet = list(type = "cri_current_accounts"),
      cell_range = "A26:AU91",
      columns = c(
        code = "B", code_long = "C", households_r = "AO", households_u = "AN",
        NFC_r = "E", FC_r = "M", GG_r = "AE", TOT_r = "AS", ROW_r = "AU"
      ),
      d_codes = common_d_codes,
      sector_d_codes = sector_d_codes,
      b5_alias = c("B.5", "B5", "B.5b", "B5b"),
      notes = "Current downstream snacompare later blanks CRI 2017 CEI household values."
    ),
    PER = list(
      layout_family = "year_sheets",
      compare = TRUE,
      old = list(type = "single_stem", stem = "input_data/sna_country_data/PER/CEI_merged"),
      new = list(type = "year_pattern", pattern = "input_data/_new/country_sna/PER/cei_econtotal_{year}*.xls*"),
      sheet = list(type = "year_variants"),
      cell_range = "A21:U40",
      columns = c(
        code = "K", code_long = "L", households_r = "P", households_u = "G",
        NFC_r = "M", FC_r = "N", GG_r = "O", TOT_r = "U", ROW_r = "S"
      ),
      d_codes = common_d_codes,
      sector_d_codes = sector_d_codes,
      b5_alias = c("B.5b", "B5b", "B.5", "B5")
    ),
    MEX = list(
      layout_family = "year_files",
      compare = TRUE,
      # Stata maps MEX year y to CSI_(y - 2000). The diagnostic keeps that
      # narrow mapping so hundreds of unrelated CSI files do not pollute the
      # comparison.
      old = list(type = "mex_year", pattern = "input_data/sna_country_data/MEX/CSI_{index}.xlsx"),
      new = list(type = "mex_year", pattern = "input_data/_new/country_sna/MEX/**/CSI_{index}.xlsx"),
      sheet = list(type = "fixed", value = "Tabulado"),
      cell_range = "A68:S153",
      columns = c(
        code = "A", code_long = "A", households_r = "I", households_u = "H",
        NFC_r = "C", FC_r = "E", GG_r = "G", TOT_r = "S", ROW_r = "N"
      ),
      d_codes = common_d_codes,
      sector_d_codes = sector_d_codes,
      b5_alias = c("B.5b", "B5b", "B.5", "B5"),
      notes = "Only CSI_(year - 2000) files are part of the current extraction contract."
    ),
    URY = list(
      layout_family = "sector_year_files",
      compare = TRUE,
      # URY is not a direct workbook read in 01b: aux_sna_ury.do first combines
      # sector files into cei.xlsx. Here we reproduce the same read logic in
      # memory and never write that intermediate workbook.
      old = list(type = "ury_sector", folder = "input_data/sna_country_data/URY"),
      new = list(type = "ury_sector", folder = "input_data/_new/country_sna/URY"),
      d_codes = common_d_codes,
      sector_d_codes = sector_d_codes,
      b5_alias = c("B.5", "B5", "B.5b", "B5b"),
      notes = "The diagnostic recreates aux_sna_ury.do in memory and does not write cei.xlsx."
    ),
    ECU = list(
      layout_family = "layout_break_account_sheets",
      compare = FALSE,
      status = "adapter_missing",
      # Ecuador is intentionally inventoried, not force-compared. The current
      # extractor expects year sheets, while the new file is organized by
      # account sheets. Treating this as a simple sheet-name variation would
      # hide the real design work needed for an adapter.
      old = list(type = "single_pattern", pattern = "input_data/sna_country_data/ECU/mcs_cei_*.xls*"),
      new = list(type = "single_pattern", pattern = "input_data/_new/country_sna/ECU/bam_cei_*.xls*"),
      notes = "New workbook is account-sheet layout; current extractor expects year sheets."
    ),
    BRA = list(
      layout_family = "year_sheets",
      compare = FALSE,
      status = "no_new_files",
      old = list(type = "single_pattern", pattern = "input_data/sna_country_data/BRA/*.xls*"),
      new = list(type = "single_pattern", pattern = "input_data/_new/country_sna/BRA/*.xls*")
    ),
    DOM = list(
      layout_family = "year_sheets",
      compare = FALSE,
      status = "no_new_files",
      old = list(type = "single_pattern", pattern = "input_data/sna_country_data/DOM/*.xls*"),
      new = list(type = "single_pattern", pattern = "input_data/_new/country_sna/DOM/*.xls*")
    )
  )
}

country_sna_excel_col_to_index <- function(col) {
  col <- toupper(as.character(col))
  vapply(strsplit(col, ""), function(chars) {
    chars <- chars[nzchar(chars)]
    sum(match(chars, LETTERS) * 26^rev(seq_along(chars) - 1L))
  }, numeric(1))
}

country_sna_parse_cell_range <- function(cell_range) {
  parts <- regmatches(cell_range, regexec("^([A-Z]+)([0-9]+):([A-Z]+)([0-9]+)$", cell_range))[[1]]
  if (length(parts) != 5L) {
    stop("Invalid Excel cell range: ", cell_range, call. = FALSE)
  }
  list(
    first_col = parts[[2]],
    first_row = as.integer(parts[[3]]),
    last_col = parts[[4]],
    last_row = as.integer(parts[[5]])
  )
}

country_sna_range_col_position <- function(col, cell_range) {
  parsed <- country_sna_parse_cell_range(cell_range)
  country_sna_excel_col_to_index(col) - country_sna_excel_col_to_index(parsed$first_col) + 1L
}

country_sna_normalize_code <- function(x) {
  # Statistical offices vary between D4/D.4, B5/B.5, extra spaces, and
  # non-breaking spaces. Normalize those superficial differences before code
  # matching; do not infer new economic concepts here.
  x <- as.character(x)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- trimws(x)
  x <- gsub("\\s+", "", x)
  x <- toupper(x)
  x <- gsub("^D([0-9])", "D.\\1", x)
  x <- gsub("^B([0-9])", "B.\\1", x)
  x
}

country_sna_num <- function(x) {
  # Keep comparisons in raw workbook units. This only strips common thousands
  # separators so the diagnostic can compare numeric cells.
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x <- gsub("\u00a0", " ", as.character(x), fixed = TRUE)
  x <- gsub(",", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

country_sna_path <- function(path, root) {
  if (grepl("^/", path)) path else file.path(root, path)
}

country_sna_glob <- function(pattern, root) {
  pattern <- country_sna_path(pattern, root)
  sort(Sys.glob(pattern))
}

country_sna_parse_years <- function(x) {
  matches <- gregexpr("(19|20)[0-9]{2}", basename(x), perl = TRUE)
  years <- unique(as.integer(unlist(regmatches(basename(x), matches))))
  years[!is.na(years)]
}

country_sna_sheet_years <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(integer())
  }
  country_sna_need("readxl")
  sheets <- readxl::excel_sheets(path)
  matches <- gregexpr("(19|20)[0-9]{2}", sheets, perl = TRUE)
  years <- unique(as.integer(unlist(regmatches(sheets, matches))))
  sort(years[!is.na(years)])
}

country_sna_file_score <- function(path) {
  # When a pattern finds several plausible files, prefer the newest year in the
  # filename, then the newest mtime. Ambiguity is still reported in inventory.
  years <- country_sna_parse_years(path)
  c(max_year = max(c(years, -Inf)), mtime = as.numeric(file.info(path)$mtime %||% 0))
}

country_sna_choose_file <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return(NA_character_)
  }
  scores <- t(vapply(paths, country_sna_file_score, numeric(2)))
  paths[order(scores[, "max_year"], scores[, "mtime"], paths, decreasing = TRUE)][[1]]
}

country_sna_single_stem_candidates <- function(stem, root) {
  stem <- country_sna_path(stem, root)
  unique(c(stem, paste0(stem, ".xlsx"), paste0(stem, ".xls")))
}

country_sna_resolve_single_file <- function(spec, root) {
  if (identical(spec$type, "single_stem")) {
    candidates <- country_sna_single_stem_candidates(spec$stem, root)
  } else {
    candidates <- country_sna_glob(spec$pattern, root)
  }
  candidates <- candidates[file.exists(candidates)]
  selected <- country_sna_choose_file(candidates)
  warnings <- character()
  if (length(candidates) > 1L) {
    # This is expected for stems like CEI_merged where both .xls and .xlsx may
    # exist. We select deterministically but keep the warning visible.
    warnings <- sprintf(
      "Multiple candidate workbooks found; selected %s from %s.",
      basename(selected),
      paste(basename(candidates), collapse = ", ")
    )
  }
  list(file = selected, candidates = candidates, warnings = warnings)
}

country_sna_year_pattern_files <- function(spec, years, root) {
  # Year-pattern matching handles families like PER/CRI where each year is a
  # separate official workbook. The rule supplies the pattern and this function
  # supplies the year loop.
  files <- stats::setNames(rep(NA_character_, length(years)), years)
  warnings <- character()
  for (year in years) {
    pattern <- gsub("\\{year\\}", as.character(year), spec$pattern)
    candidates <- country_sna_glob(pattern, root)
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates)) {
      files[[as.character(year)]] <- country_sna_choose_file(candidates)
    }
    if (length(candidates) > 1L) {
      warnings <- c(warnings, sprintf(
        "Year %s has multiple candidate workbooks; selected %s from %s.",
        year,
        basename(files[[as.character(year)]]),
        paste(basename(candidates), collapse = ", ")
      ))
    }
  }
  list(files = files, warnings = warnings)
}

country_sna_mex_files <- function(spec, years, root) {
  # Keep Mexico tied to the current Stata formula: year -> CSI_(year - 2000).
  # Other CSI files are inventoried elsewhere as extra source material.
  files <- stats::setNames(rep(NA_character_, length(years)), years)
  warnings <- character()
  for (year in years) {
    index <- year - 2000L
    pattern <- gsub("\\{index\\}", as.character(index), spec$pattern)
    candidates <- country_sna_glob(pattern, root)
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates)) {
      files[[as.character(year)]] <- country_sna_choose_file(candidates)
    }
    if (length(candidates) > 1L) {
      warnings <- c(warnings, sprintf(
        "MEX year %s / CSI_%s has multiple candidates; selected %s.",
        year,
        index,
        basename(files[[as.character(year)]])
      ))
    }
  }
  list(files = files, warnings = warnings)
}

country_sna_ury_files <- function(spec, years, root) {
  folder <- country_sna_path(spec$folder, root)
  sectors <- c(households = "S.1402", GG = "S.1301", ROW = "S.2000")
  files <- list()
  for (year in years) {
    files[[as.character(year)]] <- stats::setNames(
      file.path(folder, sprintf("CSI_3.%s_SC_%s.xlsx", year, sectors)),
      names(sectors)
    )
  }
  list(files = files, warnings = character())
}

country_sna_resolve_files <- function(spec, years, root) {
  if (identical(spec$type, "single_stem") || identical(spec$type, "single_pattern")) {
    single <- country_sna_resolve_single_file(spec, root)
    return(list(
      files = stats::setNames(rep(single$file, length(years)), years),
      candidates = single$candidates,
      warnings = single$warnings
    ))
  }
  if (identical(spec$type, "year_pattern")) {
    return(country_sna_year_pattern_files(spec, years, root))
  }
  if (identical(spec$type, "mex_year")) {
    return(country_sna_mex_files(spec, years, root))
  }
  if (identical(spec$type, "ury_sector")) {
    return(country_sna_ury_files(spec, years, root))
  }
  stop("Unknown file resolver type: ", spec$type, call. = FALSE)
}

country_sna_discover_extension_years <- function(spec, root) {
  # Extension detection is intentionally broader than active-year comparison.
  # It looks at filenames and sheet names so a 2024 source is reported even when
  # config/dina.yml still ends at 2023.
  if (identical(spec$type, "single_stem")) {
    candidates <- country_sna_single_stem_candidates(spec$stem, root)
    candidates <- candidates[file.exists(candidates)]
    file <- country_sna_choose_file(candidates)
    return(sort(unique(c(country_sna_parse_years(file), country_sna_sheet_years(file)))))
  }
  if (identical(spec$type, "single_pattern")) {
    files <- country_sna_glob(spec$pattern, root)
    return(sort(unique(c(unlist(lapply(files, country_sna_parse_years)), unlist(lapply(files, country_sna_sheet_years))))))
  }
  if (identical(spec$type, "year_pattern")) {
    files <- country_sna_glob(gsub("\\{year\\}", "*", spec$pattern), root)
    return(sort(unique(c(unlist(lapply(files, country_sna_parse_years)), unlist(lapply(files, country_sna_sheet_years))))))
  }
  if (identical(spec$type, "mex_year")) {
    files <- country_sna_glob(gsub("\\{index\\}", "*", spec$pattern), root)
    indexes <- suppressWarnings(as.integer(sub("^CSI_([0-9]+)\\.xlsx$", "\\1", basename(files))))
    return(sort(unique(2000L + indexes[!is.na(indexes)])))
  }
  if (identical(spec$type, "ury_sector")) {
    folder <- country_sna_path(spec$folder, root)
    files <- Sys.glob(file.path(folder, "CSI_3.*_SC_S.*.xlsx"))
    return(sort(unique(country_sna_parse_years(files))))
  }
  integer()
}

country_sna_sheet_variants <- function(year) {
  # These are naming variants only. Structural changes, such as account-sheet
  # layouts, are handled separately in the layout summary.
  c(
    as.character(year),
    sprintf("CEI_%s", year),
    sprintf("CEI_%sp", year),
    sprintf("CEI%s", year),
    sprintf("CEI%sP", year)
  )
}

country_sna_resolve_sheet <- function(path, year, sheet_spec) {
  if (is.na(path) || !file.exists(path)) {
    return(list(sheet = NA_character_, status = "missing_file", warning = NA_character_))
  }
  country_sna_need("readxl")
  sheets <- readxl::excel_sheets(path)
  if (identical(sheet_spec$type, "fixed")) {
    candidates <- sheet_spec$value
  } else if (identical(sheet_spec$type, "cri_current_accounts")) {
    candidates <- if (year >= 2017L) "CUENTAS CORRIENTES" else sprintf("CEI%s", year)
  } else {
    candidates <- country_sna_sheet_variants(year)
  }
  hit <- sheets[tolower(trimws(sheets)) %in% tolower(trimws(candidates))]
  if (length(hit)) {
    return(list(sheet = hit[[1]], status = "matched", warning = NA_character_))
  }
  list(
    sheet = NA_character_,
    status = "missing_sheet",
    warning = sprintf("No matching sheet for year %s in %s.", year, basename(path))
  )
}

country_sna_read_range <- function(path, sheet, cell_range, columns) {
  country_sna_need("readxl")
  parsed <- country_sna_parse_cell_range(cell_range)
  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet = sheet,
    range = cell_range,
    col_names = FALSE,
    .name_repair = "minimal"
  ))
  if (!nrow(raw)) {
    return(data.frame())
  }

  positions <- vapply(columns, country_sna_range_col_position, numeric(1), cell_range = cell_range)
  available <- positions <= ncol(raw) & positions >= 1L
  positions <- positions[available]
  columns <- columns[available]
  raw_code <- as.character(raw[[positions[["code"]]]])
  if ("code_long" %in% names(positions) && positions[["code"]] == positions[["code_long"]]) {
    # Mexico stores code and label in the same cell as "CODE - label"; Stata
    # splits column A on "-". Mirror only that existing production behavior.
    raw_code <- sub("-.*$", "", raw_code)
  }
  code <- country_sna_normalize_code(raw_code)
  code_long <- if ("code_long" %in% names(positions)) as.character(raw[[positions[["code_long"]]]]) else NA_character_

  rows <- list()
  for (role in setdiff(names(positions), c("code", "code_long"))) {
    rows[[role]] <- data.frame(
      role = role,
      code = code,
      code_long = code_long,
      value_raw = as.character(raw[[positions[[role]]]]),
      value = country_sna_num(raw[[positions[[role]]]]),
      row_in_range = seq_len(nrow(raw)),
      excel_row = parsed$first_row + seq_len(nrow(raw)) - 1L,
      excel_col = unname(columns[[role]]),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

country_sna_pick_value <- function(rows, variable, role, aliases) {
  # Select exactly one source value for one output variable. Duplicate rows are
  # allowed only if they carry the same numeric value; conflicting duplicates are
  # flagged instead of guessed.
  if (!nrow(rows)) {
    return(country_sna_value_row(variable, role, aliases, NA_real_, NA_character_, "source_missing"))
  }
  aliases_norm <- country_sna_normalize_code(aliases)
  candidates <- rows[rows$role == role & rows$code %in% aliases_norm, , drop = FALSE]
  if (!nrow(candidates)) {
    return(country_sna_value_row(variable, role, aliases, NA_real_, NA_character_, "no_code_match"))
  }
  usable <- candidates[!is.na(candidates$value), , drop = FALSE]
  if (!nrow(usable)) {
    return(country_sna_value_row(variable, role, aliases, NA_real_, candidates$value_raw[[1]], "non_numeric"))
  }
  unique_values <- unique(usable$value)
  if (length(unique_values) > 1L) {
    out <- usable[1L, , drop = FALSE]
    return(country_sna_value_row(
      variable,
      role,
      aliases,
      NA_real_,
      out$value_raw,
      "duplicate_conflict",
      out
    ))
  }
  out <- usable[1L, , drop = FALSE]
  status <- if (nrow(usable) > 1L) "duplicate_identical" else "matched"
  country_sna_value_row(variable, role, aliases, out$value, out$value_raw, status, out)
}

country_sna_value_row <- function(variable, role, aliases, value, raw_value, status, evidence = NULL) {
  if (is.null(evidence)) {
    evidence <- data.frame(
      code = NA_character_, code_long = NA_character_, row_in_range = NA_integer_,
      excel_row = NA_integer_, excel_col = NA_character_, stringsAsFactors = FALSE
    )
  }
  data.frame(
    variable = variable,
    value_type = "primitive",
    role = role %||% NA_character_,
    code_aliases = paste(aliases %||% character(), collapse = "|"),
    matched_code = evidence$code[[1]] %||% NA_character_,
    code_long = evidence$code_long[[1]] %||% NA_character_,
    value = as.numeric(value),
    value_raw = raw_value %||% NA_character_,
    extract_status = status,
    row_in_range = evidence$row_in_range[[1]] %||% NA_integer_,
    excel_row = evidence$excel_row[[1]] %||% NA_integer_,
    excel_col = evidence$excel_col[[1]] %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

country_sna_b_aliases <- function(rule, country, bcode) {
  if (bcode == 5L) {
    return(rule$b5_alias %||% c("B.5", "B5", "B.5b", "B5b"))
  }
  if (bcode == 2L && isTRUE(rule$b2_special)) {
    # Chile reports operating surplus and mixed income together. The grouped
    # B.2b/B.3b row has priority because that is what the current Stata logic
    # searches for before falling back to plain B.2 variants.
    return(list(c("B.2b/B.3b", "B.2b y B.3b", "B.2b / B.3b"), c("B.2", "B2", "B.2b", "B2b")))
  }
  c(sprintf("B.%s", bcode), sprintf("B%s", bcode), sprintf("B.%sb", bcode), sprintf("B%sb", bcode))
}

country_sna_pick_priority <- function(rows, variable, role, alias_groups) {
  if (!is.list(alias_groups)) {
    return(country_sna_pick_value(rows, variable, role, alias_groups))
  }
  last <- NULL
  for (aliases in alias_groups) {
    picked <- country_sna_pick_value(rows, variable, role, aliases)
    if (picked$extract_status %in% c("matched", "duplicate_identical")) {
      return(picked)
    }
    last <- picked
  }
  last
}

country_sna_extract_values <- function(rows, country, year, rule) {
  # Recreate the current production value set: household D-codes, household
  # B-codes, selected sector D-codes, and selected sector B.5 values. This is
  # not a general CEI parser.
  out <- list()
  d_roles <- c(
    `11` = "households_r", `1` = "households_r", `44` = "households_r",
    `43` = "households_u", `4` = "households_r", `5` = "households_u",
    `61` = "households_u", `62` = "households_r", `752` = "households_r",
    `75` = "households_r", `7` = "households_r"
  )
  for (code in names(d_roles)) {
    variable <- sprintf("D%s_cei", code)
    out[[variable]] <- country_sna_pick_value(
      rows,
      variable,
      d_roles[[code]],
      c(sprintf("D.%s", code), sprintf("D%s", code))
    )
  }

  for (bcode in c(2L, 3L, 5L)) {
    variable <- sprintf("B%sg_cei", bcode)
    out[[variable]] <- country_sna_pick_priority(
      rows,
      variable,
      "households_r",
      country_sna_b_aliases(rule, country, bcode)
    )
    if (bcode == 5L && country != "URY") {
      for (sector in c("NFC", "FC", "GG", "TOT")) {
        variable <- sprintf("%s_B5g_cei", sector)
        out[[variable]] <- country_sna_pick_value(
          rows,
          variable,
          sprintf("%s_r", sector),
          country_sna_b_aliases(rule, country, 5L)
        )
      }
    }
  }

  for (code in rule$sector_d_codes %||% integer()) {
    for (sector in c("NFC", "FC", "GG", "TOT", "ROW")) {
      variable <- sprintf("%s_r_D%s_cei", sector, code)
      out[[variable]] <- country_sna_pick_value(
        rows,
        variable,
        sprintf("%s_r", sector),
        c(sprintf("D.%s", code), sprintf("D%s", code))
      )
    }
  }

  values <- do.call(rbind, out)
  values$country <- country
  values$year <- year
  values <- values[, c("country", "year", setdiff(names(values), c("country", "year")))]
  country_sna_add_ratios(values)
}

country_sna_add_ratios <- function(values) {
  # Ratios are included because 01b graphs and stores them, but they are derived
  # from the primitive raw workbook values extracted above.
  lookup <- stats::setNames(values$value, values$variable)
  ratio_specs <- list(
    ratio_d43_d4 = c("D43_cei", "D4_cei"),
    ratio_d44_d4 = c("D44_cei", "D4_cei"),
    ratio_d44_b5g = c("D44_cei", "B5g_cei"),
    ratio_d75_d7 = c("D75_cei", "D7_cei"),
    ratio_d75_b5g = c("D75_cei", "B5g_cei")
  )
  rows <- list()
  for (name in names(ratio_specs)) {
    numerator <- lookup[[ratio_specs[[name]][[1]]]]
    denominator <- lookup[[ratio_specs[[name]][[2]]]]
    value <- if (!is.null(numerator) && !is.null(denominator) && !is.na(denominator) && denominator != 0) {
      numerator / denominator
    } else {
      NA_real_
    }
    rows[[name]] <- data.frame(
      country = values$country[[1]],
      year = values$year[[1]],
      variable = name,
      value_type = "derived_ratio",
      role = NA_character_,
      code_aliases = NA_character_,
      matched_code = NA_character_,
      code_long = NA_character_,
      value = value,
      value_raw = NA_character_,
      extract_status = if (is.na(value)) "ratio_missing_input" else "computed",
      row_in_range = NA_integer_,
      excel_row = NA_integer_,
      excel_col = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  ratio_d43d44 <- rows$ratio_d43_d4$value + rows$ratio_d44_d4$value
  rows$ratio_d43d44 <- rows$ratio_d43_d4
  rows$ratio_d43d44$variable <- "ratio_d43d44"
  rows$ratio_d43d44$value <- ratio_d43d44
  rows$ratio_d43d44$extract_status <- if (is.na(ratio_d43d44)) "ratio_missing_input" else "computed"
  rbind(values, do.call(rbind, rows))
}

country_sna_extract_workbook_year <- function(country, year, side, path, rule) {
  if (is.na(path) || !file.exists(path)) {
    return(list(values = data.frame(), inventory = country_sna_inventory_row(country, side, year, path, NA, "missing_file", rule)))
  }
  sheet <- country_sna_resolve_sheet(path, year, rule$sheet)
  if (!identical(sheet$status, "matched")) {
    return(list(values = data.frame(), inventory = country_sna_inventory_row(country, side, year, path, NA, sheet$status, rule, sheet$warning)))
  }
  rows <- country_sna_read_range(path, sheet$sheet, rule$cell_range, rule$columns)
  values <- country_sna_extract_values(rows, country, year, rule)
  values$source_side <- side
  values$source_file <- path
  values$sheet <- sheet$sheet
  values$cell_range <- rule$cell_range
  list(values = values, inventory = country_sna_inventory_row(country, side, year, path, sheet$sheet, "matched", rule))
}

country_sna_inventory_row <- function(country, side, year, file, sheet, status, rule, warning = NA_character_) {
  data.frame(
    country = country,
    side = side,
    year = year %||% NA_integer_,
    file = file %||% NA_character_,
    sheet = sheet %||% NA_character_,
    status = status,
    layout_family = rule$layout_family %||% NA_character_,
    warning = warning %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

country_sna_read_ury_sector <- function(path, sector_name) {
  if (is.na(path) || !file.exists(path)) {
    return(data.frame())
  }
  country_sna_need("readxl")
  raw <- suppressMessages(readxl::read_excel(path, col_names = FALSE, .name_repair = "minimal"))
  if (ncol(raw) < 7L || !nrow(raw)) {
    return(data.frame())
  }
  data.frame(
    code = country_sna_normalize_code(raw[[5]]),
    code_long = as.character(raw[[6]]),
    value = country_sna_num(raw[[7]]),
    value_raw = as.character(raw[[7]]),
    excel_row = seq_len(nrow(raw)),
    stringsAsFactors = FALSE
  )
}

country_sna_ury_rows <- function(paths) {
  # In aux_sna_ury.do, "por cobrar" becomes resources (_r) and "por pagar"
  # becomes uses (_u). Keep that transformation in memory for diagnostics.
  sectors <- c(households = "households", GG = "GG", ROW = "ROW")
  out <- list()
  for (name in names(sectors)) {
    sector <- country_sna_read_ury_sector(paths[[name]], sectors[[name]])
    if (!nrow(sector)) {
      next
    }
    for (flow in c(r = "por cobrar", u = "por pagar")) {
      keep <- grepl("B", sector$code, fixed = TRUE) | grepl(flow, sector$code_long, fixed = TRUE)
      selected <- sector[keep, , drop = FALSE]
      if (identical(unname(flow), "por cobrar")) {
        selected <- selected[!grepl("por pagar", selected$code_long, fixed = TRUE), , drop = FALSE]
      }
      if (identical(unname(flow), "por pagar")) {
        selected <- selected[!grepl("por cobrar", selected$code_long, fixed = TRUE), , drop = FALSE]
      }
      if (!nrow(selected)) {
        next
      }
      selected$role <- sprintf("%s_%s", sectors[[name]], names(flow))
      selected$code_long <- sub(sprintf(" %s", flow), "", selected$code_long, fixed = TRUE)
      selected$row_in_range <- selected$excel_row
      selected$excel_col <- "G"
      out[[paste(name, names(flow), sep = "_")]] <- selected
    }
  }
  if (!length(out)) {
    return(data.frame())
  }
  rows <- do.call(rbind, out)
  rows[, c("role", "code", "code_long", "value_raw", "value", "row_in_range", "excel_row", "excel_col")]
}

country_sna_extract_ury_year <- function(country, year, side, paths, rule) {
  existing <- paths[file.exists(paths)]
  if (!length(existing)) {
    return(list(values = data.frame(), inventory = country_sna_inventory_row(country, side, year, paste(paths, collapse = ";"), NA, "missing_file", rule)))
  }
  rows <- country_sna_ury_rows(paths)
  values <- country_sna_extract_values(rows, country, year, rule)
  values$source_side <- side
  values$source_file <- paste(existing, collapse = ";")
  values$sheet <- NA_character_
  values$cell_range <- "in_memory_aux_sna_ury"
  list(values = values, inventory = country_sna_inventory_row(country, side, year, paste(existing, collapse = ";"), NA, "matched", rule))
}

country_sna_extract_side <- function(country, side, rule, years, root) {
  resolved <- country_sna_resolve_files(rule[[side]], years, root)
  values <- list()
  inventory <- list()
  for (year in years) {
    item <- resolved$files[[as.character(year)]]
    extracted <- if (identical(rule[[side]]$type, "ury_sector")) {
      country_sna_extract_ury_year(country, year, side, item, rule)
    } else {
      country_sna_extract_workbook_year(country, year, side, item, rule)
    }
    values[[as.character(year)]] <- extracted$values
    inventory[[as.character(year)]] <- extracted$inventory
  }
  inventory <- do.call(rbind, inventory)
  if (length(resolved$warnings)) {
    inventory <- rbind(
      inventory,
      data.frame(
        country = country,
        side = side,
        year = NA_integer_,
        file = NA_character_,
        sheet = NA_character_,
        status = "warning",
        layout_family = rule$layout_family %||% NA_character_,
        warning = paste(unique(resolved$warnings), collapse = " | "),
        stringsAsFactors = FALSE
      )
    )
  }
  list(values = do.call(rbind, values), inventory = inventory, resolved = resolved)
}

country_sna_downstream_use <- function(variable) {
  # Tags make the summaries easier to triage: a change in an unused extracted
  # helper value is less urgent than one feeding snacompare or undistributed
  # profits.
  tags <- character()
  if (variable %in% c(
    "D1_cei", "D61_cei", "D62_cei", "B3g_cei", "D4_cei",
    "B2g_cei", "D5_cei", "B5g_cei", "NFC_B5g_cei",
    "FC_B5g_cei", "GG_B5g_cei", "TOT_B5g_cei"
  )) {
    tags <- c(tags, "snacompare")
  }
  if (variable %in% c(
    "NFC_B5g_cei", "FC_B5g_cei", "TOT_B5g_cei",
    "NFC_r_D4_cei", "FC_r_D4_cei", "GG_r_D4_cei",
    "TOT_r_D4_cei", "ROW_r_D4_cei"
  )) {
    tags <- c(tags, "uprofits")
  }
  if (variable %in% c(
    "D43_cei", "D44_cei", "D4_cei", "D75_cei", "D7_cei",
    "ratio_d43_d4", "ratio_d44_d4", "ratio_d43d44",
    "ratio_d44_b5g", "ratio_d75_d7", "ratio_d75_b5g"
  )) {
    tags <- c(tags, "cei_ratio_figures")
  }
  if (!length(tags)) {
    tags <- "current_extractor"
  }
  paste(unique(tags), collapse = ";")
}

country_sna_compare_values <- function(old_values, new_values) {
  # Values are linked by country-year-variable, not by physical cell address.
  # That lets the diagnostic tolerate sheet/file renames while still asking:
  # "would the current extractor produce the same CEI variable?"
  key <- c("country", "year", "variable")
  if (!nrow(old_values) && !nrow(new_values)) {
    return(data.frame())
  }
  old_keep <- old_values[, c(key, "value_type", "value", "value_raw", "extract_status", "source_file", "sheet", "cell_range", "row_in_range", "excel_row", "excel_col", "role", "matched_code", "code_long"), drop = FALSE]
  new_keep <- new_values[, c(key, "value_type", "value", "value_raw", "extract_status", "source_file", "sheet", "cell_range", "row_in_range", "excel_row", "excel_col", "role", "matched_code", "code_long"), drop = FALSE]
  merged <- merge(old_keep, new_keep, by = key, all = TRUE, suffixes = c("_old", "_new"))
  if (!nrow(merged)) {
    return(merged)
  }
  merged$value_type <- merged$value_type_old %||% merged$value_type_new
  both <- !is.na(merged$value_old) & !is.na(merged$value_new)
  merged$ratio_new_old <- ifelse(both & merged$value_old != 0, merged$value_new / merged$value_old, NA_real_)
  merged$pct_diff <- (merged$ratio_new_old - 1) * 100
  merged$status <- "not_compared"
  merged$status[is.na(merged$value_old) & !is.na(merged$value_new)] <- "missing_old"
  merged$status[!is.na(merged$value_old) & is.na(merged$value_new)] <- "missing_new"
  merged$status[both & merged$value_old == 0 & merged$value_new == 0] <- "unchanged_zero"
  merged$status[both & merged$value_old == 0 & merged$value_new != 0] <- "old_zero"
  merged$status[both & merged$value_old != 0 & abs(merged$value_new - merged$value_old) <= 1e-9] <- "unchanged"
  merged$status[both & merged$value_old != 0 & abs(merged$value_new - merged$value_old) > 1e-9] <- "changed"
  merged$abs_pct_diff <- abs(merged$pct_diff)
  merged$change_magnitude <- "not_applicable"
  merged$change_magnitude[merged$status == "unchanged"] <- "unchanged"
  merged$change_magnitude[merged$status == "unchanged_zero"] <- "unchanged_zero"
  merged$change_magnitude[merged$status == "old_zero"] <- "old_zero"
  merged$change_magnitude[merged$status == "changed" & merged$abs_pct_diff < 1] <- "anecdotal_lt_1pct"
  merged$change_magnitude[merged$status == "changed" & merged$abs_pct_diff >= 1] <- "substantive_ge_1pct"
  merged$downstream_use <- vapply(merged$variable, country_sna_downstream_use, character(1))
  merged[, c(
    "country", "year", "variable", "value_type", "downstream_use",
    "value_old", "value_new", "ratio_new_old", "pct_diff", "abs_pct_diff",
    "status", "change_magnitude",
    "extract_status_old", "extract_status_new", "source_file_old", "source_file_new",
    "sheet_old", "sheet_new", "cell_range_old", "cell_range_new",
    "excel_row_old", "excel_row_new", "excel_col_old", "excel_col_new",
    "role_old", "role_new", "matched_code_old", "matched_code_new",
    "code_long_old", "code_long_new"
  )]
}

country_sna_ratio_summary <- function(detail) {
  comparable <- detail[detail$status %in% c("changed", "unchanged"), , drop = FALSE]
  changed <- detail[detail$status == "changed" & is.finite(detail$pct_diff), , drop = FALSE]
  anecdotal <- changed[changed$change_magnitude == "anecdotal_lt_1pct", , drop = FALSE]
  substantive <- changed[changed$change_magnitude == "substantive_ge_1pct", , drop = FALSE]
  data.frame(
    values_compared = nrow(comparable),
    changed_values = nrow(changed),
    anecdotal_changes = nrow(anecdotal),
    substantive_changes = nrow(substantive),
    changed_share_pct = if (nrow(comparable)) round(nrow(changed) / nrow(comparable) * 100, 1) else NA_real_,
    substantive_share_pct = if (nrow(comparable)) round(nrow(substantive) / nrow(comparable) * 100, 1) else NA_real_,
    median_new_old = if (nrow(changed)) round(stats::median(changed$ratio_new_old, na.rm = TRUE), 6) else NA_real_,
    median_pct_diff = if (nrow(changed)) round(stats::median(changed$pct_diff, na.rm = TRUE), 3) else NA_real_,
    p10_pct_diff = if (nrow(changed)) round(unname(stats::quantile(changed$pct_diff, 0.10, na.rm = TRUE)), 3) else NA_real_,
    p90_pct_diff = if (nrow(changed)) round(unname(stats::quantile(changed$pct_diff, 0.90, na.rm = TRUE)), 3) else NA_real_,
    max_abs_pct_diff = if (nrow(changed)) round(max(abs(changed$pct_diff), na.rm = TRUE), 3) else NA_real_
  )
}

country_sna_extension_years <- function(inventory, active_years) {
  old_years <- unique(inventory$year[inventory$side == "old" & inventory$status == "matched"])
  new_years <- unique(inventory$year[inventory$side == "new" & inventory$status == "matched"])
  extension <- sort(setdiff(new_years, old_years))
  list(
    in_range = extension[extension %in% active_years],
    beyond_active = extension[extension > max(active_years)]
  )
}

country_sna_extension_info <- function(country, old_years, new_years, active_years) {
  old_years <- sort(unique(old_years[!is.na(old_years)]))
  new_years <- sort(unique(new_years[!is.na(new_years)]))
  if (identical(country, "MEX") && length(old_years)) {
    # The new MEX package contains many CSI files; under the current extractor,
    # only one year beyond the previous max is a meaningful "series extension".
    new_years <- new_years[new_years <= max(old_years) + 1L]
  }
  extension <- sort(setdiff(new_years, old_years))
  list(
    old_years = old_years,
    new_years = new_years,
    extension = extension,
    in_range = extension[extension %in% active_years],
    beyond_active = extension[extension > max(active_years)]
  )
}

country_sna_country_status <- function(country, detail, inventory, rule, active_years, extension_info) {
  if (!isTRUE(rule$compare)) {
    new_files <- inventory[inventory$side == "new" & inventory$status == "matched", , drop = FALSE]
    status <- rule$status %||% if (nrow(new_files)) "not_compared" else "no_new_files"
  } else if (!nrow(detail)) {
    status <- "no_overlap"
  } else if (any(detail$status == "changed")) {
    status <- "revisions_detected"
  } else if (length(extension_info$in_range) || length(extension_info$beyond_active)) {
    status <- "series_extension"
  } else {
    status <- "unchanged_overlap"
  }
  status
}

country_sna_country_summary <- function(country, detail, inventory, rule, active_years, extension_info) {
  overlap_years <- sort(unique(detail$year[detail$status %in% c("changed", "unchanged", "old_zero", "unchanged_zero")]))
  summary <- country_sna_ratio_summary(detail)
  warnings <- unique(na.omit(inventory$warning[nzchar(inventory$warning)]))
  data.frame(
    country = country,
    status = country_sna_country_status(country, detail, inventory, rule, active_years, extension_info),
    layout_family = rule$layout_family %||% NA_character_,
    old_years_detected = paste(extension_info$old_years, collapse = ","),
    new_years_detected = paste(extension_info$new_years, collapse = ","),
    extension_years_in_range = paste(extension_info$in_range, collapse = ","),
    extension_years_beyond_active = paste(extension_info$beyond_active, collapse = ","),
    overlap_years = paste(overlap_years, collapse = ","),
    values_compared = summary$values_compared,
    changed_values = summary$changed_values,
    anecdotal_changes = summary$anecdotal_changes,
    substantive_changes = summary$substantive_changes,
    changed_share_pct = summary$changed_share_pct,
    substantive_share_pct = summary$substantive_share_pct,
    median_new_old = summary$median_new_old,
    median_pct_diff = summary$median_pct_diff,
    p10_pct_diff = summary$p10_pct_diff,
    p90_pct_diff = summary$p90_pct_diff,
    max_abs_pct_diff = summary$max_abs_pct_diff,
    warnings = paste(warnings, collapse = " | "),
    notes = rule$notes %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

country_sna_group_summary <- function(detail, groups) {
  if (!nrow(detail)) {
    return(data.frame())
  }
  rows <- list()
  split_key <- interaction(detail[groups], drop = TRUE, lex.order = TRUE)
  for (level in levels(split_key)) {
    part <- detail[split_key == level, , drop = FALSE]
    base <- part[1L, groups, drop = FALSE]
    rows[[level]] <- cbind(base, country_sna_ratio_summary(part))
  }
  do.call(rbind, rows)
}

country_sna_empty_outputs <- function() {
  list(
    country_summary = data.frame(),
    year_summary = data.frame(),
    variable_summary = data.frame(),
    cell_detail = data.frame(),
    source_inventory = data.frame()
  )
}

country_sna_add_unmatched_notes <- function(country, rule, inventory, root, active_years) {
  notes <- data.frame()
  if (identical(country, "MEX")) {
    all_new <- country_sna_glob("input_data/_new/country_sna/MEX/**/CSI_*.xlsx", root)
    indexes <- as.integer(sub("^CSI_([0-9]+)\\.xlsx$", "\\1", basename(all_new)))
    ignored <- all_new[!is.na(indexes) & (2000L + indexes) > max(active_years) + 1L]
    if (length(ignored)) {
      notes <- rbind(notes, data.frame(
        country = country,
        side = "new",
        year = NA_integer_,
        file = NA_character_,
        sheet = NA_character_,
        status = "ignored_extra_csi_files",
        layout_family = rule$layout_family,
        warning = sprintf("%s new CSI files are outside the current CSI_(year - 2000) diagnostic window.", length(ignored)),
        stringsAsFactors = FALSE
      ))
    }
  }
  if (identical(country, "ECU")) {
    new_files <- country_sna_glob(rule$new$pattern, root)
    if (length(new_files)) {
      country_sna_need("readxl")
      notes <- rbind(notes, data.frame(
        country = country,
        side = "new",
        year = NA_integer_,
        file = paste(new_files, collapse = ";"),
        sheet = paste(readxl::excel_sheets(new_files[[1]]), collapse = ";"),
        status = "layout_break",
        layout_family = rule$layout_family,
        warning = rule$notes,
        stringsAsFactors = FALSE
      ))
    }
  }
  rbind(inventory, notes)
}

country_sna_sheet_kind <- function(sheet) {
  # Coarse layout classification is enough for diagnostics: year-sheet sources
  # can usually reuse the current extractor, while account-sheet sources need a
  # new adapter.
  sheet_clean <- trimws(sheet)
  if (grepl("^(CEI_)?(19|20)[0-9]{2}P?$", toupper(sheet_clean))) {
    return("year_sheet")
  }
  if (toupper(sheet_clean) %in% c(
    "PRODUCCION", "PRODUCCI\u00d3N", "CONSUMO INTERMEDIO", "VAB", "EBE",
    "YMB", "YNB", "YDB", "AHORRO", "FBK", "PRE-END"
  )) {
    return("account_sheet")
  }
  "other"
}

country_sna_sample_dimensions <- function(path, max_sheets = 8L) {
  if (is.na(path) || !file.exists(path)) {
    return(NA_character_)
  }
  country_sna_need("readxl")
  sheets <- utils::head(readxl::excel_sheets(path), max_sheets)
  dims <- vapply(sheets, function(sheet) {
    raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal"))
    sprintf("%s:%sx%s", sheet, nrow(raw), ncol(raw))
  }, character(1))
  paste(dims, collapse = "; ")
}

country_sna_layout_file <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(list(
      file = path %||% NA_character_,
      sheets = character(),
      years = integer(),
      year_sheet_count = 0L,
      account_sheet_count = 0L,
      sample_dimensions = NA_character_
    ))
  }
  country_sna_need("readxl")
  sheets <- readxl::excel_sheets(path)
  kinds <- vapply(sheets, country_sna_sheet_kind, character(1))
  list(
    file = path,
    sheets = sheets,
    years = country_sna_sheet_years(path),
    year_sheet_count = sum(kinds == "year_sheet"),
    account_sheet_count = sum(kinds == "account_sheet"),
    sample_dimensions = country_sna_sample_dimensions(path)
  )
}

country_sna_layout_selected_file <- function(spec, root, active_years) {
  if (identical(spec$type, "single_stem") || identical(spec$type, "single_pattern")) {
    return(country_sna_resolve_single_file(spec, root)$file)
  }
  if (identical(spec$type, "year_pattern")) {
    files <- country_sna_year_pattern_files(spec, active_years, root)$files
    return(country_sna_choose_file(stats::na.omit(files)))
  }
  if (identical(spec$type, "mex_year")) {
    files <- country_sna_mex_files(spec, active_years, root)$files
    return(country_sna_choose_file(stats::na.omit(files)))
  }
  if (identical(spec$type, "ury_sector")) {
    years <- country_sna_discover_extension_years(spec, root)
    if (!length(years)) {
      return(NA_character_)
    }
    files <- country_sna_ury_files(spec, max(years), root)$files[[1]]
    return(country_sna_choose_file(files[file.exists(files)]))
  }
  NA_character_
}

country_sna_layout_summary_country <- function(country, rule, root, active_years, extension_info) {
  # Layout comparison answers "is this just a renamed sheet, or did the source
  # structure change?" Ecuador currently lands here as a true structural break.
  old_file <- country_sna_layout_selected_file(rule$old, root, active_years)
  new_file <- country_sna_layout_selected_file(rule$new, root, active_years)
  old_layout <- country_sna_layout_file(old_file)
  new_layout <- country_sna_layout_file(new_file)
  layout_change <- "not_assessed"
  if (old_layout$year_sheet_count > 0L && new_layout$account_sheet_count > 0L && new_layout$year_sheet_count == 0L) {
    layout_change <- "year_sheets_to_account_sheets"
  } else if (!is.na(old_file) && !is.na(new_file) && old_layout$year_sheet_count > 0L && new_layout$year_sheet_count > 0L) {
    layout_change <- "same_layout_family"
  } else if (identical(rule$layout_family, "sector_year_files")) {
    layout_change <- "sector_year_files"
  } else if (is.na(new_file) || !file.exists(new_file)) {
    layout_change <- "no_new_file"
  }
  data.frame(
    country = country,
    layout_change = layout_change,
    old_file = old_file %||% NA_character_,
    new_file = new_file %||% NA_character_,
    old_sheet_count = length(old_layout$sheets),
    new_sheet_count = length(new_layout$sheets),
    old_year_sheet_count = old_layout$year_sheet_count,
    new_year_sheet_count = new_layout$year_sheet_count,
    old_account_sheet_count = old_layout$account_sheet_count,
    new_account_sheet_count = new_layout$account_sheet_count,
    old_years_detected = paste(extension_info$old_years, collapse = ","),
    new_years_detected = paste(extension_info$new_years, collapse = ","),
    old_sheets = paste(old_layout$sheets, collapse = ";"),
    new_sheets = paste(new_layout$sheets, collapse = ";"),
    old_sample_dimensions = old_layout$sample_dimensions,
    new_sample_dimensions = new_layout$sample_dimensions,
    interpretation = if (identical(layout_change, "year_sheets_to_account_sheets")) {
      "Real structural change: old source is organized by year sheets; new source is organized by account sheets."
    } else if (identical(layout_change, "same_layout_family")) {
      "Workbook layout family appears comparable; differences are not only filename suffixes."
    } else if (identical(layout_change, "no_new_file")) {
      "No new workbook available for layout comparison."
    } else {
      rule$notes %||% NA_character_
    },
    stringsAsFactors = FALSE
  )
}

country_sna_noncompare_inventory <- function(country, rule, root) {
  rows <- list()
  for (side in c("old", "new")) {
    spec <- rule[[side]]
    files <- if (identical(spec$type, "single_stem")) {
      country_sna_single_stem_candidates(spec$stem, root)
    } else if (identical(spec$type, "single_pattern")) {
      country_sna_glob(spec$pattern, root)
    } else {
      character()
    }
    files <- files[file.exists(files)]
    rows[[side]] <- data.frame(
      country = country,
      side = side,
      year = NA_integer_,
      file = if (length(files)) paste(files, collapse = ";") else NA_character_,
      sheet = NA_character_,
      status = if (length(files)) "matched" else "missing_file",
      layout_family = rule$layout_family %||% NA_character_,
      warning = if (length(files)) NA_character_ else sprintf("No %s files found.", side),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

country_sna_diagnose_country <- function(country, rule, active_years, root) {
  # The active years define the overlap comparison. Extension detection is
  # broader and probes one year beyond the active range plus all discoverable
  # workbook/sheet years.
  compare_years <- active_years
  extension_probe_years <- seq(min(active_years), max(active_years) + 1L)
  old_discovered_years <- country_sna_discover_extension_years(rule$old, root)
  new_discovered_years <- country_sna_discover_extension_years(rule$new, root)
  extension_info <- country_sna_extension_info(country, old_discovered_years, new_discovered_years, active_years)
  if (!isTRUE(rule$compare)) {
    inventory <- country_sna_noncompare_inventory(country, rule, root)
    inventory <- country_sna_add_unmatched_notes(country, rule, inventory, root, active_years)
    detail <- data.frame()
    return(list(
      country_summary = country_sna_country_summary(country, detail, inventory, rule, active_years, extension_info),
      layout_summary = country_sna_layout_summary_country(country, rule, root, active_years, extension_info),
      cell_detail = detail,
      source_inventory = inventory
    ))
  }
  old <- country_sna_extract_side(country, "old", rule, extension_probe_years, root)
  new <- country_sna_extract_side(country, "new", rule, extension_probe_years, root)
  inventory <- rbind(old$inventory, new$inventory)
  inventory <- country_sna_add_unmatched_notes(country, rule, inventory, root, active_years)

  old_values <- old$values[old$values$year %in% compare_years, , drop = FALSE]
  new_values <- new$values[new$values$year %in% compare_years, , drop = FALSE]
  detail <- if (isTRUE(rule$compare)) country_sna_compare_values(old_values, new_values) else data.frame()
  list(
    country_summary = country_sna_country_summary(country, detail, inventory, rule, active_years, extension_info),
    layout_summary = country_sna_layout_summary_country(country, rule, root, active_years, extension_info),
    cell_detail = detail,
    source_inventory = inventory
  )
}

country_sna_write_outputs <- function(outputs, output_dir) {
  country_sna_need("openxlsx")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  workbook_path <- file.path(output_dir, "country_sna_current_extractor_diagnostic.xlsx")
  wb <- openxlsx::createWorkbook()
  for (name in names(outputs)) {
    sheet <- substr(name, 1L, 31L)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, outputs[[name]])
    utils::write.csv(outputs[[name]], file.path(output_dir, paste0(name, ".csv")), row.names = FALSE, na = "")
  }
  openxlsx::saveWorkbook(wb, workbook_path, overwrite = TRUE)
  countries <- sort(unique(outputs$country_summary$country))
  country_dir <- file.path(output_dir, "countries")
  dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)
  for (country in countries) {
    country_outputs <- lapply(outputs, function(df) {
      if (!is.data.frame(df) || !("country" %in% names(df))) {
        return(df)
      }
      df[df$country == country, , drop = FALSE]
    })
    country_wb <- openxlsx::createWorkbook()
    for (name in names(country_outputs)) {
      sheet <- substr(name, 1L, 31L)
      openxlsx::addWorksheet(country_wb, sheet)
      openxlsx::writeData(country_wb, sheet, country_outputs[[name]])
    }
    openxlsx::saveWorkbook(
      country_wb,
      file.path(country_dir, sprintf("%s_country_sna_current_extractor_diagnostic.xlsx", country)),
      overwrite = TRUE
    )
  }
  workbook_path
}

run_country_sna_current_extractor_diagnostic <- function(
  root = country_sna_repo_root(),
  output_dir = file.path(root, "output", "source_diagnostics", "country_sna"),
  write_outputs = TRUE
) {
  config <- country_sna_read_config(root)
  first_year <- as.integer(config$years$first)
  last_year <- as.integer(config$years$last)
  active_years <- seq(first_year, last_year)
  rules <- country_sna_rules()

  per_country <- lapply(names(rules), function(country) {
    country_sna_diagnose_country(country, rules[[country]], active_years, root)
  })
  names(per_country) <- names(rules)

  country_summary <- do.call(rbind, lapply(per_country, `[[`, "country_summary"))
  layout_summary <- do.call(rbind, lapply(per_country, `[[`, "layout_summary"))
  cell_detail <- do.call(rbind, lapply(per_country, `[[`, "cell_detail"))
  source_inventory <- do.call(rbind, lapply(per_country, `[[`, "source_inventory"))
  year_summary <- country_sna_group_summary(cell_detail, c("country", "year"))
  variable_summary <- country_sna_group_summary(cell_detail, c("country", "variable", "value_type", "downstream_use"))

  outputs <- list(
    country_summary = country_summary,
    layout_summary = layout_summary,
    year_summary = year_summary,
    variable_summary = variable_summary,
    cell_detail = cell_detail,
    source_inventory = source_inventory
  )
  workbook_path <- if (write_outputs) country_sna_write_outputs(outputs, output_dir) else NA_character_
  list(outputs = outputs, workbook_path = workbook_path)
}
