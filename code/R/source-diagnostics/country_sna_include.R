# Country-SNA include workflow
#
# This is the deterministic side of the experimental country-SNA source
# workflow. Runtime behavior should be described by config/country_sna_include.yml:
# country files, sheet rules, cell ranges, role columns, code aliases, units,
# and compatibility exclusions. The R code below is intentionally an interpreter
# of that contract, not a new place to hide country-specific workbook facts.
#
# Dry-runs are read-only with respect to input_data and intermediary_data.
# `--apply` is guarded by a clean dry-run manifest before any source promotion is
# allowed. Generated reports are written under output/experiments/country_sna_include
# unless a caller passes an explicit output_dir for tests.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

country_sna_include_repo_root <- function(start = getwd()) {
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

country_sna_include_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

country_sna_include_has <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

country_sna_include_path <- function(path, root) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  if (grepl("^/", path)) path else file.path(root, path)
}

country_sna_include_read_yaml <- function(path) {
  country_sna_include_need("yaml")
  if (!file.exists(path)) {
    stop("Missing country-SNA extractor contract: ", path, call. = FALSE)
  }
  yaml::read_yaml(path)
}

country_sna_include_read_contract <- function(
  root = country_sna_include_repo_root(),
  contract_path = file.path(root, "config", "country_sna_include.yml")
) {
  contract <- country_sna_include_read_yaml(contract_path)
  contract$contract_path <- contract_path
  country_sna_include_validate_contract(contract)
  contract
}

country_sna_include_validate_contract <- function(contract) {
  required <- c("output_root", "years", "countries", "variables", "country_rules")
  missing <- setdiff(required, names(contract))
  if (length(missing)) {
    stop("Country-SNA extractor contract is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (is.null(contract$variables$primitives) || !length(contract$variables$primitives)) {
    stop("Country-SNA extractor contract has no primitive variables.", call. = FALSE)
  }
  absent_rules <- setdiff(contract$countries, names(contract$country_rules))
  if (length(absent_rules)) {
    stop("Country-SNA extractor contract has countries without rules: ", paste(absent_rules, collapse = ", "), call. = FALSE)
  }
  invisible(contract)
}

country_sna_include_read_dina_years <- function(root, config_path) {
  config_path <- country_sna_include_path(config_path, root)
  config <- country_sna_include_read_yaml(config_path)
  first <- as.integer(config$years$first)
  last <- as.integer(config$years$last)
  if (is.na(first) || is.na(last) || first > last) {
    stop("Invalid year range in ", config_path, call. = FALSE)
  }
  seq(first, last)
}

country_sna_include_years <- function(contract, root) {
  years <- contract$years
  if (isTRUE(years$from_config) || !is.null(years$from_config)) {
    return(country_sna_include_read_dina_years(root, years$from_config %||% "config/dina.yml"))
  }
  first <- as.integer(years$first)
  last <- as.integer(years$last)
  if (is.na(first) || is.na(last) || first > last) {
    stop("Invalid country-SNA extractor year range.", call. = FALSE)
  }
  seq(first, last)
}

country_sna_include_output_root <- function(root, contract, output_dir = NULL) {
  out <- output_dir %||% contract$output_root
  country_sna_include_path(out, root)
}

country_sna_include_output_paths <- function(root, contract, output_dir = NULL) {
  out <- country_sna_include_output_root(root, contract, output_dir)
  list(
    root = out,
    data = file.path(out, "data"),
    tables = file.path(out, "tables"),
    figures = file.path(out, "figures"),
    workbooks = file.path(out, "workbooks"),
    logs = file.path(out, "logs"),
    snapshots = file.path(out, "snapshots")
  )
}

country_sna_include_excel_col_to_index <- function(col) {
  col <- toupper(as.character(col))
  vapply(strsplit(col, ""), function(chars) {
    chars <- chars[nzchar(chars)]
    sum(match(chars, LETTERS) * 26^rev(seq_along(chars) - 1L))
  }, numeric(1))
}

country_sna_include_parse_cell_range <- function(cell_range) {
  parts <- regmatches(cell_range, regexec("^([A-Z]+)([0-9]+):([A-Z]+)([0-9]+)$", cell_range))[[1]]
  if (length(parts) != 5L) {
    stop("Invalid Excel cell range in country-SNA contract: ", cell_range, call. = FALSE)
  }
  list(
    first_col = parts[[2]],
    first_row = as.integer(parts[[3]]),
    last_col = parts[[4]],
    last_row = as.integer(parts[[5]])
  )
}

country_sna_include_range_col_position <- function(col, cell_range) {
  parsed <- country_sna_include_parse_cell_range(cell_range)
  country_sna_include_excel_col_to_index(col) -
    country_sna_include_excel_col_to_index(parsed$first_col) + 1L
}

country_sna_include_normalize_code <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- trimws(x)
  x <- gsub("\\s+", "", x)
  x <- toupper(x)
  x <- gsub("^D([0-9])", "D.\\1", x)
  x <- gsub("^B([0-9])", "B.\\1", x)
  x
}

country_sna_include_normalize_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

country_sna_include_num <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  x <- gsub("\u00a0", " ", as.character(x), fixed = TRUE)
  x <- gsub(",", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

country_sna_include_bind <- function(...) {
  dfs <- list(...)
  if (length(dfs) == 1L && is.list(dfs[[1L]]) && !is.data.frame(dfs[[1L]])) {
    dfs <- dfs[[1L]]
  }
  dfs <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, dfs)
  if (!length(dfs)) {
    return(data.frame())
  }
  names_all <- unique(unlist(lapply(dfs, names), use.names = FALSE))
  dfs <- lapply(dfs, function(df) {
    missing <- setdiff(names_all, names(df))
    for (name in missing) {
      df[[name]] <- NA
    }
    df[, names_all, drop = FALSE]
  })
  do.call(rbind, dfs)
}

country_sna_include_file_score <- function(path) {
  years <- country_sna_include_parse_years(path)
  c(max_year = max(c(years, -Inf)), mtime = as.numeric(file.info(path)$mtime %||% 0))
}

country_sna_include_choose_file <- function(paths) {
  paths <- unique(paths[file.exists(paths)])
  if (!length(paths)) {
    return(NA_character_)
  }
  scores <- t(vapply(paths, country_sna_include_file_score, numeric(2)))
  paths[order(scores[, "max_year"], scores[, "mtime"], paths, decreasing = TRUE)][[1]]
}

country_sna_include_parse_years <- function(x) {
  matches <- gregexpr("(19|20)[0-9]{2}", basename(x), perl = TRUE)
  years <- unique(as.integer(unlist(regmatches(basename(x), matches))))
  years[!is.na(years)]
}

country_sna_include_glob <- function(pattern, root) {
  pattern <- country_sna_include_path(pattern, root)
  sort(Sys.glob(pattern))
}

country_sna_include_single_stem_candidates <- function(stem, root) {
  stem <- country_sna_include_path(stem, root)
  unique(c(stem, paste0(stem, ".xlsx"), paste0(stem, ".xls")))
}

country_sna_include_resolve_single <- function(spec, root) {
  candidates <- if (identical(spec$type, "single_stem")) {
    country_sna_include_single_stem_candidates(spec$stem, root)
  } else {
    country_sna_include_glob(spec$pattern, root)
  }
  candidates <- unique(candidates[file.exists(candidates)])
  original_candidates <- candidates
  extension_priority <- as.character(spec$extension_priority %||% character())
  if (length(extension_priority) && length(candidates)) {
    for (extension in extension_priority) {
      extension <- tolower(gsub("^\\.", "", extension))
      preferred <- candidates[tolower(tools::file_ext(candidates)) == extension]
      if (length(preferred)) {
        candidates <- preferred
        break
      }
    }
  }
  selected <- country_sna_include_choose_file(candidates)
  status <- if (is.na(selected)) "missing_file" else "matched"
  warning <- NA_character_
  if (length(original_candidates) > 1L) {
    warning <- sprintf(
      "Multiple candidate workbooks found; selected %s from %s.",
      basename(selected),
      paste(basename(original_candidates), collapse = ", ")
    )
  }
  list(file = selected, candidates = original_candidates, status = status, warning = warning, selector = spec$type)
}

country_sna_include_resolve_year_pattern <- function(spec, year, root) {
  pattern <- gsub("\\{year\\}", as.character(year), spec$pattern)
  candidates <- country_sna_include_glob(pattern, root)
  candidates <- unique(candidates[file.exists(candidates)])
  selected <- country_sna_include_choose_file(candidates)
  status <- if (is.na(selected)) "missing_file" else "matched"
  warning <- NA_character_
  if (length(candidates) > 1L) {
    warning <- sprintf(
      "Year %s has multiple candidate workbooks; selected %s from %s.",
      year,
      basename(selected),
      paste(basename(candidates), collapse = ", ")
    )
  }
  list(file = selected, candidates = candidates, status = status, warning = warning, selector = spec$type)
}

country_sna_include_parse_mex_index <- function(index_file) {
  if (!file.exists(index_file)) {
    return(data.frame())
  }
  lines <- readLines(index_file, warn = FALSE, encoding = "UTF-8")
  lines <- gsub("^\ufeff", "", lines)
  lines <- lines[grepl("|", lines, fixed = TRUE)]
  rows <- lapply(lines, function(line) {
    parts <- strsplit(line, "|", fixed = TRUE)[[1]]
    if (length(parts) < 4L) {
      return(NULL)
    }
    year <- suppressWarnings(as.integer(parts[[length(parts)]]))
    data.frame(
      file = parts[[1]],
      content = paste(parts[-c(1L, length(parts))], collapse = "|"),
      year = year,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

country_sna_include_resolve_indexed_year_file <- function(spec, year, root) {
  folder <- country_sna_include_path(spec$folder, root)
  index_file <- country_sna_include_path(spec$index_file, root)
  index <- country_sna_include_parse_mex_index(index_file)
  title <- country_sna_include_normalize_text(spec$title_contains %||% "")
  unit <- country_sna_include_normalize_text(spec$unit_contains %||% "")
  matched <- data.frame()
  # The include contract follows the same Mexico rule as the explorer: the
  # source index is authoritative, while the old CSI_(year - 2000) convention is
  # only a fallback. This keeps legacy parity possible without hiding a future
  # numbering change inside hardcoded arithmetic.
  if (nrow(index)) {
    content <- country_sna_include_normalize_text(index$content)
    matched <- index[
      index$year == year &
        grepl(title, content, fixed = TRUE) &
        grepl(unit, content, fixed = TRUE),
      ,
      drop = FALSE
    ]
  }

  candidates <- if (nrow(matched)) file.path(folder, matched$file) else character()
  candidates <- candidates[file.exists(candidates)]
  selected <- country_sna_include_choose_file(candidates)
  warning <- NA_character_
  selector <- "indexed_year_file"
  if (is.na(selected) && !is.null(spec$fallback_pattern)) {
    offset <- as.integer(spec$fallback_index_offset %||% 0L)
    pattern <- gsub("\\{index\\}", as.character(year - offset), spec$fallback_pattern)
    fallback <- country_sna_include_glob(pattern, root)
    fallback <- fallback[file.exists(fallback)]
    selected <- country_sna_include_choose_file(fallback)
    candidates <- fallback
    selector <- "indexed_year_file_fallback"
    if (!is.na(selected)) {
      warning <- sprintf("Index did not resolve year %s; used fallback %s.", year, basename(selected))
    }
  }
  status <- if (is.na(selected)) "missing_file" else "matched"
  if (length(candidates) > 1L) {
    warning <- paste(
      na.omit(c(warning, sprintf("Year %s has multiple indexed candidates; selected %s.", year, basename(selected)))),
      collapse = " | "
    )
  }
  list(file = selected, candidates = candidates, status = status, warning = warning, selector = selector)
}

country_sna_include_resolve_source <- function(spec, year, root) {
  # Deterministic extraction still uses explicit source selectors from YAML,
  # but the selectors describe matching rules rather than one release filename.
  # That gives the contract room for future official file renames while keeping
  # every arbitrary choice reviewable in one place.
  if (identical(spec$type, "single_stem") || identical(spec$type, "single_pattern")) {
    return(country_sna_include_resolve_single(spec, root))
  }
  if (identical(spec$type, "year_pattern")) {
    return(country_sna_include_resolve_year_pattern(spec, year, root))
  }
  if (identical(spec$type, "indexed_year_file")) {
    return(country_sna_include_resolve_indexed_year_file(spec, year, root))
  }
  if (identical(spec$type, "sector_file_bundle")) {
    return(country_sna_include_resolve_sector_bundle(spec, year, root))
  }
  stop("Unknown country-SNA source resolver: ", spec$type, call. = FALSE)
}

country_sna_include_resolve_sector_bundle <- function(spec, year, root) {
  folder <- country_sna_include_path(spec$folder, root)
  sectors <- spec$sectors %||% list()
  paths <- vapply(names(sectors), function(name) {
    template <- spec$filename_template
    template <- gsub("\\{year\\}", as.character(year), template)
    template <- gsub("\\{sector_code\\}", as.character(sectors[[name]]), template)
    file.path(folder, template)
  }, character(1))
  existing <- paths[file.exists(paths)]
  status <- if (length(existing)) "matched" else "missing_file"
  warning <- if (length(existing) && length(existing) < length(paths)) {
    sprintf("Sector bundle for %s is incomplete: %s missing.", year, paste(names(paths)[!file.exists(paths)], collapse = ", "))
  } else {
    NA_character_
  }
  list(file = paste(existing, collapse = ";"), paths = paths, candidates = existing, status = status, warning = warning, selector = spec$type)
}

country_sna_include_year_matches <- function(condition, year) {
  condition <- condition %||% list()
  min_year <- if (is.null(condition$min)) -Inf else as.integer(condition$min)
  max_year <- if (is.null(condition$max)) Inf else as.integer(condition$max)
  exact <- condition$only
  if (!is.null(exact) && !(year %in% as.integer(exact))) {
    return(FALSE)
  }
  year >= min_year && year <= max_year
}

country_sna_include_layout_for_year <- function(country_rule, year) {
  layouts <- country_rule$layouts %||% list()
  if (!length(layouts)) {
    return(list(id = "default", years = list(min = -Inf, max = Inf), range = NA_character_))
  }
  matches <- which(vapply(layouts, function(layout) {
    country_sna_include_year_matches(layout$years, year)
  }, logical(1)))
  if (!length(matches)) {
    stop("No country-SNA layout matches year ", year, call. = FALSE)
  }
  layout <- layouts[[matches[[length(matches)]]]]
  layout$id <- layout$id %||% sprintf("layout_%s", matches[[length(matches)]])
  layout
}

country_sna_include_sheet_candidates <- function(sheet_spec, year) {
  type <- sheet_spec$type %||% "year_variants"
  if (identical(type, "fixed")) {
    return(as.character(sheet_spec$value))
  }
  if (identical(type, "template")) {
    return(gsub("\\{year\\}", as.character(year), as.character(sheet_spec$value)))
  }
  c(
    as.character(year),
    sprintf("%sp", year),
    sprintf("CEI_%s", year),
    sprintf("CEI_%sp", year),
    sprintf("CEI%s", year),
    sprintf("CEI%sP", year)
  )
}

country_sna_include_resolve_sheet <- function(path, year, sheet_spec) {
  if (is.na(path) || !file.exists(path)) {
    return(list(sheet = NA_character_, status = "missing_file", warning = NA_character_, candidates = character()))
  }
  country_sna_include_need("readxl")
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  candidates <- country_sna_include_sheet_candidates(sheet_spec, year)
  hit <- sheets[tolower(trimws(sheets)) %in% tolower(trimws(candidates))]
  if (length(hit)) {
    return(list(sheet = hit[[1]], status = "matched", warning = NA_character_, candidates = candidates))
  }
  list(
    sheet = NA_character_,
    status = "missing_sheet",
    warning = sprintf("No matching sheet for year %s in %s.", year, basename(path)),
    candidates = candidates
  )
}

country_sna_include_read_workbook_table <- function(path, sheet, cell_range, columns) {
  country_sna_include_need("readxl")
  parsed <- country_sna_include_parse_cell_range(cell_range)
  raw <- suppressMessages(readxl::read_excel(
    path,
    sheet = sheet,
    range = cell_range,
    col_names = FALSE,
    .name_repair = "minimal"
  ))
  if (!nrow(raw)) {
    return(list(rows = data.frame(), health = "empty_range"))
  }

  positions <- vapply(columns, country_sna_include_range_col_position, numeric(1), cell_range = cell_range)
  available <- positions <= ncol(raw) & positions >= 1L
  missing_roles <- names(positions)[!available]
  positions <- positions[available]
  columns <- columns[available]
  if (!("code" %in% names(positions))) {
    return(list(rows = data.frame(), health = "missing_code_column"))
  }

  raw_code <- as.character(raw[[positions[["code"]]]])
  if ("code_long" %in% names(positions) && positions[["code"]] == positions[["code_long"]]) {
    # Some official files put "CODE - label" in a single cell. Splitting only
    # at the first dash keeps this as a structural parsing rule, not a
    # country-specific hardcoded value.
    raw_code <- sub("-.*$", "", raw_code)
  }
  code <- country_sna_include_normalize_code(raw_code)
  code_long <- if ("code_long" %in% names(positions)) as.character(raw[[positions[["code_long"]]]]) else NA_character_

  rows <- list()
  for (role in setdiff(names(positions), c("code", "code_long"))) {
    rows[[role]] <- data.frame(
      role = role,
      code = code,
      code_long = code_long,
      value_raw = as.character(raw[[positions[[role]]]]),
      value = country_sna_include_num(raw[[positions[[role]]]]),
      row_in_range = seq_len(nrow(raw)),
      excel_row = parsed$first_row + seq_len(nrow(raw)) - 1L,
      excel_col = unname(columns[[role]]),
      stringsAsFactors = FALSE
    )
  }
  health <- if (length(missing_roles)) {
    paste0("missing_role_columns:", paste(missing_roles, collapse = ","))
  } else {
    "read_ok"
  }
  list(rows = country_sna_include_bind(rows), health = health)
}

country_sna_include_excel_index_to_col <- function(index) {
  index <- as.integer(index)
  if (is.na(index) || index < 1L) {
    return(NA_character_)
  }
  out <- character()
  while (index > 0L) {
    index <- index - 1L
    out <- c(LETTERS[(index %% 26L) + 1L], out)
    index <- index %/% 26L
  }
  paste(out, collapse = "")
}

country_sna_include_year_column <- function(year_columns, year) {
  year <- as.integer(year)
  if (!is.null(year_columns$columns)) {
    col <- year_columns$columns[[as.character(year)]]
    return(if (is.null(col)) NA_character_ else as.character(col))
  }
  first_year <- as.integer(year_columns$first_year)
  last_year <- as.integer(year_columns$last_year %||% Inf)
  first_col <- as.character(year_columns$first_col %||% NA_character_)
  if (is.na(first_year) || is.na(first_col) || year < first_year || year > last_year) {
    return(NA_character_)
  }
  first_index <- country_sna_include_excel_col_to_index(first_col)
  country_sna_include_excel_index_to_col(first_index + year - first_year)
}

country_sna_include_apply_row_edits <- function(rows, row_edits) {
  if (!nrow(rows)) {
    return(rows)
  }
  drop_rows <- as.integer(row_edits$drop_rows %||% integer())
  if (length(drop_rows)) {
    rows <- rows[!(rows$row_in_range %in% drop_rows), , drop = FALSE]
  }
  drop_excel_rows <- as.integer(row_edits$drop_excel_rows %||% integer())
  if (length(drop_excel_rows)) {
    rows <- rows[!(rows$excel_row %in% drop_excel_rows), , drop = FALSE]
  }
  recodes <- row_edits$recode_rows %||% list()
  for (rule in recodes) {
    hit <- rep(TRUE, nrow(rows))
    if (!is.null(rule$row_in_range)) {
      hit <- hit & rows$row_in_range == as.integer(rule$row_in_range)
    }
    if (!is.null(rule$excel_row)) {
      hit <- hit & rows$excel_row == as.integer(rule$excel_row)
    }
    if (!is.null(rule$code)) {
      rows$code[hit] <- country_sna_include_normalize_code(rule$code)
    }
  }
  rows
}

country_sna_include_read_year_column_table <- function(spec, year, root) {
  source <- country_sna_include_resolve_source(spec$source, year, root)
  source$sheet <- NA_character_
  layout <- list(
    id = spec$id %||% NA_character_,
    range = spec$range %||% NA_character_,
    sheet = spec$sheet %||% list()
  )
  if (!identical(source$status, "matched")) {
    return(list(rows = data.frame(), source = source, layout = layout, status = source$status, health = NA_character_))
  }
  sheet <- country_sna_include_resolve_sheet(source$file, year, spec$sheet)
  source$sheet <- sheet$sheet
  if (!identical(sheet$status, "matched")) {
    source$warning <- paste(na.omit(c(source$warning, sheet$warning)), collapse = " | ")
    return(list(rows = data.frame(), source = source, layout = layout, status = sheet$status, health = NA_character_))
  }
  value_col <- country_sna_include_year_column(spec$year_columns, year)
  if (is.na(value_col)) {
    return(list(rows = data.frame(), source = source, layout = layout, status = "missing_year_column", health = NA_character_))
  }
  role <- as.character(spec$role %||% "households_r")
  columns <- c(spec$columns[c("code", "code_long")], stats::setNames(list(value_col), role))
  table <- country_sna_include_read_workbook_table(source$file, sheet$sheet, spec$range, columns)
  rows <- country_sna_include_apply_row_edits(table$rows, spec$row_edits %||% list())
  list(rows = rows, source = source, layout = layout, status = table$health, health = table$health, role = role)
}

country_sna_include_read_sector_file <- function(path, columns) {
  if (is.na(path) || !file.exists(path)) {
    return(data.frame())
  }
  country_sna_include_need("openxlsx")
  # Uruguay sector files have intentionally blank leading columns. Stata keeps
  # those columns when referring to E/F/G; openxlsx can preserve that grid.
  raw <- suppressWarnings(openxlsx::read.xlsx(
    path,
    colNames = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE,
    detectDates = FALSE
  ))
  if (!nrow(raw)) {
    return(data.frame())
  }
  max_col <- ncol(raw)
  code_pos <- country_sna_include_excel_col_to_index(columns$code)
  label_pos <- country_sna_include_excel_col_to_index(columns$code_long)
  value_pos <- country_sna_include_excel_col_to_index(columns$value)
  if (max(c(code_pos, label_pos, value_pos)) > max_col) {
    return(data.frame())
  }
  data.frame(
    code = country_sna_include_normalize_code(raw[[code_pos]]),
    code_long = as.character(raw[[label_pos]]),
    value_raw = as.character(raw[[value_pos]]),
    value = country_sna_include_num(raw[[value_pos]]),
    row_in_range = seq_len(nrow(raw)),
    excel_row = seq_len(nrow(raw)),
    excel_col = columns$value,
    stringsAsFactors = FALSE
  )
}

country_sna_include_read_sector_bundle <- function(source_spec, source) {
  out <- list()
  directions <- source_spec$directions %||% list(r = "por cobrar", u = "por pagar")
  sectors <- source_spec$sectors %||% list()
  for (sector in names(sectors)) {
    rows <- country_sna_include_read_sector_file(source$paths[[sector]], source_spec$columns)
    if (!nrow(rows)) {
      next
    }
    for (direction in names(directions)) {
      phrase <- directions[[direction]]
      keep <- grepl("B", rows$code, fixed = TRUE) | grepl(phrase, rows$code_long, fixed = TRUE)
      selected <- rows[keep, , drop = FALSE]
      if (!nrow(selected)) {
        next
      }
      other_phrases <- unlist(directions[setdiff(names(directions), direction)], use.names = FALSE)
      for (other in other_phrases) {
        selected <- selected[!grepl(other, selected$code_long, fixed = TRUE), , drop = FALSE]
      }
      selected$role <- sprintf("%s_%s", sector, direction)
      selected$code_long <- sub(sprintf(" %s", phrase), "", selected$code_long, fixed = TRUE)
      out[[paste(sector, direction, sep = "_")]] <- selected
    }
  }
  country_sna_include_bind(out)
}

country_sna_include_alias_groups <- function(contract, country_rule, account) {
  overrides <- country_rule$code_alias_overrides %||% list()
  aliases <- overrides[[account]] %||% contract$code_aliases[[account]] %||% account
  if (is.character(aliases)) {
    return(list(aliases))
  }
  if (is.list(aliases) && all(vapply(aliases, is.character, logical(1)))) {
    return(aliases)
  }
  list(as.character(unlist(aliases, use.names = FALSE)))
}

country_sna_include_flow_direction <- function(role) {
  if (is.na(role)) {
    return(NA_character_)
  }
  if (grepl("_r$", role)) {
    return("resource")
  }
  if (grepl("_u$", role)) {
    return("use")
  }
  NA_character_
}

country_sna_include_contract_status <- function(extract_status) {
  switch(
    extract_status,
    matched = "contract_ok",
    duplicate_identical = "contract_ok",
    computed = "contract_ok",
    excluded_by_contract = "contract_exclusion",
    zero_treated_missing = "zero_treated_missing",
    no_code_match = "expected_code_missing",
    no_role_match = "expected_role_missing",
    source_missing = "source_missing",
    missing_file = "source_missing",
    missing_sheet = "missing_sheet",
    non_numeric = "non_numeric",
    duplicate_conflict = "duplicate_code_conflict",
    ratio_missing_input = "derived_missing_input",
    extract_status
  )
}

country_sna_include_value_row <- function(
  country,
  year,
  spec,
  value,
  raw_value,
  extract_status,
  evidence = NULL,
  value_type = "primitive",
  source = list(),
  layout = list(),
  country_rule = list()
) {
  if (is.null(evidence)) {
    evidence <- data.frame(
      code = NA_character_, code_long = NA_character_, row_in_range = NA_integer_,
      excel_row = NA_integer_, excel_col = NA_character_, stringsAsFactors = FALSE
    )
  }
  value <- as.numeric(value)
  magnitude <- as.numeric(country_rule$magnitude %||% 1)
  value_standardized <- if (isTRUE(value_type == "primitive") && !is.na(value)) value * magnitude else value
  value_for_formula <- value_standardized
  if (isTRUE(value_type == "primitive") && is.na(value_for_formula) &&
      extract_status %in% c("zero_treated_missing", "no_code_match", "no_role_match", "non_numeric")) {
    # The compatibility export leaves these primitive cells missing, but the
    # legacy ratio formulas used zero-valued locals after a successful workbook
    # import. Keep that calculation value explicit instead of smuggling it into
    # the exported variable.
    value_for_formula <- 0
  }
  role <- spec$role %||% NA_character_
  data.frame(
    country = country,
    year = as.integer(year),
    variable = spec$name %||% NA_character_,
    account_code = spec$account %||% NA_character_,
    sector_role = role,
    flow_direction = country_sna_include_flow_direction(role),
    value_raw = raw_value %||% NA_character_,
    value_standardized = value_standardized,
    value_for_formula = value_for_formula,
    value_type = value_type,
    source_file = source$file %||% NA_character_,
    sheet = source$sheet %||% NA_character_,
    cell_range = layout$range %||% NA_character_,
    row_evidence = evidence$excel_row[[1]] %||% NA_integer_,
    column_evidence = evidence$excel_col[[1]] %||% NA_character_,
    matched_code = evidence$code[[1]] %||% NA_character_,
    code_long = evidence$code_long[[1]] %||% NA_character_,
    extract_status = extract_status,
    contract_status = country_sna_include_contract_status(extract_status),
    layout_id = layout$id %||% NA_character_,
    adapter = country_rule$adapter %||% NA_character_,
    warning = source$warning %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

country_sna_include_pick_from_group <- function(rows, role, aliases, zero_as_missing = TRUE) {
  aliases_norm <- country_sna_include_normalize_code(aliases)
  if (!nrow(rows)) {
    return(list(status = "source_missing", value = NA_real_, raw = NA_character_, evidence = NULL))
  }
  if (!(role %in% rows$role)) {
    return(list(status = "no_role_match", value = NA_real_, raw = NA_character_, evidence = NULL))
  }
  candidates <- rows[rows$role == role & rows$code %in% aliases_norm, , drop = FALSE]
  if (!nrow(candidates)) {
    return(list(status = "no_code_match", value = NA_real_, raw = NA_character_, evidence = NULL))
  }
  usable <- candidates[!is.na(candidates$value), , drop = FALSE]
  if (!nrow(usable)) {
    return(list(status = "non_numeric", value = NA_real_, raw = candidates$value_raw[[1]], evidence = candidates[1L, , drop = FALSE]))
  }
  if (isTRUE(zero_as_missing)) {
    nonzero <- usable[usable$value != 0, , drop = FALSE]
    if (!nrow(nonzero)) {
      out <- usable[1L, , drop = FALSE]
      return(list(status = "zero_treated_missing", value = NA_real_, raw = out$value_raw[[1]], evidence = out))
    }
    usable <- nonzero
  }
  unique_values <- unique(usable$value)
  if (length(unique_values) > 1L) {
    return(list(status = "duplicate_conflict", value = NA_real_, raw = usable$value_raw[[1]], evidence = usable[1L, , drop = FALSE]))
  }
  out <- usable[1L, , drop = FALSE]
  status <- if (nrow(usable) > 1L) "duplicate_identical" else "matched"
  list(status = status, value = unique_values[[1]], raw = out$value_raw[[1]], evidence = out)
}

country_sna_include_extract_primitive <- function(
  rows,
  country,
  year,
  spec,
  contract,
  country_rule,
  source,
  layout,
  forced_status = NULL
) {
  if (!is.null(forced_status)) {
    return(country_sna_include_value_row(
      country, year, spec, NA_real_, NA_character_, forced_status,
      source = source, layout = layout, country_rule = country_rule
    ))
  }
  aliases <- country_sna_include_alias_groups(contract, country_rule, spec$account)
  last <- NULL
  for (group in aliases) {
    picked <- country_sna_include_pick_from_group(
      rows,
      spec$role,
      group,
      zero_as_missing = isTRUE(contract$thresholds$zero_as_missing)
    )
    last <- picked
    if (picked$status %in% c("matched", "duplicate_identical", "zero_treated_missing", "duplicate_conflict", "non_numeric")) {
      break
    }
  }
  country_sna_include_value_row(
    country,
    year,
    spec,
    last$value,
    last$raw,
    last$status,
    evidence = last$evidence,
    source = source,
    layout = layout,
    country_rule = country_rule
  )
}

country_sna_include_add_derived <- function(values, country, year, contract, country_rule, source, layout) {
  derived <- contract$variables$derived %||% list()
  if (!length(derived)) {
    return(data.frame())
  }
  lookup <- stats::setNames(values$value_for_formula, values$variable)
  rows <- list()
  for (spec in derived) {
    value <- NA_real_
    status <- "ratio_missing_input"
    if (identical(spec$op, "ratio")) {
      numerator <- lookup[[spec$numerator]]
      denominator <- lookup[[spec$denominator]]
      if (!is.null(numerator) && !is.null(denominator) && !is.na(numerator) && !is.na(denominator) && denominator != 0) {
        value <- numerator / denominator
        status <- "computed"
      }
    } else if (identical(spec$op, "sum")) {
      inputs <- lookup[as.character(spec$inputs)]
      if (length(inputs) && all(!is.na(inputs))) {
        value <- sum(inputs)
        status <- "computed"
      }
    } else {
      status <- "unknown_derived_operation"
    }
    rows[[spec$name]] <- country_sna_include_value_row(
      country,
      year,
      spec,
      value,
      NA_character_,
      status,
      value_type = "derived_ratio",
      source = source,
      layout = layout,
      country_rule = country_rule
    )
    lookup[[spec$name]] <- rows[[spec$name]]$value_for_formula[[1]]
  }
  country_sna_include_bind(rows)
}

country_sna_include_primitive_override_for <- function(country_rule, variable) {
  overrides <- country_rule$primitive_overrides %||% list()
  if (!length(overrides)) {
    return(NULL)
  }
  for (i in seq_along(overrides)) {
    override <- overrides[[i]]
    override$id <- override$id %||% paste0("primitive_override_", i)
    variables <- as.character(override$variables %||% character())
    if (variable %in% variables) {
      return(override)
    }
  }
  NULL
}

country_sna_include_read_primitive_overrides <- function(country_rule, year, root) {
  overrides <- country_rule$primitive_overrides %||% list()
  if (!length(overrides)) {
    return(list())
  }
  out <- list()
  for (i in seq_along(overrides)) {
    override <- overrides[[i]]
    id <- override$id %||% paste0("primitive_override_", i)
    if (identical(override$type, "year_column_table")) {
      out[[id]] <- country_sna_include_read_year_column_table(override, year, root)
    }
  }
  out
}

country_sna_include_source_match_row <- function(country, year, country_rule, layout, source, sheet = NULL, table_health = NA_character_) {
  data.frame(
    country = country,
    year = as.integer(year),
    adapter = country_rule$adapter %||% NA_character_,
    layout_id = layout$id %||% NA_character_,
    source_selector = source$selector %||% NA_character_,
    source_status = source$status %||% NA_character_,
    source_file = source$file %||% NA_character_,
    candidate_count = length(source$candidates %||% character()),
    sheet = sheet$sheet %||% NA_character_,
    sheet_status = sheet$status %||% NA_character_,
    cell_range = layout$range %||% NA_character_,
    table_health = table_health %||% NA_character_,
    warning = paste(na.omit(c(source$warning, sheet$warning)), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

country_sna_include_extract_year <- function(country, year, contract, root) {
  country_rule <- contract$country_rules[[country]]
  layout <- country_sna_include_layout_for_year(country_rule, year)
  source_spec <- layout$source %||% country_rule$source
  source <- country_sna_include_resolve_source(source_spec, year, root)
  source$sheet <- NA_character_
  sheet <- list(sheet = NA_character_, status = NA_character_, warning = NA_character_)
  table_health <- NA_character_

  primitive_specs <- contract$variables$primitives
  rows <- data.frame()
  forced_status <- NULL
  primitive_overrides <- country_sna_include_read_primitive_overrides(country_rule, year, root)

  if (!identical(source$status, "matched")) {
    forced_status <- source$status
  } else if (identical(country_rule$adapter, "sector_file_bundle")) {
    rows <- country_sna_include_read_sector_bundle(source_spec, source)
    table_health <- if (nrow(rows)) "read_ok" else "empty_sector_bundle"
  } else {
    sheet <- country_sna_include_resolve_sheet(source$file, year, layout$sheet)
    source$sheet <- sheet$sheet
    if (!identical(sheet$status, "matched")) {
      forced_status <- sheet$status
    } else {
      table <- country_sna_include_read_workbook_table(source$file, sheet$sheet, layout$range, layout$columns)
      rows <- table$rows
      table_health <- table$health
    }
  }

  primitives <- lapply(primitive_specs, function(spec) {
    active_rows <- rows
    active_source <- source
    active_layout <- layout
    active_forced_status <- forced_status
    active_spec <- spec
    override <- country_sna_include_primitive_override_for(country_rule, spec$name)
    if (!is.null(override)) {
      override_id <- override$id %||% NA_character_
      override_data <- primitive_overrides[[override_id]]
      active_rows <- override_data$rows %||% data.frame()
      active_source <- override_data$source %||% source
      active_layout <- override_data$layout %||% layout
      active_forced_status <- if (identical(override_data$status, "read_ok")) NULL else override_data$status %||% "source_missing"
      active_spec$role <- override$role %||% override_data$role %||% spec$role
    }
    country_sna_include_extract_primitive(
      active_rows,
      country,
      year,
      active_spec,
      contract,
      country_rule,
      active_source,
      active_layout,
      forced_status = active_forced_status
    )
  })
  primitives <- country_sna_include_bind(primitives)
  derived <- country_sna_include_add_derived(primitives, country, year, contract, country_rule, source, layout)
  values <- country_sna_include_bind(primitives, derived)
  source_match <- country_sna_include_source_match_row(country, year, country_rule, layout, source, sheet, table_health)
  list(values = values, source_match = source_match)
}

country_sna_include_apply_exclusions <- function(values, contract) {
  exclusions <- contract$post_merge_exclusions %||% list()
  if (!length(exclusions) || !nrow(values)) {
    return(values)
  }
  for (rule in exclusions) {
    min_year <- if (is.null(rule$years$min)) -Inf else as.integer(rule$years$min)
    max_year <- if (is.null(rule$years$max)) Inf else as.integer(rule$years$max)
    variables <- as.character(rule$variables %||% character())
    hit <- values$country == rule$country &
      values$year >= min_year &
      values$year <= max_year &
      values$variable %in% variables
    if (any(hit)) {
      values$value_standardized[hit] <- NA_real_
      values$extract_status[hit] <- "excluded_by_contract"
      values$contract_status[hit] <- country_sna_include_contract_status("excluded_by_contract")
      values$warning[hit] <- rule$reason %||% values$warning[hit]
    }
  }
  values
}

country_sna_include_extract_all <- function(contract, root, years = country_sna_include_years(contract, root)) {
  extracted <- list()
  matches <- list()
  for (country in contract$countries) {
    for (year in years) {
      key <- paste(country, year, sep = "_")
      result <- country_sna_include_extract_year(country, year, contract, root)
      extracted[[key]] <- result$values
      matches[[key]] <- result$source_match
    }
  }
  values <- country_sna_include_bind(extracted)
  values <- country_sna_include_apply_exclusions(values, contract)
  source_matches <- country_sna_include_bind(matches)
  list(values_long = values, source_matches = source_matches)
}

country_sna_include_variable_names <- function(contract) {
  c(
    vapply(contract$variables$primitives, function(x) x$name, character(1)),
    vapply(contract$variables$derived %||% list(), function(x) x$name, character(1))
  )
}

country_sna_include_wide <- function(values, contract, years) {
  countries <- contract$countries
  grid <- expand.grid(country = countries, year = years, stringsAsFactors = FALSE)
  grid <- grid[order(match(grid$country, countries), grid$year), , drop = FALSE]
  variables <- country_sna_include_variable_names(contract)
  for (variable in variables) {
    grid[[variable]] <- NA_real_
  }
  if (!nrow(values)) {
    return(grid)
  }
  keep <- values[values$variable %in% variables, , drop = FALSE]
  for (i in seq_len(nrow(keep))) {
    row <- grid$country == keep$country[[i]] & grid$year == keep$year[[i]]
    if (any(row)) {
      grid[[keep$variable[[i]]]][row] <- keep$value_standardized[[i]]
    }
  }
  grid
}

country_sna_include_contract_health <- function(values, source_matches) {
  keys <- unique(source_matches[, c("country", "year"), drop = FALSE])
  rows <- list()
  for (i in seq_len(nrow(keys))) {
    country <- keys$country[[i]]
    year <- keys$year[[i]]
    source <- source_matches[source_matches$country == country & source_matches$year == year, , drop = FALSE]
    part <- values[values$country == country & values$year == year & values$value_type == "primitive", , drop = FALSE]
    flags <- unique(c(
      source$source_status[source$source_status != "matched"],
      source$sheet_status[!is.na(source$sheet_status) & source$sheet_status != "matched"],
      source$table_health[!is.na(source$table_health) & source$table_health != "read_ok"],
      part$contract_status[part$contract_status != "contract_ok" & part$contract_status != "contract_exclusion"]
    ))
    flags <- flags[nzchar(flags) & !is.na(flags)]
    status <- if (!length(flags)) {
      "contract_ok"
    } else if (any(flags %in% c("missing_file", "source_missing", "missing_sheet", "duplicate_code_conflict"))) {
      "attention"
    } else {
      "warning"
    }
    rows[[paste(country, year, sep = "_")]] <- data.frame(
      country = country,
      year = year,
      status = status,
      flags = paste(flags, collapse = ";"),
      matched_values = sum(part$contract_status == "contract_ok", na.rm = TRUE),
      excluded_values = sum(part$contract_status == "contract_exclusion", na.rm = TRUE),
      missing_values = sum(part$contract_status %in% c("expected_code_missing", "expected_role_missing", "source_missing", "missing_sheet"), na.rm = TRUE),
      duplicate_conflicts = sum(part$contract_status == "duplicate_code_conflict", na.rm = TRUE),
      non_numeric = sum(part$contract_status == "non_numeric", na.rm = TRUE),
      zero_treated_missing = sum(part$contract_status == "zero_treated_missing", na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
  country_sna_include_bind(rows)
}

country_sna_include_country_summary <- function(wide, health) {
  if (!nrow(wide)) {
    return(data.frame())
  }
  rows <- list()
  for (country in unique(wide$country)) {
    part <- wide[wide$country == country, , drop = FALSE]
    h <- health[health$country == country, , drop = FALSE]
    ratio <- part$ratio_d43d44
    transfer <- part$ratio_d75_d7
    rows[[country]] <- data.frame(
      country = country,
      years = paste(range(part$year), collapse = "-"),
      rows = nrow(part),
      years_with_property_ratio = sum(!is.na(ratio)),
      years_with_transfer_ratio = sum(!is.na(transfer)),
      attention_years = sum(h$status == "attention", na.rm = TRUE),
      warning_years = sum(h$status == "warning", na.rm = TRUE),
      status = if (any(h$status == "attention", na.rm = TRUE)) "attention" else if (any(h$status == "warning", na.rm = TRUE)) "warning" else "contract_ok",
      stringsAsFactors = FALSE
    )
  }
  country_sna_include_bind(rows)
}

country_sna_include_empty_parity <- function(status, benchmark_path, message) {
  list(
    parity_summary = data.frame(
      status = status,
      benchmark_path = benchmark_path %||% NA_character_,
      message = message,
      country_year_rows = 0L,
      variables = 0L,
      cells = 0L,
      same_within_tolerance = 0L,
      both_missing = 0L,
      numeric_differences = 0L,
      candidate_missing_benchmark_value = 0L,
      candidate_value_benchmark_missing = 0L,
      benchmark_duplicate_conflicts = 0L,
      stringsAsFactors = FALSE
    ),
    parity_variable_summary = data.frame(stringsAsFactors = FALSE),
    parity_country_summary = data.frame(stringsAsFactors = FALSE),
    parity_cell_detail = data.frame(stringsAsFactors = FALSE)
  )
}

country_sna_include_float_tolerance <- function(candidate, benchmark, rel_tol = 1e-6, abs_tol = 1e-8) {
  abs_tol + rel_tol * pmax(1, abs(candidate), abs(benchmark))
}

country_sna_include_benchmark_value <- function(production, key_col, country, year, variable, rel_tol, abs_tol) {
  hit <- as.character(production[[key_col]]) == country & as.integer(production$year) == as.integer(year)
  values_all <- suppressWarnings(as.numeric(production[[variable]][hit]))
  values <- values_all[!is.na(values_all)]
  if (!length(values)) {
    return(list(value = NA_real_, rows = length(values_all), non_missing = 0L, spread = NA_real_, conflict = FALSE))
  }
  spread <- max(values) - min(values)
  tolerance <- country_sna_include_float_tolerance(max(values), min(values), rel_tol = rel_tol, abs_tol = abs_tol)
  list(
    value = values[[1L]],
    rows = length(values_all),
    non_missing = length(values),
    spread = spread,
    conflict = isTRUE(spread > tolerance)
  )
}

country_sna_include_parity_report <- function(
  wide,
  contract,
  root = country_sna_include_repo_root(),
  production_path = NULL,
  rel_tol = 1e-6,
  abs_tol = 1e-8
) {
  production_path <- production_path %||% contract$base_dataset
  production_path <- country_sna_include_path(production_path, root)
  if (is.na(production_path) || !file.exists(production_path)) {
    return(country_sna_include_empty_parity("benchmark_missing", production_path, "Benchmark DTA is not available."))
  }
  if (!country_sna_include_has("haven")) {
    return(country_sna_include_empty_parity("haven_missing", production_path, "Package haven is required to read the benchmark DTA."))
  }
  if (!nrow(wide)) {
    return(country_sna_include_empty_parity("candidate_empty", production_path, "Candidate wide table has no rows."))
  }

  production <- as.data.frame(haven::read_dta(production_path))
  key_col <- if ("country" %in% names(production)) {
    "country"
  } else if ("_ISO3C_" %in% names(production)) {
    "_ISO3C_"
  } else {
    NA_character_
  }
  if (is.na(key_col) || !("year" %in% names(production))) {
    return(country_sna_include_empty_parity("benchmark_key_missing", production_path, "Benchmark DTA has no country/_ISO3C_ plus year key."))
  }

  variables <- setdiff(intersect(names(wide), names(production)), c("country", "year"))
  variables <- variables[grepl("(^ratio_d|_cei$)", variables)]
  if (!length(variables)) {
    return(country_sna_include_empty_parity("no_shared_variables", production_path, "Candidate and benchmark have no shared country-SNA variables."))
  }

  rows <- vector("list", nrow(wide) * length(variables))
  index <- 0L
  for (i in seq_len(nrow(wide))) {
    country <- as.character(wide$country[[i]])
    year <- as.integer(wide$year[[i]])
    for (variable in variables) {
      index <- index + 1L
      candidate <- suppressWarnings(as.numeric(wide[[variable]][[i]]))
      benchmark <- country_sna_include_benchmark_value(
        production,
        key_col,
        country,
        year,
        variable,
        rel_tol = rel_tol,
        abs_tol = abs_tol
      )
      value <- benchmark$value
      abs_diff <- abs(candidate - value)
      denominator <- max(c(1, abs(candidate), abs(value)), na.rm = TRUE)
      rel_diff <- if (is.na(abs_diff)) NA_real_ else abs_diff / denominator
      tolerance <- country_sna_include_float_tolerance(candidate, value, rel_tol = rel_tol, abs_tol = abs_tol)
      status <- if (isTRUE(benchmark$conflict)) {
        "benchmark_duplicate_conflict"
      } else if (is.na(candidate) && is.na(value)) {
        "both_missing"
      } else if (is.na(candidate) && !is.na(value)) {
        "candidate_missing_benchmark_value"
      } else if (!is.na(candidate) && is.na(value)) {
        "candidate_value_benchmark_missing"
      } else if (abs_diff <= tolerance) {
        "same_within_tolerance"
      } else {
        "numeric_difference"
      }
      rows[[index]] <- data.frame(
        country = country,
        year = year,
        variable = variable,
        candidate_value = candidate,
        benchmark_value = value,
        abs_diff = abs_diff,
        rel_diff = rel_diff,
        tolerance = tolerance,
        benchmark_rows = benchmark$rows,
        benchmark_non_missing = benchmark$non_missing,
        benchmark_spread = benchmark$spread,
        status = status,
        stringsAsFactors = FALSE
      )
    }
  }
  detail <- country_sna_include_bind(rows)

  count_status <- function(status) sum(detail$status == status, na.rm = TRUE)
  failing <- count_status("numeric_difference") +
    count_status("candidate_missing_benchmark_value") +
    count_status("candidate_value_benchmark_missing") +
    count_status("benchmark_duplicate_conflict")
  summary <- data.frame(
    status = if (failing == 0L) "parity_ok" else "parity_failed",
    benchmark_path = production_path,
    message = if (failing == 0L) "Candidate matches benchmark within tolerance." else "Candidate does not match benchmark.",
    country_year_rows = nrow(wide),
    variables = length(variables),
    cells = nrow(detail),
    same_within_tolerance = count_status("same_within_tolerance"),
    both_missing = count_status("both_missing"),
    numeric_differences = count_status("numeric_difference"),
    candidate_missing_benchmark_value = count_status("candidate_missing_benchmark_value"),
    candidate_value_benchmark_missing = count_status("candidate_value_benchmark_missing"),
    benchmark_duplicate_conflicts = count_status("benchmark_duplicate_conflict"),
    stringsAsFactors = FALSE
  )

  summarize_group <- function(data, group) {
    parts <- split(data, data[[group]], drop = TRUE)
    country_sna_include_bind(lapply(parts, function(part) {
      data.frame(
        setNames(list(part[[group]][[1L]]), group),
        cells = nrow(part),
        same_within_tolerance = sum(part$status == "same_within_tolerance", na.rm = TRUE),
        both_missing = sum(part$status == "both_missing", na.rm = TRUE),
        numeric_differences = sum(part$status == "numeric_difference", na.rm = TRUE),
        candidate_missing_benchmark_value = sum(part$status == "candidate_missing_benchmark_value", na.rm = TRUE),
        candidate_value_benchmark_missing = sum(part$status == "candidate_value_benchmark_missing", na.rm = TRUE),
        benchmark_duplicate_conflicts = sum(part$status == "benchmark_duplicate_conflict", na.rm = TRUE),
        max_abs_diff = if (any(part$status == "numeric_difference", na.rm = TRUE)) max(part$abs_diff[part$status == "numeric_difference"], na.rm = TRUE) else 0,
        max_rel_diff = if (any(part$status == "numeric_difference", na.rm = TRUE)) max(part$rel_diff[part$status == "numeric_difference"], na.rm = TRUE) else 0,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))
  }

  list(
    parity_summary = summary,
    parity_variable_summary = summarize_group(detail, "variable"),
    parity_country_summary = summarize_group(detail, "country"),
    parity_cell_detail = detail
  )
}

country_sna_include_default_exploration_root <- function(root) {
  file.path(root, "output", "experiments", "country_sna_explore")
}

country_sna_include_resolve_exploration_run <- function(root, exploration_run = NULL) {
  path <- exploration_run %||% country_sna_include_default_exploration_root(root)
  country_sna_include_path(path, root)
}

country_sna_include_read_explorer_table <- function(exploration_root, name) {
  path <- file.path(exploration_root, "tables", paste0(name, ".csv"))
  if (!file.exists(path)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

country_sna_include_read_expectations <- function(root, exploration_run = NULL) {
  exploration_root <- country_sna_include_resolve_exploration_run(root, exploration_run)
  list(
    root = exploration_root,
    year_expectations = country_sna_include_read_explorer_table(exploration_root, "year_expectations"),
    variable_expectations = country_sna_include_read_explorer_table(exploration_root, "variable_expectations"),
    structure_summary = country_sna_include_read_explorer_table(exploration_root, "structure_summary")
  )
}

country_sna_include_expected_years <- function(expectations) {
  years <- expectations$variable_expectations$year %||% integer()
  years <- suppressWarnings(as.integer(years[!is.na(years)]))
  sort(unique(years))
}

country_sna_include_lookup_value <- function(wide, country, year, variable) {
  if (!nrow(wide) || !(variable %in% names(wide))) return(NA_real_)
  hit <- wide$country == country & as.integer(wide$year) == as.integer(year)
  if (!any(hit)) return(NA_real_)
  suppressWarnings(as.numeric(wide[[variable]][which(hit)[[1L]]]))
}

country_sna_include_expectation_detail <- function(wide, values_long, expectations) {
  expected <- expectations$variable_expectations
  if (!nrow(expected)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  rows <- lapply(seq_len(nrow(expected)), function(i) {
    row <- expected[i, , drop = FALSE]
    value <- country_sna_include_lookup_value(wide, row$country[[1L]], row$year[[1L]], row$variable[[1L]])
    matched <- values_long[
      values_long$country == row$country[[1L]] &
        values_long$year == row$year[[1L]] &
        values_long$variable == row$variable[[1L]],
      ,
      drop = FALSE
    ]
    extract_status <- if (nrow(matched)) matched$extract_status[[1L]] else "not_extracted"
    status <- if (identical(row$expected_status[[1L]], "blocked_adapter_required")) {
      "blocked_adapter_required"
    } else if (identical(row$expected_status[[1L]], "expected_contract_missing")) {
      if (is.na(value)) "ok_contract_missing" else "revision_detected"
    } else if (extract_status == "duplicate_conflict") {
      "warning_ambiguous_match"
    } else if (!is.na(value)) {
      "ok_value_found"
    } else {
      "warning_missing_expected_value"
    }
    data.frame(
      country = row$country,
      year = as.integer(row$year),
      year_role = row$year_role,
      variable = row$variable,
      expected_status = row$expected_status,
      extracted_value = value,
      extract_status = extract_status,
      status = status,
      source_file = if (nrow(matched)) matched$source_file[[1L]] else NA_character_,
      sheet = if (nrow(matched)) matched$sheet[[1L]] else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  country_sna_include_bind(rows)
}

country_sna_include_expectation_summary <- function(detail) {
  if (!nrow(detail)) {
    return(data.frame(
      country = character(),
      status = character(),
      expected_values = integer(),
      found_values = integer(),
      expected_contract_missing = integer(),
      warnings = integer(),
      blocked = integer(),
      stringsAsFactors = FALSE
    ))
  }
  parts <- split(detail, detail$country, drop = TRUE)
  country_sna_include_bind(lapply(parts, function(part) {
    blocked <- sum(part$status == "blocked_adapter_required", na.rm = TRUE)
    warnings <- sum(part$status %in% c("warning_missing_expected_value", "warning_ambiguous_match", "revision_detected"), na.rm = TRUE)
    status <- if (blocked > 0L) {
      "blocked"
    } else if (warnings > 0L) {
      "check_following"
    } else {
      "all_good"
    }
    data.frame(
      country = part$country[[1L]],
      status = status,
      expected_values = sum(part$expected_status == "expected_value", na.rm = TRUE),
      found_values = sum(part$status == "ok_value_found", na.rm = TRUE),
      expected_contract_missing = sum(part$status == "ok_contract_missing", na.rm = TRUE),
      warnings = warnings,
      blocked = blocked,
      stringsAsFactors = FALSE
    )
  }))
}

country_sna_include_overall_status <- function(summary) {
  if (!nrow(summary)) return("check_following")
  if (any(summary$status == "blocked", na.rm = TRUE)) return("blocked")
  if (any(summary$status == "check_following", na.rm = TRUE)) return("check_following")
  "all_good"
}

country_sna_include_manifest <- function(include_summary, expectations, dry_run = TRUE) {
  data.frame(
    key = c("run_at", "dry_run", "status", "exploration_run"),
    value = c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      as.character(isTRUE(dry_run)),
      country_sna_include_overall_status(include_summary),
      expectations$root %||% ""
    ),
    stringsAsFactors = FALSE
  )
}

country_sna_include_manifest_value <- function(manifest, key) {
  hit <- manifest$key == key
  if (!any(hit)) return("")
  as.character(manifest$value[which(hit)[[1L]]])
}

country_sna_include_assert_clean_dry_run <- function(root, contract, output_dir, exploration_root) {
  paths <- country_sna_include_output_paths(root, contract, output_dir)
  manifest_path <- file.path(paths$logs, "include_manifest.csv")
  # Apply is gated by a prior dry-run manifest so source promotion cannot be
  # separated from the exact exploration evidence and expectation checks that
  # were reviewed. A new exploration run must get its own clean dry run.
  if (!file.exists(manifest_path)) {
    stop("`--apply` requires a clean include dry-run manifest. Run `dina sources include country-sna --dry-run` first.", call. = FALSE)
  }
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  status <- country_sna_include_manifest_value(manifest, "status")
  dry_run <- country_sna_include_manifest_value(manifest, "dry_run")
  prior_exploration <- country_sna_include_manifest_value(manifest, "exploration_run")
  if (!identical(status, "all_good") || !identical(dry_run, "TRUE") || !identical(normalizePath(prior_exploration, mustWork = FALSE), normalizePath(exploration_root, mustWork = FALSE))) {
    stop("`--apply` requires a clean dry-run manifest for the same exploration run.", call. = FALSE)
  }
  TRUE
}

country_sna_include_digest <- function(path) {
  country_sna_include_need("digest")
  digest::digest(file = path, algo = "sha256")
}

country_sna_include_registry <- function(root) {
  path <- file.path(root, "config", "sources.yml")
  config <- country_sna_include_read_yaml(path)
  config$sources %||% list()
}

country_sna_include_source_destination <- function(root, source_inventory_row) {
  registry <- country_sna_include_registry(root)
  country <- source_inventory_row$country[[1L]]
  matches <- Filter(function(source) {
    identical(source$family %||% "", "country_sna") && identical(source$country %||% "", country)
  }, registry)
  if (length(matches) != 1L) {
    return(list(status = "ambiguous_registry", destination = NA_character_))
  }
  template <- matches[[1L]]$destination %||% ""
  if (!nzchar(template) || !grepl("\\{basename\\}", template)) {
    return(list(status = "ambiguous_destination", destination = NA_character_))
  }
  list(
    status = "ok",
    destination = country_sna_include_path(gsub("{basename}", basename(source_inventory_row$file[[1L]]), template, fixed = TRUE), root)
  )
}

country_sna_include_apply_sources <- function(root, expectations) {
  inventory <- country_sna_include_read_explorer_table(expectations$root, "source_inventory")
  if (!nrow(inventory)) {
    return(data.frame(action = "none", status = "no_source_inventory", stringsAsFactors = FALSE))
  }
  candidates <- inventory[
    inventory$source_set == "new" &
      inventory$status %in% c("matched", "ambiguous_stem") &
      !is.na(inventory$file) &
      nzchar(inventory$file),
    ,
    drop = FALSE
  ]
  if (!nrow(candidates)) {
    return(data.frame(action = "none", status = "no_new_files_to_promote", stringsAsFactors = FALSE))
  }

  # Apply is intentionally conservative: source promotion is allowed only when
  # the registry maps one country-SNA source to a destination template and an
  # existing destination is byte-identical. Different existing content blocks
  # the operation so the pipeline cannot accidentally consume a silent overwrite.
  rows <- lapply(seq_len(nrow(candidates)), function(i) {
    row <- candidates[i, , drop = FALSE]
    destination <- country_sna_include_source_destination(root, row)
    if (!identical(destination$status, "ok")) {
      return(data.frame(source = row$file, destination = NA_character_, action = "blocked", status = destination$status, stringsAsFactors = FALSE))
    }
    if (file.exists(destination$destination)) {
      same <- identical(country_sna_include_digest(row$file), country_sna_include_digest(destination$destination))
      if (!same) {
        return(data.frame(source = row$file, destination = destination$destination, action = "blocked", status = "destination_exists_different_content", stringsAsFactors = FALSE))
      }
      return(data.frame(source = row$file, destination = destination$destination, action = "skip", status = "already_present_identical", stringsAsFactors = FALSE))
    }
    dir.create(dirname(destination$destination), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(row$file, destination$destination, overwrite = FALSE)
    data.frame(source = row$file, destination = destination$destination, action = if (ok) "copied" else "blocked", status = if (ok) "promoted" else "copy_failed", stringsAsFactors = FALSE)
  })
  out <- country_sna_include_bind(rows)
  if (any(out$action == "blocked", na.rm = TRUE)) {
    stop("`--apply` blocked: at least one source destination is ambiguous or would overwrite different content.", call. = FALSE)
  }
  out
}

country_sna_include_merge_candidate <- function(wide, contract, root, output_path) {
  base <- country_sna_include_path(contract$base_dataset, root)
  if (!file.exists(base)) {
    return(list(path = NA_character_, status = "base_dataset_missing", warning = sprintf("Base dataset not found: %s", base)))
  }
  if (!country_sna_include_has("haven")) {
    return(list(path = NA_character_, status = "haven_missing", warning = "Package haven is required to write the candidate DTA."))
  }
  data <- haven::read_dta(base)
  key <- if ("_ISO3C_" %in% names(data)) "_ISO3C_" else if ("country" %in% names(data)) "country" else NA_character_
  if (is.na(key) || !("year" %in% names(data))) {
    return(list(path = NA_character_, status = "base_key_missing", warning = "Base dataset has no country/_ISO3C_ plus year key."))
  }
  patch_vars <- setdiff(names(wide), c("country", "year"))
  for (variable in patch_vars) {
    if (!(variable %in% names(data))) {
      data[[variable]] <- NA_real_
    }
  }
  for (i in seq_len(nrow(wide))) {
    hit <- as.character(data[[key]]) == wide$country[[i]] & as.integer(data$year) == as.integer(wide$year[[i]])
    if (!any(hit)) {
      next
    }
    for (variable in patch_vars) {
      value <- wide[[variable]][[i]]
      data[[variable]][hit] <- value
    }
  }
  exclusions <- contract$post_merge_exclusions %||% list()
  for (rule in exclusions) {
    min_year <- if (is.null(rule$years$min)) -Inf else as.integer(rule$years$min)
    max_year <- if (is.null(rule$years$max)) Inf else as.integer(rule$years$max)
    hit <- as.character(data[[key]]) == rule$country & as.integer(data$year) >= min_year & as.integer(data$year) <= max_year
    for (variable in as.character(rule$variables %||% character())) {
      if (variable %in% names(data)) {
        data[[variable]][hit] <- NA_real_
      }
    }
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  haven::write_dta(data, output_path)
  list(path = output_path, status = "written", warning = NA_character_)
}

country_sna_include_write_figures <- function(wide, paths) {
  if (!country_sna_include_has("ggplot2")) {
    return(data.frame(path = NA_character_, status = "ggplot2_missing", stringsAsFactors = FALSE))
  }
  dir.create(paths$figures, recursive = TRUE, showWarnings = FALSE)
  figures <- list(
    property_income_matching = list(
      variable = "ratio_d43d44",
      transform = function(x) (1 - x) * 100,
      y = "Unmatched property income (%)"
    ),
    remittances_in_transfers = list(
      variable = "ratio_d75_d7",
      transform = function(x) x * 100,
      y = "D75 / D7 (%)"
    )
  )
  rows <- list()
  for (name in names(figures)) {
    spec <- figures[[name]]
    if (!(spec$variable %in% names(wide))) {
      next
    }
    df <- wide[, c("country", "year", spec$variable), drop = FALSE]
    names(df)[[3]] <- "value"
    df$value <- spec$transform(df$value)
    df <- df[!is.na(df$value), , drop = FALSE]
    if (!nrow(df)) {
      rows[[name]] <- data.frame(path = NA_character_, status = "no_values", stringsAsFactors = FALSE)
      next
    }
    p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = value, color = country, group = country)) +
      ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
      ggplot2::geom_point(size = 1.3, na.rm = TRUE) +
      ggplot2::labs(x = NULL, y = spec$y, color = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")
    png_path <- file.path(paths$figures, paste0(name, ".png"))
    pdf_path <- file.path(paths$figures, paste0(name, ".pdf"))
    ggplot2::ggsave(png_path, p, width = 8, height = 4.5, dpi = 160)
    ggplot2::ggsave(pdf_path, p, width = 8, height = 4.5)
    rows[[name]] <- data.frame(path = paste(c(png_path, pdf_path), collapse = ";"), status = "written", stringsAsFactors = FALSE)
  }
  country_sna_include_bind(rows)
}

country_sna_include_write_workbook <- function(outputs, paths) {
  if (!country_sna_include_has("openxlsx")) {
    return(list(path = NA_character_, status = "openxlsx_missing"))
  }
  dir.create(paths$workbooks, recursive = TRUE, showWarnings = FALSE)
  workbook_path <- file.path(paths$workbooks, "country_sna_include.xlsx")
  wb <- openxlsx::createWorkbook()
  for (name in names(outputs)) {
    if (!is.data.frame(outputs[[name]])) {
      next
    }
    sheet <- substr(name, 1L, 31L)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, outputs[[name]])
  }
  openxlsx::saveWorkbook(wb, workbook_path, overwrite = TRUE)
  list(path = workbook_path, status = "written")
}

country_sna_include_write_outputs <- function(result, root, contract, output_dir = NULL) {
  paths <- country_sna_include_output_paths(root, contract, output_dir)
  for (path in unlist(paths, use.names = FALSE)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  tables <- result$outputs
  table_files <- c(
    values_long = "country_sna_values_long.csv",
    values_wide = "country_sna_values_wide.csv",
    source_matches = "source_matches.csv",
    contract_health = "contract_health.csv",
    country_summary = "country_summary.csv"
  )
  for (name in names(tables)) {
    if (!is.data.frame(tables[[name]])) next
    file_name <- if (name %in% names(table_files)) table_files[[name]] else paste0(name, ".csv")
    utils::write.csv(tables[[name]], file.path(paths$tables, file_name), row.names = FALSE, na = "")
  }

  patch_csv <- file.path(paths$data, "country_sna_candidate_patch.csv")
  utils::write.csv(tables$values_wide, patch_csv, row.names = FALSE, na = "")

  patch_dta <- NA_character_
  patch_dta_status <- "haven_missing"
  if (country_sna_include_has("haven")) {
    patch_dta <- file.path(paths$data, "country_sna_candidate_patch.dta")
    haven::write_dta(tables$values_wide, patch_dta)
    patch_dta_status <- "written"
  }

  candidate <- country_sna_include_merge_candidate(
    tables$values_wide,
    contract,
    root,
    file.path(paths$data, "UNDATA-WID-Merged.country_sna_candidate.dta")
  )
  figures <- country_sna_include_write_figures(tables$values_wide, paths)
  workbook <- country_sna_include_write_workbook(tables, paths)

  parity_status <- if (!is.null(tables$parity_summary) && nrow(tables$parity_summary)) tables$parity_summary$status[[1L]] else NA_character_
  run_metadata <- data.frame(
    key = c("contract", "output_root", "patch_csv", "patch_dta", "candidate_dta", "workbook", "parity_status", "include_status"),
    value = c(
      contract$contract_path %||% NA_character_,
      paths$root,
      patch_csv,
      if (is.na(patch_dta)) patch_dta_status else patch_dta,
      if (is.na(candidate$path)) candidate$status else candidate$path,
      if (is.na(workbook$path)) workbook$status else workbook$path,
      parity_status,
      if (!is.null(tables$include_manifest) && nrow(tables$include_manifest)) country_sna_include_manifest_value(tables$include_manifest, "status") else ""
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(run_metadata, file.path(paths$logs, "run_metadata.csv"), row.names = FALSE, na = "")
  if (!is.null(tables$include_manifest) && nrow(tables$include_manifest)) {
    utils::write.csv(tables$include_manifest, file.path(paths$logs, "include_manifest.csv"), row.names = FALSE, na = "")
  }

  parity_warning <- if (!is.null(tables$parity_summary) && nrow(tables$parity_summary) && !identical(tables$parity_summary$status[[1L]], "parity_ok")) {
    sprintf("Parity status: %s", tables$parity_summary$status[[1L]])
  } else {
    character()
  }
  warnings <- unique(na.omit(c(
    tables$source_matches$warning,
    tables$values_long$warning,
    parity_warning,
    candidate$warning,
    figures$status[figures$status != "written"],
    workbook$status[workbook$status != "written"],
    patch_dta_status[patch_dta_status != "written"]
  )))
  writeLines(warnings %||% character(), file.path(paths$logs, "warnings.txt"))

  list(paths = paths, patch_csv = patch_csv, patch_dta = patch_dta, candidate = candidate, figures = figures, workbook = workbook)
}

run_country_sna_include <- function(
  root = country_sna_include_repo_root(),
  contract_path = file.path(root, "config", "country_sna_include.yml"),
  output_dir = NULL,
  exploration_run = NULL,
  years = NULL,
  write_outputs = TRUE,
  apply = FALSE
) {
  contract <- country_sna_include_read_contract(root, contract_path)
  expectations <- country_sna_include_read_expectations(root, exploration_run)
  expected_years <- country_sna_include_expected_years(expectations)
  years <- years %||% sort(unique(c(country_sna_include_years(contract, root), expected_years)))
  if (isTRUE(apply)) {
    country_sna_include_assert_clean_dry_run(root, contract, output_dir, expectations$root)
  }
  extracted <- country_sna_include_extract_all(contract, root, years)
  values_wide <- country_sna_include_wide(extracted$values_long, contract, years)
  contract_health <- country_sna_include_contract_health(extracted$values_long, extracted$source_matches)
  country_summary <- country_sna_include_country_summary(values_wide, contract_health)
  parity <- country_sna_include_parity_report(values_wide, contract, root)
  include_detail <- country_sna_include_expectation_detail(values_wide, extracted$values_long, expectations)
  include_summary <- country_sna_include_expectation_summary(include_detail)
  include_manifest <- country_sna_include_manifest(include_summary, expectations, dry_run = !isTRUE(apply))
  apply_report <- if (isTRUE(apply)) {
    country_sna_include_apply_sources(root, expectations)
  } else {
    data.frame(action = "dry_run", status = "not_applied", stringsAsFactors = FALSE)
  }
  outputs <- list(
    include_summary = include_summary,
    include_detail = include_detail,
    include_manifest = include_manifest,
    apply_report = apply_report,
    country_summary = country_summary,
    values_long = extracted$values_long,
    values_wide = values_wide,
    source_matches = extracted$source_matches,
    contract_health = contract_health,
    parity_summary = parity$parity_summary,
    parity_variable_summary = parity$parity_variable_summary,
    parity_country_summary = parity$parity_country_summary,
    parity_cell_detail = parity$parity_cell_detail
  )
  written <- if (isTRUE(write_outputs)) {
    country_sna_include_write_outputs(list(outputs = outputs), root, contract, output_dir)
  } else {
    list(paths = country_sna_include_output_paths(root, contract, output_dir))
  }
  list(outputs = outputs, paths = written$paths, written = written, contract = contract, years = years)
}
