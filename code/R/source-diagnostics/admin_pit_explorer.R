# Experimental PIT admin source explorer.
#
# This module is intentionally isolated from the mainstream admin-data pipeline.
# It inventories incoming PIT files and writes review tables for the staged
# include workflow. It does not modify legacy cleaner scripts or source files.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

admin_pit_explorer_repo_root <- function(start = getwd()) {
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

admin_pit_explorer_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

admin_pit_explorer_has <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

admin_pit_explorer_path <- function(path, root) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  if (grepl("^/", path)) path else file.path(root, path)
}

admin_pit_explorer_relative_path <- function(path, root) {
  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

admin_pit_explorer_read_yaml <- function(path, default = NULL) {
  admin_pit_explorer_need("yaml")
  if (!file.exists(path)) {
    if (!is.null(default)) return(default)
    stop("Missing YAML file: ", path, call. = FALSE)
  }
  yaml::read_yaml(path)
}

admin_pit_explorer_deep_merge <- function(base, override) {
  if (!is.list(base) || !is.list(override)) return(override)
  out <- base
  for (name in names(override)) {
    if (!is.null(out[[name]]) && is.list(out[[name]]) && is.list(override[[name]])) {
      out[[name]] <- admin_pit_explorer_deep_merge(out[[name]], override[[name]])
    } else {
      out[[name]] <- override[[name]]
    }
  }
  out
}

admin_pit_explorer_active_override_path <- function(root) {
  active <- file.path(root, "output", "updates", ".active_update")
  if (!file.exists(active)) return("")
  update_id <- trimws(readLines(active, warn = FALSE, n = 1L))
  if (!nzchar(update_id)) return("")
  path <- file.path(root, "output", "updates", update_id, "config.override.yml")
  if (file.exists(path)) path else ""
}

admin_pit_explorer_effective_config <- function(root, config_path = "config/dina.yml", use_active_update_override = TRUE) {
  base_path <- admin_pit_explorer_path(config_path, root)
  config <- admin_pit_explorer_read_yaml(base_path, default = list())
  override_path <- ""
  if (isTRUE(use_active_update_override)) {
    override_path <- admin_pit_explorer_active_override_path(root)
    if (nzchar(override_path)) {
      config <- admin_pit_explorer_deep_merge(config, admin_pit_explorer_read_yaml(override_path, default = list()))
    }
  }
  attr(config, "config_path") <- normalizePath(base_path, mustWork = FALSE)
  attr(config, "override_path") <- normalizePath(override_path, mustWork = FALSE)
  config
}

admin_pit_explorer_read_contract <- function(
  root = admin_pit_explorer_repo_root(),
  contract_path = file.path(root, "config", "admin_pit_explorer.yml")
) {
  contract <- admin_pit_explorer_read_yaml(contract_path)
  ids <- contract$source_discovery$source_ids %||% NULL
  if (is.null(contract$output_root) || is.null(ids)) {
    stop("Invalid PIT admin explorer contract: missing output_root or source ids.", call. = FALSE)
  }
  contract$contract_path <- contract_path
  contract
}

admin_pit_explorer_years <- function(contract, root) {
  years <- contract$years %||% list()
  config <- admin_pit_explorer_effective_config(
    root,
    years$from_config %||% "config/dina.yml",
    use_active_update_override = isTRUE(years$use_active_update_override %||% TRUE)
  )
  first <- as.integer(config$years$first)
  last <- as.integer(config$years$last)
  if (is.na(first) || is.na(last) || first > last) {
    stop("Invalid DINA year range for PIT admin explorer.", call. = FALSE)
  }
  seq.int(first, last)
}

admin_pit_explorer_output_root <- function(root, contract, output_dir = NULL) {
  admin_pit_explorer_path(output_dir %||% contract$output_root, root)
}

admin_pit_explorer_output_paths <- function(root, contract, output_dir = NULL) {
  out <- admin_pit_explorer_output_root(root, contract, output_dir)
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"))
}

admin_pit_explorer_bind <- function(...) {
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

admin_pit_explorer_source_values <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character())
  if (is.atomic(x)) return(as.character(x))
  unlist(x, use.names = FALSE)
}

admin_pit_explorer_template <- function(x, values = list()) {
  if (is.null(x) || !length(x)) return("")
  out <- as.character(x[[1L]])
  for (name in names(values)) {
    out <- gsub(paste0("\\{", name, "\\}"), as.character(values[[name]]), out)
  }
  out
}

admin_pit_explorer_registry <- function(root) {
  config <- admin_pit_explorer_read_yaml(file.path(root, "config", "sources.yml"), default = list(sources = list()))
  config$sources %||% list()
}

admin_pit_explorer_registry_source <- function(root, source_id) {
  registry <- admin_pit_explorer_registry(root)
  matches <- Filter(function(source) identical(source$id %||% "", source_id), registry)
  if (!length(matches)) NULL else matches[[1L]]
}

admin_pit_explorer_read_include_contract <- function(root) {
  admin_pit_explorer_read_yaml(
    file.path(root, "config", "admin_pit_include.yml"),
    default = list(cleaners = list(static_dependencies = list(), auxiliary_sources = list()))
  )
}

admin_pit_explorer_supported_ids <- function(contract) {
  as.character(contract$source_discovery$source_ids %||% character())
}

admin_pit_explorer_supported_sources <- function(contract, root, countries = NULL) {
  ids <- admin_pit_explorer_supported_ids(contract)
  sources <- Filter(function(source) (source$id %||% "") %in% ids, admin_pit_explorer_registry(root))
  if (!is.null(countries) && length(countries)) {
    countries <- toupper(as.character(countries))
    sources <- Filter(function(source) toupper(source$country %||% "") %in% countries, sources)
  }
  sources
}

admin_pit_explorer_unsupported_sources <- function(contract, root, countries = NULL) {
  ids <- admin_pit_explorer_supported_ids(contract)
  families <- as.character(contract$source_discovery$unsupported_admin_families %||% character())
  sources <- Filter(function(source) (source$family %||% "") %in% families && !((source$id %||% "") %in% ids), admin_pit_explorer_registry(root))
  if (!is.null(countries) && length(countries)) {
    countries <- toupper(as.character(countries))
    sources <- Filter(function(source) toupper(source$country %||% "") %in% countries, sources)
  }
  rows <- lapply(sources, function(source) {
    data.frame(
      source_id = source$id %||% "",
      family = source$family %||% "",
      country = source$country %||% "",
      workflow_status = "unsupported_in_admin_pit_v1",
      notes = paste(admin_pit_explorer_source_values(source$notes), collapse = " "),
      stringsAsFactors = FALSE
    )
  })
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_destination_for_incoming <- function(source, incoming) {
  destinations <- unique(admin_pit_explorer_source_values(source$destination %||% source$destinations))
  if (!length(destinations)) {
    return(list(status = "missing_destination", destination = ""))
  }
  if (length(destinations) > 1L) {
    return(list(status = "ambiguous_destination", destination = paste(destinations, collapse = ", ")))
  }
  list(
    status = "ready",
    destination = admin_pit_explorer_template(destinations[[1L]], list(
      source = source$id %||% "",
      basename = basename(incoming),
      incoming = incoming
    ))
  )
}

admin_pit_explorer_parse_years <- function(path) {
  matches <- gregexpr("(19|20)[0-9]{2}", basename(as.character(path)), perl = TRUE)
  years <- unique(as.integer(unlist(regmatches(basename(as.character(path)), matches))))
  years[!is.na(years)]
}

admin_pit_explorer_expand_year_span <- function(years, max_span = 35L) {
  years <- sort(unique(as.integer(years[!is.na(years)])))
  if (length(years) < 2L) return(years)
  span <- max(years) - min(years)
  if (span > 0L && span <= as.integer(max_span)) {
    return(seq.int(min(years), max(years)))
  }
  years
}

admin_pit_explorer_col_years <- function(path, rule) {
  span <- admin_pit_explorer_parse_years(path)
  if (length(span) >= 2L) {
    return(admin_pit_explorer_expand_year_span(span, max_span = 35L))
  }
  integer()
}

admin_pit_explorer_workbook_years <- function(path, rule) {
  spec <- rule$workbook_years %||% list()
  sheet <- spec$sheet %||% "Datos"
  range <- spec$range %||% "A8:A5000"
  ext <- tolower(tools::file_ext(path))
  values <- tryCatch({
    if (identical(ext, "xlsb")) {
      if (!admin_pit_explorer_has("readxlsb")) return(integer())
      readxlsb::read_xlsb(path, sheet = sheet, range = range, col_names = TRUE)
    } else {
      if (!admin_pit_explorer_has("readxl")) return(integer())
      readxl::read_excel(path, sheet = sheet, range = range, col_names = TRUE)
    }
  }, error = function(e) NULL)
  if (is.null(values)) return(integer())
  years <- suppressWarnings(as.integer(unlist(values, use.names = FALSE)))
  min_year <- as.integer(spec$min %||% 1900L)
  max_year <- as.integer(spec$max %||% 2099L)
  sort(unique(years[!is.na(years) & years >= min_year & years <= max_year]))
}

admin_pit_explorer_years_for_path <- function(source_id, path, rule) {
  if (!file.exists(path)) return(integer())
  if (identical(source_id, "col-pit-total")) {
    return(admin_pit_explorer_col_years(path, rule))
  }
  workbook_years <- admin_pit_explorer_workbook_years(path, rule)
  if (length(workbook_years)) return(workbook_years)
  max_span <- as.integer(rule$filename_year_span_max %||% 2L)
  admin_pit_explorer_expand_year_span(admin_pit_explorer_parse_years(path), max_span = max_span)
}

admin_pit_explorer_excel_sheet_status <- function(path, expected_sheets) {
  ext <- tolower(tools::file_ext(path))
  sheets <- tryCatch({
    if (identical(ext, "xlsb")) {
      if (!admin_pit_explorer_has("readxlsb")) {
        return(list(status = "structure_review_needed", evidence = "readxlsb_not_installed"))
      }
      readxlsb::excel_sheets(path)
    } else {
      if (!admin_pit_explorer_has("readxl")) {
        return(list(status = "structure_review_needed", evidence = "readxl_not_installed"))
      }
      readxl::excel_sheets(path)
    }
  }, error = function(e) return(structure(conditionMessage(e), class = "admin_pit_error")))
  if (inherits(sheets, "admin_pit_error")) {
    return(list(status = "structure_review_needed", evidence = paste0("sheet_read_error:", as.character(sheets))))
  }
  missing <- setdiff(expected_sheets, sheets)
  if (length(missing)) {
    list(status = "blocked_structure_mismatch", evidence = paste("missing_sheets", paste(missing, collapse = ","), sep = ":"))
  } else {
    list(status = "structure_evidence_available", evidence = paste("sheets", paste(expected_sheets, collapse = ","), sep = ":"))
  }
}

admin_pit_explorer_bra_expected_sheet <- function(year, rule) {
  sheets <- rule$sheets %||% list()
  for (item in sheets) {
    years <- item$years %||% list()
    min_year <- if (is.null(years$min)) -Inf else as.integer(years$min)
    max_year <- if (is.null(years$max)) Inf else as.integer(years$max)
    if (year >= min_year && year <= max_year) return(item$sheet %||% "")
  }
  ""
}

admin_pit_explorer_structure_for_path <- function(source_id, path, years, rule) {
  if (!file.exists(path)) {
    return(list(status = "no_file", evidence = "source_not_present"))
  }
  if (identical(source_id, "chl-pit-total")) {
    return(admin_pit_explorer_excel_sheet_status(path, as.character(rule$expected_sheets %||% "Datos")))
  }
  if (identical(source_id, "bra-pit-total")) {
    if (!length(years)) {
      return(list(status = "structure_review_needed", evidence = "year_not_detected"))
    }
    expected <- admin_pit_explorer_bra_expected_sheet(max(years), rule)
    if (!nzchar(expected)) {
      return(list(status = "blocked_structure_mismatch", evidence = paste0("unsupported_layout_year:", max(years))))
    }
    sheet <- admin_pit_explorer_excel_sheet_status(path, expected)
    sheet$evidence <- paste(sheet$evidence, paste0("layout_year:", max(years)), sep = ";")
    return(sheet)
  }
  if (identical(source_id, "col-pit-total")) {
    if (!dir.exists(path) || !length(years)) {
      return(list(status = "blocked_structure_mismatch", evidence = "folder_span_missing"))
    }
    folder <- file.path(path, basename(path))
    if (!dir.exists(folder)) folder <- path
    first <- as.integer(rule$first_year %||% min(years))
    offset <- as.integer(rule$index_offset %||% 2007L)
    template <- rule$expected_file_template %||% "{index}_Cuantiles_Ingreso_Bruto_Naturales_{year}_F-210.xlsx"
    expected_years <- years[years >= first]
    missing <- character()
    for (year in expected_years) {
      rel <- admin_pit_explorer_template(template, list(year = year, index = year - offset))
      if (!file.exists(file.path(folder, rel))) missing <- c(missing, rel)
    }
    if (length(missing)) {
      return(list(status = "blocked_structure_mismatch", evidence = paste("missing_expected_files", paste(missing, collapse = ","), sep = ":")))
    }
    return(list(status = "structure_evidence_available", evidence = paste0("folder_years:", paste(expected_years, collapse = ","))))
  }
  list(status = "structure_review_needed", evidence = "unsupported_source_check")
}

admin_pit_explorer_source_row <- function(source, source_set, path, root, rule, status = "matched") {
  source_id <- source$id %||% ""
  years <- admin_pit_explorer_years_for_path(source_id, path, rule)
  destination <- list(status = "", destination = "")
  if (identical(source_set, "new") && file.exists(path)) {
    destination <- admin_pit_explorer_destination_for_incoming(source, basename(path))
  }
  file_kind <- if (!file.exists(path)) "missing" else if (dir.exists(path)) "dir" else "file"
  data.frame(
    source_id = source_id,
    family = source$family %||% "",
    country = toupper(source$country %||% ""),
    source_set = source_set,
    file = if (nzchar(path)) normalizePath(path, mustWork = FALSE) else "",
    rel = if (nzchar(path)) admin_pit_explorer_relative_path(path, root) else "",
    kind = file_kind,
    status = status,
    years = paste(years, collapse = ","),
    year_start = if (length(years)) min(years) else NA_integer_,
    year_end = if (length(years)) max(years) else NA_integer_,
    destination = destination$destination %||% "",
    destination_status = destination$status %||% "",
    stringsAsFactors = FALSE
  )
}

admin_pit_explorer_source_inventory <- function(sources, contract, root) {
  rules <- contract$source_discovery$structure_checks %||% list()
  rows <- list()
  for (source in sources) {
    source_id <- source$id %||% ""
    rule <- rules[[source_id]] %||% list()
    for (source_set in c("old", "new")) {
      field <- if (identical(source_set, "old")) "canonical" else "inbox"
      patterns <- admin_pit_explorer_source_values(source[[field]])
      paths <- unique(unlist(lapply(patterns, function(pattern) {
        Sys.glob(admin_pit_explorer_path(pattern, root))
      }), use.names = FALSE))
      paths <- sort(paths[file.exists(paths)])
      if (!length(paths)) {
        rows[[length(rows) + 1L]] <- admin_pit_explorer_source_row(source, source_set, "", root, rule, status = "no_file")
      } else {
        for (path in paths) {
          rows[[length(rows) + 1L]] <- admin_pit_explorer_source_row(source, source_set, path, root, rule, status = "matched")
        }
      }
    }
  }
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_existing_glob <- function(patterns, root) {
  patterns <- admin_pit_explorer_source_values(patterns)
  if (!length(patterns)) return(character())
  paths <- unique(unlist(lapply(patterns, function(pattern) Sys.glob(admin_pit_explorer_path(pattern, root))), use.names = FALSE))
  paths <- as.character(paths %||% character())
  sort(paths[file.exists(paths)])
}

admin_pit_explorer_aux_destination <- function(source, path) {
  destinations <- unique(admin_pit_explorer_source_values(source$destination %||% source$destinations))
  if (!length(destinations)) return("")
  admin_pit_explorer_template(destinations[[1L]], list(source = source$id %||% "", basename = basename(path), incoming = path))
}

admin_pit_explorer_csv_years <- function(path, year_col = "year") {
  if (!file.exists(path) || dir.exists(path)) return(integer())
  data <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(data) || !nrow(data)) return(integer())
  names(data) <- tolower(names(data))
  if (!(tolower(year_col) %in% names(data))) return(integer())
  years <- suppressWarnings(as.integer(data[[tolower(year_col)]]))
  sort(unique(years[!is.na(years)]))
}

admin_pit_explorer_aux_years <- function(aux_id, path) {
  if (identical(aux_id, "bra-minwage")) {
    return(admin_pit_explorer_csv_years(path, "year"))
  }
  admin_pit_explorer_csv_years(path, "year")
}

admin_pit_explorer_aux_required_years <- function(aux_id, years) {
  if (identical(aux_id, "bra-minwage")) {
    last <- max(as.integer(years), na.rm = TRUE)
    if (is.infinite(last) || is.na(last) || last < 2007L) return(integer())
    return(seq.int(2007L, last))
  }
  integer()
}

admin_pit_explorer_fetch_command <- function(source, aux_id) {
  if (is.null(source)) return("")
  if (nzchar(source$fetcher %||% "") || identical(aux_id, "bra-minwage")) {
    return(sprintf("dina sources fetch %s", aux_id))
  }
  ""
}

admin_pit_explorer_aux_row <- function(source_id, country, aux_id, artifact_class, source_set, from_rel, to_rel, status, severity, years, required_years, next_command = "", detail = "") {
  data.frame(
    source_id = source_id,
    country = country,
    dependency_id = aux_id,
    artifact_class = artifact_class,
    source_set = source_set,
    from_rel = from_rel,
    to_rel = to_rel,
    status = status,
    severity = severity,
    years = paste(years, collapse = ","),
    year_start = if (length(years)) min(years) else NA_integer_,
    year_end = if (length(years)) max(years) else NA_integer_,
    required_years = paste(required_years, collapse = ","),
    required_year_start = if (length(required_years)) min(required_years) else NA_integer_,
    required_year_end = if (length(required_years)) max(required_years) else NA_integer_,
    next_command = next_command,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

admin_pit_explorer_aux_dependency_detail <- function(root, include_contract, source_ids, years) {
  aux <- include_contract$cleaners$auxiliary_sources %||% list()
  rows <- list()
  for (source_id in intersect(names(aux), source_ids)) {
    country <- admin_pit_explorer_source_country(source_id)
    for (aux_id in as.character(aux[[source_id]] %||% character())) {
      source <- admin_pit_explorer_registry_source(root, aux_id)
      required_years <- admin_pit_explorer_aux_required_years(aux_id, years)
      if (is.null(source)) {
        rows[[length(rows) + 1L]] <- admin_pit_explorer_aux_row(source_id, country, aux_id, "aux_source", "missing", "", "", "missing_aux_registry", "warning", integer(), required_years, "", "Aux source is not configured in config/sources.yml.")
        next
      }
      inbox <- admin_pit_explorer_existing_glob(source$inbox, root)
      canonical <- admin_pit_explorer_existing_glob(source$canonical, root)
      fetch <- admin_pit_explorer_fetch_command(source, aux_id)
      if (length(inbox)) {
        for (path in inbox) {
          aux_years <- admin_pit_explorer_aux_years(aux_id, path)
          rows[[length(rows) + 1L]] <- admin_pit_explorer_aux_row(
            source_id,
            country,
            aux_id,
            "aux_source",
            "new",
            admin_pit_explorer_relative_path(path, root),
            admin_pit_explorer_aux_destination(source, basename(path)),
            "aux_candidate",
            "info",
            aux_years,
            required_years,
            "",
            "Candidate aux source found in _new; include will validate append-only overlap."
          )
        }
        next
      }
      if (length(canonical)) {
        for (path in canonical) {
          aux_years <- admin_pit_explorer_aux_years(aux_id, path)
          missing_required <- setdiff(required_years, aux_years)
          blocked <- length(missing_required) > 0L
          rows[[length(rows) + 1L]] <- admin_pit_explorer_aux_row(
            source_id,
            country,
            aux_id,
            "aux_source",
            "canonical",
            admin_pit_explorer_relative_path(path, root),
            admin_pit_explorer_relative_path(path, root),
            if (blocked) "blocked_aux_missing_years" else "carried_forward_aux",
            if (blocked) "blocked" else "info",
            aux_years,
            required_years,
            if (blocked) fetch else "",
            if (blocked) {
              paste("Canonical aux source is missing required years:", paste(missing_required, collapse = ","))
            } else {
              "Canonical aux source covers required years and can be carried forward."
            }
          )
        }
        next
      }
      missing_patterns <- paste(c(admin_pit_explorer_source_values(source$inbox), admin_pit_explorer_source_values(source$canonical)), collapse = ", ")
      rows[[length(rows) + 1L]] <- admin_pit_explorer_aux_row(
        source_id,
        country,
        aux_id,
        "aux_source",
        "missing",
        missing_patterns,
        "",
        if (identical(aux_id, "chl-uta")) "legacy_live_aux" else "blocked_missing_aux",
        if (identical(aux_id, "chl-uta")) "warning" else "blocked",
        integer(),
        required_years,
        if (identical(aux_id, "chl-uta")) "" else fetch,
        if (identical(aux_id, "chl-uta")) {
          "No materialized UTA aux source is configured; current legacy live behavior is noted for review."
        } else {
          "Aux source is missing in both _new and canonical paths."
        }
      )
    }
  }
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_source_country <- function(source_id) {
  switch(source_id, "chl-pit-total" = "CHL", "bra-pit-total" = "BRA", "col-pit-total" = "COL", "")
}

admin_pit_explorer_aux_dependency_summary <- function(detail) {
  if (!nrow(detail)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(detail[, c("source_id", "country", "dependency_id", "artifact_class"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    rows <- detail[
      detail$source_id == key$source_id & detail$country == key$country & detail$dependency_id == key$dependency_id,
      ,
      drop = FALSE
    ]
    severity <- if (any(rows$severity == "blocked", na.rm = TRUE)) {
      "blocked"
    } else if (any(rows$severity == "warning", na.rm = TRUE)) {
      "warning"
    } else {
      "info"
    }
    status <- rows$status[[which.max(match(rows$severity, c("info", "warning", "blocked"), nomatch = 0L))]]
    years <- sort(unique(suppressWarnings(as.integer(unlist(strsplit(paste(rows$years, collapse = ","), "[^0-9]+"))))))
    years <- years[!is.na(years)]
    required <- sort(unique(suppressWarnings(as.integer(unlist(strsplit(paste(rows$required_years, collapse = ","), "[^0-9]+"))))))
    required <- required[!is.na(required)]
    command <- rows$next_command[nzchar(rows$next_command %||% "")]
    data.frame(
      source_id = key$source_id,
      country = key$country,
      dependency_id = key$dependency_id,
      artifact_class = key$artifact_class,
      status = status,
      severity = severity,
      available_years = paste(years, collapse = ","),
      required_years = paste(required, collapse = ","),
      next_command = if (length(command)) command[[1L]] else "",
      detail = paste(unique(rows$detail[nzchar(rows$detail %||% "")]), collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_static_dependency_report <- function(root, include_contract, source_ids) {
  deps <- include_contract$cleaners$static_dependencies %||% list()
  rows <- list()
  for (source_id in intersect(names(deps), source_ids)) {
    country <- admin_pit_explorer_source_country(source_id)
    for (pattern in unique(as.character(deps[[source_id]] %||% character()))) {
      matches <- admin_pit_explorer_existing_glob(pattern, root)
      if (!length(matches)) {
        rows[[length(rows) + 1L]] <- data.frame(
          source_id = source_id,
          country = country,
          dependency_id = pattern,
          artifact_class = "static_dependency",
          from_rel = pattern,
          status = "missing_static_dependency",
          severity = "blocked",
          next_command = "",
          detail = "Required carry-forward dependency is missing from canonical paths.",
          stringsAsFactors = FALSE
        )
      } else {
        for (path in matches) {
          rows[[length(rows) + 1L]] <- data.frame(
            source_id = source_id,
            country = country,
            dependency_id = pattern,
            artifact_class = "static_dependency",
            from_rel = admin_pit_explorer_relative_path(path, root),
            status = "available_static_dependency",
            severity = "info",
            next_command = "",
            detail = "Static dependency is available for carry-forward staging.",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_dependency_actions <- function(aux_summary, static_report) {
  rows <- list()
  if (nrow(aux_summary)) {
    needs_action <- aux_summary[aux_summary$severity %in% c("blocked", "warning") | nzchar(aux_summary$next_command %||% ""), , drop = FALSE]
    if (nrow(needs_action)) {
      rows[[length(rows) + 1L]] <- data.frame(
        source_id = needs_action$source_id,
        country = needs_action$country,
        dependency_id = needs_action$dependency_id,
        artifact_class = needs_action$artifact_class,
        status = needs_action$status,
        action = ifelse(nzchar(needs_action$next_command %||% ""), "fetch_aux_source", "review_aux_dependency"),
        next_command = needs_action$next_command,
        detail = needs_action$detail,
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(static_report)) {
    blocked <- static_report[static_report$severity == "blocked", , drop = FALSE]
    if (nrow(blocked)) {
      rows[[length(rows) + 1L]] <- data.frame(
        source_id = blocked$source_id,
        country = blocked$country,
        dependency_id = blocked$dependency_id,
        artifact_class = blocked$artifact_class,
        status = blocked$status,
        action = "provide_static_dependency",
        next_command = "",
        detail = blocked$detail,
        stringsAsFactors = FALSE
      )
    }
  }
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_structure_summary <- function(source_inventory, contract) {
  rules <- contract$source_discovery$structure_checks %||% list()
  keys <- unique(source_inventory[, c("source_id", "country"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    detail <- source_inventory[source_inventory$source_id == key$source_id & source_inventory$country == key$country & source_inventory$source_set == "new", , drop = FALSE]
    matched <- detail[detail$status == "matched", , drop = FALSE]
    if (!nrow(matched)) {
      status <- "blocked_structure_mismatch"
      evidence <- "no_incoming_source"
    } else {
      checks <- lapply(seq_len(nrow(matched)), function(j) {
        row <- matched[j, , drop = FALSE]
        years <- suppressWarnings(as.integer(unlist(strsplit(row$years, "[^0-9]+"))))
        years <- years[!is.na(years)]
        admin_pit_explorer_structure_for_path(row$source_id, row$file, years, rules[[row$source_id]] %||% list())
      })
      statuses <- vapply(checks, `[[`, character(1), "status")
      evidence <- paste(vapply(checks, `[[`, character(1), "evidence"), collapse = ";")
      status <- if (any(statuses == "blocked_structure_mismatch")) {
        "blocked_structure_mismatch"
      } else if (any(statuses == "structure_review_needed")) {
        "structure_review_needed"
      } else {
        "structure_evidence_available"
      }
    }
    data.frame(
      source_id = key$source_id,
      country = key$country,
      structure_status = status,
      structure_evidence = evidence,
      stringsAsFactors = FALSE
    )
  })
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_extension_summary <- function(source_inventory) {
  keys <- unique(source_inventory[, c("source_id", "country"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    old <- source_inventory[source_inventory$source_id == key$source_id & source_inventory$country == key$country & source_inventory$source_set == "old" & source_inventory$status == "matched", , drop = FALSE]
    new <- source_inventory[source_inventory$source_id == key$source_id & source_inventory$country == key$country & source_inventory$source_set == "new" & source_inventory$status == "matched", , drop = FALSE]
    old_years <- sort(unique(suppressWarnings(as.integer(unlist(strsplit(paste(old$years, collapse = ","), "[^0-9]+"))))))
    new_years <- sort(unique(suppressWarnings(as.integer(unlist(strsplit(paste(new$years, collapse = ","), "[^0-9]+"))))))
    old_years <- old_years[!is.na(old_years)]
    new_years <- new_years[!is.na(new_years)]
    extension <- setdiff(new_years, old_years)
    overlap <- intersect(new_years, old_years)
    data.frame(
      source_id = key$source_id,
      country = key$country,
      old_years = paste(old_years, collapse = ","),
      new_years = paste(new_years, collapse = ","),
      extension_years = paste(extension, collapse = ","),
      overlap_years = paste(overlap, collapse = ","),
      status = if (!length(new_years)) "blocked_missing_new" else if (length(extension)) "extension" else "overlap_only",
      stringsAsFactors = FALSE
    )
  })
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_year_expectations <- function(source_inventory, structure_summary) {
  new_rows <- source_inventory[source_inventory$source_set == "new" & source_inventory$status == "matched", , drop = FALSE]
  rows <- lapply(seq_len(nrow(new_rows)), function(i) {
    row <- new_rows[i, , drop = FALSE]
    years <- sort(unique(suppressWarnings(as.integer(unlist(strsplit(row$years, "[^0-9]+"))))))
    years <- years[!is.na(years)]
    if (!length(years)) years <- NA_integer_
    structure <- structure_summary[structure_summary$source_id == row$source_id & structure_summary$country == row$country, , drop = FALSE]
    data.frame(
      source_id = row$source_id,
      country = row$country,
      year = years,
      year_role = "incoming_coverage",
      structure_status = if (nrow(structure)) structure$structure_status[[1L]] else "",
      stringsAsFactors = FALSE
    )
  })
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_review_actions <- function(extension_summary, structure_summary) {
  keys <- unique(extension_summary[, c("source_id", "country"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    ext <- extension_summary[extension_summary$source_id == key$source_id & extension_summary$country == key$country, , drop = FALSE]
    structure <- structure_summary[structure_summary$source_id == key$source_id & structure_summary$country == key$country, , drop = FALSE]
    action <- if (nrow(structure) && identical(structure$structure_status[[1L]], "blocked_structure_mismatch")) {
      "blocked_review_source"
    } else if (nrow(ext) && nzchar(ext$extension_years[[1L]] %||% "")) {
      "candidate_extension"
    } else {
      "review_overlap"
    }
    data.frame(source_id = key$source_id, country = key$country, action = action, stringsAsFactors = FALSE)
  })
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_hash_path <- function(path) {
  if (!file.exists(path) || !admin_pit_explorer_has("digest")) return(NA_character_)
  if (!dir.exists(path)) return(digest::digest(file = path, algo = "sha256"))
  files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) return(digest::digest("", algo = "sha256"))
  rel <- substring(files, nchar(normalizePath(path, mustWork = FALSE)) + 2L)
  lines <- sprintf("%s %s", rel, vapply(files, function(file) digest::digest(file = file, algo = "sha256"), character(1)))
  digest::digest(paste(sort(lines), collapse = "\n"), algo = "sha256")
}

admin_pit_explorer_source_fingerprints <- function(source_inventory) {
  matched <- source_inventory[source_inventory$status == "matched" & nzchar(source_inventory$file), , drop = FALSE]
  rows <- lapply(seq_len(nrow(matched)), function(i) {
    row <- matched[i, , drop = FALSE]
    info <- file.info(row$file)
    data.frame(
      source_id = row$source_id,
      source_set = row$source_set,
      rel = row$rel,
      kind = row$kind,
      size = as.numeric(info$size[[1L]]),
      mtime = as.character(info$mtime[[1L]]),
      sha256 = admin_pit_explorer_hash_path(row$file),
      stringsAsFactors = FALSE
    )
  })
  admin_pit_explorer_bind(rows)
}

admin_pit_explorer_manifest <- function(contract, years, status) {
  config <- attr(years, "config")
  data.frame(
    key = c("source_type", "workflow", "status", "year_first", "year_last", "supported_source_ids"),
    value = c(
      "admin",
      "admin_pit",
      status,
      min(years),
      max(years),
      paste(admin_pit_explorer_supported_ids(contract), collapse = ",")
    ),
    stringsAsFactors = FALSE
  )
}

admin_pit_explorer_write_outputs <- function(outputs, paths, manifest) {
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  for (name in names(outputs)) {
    utils::write.csv(outputs[[name]], file.path(paths$tables, paste0(name, ".csv")), row.names = FALSE, na = "")
  }
  utils::write.csv(manifest, file.path(paths$logs, "explore_manifest.csv"), row.names = FALSE, na = "")
  utils::write.csv(manifest, file.path(paths$tables, "explore_manifest.csv"), row.names = FALSE, na = "")
  invisible(paths)
}

run_admin_pit_explorer <- function(
  root = admin_pit_explorer_repo_root(),
  contract_path = file.path(root, "config", "admin_pit_explorer.yml"),
  output_dir = NULL,
  countries = NULL,
  write_outputs = TRUE
) {
  contract <- admin_pit_explorer_read_contract(root, contract_path)
  years <- admin_pit_explorer_years(contract, root)
  sources <- admin_pit_explorer_supported_sources(contract, root, countries)
  source_ids <- unique(vapply(sources, function(source) source$id %||% "", character(1)))
  unsupported <- admin_pit_explorer_unsupported_sources(contract, root, countries)
  inventory <- admin_pit_explorer_source_inventory(sources, contract, root)
  structure <- admin_pit_explorer_structure_summary(inventory, contract)
  extension <- admin_pit_explorer_extension_summary(inventory)
  expectations <- admin_pit_explorer_year_expectations(inventory, structure)
  actions <- admin_pit_explorer_review_actions(extension, structure)
  fingerprints <- admin_pit_explorer_source_fingerprints(inventory)
  include_contract <- admin_pit_explorer_read_include_contract(root)
  aux_detail <- admin_pit_explorer_aux_dependency_detail(root, include_contract, source_ids, years)
  aux_summary <- admin_pit_explorer_aux_dependency_summary(aux_detail)
  static_report <- admin_pit_explorer_static_dependency_report(root, include_contract, source_ids)
  dependency_actions <- admin_pit_explorer_dependency_actions(aux_summary, static_report)
  blocked <- (nrow(structure) && any(structure$structure_status == "blocked_structure_mismatch", na.rm = TRUE)) ||
    (nrow(aux_summary) && any(aux_summary$severity == "blocked", na.rm = TRUE)) ||
    (nrow(static_report) && any(static_report$severity == "blocked", na.rm = TRUE))
  warning <- (nrow(structure) && any(structure$structure_status == "structure_review_needed", na.rm = TRUE)) ||
    (nrow(aux_summary) && any(aux_summary$severity == "warning", na.rm = TRUE)) ||
    (nrow(static_report) && any(static_report$severity == "warning", na.rm = TRUE))
  status <- if (blocked) "blocked" else if (warning) "check_following" else "all_good"
  manifest <- admin_pit_explorer_manifest(contract, years, status)
  paths <- admin_pit_explorer_output_paths(root, contract, output_dir)
  outputs <- list(
    source_inventory = inventory,
    structure_summary = structure,
    extension_summary = extension,
    year_expectations = expectations,
    review_actions = actions,
    unsupported_sources = unsupported,
    source_fingerprints = fingerprints,
    aux_dependency_summary = aux_summary,
    aux_dependency_detail = aux_detail,
    static_dependency_report = static_report,
    dependency_actions = dependency_actions
  )
  if (isTRUE(write_outputs)) {
    admin_pit_explorer_write_outputs(outputs, paths, manifest)
  }
  list(paths = paths, outputs = outputs, manifest = manifest, contract = contract)
}
