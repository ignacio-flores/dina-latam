# Experimental WID source explore/include workflow.
#
# This workflow is deliberately narrow in v1: it validates and promotes the
# registry source `population`, which is fetched from WID npopul into `_new`.
# Explore writes review tables, include dry-run stages the incoming DTA under a
# run directory, confirm backs up and promotes the canonical file, and restore
# rolls back from the confirm snapshot.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

wid_include_repo_root <- function(start = getwd()) {
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

wid_include_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

wid_include_has <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

wid_include_path <- function(path, root) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  if (grepl("^/", path)) path else file.path(root, path)
}

wid_include_relative_path <- function(path, root) {
  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

wid_include_source_values <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character())
  if (is.atomic(x)) return(as.character(x))
  unlist(x, use.names = FALSE)
}

wid_include_read_yaml <- function(path, default = NULL) {
  wid_include_need("yaml")
  if (!file.exists(path)) {
    if (!is.null(default)) return(default)
    stop("Missing YAML file: ", path, call. = FALSE)
  }
  yaml::read_yaml(path)
}

wid_include_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame(stringsAsFactors = FALSE))
  lines <- readLines(path, warn = FALSE, n = 5L)
  if (!length(lines) || all(!nzchar(trimws(lines)))) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA")),
    error = function(e) {
      if (grepl("first five rows are empty", conditionMessage(e), fixed = TRUE)) {
        return(data.frame(stringsAsFactors = FALSE))
      }
      stop(e)
    }
  )
}

wid_include_bind <- function(...) {
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

wid_include_empty_mappings <- function() {
  data.frame(
    source_id = character(),
    source_set = character(),
    from_rel = character(),
    to_rel = character(),
    staged_to = character(),
    validation_status = character(),
    copy_status = character(),
    stringsAsFactors = FALSE
  )
}

wid_include_empty_fingerprints <- function() {
  data.frame(
    source_id = character(),
    source_set = character(),
    rel = character(),
    exists = logical(),
    kind = character(),
    size = numeric(),
    mtime = character(),
    hash_algorithm = character(),
    hash = character(),
    stringsAsFactors = FALSE
  )
}

wid_include_empty_promotion_plan <- function() {
  data.frame(
    source_id = character(),
    artifact_type = character(),
    from_rel = character(),
    to_rel = character(),
    promotion_scope = character(),
    stringsAsFactors = FALSE
  )
}

wid_include_empty_promotion_fingerprints <- function() {
  data.frame(
    source_id = character(),
    artifact_type = character(),
    from_rel = character(),
    to_rel = character(),
    exists = logical(),
    kind = character(),
    hash_algorithm = character(),
    hash = character(),
    stringsAsFactors = FALSE
  )
}

wid_include_write_csvs <- function(tables, paths, manifest_name) {
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  for (name in names(tables)) {
    utils::write.csv(tables[[name]], file.path(paths$tables, paste0(name, ".csv")), row.names = FALSE, na = "")
  }
  if (!is.null(tables[[manifest_name]])) {
    utils::write.csv(tables[[manifest_name]], file.path(paths$logs, paste0(manifest_name, ".csv")), row.names = FALSE, na = "")
  }
  invisible(paths)
}

wid_include_read_contract <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml")
) {
  contract <- wid_include_read_yaml(contract_path)
  required <- c("source_ids", "explore_output_root", "output_root", "years", "schema")
  missing <- setdiff(required, names(contract))
  if (length(missing)) {
    stop("Invalid WID include contract; missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  contract$contract_path <- contract_path
  contract
}

wid_include_supported_ids <- function(contract) {
  as.character(contract$source_ids %||% character())
}

wid_include_registry <- function(root) {
  config <- wid_include_read_yaml(file.path(root, "config", "sources.yml"), default = list(sources = list()))
  config$sources %||% list()
}

wid_include_registry_source <- function(root, source_id) {
  registry <- wid_include_registry(root)
  matches <- Filter(function(source) identical(source$id %||% "", source_id), registry)
  if (!length(matches)) NULL else matches[[1L]]
}

wid_include_registry_family <- function(source) {
  tolower(gsub("-", "_", as.character(source$family %||% "")))
}

wid_include_registry_wid_sources <- function(root) {
  registry <- wid_include_registry(root)
  Filter(function(source) identical(wid_include_registry_family(source), "wid"), registry)
}

wid_include_schema <- function(contract, source_id) {
  schema <- (contract$schema %||% list())[[source_id]]
  if (is.null(schema)) {
    stop("WID include contract has no schema for source: ", source_id, call. = FALSE)
  }
  list(
    required_columns = as.character(schema$required_columns %||% character()),
    key_columns = as.character(schema$key_columns %||% character()),
    numeric_columns = as.character(schema$numeric_columns %||% character())
  )
}

wid_include_config_years <- function(root, contract) {
  years <- contract$years %||% list()
  first <- suppressWarnings(as.integer(years$first %||% 1990L))
  config_path <- years$from_config %||% "config/dina.yml"
  config <- wid_include_read_yaml(wid_include_path(config_path, root))
  last <- suppressWarnings(as.integer(config$years$last))
  if (is.na(first) || is.na(last) || first > last) {
    stop("Invalid WID required year range in contract/config.", call. = FALSE)
  }
  seq.int(first, last)
}

wid_include_output_root <- function(root, contract, output_dir = NULL, explore = FALSE) {
  rel <- output_dir %||% if (isTRUE(explore)) contract$explore_output_root else contract$output_root
  wid_include_path(rel, root)
}

wid_include_explore_paths <- function(root, contract, output_dir = NULL) {
  out <- wid_include_output_root(root, contract, output_dir, explore = TRUE)
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"))
}

wid_include_run_id <- function(prefix = "wid-include") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

wid_include_confirm_id <- function(prefix = "wid-confirm") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

wid_include_output_paths_for_run <- function(root, contract, output_dir = NULL, run_id = NULL) {
  base <- wid_include_output_root(root, contract, output_dir, explore = FALSE)
  out <- if (is.null(run_id) || !nzchar(run_id)) base else file.path(base, "runs", run_id)
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"), staged_repo = file.path(out, "staged_repo"))
}

wid_include_output_paths_for_confirm <- function(root, contract, output_dir = NULL, confirm_id = NULL) {
  base <- wid_include_output_root(root, contract, output_dir, explore = FALSE)
  out <- file.path(base, "confirms", confirm_id %||% wid_include_confirm_id())
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"), snapshots = file.path(out, "snapshots"))
}

wid_include_destination <- function(source, incoming_rel = "") {
  destination <- wid_include_source_values(source$destination)
  if (length(destination) && nzchar(destination[[1L]])) {
    return(gsub("\\{basename\\}", basename(incoming_rel), destination[[1L]]))
  }
  canonical <- wid_include_source_values(source$canonical)
  if (length(canonical)) canonical[[1L]] else ""
}

wid_include_source_paths <- function(root, source) {
  canonical <- wid_include_source_values(source$canonical)
  inbox <- wid_include_source_values(source$inbox)
  fetch_target <- wid_include_source_values(source$fetch_target)
  incoming_rel <- if (length(fetch_target) && nzchar(fetch_target[[1L]])) fetch_target[[1L]] else if (length(inbox)) inbox[[1L]] else ""
  current_rel <- if (length(canonical)) canonical[[1L]] else ""
  list(
    current_rel = current_rel,
    incoming_rel = incoming_rel,
    destination = wid_include_destination(source, incoming_rel),
    current = wid_include_path(current_rel, root),
    incoming = wid_include_path(incoming_rel, root)
  )
}

wid_include_read_dta <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    return(list(ok = FALSE, data = data.frame(stringsAsFactors = FALSE), error = "file_not_found"))
  }
  wid_include_need("haven")
  data <- tryCatch(as.data.frame(haven::read_dta(path), stringsAsFactors = FALSE), error = function(e) e)
  if (inherits(data, "error")) {
    return(list(ok = FALSE, data = data.frame(stringsAsFactors = FALSE), error = conditionMessage(data)))
  }
  list(ok = TRUE, data = data, error = "")
}

wid_include_normalize_population <- function(data, schema) {
  out <- data
  if ("country" %in% names(out)) out$country <- trimws(as.character(out$country))
  if ("region" %in% names(out)) out$region <- trimws(as.character(out$region))
  if ("year" %in% names(out)) out$year <- suppressWarnings(as.integer(out$year))
  for (col in schema$numeric_columns) {
    if (col %in% names(out)) out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
  }
  out
}

wid_include_year_label <- function(years) {
  years <- sort(unique(as.integer(years[!is.na(years)])))
  if (!length(years)) return("")
  breaks <- c(TRUE, diff(years) != 1L)
  groups <- split(years, cumsum(breaks))
  paste(vapply(groups, function(group) {
    if (length(group) == 1L) as.character(group[[1L]]) else sprintf("%s-%s", group[[1L]], group[[length(group)]])
  }, character(1)), collapse = ",")
}

wid_include_dataset_status <- function(source_id, source_set, path, rel, destination, data, read_ok, read_error, schema, required_years) {
  exists <- file.exists(path) && !dir.exists(path)
  if (!exists || !isTRUE(read_ok)) {
    status <- if (identical(source_set, "incoming")) "missing_incoming_source" else "missing_current_source"
    if (exists && nzchar(read_error %||% "")) status <- "read_failed"
    return(data.frame(
      source_id = source_id,
      source_set = source_set,
      file = path,
      rel = rel,
      destination = destination,
      exists = exists,
      status = status,
      rows = 0L,
      countries = 0L,
      first_year = NA_integer_,
      last_year = NA_integer_,
      missing_required_columns = "",
      duplicate_keys = 0L,
      missing_value_rows = 0L,
      missing_required_year_count = length(required_years),
      missing_required_years = wid_include_year_label(required_years),
      read_error = read_error %||% "",
      stringsAsFactors = FALSE
    ))
  }

  missing_cols <- setdiff(schema$required_columns, names(data))
  if (length(missing_cols)) {
    return(data.frame(
      source_id = source_id,
      source_set = source_set,
      file = path,
      rel = rel,
      destination = destination,
      exists = TRUE,
      status = "schema_mismatch",
      rows = nrow(data),
      countries = if ("country" %in% names(data)) length(unique(trimws(as.character(data$country)))) else 0L,
      first_year = if ("year" %in% names(data)) suppressWarnings(min(as.integer(data$year), na.rm = TRUE)) else NA_integer_,
      last_year = if ("year" %in% names(data)) suppressWarnings(max(as.integer(data$year), na.rm = TRUE)) else NA_integer_,
      missing_required_columns = paste(missing_cols, collapse = ","),
      duplicate_keys = 0L,
      missing_value_rows = 0L,
      missing_required_year_count = length(required_years),
      missing_required_years = wid_include_year_label(required_years),
      read_error = "",
      stringsAsFactors = FALSE
    ))
  }

  data <- wid_include_normalize_population(data, schema)
  key <- paste(data$country, data$year, sep = "\r")
  duplicate_keys <- sum(duplicated(key))
  missing_value_rows <- sum(!stats::complete.cases(data[, schema$numeric_columns, drop = FALSE]))
  country_years <- split(data$year, data$country)
  missing_by_country <- lapply(country_years, function(years) setdiff(required_years, unique(years)))
  missing_required <- unique(unlist(missing_by_country, use.names = FALSE))
  status <- if (!nrow(data)) {
    "empty_source"
  } else if (duplicate_keys > 0L) {
    "duplicate_keys"
  } else if (missing_value_rows > 0L) {
    "missing_values"
  } else if (length(missing_required)) {
    "missing_required_years"
  } else {
    "valid"
  }
  data.frame(
    source_id = source_id,
    source_set = source_set,
    file = path,
    rel = rel,
    destination = destination,
    exists = TRUE,
    status = status,
    rows = nrow(data),
    countries = length(unique(data$country)),
    first_year = suppressWarnings(min(data$year, na.rm = TRUE)),
    last_year = suppressWarnings(max(data$year, na.rm = TRUE)),
    missing_required_columns = "",
    duplicate_keys = duplicate_keys,
    missing_value_rows = missing_value_rows,
    missing_required_year_count = sum(lengths(missing_by_country)),
    missing_required_years = wid_include_year_label(missing_required),
    read_error = "",
    stringsAsFactors = FALSE
  )
}

wid_include_validation_report <- function(inventory) {
  rows <- list()
  incoming <- inventory[inventory$source_set == "incoming", , drop = FALSE]
  for (i in seq_len(nrow(incoming))) {
    row <- incoming[i, , drop = FALSE]
    source_id <- row$source_id[[1L]]
    status <- row$status[[1L]]
    if (identical(status, "missing_incoming_source")) {
      rows[[length(rows) + 1L]] <- data.frame(
        source_id = source_id,
        check = "incoming_file",
        status = "blocked_missing_incoming_source",
        severity = "blocked",
        detail = "Incoming WID population file is missing from _new.",
        next_command = sprintf("dina sources fetch %s", source_id),
        stringsAsFactors = FALSE
      )
    } else if (identical(status, "read_failed")) {
      rows[[length(rows) + 1L]] <- data.frame(source_id = source_id, check = "read", status = "blocked_read_failed", severity = "blocked", detail = row$read_error[[1L]], next_command = sprintf("dina sources fetch %s", source_id), stringsAsFactors = FALSE)
    } else if (identical(status, "schema_mismatch")) {
      rows[[length(rows) + 1L]] <- data.frame(source_id = source_id, check = "schema", status = "blocked_schema", severity = "blocked", detail = paste("Missing required columns:", row$missing_required_columns[[1L]]), next_command = sprintf("dina sources fetch %s", source_id), stringsAsFactors = FALSE)
    } else if (identical(status, "empty_source")) {
      rows[[length(rows) + 1L]] <- data.frame(source_id = source_id, check = "rows", status = "blocked_empty_source", severity = "blocked", detail = "Incoming WID population file has no rows.", next_command = sprintf("dina sources fetch %s", source_id), stringsAsFactors = FALSE)
    } else if (identical(status, "duplicate_keys")) {
      rows[[length(rows) + 1L]] <- data.frame(source_id = source_id, check = "keys", status = "blocked_duplicate_keys", severity = "blocked", detail = sprintf("%s duplicate country/year rows.", row$duplicate_keys[[1L]]), next_command = sprintf("dina sources fetch %s", source_id), stringsAsFactors = FALSE)
    } else if (identical(status, "missing_values")) {
      rows[[length(rows) + 1L]] <- data.frame(source_id = source_id, check = "values", status = "blocked_missing_values", severity = "blocked", detail = sprintf("%s rows have missing totalpop/adultpop values.", row$missing_value_rows[[1L]]), next_command = sprintf("dina sources fetch %s", source_id), stringsAsFactors = FALSE)
    } else if (identical(status, "missing_required_years")) {
      rows[[length(rows) + 1L]] <- data.frame(source_id = source_id, check = "required_years", status = "blocked_missing_required_years", severity = "blocked", detail = paste("Missing required years:", row$missing_required_years[[1L]]), next_command = sprintf("dina sources fetch %s", source_id), stringsAsFactors = FALSE)
    } else {
      rows[[length(rows) + 1L]] <- data.frame(source_id = source_id, check = "incoming_file", status = "ok", severity = "info", detail = "Incoming WID population file passed blocking checks.", next_command = "dina sources include wid --dry-run", stringsAsFactors = FALSE)
    }
  }
  wid_include_bind(rows)
}

wid_include_keyed_data <- function(data, schema) {
  if (!nrow(data) || length(setdiff(schema$required_columns, names(data)))) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  out <- wid_include_normalize_population(data, schema)
  out$country_key <- trimws(out$country)
  out[, c("country_key", "country", "region", "year", schema$numeric_columns), drop = FALSE]
}

wid_include_coverage_differences <- function(current, incoming, schema, source_id) {
  current <- wid_include_keyed_data(current, schema)
  incoming <- wid_include_keyed_data(incoming, schema)
  if (!nrow(current) || !nrow(incoming)) return(data.frame(stringsAsFactors = FALSE))
  current_keys <- unique(current[, c("country_key", "country", "year"), drop = FALSE])
  incoming_keys <- unique(incoming[, c("country_key", "country", "year"), drop = FALSE])
  key_current <- paste(current_keys$country_key, current_keys$year, sep = "\r")
  key_incoming <- paste(incoming_keys$country_key, incoming_keys$year, sep = "\r")
  current_only <- current_keys[!(key_current %in% key_incoming), , drop = FALSE]
  incoming_only <- incoming_keys[!(key_incoming %in% key_current), , drop = FALSE]
  if (nrow(current_only)) {
    current_only <- data.frame(source_id = source_id, coverage_status = "current_only", country = current_only$country, year = current_only$year, stringsAsFactors = FALSE)
  }
  if (nrow(incoming_only)) {
    incoming_only <- data.frame(source_id = source_id, coverage_status = "incoming_only", country = incoming_only$country, year = incoming_only$year, stringsAsFactors = FALSE)
  }
  out <- wid_include_bind(current_only, incoming_only)
  if (nrow(out)) out[order(out$country, out$year, out$coverage_status), , drop = FALSE] else out
}

wid_include_pct_diff <- function(incoming, current) {
  ifelse(is.na(current) | abs(current) < .Machine$double.eps, NA_real_, 100 * (incoming - current) / current)
}

wid_include_overlap_differences <- function(current, incoming, schema, source_id) {
  current <- wid_include_keyed_data(current, schema)
  incoming <- wid_include_keyed_data(incoming, schema)
  if (!nrow(current) || !nrow(incoming)) return(data.frame(stringsAsFactors = FALSE))
  current <- current[!duplicated(paste(current$country_key, current$year, sep = "\r")), , drop = FALSE]
  incoming <- incoming[!duplicated(paste(incoming$country_key, incoming$year, sep = "\r")), , drop = FALSE]
  suffix_current <- c("country_current", "region_current", paste0(schema$numeric_columns, "_current"))
  suffix_incoming <- c("country_incoming", "region_incoming", paste0(schema$numeric_columns, "_incoming"))
  names(current)[match(c("country", "region", schema$numeric_columns), names(current))] <- suffix_current
  names(incoming)[match(c("country", "region", schema$numeric_columns), names(incoming))] <- suffix_incoming
  overlap <- merge(current, incoming, by = c("country_key", "year"), all = FALSE, sort = FALSE)
  if (!nrow(overlap)) return(data.frame(stringsAsFactors = FALSE))
  out <- data.frame(
    source_id = source_id,
    country = overlap$country_incoming,
    year = overlap$year,
    stringsAsFactors = FALSE
  )
  for (col in schema$numeric_columns) {
    current_col <- overlap[[paste0(col, "_current")]]
    incoming_col <- overlap[[paste0(col, "_incoming")]]
    out[[paste0(col, "_current")]] <- current_col
    out[[paste0(col, "_incoming")]] <- incoming_col
    out[[paste0(col, "_abs_diff")]] <- incoming_col - current_col
    out[[paste0(col, "_pct_diff")]] <- wid_include_pct_diff(incoming_col, current_col)
  }
  out[order(out$country, out$year), , drop = FALSE]
}

wid_include_overlap_summary <- function(overlap, schema, source_id) {
  if (!nrow(overlap)) return(data.frame(stringsAsFactors = FALSE))
  countries <- sort(unique(overlap$country))
  rows <- lapply(countries, function(country) {
    part <- overlap[overlap$country == country, , drop = FALSE]
    row <- data.frame(source_id = source_id, country = country, overlap_years = length(unique(part$year)), stringsAsFactors = FALSE)
    for (col in schema$numeric_columns) {
      pct <- part[[paste0(col, "_pct_diff")]]
      row[[paste0(col, "_mean_pct_diff")]] <- mean(pct, na.rm = TRUE)
      row[[paste0(col, "_min_pct_diff")]] <- suppressWarnings(min(pct, na.rm = TRUE))
      row[[paste0(col, "_max_pct_diff")]] <- suppressWarnings(max(pct, na.rm = TRUE))
      row[[paste0(col, "_max_abs_pct_diff")]] <- suppressWarnings(max(abs(pct), na.rm = TRUE))
    }
    row
  })
  out <- wid_include_bind(rows)
  out[] <- lapply(out, function(x) {
    if (is.numeric(x)) {
      x[is.infinite(x)] <- NA_real_
    }
    x
  })
  out
}

wid_include_overlap_year_summary <- function(overlap, schema, source_id) {
  if (!nrow(overlap)) return(data.frame(stringsAsFactors = FALSE))
  years <- sort(unique(overlap$year))
  rows <- lapply(years, function(year) {
    part <- overlap[overlap$year == year, , drop = FALSE]
    row <- data.frame(source_id = source_id, year = year, countries = length(unique(part$country)), stringsAsFactors = FALSE)
    for (col in schema$numeric_columns) {
      current <- sum(part[[paste0(col, "_current")]], na.rm = TRUE)
      incoming <- sum(part[[paste0(col, "_incoming")]], na.rm = TRUE)
      row[[paste0(col, "_current")]] <- current
      row[[paste0(col, "_incoming")]] <- incoming
      row[[paste0(col, "_abs_diff")]] <- incoming - current
      row[[paste0(col, "_pct_diff")]] <- wid_include_pct_diff(incoming, current)
    }
    row
  })
  wid_include_bind(rows)
}

wid_include_review_actions <- function(validation_report) {
  if (!nrow(validation_report)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(validation_report$source_id)
  rows <- lapply(keys, function(source_id) {
    part <- validation_report[validation_report$source_id == source_id, , drop = FALSE]
    blocked <- part[part$severity == "blocked", , drop = FALSE]
    if (nrow(blocked)) {
      status <- if (any(blocked$status == "blocked_missing_incoming_source")) "missing_incoming_source" else "blocked_validation"
      next_command <- blocked$next_command[[1L]]
      detail <- paste(unique(blocked$detail), collapse = "; ")
    } else {
      status <- "ready_for_include"
      next_command <- "dina sources include wid --dry-run"
      detail <- "Incoming WID population file is ready for include dry-run review."
    }
    data.frame(source_id = source_id, action = status, severity = if (nrow(blocked)) "blocked" else "info", next_command = next_command, detail = detail, stringsAsFactors = FALSE)
  })
  wid_include_bind(rows)
}

wid_include_manifest <- function(run_id, status, contract, required_years, unsupported_sources = character(), dry_run = FALSE) {
  data.frame(
    key = c("run_id", "source_type", "workflow", "status", "dry_run", "supported_source_ids", "unsupported_wid_source_ids", "required_years"),
    value = c(run_id %||% "explore", "wid", "wid_population", status, as.character(isTRUE(dry_run)), paste(wid_include_supported_ids(contract), collapse = ","), paste(unsupported_sources, collapse = ","), wid_include_year_label(required_years)),
    stringsAsFactors = FALSE
  )
}

wid_include_unsupported_sources <- function(root, contract) {
  wid_sources <- wid_include_registry_wid_sources(root)
  supported <- wid_include_supported_ids(contract)
  unsupported <- Filter(function(source) !(source$id %||% "") %in% supported, wid_sources)
  if (!length(unsupported)) return(data.frame(stringsAsFactors = FALSE))
  wid_include_bind(lapply(unsupported, function(source) {
    data.frame(
      source_id = source$id %||% "",
      family = source$family %||% "",
      method = source$method %||% "",
      status = "unsupported_by_wid_workflow_v1",
      detail = "Listed by the registry, but not included in config/wid_include.yml source_ids.",
      stringsAsFactors = FALSE
    )
  }))
}

wid_include_explore_one <- function(root, contract, source_id, required_years) {
  source <- wid_include_registry_source(root, source_id)
  if (is.null(source)) {
    stop("WID include contract source is missing from registry: ", source_id, call. = FALSE)
  }
  schema <- wid_include_schema(contract, source_id)
  paths <- wid_include_source_paths(root, source)
  current_read <- wid_include_read_dta(paths$current)
  incoming_read <- wid_include_read_dta(paths$incoming)
  current_data <- if (isTRUE(current_read$ok)) wid_include_normalize_population(current_read$data, schema) else data.frame(stringsAsFactors = FALSE)
  incoming_data <- if (isTRUE(incoming_read$ok)) wid_include_normalize_population(incoming_read$data, schema) else data.frame(stringsAsFactors = FALSE)
  inventory <- wid_include_bind(
    wid_include_dataset_status(source_id, "current", paths$current, paths$current_rel, paths$destination, current_read$data, current_read$ok, current_read$error, schema, required_years),
    wid_include_dataset_status(source_id, "incoming", paths$incoming, paths$incoming_rel, paths$destination, incoming_read$data, incoming_read$ok, incoming_read$error, schema, required_years)
  )
  validation <- wid_include_validation_report(inventory)
  coverage <- wid_include_coverage_differences(current_data, incoming_data, schema, source_id)
  overlap <- wid_include_overlap_differences(current_data, incoming_data, schema, source_id)
  list(
    source = source,
    paths = paths,
    schema = schema,
    current_data = current_data,
    incoming_data = incoming_data,
    inventory = inventory,
    validation = validation,
    coverage = coverage,
    overlap = overlap,
    overlap_summary = wid_include_overlap_summary(overlap, schema, source_id),
    overlap_year_summary = wid_include_overlap_year_summary(overlap, schema, source_id)
  )
}

run_wid_explorer <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  output_dir = NULL,
  write_outputs = TRUE,
  dry_run = FALSE
) {
  contract <- wid_include_read_contract(root, contract_path)
  required_years <- wid_include_config_years(root, contract)
  paths <- wid_include_explore_paths(root, contract, output_dir)
  explored <- lapply(wid_include_supported_ids(contract), function(source_id) {
    wid_include_explore_one(root, contract, source_id, required_years)
  })
  unsupported <- wid_include_unsupported_sources(root, contract)
  validation <- wid_include_bind(lapply(explored, `[[`, "validation"))
  status <- if (nrow(validation) && any(validation$severity == "blocked", na.rm = TRUE)) "blocked" else "all_good"
  unsupported_ids <- if (nrow(unsupported)) unsupported$source_id else character()
  tables <- list(
    source_inventory = wid_include_bind(lapply(explored, `[[`, "inventory")),
    validation_report = validation,
    coverage_differences = wid_include_bind(lapply(explored, `[[`, "coverage")),
    overlap_differences = wid_include_bind(lapply(explored, `[[`, "overlap")),
    overlap_summary = wid_include_bind(lapply(explored, `[[`, "overlap_summary")),
    overlap_year_summary = wid_include_bind(lapply(explored, `[[`, "overlap_year_summary")),
    review_actions = wid_include_review_actions(validation),
    unsupported_sources = unsupported,
    explore_manifest = wid_include_manifest("explore", status, contract, required_years, unsupported_ids, dry_run = dry_run)
  )
  if (isTRUE(write_outputs)) {
    wid_include_write_csvs(tables, paths, "explore_manifest")
  }
  list(paths = paths, outputs = tables, manifest = tables$explore_manifest, contract = contract, required_years = required_years, status = status)
}

wid_include_manifest_value <- function(manifest, key) {
  if (!nrow(manifest) || !("key" %in% names(manifest)) || !("value" %in% names(manifest))) return("")
  hit <- manifest$key == key
  if (!any(hit)) "" else as.character(manifest$value[which(hit)[[1L]]])
}

wid_include_exploration_root <- function(root, contract, exploration_run = NULL) {
  path <- exploration_run %||% contract$explore_output_root
  normalizePath(wid_include_path(path, root), mustWork = FALSE)
}

wid_include_read_exploration <- function(root, contract, exploration_run = NULL) {
  run <- wid_include_exploration_root(root, contract, exploration_run)
  tables <- file.path(run, "tables")
  logs <- file.path(run, "logs")
  list(
    root = run,
    source_inventory = wid_include_read_csv(file.path(tables, "source_inventory.csv")),
    validation_report = wid_include_read_csv(file.path(tables, "validation_report.csv")),
    coverage_differences = wid_include_read_csv(file.path(tables, "coverage_differences.csv")),
    overlap_differences = wid_include_read_csv(file.path(tables, "overlap_differences.csv")),
    overlap_summary = wid_include_read_csv(file.path(tables, "overlap_summary.csv")),
    overlap_year_summary = wid_include_read_csv(file.path(tables, "overlap_year_summary.csv")),
    review_actions = wid_include_read_csv(file.path(tables, "review_actions.csv")),
    unsupported_sources = wid_include_read_csv(file.path(tables, "unsupported_sources.csv")),
    explore_manifest = wid_include_read_csv(file.path(logs, "explore_manifest.csv"))
  )
}

wid_include_copy_file <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
  isTRUE(ok)
}

wid_include_copy_path <- function(from, to) {
  if (!file.exists(from)) return("missing_source")
  if (dir.exists(from)) return("unsupported_directory_source")
  if (wid_include_copy_file(from, to)) "staged" else "copy_failed"
}

wid_include_hash_path <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(NA_character_)
  if (wid_include_has("digest")) {
    return(digest::digest(file = path, algo = "sha256"))
  }
  unname(as.character(tools::md5sum(path)))
}

wid_include_hash_algorithm <- function() {
  if (wid_include_has("digest")) "sha256" else "md5"
}

wid_include_stage_sources <- function(root, paths, exploration) {
  inventory <- exploration$source_inventory
  incoming <- inventory[inventory$source_set == "incoming", , drop = FALSE]
  if (!nrow(incoming)) return(wid_include_empty_mappings())
  dir.create(paths$staged_repo, recursive = TRUE, showWarnings = FALSE)
  rows <- lapply(seq_len(nrow(incoming)), function(i) {
    row <- incoming[i, , drop = FALSE]
    from <- if (grepl("^/", row$file[[1L]] %||% "")) row$file[[1L]] else file.path(root, row$rel[[1L]])
    to_rel <- row$destination[[1L]] %||% ""
    staged_to <- if (nzchar(to_rel)) file.path(paths$staged_repo, to_rel) else ""
    copy_status <- if (identical(row$status[[1L]], "valid") && nzchar(staged_to)) wid_include_copy_path(from, staged_to) else "not_staged"
    data.frame(
      source_id = row$source_id,
      source_set = "incoming",
      from_rel = wid_include_relative_path(from, root),
      to_rel = to_rel,
      staged_to = staged_to,
      validation_status = row$status,
      copy_status = copy_status,
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
}

wid_include_source_fingerprints <- function(root, mappings) {
  incoming <- mappings[mappings$source_set == "incoming", , drop = FALSE]
  if (!nrow(incoming)) return(wid_include_empty_fingerprints())
  algo <- wid_include_hash_algorithm()
  rows <- lapply(seq_len(nrow(incoming)), function(i) {
    row <- incoming[i, , drop = FALSE]
    path <- file.path(root, row$from_rel[[1L]])
    exists <- file.exists(path) && !dir.exists(path)
    info <- if (exists) file.info(path) else data.frame(size = NA_real_, mtime = as.POSIXct(NA))
    data.frame(
      source_id = row$source_id,
      source_set = row$source_set,
      rel = row$from_rel,
      exists = exists,
      kind = if (exists) "file" else "missing",
      size = if (exists) as.numeric(info$size[[1L]]) else NA_real_,
      mtime = if (exists) as.character(info$mtime[[1L]]) else NA_character_,
      hash_algorithm = algo,
      hash = if (exists) wid_include_hash_path(path) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
}

wid_include_promotion_plan <- function(mappings) {
  staged <- mappings[mappings$copy_status == "staged", , drop = FALSE]
  if (!nrow(staged)) return(wid_include_empty_promotion_plan())
  data.frame(
    source_id = staged$source_id,
    artifact_type = "wid_population",
    from_rel = staged$staged_to,
    to_rel = staged$to_rel,
    promotion_scope = "promote",
    stringsAsFactors = FALSE
  )
}

wid_include_promotion_fingerprints <- function(promotion_plan) {
  if (!nrow(promotion_plan)) return(wid_include_empty_promotion_fingerprints())
  algo <- wid_include_hash_algorithm()
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    exists <- file.exists(from) && !dir.exists(from)
    data.frame(
      source_id = row$source_id,
      artifact_type = row$artifact_type,
      from_rel = from,
      to_rel = row$to_rel,
      exists = exists,
      kind = if (exists) "file" else "missing",
      hash_algorithm = algo,
      hash = if (exists) wid_include_hash_path(from) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
}

wid_include_summary <- function(exploration, mappings, promotion_plan) {
  validation <- exploration$validation_report
  inventory <- exploration$source_inventory
  source_ids <- unique(c(
    if ("source_id" %in% names(validation)) validation$source_id else character(),
    if ("source_id" %in% names(mappings)) mappings$source_id else character(),
    if ("source_id" %in% names(inventory)) inventory$source_id else character()
  ))
  if (!length(source_ids)) return(data.frame(stringsAsFactors = FALSE))
  coverage <- exploration$coverage_differences
  overlap <- exploration$overlap_differences
  rows <- lapply(source_ids, function(source_id) {
    val <- validation[validation$source_id == source_id, , drop = FALSE]
    map <- mappings[mappings$source_id == source_id, , drop = FALSE]
    inv <- inventory[inventory$source_id == source_id & inventory$source_set == "incoming", , drop = FALSE]
    blocked <- if (nrow(val)) sum(val$severity == "blocked", na.rm = TRUE) else 0L
    copy_blocked <- if (nrow(map)) sum(!map$copy_status %in% c("staged"), na.rm = TRUE) else 1L
    status <- if (blocked > 0L || copy_blocked > 0L || !nrow(promotion_plan)) "blocked" else "all_good"
    data.frame(
      source_id = source_id,
      status = status,
      incoming_rows = if (nrow(inv)) inv$rows[[1L]] else 0L,
      incoming_countries = if (nrow(inv)) inv$countries[[1L]] else 0L,
      incoming_first_year = if (nrow(inv)) inv$first_year[[1L]] else NA_integer_,
      incoming_last_year = if (nrow(inv)) inv$last_year[[1L]] else NA_integer_,
      staged_sources = if (nrow(map)) sum(map$copy_status == "staged", na.rm = TRUE) else 0L,
      promotions = if (nrow(promotion_plan)) nrow(promotion_plan[promotion_plan$source_id == source_id, , drop = FALSE]) else 0L,
      overlap_rows = if (nrow(overlap)) nrow(overlap[overlap$source_id == source_id, , drop = FALSE]) else 0L,
      coverage_differences = if (nrow(coverage)) nrow(coverage[coverage$source_id == source_id, , drop = FALSE]) else 0L,
      warnings = 0L,
      blocked = blocked + copy_blocked,
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
}

wid_include_overall_status <- function(summary) {
  if (!nrow(summary)) return("blocked")
  if (any(summary$status == "blocked", na.rm = TRUE)) return("blocked")
  "all_good"
}

wid_include_include_manifest <- function(run_id, status, exploration_root, contract) {
  data.frame(
    key = c("run_id", "source_type", "workflow", "status", "dry_run", "exploration_run", "supported_source_ids"),
    value = c(run_id, "wid", "wid_population", status, "TRUE", normalizePath(exploration_root, mustWork = FALSE), paste(wid_include_supported_ids(contract), collapse = ",")),
    stringsAsFactors = FALSE
  )
}

run_wid_include <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  exploration_run = NULL,
  output_dir = NULL,
  write_outputs = TRUE,
  run_id = NULL
) {
  contract <- wid_include_read_contract(root, contract_path)
  run_id <- run_id %||% wid_include_run_id()
  paths <- wid_include_output_paths_for_run(root, contract, output_dir, run_id)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$staged_repo, recursive = TRUE, showWarnings = FALSE)
  exploration <- wid_include_read_exploration(root, contract, exploration_run)
  if (!nrow(exploration$source_inventory) || !nrow(exploration$validation_report)) {
    fresh <- run_wid_explorer(root = root, contract_path = contract_path, write_outputs = FALSE, dry_run = TRUE)
    exploration <- c(list(root = fresh$paths$root), fresh$outputs)
  }
  mappings <- wid_include_stage_sources(root, paths, exploration)
  source_fingerprints <- wid_include_source_fingerprints(root, mappings)
  promotion_plan <- wid_include_promotion_plan(mappings)
  promotion_fingerprints <- wid_include_promotion_fingerprints(promotion_plan)
  summary <- wid_include_summary(exploration, mappings, promotion_plan)
  status <- wid_include_overall_status(summary)
  include_detail <- exploration$validation_report
  include_manifest <- wid_include_include_manifest(run_id, status, exploration$root, contract)
  outputs <- list(
    staged_source_mappings = mappings,
    include_detail = include_detail,
    include_summary = summary,
    promotion_plan = promotion_plan,
    source_fingerprints = source_fingerprints,
    promotion_fingerprints = promotion_fingerprints,
    coverage_differences = exploration$coverage_differences,
    overlap_differences = exploration$overlap_differences,
    overlap_summary = exploration$overlap_summary,
    overlap_year_summary = exploration$overlap_year_summary,
    include_manifest = include_manifest
  )
  if (isTRUE(write_outputs)) {
    wid_include_write_csvs(outputs, paths, "include_manifest")
  }
  list(paths = paths, outputs = outputs, manifest = include_manifest, contract = contract, run_id = run_id)
}

wid_include_resolve_run <- function(root, contract, include_run) {
  if (is.null(include_run) || !nzchar(include_run)) {
    stop("--include-run is required for WID confirm.", call. = FALSE)
  }
  candidates <- normalizePath(c(include_run, file.path(root, include_run), file.path(wid_include_output_root(root, contract), "runs", include_run)), mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, "logs", "include_manifest.csv"))]
  if (!length(hit)) stop("WID include run not found: ", include_run, call. = FALSE)
  hit[[1L]]
}

wid_include_resolve_confirm <- function(root, contract, confirm_run) {
  if (is.null(confirm_run) || !nzchar(confirm_run)) stop("Confirm run is required for WID restore.", call. = FALSE)
  candidates <- normalizePath(c(confirm_run, file.path(root, confirm_run), file.path(wid_include_output_root(root, contract), "confirms", confirm_run)), mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, "logs", "confirm_manifest.csv"))]
  if (!length(hit)) stop("WID confirm run not found: ", confirm_run, call. = FALSE)
  hit[[1L]]
}

wid_include_verify_source_fingerprints <- function(root, include_run, mappings) {
  expected <- wid_include_read_csv(file.path(include_run, "tables", "source_fingerprints.csv"))
  incoming <- mappings[mappings$source_set == "incoming" & mappings$copy_status == "staged", , drop = FALSE]
  if (!nrow(incoming)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(incoming)), function(i) {
    row <- incoming[i, , drop = FALSE]
    rel <- row$from_rel[[1L]]
    path <- file.path(root, rel)
    hit <- expected[expected$source_set == "incoming" & expected$rel == rel, , drop = FALSE]
    dry_hash <- if (nrow(hit)) hit$hash[[1L]] else NA_character_
    dry_kind <- if (nrow(hit)) hit$kind[[1L]] else NA_character_
    current_exists <- file.exists(path) && !dir.exists(path)
    current_kind <- if (current_exists) "file" else "missing"
    current_hash <- if (current_exists) wid_include_hash_path(path) else NA_character_
    status <- if (!nrow(hit)) "missing_dry_run_fingerprint" else if (!current_exists) "missing_current_source" else if (!identical(as.character(dry_kind), as.character(current_kind))) "kind_changed" else if (!identical(as.character(dry_hash), as.character(current_hash))) "hash_changed" else "ok"
    data.frame(source_id = row$source_id, rel = rel, dry_run_kind = dry_kind, current_kind = current_kind, dry_run_hash = dry_hash, current_hash = current_hash, status = status, stringsAsFactors = FALSE)
  })
  report <- wid_include_bind(rows)
  if (nrow(report) && any(report$status != "ok", na.rm = TRUE)) {
    bad <- report[report$status != "ok", , drop = FALSE]
    stop("Confirm refused: incoming WID source fingerprints changed since dry-run. Rerun the include dry-run. First mismatch: ", bad$rel[[1L]], " (", bad$status[[1L]], ").", call. = FALSE)
  }
  report
}

wid_include_verify_promotion_fingerprints <- function(include_run, promotion_plan) {
  expected <- wid_include_read_csv(file.path(include_run, "tables", "promotion_fingerprints.csv"))
  if (!nrow(promotion_plan)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    hit <- expected[expected$source_id == row$source_id & expected$artifact_type == row$artifact_type & expected$to_rel == row$to_rel, , drop = FALSE]
    dry_hash <- if (nrow(hit)) hit$hash[[1L]] else NA_character_
    dry_kind <- if (nrow(hit)) hit$kind[[1L]] else NA_character_
    exists <- file.exists(from) && !dir.exists(from)
    kind <- if (exists) "file" else "missing"
    hash <- if (exists) wid_include_hash_path(from) else NA_character_
    status <- if (!nrow(hit)) "missing_dry_run_fingerprint" else if (!exists) "missing_staged_artifact" else if (!identical(as.character(dry_kind), as.character(kind))) "kind_changed" else if (!identical(as.character(dry_hash), as.character(hash))) "hash_changed" else "ok"
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from = from, to = row$to_rel, dry_run_kind = dry_kind, current_kind = kind, dry_run_hash = dry_hash, current_hash = hash, status = status, stringsAsFactors = FALSE)
  })
  report <- wid_include_bind(rows)
  if (nrow(report) && any(report$status != "ok", na.rm = TRUE)) {
    bad <- report[report$status != "ok", , drop = FALSE]
    stop("Confirm refused: staged WID promotion artifacts changed since dry-run. Rerun the include dry-run. First mismatch: ", bad$to[[1L]], " (", bad$status[[1L]], ").", call. = FALSE)
  }
  report
}

wid_include_confirm_manifest <- function(confirm_id, include_run, status) {
  data.frame(
    key = c("confirm_id", "source_type", "workflow", "status", "include_run", "confirmed_at"),
    value = c(confirm_id, "wid", "wid_population", status, normalizePath(include_run, mustWork = FALSE), as.character(Sys.time())),
    stringsAsFactors = FALSE
  )
}

wid_include_confirm_sources <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  include_run = NULL,
  output_dir = NULL
) {
  contract <- wid_include_read_contract(root, contract_path)
  include_run <- wid_include_resolve_run(root, contract, include_run)
  include_manifest <- wid_include_read_csv(file.path(include_run, "logs", "include_manifest.csv"))
  status <- wid_include_manifest_value(include_manifest, "status")
  if (!identical(status, "all_good")) stop("WID include run is not clean; status is ", status, ".", call. = FALSE)
  mappings <- wid_include_read_csv(file.path(include_run, "tables", "staged_source_mappings.csv"))
  promotion_plan <- wid_include_read_csv(file.path(include_run, "tables", "promotion_plan.csv"))
  if (!nrow(promotion_plan)) stop("WID include run has no staged artifact to promote.", call. = FALSE)
  source_check <- wid_include_verify_source_fingerprints(root, include_run, mappings)
  staged_check <- wid_include_verify_promotion_fingerprints(include_run, promotion_plan)
  confirm_id <- wid_include_confirm_id()
  paths <- wid_include_output_paths_for_confirm(root, contract, output_dir, confirm_id)
  report <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    to <- file.path(root, row$to_rel[[1L]])
    backup <- file.path(paths$snapshots, "original", row$to_rel[[1L]])
    backup_status <- if (file.exists(to)) wid_include_copy_path(to, backup) else "destination_absent"
    promote_status <- wid_include_copy_path(from, to)
    data.frame(
      source_id = row$source_id,
      artifact_type = row$artifact_type,
      from = from,
      to = row$to_rel,
      backup = if (identical(backup_status, "destination_absent")) "" else wid_include_relative_path(backup, root),
      backup_status = if (identical(backup_status, "staged")) "backed_up" else backup_status,
      promote_status = promote_status,
      stringsAsFactors = FALSE
    )
  })
  promote_report <- wid_include_bind(report)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(promote_report, file.path(paths$tables, "promote_report.csv"), row.names = FALSE, na = "")
  utils::write.csv(source_check, file.path(paths$tables, "source_fingerprint_check.csv"), row.names = FALSE, na = "")
  utils::write.csv(staged_check, file.path(paths$tables, "staged_artifact_fingerprint_check.csv"), row.names = FALSE, na = "")
  manifest <- wid_include_confirm_manifest(confirm_id, include_run, "confirmed")
  utils::write.csv(manifest, file.path(paths$logs, "confirm_manifest.csv"), row.names = FALSE, na = "")
  list(paths = paths, outputs = list(promote_report = promote_report, source_fingerprint_check = source_check, staged_artifact_fingerprint_check = staged_check), manifest = manifest, contract = contract)
}

wid_include_restore_sources <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  confirm_run = NULL
) {
  contract <- wid_include_read_contract(root, contract_path)
  confirm_run <- wid_include_resolve_confirm(root, contract, confirm_run)
  promote_report <- wid_include_read_csv(file.path(confirm_run, "tables", "promote_report.csv"))
  rows <- lapply(seq_len(nrow(promote_report)), function(i) {
    row <- promote_report[i, , drop = FALSE]
    dest <- file.path(root, row$to[[1L]])
    if (identical(row$backup_status[[1L]], "backed_up")) {
      backup <- file.path(root, row$backup[[1L]])
      restore_status <- wid_include_copy_path(backup, dest)
    } else if (identical(row$backup_status[[1L]], "destination_absent")) {
      if (file.exists(dest)) unlink(dest, recursive = TRUE)
      restore_status <- "removed_promoted_destination"
    } else {
      restore_status <- "no_backup_available"
    }
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, destination = row$to, restore_status = restore_status, stringsAsFactors = FALSE)
  })
  restore_report <- wid_include_bind(rows)
  utils::write.csv(restore_report, file.path(confirm_run, "tables", "restore_report.csv"), row.names = FALSE, na = "")
  list(paths = list(root = confirm_run, restore_report = file.path(confirm_run, "tables", "restore_report.csv")), outputs = list(restore_report = restore_report), contract = contract)
}

wid_include_table_run <- function(root, run = NULL) {
  path <- run %||% file.path(root, "output", "experiments", "wid_explore")
  normalizePath(if (grepl("^/", path)) path else file.path(root, path), mustWork = FALSE)
}

wid_include_table_file <- function(root, table, run = NULL) {
  table <- gsub("-", "_", table %||% "", fixed = TRUE)
  run <- wid_include_table_run(root, run)
  candidates <- c(
    file.path(run, "tables", paste0(table, ".csv")),
    file.path(root, "output", "experiments", "wid_include", "runs", basename(run), "tables", paste0(table, ".csv"))
  )
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop("WID source table not found: ", table, call. = FALSE)
  }
  hit[[1L]]
}

wid_include_table_available <- function(root, run = NULL) {
  run <- wid_include_table_run(root, run)
  candidates <- c(
    file.path(run, "tables"),
    file.path(root, "output", "experiments", "wid_include", "runs", basename(run), "tables")
  )
  tables <- character()
  for (dir in candidates[dir.exists(candidates)]) {
    tables <- c(tables, tools::file_path_sans_ext(basename(Sys.glob(file.path(dir, "*.csv")))))
  }
  unique(tables)
}

wid_include_read_table <- function(root, table, run = NULL) {
  wid_include_read_csv(wid_include_table_file(root, table, run))
}

wid_include_table_catalog <- function() {
  data.frame(
    table = c(
      "source_inventory",
      "validation_report",
      "coverage_differences",
      "overlap_differences",
      "overlap_summary",
      "overlap_year_summary",
      "review_actions",
      "unsupported_sources",
      "explore_manifest",
      "include_summary",
      "include_detail",
      "staged_source_mappings",
      "promotion_plan",
      "source_fingerprints",
      "promotion_fingerprints",
      "include_manifest"
    ),
    contents = c(
      "canonical and incoming WID population file inventory",
      "blocking schema/key/value/coverage checks for incoming population",
      "country-year keys present only in canonical or only in incoming",
      "overlapping country-year total/adult population differences and pct differences",
      "country-level mean/min/max pct differences over overlap years",
      "year-level aggregate current/incoming totals and pct differences",
      "review action and next command per WID source",
      "family=wid registry sources not supported by this v1 workflow",
      "explore run metadata",
      "include dry-run status",
      "include dry-run validation detail",
      "incoming file copied into the staged repo",
      "staged artifacts eligible for confirm promotion",
      "incoming source hashes captured at dry-run",
      "staged artifact hashes captured at dry-run",
      "include run metadata"
    ),
    stringsAsFactors = FALSE
  )
}
