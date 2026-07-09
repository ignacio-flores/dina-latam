# Shared helpers for the WID source explore/include workflow.

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
      if (grepl("first five rows are empty|no lines available", conditionMessage(e))) {
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
  required <- c("source_ids", "explore_output_root", "output_root", "years", "artifacts")
  missing <- setdiff(required, names(contract))
  if (length(missing)) {
    stop("Invalid WID include contract; missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  contract$contract_path <- contract_path
  contract
}

wid_include_supported_ids <- function(contract) {
  as.character(contract$source_ids %||% names(contract$artifacts %||% list()))
}

wid_include_artifact <- function(contract, source_id) {
  artifact <- (contract$artifacts %||% list())[[source_id]]
  if (is.null(artifact)) {
    stop("WID include contract has no artifact for source: ", source_id, call. = FALSE)
  }
  artifact$source_id <- source_id
  artifact$area_sets <- contract$area_sets %||% list()
  artifact$area_metadata <- contract$area_metadata %||% list()
  artifact
}

wid_include_artifacts <- function(contract) {
  ids <- wid_include_supported_ids(contract)
  stats::setNames(lapply(ids, function(id) wid_include_artifact(contract, id)), ids)
}

wid_include_schema <- function(artifact) {
  schema <- artifact$schema %||% list()
  list(
    required_columns = as.character(schema$required_columns %||% character()),
    key_columns = as.character(schema$key_columns %||% character()),
    numeric_columns = as.character(schema$numeric_columns %||% character()),
    required_years = as.character(schema$required_years %||% "none")
  )
}

wid_include_config <- function(root, contract) {
  path <- wid_include_path((contract$years %||% list())$from_config %||% "config/dina.yml", root)
  wid_include_read_yaml(path)
}

wid_include_years <- function(root, contract, artifact = NULL) {
  cfg <- wid_include_config(root, contract)
  last <- suppressWarnings(as.integer(cfg$years$last))
  first <- suppressWarnings(as.integer(((artifact$request %||% list())$first_year %||% (contract$years %||% list())$first %||% cfg$years$first)))
  if (is.na(first) || is.na(last) || first > last) {
    stop("Invalid WID required year range in contract/config.", call. = FALSE)
  }
  seq.int(first, last)
}

wid_include_required_years <- function(root, contract, artifact) {
  schema <- wid_include_schema(artifact)
  policy <- schema$required_years[[1L]] %||% "none"
  if (identical(policy, "none")) return(integer())
  cfg <- wid_include_config(root, contract)
  last <- suppressWarnings(as.integer(cfg$years$last))
  first <- switch(
    policy,
    configured_window = suppressWarnings(as.integer(cfg$years$first)),
    config = suppressWarnings(as.integer(((artifact$request %||% list())$first_year %||% (contract$years %||% list())$first %||% cfg$years$first))),
    suppressWarnings(as.integer(cfg$years$first))
  )
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

wid_include_run_id <- function(prefix = "wid-include") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

wid_include_confirm_id <- function(prefix = "wid-confirm") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
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

wid_include_read_dta <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    return(list(ok = FALSE, data = data.frame(stringsAsFactors = FALSE), error = "missing"))
  }
  wid_include_need("haven")
  data <- tryCatch(as.data.frame(haven::read_dta(path), stringsAsFactors = FALSE), error = function(e) e)
  if (inherits(data, "error")) {
    return(list(ok = FALSE, data = data.frame(stringsAsFactors = FALSE), error = conditionMessage(data)))
  }
  list(ok = TRUE, data = data, error = "")
}

wid_include_write_dta <- function(data, path) {
  wid_include_need("haven")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  haven::write_dta(data, path)
  path
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

wid_include_registry_wid_sources <- function(root) {
  registry <- wid_include_registry(root)
  Filter(function(source) identical(tolower(source$family %||% ""), "wid") || identical(tolower(source$method %||% ""), "wid"), registry)
}

wid_include_normalize_area_metadata <- function(metadata) {
  if (is.null(metadata) || !length(metadata)) {
    return(data.frame(area = character(), iso3 = character(), country_name = character(), region = character(), stringsAsFactors = FALSE))
  }
  if (is.data.frame(metadata)) {
    out <- as.data.frame(metadata, stringsAsFactors = FALSE)
  } else {
    out <- wid_include_bind(lapply(metadata, function(row) as.data.frame(row, stringsAsFactors = FALSE)))
  }
  if ("wid_area" %in% names(out) && !"area" %in% names(out)) names(out)[names(out) == "wid_area"] <- "area"
  required <- c("area", "iso3", "country_name", "region")
  missing <- setdiff(required, names(out))
  for (col in missing) out[[col]] <- NA_character_
  out <- out[required]
  out$area <- toupper(as.character(out$area))
  out$iso3 <- toupper(as.character(out$iso3))
  out$country_name <- as.character(out$country_name)
  out$region <- as.character(out$region)
  out
}

wid_include_area_map <- function(contract_or_artifact = NULL) {
  wid_include_normalize_area_metadata((contract_or_artifact %||% list())$area_metadata %||% list())
}

wid_include_areas <- function(area_set, contract_or_artifact = NULL) {
  area_set <- area_set %||% "project_latam"
  values <- wid_include_source_values(area_set)
  if (!length(values)) return(character())
  sets <- (contract_or_artifact %||% list())$area_sets %||% list()
  if (length(values) == 1L && values[[1L]] %in% names(sets)) {
    return(toupper(wid_include_source_values(sets[[values[[1L]]]])))
  }
  toupper(trimws(unlist(strsplit(values, ",", fixed = TRUE), use.names = FALSE)))
}

wid_include_iso3 <- function(area, contract_or_artifact = NULL) {
  map <- wid_include_area_map(contract_or_artifact)
  out <- map$iso3[match(toupper(area), map$area)]
  out
}

wid_include_country_name <- function(area, contract_or_artifact = NULL) {
  map <- wid_include_area_map(contract_or_artifact)
  out <- map$country_name[match(toupper(area), map$area)]
  ifelse(is.na(out), toupper(area), out)
}

wid_include_country_region <- function(area, contract_or_artifact = NULL) {
  map <- wid_include_area_map(contract_or_artifact)
  map$region[match(toupper(area), map$area)]
}

wid_include_output_map <- function(artifact) {
  artifact$output_map %||% list()
}

wid_include_required_output_map <- function(artifact, required_names) {
  map <- wid_include_output_map(artifact)
  missing <- setdiff(required_names, names(map))
  if (length(missing)) {
    stop("WID artifact ", artifact$source_id, " is missing output_map entries: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  stats::setNames(as.character(wid_include_source_values(map[required_names])), required_names)
}

wid_include_mapping_summary <- function(x, prefix = "") {
  if (is.null(x) || !length(x)) return("")
  if (is.atomic(x)) return(paste(as.character(x), collapse = "|"))
  parts <- unlist(lapply(names(x), function(name) {
    value <- x[[name]]
    key <- if (nzchar(prefix)) paste(prefix, name, sep = ".") else name
    if (is.list(value) && !is.data.frame(value)) {
      wid_include_mapping_summary(value, key)
    } else {
      paste0(key, "=", paste(wid_include_source_values(value), collapse = "|"))
    }
  }), use.names = FALSE)
  paste(parts[nzchar(parts)], collapse = "; ")
}

wid_include_raw_fixture_path <- function(source_id, artifact) {
  fixture_dir <- Sys.getenv("DINA_WID_FIXTURE_DIR", unset = "")
  if (!nzchar(fixture_dir)) return("")
  candidates <- c(
    file.path(fixture_dir, paste0(source_id, ".dta")),
    file.path(fixture_dir, basename(artifact$raw %||% ""))
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) hit[[1L]] else ""
}

wid_include_fetch_raw <- function(root, contract, artifact, staged_raw) {
  fixture <- wid_include_raw_fixture_path(artifact$source_id, artifact)
  if (nzchar(fixture)) {
    read <- wid_include_read_dta(fixture)
    if (!isTRUE(read$ok)) stop("Fixture WID raw file is unreadable: ", read$error, call. = FALSE)
    wid_include_write_dta(read$data, staged_raw)
    return(read$data)
  }
  if (nzchar(Sys.getenv("DINA_WID_FIXTURE_DIR", unset = ""))) {
    stop("WID download failed for ", artifact$source_id, ": no fixture raw file found.", call. = FALSE)
  }
  wid_include_need("wid")
  request <- artifact$request %||% list()
  args <- list(
    indicators = wid_include_source_values(request$indicators),
    areas = wid_include_areas(request$area_set %||% request$areas, artifact),
    years = wid_include_years(root, contract, artifact),
    metadata = FALSE,
    verbose = FALSE
  )
  for (name in c("ages", "pop", "perc")) {
    value <- wid_include_source_values(request[[name]])
    if (length(value)) args[[name]] <- value
  }
  raw <- tryCatch(do.call(wid::download_wid, args), error = function(e) e)
  if (inherits(raw, "error")) {
    stop("WID download failed for ", artifact$source_id, ": ", conditionMessage(raw), call. = FALSE)
  }
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (!nrow(raw)) stop("WID returned no rows for ", artifact$source_id, call. = FALSE)
  wid_include_write_dta(raw, staged_raw)
  raw
}

wid_include_indicator <- function(variable, indicators) {
  variable <- as.character(variable)
  out <- rep(NA_character_, length(variable))
  for (indicator in indicators) {
    hit <- startsWith(variable, indicator)
    out[hit & is.na(out)] <- indicator
  }
  out
}

wid_include_first_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) x[[1L]] else NA_real_
}

wid_include_wide_values <- function(raw, indicators, id_cols = c("country", "year")) {
  required <- c(id_cols, "variable", "value")
  missing <- setdiff(required, names(raw))
  if (length(missing)) stop("Raw WID data is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  raw$indicator <- wid_include_indicator(raw$variable, indicators)
  raw <- raw[!is.na(raw$indicator), c(id_cols, "indicator", "value"), drop = FALSE]
  raw$value <- suppressWarnings(as.numeric(raw$value))
  raw <- stats::aggregate(value ~ ., data = raw, FUN = wid_include_first_value)
  stats::reshape(raw, idvar = id_cols, timevar = "indicator", direction = "wide")
}

wid_include_clean_value_names <- function(data) {
  names(data) <- sub("^value\\.", "", names(data))
  data
}

wid_include_derive_population <- function(raw, artifact, years) {
  required_vars <- wid_include_required_output_map(artifact, c("totalpop", "adultpop"))
  missing <- setdiff(c("country", "variable", "year", "value"), names(raw))
  if (length(missing)) stop("Raw population data is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  raw$country <- toupper(as.character(raw$country))
  raw$variable <- as.character(raw$variable)
  raw$year <- suppressWarnings(as.integer(raw$year))
  raw$value <- suppressWarnings(as.numeric(raw$value))
  raw <- raw[raw$variable %in% unname(required_vars) & raw$year %in% years, c("country", "year", "variable", "value"), drop = FALSE]
  raw <- stats::aggregate(value ~ country + year + variable, data = raw, FUN = wid_include_first_value)
  areas <- wid_include_areas((artifact$request %||% list())$area_set, artifact)
  grid <- expand.grid(country = areas, year = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  total <- raw[raw$variable == required_vars[["totalpop"]], c("country", "year", "value"), drop = FALSE]
  adult <- raw[raw$variable == required_vars[["adultpop"]], c("country", "year", "value"), drop = FALSE]
  names(total)[names(total) == "value"] <- "totalpop"
  names(adult)[names(adult) == "value"] <- "adultpop"
  out <- merge(grid, total, by = c("country", "year"), all.x = TRUE, sort = FALSE)
  out <- merge(out, adult, by = c("country", "year"), all.x = TRUE, sort = FALSE)
  out$region <- wid_include_country_region(out$country, artifact)
  out$country_name <- wid_include_country_name(out$country, artifact)
  out <- out[order(match(out$country, areas), out$year), c("country_name", "region", "year", "totalpop", "adultpop")]
  names(out)[names(out) == "country_name"] <- "country"
  rownames(out) <- NULL
  attr(out$country, "label") <- "Location"
  attr(out$region, "label") <- "region in Latin America & Caribbean"
  attr(out$totalpop, "label") <- "Total Population"
  attr(out$adultpop, "label") <- "Adult Population (20+)"
  out
}

wid_include_derive_raw_subset <- function(raw) {
  keep <- intersect(c("country", "variable", "year", "value", "percentile", "age", "pop"), names(raw))
  out <- raw[, keep, drop = FALSE]
  out$year <- suppressWarnings(as.integer(out$year))
  if ("value" %in% names(out)) out$value <- suppressWarnings(as.numeric(out$value))
  out[order(out$country, out$variable, out$year), , drop = FALSE]
}

wid_include_derive_public_spending <- function(raw, artifact) {
  map <- wid_include_output_map(artifact)
  shares <- map$shares %||% list()
  denominator <- as.character(map$denominator %||% "")
  if (!length(shares) || !nzchar(denominator)) {
    stop("WID public spending artifact requires output_map shares and denominator.", call. = FALSE)
  }
  share_names <- names(shares)
  share_codes <- as.character(wid_include_source_values(shares))
  indicators <- unique(c(share_codes, denominator))
  wide <- wid_include_clean_value_names(wid_include_wide_values(raw, indicators))
  for (col in indicators) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  out <- data.frame(year = suppressWarnings(as.integer(wide$year)), stringsAsFactors = FALSE)
  denom <- suppressWarnings(as.numeric(wide[[denominator]]))
  for (i in seq_along(share_names)) {
    code <- share_codes[[i]]
    out[[share_names[[i]]]] <- suppressWarnings(as.numeric(wide[[code]])) / denom * 100
  }
  out$iso <- wid_include_iso3(wide$country, artifact)
  out$source <- as.character(map$source %||% "WID_web")
  min_year <- suppressWarnings(as.integer((artifact$derive %||% list())$min_year %||% 2000L))
  out <- out[!is.na(out$iso) & out$year >= min_year, , drop = FALSE]
  out[order(out$iso, out$year), , drop = FALSE]
}

wid_include_derive_prices <- function(raw, artifact) {
  map <- wid_include_required_output_map(artifact, c("defl_xxxx", "xppp_eur"))
  wide <- wid_include_clean_value_names(wid_include_wide_values(raw, unname(map)))
  for (col in unname(map)) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  out <- data.frame(
    country = wid_include_iso3(wide$country, artifact),
    countrycode = toupper(as.character(wide$country)),
    year = suppressWarnings(as.integer(wide$year)),
    defl_xxxx = suppressWarnings(as.numeric(wide[[map[["defl_xxxx"]]]])),
    xppp_eur = suppressWarnings(as.numeric(wide[[map[["xppp_eur"]]]])),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$country), , drop = FALSE]
  out[order(out$country, out$year), , drop = FALSE]
}

wid_include_match_variable_map <- function(variable, output_map) {
  variable <- as.character(variable)
  out <- rep(NA_character_, length(variable))
  for (name in names(output_map)) {
    patterns <- wid_include_source_values(output_map[[name]])
    hit <- rep(FALSE, length(variable))
    for (pattern in patterns) {
      hit <- hit | startsWith(variable, pattern) | variable == pattern
    }
    out[hit & is.na(out)] <- name
  }
  out
}

wid_include_derive_export_scaling <- function(raw, artifact) {
  output_map <- wid_include_output_map(artifact)
  if (!length(output_map)) stop("WID export scaling artifact requires output_map.", call. = FALSE)
  raw$indicator <- wid_include_match_variable_map(raw$variable, output_map)
  raw <- raw[!is.na(raw$indicator), c("country", "year", "indicator", "value"), drop = FALSE]
  raw$value <- suppressWarnings(as.numeric(raw$value))
  raw <- stats::aggregate(value ~ country + year + indicator, data = raw, FUN = wid_include_first_value)
  wide <- wid_include_clean_value_names(stats::reshape(raw, idvar = c("country", "year"), timevar = "indicator", direction = "wide"))
  for (col in names(output_map)) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  out <- data.frame(
    iso = toupper(as.character(wide$country)),
    year = suppressWarnings(as.integer(wide$year)),
    stringsAsFactors = FALSE
  )
  for (col in names(output_map)) {
    out[[col]] <- suppressWarnings(as.numeric(wide[[col]]))
  }
  out[order(out$iso, out$year), , drop = FALSE]
}

wid_include_derive_sptinc <- function(raw, artifact) {
  map <- wid_include_output_map(artifact)
  if (!"percentile" %in% names(raw)) raw$percentile <- wid_include_source_values((artifact$request %||% list())$perc)[[1L]] %||% "p0p20"
  ages <- wid_include_source_values((artifact$request %||% list())$ages)
  if ("age" %in% names(raw) && length(ages)) raw <- raw[as.character(raw$age) %in% ages, , drop = FALSE]
  out <- data.frame(
    country = wid_include_iso3(raw[[as.character(map$country %||% "country")]], artifact),
    year = suppressWarnings(as.integer(raw[[as.character(map$year %||% "year")]])),
    widcode = as.character(raw[[as.character(map$widcode %||% "variable")]]),
    p = as.character(raw[[as.character(map$p %||% "percentile")]]),
    value_web = suppressWarnings(as.numeric(raw[[as.character(map$value_web %||% "value")]])),
    stringsAsFactors = FALSE
  )
  min_year <- suppressWarnings(as.integer((artifact$derive %||% list())$min_year %||% ((artifact$request %||% list())$first_year %||% 2000L)))
  out <- out[!is.na(out$country) & out$year >= min_year, , drop = FALSE]
  out[order(out$country, out$year, out$widcode, out$p), , drop = FALSE]
}

wid_include_derive_artifact <- function(raw, artifact, years) {
  type <- artifact$type %||% "raw_subset"
  switch(
    type,
    population_total_adult = wid_include_derive_population(raw, artifact, years),
    raw_subset = wid_include_derive_raw_subset(raw),
    public_spending_gdp_shares = wid_include_derive_public_spending(raw, artifact),
    prices_deflator_ppp = wid_include_derive_prices(raw, artifact),
    export_scaling = wid_include_derive_export_scaling(raw, artifact),
    export_sptinc_check = wid_include_derive_sptinc(raw, artifact),
    stop("Unsupported WID artifact type: ", type, call. = FALSE)
  )
}

wid_include_artifact_paths <- function(root, artifact, staged_repo = NULL) {
  canonical_rel <- artifact$canonical %||% file.path("input_data", "wid", paste0(artifact$source_id, ".dta"))
  raw_rel <- artifact$raw %||% file.path("input_data", "wid", paste0("raw_", artifact$source_id, ".dta"))
  base <- staged_repo %||% root
  list(
    canonical_rel = canonical_rel,
    raw_rel = raw_rel,
    canonical = file.path(base, canonical_rel),
    raw = file.path(base, raw_rel)
  )
}

wid_include_dataset_status <- function(source_id, artifact_type, source_set, path, rel, destination, data = NULL, read_ok = NULL, read_error = "") {
  exists <- file.exists(path) && !dir.exists(path)
  if (is.null(data) || is.null(read_ok)) {
    read <- wid_include_read_dta(path)
    data <- read$data
    read_ok <- read$ok
    read_error <- read$error
  }
  info <- if (exists) file.info(path) else data.frame(size = NA_real_, mtime = as.POSIXct(NA))
  status <- if (!exists) {
    if (identical(source_set, "current")) "missing_current_artifact" else "missing_candidate"
  } else if (!isTRUE(read_ok)) {
    "read_failed"
  } else {
    "present"
  }
  data.frame(
    source_id = source_id,
    artifact_type = artifact_type,
    source_set = source_set,
    file = normalizePath(path, mustWork = FALSE),
    rel = rel,
    destination = destination,
    exists = exists,
    status = status,
    rows = if (isTRUE(read_ok)) nrow(data) else 0L,
    cols = if (isTRUE(read_ok)) ncol(data) else 0L,
    countries = if (isTRUE(read_ok) && "country" %in% names(data)) length(unique(data$country)) else if (isTRUE(read_ok) && "iso" %in% names(data)) length(unique(data$iso)) else NA_integer_,
    first_year = if (isTRUE(read_ok) && "year" %in% names(data) && nrow(data)) suppressWarnings(min(as.integer(data$year), na.rm = TRUE)) else NA_integer_,
    last_year = if (isTRUE(read_ok) && "year" %in% names(data) && nrow(data)) suppressWarnings(max(as.integer(data$year), na.rm = TRUE)) else NA_integer_,
    size = if (exists) as.numeric(info$size[[1L]]) else NA_real_,
    mtime = if (exists) as.character(info$mtime[[1L]]) else NA_character_,
    hash_algorithm = wid_include_hash_algorithm(),
    hash = if (exists) wid_include_hash_path(path) else NA_character_,
    read_error = read_error,
    stringsAsFactors = FALSE
  )
}

wid_include_validate_candidate <- function(source_id, artifact, data, years) {
  schema <- wid_include_schema(artifact)
  rows <- list()
  add <- function(check, status, severity, detail) {
    rows[[length(rows) + 1L]] <<- data.frame(source_id = source_id, check = check, status = status, severity = severity, detail = detail, next_command = if (identical(severity, "blocked")) "dina sources include wid --dry-run" else "", stringsAsFactors = FALSE)
  }
  if (!nrow(data)) {
    add("rows", "blocked_empty_source", "blocked", "Candidate WID artifact has no rows.")
    return(wid_include_bind(rows))
  }
  missing_cols <- setdiff(schema$required_columns, names(data))
  if (length(missing_cols)) {
    add("schema", "blocked_schema", "blocked", paste("Missing required columns:", paste(missing_cols, collapse = ",")))
  }
  if (!length(missing_cols)) {
    key <- do.call(paste, c(data[schema$key_columns], sep = "\r"))
    dup <- sum(duplicated(key), na.rm = TRUE)
    if (dup > 0L) add("keys", "blocked_duplicate_keys", "blocked", sprintf("%s duplicate key rows.", dup))
    numeric_cols <- intersect(schema$numeric_columns, names(data))
    missing_values <- sum(!stats::complete.cases(data[numeric_cols]))
    if (missing_values > 0L) add("values", "blocked_missing_values", "blocked", sprintf("%s rows have missing numeric values.", missing_values))
    if ("year" %in% names(data) && length(years)) {
      missing_years <- setdiff(years, sort(unique(suppressWarnings(as.integer(data$year)))))
      if (length(missing_years)) {
        add("required_years", "blocked_missing_required_years", "blocked", paste("Missing required years:", paste(missing_years, collapse = ",")))
      }
    }
  }
  if (!length(rows)) add("candidate", "ok", "info", "Candidate WID artifact passed blocking checks.")
  wid_include_bind(rows)
}

wid_include_pct_diff <- function(candidate, current) {
  ifelse(is.na(current) | abs(current) < .Machine$double.eps, NA_real_, 100 * (candidate - current) / current)
}

wid_include_compare_artifact <- function(source_id, artifact, candidate_path, current_path, candidate_data) {
  schema <- wid_include_schema(artifact)
  current_read <- wid_include_read_dta(current_path)
  current_exists <- file.exists(current_path)
  if (!current_exists || !isTRUE(current_read$ok)) {
    return(list(
      artifact = data.frame(
        source_id = source_id,
        comparison_status = if (!current_exists) "no_current_artifact" else "current_read_failed",
        current_rel = artifact$canonical,
        candidate_rel = normalizePath(candidate_path, mustWork = FALSE),
        current_rows = if (isTRUE(current_read$ok)) nrow(current_read$data) else 0L,
        candidate_rows = nrow(candidate_data),
        current_cols = if (isTRUE(current_read$ok)) ncol(current_read$data) else 0L,
        candidate_cols = ncol(candidate_data),
        added_columns = paste(names(candidate_data), collapse = ","),
        dropped_columns = "",
        key_current_only = NA_integer_,
        key_candidate_only = NA_integer_,
        stringsAsFactors = FALSE
      ),
      numeric = data.frame(stringsAsFactors = FALSE)
    ))
  }
  current_data <- current_read$data
  added <- setdiff(names(candidate_data), names(current_data))
  dropped <- setdiff(names(current_data), names(candidate_data))
  key_cols <- intersect(schema$key_columns, intersect(names(current_data), names(candidate_data)))
  current_only <- candidate_only <- NA_integer_
  if (length(key_cols)) {
    current_key <- do.call(paste, c(current_data[key_cols], sep = "\r"))
    candidate_key <- do.call(paste, c(candidate_data[key_cols], sep = "\r"))
    current_only <- sum(!(unique(current_key) %in% unique(candidate_key)))
    candidate_only <- sum(!(unique(candidate_key) %in% unique(current_key)))
  }
  numeric_rows <- lapply(intersect(schema$numeric_columns, intersect(names(current_data), names(candidate_data))), function(col) {
    current_sum <- sum(suppressWarnings(as.numeric(current_data[[col]])), na.rm = TRUE)
    candidate_sum <- sum(suppressWarnings(as.numeric(candidate_data[[col]])), na.rm = TRUE)
    data.frame(source_id = source_id, column = col, current_sum = current_sum, candidate_sum = candidate_sum, abs_diff = candidate_sum - current_sum, pct_diff = wid_include_pct_diff(candidate_sum, current_sum), stringsAsFactors = FALSE)
  })
  list(
    artifact = data.frame(
      source_id = source_id,
      comparison_status = "compared",
      current_rel = artifact$canonical,
      candidate_rel = normalizePath(candidate_path, mustWork = FALSE),
      current_rows = nrow(current_data),
      candidate_rows = nrow(candidate_data),
      current_cols = ncol(current_data),
      candidate_cols = ncol(candidate_data),
      added_columns = paste(added, collapse = ","),
      dropped_columns = paste(dropped, collapse = ","),
      key_current_only = current_only,
      key_candidate_only = candidate_only,
      stringsAsFactors = FALSE
    ),
    numeric = wid_include_bind(numeric_rows)
  )
}

wid_include_manifest <- function(run_id, status, contract, dry_run = FALSE, exploration_root = "") {
  data.frame(
    key = c("run_id", "source_type", "workflow", "status", "dry_run", "supported_source_ids", "exploration_run"),
    value = c(run_id %||% "explore", "wid", "wid_sources", status, as.character(isTRUE(dry_run)), paste(wid_include_supported_ids(contract), collapse = ","), exploration_root),
    stringsAsFactors = FALSE
  )
}

wid_include_workflow_inputs <- function(root, contract) {
  unique(c(
    contract$contract_path,
    file.path(root, "config", "dina.yml"),
    file.path(root, "config", "sources.yml"),
    file.path(root, "code", "R", "source-diagnostics", "wid_common.R"),
    file.path(root, "code", "R", "source-diagnostics", "wid_explorer.R"),
    file.path(root, "code", "R", "source-diagnostics", "wid_include.R")
  ))
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
  if (!length(hit)) stop("WID source table not found: ", table, call. = FALSE)
  hit[[1L]]
}

wid_include_table_available <- function(root, run = NULL) {
  run <- wid_include_table_run(root, run)
  candidates <- c(file.path(run, "tables"), file.path(root, "output", "experiments", "wid_include", "runs", basename(run), "tables"))
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
    table = c("wid_request_plan", "source_inventory", "wid_artifact_status", "validation_report", "review_actions", "unsupported_sources", "explore_manifest", "include_summary", "include_detail", "promotion_plan", "source_fingerprints", "promotion_fingerprints", "wid_artifact_comparison", "wid_numeric_comparison", "include_manifest", "promote_report", "restore_report"),
    contents = c("configured WID request, area set, raw/derived outputs, and output mappings", "current and staged WID artifact inventory", "current WID artifact missing/stale status", "blocking validation checks", "review action and next command per WID source", "registry WID rows not included in the workflow", "explore run metadata", "include dry-run summary", "include dry-run validation detail", "staged WID raw and derived artifacts eligible for confirm", "staged raw WID fingerprints", "staged promotion artifact fingerprints", "candidate versus current artifact file/schema/key comparison", "candidate versus current numeric aggregate comparison", "include run metadata", "confirm promotion report", "restore report"),
    stringsAsFactors = FALSE
  )
}
