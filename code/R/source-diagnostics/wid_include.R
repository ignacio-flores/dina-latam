# WID source explore/include workflow.
#
# Explore is review-only. Include dry-runs fetch WID data into a staged run,
# derive local pipeline-facing artifacts under input_data/wid, and compare those
# candidates with existing canonical files when available. Confirm promotes a
# clean staged run with backups and fingerprint checks.

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
  artifact
}

wid_include_artifacts <- function(contract) {
  stats::setNames(lapply(wid_include_supported_ids(contract), function(id) wid_include_artifact(contract, id)), wid_include_supported_ids(contract))
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

wid_include_area_map <- function() {
  data.frame(
    area = c("AG", "AR", "AW", "BS", "BB", "BZ", "BO", "BR", "CL", "CO", "CR", "CU", "CW", "DO", "EC", "SV", "GF", "GD", "GP", "GT", "GY", "HT", "HN", "JM", "MQ", "MX", "NI", "PA", "PY", "PE", "PR", "LC", "VC", "SR", "TT", "VI", "UY", "VE"),
    iso3 = c("ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "CUW", "DOM", "ECU", "SLV", "GUF", "GRD", "GLP", "GTM", "GUY", "HTI", "HND", "JAM", "MTQ", "MEX", "NIC", "PAN", "PRY", "PER", "PRI", "LCA", "VCT", "SUR", "TTO", "VIR", "URY", "VEN"),
    country_name = c("Antigua and Barbuda", "Argentina", "Aruba", "Bahamas", "Barbados", "Belize", "Bolivia (Plurinational State of)", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Dominican Republic", "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guadeloupe", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica", "Martinique", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Saint Lucia", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "United States Virgin Islands", "Uruguay", "Venezuela (Bolivarian Republic of)"),
    region = c("Caribbean", "South America", "Caribbean", "Caribbean", "Caribbean", "Central America", "South America", "South America", "South America", "South America", "Central America", "Caribbean", "Caribbean", "Caribbean", "South America", "Central America", "South America", "Caribbean", "Caribbean", "Central America", "South America", "Caribbean", "Central America", "Caribbean", "Caribbean", "Central America", "Central America", "Central America", "South America", "South America", "Caribbean", "Caribbean", "Caribbean", "South America", "Caribbean", "Caribbean", "South America", "South America"),
    stringsAsFactors = FALSE
  )
}

wid_include_areas <- function(area_set) {
  area_set <- area_set %||% "project_latam"
  switch(
    area_set,
    project_latam = c("AR", "BO", "BR", "CL", "CO", "CR", "DO", "EC", "GT", "HN", "MX", "NI", "PA", "PE", "PY", "SV", "VE", "UY"),
    export_countries = c("AR", "BR", "CL", "CO", "CR", "DO", "EC", "MX", "PE", "SV", "UY"),
    population_legacy = setdiff(wid_include_area_map()$area, c("GF", "GP", "MQ")),
    trimws(unlist(strsplit(as.character(area_set), ",", fixed = TRUE), use.names = FALSE))
  )
}

wid_include_iso3 <- function(area) {
  map <- wid_include_area_map()
  out <- map$iso3[match(toupper(area), map$area)]
  out
}

wid_include_country_name <- function(area) {
  map <- wid_include_area_map()
  map$country_name[match(toupper(area), map$area)]
}

wid_include_country_region <- function(area) {
  map <- wid_include_area_map()
  map$region[match(toupper(area), map$area)]
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
    areas = wid_include_areas(request$area_set %||% request$areas),
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
  required_vars <- c(totalpop = "npopul999i", adultpop = "npopul992i")
  missing <- setdiff(c("country", "variable", "year", "value"), names(raw))
  if (length(missing)) stop("Raw population data is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  raw$country <- toupper(as.character(raw$country))
  raw$variable <- as.character(raw$variable)
  raw$year <- suppressWarnings(as.integer(raw$year))
  raw$value <- suppressWarnings(as.numeric(raw$value))
  raw <- raw[raw$variable %in% unname(required_vars) & raw$year %in% years, c("country", "year", "variable", "value"), drop = FALSE]
  raw <- stats::aggregate(value ~ country + year + variable, data = raw, FUN = wid_include_first_value)
  areas <- wid_include_areas((artifact$request %||% list())$area_set)
  grid <- expand.grid(country = areas, year = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  total <- raw[raw$variable == required_vars[["totalpop"]], c("country", "year", "value"), drop = FALSE]
  adult <- raw[raw$variable == required_vars[["adultpop"]], c("country", "year", "value"), drop = FALSE]
  names(total)[names(total) == "value"] <- "totalpop"
  names(adult)[names(adult) == "value"] <- "adultpop"
  out <- merge(grid, total, by = c("country", "year"), all.x = TRUE, sort = FALSE)
  out <- merge(out, adult, by = c("country", "year"), all.x = TRUE, sort = FALSE)
  out$region <- wid_include_country_region(out$country)
  out$country_name <- wid_include_country_name(out$country)
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

wid_include_derive_public_spending <- function(raw) {
  wide <- wid_include_clean_value_names(wid_include_wide_values(raw, c("meduge", "mheage", "mexpgo", "mcongo", "mgdpro")))
  for (col in c("meduge", "mheage", "mexpgo", "mcongo", "mgdpro")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  out <- data.frame(
    year = suppressWarnings(as.integer(wide$year)),
    con = suppressWarnings(as.numeric(wide$mcongo)) / suppressWarnings(as.numeric(wide$mgdpro)) * 100,
    edu = suppressWarnings(as.numeric(wide$meduge)) / suppressWarnings(as.numeric(wide$mgdpro)) * 100,
    exp = suppressWarnings(as.numeric(wide$mexpgo)) / suppressWarnings(as.numeric(wide$mgdpro)) * 100,
    hea = suppressWarnings(as.numeric(wide$mheage)) / suppressWarnings(as.numeric(wide$mgdpro)) * 100,
    iso = wid_include_iso3(wide$country),
    source = "WID_web",
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$iso) & out$year >= 2000, , drop = FALSE]
  out[order(out$iso, out$year), , drop = FALSE]
}

wid_include_derive_prices <- function(raw) {
  wide <- wid_include_clean_value_names(wid_include_wide_values(raw, c("inyixx", "xlceup")))
  for (col in c("inyixx", "xlceup")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  out <- data.frame(
    country = wid_include_iso3(wide$country),
    countrycode = toupper(as.character(wide$country)),
    year = suppressWarnings(as.integer(wide$year)),
    defl_xxxx = suppressWarnings(as.numeric(wide$inyixx)),
    xppp_eur = suppressWarnings(as.numeric(wide$xlceup)),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$country), , drop = FALSE]
  out[order(out$country, out$year), , drop = FALSE]
}

wid_include_derive_export_scaling <- function(raw) {
  vars <- as.character(raw$variable)
  raw$indicator <- ifelse(startsWith(vars, "anninc992"), "anninc992i",
    ifelse(startsWith(vars, "anninc999") | vars == "anninc", "nninc_lcu_constc",
      ifelse(startsWith(vars, "xlceup"), "ppp_eur",
        ifelse(startsWith(vars, "xlceux"), "mer_eur", NA_character_)
      )
    )
  )
  raw <- raw[!is.na(raw$indicator), c("country", "year", "indicator", "value"), drop = FALSE]
  raw$value <- suppressWarnings(as.numeric(raw$value))
  raw <- stats::aggregate(value ~ country + year + indicator, data = raw, FUN = wid_include_first_value)
  wide <- wid_include_clean_value_names(stats::reshape(raw, idvar = c("country", "year"), timevar = "indicator", direction = "wide"))
  for (col in c("anninc992i", "nninc_lcu_constc", "ppp_eur", "mer_eur")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  out <- data.frame(
    iso = toupper(as.character(wide$country)),
    year = suppressWarnings(as.integer(wide$year)),
    anninc992i = suppressWarnings(as.numeric(wide$anninc992i)),
    nninc_lcu_constc = suppressWarnings(as.numeric(wide$nninc_lcu_constc)),
    ppp_eur = suppressWarnings(as.numeric(wide$ppp_eur)),
    mer_eur = suppressWarnings(as.numeric(wide$mer_eur)),
    stringsAsFactors = FALSE
  )
  out[order(out$iso, out$year), , drop = FALSE]
}

wid_include_derive_sptinc <- function(raw) {
  if (!"percentile" %in% names(raw)) raw$percentile <- "p0p20"
  if ("age" %in% names(raw)) raw <- raw[as.character(raw$age) == "992", , drop = FALSE]
  out <- data.frame(
    country = wid_include_iso3(raw$country),
    year = suppressWarnings(as.integer(raw$year)),
    widcode = as.character(raw$variable),
    p = as.character(raw$percentile),
    value_web = suppressWarnings(as.numeric(raw$value)),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$country) & out$year >= 2000, , drop = FALSE]
  out[order(out$country, out$year, out$widcode, out$p), , drop = FALSE]
}

wid_include_derive_artifact <- function(raw, artifact, years) {
  type <- artifact$type %||% "raw_subset"
  switch(
    type,
    population_total_adult = wid_include_derive_population(raw, artifact, years),
    raw_subset = wid_include_derive_raw_subset(raw),
    public_spending_gdp_shares = wid_include_derive_public_spending(raw),
    prices_deflator_ppp = wid_include_derive_prices(raw),
    export_scaling = wid_include_derive_export_scaling(raw),
    export_sptinc_check = wid_include_derive_sptinc(raw),
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
  candidate_exists <- file.exists(candidate_path)
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

wid_include_current_status <- function(root, contract, artifact) {
  paths <- wid_include_artifact_paths(root, artifact)
  read <- wid_include_read_dta(paths$canonical)
  status <- wid_include_dataset_status(artifact$source_id, "derived", "current", paths$canonical, paths$canonical_rel, paths$canonical_rel, read$data, read$ok, read$error)
  inputs <- c(
    contract$contract_path,
    file.path(root, "config", "dina.yml"),
    file.path(root, "config", "sources.yml"),
    file.path(root, "code", "R", "source-diagnostics", "wid_include.R")
  )
  inputs <- inputs[file.exists(inputs)]
  latest_input <- if (length(inputs)) max(file.info(inputs)$mtime, na.rm = TRUE) else as.POSIXct(NA)
  if (isTRUE(status$exists[[1L]]) && !is.na(latest_input) && latest_input > file.info(paths$canonical)$mtime[[1L]]) {
    status$status <- "stale"
  }
  status
}

wid_include_review_actions <- function(status) {
  if (!nrow(status)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(unique(status$source_id), function(source_id) {
    part <- status[status$source_id == source_id, , drop = FALSE]
    needs <- part$status %in% c("missing_current_artifact", "stale", "read_failed")
    data.frame(
      source_id = source_id,
      action = if (any(needs)) "review_include" else "no_action",
      severity = if (any(needs)) "info" else "info",
      next_command = if (any(needs)) "dina sources include wid --dry-run" else "",
      detail = if (any(needs)) "WID artifact is missing, unreadable, or older than WID workflow inputs." else "WID artifact is present.",
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
}

wid_include_manifest <- function(run_id, status, contract, dry_run = FALSE, exploration_root = "") {
  data.frame(
    key = c("run_id", "source_type", "workflow", "status", "dry_run", "supported_source_ids", "exploration_run"),
    value = c(run_id %||% "explore", "wid", "wid_sources", status, as.character(isTRUE(dry_run)), paste(wid_include_supported_ids(contract), collapse = ","), exploration_root),
    stringsAsFactors = FALSE
  )
}

wid_include_unsupported_sources <- function(root, contract) {
  wid_sources <- wid_include_registry_wid_sources(root)
  supported <- wid_include_supported_ids(contract)
  unsupported <- Filter(function(source) {
    source_id <- source$id %||% ""
    covered_by <- source$covered_by %||% ""
    !(source_id %in% supported || covered_by %in% supported)
  }, wid_sources)
  if (!length(unsupported)) return(data.frame(stringsAsFactors = FALSE))
  wid_include_bind(lapply(unsupported, function(source) {
    data.frame(source_id = source$id %||% "", family = source$family %||% "", method = source$method %||% "", status = "unsupported_by_wid_workflow", detail = "Listed by the registry but not included in config/wid_include.yml source_ids.", stringsAsFactors = FALSE)
  }))
}

run_wid_explorer <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  output_dir = NULL,
  write_outputs = TRUE,
  dry_run = FALSE
) {
  contract <- wid_include_read_contract(root, contract_path)
  paths <- wid_include_explore_paths(root, contract, output_dir)
  artifacts <- wid_include_artifacts(contract)
  inventory <- wid_include_bind(lapply(artifacts, function(artifact) wid_include_current_status(root, contract, artifact)))
  artifact_status <- data.frame(
    source_id = inventory$source_id,
    artifact = inventory$destination,
    exists = inventory$exists,
    status = inventory$status,
    rows = inventory$rows,
    first_year = inventory$first_year,
    last_year = inventory$last_year,
    next_command = ifelse(inventory$status %in% c("missing_current_artifact", "stale", "read_failed"), "dina sources include wid --dry-run", ""),
    stringsAsFactors = FALSE
  )
  actions <- wid_include_review_actions(inventory)
  unsupported <- wid_include_unsupported_sources(root, contract)
  overall <- if (any(inventory$status == "read_failed", na.rm = TRUE)) "blocked" else "review"
  tables <- list(
    source_inventory = inventory,
    wid_artifact_status = artifact_status,
    validation_report = data.frame(stringsAsFactors = FALSE),
    wid_artifact_comparison = data.frame(stringsAsFactors = FALSE),
    wid_numeric_comparison = data.frame(stringsAsFactors = FALSE),
    review_actions = actions,
    unsupported_sources = unsupported,
    explore_manifest = wid_include_manifest("explore", overall, contract, dry_run = dry_run)
  )
  if (isTRUE(write_outputs)) {
    wid_include_write_csvs(tables, paths, "explore_manifest")
  }
  list(paths = paths, outputs = tables, manifest = tables$explore_manifest, contract = contract, status = overall)
}

wid_include_prepare_one <- function(root, contract, paths, artifact) {
  source_id <- artifact$source_id
  years <- wid_include_years(root, contract, artifact)
  required_years <- wid_include_required_years(root, contract, artifact)
  stage_paths <- wid_include_artifact_paths(root, artifact, staged_repo = paths$staged_repo)
  prod_paths <- wid_include_artifact_paths(root, artifact)
  result <- tryCatch({
    raw <- wid_include_fetch_raw(root, contract, artifact, stage_paths$raw)
    candidate <- wid_include_derive_artifact(raw, artifact, years)
    wid_include_write_dta(candidate, stage_paths$canonical)
    validation <- wid_include_validate_candidate(source_id, artifact, candidate, required_years)
    comparison <- wid_include_compare_artifact(source_id, artifact, stage_paths$canonical, prod_paths$canonical, candidate)
    list(ok = !any(validation$severity == "blocked", na.rm = TRUE), raw = raw, candidate = candidate, validation = validation, comparison = comparison, error = "")
  }, error = function(e) {
    validation <- data.frame(source_id = source_id, check = "derive_or_fetch", status = if (grepl("WID download failed|WID returned", conditionMessage(e))) "blocked_fetch_failed" else "blocked_derivation_failed", severity = "blocked", detail = conditionMessage(e), next_command = "dina sources include wid --dry-run", stringsAsFactors = FALSE)
    list(ok = FALSE, raw = data.frame(stringsAsFactors = FALSE), candidate = data.frame(stringsAsFactors = FALSE), validation = validation, comparison = list(artifact = data.frame(stringsAsFactors = FALSE), numeric = data.frame(stringsAsFactors = FALSE)), error = conditionMessage(e))
  })
  candidate_read <- wid_include_read_dta(stage_paths$canonical)
  raw_read <- wid_include_read_dta(stage_paths$raw)
  inventory <- wid_include_bind(
    wid_include_dataset_status(source_id, "derived", "candidate", stage_paths$canonical, wid_include_relative_path(stage_paths$canonical, root), artifact$canonical, candidate_read$data, candidate_read$ok, candidate_read$error),
    wid_include_dataset_status(source_id, "raw", "candidate", stage_paths$raw, wid_include_relative_path(stage_paths$raw, root), artifact$raw, raw_read$data, raw_read$ok, raw_read$error)
  )
  promotions <- if (isTRUE(result$ok)) {
    wid_include_bind(
      data.frame(source_id = source_id, artifact_type = "derived", from_rel = stage_paths$canonical, to_rel = stage_paths$canonical_rel, promotion_scope = "promote", stringsAsFactors = FALSE),
      data.frame(source_id = source_id, artifact_type = "raw", from_rel = stage_paths$raw, to_rel = stage_paths$raw_rel, promotion_scope = "promote", stringsAsFactors = FALSE)
    )
  } else {
    data.frame(source_id = character(), artifact_type = character(), from_rel = character(), to_rel = character(), promotion_scope = character(), stringsAsFactors = FALSE)
  }
  list(source_id = source_id, ok = result$ok, validation = result$validation, inventory = inventory, comparison = result$comparison$artifact, numeric = result$comparison$numeric, promotion_plan = promotions)
}

wid_include_read_exploration <- function(root, contract, exploration_run = NULL) {
  run <- normalizePath(wid_include_path(exploration_run %||% contract$explore_output_root, root), mustWork = FALSE)
  tables <- file.path(run, "tables")
  logs <- file.path(run, "logs")
  list(
    root = run,
    source_inventory = wid_include_read_csv(file.path(tables, "source_inventory.csv")),
    validation_report = wid_include_read_csv(file.path(tables, "validation_report.csv")),
    wid_artifact_status = wid_include_read_csv(file.path(tables, "wid_artifact_status.csv")),
    wid_artifact_comparison = wid_include_read_csv(file.path(tables, "wid_artifact_comparison.csv")),
    wid_numeric_comparison = wid_include_read_csv(file.path(tables, "wid_numeric_comparison.csv")),
    review_actions = wid_include_read_csv(file.path(tables, "review_actions.csv")),
    unsupported_sources = wid_include_read_csv(file.path(tables, "unsupported_sources.csv")),
    explore_manifest = wid_include_read_csv(file.path(logs, "explore_manifest.csv"))
  )
}

wid_include_promotion_fingerprints <- function(promotion_plan) {
  if (!nrow(promotion_plan)) {
    return(data.frame(source_id = character(), artifact_type = character(), from_rel = character(), to_rel = character(), exists = logical(), kind = character(), hash_algorithm = character(), hash = character(), stringsAsFactors = FALSE))
  }
  algo <- wid_include_hash_algorithm()
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    exists <- file.exists(from) && !dir.exists(from)
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from_rel = from, to_rel = row$to_rel, exists = exists, kind = if (exists) "file" else "missing", hash_algorithm = algo, hash = if (exists) wid_include_hash_path(from) else NA_character_, stringsAsFactors = FALSE)
  })
  wid_include_bind(rows)
}

wid_include_source_fingerprints <- function(promotion_plan) {
  staged_raw <- promotion_plan[promotion_plan$artifact_type == "raw", , drop = FALSE]
  if (!nrow(staged_raw)) {
    return(data.frame(source_id = character(), source_set = character(), rel = character(), exists = logical(), kind = character(), hash_algorithm = character(), hash = character(), stringsAsFactors = FALSE))
  }
  algo <- wid_include_hash_algorithm()
  rows <- lapply(seq_len(nrow(staged_raw)), function(i) {
    row <- staged_raw[i, , drop = FALSE]
    path <- row$from_rel[[1L]]
    exists <- file.exists(path) && !dir.exists(path)
    data.frame(source_id = row$source_id, source_set = "staged_raw", rel = path, exists = exists, kind = if (exists) "file" else "missing", hash_algorithm = algo, hash = if (exists) wid_include_hash_path(path) else NA_character_, stringsAsFactors = FALSE)
  })
  wid_include_bind(rows)
}

wid_include_summary <- function(prepared, promotion_plan) {
  if (!length(prepared)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(prepared, function(item) {
    inv <- item$inventory[item$inventory$artifact_type == "derived", , drop = FALSE]
    blocked <- if (nrow(item$validation)) sum(item$validation$severity == "blocked", na.rm = TRUE) else 0L
    promos <- promotion_plan[promotion_plan$source_id == item$source_id, , drop = FALSE]
    data.frame(
      source_id = item$source_id,
      status = if (blocked > 0L || !nrow(promos)) "blocked" else "all_good",
      incoming_rows = if (nrow(inv)) inv$rows[[1L]] else 0L,
      incoming_countries = if (nrow(inv)) inv$countries[[1L]] else NA_integer_,
      incoming_first_year = if (nrow(inv)) inv$first_year[[1L]] else NA_integer_,
      incoming_last_year = if (nrow(inv)) inv$last_year[[1L]] else NA_integer_,
      staged_sources = nrow(promos),
      promotions = nrow(promos),
      overlap_rows = NA_integer_,
      coverage_differences = NA_integer_,
      warnings = 0L,
      blocked = blocked,
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
  if (!nrow(exploration$source_inventory)) {
    fresh <- run_wid_explorer(root = root, contract_path = contract_path, write_outputs = FALSE, dry_run = TRUE)
    exploration <- c(list(root = fresh$paths$root), fresh$outputs)
  }
  prepared <- lapply(wid_include_artifacts(contract), function(artifact) wid_include_prepare_one(root, contract, paths, artifact))
  validation <- wid_include_bind(lapply(prepared, `[[`, "validation"))
  inventory <- wid_include_bind(exploration$source_inventory, lapply(prepared, `[[`, "inventory"))
  comparison <- wid_include_bind(lapply(prepared, `[[`, "comparison"))
  numeric <- wid_include_bind(lapply(prepared, `[[`, "numeric"))
  promotion_plan <- wid_include_bind(lapply(prepared, `[[`, "promotion_plan"))
  source_fingerprints <- wid_include_source_fingerprints(promotion_plan)
  promotion_fingerprints <- wid_include_promotion_fingerprints(promotion_plan)
  summary <- wid_include_summary(prepared, promotion_plan)
  status <- wid_include_overall_status(summary)
  manifest <- wid_include_manifest(run_id, status, contract, dry_run = TRUE, exploration_root = exploration$root)
  outputs <- list(
    source_inventory = inventory,
    include_detail = validation,
    validation_report = validation,
    include_summary = summary,
    staged_source_mappings = data.frame(stringsAsFactors = FALSE),
    promotion_plan = promotion_plan,
    source_fingerprints = source_fingerprints,
    promotion_fingerprints = promotion_fingerprints,
    wid_artifact_comparison = comparison,
    wid_numeric_comparison = numeric,
    coverage_differences = data.frame(stringsAsFactors = FALSE),
    overlap_differences = data.frame(stringsAsFactors = FALSE),
    overlap_summary = data.frame(stringsAsFactors = FALSE),
    overlap_year_summary = data.frame(stringsAsFactors = FALSE),
    include_manifest = manifest
  )
  if (isTRUE(write_outputs)) {
    wid_include_write_csvs(outputs, paths, "include_manifest")
  }
  list(paths = paths, outputs = outputs, manifest = manifest, contract = contract, run_id = run_id)
}

wid_include_manifest_value <- function(manifest, key) {
  if (!nrow(manifest) || !("key" %in% names(manifest)) || !("value" %in% names(manifest))) return("")
  hit <- manifest$key == key
  if (!any(hit)) "" else as.character(manifest$value[which(hit)[[1L]]])
}

wid_include_resolve_run <- function(root, contract, include_run) {
  if (is.null(include_run) || !nzchar(include_run)) stop("--include-run is required for WID confirm.", call. = FALSE)
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

wid_include_verify_promotion_fingerprints <- function(include_run, promotion_plan) {
  expected <- wid_include_read_csv(file.path(include_run, "tables", "promotion_fingerprints.csv"))
  if (!nrow(promotion_plan)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    hit <- expected[expected$source_id == row$source_id & expected$artifact_type == row$artifact_type & expected$to_rel == row$to_rel, , drop = FALSE]
    expected_hash <- if (nrow(hit)) hit$hash[[1L]] else NA_character_
    exists <- file.exists(from) && !dir.exists(from)
    hash <- if (exists) wid_include_hash_path(from) else NA_character_
    status <- if (!nrow(hit)) "missing_dry_run_fingerprint" else if (!exists) "missing_staged_artifact" else if (!identical(as.character(expected_hash), as.character(hash))) "hash_changed" else "ok"
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from = from, to = row$to_rel, dry_run_hash = expected_hash, current_hash = hash, status = status, stringsAsFactors = FALSE)
  })
  report <- wid_include_bind(rows)
  if (nrow(report) && any(report$status != "ok", na.rm = TRUE)) {
    bad <- report[report$status != "ok", , drop = FALSE]
    stop("Confirm refused: staged WID artifacts changed since dry-run. Rerun the include dry-run. First mismatch: ", bad$to[[1L]], " (", bad$status[[1L]], ").", call. = FALSE)
  }
  report
}

wid_include_copy_path <- function(from, to) {
  if (!file.exists(from)) return("missing_source")
  if (dir.exists(from)) return("unsupported_directory_source")
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(file.copy(from, to, overwrite = TRUE, copy.date = TRUE))) "staged" else "copy_failed"
}

wid_include_confirm_manifest <- function(confirm_id, include_run, status) {
  data.frame(key = c("confirm_id", "source_type", "workflow", "status", "include_run", "confirmed_at"), value = c(confirm_id, "wid", "wid_sources", status, normalizePath(include_run, mustWork = FALSE), as.character(Sys.time())), stringsAsFactors = FALSE)
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
  promotion_plan <- wid_include_read_csv(file.path(include_run, "tables", "promotion_plan.csv"))
  if (!nrow(promotion_plan)) stop("WID include run has no staged artifact to promote.", call. = FALSE)
  staged_check <- wid_include_verify_promotion_fingerprints(include_run, promotion_plan)
  missing <- promotion_plan[!file.exists(promotion_plan$from_rel), , drop = FALSE]
  if (nrow(missing)) stop("Confirm refused: staged WID artifacts are missing.", call. = FALSE)
  confirm_id <- wid_include_confirm_id()
  paths <- wid_include_output_paths_for_confirm(root, contract, output_dir, confirm_id)
  report <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    to <- file.path(root, row$to_rel[[1L]])
    backup <- file.path(paths$snapshots, "original", row$to_rel[[1L]])
    backup_status <- if (file.exists(to)) wid_include_copy_path(to, backup) else "destination_absent"
    promote_status <- if (backup_status %in% c("staged", "destination_absent")) {
      wid_include_copy_path(from, to)
    } else {
      "skipped_backup_failed"
    }
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from = from, to = row$to_rel, backup = if (identical(backup_status, "destination_absent")) "" else wid_include_relative_path(backup, root), backup_status = if (identical(backup_status, "staged")) "backed_up" else backup_status, promote_status = promote_status, stringsAsFactors = FALSE)
  })
  promote_report <- wid_include_bind(report)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(promote_report, file.path(paths$tables, "promote_report.csv"), row.names = FALSE, na = "")
  utils::write.csv(staged_check, file.path(paths$tables, "staged_artifact_fingerprint_check.csv"), row.names = FALSE, na = "")
  utils::write.csv(data.frame(stringsAsFactors = FALSE), file.path(paths$tables, "source_fingerprint_check.csv"), row.names = FALSE, na = "")
  bad_copy <- promote_report[
    !(promote_report$backup_status %in% c("backed_up", "destination_absent")) |
      promote_report$promote_status != "staged",
    ,
    drop = FALSE
  ]
  if (nrow(bad_copy)) {
    stop("WID confirm copy failed for ", bad_copy$to[[1L]], ".", call. = FALSE)
  }
  manifest <- wid_include_confirm_manifest(confirm_id, include_run, "confirmed")
  utils::write.csv(manifest, file.path(paths$logs, "confirm_manifest.csv"), row.names = FALSE, na = "")
  list(paths = paths, outputs = list(promote_report = promote_report, source_fingerprint_check = data.frame(stringsAsFactors = FALSE), staged_artifact_fingerprint_check = staged_check), manifest = manifest, contract = contract)
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
      restore_status <- wid_include_copy_path(file.path(root, row$backup[[1L]]), dest)
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
    table = c("source_inventory", "wid_artifact_status", "validation_report", "review_actions", "unsupported_sources", "explore_manifest", "include_summary", "include_detail", "promotion_plan", "source_fingerprints", "promotion_fingerprints", "wid_artifact_comparison", "wid_numeric_comparison", "include_manifest", "promote_report", "restore_report"),
    contents = c("current and staged WID artifact inventory", "current WID artifact missing/stale status", "blocking validation checks", "review action and next command per WID source", "registry WID rows not included in the workflow", "explore run metadata", "include dry-run summary", "include dry-run validation detail", "staged WID raw and derived artifacts eligible for confirm", "staged raw WID fingerprints", "staged promotion artifact fingerprints", "candidate versus current artifact file/schema/key comparison", "candidate versus current numeric aggregate comparison", "include run metadata", "confirm promotion report", "restore report"),
    stringsAsFactors = FALSE
  )
}
