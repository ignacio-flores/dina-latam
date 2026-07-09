# Survey source explore/include workflow for SurveyPop.dta.
#
# Explore inventories canonical and incoming CEPAL survey files and reports
# whether the derived SurveyPop.dta is missing or stale. Include dry-runs build a
# staged SurveyPop.dta from canonical surveys plus any incoming overlay under
# input_data/_new/surveys. Confirm promotes staged survey sources and the staged
# SurveyPop.dta after writing a backup snapshot.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

survey_pop_repo_root <- function(start = getwd()) {
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

survey_pop_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

survey_pop_has <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

survey_pop_path <- function(path, root) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  if (grepl("^/", path)) path else file.path(root, path)
}

survey_pop_relative_path <- function(path, root) {
  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

survey_pop_read_yaml <- function(path, default = NULL) {
  survey_pop_need("yaml")
  if (!file.exists(path)) {
    if (!is.null(default)) return(default)
    stop("Missing YAML file: ", path, call. = FALSE)
  }
  yaml::read_yaml(path)
}

survey_pop_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame(stringsAsFactors = FALSE))
  lines <- readLines(path, warn = FALSE, n = 5L)
  if (!length(lines) || all(!nzchar(trimws(lines)))) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

survey_pop_bind <- function(...) {
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

survey_pop_write_csvs <- function(tables, paths, manifest_name) {
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

survey_pop_read_contract <- function(
  root = survey_pop_repo_root(),
  contract_path = file.path(root, "config", "survey_population_include.yml")
) {
  contract <- survey_pop_read_yaml(contract_path)
  required <- c("source_type", "source_ids", "explore_output_root", "output_root", "years", "paths")
  missing <- setdiff(required, names(contract))
  if (length(missing)) {
    stop("Invalid survey population contract; missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  contract$contract_path <- contract_path
  contract
}

survey_pop_supported_ids <- function(contract) {
  as.character(contract$source_ids %||% "surveys-cepal")
}

survey_pop_config <- function(root, contract) {
  config_path <- survey_pop_path((contract$years %||% list())$from_config %||% "config/dina.yml", root)
  survey_pop_read_yaml(config_path)
}

survey_pop_years <- function(root, contract) {
  config <- survey_pop_config(root, contract)
  first <- suppressWarnings(as.integer(config$years$first %||% config$first_y))
  last <- suppressWarnings(as.integer(config$years$last %||% config$last_y))
  if (is.na(first) || is.na(last) || first > last) {
    stop("Invalid survey population year range in config.", call. = FALSE)
  }
  seq.int(first, last)
}

survey_pop_countries <- function(root, contract) {
  config <- survey_pop_config(root, contract)
  toupper(as.character(config$countries %||% character()))
}

survey_pop_paths <- function(root, contract) {
  paths <- contract$paths %||% list()
  list(
    canonical_surveys = survey_pop_path(paths$canonical_surveys %||% "input_data/surveys_CEPAL", root),
    incoming_surveys = survey_pop_path(paths$incoming_surveys %||% "input_data/_new/surveys", root),
    population = survey_pop_path(paths$population %||% "input_data/population/PopulationLatAm.dta", root),
    survey_pop = survey_pop_path(paths$survey_pop %||% "intermediary_data/population/SurveyPop.dta", root)
  )
}

survey_pop_output_root <- function(root, contract, output_dir = NULL, explore = FALSE) {
  rel <- output_dir %||% if (isTRUE(explore)) contract$explore_output_root else contract$output_root
  survey_pop_path(rel, root)
}

survey_pop_explore_paths <- function(root, contract, output_dir = NULL) {
  out <- survey_pop_output_root(root, contract, output_dir, explore = TRUE)
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"))
}

survey_pop_run_id <- function(prefix = "survey-include") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

survey_pop_confirm_id <- function(prefix = "survey-confirm") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

survey_pop_output_paths_for_run <- function(root, contract, output_dir = NULL, run_id = NULL) {
  base <- survey_pop_output_root(root, contract, output_dir, explore = FALSE)
  out <- if (is.null(run_id) || !nzchar(run_id)) base else file.path(base, "runs", run_id)
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"), staged_repo = file.path(out, "staged_repo"))
}

survey_pop_output_paths_for_confirm <- function(root, contract, output_dir = NULL, confirm_id = NULL) {
  base <- survey_pop_output_root(root, contract, output_dir, explore = FALSE)
  out <- file.path(base, "confirms", confirm_id %||% survey_pop_confirm_id())
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"), snapshots = file.path(out, "snapshots"))
}

survey_pop_hash_path <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(NA_character_)
  if (survey_pop_has("digest")) {
    return(digest::digest(file = path, algo = "sha256"))
  }
  unname(as.character(tools::md5sum(path)))
}

survey_pop_hash_algorithm <- function() {
  if (survey_pop_has("digest")) "sha256" else "md5"
}

survey_pop_copy_file <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  isTRUE(file.copy(from, to, overwrite = TRUE, copy.date = TRUE))
}

survey_pop_copy_path <- function(from, to) {
  if (!file.exists(from)) return("missing_source")
  if (dir.exists(from)) return("unsupported_directory_source")
  if (survey_pop_copy_file(from, to)) "staged" else "copy_failed"
}

survey_pop_survey_match <- function(path) {
  base <- basename(path)
  m <- regexec("^([A-Z]{3})_([0-9]{4})N\\.dta$", base, ignore.case = FALSE)
  hit <- regmatches(base, m)[[1]]
  if (length(hit) != 3L) return(list(country = NA_character_, year = NA_integer_, is_survey = FALSE))
  list(country = hit[[2]], year = as.integer(hit[[3]]), is_survey = TRUE)
}

survey_pop_list_files <- function(path) {
  if (!dir.exists(path)) return(character())
  list.files(path, recursive = TRUE, all.files = FALSE, full.names = TRUE, no.. = TRUE)
}

survey_pop_incoming_destination <- function(root, contract, path) {
  paths <- survey_pop_paths(root, contract)
  rel <- survey_pop_relative_path(path, paths$incoming_surveys)
  parts <- strsplit(rel, .Platform$file.sep, fixed = TRUE)[[1]]
  info <- survey_pop_survey_match(path)
  if (length(parts) > 1L && (grepl("^[A-Z]{3}$", parts[[1L]]) || startsWith(parts[[1L]], "_"))) {
    return(file.path("input_data", "surveys_CEPAL", rel))
  }
  if (isTRUE(info$is_survey)) {
    return(file.path("input_data", "surveys_CEPAL", info$country, basename(path)))
  }
  file.path("input_data", "surveys_CEPAL", rel)
}

survey_pop_file_vars <- function(path) {
  if (!file.exists(path) || dir.exists(path) || !grepl("\\.dta$", path, ignore.case = TRUE)) {
    return(character())
  }
  survey_pop_need("haven")
  data <- tryCatch(haven::read_dta(path, n_max = 0), error = function(e) e)
  if (inherits(data, "error")) character() else names(data)
}

survey_pop_inventory_row <- function(root, contract, source_set, path) {
  paths <- survey_pop_paths(root, contract)
  info <- survey_pop_survey_match(path)
  rel <- survey_pop_relative_path(path, root)
  destination <- if (identical(source_set, "incoming")) survey_pop_incoming_destination(root, contract, path) else rel
  vars <- survey_pop_file_vars(path)
  required <- if (identical(info$country, "ARG")) c("_fep", "edad", "id_hogar") else c("_fep", "edad")
  missing_required <- if (isTRUE(info$is_survey)) setdiff(required, vars) else character()
  stat <- if (!file.exists(path)) {
    "missing"
  } else if (!isTRUE(info$is_survey)) {
    "non_survey_file"
  } else if (length(missing_required)) {
    "missing_required_columns"
  } else {
    "ok"
  }
  data.frame(
    source_id = "surveys-cepal",
    source_set = source_set,
    file = normalizePath(path, mustWork = FALSE),
    rel = rel,
    destination = destination,
    country = info$country,
    year = info$year,
    is_survey = isTRUE(info$is_survey),
    status = stat,
    missing_required_columns = paste(missing_required, collapse = ","),
    size = if (file.exists(path)) as.numeric(file.info(path)$size[[1L]]) else NA_real_,
    mtime = if (file.exists(path)) as.character(file.info(path)$mtime[[1L]]) else NA_character_,
    stringsAsFactors = FALSE
  )
}

survey_pop_source_inventory <- function(root, contract) {
  paths <- survey_pop_paths(root, contract)
  canonical <- survey_pop_list_files(paths$canonical_surveys)
  incoming <- survey_pop_list_files(paths$incoming_surveys)
  survey_pop_bind(c(
    lapply(canonical, survey_pop_inventory_row, root = root, contract = contract, source_set = "canonical"),
    lapply(incoming, survey_pop_inventory_row, root = root, contract = contract, source_set = "incoming")
  ))
}

survey_pop_year_label <- function(years) {
  years <- sort(unique(as.integer(years[!is.na(years)])))
  if (!length(years)) return("")
  breaks <- c(TRUE, diff(years) != 1L)
  groups <- split(years, cumsum(breaks))
  paste(vapply(groups, function(group) {
    if (length(group) == 1L) as.character(group[[1L]]) else sprintf("%s-%s", group[[1L]], group[[length(group)]])
  }, character(1)), collapse = ",")
}

survey_pop_year_coverage <- function(inventory, countries, years) {
  rows <- lapply(countries, function(country) {
    canonical <- inventory[inventory$source_set == "canonical" & inventory$is_survey & inventory$country == country, , drop = FALSE]
    incoming <- inventory[inventory$source_set == "incoming" & inventory$is_survey & inventory$country == country, , drop = FALSE]
    effective <- sort(unique(c(canonical$year, incoming$year)))
    data.frame(
      country = country,
      configured_years = survey_pop_year_label(years),
      canonical_years = survey_pop_year_label(canonical$year),
      incoming_years = survey_pop_year_label(incoming$year),
      effective_years = survey_pop_year_label(effective),
      missing_configured_years = survey_pop_year_label(setdiff(years, effective)),
      survey_files = length(effective),
      stringsAsFactors = FALSE
    )
  })
  survey_pop_bind(rows)
}

survey_pop_variable_report <- function(inventory, countries, years) {
  surveys <- inventory[inventory$is_survey & inventory$country %in% countries & inventory$year %in% years, , drop = FALSE]
  if (!nrow(surveys)) {
    return(data.frame(source_id = character(), source_set = character(), country = character(), year = integer(), rel = character(), status = character(), missing_required_columns = character(), severity = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    source_id = surveys$source_id,
    source_set = surveys$source_set,
    country = surveys$country,
    year = surveys$year,
    rel = surveys$rel,
    status = surveys$status,
    missing_required_columns = surveys$missing_required_columns,
    severity = ifelse(surveys$status == "missing_required_columns", "blocked", "info"),
    stringsAsFactors = FALSE
  )
}

survey_pop_latest_mtime <- function(paths) {
  paths <- unique(paths[file.exists(paths)])
  if (!length(paths)) return(as.POSIXct(NA))
  max(file.info(paths)$mtime, na.rm = TRUE)
}

survey_pop_status <- function(root, contract, inventory) {
  paths <- survey_pop_paths(root, contract)
  survey_pop <- paths$survey_pop
  inventory_files <- if ("file" %in% names(inventory)) inventory$file else character()
  inventory_source_sets <- if ("source_set" %in% names(inventory)) inventory$source_set else character()
  input_files <- c(
    inventory_files[file.exists(inventory_files)],
    paths$population,
    contract$contract_path,
    file.path(root, "config", "dina.yml"),
    file.path(root, "code", "R", "source-diagnostics", "survey_population_include.R")
  )
  latest_input <- survey_pop_latest_mtime(input_files)
  existing <- file.exists(survey_pop) && !dir.exists(survey_pop)
  incoming_count <- sum(inventory_source_sets == "incoming", na.rm = TRUE)
  output_mtime <- if (existing) file.info(survey_pop)$mtime[[1L]] else as.POSIXct(NA)
  status <- if (!existing) {
    "missing"
  } else if (incoming_count > 0L) {
    "stale_incoming_pending"
  } else if (!is.na(latest_input) && latest_input > output_mtime) {
    "stale"
  } else {
    "current"
  }
  data.frame(
    artifact = "SurveyPop.dta",
    rel = survey_pop_relative_path(survey_pop, root),
    exists = existing,
    status = status,
    severity = if (status %in% c("missing", "stale", "stale_incoming_pending")) "blocked" else "info",
    latest_input_mtime = as.character(latest_input),
    output_mtime = as.character(output_mtime),
    incoming_files = incoming_count,
    next_command = if (status %in% c("missing", "stale", "stale_incoming_pending")) "dina sources include surveys --dry-run" else "",
    detail = if (status %in% c("missing", "stale", "stale_incoming_pending")) "SurveyPop.dta should be regenerated through the surveys include workflow." else "SurveyPop.dta is present and not older than survey-population inputs.",
    stringsAsFactors = FALSE
  )
}

survey_pop_review_actions <- function(status, variable_report) {
  blocked_vars <- if (nrow(variable_report)) variable_report[variable_report$severity == "blocked", , drop = FALSE] else data.frame()
  rows <- list()
  if (nrow(blocked_vars)) {
    rows[[length(rows) + 1L]] <- data.frame(
      source_id = "surveys-cepal",
      action = "review_survey_variables",
      severity = "blocked",
      next_command = "dina sources table surveys variable_report",
      detail = "Some survey files are missing _fep, edad, or ARG id_hogar.",
      stringsAsFactors = FALSE
    )
  }
  if (nrow(status) && status$severity[[1L]] == "blocked") {
    rows[[length(rows) + 1L]] <- data.frame(
      source_id = "surveys-cepal",
      action = status$status[[1L]],
      severity = status$severity[[1L]],
      next_command = status$next_command[[1L]],
      detail = status$detail[[1L]],
      stringsAsFactors = FALSE
    )
  } else if (!nrow(blocked_vars)) {
    rows[[length(rows) + 1L]] <- data.frame(
      source_id = "surveys-cepal",
      action = "no_action",
      severity = "info",
      next_command = "",
      detail = "No survey-population action is needed.",
      stringsAsFactors = FALSE
    )
  }
  survey_pop_bind(rows)
}

survey_pop_manifest <- function(run_id, source_type, workflow, status, countries, years, dry_run = FALSE) {
  data.frame(
    key = c("run_id", "source_type", "workflow", "status", "dry_run", "countries", "years"),
    value = c(run_id, source_type, workflow, status, as.character(isTRUE(dry_run)), paste(countries, collapse = ","), survey_pop_year_label(years)),
    stringsAsFactors = FALSE
  )
}

survey_pop_overlay_paths <- function(root, contract, inventory, countries, years) {
  surveys <- inventory[inventory$is_survey & inventory$country %in% countries & inventory$year %in% years, , drop = FALSE]
  if (!nrow(surveys)) {
    return(data.frame(country = character(), year = integer(), path = character(), source_set = character(), rel = character(), stringsAsFactors = FALSE))
  }
  surveys$key <- paste(surveys$country, surveys$year, sep = "\r")
  surveys$rank <- ifelse(surveys$source_set == "incoming", 2L, 1L)
  surveys <- surveys[order(surveys$key, surveys$rank), , drop = FALSE]
  surveys <- surveys[!duplicated(surveys$key, fromLast = TRUE), , drop = FALSE]
  data.frame(country = surveys$country, year = surveys$year, path = surveys$file, source_set = surveys$source_set, rel = surveys$rel, stringsAsFactors = FALSE)
}

survey_pop_read_population_targets <- function(root, contract) {
  paths <- survey_pop_paths(root, contract)
  if (!file.exists(paths$population)) {
    return(data.frame(country = character(), year = integer(), totalpop = numeric(), stringsAsFactors = FALSE))
  }
  survey_pop_need("haven")
  pop <- as.data.frame(haven::read_dta(paths$population), stringsAsFactors = FALSE)
  if (!all(c("country", "year", "totalpop") %in% names(pop))) {
    return(data.frame(country = character(), year = integer(), totalpop = numeric(), stringsAsFactors = FALSE))
  }
  data.frame(
    country = trimws(as.character(pop$country)),
    year = as.integer(pop$year),
    totalpop = as.numeric(pop$totalpop),
    stringsAsFactors = FALSE
  )
}

survey_pop_arg_target <- function(pop_targets, year) {
  hit <- pop_targets[pop_targets$country == "Argentina" & pop_targets$year == as.integer(year), , drop = FALSE]
  if (!nrow(hit)) return(NA_real_)
  hit$totalpop[[1L]]
}

survey_pop_read_survey <- function(path, country) {
  survey_pop_need("haven")
  survey_pop_need("tidyselect")
  cols <- if (identical(country, "ARG")) c("_fep", "edad", "id_hogar") else c("_fep", "edad")
  out <- tryCatch(as.data.frame(haven::read_dta(path, col_select = tidyselect::all_of(cols)), stringsAsFactors = FALSE), error = function(e) e)
  if (inherits(out, "error")) {
    return(list(ok = FALSE, data = data.frame(stringsAsFactors = FALSE), error = conditionMessage(out)))
  }
  list(ok = TRUE, data = out, error = "")
}

survey_pop_summarize_file <- function(path, country, year, pop_targets) {
  if (!file.exists(path)) {
    return(data.frame(country = country, year = year, totpop = NA_real_, pct_adults = NA_real_, adults = NA_real_, nonadults = NA_real_, source_status = "missing_file", detail = "Survey file is missing.", stringsAsFactors = FALSE))
  }
  read <- survey_pop_read_survey(path, country)
  if (!isTRUE(read$ok)) {
    return(data.frame(country = country, year = year, totpop = NA_real_, pct_adults = NA_real_, adults = NA_real_, nonadults = NA_real_, source_status = "read_failed", detail = read$error, stringsAsFactors = FALSE))
  }
  data <- read$data
  fep <- suppressWarnings(round(as.numeric(data[["_fep"]])))
  edad <- suppressWarnings(as.numeric(data[["edad"]]))
  weight <- fep
  status <- "ok"
  detail <- ""
  if (identical(country, "ARG")) {
    id <- data[["id_hogar"]]
    keep <- !is.na(weight) & weight != 0
    data <- data[keep, , drop = FALSE]
    weight <- weight[keep]
    edad <- edad[keep]
    id <- id[keep]
    if (length(weight)) {
      weight <- ave(weight, id, FUN = function(z) mean(z, na.rm = TRUE))
    }
    keep <- !is.na(weight) & weight >= 1
    weight <- weight[keep]
    edad <- edad[keep]
    target <- survey_pop_arg_target(pop_targets, year)
    total_weight <- sum(weight, na.rm = TRUE)
    if (is.na(target) || target <= 0) {
      status <- "missing_arg_population_target"
      detail <- "Argentina totalpop target is missing from PopulationLatAm.dta."
      weight <- rep(NA_real_, length(weight))
    } else if (!length(weight) || total_weight <= 0) {
      status <- "empty_arg_weight"
      detail <- "Argentina survey has no positive weight after cleaning."
      weight <- rep(NA_real_, length(weight))
    } else {
      weight <- weight * target / total_weight
    }
  }
  adult <- !is.na(edad) & edad > 19
  adults <- sum(weight[adult], na.rm = TRUE)
  nonadults <- sum(weight[!adult], na.rm = TRUE)
  totpop <- adults + nonadults
  if (!is.finite(totpop) || totpop <= 0 || !identical(status, "ok")) {
    totpop <- NA_real_
    adults <- NA_real_
    nonadults <- NA_real_
  }
  pct_adults <- if (!is.na(totpop) && totpop > 0) adults / totpop * 100 else NA_real_
  data.frame(country = country, year = year, totpop = totpop, pct_adults = pct_adults, adults = adults, nonadults = nonadults, source_status = status, detail = detail, stringsAsFactors = FALSE)
}

survey_pop_linear_interpolate <- function(years, values) {
  out <- rep(NA_real_, length(years))
  ok <- !is.na(values)
  if (!any(ok)) return(out)
  if (sum(ok) == 1L) {
    out[years == years[ok][[1L]]] <- values[ok][[1L]]
    return(out)
  }
  stats::approx(x = years[ok], y = values[ok], xout = years, method = "linear", rule = 1)$y
}

survey_pop_linear_extrapolate <- function(years, values) {
  out <- survey_pop_linear_interpolate(years, values)
  ok <- !is.na(values)
  if (!any(ok)) return(out)
  if (sum(ok) == 1L) {
    out[] <- values[ok][[1L]]
    return(out)
  }
  x <- years[ok]
  y <- values[ok]
  left <- years < min(x)
  right <- years > max(x)
  if (any(left)) {
    slope <- (y[[2L]] - y[[1L]]) / (x[[2L]] - x[[1L]])
    out[left] <- y[[1L]] + slope * (years[left] - x[[1L]])
  }
  if (any(right)) {
    n <- length(x)
    slope <- (y[[n]] - y[[n - 1L]]) / (x[[n]] - x[[n - 1L]])
    out[right] <- y[[n]] + slope * (years[right] - x[[n]])
  }
  out
}

survey_pop_add_interpolation <- function(popdata, countries, years) {
  rows <- lapply(countries, function(country) {
    part <- popdata[popdata$country == country, , drop = FALSE]
    grid <- data.frame(country = country, year = years, stringsAsFactors = FALSE)
    part <- merge(grid, part, by = c("country", "year"), all.x = TRUE, sort = FALSE)
    part <- part[order(part$year), , drop = FALSE]
    part$totpop_i <- survey_pop_linear_interpolate(part$year, part$totpop)
    part$pct_adults_i <- survey_pop_linear_interpolate(part$year, part$pct_adults)
    part$totpop_ie <- survey_pop_linear_extrapolate(part$year, part$totpop)
    part$pct_adults_ie <- survey_pop_linear_extrapolate(part$year, part$pct_adults)
    part
  })
  out <- survey_pop_bind(rows)
  out[, c("country", "year", "totpop", "pct_adults", "adults", "nonadults", "totpop_i", "pct_adults_i", "totpop_ie", "pct_adults_ie"), drop = FALSE]
}

survey_pop_build_candidate <- function(root, contract, inventory = NULL) {
  inventory <- inventory %||% survey_pop_source_inventory(root, contract)
  countries <- survey_pop_countries(root, contract)
  years <- survey_pop_years(root, contract)
  overlay <- survey_pop_overlay_paths(root, contract, inventory, countries, years)
  pop_targets <- survey_pop_read_population_targets(root, contract)
  rows <- list()
  source_rows <- list()
  for (country in countries) {
    for (year in years) {
      hit <- overlay[overlay$country == country & overlay$year == year, , drop = FALSE]
      if (nrow(hit)) {
        row <- survey_pop_summarize_file(hit$path[[1L]], country, year, pop_targets)
        row$source_set <- hit$source_set[[1L]]
        row$source_rel <- hit$rel[[1L]]
      } else {
        row <- data.frame(country = country, year = year, totpop = NA_real_, pct_adults = NA_real_, adults = NA_real_, nonadults = NA_real_, source_status = "missing_file", detail = "No canonical or incoming survey file for this country-year.", source_set = "", source_rel = "", stringsAsFactors = FALSE)
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  raw <- survey_pop_bind(rows)
  data <- survey_pop_add_interpolation(raw, countries, years)
  status <- raw[, c("country", "year", "source_status", "detail", "source_set", "source_rel"), drop = FALSE]
  no_data_countries <- vapply(countries, function(country) all(is.na(raw$totpop[raw$country == country])), logical(1))
  country_status <- data.frame(
    country = countries,
    observed_years = vapply(countries, function(country) survey_pop_year_label(raw$year[raw$country == country & !is.na(raw$totpop)]), character(1)),
    status = ifelse(no_data_countries, "blocked_no_observed_surveys", "ok"),
    severity = ifelse(no_data_countries, "blocked", "info"),
    stringsAsFactors = FALSE
  )
  list(data = data, source_status = status, country_status = country_status)
}

survey_pop_empty_promotion_plan <- function() {
  data.frame(source_id = character(), artifact_type = character(), from_rel = character(), to_rel = character(), promotion_scope = character(), stringsAsFactors = FALSE)
}

survey_pop_stage_sources <- function(root, contract, paths, inventory) {
  incoming <- inventory[inventory$source_set == "incoming", , drop = FALSE]
  if (!nrow(incoming)) {
    return(data.frame(source_id = character(), source_set = character(), from_rel = character(), to_rel = character(), staged_to = character(), validation_status = character(), copy_status = character(), stringsAsFactors = FALSE))
  }
  rows <- lapply(seq_len(nrow(incoming)), function(i) {
    row <- incoming[i, , drop = FALSE]
    staged_to <- file.path(paths$staged_repo, row$destination[[1L]])
    copy_status <- survey_pop_copy_path(row$file[[1L]], staged_to)
    data.frame(
      source_id = row$source_id,
      source_set = row$source_set,
      from_rel = survey_pop_relative_path(row$file[[1L]], root),
      to_rel = row$destination,
      staged_to = staged_to,
      validation_status = row$status,
      copy_status = copy_status,
      stringsAsFactors = FALSE
    )
  })
  survey_pop_bind(rows)
}

survey_pop_write_candidate <- function(candidate, paths, root) {
  out <- file.path(paths$staged_repo, "intermediary_data", "population", "SurveyPop.dta")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  survey_pop_need("haven")
  haven::write_dta(candidate$data, out)
  out
}

survey_pop_read_current <- function(root, contract) {
  paths <- survey_pop_paths(root, contract)
  if (!file.exists(paths$survey_pop)) return(data.frame(stringsAsFactors = FALSE))
  survey_pop_need("haven")
  as.data.frame(haven::read_dta(paths$survey_pop), stringsAsFactors = FALSE)
}

survey_pop_diff_value <- function(candidate, current, col) {
  cval <- candidate[[paste0(col, "_candidate")]]
  oval <- current[[paste0(col, "_current")]]
  cval - oval
}

survey_pop_comparison <- function(candidate, current) {
  if (!nrow(current)) {
    out <- candidate
    out$comparison_status <- "no_current_survey_pop"
    return(out)
  }
  key_cols <- c("country", "year")
  value_cols <- intersect(c("totpop", "pct_adults", "adults", "nonadults", "totpop_ie", "pct_adults_ie"), intersect(names(candidate), names(current)))
  cand <- candidate[, c(key_cols, value_cols), drop = FALSE]
  cur <- current[, c(key_cols, value_cols), drop = FALSE]
  names(cand)[match(value_cols, names(cand))] <- paste0(value_cols, "_candidate")
  names(cur)[match(value_cols, names(cur))] <- paste0(value_cols, "_current")
  out <- merge(cand, cur, by = key_cols, all = TRUE, sort = FALSE)
  for (col in value_cols) {
    out[[paste0(col, "_diff")]] <- out[[paste0(col, "_candidate")]] - out[[paste0(col, "_current")]]
  }
  out$comparison_status <- ifelse(is.na(out$totpop_current), "candidate_only", ifelse(is.na(out$totpop_candidate), "current_only", "overlap"))
  out[order(out$country, out$year), , drop = FALSE]
}

survey_pop_source_fingerprints <- function(root, mappings) {
  incoming <- mappings[mappings$source_set == "incoming" & mappings$copy_status == "staged", , drop = FALSE]
  if (!nrow(incoming)) {
    return(data.frame(source_id = character(), source_set = character(), rel = character(), exists = logical(), kind = character(), size = numeric(), mtime = character(), hash_algorithm = character(), hash = character(), stringsAsFactors = FALSE))
  }
  algo <- survey_pop_hash_algorithm()
  rows <- lapply(seq_len(nrow(incoming)), function(i) {
    row <- incoming[i, , drop = FALSE]
    path <- file.path(root, row$from_rel[[1L]])
    exists <- file.exists(path) && !dir.exists(path)
    info <- if (exists) file.info(path) else data.frame(size = NA_real_, mtime = as.POSIXct(NA))
    data.frame(source_id = row$source_id, source_set = row$source_set, rel = row$from_rel, exists = exists, kind = if (exists) "file" else "missing", size = if (exists) as.numeric(info$size[[1L]]) else NA_real_, mtime = if (exists) as.character(info$mtime[[1L]]) else NA_character_, hash_algorithm = algo, hash = if (exists) survey_pop_hash_path(path) else NA_character_, stringsAsFactors = FALSE)
  })
  survey_pop_bind(rows)
}

survey_pop_promotion_plan <- function(mappings, candidate_path) {
  staged_sources <- mappings[mappings$copy_status == "staged", , drop = FALSE]
  source_plan <- if (nrow(staged_sources)) {
    data.frame(source_id = staged_sources$source_id, artifact_type = "survey_source", from_rel = staged_sources$staged_to, to_rel = staged_sources$to_rel, promotion_scope = "promote", stringsAsFactors = FALSE)
  } else {
    survey_pop_empty_promotion_plan()
  }
  candidate_plan <- data.frame(source_id = "surveys-cepal", artifact_type = "survey_population", from_rel = candidate_path, to_rel = file.path("intermediary_data", "population", "SurveyPop.dta"), promotion_scope = "promote", stringsAsFactors = FALSE)
  survey_pop_bind(source_plan, candidate_plan)
}

survey_pop_promotion_fingerprints <- function(promotion_plan) {
  if (!nrow(promotion_plan)) {
    return(data.frame(source_id = character(), artifact_type = character(), from_rel = character(), to_rel = character(), exists = logical(), kind = character(), hash_algorithm = character(), hash = character(), stringsAsFactors = FALSE))
  }
  algo <- survey_pop_hash_algorithm()
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    exists <- file.exists(from) && !dir.exists(from)
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from_rel = from, to_rel = row$to_rel, exists = exists, kind = if (exists) "file" else "missing", hash_algorithm = algo, hash = if (exists) survey_pop_hash_path(from) else NA_character_, stringsAsFactors = FALSE)
  })
  survey_pop_bind(rows)
}

survey_pop_include_summary <- function(candidate, variable_report, mappings, promotion_plan) {
  blocked_vars <- if (nrow(variable_report)) sum(variable_report$severity == "blocked", na.rm = TRUE) else 0L
  blocked_countries <- if (nrow(candidate$country_status)) sum(candidate$country_status$severity == "blocked", na.rm = TRUE) else 0L
  copy_blocked <- if (nrow(mappings)) sum(!mappings$copy_status %in% "staged", na.rm = TRUE) else 0L
  blocked <- blocked_vars + blocked_countries + copy_blocked
  data.frame(
    source_id = "surveys-cepal",
    status = if (blocked > 0L || !nrow(promotion_plan)) "blocked" else "all_good",
    candidate_rows = nrow(candidate$data),
    observed_country_years = sum(!is.na(candidate$data$totpop)),
    staged_sources = if (nrow(mappings)) sum(mappings$copy_status == "staged", na.rm = TRUE) else 0L,
    promotions = nrow(promotion_plan),
    warnings = 0L,
    blocked = blocked,
    stringsAsFactors = FALSE
  )
}

survey_pop_overall_status <- function(summary) {
  if (!nrow(summary)) return("blocked")
  if (any(summary$status == "blocked", na.rm = TRUE)) return("blocked")
  "all_good"
}

run_survey_pop_explorer <- function(
  root = survey_pop_repo_root(),
  contract_path = file.path(root, "config", "survey_population_include.yml"),
  output_dir = NULL,
  countries = NULL,
  write_outputs = TRUE,
  dry_run = FALSE
) {
  contract <- survey_pop_read_contract(root, contract_path)
  all_countries <- survey_pop_countries(root, contract)
  if (!is.null(countries)) {
    all_countries <- intersect(all_countries, toupper(countries))
  }
  years <- survey_pop_years(root, contract)
  paths <- survey_pop_explore_paths(root, contract, output_dir)
  inventory <- survey_pop_source_inventory(root, contract)
  coverage <- survey_pop_year_coverage(inventory, all_countries, years)
  variable_report <- survey_pop_variable_report(inventory, all_countries, years)
  status <- survey_pop_status(root, contract, inventory)
  actions <- survey_pop_review_actions(status, variable_report)
  overall <- if (any(actions$severity == "blocked", na.rm = TRUE)) "blocked" else "all_good"
  tables <- list(
    source_inventory = inventory,
    year_coverage = coverage,
    variable_report = variable_report,
    survey_pop_status = status,
    review_actions = actions,
    explore_manifest = survey_pop_manifest("explore", "surveys", "survey_population", overall, all_countries, years, dry_run = dry_run)
  )
  if (isTRUE(write_outputs)) {
    survey_pop_write_csvs(tables, paths, "explore_manifest")
  }
  list(paths = paths, outputs = tables, manifest = tables$explore_manifest, contract = contract, countries = all_countries, years = years, status = overall)
}

survey_pop_manifest_value <- function(manifest, key) {
  if (!nrow(manifest) || !("key" %in% names(manifest)) || !("value" %in% names(manifest))) return("")
  hit <- manifest$key == key
  if (!any(hit)) "" else as.character(manifest$value[which(hit)[[1L]]])
}

survey_pop_exploration_root <- function(root, contract, exploration_run = NULL) {
  path <- exploration_run %||% contract$explore_output_root
  normalizePath(survey_pop_path(path, root), mustWork = FALSE)
}

survey_pop_read_exploration <- function(root, contract, exploration_run = NULL) {
  run <- survey_pop_exploration_root(root, contract, exploration_run)
  tables <- file.path(run, "tables")
  logs <- file.path(run, "logs")
  list(
    root = run,
    source_inventory = survey_pop_read_csv(file.path(tables, "source_inventory.csv")),
    year_coverage = survey_pop_read_csv(file.path(tables, "year_coverage.csv")),
    variable_report = survey_pop_read_csv(file.path(tables, "variable_report.csv")),
    survey_pop_status = survey_pop_read_csv(file.path(tables, "survey_pop_status.csv")),
    review_actions = survey_pop_read_csv(file.path(tables, "review_actions.csv")),
    explore_manifest = survey_pop_read_csv(file.path(logs, "explore_manifest.csv"))
  )
}

survey_pop_include_manifest <- function(run_id, status, exploration_root, countries, years) {
  data.frame(
    key = c("run_id", "source_type", "workflow", "status", "dry_run", "exploration_run", "countries", "years"),
    value = c(run_id, "surveys", "survey_population", status, "TRUE", normalizePath(exploration_root, mustWork = FALSE), paste(countries, collapse = ","), survey_pop_year_label(years)),
    stringsAsFactors = FALSE
  )
}

run_survey_pop_include <- function(
  root = survey_pop_repo_root(),
  contract_path = file.path(root, "config", "survey_population_include.yml"),
  exploration_run = NULL,
  output_dir = NULL,
  write_outputs = TRUE,
  run_id = NULL
) {
  contract <- survey_pop_read_contract(root, contract_path)
  run_id <- run_id %||% survey_pop_run_id()
  paths <- survey_pop_output_paths_for_run(root, contract, output_dir, run_id)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$staged_repo, recursive = TRUE, showWarnings = FALSE)
  exploration <- survey_pop_read_exploration(root, contract, exploration_run)
  if (!nrow(exploration$source_inventory)) {
    fresh <- run_survey_pop_explorer(root = root, contract_path = contract_path, write_outputs = FALSE, dry_run = TRUE)
    exploration <- c(list(root = fresh$paths$root), fresh$outputs)
  }
  countries <- survey_pop_countries(root, contract)
  years <- survey_pop_years(root, contract)
  mappings <- survey_pop_stage_sources(root, contract, paths, exploration$source_inventory)
  candidate <- survey_pop_build_candidate(root, contract, exploration$source_inventory)
  candidate_path <- survey_pop_write_candidate(candidate, paths, root)
  current <- survey_pop_read_current(root, contract)
  comparison <- survey_pop_comparison(candidate$data, current)
  promotion_plan <- survey_pop_promotion_plan(mappings, candidate_path)
  source_fingerprints <- survey_pop_source_fingerprints(root, mappings)
  promotion_fingerprints <- survey_pop_promotion_fingerprints(promotion_plan)
  summary <- survey_pop_include_summary(candidate, exploration$variable_report, mappings, promotion_plan)
  status <- survey_pop_overall_status(summary)
  manifest <- survey_pop_include_manifest(run_id, status, exploration$root, countries, years)
  include_detail <- survey_pop_bind(
    exploration$variable_report,
    candidate$source_status,
    candidate$country_status
  )
  outputs <- list(
    include_summary = summary,
    include_detail = include_detail,
    staged_source_mappings = mappings,
    candidate_source_status = candidate$source_status,
    candidate_country_status = candidate$country_status,
    survey_pop_comparison = comparison,
    promotion_plan = promotion_plan,
    source_fingerprints = source_fingerprints,
    promotion_fingerprints = promotion_fingerprints,
    include_manifest = manifest
  )
  if (isTRUE(write_outputs)) {
    survey_pop_write_csvs(outputs, paths, "include_manifest")
  }
  list(paths = paths, outputs = outputs, manifest = manifest, contract = contract, run_id = run_id)
}

survey_pop_resolve_run <- function(root, contract, include_run) {
  if (is.null(include_run) || !nzchar(include_run)) {
    stop("--include-run is required for survey confirm.", call. = FALSE)
  }
  candidates <- normalizePath(c(include_run, file.path(root, include_run), file.path(survey_pop_output_root(root, contract), "runs", include_run)), mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, "logs", "include_manifest.csv"))]
  if (!length(hit)) stop("Survey include run not found: ", include_run, call. = FALSE)
  hit[[1L]]
}

survey_pop_resolve_confirm <- function(root, contract, confirm_run) {
  if (is.null(confirm_run) || !nzchar(confirm_run)) stop("Confirm run is required for survey restore.", call. = FALSE)
  candidates <- normalizePath(c(confirm_run, file.path(root, confirm_run), file.path(survey_pop_output_root(root, contract), "confirms", confirm_run)), mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, "logs", "confirm_manifest.csv"))]
  if (!length(hit)) stop("Survey confirm run not found: ", confirm_run, call. = FALSE)
  hit[[1L]]
}

survey_pop_verify_source_fingerprints <- function(root, include_run, mappings) {
  expected <- survey_pop_read_csv(file.path(include_run, "tables", "source_fingerprints.csv"))
  incoming <- mappings[mappings$source_set == "incoming" & mappings$copy_status == "staged", , drop = FALSE]
  if (!nrow(incoming)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(incoming)), function(i) {
    row <- incoming[i, , drop = FALSE]
    rel <- row$from_rel[[1L]]
    path <- file.path(root, rel)
    hit <- expected[expected$source_set == "incoming" & expected$rel == rel, , drop = FALSE]
    dry_hash <- if (nrow(hit)) hit$hash[[1L]] else NA_character_
    current_hash <- if (file.exists(path)) survey_pop_hash_path(path) else NA_character_
    status <- if (!nrow(hit)) "missing_dry_run_fingerprint" else if (!file.exists(path)) "missing_current_source" else if (!identical(as.character(dry_hash), as.character(current_hash))) "hash_changed" else "ok"
    data.frame(source_id = row$source_id, rel = rel, dry_run_hash = dry_hash, current_hash = current_hash, status = status, stringsAsFactors = FALSE)
  })
  report <- survey_pop_bind(rows)
  if (nrow(report) && any(report$status != "ok", na.rm = TRUE)) {
    bad <- report[report$status != "ok", , drop = FALSE]
    stop("Confirm refused: incoming survey fingerprints changed since dry-run. Rerun the include dry-run. First mismatch: ", bad$rel[[1L]], " (", bad$status[[1L]], ").", call. = FALSE)
  }
  report
}

survey_pop_verify_promotion_fingerprints <- function(include_run, promotion_plan) {
  expected <- survey_pop_read_csv(file.path(include_run, "tables", "promotion_fingerprints.csv"))
  if (!nrow(promotion_plan)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    hit <- expected[expected$artifact_type == row$artifact_type & expected$to_rel == row$to_rel, , drop = FALSE]
    dry_hash <- if (nrow(hit)) hit$hash[[1L]] else NA_character_
    current_hash <- if (file.exists(from)) survey_pop_hash_path(from) else NA_character_
    status <- if (!nrow(hit)) "missing_dry_run_fingerprint" else if (!file.exists(from)) "missing_staged_artifact" else if (!identical(as.character(dry_hash), as.character(current_hash))) "hash_changed" else "ok"
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from = from, to = row$to_rel, dry_run_hash = dry_hash, current_hash = current_hash, status = status, stringsAsFactors = FALSE)
  })
  report <- survey_pop_bind(rows)
  if (nrow(report) && any(report$status != "ok", na.rm = TRUE)) {
    bad <- report[report$status != "ok", , drop = FALSE]
    stop("Confirm refused: staged survey artifacts changed since dry-run. Rerun the include dry-run. First mismatch: ", bad$to[[1L]], " (", bad$status[[1L]], ").", call. = FALSE)
  }
  report
}

survey_pop_confirm_manifest <- function(confirm_id, include_run, status) {
  data.frame(
    key = c("confirm_id", "source_type", "workflow", "status", "include_run", "confirmed_at"),
    value = c(confirm_id, "surveys", "survey_population", status, normalizePath(include_run, mustWork = FALSE), as.character(Sys.time())),
    stringsAsFactors = FALSE
  )
}

survey_pop_confirm_sources <- function(
  root = survey_pop_repo_root(),
  contract_path = file.path(root, "config", "survey_population_include.yml"),
  include_run = NULL,
  output_dir = NULL
) {
  contract <- survey_pop_read_contract(root, contract_path)
  include_run <- survey_pop_resolve_run(root, contract, include_run)
  include_manifest <- survey_pop_read_csv(file.path(include_run, "logs", "include_manifest.csv"))
  status <- survey_pop_manifest_value(include_manifest, "status")
  if (!identical(status, "all_good")) stop("Survey include run is not clean; status is ", status, ".", call. = FALSE)
  mappings <- survey_pop_read_csv(file.path(include_run, "tables", "staged_source_mappings.csv"))
  promotion_plan <- survey_pop_read_csv(file.path(include_run, "tables", "promotion_plan.csv"))
  if (!nrow(promotion_plan)) stop("Survey include run has no staged artifact to promote.", call. = FALSE)
  source_check <- survey_pop_verify_source_fingerprints(root, include_run, mappings)
  staged_check <- survey_pop_verify_promotion_fingerprints(include_run, promotion_plan)
  confirm_id <- survey_pop_confirm_id()
  paths <- survey_pop_output_paths_for_confirm(root, contract, output_dir, confirm_id)
  report <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    to <- file.path(root, row$to_rel[[1L]])
    backup <- file.path(paths$snapshots, "original", row$to_rel[[1L]])
    backup_status <- if (file.exists(to)) survey_pop_copy_path(to, backup) else "destination_absent"
    promote_status <- survey_pop_copy_path(from, to)
    data.frame(
      source_id = row$source_id,
      artifact_type = row$artifact_type,
      from = from,
      to = row$to_rel,
      backup = if (identical(backup_status, "destination_absent")) "" else survey_pop_relative_path(backup, root),
      backup_status = if (identical(backup_status, "staged")) "backed_up" else backup_status,
      promote_status = promote_status,
      stringsAsFactors = FALSE
    )
  })
  promote_report <- survey_pop_bind(report)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(promote_report, file.path(paths$tables, "promote_report.csv"), row.names = FALSE, na = "")
  utils::write.csv(source_check, file.path(paths$tables, "source_fingerprint_check.csv"), row.names = FALSE, na = "")
  utils::write.csv(staged_check, file.path(paths$tables, "staged_artifact_fingerprint_check.csv"), row.names = FALSE, na = "")
  manifest <- survey_pop_confirm_manifest(confirm_id, include_run, "confirmed")
  utils::write.csv(manifest, file.path(paths$logs, "confirm_manifest.csv"), row.names = FALSE, na = "")
  list(paths = paths, outputs = list(promote_report = promote_report, source_fingerprint_check = source_check, staged_artifact_fingerprint_check = staged_check), manifest = manifest, contract = contract)
}

survey_pop_restore_sources <- function(
  root = survey_pop_repo_root(),
  contract_path = file.path(root, "config", "survey_population_include.yml"),
  confirm_run = NULL
) {
  contract <- survey_pop_read_contract(root, contract_path)
  confirm_run <- survey_pop_resolve_confirm(root, contract, confirm_run)
  promote_report <- survey_pop_read_csv(file.path(confirm_run, "tables", "promote_report.csv"))
  rows <- lapply(seq_len(nrow(promote_report)), function(i) {
    row <- promote_report[i, , drop = FALSE]
    dest <- file.path(root, row$to[[1L]])
    if (identical(row$backup_status[[1L]], "backed_up")) {
      backup <- file.path(root, row$backup[[1L]])
      restore_status <- survey_pop_copy_path(backup, dest)
    } else if (identical(row$backup_status[[1L]], "destination_absent")) {
      if (file.exists(dest)) unlink(dest, recursive = TRUE)
      restore_status <- "removed_promoted_destination"
    } else {
      restore_status <- "no_backup_available"
    }
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, destination = row$to, restore_status = restore_status, stringsAsFactors = FALSE)
  })
  restore_report <- survey_pop_bind(rows)
  utils::write.csv(restore_report, file.path(confirm_run, "tables", "restore_report.csv"), row.names = FALSE, na = "")
  list(paths = list(root = confirm_run, restore_report = file.path(confirm_run, "tables", "restore_report.csv")), outputs = list(restore_report = restore_report), contract = contract)
}

survey_pop_table_run <- function(root, run = NULL) {
  path <- run %||% file.path(root, "output", "experiments", "survey_population_explore")
  normalizePath(if (grepl("^/", path)) path else file.path(root, path), mustWork = FALSE)
}

survey_pop_table_file <- function(root, table, run = NULL) {
  table <- gsub("-", "_", table %||% "", fixed = TRUE)
  run <- survey_pop_table_run(root, run)
  candidates <- c(
    file.path(run, "tables", paste0(table, ".csv")),
    file.path(root, "output", "experiments", "survey_population_include", "runs", basename(run), "tables", paste0(table, ".csv"))
  )
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop("Survey source table not found: ", table, call. = FALSE)
  }
  hit[[1L]]
}

survey_pop_table_available <- function(root, run = NULL) {
  run <- survey_pop_table_run(root, run)
  candidates <- c(
    file.path(run, "tables"),
    file.path(root, "output", "experiments", "survey_population_include", "runs", basename(run), "tables")
  )
  tables <- character()
  for (dir in candidates[dir.exists(candidates)]) {
    tables <- c(tables, tools::file_path_sans_ext(basename(Sys.glob(file.path(dir, "*.csv")))))
  }
  unique(tables)
}

survey_pop_read_table <- function(root, table, run = NULL) {
  survey_pop_read_csv(survey_pop_table_file(root, table, run))
}

survey_pop_table_catalog <- function() {
  data.frame(
    table = c(
      "source_inventory",
      "year_coverage",
      "variable_report",
      "survey_pop_status",
      "review_actions",
      "explore_manifest",
      "include_summary",
      "include_detail",
      "staged_source_mappings",
      "candidate_source_status",
      "candidate_country_status",
      "survey_pop_comparison",
      "promotion_plan",
      "source_fingerprints",
      "promotion_fingerprints",
      "include_manifest"
    ),
    contents = c(
      "canonical and incoming survey files with parsed country/year and variable status",
      "configured, canonical, incoming, and effective survey-year coverage by country",
      "required _fep/edad/id_hogar availability by survey file",
      "current SurveyPop.dta missing/stale/current status",
      "review action and next command for survey population",
      "explore run metadata",
      "include dry-run status",
      "include dry-run validation and candidate detail",
      "incoming survey files copied into the staged repo",
      "country-year source status used to build the candidate",
      "country-level observed-year availability for candidate generation",
      "candidate versus current SurveyPop.dta comparison",
      "staged survey source and SurveyPop artifacts eligible for confirm promotion",
      "incoming source hashes captured at dry-run",
      "staged artifact hashes captured at dry-run",
      "include run metadata"
    ),
    stringsAsFactors = FALSE
  )
}
