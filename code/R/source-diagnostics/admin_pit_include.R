# Experimental PIT admin source include workflow.
#
# This staged workflow is isolated from the mainstream admin-data pipeline.
# Dry-runs stage raw/admin dependencies and candidate _clean outputs under
# output/experiments. Confirm/restore only promote reviewed staged artifacts.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

admin_pit_include_repo_root <- function(start = getwd()) {
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

admin_pit_include_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

admin_pit_include_path <- function(path, root) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  if (grepl("^/", path)) path else file.path(root, path)
}

admin_pit_include_relative_path <- function(path, root) {
  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
}

admin_pit_include_read_yaml <- function(path, default = NULL) {
  admin_pit_include_need("yaml")
  if (!file.exists(path)) {
    if (!is.null(default)) return(default)
    stop("Missing YAML file: ", path, call. = FALSE)
  }
  yaml::read_yaml(path)
}

admin_pit_include_load_explorer <- function(root) {
  if (!exists("run_admin_pit_explorer", mode = "function")) {
    source(file.path(root, "code", "R", "source-diagnostics", "admin_pit_explorer.R"), local = parent.frame())
  }
}

admin_pit_include_read_contract <- function(
  root = admin_pit_include_repo_root(),
  contract_path = file.path(root, "config", "admin_pit_include.yml")
) {
  contract <- admin_pit_include_read_yaml(contract_path)
  if (is.null(contract$output_root) || is.null(contract$source_ids)) {
    stop("Invalid PIT admin include contract: missing output_root or source_ids.", call. = FALSE)
  }
  contract$contract_path <- contract_path
  contract
}

admin_pit_include_supported_ids <- function(contract) {
  as.character(contract$source_ids %||% character())
}

admin_pit_include_output_root <- function(root, contract, output_dir = NULL) {
  admin_pit_include_path(output_dir %||% contract$output_root, root)
}

admin_pit_include_run_id <- function(prefix = "admin-pit-include") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

admin_pit_include_confirm_id <- function(prefix = "admin-pit-confirm") {
  paste0(prefix, "-", format(Sys.time(), "%Y%m%d-%H%M%S"))
}

admin_pit_include_output_paths_for_run <- function(root, contract, output_dir = NULL, run_id = NULL) {
  base <- admin_pit_include_output_root(root, contract, output_dir)
  out <- if (is.null(run_id) || !nzchar(run_id)) base else file.path(base, "runs", run_id)
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"), staged_repo = file.path(out, "staged_repo"))
}

admin_pit_include_output_paths_for_confirm <- function(root, contract, output_dir = NULL, confirm_id = NULL) {
  base <- admin_pit_include_output_root(root, contract, output_dir)
  out <- file.path(base, "confirms", confirm_id %||% admin_pit_include_confirm_id())
  list(root = out, tables = file.path(out, "tables"), logs = file.path(out, "logs"), snapshots = file.path(out, "snapshots"))
}

admin_pit_include_bind <- function(...) {
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

admin_pit_include_read_csv <- function(path) {
  if (!file.exists(path)) return(data.frame(stringsAsFactors = FALSE))
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

admin_pit_include_manifest_value <- function(manifest, key) {
  if (!nrow(manifest) || !("key" %in% names(manifest)) || !("value" %in% names(manifest))) return("")
  hit <- manifest$key == key
  if (!any(hit)) "" else as.character(manifest$value[which(hit)[[1L]]])
}

admin_pit_include_exploration_root <- function(root, contract, exploration_run = NULL) {
  path <- exploration_run %||% contract$explore_output_root %||% "output/experiments/admin_pit_explore"
  normalizePath(admin_pit_include_path(path, root), mustWork = FALSE)
}

admin_pit_include_read_exploration <- function(root, contract, exploration_run = NULL) {
  run <- admin_pit_include_exploration_root(root, contract, exploration_run)
  tables <- file.path(run, "tables")
  logs <- file.path(run, "logs")
  list(
    root = run,
    manifest = admin_pit_include_read_csv(file.path(logs, "explore_manifest.csv")),
    year_expectations = admin_pit_include_read_csv(file.path(tables, "year_expectations.csv")),
    source_inventory = admin_pit_include_read_csv(file.path(tables, "source_inventory.csv")),
    structure_summary = admin_pit_include_read_csv(file.path(tables, "structure_summary.csv")),
    source_fingerprints = admin_pit_include_read_csv(file.path(tables, "source_fingerprints.csv"))
  )
}

admin_pit_include_copy_file <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
  isTRUE(ok)
}

admin_pit_include_copy_dir <- function(from, to) {
  if (dir.exists(to)) unlink(to, recursive = TRUE)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  entries <- list.files(from, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE, include.dirs = TRUE)
  dirs <- entries[dir.exists(entries)]
  for (dir in dirs) {
    rel <- substring(dir, nchar(normalizePath(from, mustWork = FALSE)) + 2L)
    if (nzchar(rel)) dir.create(file.path(to, rel), recursive = TRUE, showWarnings = FALSE)
  }
  files <- entries[file.exists(entries) & !dir.exists(entries)]
  for (file in files) {
    rel <- substring(file, nchar(normalizePath(from, mustWork = FALSE)) + 2L)
    if (!admin_pit_include_copy_file(file, file.path(to, rel))) return(FALSE)
  }
  TRUE
}

admin_pit_include_copy_path <- function(from, to) {
  if (!file.exists(from)) return("missing_source")
  ok <- if (dir.exists(from)) admin_pit_include_copy_dir(from, to) else admin_pit_include_copy_file(from, to)
  if (isTRUE(ok)) "staged" else "copy_failed"
}

admin_pit_include_hash_path <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  admin_pit_include_need("digest")
  if (!dir.exists(path)) {
    return(digest::digest(file = path, algo = "sha256"))
  }
  files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) return(digest::digest("", algo = "sha256"))
  rel <- substring(files, nchar(normalizePath(path, mustWork = FALSE)) + 2L)
  lines <- sprintf("%s %s", rel, vapply(files, function(file) digest::digest(file = file, algo = "sha256"), character(1)))
  digest::digest(paste(sort(lines), collapse = "\n"), algo = "sha256")
}

admin_pit_include_source_values <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character())
  if (is.atomic(x)) return(as.character(x))
  unlist(x, use.names = FALSE)
}

admin_pit_include_registry <- function(root) {
  config <- admin_pit_include_read_yaml(file.path(root, "config", "sources.yml"), default = list(sources = list()))
  config$sources %||% list()
}

admin_pit_include_registry_source <- function(root, source_id) {
  registry <- admin_pit_include_registry(root)
  matches <- Filter(function(source) identical(source$id %||% "", source_id), registry)
  if (!length(matches)) NULL else matches[[1L]]
}

admin_pit_include_template <- function(x, values = list()) {
  if (is.null(x) || !length(x)) return("")
  out <- as.character(x[[1L]])
  for (name in names(values)) {
    out <- gsub(paste0("\\{", name, "\\}"), as.character(values[[name]]), out)
  }
  out
}

admin_pit_include_effective_config <- function(root, contract) {
  admin_pit_include_load_explorer(root)
  years <- contract$years %||% list()
  admin_pit_explorer_effective_config(
    root,
    years$from_config %||% "config/dina.yml",
    use_active_update_override = isTRUE(years$use_active_update_override %||% TRUE)
  )
}

admin_pit_include_config_path <- function(root, contract) {
  config <- admin_pit_include_effective_config(root, contract)
  normalizePath(attr(config, "config_path") %||% file.path(root, "config", "dina.yml"), mustWork = FALSE)
}

admin_pit_include_override_path <- function(root, contract) {
  config <- admin_pit_include_effective_config(root, contract)
  normalizePath(attr(config, "override_path") %||% "", mustWork = FALSE)
}

admin_pit_include_expected_outputs <- function(contract, source_id) {
  as.character((contract$cleaners$expected_outputs %||% list())[[source_id]] %||% character())
}

admin_pit_include_source_country <- function(source_id) {
  switch(source_id, "chl-pit-total" = "CHL", "bra-pit-total" = "BRA", "col-pit-total" = "COL", "")
}

admin_pit_include_survey_pop_dependency <- function(rel, dependency_id = "") {
  candidates <- gsub("\\\\", "/", c(rel %||% "", dependency_id %||% ""))
  any(candidates == "intermediary_data/population/SurveyPop.dta")
}

admin_pit_include_wid_population_dependency <- function(rel, dependency_id = "") {
  candidates <- gsub("\\\\", "/", c(rel %||% "", dependency_id %||% ""))
  any(candidates == "input_data/wid/population_total_adult_npopul.dta")
}

admin_pit_include_wid_population_status <- function(root, rel) {
  population <- admin_pit_include_path(rel, root)
  next_command <- "dina sources explore wid"
  guidance <- "Run `dina sources explore wid --fetch` if WID artifacts are missing or stale, then `dina sources include wid --dry-run`, then confirm a clean WID include run."
  if (!file.exists(population)) {
    return(list(
      status = "missing_static_dependency",
      severity = "blocked",
      next_command = next_command,
      detail = paste("Required WID population artifact is missing.", guidance)
    ))
  }
  inputs <- c(
    file.path(root, "config", "dina.yml"),
    file.path(root, "config", "wid_include.yml"),
    file.path(root, "code", "R", "source-diagnostics", "wid_common.R"),
    file.path(root, "code", "R", "source-diagnostics", "wid_explorer.R"),
    file.path(root, "code", "R", "source-diagnostics", "wid_include.R")
  )
  inputs <- inputs[file.exists(inputs)]
  if (length(inputs)) {
    latest_input <- max(file.info(inputs)$mtime, na.rm = TRUE)
    output_mtime <- file.info(population)$mtime[[1L]]
    if (!is.na(latest_input) && !is.na(output_mtime) && latest_input > output_mtime) {
      return(list(
        status = "stale_static_dependency",
        severity = "blocked",
        next_command = next_command,
        detail = paste("WID population artifact is older than the WID source workflow contract/code.", guidance)
      ))
    }
  }
  list(
    status = "carried_forward",
    severity = "info",
    next_command = "",
    detail = "WID population artifact is available for carry-forward staging."
  )
}

admin_pit_include_survey_pop_status <- function(root, rel) {
  survey_pop <- admin_pit_include_path(rel, root)
  next_command <- "dina sources explore surveys"
  guidance <- "Run `dina sources explore surveys`, then `dina sources include surveys --dry-run`, then confirm a clean surveys include run."
  if (!file.exists(survey_pop)) {
    return(list(
      status = "missing_static_dependency",
      severity = "blocked",
      next_command = next_command,
      detail = paste("Required SurveyPop.dta is missing.", guidance)
    ))
  }
  inputs <- c(
    Sys.glob(file.path(root, "input_data", "surveys_CEPAL", "*", "*.dta")),
    file.path(root, "input_data", "wid", "population_total_adult_npopul.dta"),
    file.path(root, "config", "dina.yml"),
    file.path(root, "config", "survey_population_include.yml"),
    file.path(root, "code", "R", "source-diagnostics", "survey_sources_include.R")
  )
  inputs <- inputs[file.exists(inputs)]
  if (length(inputs)) {
    latest_input <- max(file.info(inputs)$mtime, na.rm = TRUE)
    output_mtime <- file.info(survey_pop)$mtime[[1L]]
    if (!is.na(latest_input) && !is.na(output_mtime) && latest_input > output_mtime) {
      return(list(
        status = "stale_static_dependency",
        severity = "blocked",
        next_command = next_command,
        detail = paste("SurveyPop.dta is older than survey-source inputs.", guidance)
      ))
    }
  }
  list(
    status = "carried_forward",
    severity = "info",
    next_command = "",
    detail = "Static dependency is available for carry-forward staging."
  )
}

admin_pit_include_static_dependency_status <- function(root, rel, dependency_id, copy_status) {
  if (admin_pit_include_survey_pop_dependency(rel, dependency_id)) {
    return(admin_pit_include_survey_pop_status(root, "intermediary_data/population/SurveyPop.dta"))
  }
  if (admin_pit_include_wid_population_dependency(rel, dependency_id)) {
    return(admin_pit_include_wid_population_status(root, "input_data/wid/population_total_adult_npopul.dta"))
  }
  if (identical(copy_status, "missing_dependency")) {
    return(list(
      status = "missing_dependency",
      severity = "blocked",
      next_command = "",
      detail = "Required carry-forward dependency is missing from canonical paths."
    ))
  }
  list(
    status = if (identical(copy_status, "staged")) "carried_forward" else copy_status,
    severity = "info",
    next_command = "",
    detail = "Static dependency is available for carry-forward staging."
  )
}

admin_pit_include_stage_dependency_path <- function(root, paths, source_id, country, rel, dependency_type, dependency_id = "", promotion_scope = "carry_forward") {
  from <- admin_pit_include_path(rel, root)
  staged_to <- file.path(paths$staged_repo, rel)
  copy_status <- if (file.exists(from)) admin_pit_include_copy_path(from, staged_to) else "missing_dependency"
  dependency_id <- dependency_id %||% ""
  if (!nzchar(dependency_id)) dependency_id <- rel
  dependency_status <- if (identical(dependency_type, "static_dependency")) {
    admin_pit_include_static_dependency_status(root, rel, dependency_id, copy_status)
  } else {
    list(
      status = if (identical(copy_status, "staged")) "carried_forward" else copy_status,
      severity = if (identical(copy_status, "missing_dependency")) "blocked" else "info",
      next_command = "",
      detail = if (identical(copy_status, "missing_dependency")) "Required dependency is missing from canonical paths." else "Dependency is available for carry-forward staging."
    )
  }
  data.frame(
    source_id = source_id,
    country = country,
    dependency_id = dependency_id,
    dependency_type = dependency_type,
    artifact_class = dependency_type,
    from_rel = rel,
    to_rel = rel,
    staged_to = if (identical(copy_status, "missing_dependency")) "" else staged_to,
    status = dependency_status$status,
    promotion_scope = promotion_scope,
    severity = dependency_status$severity,
    next_command = dependency_status$next_command,
    detail = dependency_status$detail,
    stringsAsFactors = FALSE
  )
}

admin_pit_include_stage_static_dependencies <- function(root, paths, contract) {
  deps <- contract$cleaners$static_dependencies %||% list()
  rows <- list()
  for (source_id in names(deps)) {
    country <- admin_pit_include_source_country(source_id)
    patterns <- unique(as.character(deps[[source_id]] %||% character()))
    for (pattern in patterns) {
      matches <- Sys.glob(admin_pit_include_path(pattern, root))
      matches <- matches[file.exists(matches)]
      if (!length(matches)) {
        rows[[length(rows) + 1L]] <- admin_pit_include_stage_dependency_path(root, paths, source_id, country, pattern, "static_dependency", dependency_id = pattern)
      } else {
        for (match in sort(matches)) {
          rows[[length(rows) + 1L]] <- admin_pit_include_stage_dependency_path(root, paths, source_id, country, admin_pit_include_relative_path(match, root), "static_dependency", dependency_id = pattern)
        }
      }
    }
  }
  admin_pit_include_bind(rows)
}

admin_pit_include_aux_destination <- function(source, path) {
  destinations <- unique(admin_pit_include_source_values(source$destination %||% source$destinations))
  if (!length(destinations)) return("")
  admin_pit_include_template(destinations[[1L]], list(source = source$id %||% "", basename = basename(path), incoming = path))
}

admin_pit_include_existing_glob <- function(patterns, root) {
  patterns <- admin_pit_include_source_values(patterns)
  if (!length(patterns)) return(character())
  paths <- unique(unlist(lapply(patterns, function(pattern) Sys.glob(admin_pit_include_path(pattern, root))), use.names = FALSE))
  paths <- as.character(paths %||% character())
  sort(paths[file.exists(paths)])
}

admin_pit_include_stage_one_aux <- function(root, paths, source_id, aux_id) {
  country <- admin_pit_include_source_country(source_id)
  source <- admin_pit_include_registry_source(root, aux_id)
  if (is.null(source)) {
    return(data.frame(source_id = source_id, country = country, dependency_id = aux_id, dependency_type = "aux_dependency", from_rel = "", to_rel = "", staged_to = "", status = "missing_aux_registry", promotion_scope = "none", severity = "warning", stringsAsFactors = FALSE))
  }
  inbox <- admin_pit_include_existing_glob(source$inbox, root)
  if (length(inbox)) {
    rows <- lapply(inbox, function(path) {
      to_rel <- admin_pit_include_aux_destination(source, basename(path))
      if (!nzchar(to_rel)) to_rel <- admin_pit_include_relative_path(path, root)
      staged_to <- file.path(paths$staged_repo, to_rel)
      copy_status <- admin_pit_include_copy_path(path, staged_to)
      data.frame(
        source_id = source_id,
        country = country,
        dependency_id = aux_id,
        dependency_type = "aux_dependency",
        from_rel = admin_pit_include_relative_path(path, root),
        to_rel = to_rel,
        staged_to = staged_to,
        status = if (identical(copy_status, "staged")) "staged_new_aux" else copy_status,
        promotion_scope = "promote",
        severity = if (identical(copy_status, "staged")) "info" else "blocked",
        stringsAsFactors = FALSE
      )
    })
    return(admin_pit_include_bind(rows))
  }
  canonical <- admin_pit_include_existing_glob(source$canonical, root)
  if (length(canonical)) {
    rows <- lapply(canonical, function(path) {
      admin_pit_include_stage_dependency_path(root, paths, source_id, country, admin_pit_include_relative_path(path, root), "aux_dependency", dependency_id = aux_id, promotion_scope = "carry_forward")
    })
    out <- admin_pit_include_bind(rows)
    out$status <- "carried_forward_aux"
    out$severity <- "info"
    return(out)
  }
  data.frame(
    source_id = source_id,
    country = country,
    dependency_id = aux_id,
    dependency_type = "aux_dependency",
    from_rel = paste(c(admin_pit_include_source_values(source$inbox), admin_pit_include_source_values(source$canonical)), collapse = ", "),
    to_rel = "",
    staged_to = "",
    status = if (identical(aux_id, "chl-uta")) "legacy_live_aux" else "missing_aux_dependency",
    promotion_scope = "none",
    severity = if (identical(aux_id, "chl-uta")) "warning" else "blocked",
    stringsAsFactors = FALSE
  )
}

admin_pit_include_stage_aux_dependencies <- function(root, paths, contract) {
  aux <- contract$cleaners$auxiliary_sources %||% list()
  rows <- list()
  for (source_id in names(aux)) {
    for (aux_id in as.character(aux[[source_id]] %||% character())) {
      rows[[length(rows) + 1L]] <- admin_pit_include_stage_one_aux(root, paths, source_id, aux_id)
    }
  }
  admin_pit_include_bind(rows)
}

admin_pit_include_required_bra_minwage_years <- function(root, contract) {
  config <- admin_pit_include_effective_config(root, contract)
  last <- suppressWarnings(as.integer(config$years$last))
  if (is.na(last) || last < 2007L) return(integer())
  seq.int(2007L, last)
}

admin_pit_include_read_minwage <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    return(list(ok = FALSE, data = data.frame(stringsAsFactors = FALSE), status = "blocked_aux_file_missing", detail = "wiki_minwage.csv file is missing."))
  }
  data <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) e)
  if (inherits(data, "error")) {
    return(list(ok = FALSE, data = data.frame(stringsAsFactors = FALSE), status = "blocked_aux_read_failed", detail = conditionMessage(data)))
  }
  names(data) <- tolower(names(data))
  missing <- setdiff(c("year", "minwage"), names(data))
  if (length(missing)) {
    return(list(ok = FALSE, data = data.frame(stringsAsFactors = FALSE), status = "blocked_aux_schema", detail = paste("Missing required columns:", paste(missing, collapse = ","))))
  }
  out <- data.frame(
    year = suppressWarnings(as.integer(data$year)),
    minwage = suppressWarnings(as.numeric(data$minwage)),
    stringsAsFactors = FALSE
  )
  if (!nrow(out)) {
    return(list(ok = FALSE, data = out, status = "blocked_aux_empty", detail = "wiki_minwage.csv has no rows."))
  }
  if (any(is.na(out$year)) || any(is.na(out$minwage))) {
    return(list(ok = FALSE, data = out, status = "blocked_aux_non_numeric", detail = "Columns year and minwage must be numeric."))
  }
  if (any(out$minwage <= 0, na.rm = TRUE)) {
    return(list(ok = FALSE, data = out, status = "blocked_aux_non_positive", detail = "Minimum-wage values must be positive."))
  }
  duplicated_years <- unique(out$year[duplicated(out$year)])
  if (length(duplicated_years)) {
    return(list(ok = FALSE, data = out, status = "blocked_aux_duplicate_years", detail = paste("Duplicate years:", paste(duplicated_years, collapse = ","))))
  }
  out <- out[order(out$year), , drop = FALSE]
  list(ok = TRUE, data = out, status = "ok", detail = "")
}

admin_pit_include_aux_fetch_command <- function(root, aux_id) {
  source <- admin_pit_include_registry_source(root, aux_id)
  if (!is.null(source) && (nzchar(source$fetcher %||% "") || identical(aux_id, "bra-minwage"))) {
    return(sprintf("dina sources fetch %s", aux_id))
  }
  ""
}

admin_pit_include_aux_validation_row <- function(source_id, country, dependency_id, status, severity, required_years = integer(), available_years = integer(), overlap_years = integer(), extension_years = integer(), changed_overlap_years = integer(), missing_required_years = integer(), missing_canonical_years = integer(), next_command = "", detail = "") {
  data.frame(
    source_id = source_id,
    country = country,
    dependency_id = dependency_id,
    artifact_class = "aux_source",
    status = status,
    severity = severity,
    required_years = paste(required_years, collapse = ","),
    available_years = paste(available_years, collapse = ","),
    overlap_years = paste(overlap_years, collapse = ","),
    extension_years = paste(extension_years, collapse = ","),
    changed_overlap_years = paste(changed_overlap_years, collapse = ","),
    missing_required_years = paste(missing_required_years, collapse = ","),
    missing_canonical_years = paste(missing_canonical_years, collapse = ","),
    next_command = next_command,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

admin_pit_include_validate_bra_minwage <- function(root, contract, aux_rows) {
  if (!nrow(aux_rows)) return(data.frame(stringsAsFactors = FALSE))
  row <- aux_rows[1L, , drop = FALSE]
  required <- admin_pit_include_required_bra_minwage_years(root, contract)
  fetch <- admin_pit_include_aux_fetch_command(root, "bra-minwage")
  if (identical(row$severity[[1L]], "blocked")) {
    return(admin_pit_include_aux_validation_row(
      row$source_id[[1L]],
      row$country[[1L]],
      row$dependency_id[[1L]],
      "blocked_missing_aux",
      "blocked",
      required,
      integer(),
      next_command = fetch,
      detail = "Brazil minimum-wage aux file is missing in both _new and canonical paths."
    ))
  }
  candidate_path <- row$staged_to[[1L]]
  candidate <- admin_pit_include_read_minwage(candidate_path)
  if (!isTRUE(candidate$ok)) {
    return(admin_pit_include_aux_validation_row(
      row$source_id[[1L]],
      row$country[[1L]],
      row$dependency_id[[1L]],
      candidate$status,
      "blocked",
      required,
      integer(),
      next_command = fetch,
      detail = candidate$detail
    ))
  }
  available <- candidate$data$year
  missing_required <- setdiff(required, available)
  if (length(missing_required)) {
    return(admin_pit_include_aux_validation_row(
      row$source_id[[1L]],
      row$country[[1L]],
      row$dependency_id[[1L]],
      "blocked_aux_missing_required_years",
      "blocked",
      required,
      available,
      missing_required_years = missing_required,
      next_command = fetch,
      detail = paste("wiki_minwage.csv is missing required years:", paste(missing_required, collapse = ","))
    ))
  }
  if (!identical(row$status[[1L]], "staged_new_aux")) {
    return(admin_pit_include_aux_validation_row(
      row$source_id[[1L]],
      row$country[[1L]],
      row$dependency_id[[1L]],
      "aux_carried_forward_valid",
      "info",
      required,
      available,
      detail = "Canonical wiki_minwage.csv covers required years and is carried forward."
    ))
  }
  source <- admin_pit_include_registry_source(root, "bra-minwage")
  canonical_paths <- if (is.null(source)) character() else admin_pit_include_existing_glob(source$canonical, root)
  if (length(canonical_paths)) {
    canonical <- admin_pit_include_read_minwage(canonical_paths[[1L]])
    if (!isTRUE(canonical$ok)) {
      return(admin_pit_include_aux_validation_row(
        row$source_id[[1L]],
        row$country[[1L]],
        row$dependency_id[[1L]],
        "blocked_aux_canonical_invalid",
        "blocked",
        required,
        available,
        next_command = fetch,
        detail = canonical$detail
      ))
    }
    missing_canonical <- setdiff(canonical$data$year, candidate$data$year)
    overlap <- intersect(canonical$data$year, candidate$data$year)
    changed <- overlap[abs(candidate$data$minwage[match(overlap, candidate$data$year)] - canonical$data$minwage[match(overlap, canonical$data$year)]) > 1e-9]
    extension <- setdiff(candidate$data$year, canonical$data$year)
    if (length(changed)) {
      return(admin_pit_include_aux_validation_row(
        row$source_id[[1L]],
        row$country[[1L]],
        row$dependency_id[[1L]],
        "blocked_aux_overlap_changed",
        "blocked",
        required,
        available,
        overlap_years = overlap,
        extension_years = extension,
        changed_overlap_years = changed,
        next_command = fetch,
        detail = paste("Overlapping minimum-wage values changed for years:", paste(changed, collapse = ","))
      ))
    }
    if (length(missing_canonical)) {
      return(admin_pit_include_aux_validation_row(
        row$source_id[[1L]],
        row$country[[1L]],
        row$dependency_id[[1L]],
        "blocked_aux_missing_canonical_years",
        "blocked",
        required,
        available,
        overlap_years = overlap,
        extension_years = extension,
        missing_canonical_years = missing_canonical,
        next_command = fetch,
        detail = paste("New wiki_minwage.csv omits canonical historical years:", paste(missing_canonical, collapse = ","))
      ))
    }
    return(admin_pit_include_aux_validation_row(
      row$source_id[[1L]],
      row$country[[1L]],
      row$dependency_id[[1L]],
      "aux_validated_append_only",
      "info",
      required,
      available,
      overlap_years = overlap,
      extension_years = extension,
      detail = "New wiki_minwage.csv preserves overlap values and appends new years."
    ))
  }
  admin_pit_include_aux_validation_row(
    row$source_id[[1L]],
    row$country[[1L]],
    row$dependency_id[[1L]],
    "aux_validated_complete",
    "info",
    required,
    available,
    extension_years = available,
    detail = "New wiki_minwage.csv has required coverage; no canonical overlap exists yet."
  )
}

admin_pit_include_validate_aux_dependencies <- function(root, contract, aux_dependencies) {
  if (!nrow(aux_dependencies)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(aux_dependencies[, c("source_id", "country", "dependency_id"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    dep <- aux_dependencies[
      aux_dependencies$source_id == key$source_id & aux_dependencies$country == key$country & aux_dependencies$dependency_id == key$dependency_id,
      ,
      drop = FALSE
    ]
    if (identical(key$dependency_id[[1L]], "bra-minwage")) {
      return(admin_pit_include_validate_bra_minwage(root, contract, dep))
    }
    severity <- if (any(dep$severity == "blocked", na.rm = TRUE)) "blocked" else if (any(dep$severity == "warning", na.rm = TRUE)) "warning" else "info"
    status <- if (severity == "blocked") {
      dep$status[dep$severity == "blocked"][[1L]]
    } else if (severity == "warning") {
      dep$status[dep$severity == "warning"][[1L]]
    } else {
      "aux_validation_not_required"
    }
    admin_pit_include_aux_validation_row(
      key$source_id[[1L]],
      key$country[[1L]],
      key$dependency_id[[1L]],
      status,
      severity,
      detail = if (identical(key$dependency_id[[1L]], "chl-uta") && severity == "warning") "CHL UTA remains a legacy live auxiliary dependency for now." else "No additional aux validation is required."
    )
  })
  admin_pit_include_bind(rows)
}

admin_pit_include_aux_dependency_summary <- function(aux_dependencies, aux_validation) {
  if (!nrow(aux_dependencies) && !nrow(aux_validation)) return(data.frame(stringsAsFactors = FALSE))
  keys <- unique(admin_pit_include_bind(
    aux_dependencies[, intersect(c("source_id", "country", "dependency_id"), names(aux_dependencies)), drop = FALSE],
    aux_validation[, intersect(c("source_id", "country", "dependency_id"), names(aux_validation)), drop = FALSE]
  ))
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    dep <- aux_dependencies[
      aux_dependencies$source_id == key$source_id & aux_dependencies$country == key$country & aux_dependencies$dependency_id == key$dependency_id,
      ,
      drop = FALSE
    ]
    val <- aux_validation[
      aux_validation$source_id == key$source_id & aux_validation$country == key$country & aux_validation$dependency_id == key$dependency_id,
      ,
      drop = FALSE
    ]
    severity <- if (nrow(val)) val$severity[[1L]] else if (any(dep$severity == "blocked", na.rm = TRUE)) "blocked" else if (any(dep$severity == "warning", na.rm = TRUE)) "warning" else "info"
    status <- if (nrow(val)) val$status[[1L]] else dep$status[[1L]]
    command <- if (nrow(val) && nzchar(val$next_command[[1L]] %||% "")) val$next_command[[1L]] else ""
    data.frame(
      source_id = key$source_id,
      country = key$country,
      dependency_id = key$dependency_id,
      artifact_class = "aux_source",
      stage_status = if (nrow(dep)) paste(unique(dep$status), collapse = ",") else "",
      validation_status = status,
      severity = severity,
      next_command = command,
      detail = if (nrow(val)) val$detail[[1L]] else "",
      stringsAsFactors = FALSE
    )
  })
  admin_pit_include_bind(rows)
}

admin_pit_include_years_for_source <- function(exploration, source_id) {
  rows <- exploration$year_expectations
  rows <- rows[rows$source_id == source_id, , drop = FALSE]
  years <- suppressWarnings(as.integer(rows$year))
  sort(unique(years[!is.na(years)]))
}

admin_pit_include_col_years <- function(exploration) {
  inventory <- exploration$source_inventory
  rows <- inventory[inventory$source_id == "col-pit-total" & inventory$source_set == "new" & inventory$status == "matched", , drop = FALSE]
  if (!nrow(rows)) return(integer())
  years <- unique(unlist(lapply(rows$years, function(x) suppressWarnings(as.integer(unlist(strsplit(as.character(x), "[^0-9]+"))))), use.names = FALSE))
  sort(years[!is.na(years)])
}

admin_pit_include_col_source_dir <- function(paths, exploration) {
  inventory <- exploration$source_inventory
  rows <- inventory[inventory$source_id == "col-pit-total" & inventory$source_set == "new" & inventory$status == "matched", , drop = FALSE]
  if (!nrow(rows)) return("")
  row <- rows[order(rows$year_end, decreasing = TRUE), , drop = FALSE][1L, , drop = FALSE]
  dest <- row$destination[[1L]] %||% row$rel[[1L]]
  staged <- file.path(paths$staged_repo, dest)
  nested <- file.path(staged, basename(staged))
  if (dir.exists(nested)) nested else staged
}

admin_pit_include_mock_cleaner <- function(paths, contract, exploration, source_id) {
  admin_pit_include_need("openxlsx")
  outputs <- admin_pit_include_expected_outputs(contract, source_id)
  years <- admin_pit_include_years_for_source(exploration, source_id)
  if (identical(source_id, "col-pit-total")) {
    col_years <- admin_pit_include_col_years(exploration)
    if (length(col_years)) years <- col_years
  }
  if (!length(years)) years <- 2000L
  for (rel in outputs) {
    path <- file.path(paths$staged_repo, rel)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    wb <- openxlsx::createWorkbook()
    for (year in years) {
      openxlsx::addWorksheet(wb, as.character(year))
      openxlsx::writeData(wb, as.character(year), data.frame(year = year, country = admin_pit_include_source_country(source_id), component = basename(rel), p = 0, stringsAsFactors = FALSE))
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  }
  TRUE
}

admin_pit_include_run_r_candidate_cleaner <- function(root, paths, contract, source_id, country) {
  source(file.path(root, "code", "R", "admin_cleaners", "admin_pit_candidate_cleaners.R"), local = TRUE)
  config_path <- admin_pit_include_config_path(root, contract)
  override_path <- admin_pit_include_override_path(root, contract)
  log <- file.path(paths$logs, paste0("cleaner-", country, ".log"))
  dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
  out <- tryCatch({
    result <- if (identical(country, "CHL")) {
      admin_pit_candidate_clean_chl(repo_root = root, input_root = paths$staged_repo, output_root = paths$staged_repo, config_path = config_path, override_path = override_path)
    } else {
      admin_pit_candidate_clean_bra(repo_root = root, input_root = paths$staged_repo, output_root = paths$staged_repo, config_path = config_path, override_path = override_path)
    }
    writeLines(capture.output(print(result)), log)
    list(status = "succeeded", log = log, exit_status = 0L, reason = "")
  }, error = function(e) {
    writeLines(conditionMessage(e), log)
    list(status = "failed", log = log, exit_status = 1L, reason = conditionMessage(e))
  })
  out
}

admin_pit_include_stata_command <- function(root, contract) {
  config <- admin_pit_include_effective_config(root, contract)
  cmd <- Sys.getenv("DINA_STATA_CMD", unset = config$stata$command %||% "")
  if (!nzchar(cmd) || identical(cmd, "${DINA_STATA_CMD}")) "" else cmd
}

admin_pit_include_col_temp_do <- function(root, paths, source_dir, output_dir, first_year, last_year) {
  original <- file.path(root, "code", "Stata", "tax-data", "COL-diverse.do")
  lines <- readLines(original, warn = FALSE)
  lines <- gsub("local lasty_col_tax = 2023", sprintf("local lasty_col_tax = %s", last_year), lines, fixed = TRUE)
  lines <- gsub("forvalues y = 2014/`lasty_col_tax' {", sprintf("forvalues y = %s/`lasty_col_tax' {", first_year), lines, fixed = TRUE)
  route_line <- grep("^\\s*global route\\s*///\\s*$", lines)
  if (length(route_line)) {
    i <- route_line[[1L]]
    lines[[i]] <- sprintf('global route "%s"', normalizePath(source_dir, mustWork = FALSE))
    if (length(lines) >= i + 1L && grepl("input_data/admin_data/COL/1_Cuantiles", lines[[i + 1L]], fixed = TRUE)) {
      lines[[i + 1L]] <- ""
    }
  }
  lines <- gsub('local dirpath "input_data/admin_data/COL/_clean"', sprintf('local dirpath "%s"', normalizePath(output_dir, mustWork = FALSE)), lines, fixed = TRUE)
  lines <- gsub('"input_data/admin_data/COL/_clean/total-`v\'-COL.xlsx"', sprintf('"%s/total-`v\'-COL.xlsx"', normalizePath(output_dir, mustWork = FALSE)), lines, fixed = TRUE)
  do_file <- file.path(paths$logs, "cleaner-COL-isolated.do")
  writeLines(lines, do_file)
  do_file
}

admin_pit_include_run_col_cleaner <- function(root, paths, contract, exploration) {
  stata <- admin_pit_include_stata_command(root, contract)
  if (!nzchar(stata)) {
    return(list(status = "failed", log = "", exit_status = NA_integer_, reason = "stata_not_configured"))
  }
  years <- admin_pit_include_col_years(exploration)
  if (!length(years)) {
    return(list(status = "failed", log = "", exit_status = NA_integer_, reason = "col_years_missing"))
  }
  source_dir <- admin_pit_include_col_source_dir(paths, exploration)
  if (!dir.exists(source_dir)) {
    return(list(status = "failed", log = "", exit_status = NA_integer_, reason = "col_source_dir_missing"))
  }
  output_dir <- file.path(paths$staged_repo, "input_data", "admin_data", "COL", "_clean")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  do_file <- admin_pit_include_col_temp_do(root, paths, source_dir, output_dir, min(years), max(years))
  out <- tryCatch(system2(stata, args = c("-b", "do", do_file), stdout = TRUE, stderr = TRUE), error = function(e) structure(conditionMessage(e), status = 1L))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  log <- file.path(paths$logs, "cleaner-COL.log")
  writeLines(as.character(out), log)
  list(status = if (identical(as.integer(status), 0L)) "succeeded" else "failed", log = log, exit_status = as.integer(status), reason = "")
}

admin_pit_include_cleaner_output_rows <- function(paths, contract, source_id) {
  outputs <- admin_pit_include_expected_outputs(contract, source_id)
  country <- admin_pit_include_source_country(source_id)
  rows <- lapply(outputs, function(rel) {
    staged <- file.path(paths$staged_repo, rel)
    exists <- file.exists(staged)
    data.frame(
      source_id = source_id,
      country = country,
      artifact_type = "clean_output",
      rel = rel,
      staged_rel = admin_pit_include_relative_path(staged, paths$root),
      staged_to = staged,
      exists = exists,
      sha256 = if (exists) admin_pit_include_hash_path(staged) else NA_character_,
      status = if (exists) "generated" else "missing_clean_output",
      promotion_scope = "promote",
      stringsAsFactors = FALSE
    )
  })
  admin_pit_include_bind(rows)
}

admin_pit_include_run_cleaners <- function(root, paths, contract, exploration, source_summary, cleaner_mode = NULL, static_dependencies = data.frame(), aux_dependencies = data.frame(), aux_validation = data.frame()) {
  mode <- cleaner_mode %||% contract$cleaners$mode %||% "real"
  source_ids <- admin_pit_include_supported_ids(contract)
  summaries <- list()
  outputs <- list()
  for (source_id in source_ids) {
    country <- admin_pit_include_source_country(source_id)
    source_row <- source_summary[source_summary$source_id == source_id & source_summary$country == country, , drop = FALSE]
    static <- static_dependencies[static_dependencies$source_id == source_id & static_dependencies$country == country & static_dependencies$severity == "blocked", , drop = FALSE]
    aux <- aux_dependencies[aux_dependencies$source_id == source_id & aux_dependencies$country == country & aux_dependencies$severity == "blocked", , drop = FALSE]
    aux_val <- aux_validation[aux_validation$source_id == source_id & aux_validation$country == country & aux_validation$severity == "blocked", , drop = FALSE]
    dependency_blockers <- unique(c(static$status %||% character(), aux$status %||% character(), aux_val$status %||% character()))
    if (nrow(source_row) && identical(source_row$status[[1L]], "blocked")) {
      run <- list(status = "skipped", log = "", exit_status = NA_integer_, reason = "source_checks_blocked")
    } else if (length(dependency_blockers)) {
      run <- list(status = "skipped", log = "", exit_status = NA_integer_, reason = paste("blocked_dependency", paste(dependency_blockers, collapse = ","), sep = ":"))
    } else if (identical(mode, "mock")) {
      ok <- tryCatch(admin_pit_include_mock_cleaner(paths, contract, exploration, source_id), error = function(e) e)
      run <- if (isTRUE(ok)) list(status = "succeeded", log = "", exit_status = 0L, reason = "") else list(status = "failed", log = "", exit_status = 1L, reason = conditionMessage(ok))
    } else if (source_id %in% c("chl-pit-total", "bra-pit-total")) {
      run <- admin_pit_include_run_r_candidate_cleaner(root, paths, contract, source_id, country)
    } else if (identical(source_id, "col-pit-total")) {
      run <- admin_pit_include_run_col_cleaner(root, paths, contract, exploration)
    } else {
      run <- list(status = "failed", log = "", exit_status = 1L, reason = "unsupported_cleaner")
    }
    output_rows <- admin_pit_include_cleaner_output_rows(paths, contract, source_id)
    missing <- if (nrow(output_rows)) sum(!output_rows$exists) else 0L
    cleaner_status <- if (!identical(run$status, "succeeded") || missing > 0L) "blocked" else "all_good"
    summaries[[length(summaries) + 1L]] <- data.frame(
      source_id = source_id,
      country = country,
      cleaner_mode = mode,
      cleaner_status = cleaner_status,
      run_status = run$status,
      reason = run$reason %||% "",
      outputs_expected = length(admin_pit_include_expected_outputs(contract, source_id)),
      outputs_found = if (nrow(output_rows)) sum(output_rows$exists) else 0L,
      missing_outputs = missing,
      log = if (nzchar(run$log %||% "")) admin_pit_include_relative_path(run$log, paths$root) else "",
      stringsAsFactors = FALSE
    )
    outputs[[length(outputs) + 1L]] <- output_rows
  }
  list(summary = admin_pit_include_bind(summaries), outputs = admin_pit_include_bind(outputs))
}

admin_pit_include_prepare_staged_sources <- function(root, paths, exploration, contract) {
  inventory <- exploration$source_inventory
  supported <- admin_pit_include_supported_ids(contract)
  inventory <- inventory[inventory$source_id %in% supported & inventory$status == "matched", , drop = FALSE]
  rows <- list()
  dir.create(paths$staged_repo, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(inventory))) {
    row <- inventory[i, , drop = FALSE]
    from <- if (grepl("^/", row$file[[1L]] %||% "")) row$file[[1L]] else file.path(root, row$rel[[1L]] %||% "")
    to_rel <- if (identical(row$source_set[[1L]], "new")) row$destination[[1L]] %||% "" else row$rel[[1L]] %||% ""
    if (!nzchar(to_rel)) {
      copy_status <- "missing_destination"
      staged_to <- ""
    } else {
      staged_to <- file.path(paths$staged_repo, to_rel)
      copy_status <- admin_pit_include_copy_path(from, staged_to)
    }
    rows[[length(rows) + 1L]] <- data.frame(
      source_id = row$source_id,
      country = row$country,
      source_set = row$source_set,
      from_rel = admin_pit_include_relative_path(from, root),
      to_rel = to_rel,
      staged_to = staged_to,
      destination_status = row$destination_status %||% "",
      copy_status = copy_status,
      stringsAsFactors = FALSE
    )
  }
  admin_pit_include_bind(rows)
}

admin_pit_include_detail <- function(exploration, mappings, contract) {
  expectations <- exploration$year_expectations
  rows <- list()
  for (source_id in admin_pit_include_supported_ids(contract)) {
    country <- admin_pit_include_source_country(source_id)
    source_maps <- mappings[mappings$source_id == source_id & mappings$country == country & mappings$source_set == "new", , drop = FALSE]
    source_expectations <- expectations[expectations$source_id == source_id & expectations$country == country, , drop = FALSE]
    if (!nrow(source_maps) && !nrow(source_expectations)) {
      rows[[length(rows) + 1L]] <- data.frame(
        source_id = source_id,
        country = country,
        year = NA_integer_,
        year_role = "missing_incoming_source",
        structure_status = "",
        status = "blocked_missing_incoming_source",
        reason = "No incoming PIT source was found in the _new admin bucket.",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!nrow(expectations)) return(admin_pit_include_bind(rows))
  expectation_rows <- lapply(seq_len(nrow(expectations)), function(i) {
    exp <- expectations[i, , drop = FALSE]
    structure_status <- exp$structure_status[[1L]] %||% ""
    incoming <- mappings[mappings$source_id == exp$source_id & mappings$country == exp$country & mappings$source_set == "new", , drop = FALSE]
    if (!nrow(incoming)) {
      status <- "blocked_missing_incoming_source"
      reason <- "No incoming source mapping was staged."
    } else if (identical(structure_status, "blocked_structure_mismatch")) {
      status <- "blocked_structure_mismatch"
      reason <- "Explorer structure check is blocked."
    } else if (any(incoming$copy_status %in% c("missing_destination", "copy_failed", "missing_source"), na.rm = TRUE) ||
      any(!incoming$destination_status %in% c("ready", ""), na.rm = TRUE)) {
      status <- "blocked_destination_missing"
      reason <- paste(unique(c(incoming$copy_status, incoming$destination_status)), collapse = ";")
    } else if (identical(structure_status, "structure_review_needed")) {
      status <- "warning_structure_review"
      reason <- "Explorer could not fully verify shallow structure."
    } else {
      status <- "ok_source_staged"
      reason <- "Incoming PIT source was staged at its canonical destination."
    }
    data.frame(source_id = exp$source_id, country = exp$country, year = as.integer(exp$year), year_role = exp$year_role, structure_status = structure_status, status = status, reason = reason, stringsAsFactors = FALSE)
  })
  admin_pit_include_bind(c(rows, expectation_rows))
}

admin_pit_include_summary <- function(detail, mappings, contract, cleaner_summary = data.frame(), static_dependencies = data.frame(), aux_dependencies = data.frame(), aux_validation = data.frame()) {
  if (!nrow(detail)) return(data.frame(stringsAsFactors = FALSE))
  blocked <- as.character(contract$statuses$blocked %||% c("blocked_structure_mismatch", "blocked_destination_missing", "blocked_missing_incoming_source"))
  warnings <- as.character(contract$statuses$warnings %||% c("warning_structure_review"))
  keys <- unique(detail[, c("source_id", "country"), drop = FALSE])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    key <- keys[i, , drop = FALSE]
    rows <- detail[detail$source_id == key$source_id & detail$country == key$country, , drop = FALSE]
    source_maps <- mappings[mappings$source_id == key$source_id & mappings$country == key$country & mappings$source_set == "new", , drop = FALSE]
    cleaner <- cleaner_summary[cleaner_summary$source_id == key$source_id & cleaner_summary$country == key$country, , drop = FALSE]
    static <- static_dependencies[static_dependencies$source_id == key$source_id & static_dependencies$country == key$country, , drop = FALSE]
    aux <- aux_dependencies[aux_dependencies$source_id == key$source_id & aux_dependencies$country == key$country, , drop = FALSE]
    aux_val <- aux_validation[aux_validation$source_id == key$source_id & aux_validation$country == key$country, , drop = FALSE]
    cleaner_blocked <- nrow(cleaner) && any(cleaner$cleaner_status == "blocked", na.rm = TRUE)
    dep_blocked <- (nrow(static) && any(static$severity == "blocked", na.rm = TRUE)) ||
      (nrow(aux) && any(aux$severity == "blocked", na.rm = TRUE)) ||
      (nrow(aux_val) && any(aux_val$severity == "blocked", na.rm = TRUE))
    dep_warnings <- (if (nrow(static)) sum(static$severity == "warning", na.rm = TRUE) else 0L) +
      (if (nrow(aux)) sum(aux$severity == "warning", na.rm = TRUE) else 0L) +
      (if (nrow(aux_val)) sum(aux_val$severity == "warning", na.rm = TRUE) else 0L)
    status <- if (any(rows$status %in% blocked) || cleaner_blocked || dep_blocked) {
      "blocked"
    } else if (any(rows$status %in% warnings) || dep_warnings > 0L) {
      "check_following"
    } else {
      "all_good"
    }
    data.frame(
      source_id = key$source_id,
      country = key$country,
      status = status,
      expected_years = length(unique(rows$year[!is.na(rows$year)])),
      staged_sources = nrow(source_maps[source_maps$copy_status == "staged", , drop = FALSE]),
      clean_outputs = if (nrow(cleaner)) cleaner$outputs_found[[1L]] else 0L,
      dependency_warnings = dep_warnings,
      warnings = sum(rows$status %in% warnings) + dep_warnings,
      blocked = sum(rows$status %in% blocked) + if (cleaner_blocked) 1L else 0L + if (dep_blocked) 1L else 0L,
      stringsAsFactors = FALSE
    )
  })
  admin_pit_include_bind(rows)
}

admin_pit_include_overall_status <- function(summary) {
  if (!nrow(summary)) return("check_following")
  if (any(summary$status == "blocked", na.rm = TRUE)) return("blocked")
  if (any(summary$status == "check_following", na.rm = TRUE)) return("check_following")
  "all_good"
}

admin_pit_include_source_fingerprints <- function(root, mappings) {
  if (!nrow(mappings)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(mappings)), function(i) {
    row <- mappings[i, , drop = FALSE]
    rel <- row$from_rel[[1L]]
    path <- file.path(root, rel)
    exists <- file.exists(path)
    info <- if (exists) file.info(path) else data.frame(size = NA_real_, mtime = NA)
    data.frame(
      source_id = row$source_id,
      source_set = row$source_set,
      rel = rel,
      exists = exists,
      kind = if (!exists) "missing" else if (dir.exists(path)) "dir" else "file",
      size = if (exists) as.numeric(info$size[[1L]]) else NA_real_,
      mtime = if (exists) as.character(info$mtime[[1L]]) else NA_character_,
      sha256 = admin_pit_include_hash_path(path),
      stringsAsFactors = FALSE
    )
  })
  admin_pit_include_bind(rows)
}

admin_pit_include_promotion_plan <- function(mappings, cleaner_outputs, aux_dependencies, aux_validation = data.frame()) {
  raw <- mappings[mappings$source_set == "new" & mappings$copy_status == "staged", , drop = FALSE]
  raw_plan <- if (nrow(raw)) {
    data.frame(source_id = raw$source_id, country = raw$country, artifact_type = "raw_source", from_rel = raw$staged_to, to_rel = raw$to_rel, promotion_scope = "promote", stringsAsFactors = FALSE)
  } else data.frame(stringsAsFactors = FALSE)
  clean <- cleaner_outputs[cleaner_outputs$promotion_scope == "promote" & cleaner_outputs$exists %in% TRUE, , drop = FALSE]
  clean_plan <- if (nrow(clean)) {
    data.frame(source_id = clean$source_id, country = clean$country, artifact_type = clean$artifact_type, from_rel = clean$staged_to, to_rel = clean$rel, promotion_scope = "promote", stringsAsFactors = FALSE)
  } else data.frame(stringsAsFactors = FALSE)
  aux <- aux_dependencies[aux_dependencies$promotion_scope == "promote" & aux_dependencies$status == "staged_new_aux", , drop = FALSE]
  if (nrow(aux) && nrow(aux_validation)) {
    blocked_keys <- paste(
      aux_validation$source_id[aux_validation$severity == "blocked"],
      aux_validation$country[aux_validation$severity == "blocked"],
      aux_validation$dependency_id[aux_validation$severity == "blocked"],
      sep = "\r"
    )
    aux_key <- paste(aux$source_id, aux$country, aux$dependency_id, sep = "\r")
    aux <- aux[!(aux_key %in% blocked_keys), , drop = FALSE]
  }
  aux_plan <- if (nrow(aux)) {
    data.frame(source_id = aux$dependency_id, country = aux$country, artifact_type = "aux_source", from_rel = aux$staged_to, to_rel = aux$to_rel, promotion_scope = "promote", stringsAsFactors = FALSE)
  } else data.frame(stringsAsFactors = FALSE)
  admin_pit_include_bind(raw_plan, clean_plan, aux_plan)
}

admin_pit_include_promotion_fingerprints <- function(promotion_plan) {
  if (!nrow(promotion_plan)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    path <- row$from_rel[[1L]]
    exists <- file.exists(path)
    data.frame(
      source_id = row$source_id,
      country = row$country,
      artifact_type = row$artifact_type,
      from_rel = path,
      to_rel = row$to_rel,
      exists = exists,
      kind = if (!exists) "missing" else if (dir.exists(path)) "dir" else "file",
      sha256 = if (exists) admin_pit_include_hash_path(path) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  admin_pit_include_bind(rows)
}

admin_pit_include_manifest <- function(run_id, status, exploration_root, contract) {
  data.frame(
    key = c("run_id", "source_type", "workflow", "status", "dry_run", "cleaners", "exploration_run", "supported_source_ids"),
    value = c(run_id, "admin", "admin_pit", status, "TRUE", "TRUE", normalizePath(exploration_root, mustWork = FALSE), paste(admin_pit_include_supported_ids(contract), collapse = ",")),
    stringsAsFactors = FALSE
  )
}

admin_pit_include_write_outputs <- function(outputs, paths, manifest) {
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  for (name in names(outputs)) {
    utils::write.csv(outputs[[name]], file.path(paths$tables, paste0(name, ".csv")), row.names = FALSE, na = "")
  }
  utils::write.csv(manifest, file.path(paths$logs, "include_manifest.csv"), row.names = FALSE, na = "")
  utils::write.csv(manifest, file.path(paths$tables, "include_manifest.csv"), row.names = FALSE, na = "")
  invisible(paths)
}

run_admin_pit_include <- function(
  root = admin_pit_include_repo_root(),
  contract_path = file.path(root, "config", "admin_pit_include.yml"),
  exploration_run = NULL,
  output_dir = NULL,
  write_outputs = TRUE,
  run_id = NULL,
  cleaner_mode = NULL
) {
  admin_pit_include_load_explorer(root)
  contract <- admin_pit_include_read_contract(root, contract_path)
  exploration <- admin_pit_include_read_exploration(root, contract, exploration_run)
  run_id <- run_id %||% admin_pit_include_run_id()
  paths <- admin_pit_include_output_paths_for_run(root, contract, output_dir, run_id)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$staged_repo, recursive = TRUE, showWarnings = FALSE)
  aux_dependencies <- admin_pit_include_stage_aux_dependencies(root, paths, contract)
  aux_validation <- admin_pit_include_validate_aux_dependencies(root, contract, aux_dependencies)
  aux_summary <- admin_pit_include_aux_dependency_summary(aux_dependencies, aux_validation)
  static_dependencies <- admin_pit_include_stage_static_dependencies(root, paths, contract)
  mappings <- admin_pit_include_prepare_staged_sources(root, paths, exploration, contract)
  source_fingerprints <- admin_pit_include_source_fingerprints(root, mappings)
  detail <- admin_pit_include_detail(exploration, mappings, contract)
  source_summary <- admin_pit_include_summary(detail, mappings, contract, static_dependencies = static_dependencies, aux_dependencies = aux_dependencies, aux_validation = aux_validation)
  cleaners <- admin_pit_include_run_cleaners(root, paths, contract, exploration, source_summary, cleaner_mode = cleaner_mode, static_dependencies = static_dependencies, aux_dependencies = aux_dependencies, aux_validation = aux_validation)
  summary <- admin_pit_include_summary(detail, mappings, contract, cleaners$summary, static_dependencies, aux_dependencies, aux_validation)
  status <- admin_pit_include_overall_status(summary)
  manifest <- admin_pit_include_manifest(run_id, status, exploration$root, contract)
  promotion_plan <- admin_pit_include_promotion_plan(mappings, cleaners$outputs, aux_dependencies, aux_validation)
  promotion_fingerprints <- admin_pit_include_promotion_fingerprints(promotion_plan)
  outputs <- list(
    staged_source_mappings = mappings,
    static_dependency_report = static_dependencies,
    aux_dependency_report = aux_dependencies,
    aux_validation_report = aux_validation,
    aux_dependency_summary = aux_summary,
    include_detail = detail,
    include_summary = summary,
    cleaner_summary = cleaners$summary,
    cleaner_outputs = cleaners$outputs,
    promotion_plan = promotion_plan,
    source_fingerprints = source_fingerprints,
    promotion_fingerprints = promotion_fingerprints
  )
  if (isTRUE(write_outputs)) {
    admin_pit_include_write_outputs(outputs, paths, manifest)
  }
  list(paths = paths, outputs = outputs, manifest = manifest, contract = contract, run_id = run_id)
}

admin_pit_include_resolve_run <- function(root, contract, include_run) {
  if (is.null(include_run) || !nzchar(include_run)) {
    stop("--include-run is required for admin PIT confirm.", call. = FALSE)
  }
  candidates <- normalizePath(c(include_run, file.path(root, include_run), file.path(admin_pit_include_output_root(root, contract), "runs", include_run)), mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, "logs", "include_manifest.csv"))]
  if (!length(hit)) stop("Admin PIT include run not found: ", include_run, call. = FALSE)
  hit[[1L]]
}

admin_pit_include_resolve_confirm <- function(root, contract, confirm_run) {
  if (is.null(confirm_run) || !nzchar(confirm_run)) stop("Confirm run is required for admin PIT restore.", call. = FALSE)
  candidates <- normalizePath(c(confirm_run, file.path(root, confirm_run), file.path(admin_pit_include_output_root(root, contract), "confirms", confirm_run)), mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, "logs", "confirm_manifest.csv"))]
  if (!length(hit)) stop("Admin PIT confirm run not found: ", confirm_run, call. = FALSE)
  hit[[1L]]
}

admin_pit_include_verify_source_fingerprints <- function(root, include_run, mappings, contract) {
  expected <- admin_pit_include_read_csv(file.path(include_run, "tables", "source_fingerprints.csv"))
  incoming <- mappings[mappings$source_set == "new" & mappings$copy_status == "staged", , drop = FALSE]
  if (!nrow(incoming)) return(data.frame(stringsAsFactors = FALSE))
  compare <- as.character(contract$fingerprints$compare %||% c("kind", "sha256"))
  require_sha <- isTRUE(contract$fingerprints$require_sha256 %||% TRUE)
  rows <- lapply(seq_len(nrow(incoming)), function(i) {
    row <- incoming[i, , drop = FALSE]
    rel <- row$from_rel[[1L]]
    path <- file.path(root, rel)
    hit <- expected[expected$source_set == "new" & expected$rel == rel, , drop = FALSE]
    dry_kind <- if (nrow(hit)) hit$kind[[1L]] else NA_character_
    dry_sha <- if (nrow(hit)) hit$sha256[[1L]] else NA_character_
    current_exists <- file.exists(path)
    current_kind <- if (!current_exists) "missing" else if (dir.exists(path)) "dir" else "file"
    current_sha <- if (current_exists) admin_pit_include_hash_path(path) else NA_character_
    kind_ok <- !("kind" %in% compare) || identical(as.character(dry_kind), as.character(current_kind))
    sha_ok <- !("sha256" %in% compare) || (!is.na(dry_sha) && identical(as.character(dry_sha), as.character(current_sha)))
    status <- if (!nrow(hit)) "missing_dry_run_fingerprint" else if (!current_exists) "missing_current_source" else if (require_sha && is.na(dry_sha)) "missing_dry_run_sha256" else if (!kind_ok) "kind_changed" else if (!sha_ok) "sha256_changed" else "ok"
    data.frame(source_id = row$source_id, country = row$country, rel = rel, dry_run_kind = dry_kind, current_kind = current_kind, dry_run_sha256 = dry_sha, current_sha256 = current_sha, status = status, stringsAsFactors = FALSE)
  })
  report <- admin_pit_include_bind(rows)
  if (nrow(report) && any(report$status != "ok", na.rm = TRUE)) {
    bad <- report[report$status != "ok", , drop = FALSE]
    stop("Confirm refused: incoming PIT source fingerprints changed since dry-run. Rerun the include dry-run. First mismatch: ", bad$rel[[1L]], " (", bad$status[[1L]], ").", call. = FALSE)
  }
  report
}

admin_pit_include_verify_promotion_fingerprints <- function(include_run, promotion_plan) {
  expected <- admin_pit_include_read_csv(file.path(include_run, "tables", "promotion_fingerprints.csv"))
  if (!nrow(promotion_plan)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    hit <- expected[expected$source_id == row$source_id & expected$artifact_type == row$artifact_type & expected$to_rel == row$to_rel, , drop = FALSE]
    dry_sha <- if (nrow(hit)) hit$sha256[[1L]] else NA_character_
    dry_kind <- if (nrow(hit)) hit$kind[[1L]] else NA_character_
    exists <- file.exists(from)
    kind <- if (!exists) "missing" else if (dir.exists(from)) "dir" else "file"
    sha <- if (exists) admin_pit_include_hash_path(from) else NA_character_
    status <- if (!nrow(hit)) "missing_dry_run_fingerprint" else if (!exists) "missing_staged_artifact" else if (!identical(as.character(dry_kind), as.character(kind))) "kind_changed" else if (!identical(as.character(dry_sha), as.character(sha))) "sha256_changed" else "ok"
    data.frame(source_id = row$source_id, country = row$country, artifact_type = row$artifact_type, from = from, to = row$to_rel, dry_run_kind = dry_kind, current_kind = kind, dry_run_sha256 = dry_sha, current_sha256 = sha, status = status, stringsAsFactors = FALSE)
  })
  report <- admin_pit_include_bind(rows)
  if (nrow(report) && any(report$status != "ok", na.rm = TRUE)) {
    bad <- report[report$status != "ok", , drop = FALSE]
    stop("Confirm refused: staged admin promotion artifacts changed since dry-run. Rerun the include dry-run. First mismatch: ", bad$to[[1L]], " (", bad$status[[1L]], ").", call. = FALSE)
  }
  report
}

admin_pit_include_confirm_manifest <- function(confirm_id, include_run, status) {
  data.frame(key = c("confirm_id", "source_type", "workflow", "status", "include_run", "confirmed_at"), value = c(confirm_id, "admin", "admin_pit", status, normalizePath(include_run, mustWork = FALSE), as.character(Sys.time())), stringsAsFactors = FALSE)
}

admin_pit_include_confirm_sources <- function(root = admin_pit_include_repo_root(), contract_path = file.path(root, "config", "admin_pit_include.yml"), include_run = NULL, output_dir = NULL) {
  contract <- admin_pit_include_read_contract(root, contract_path)
  include_run <- admin_pit_include_resolve_run(root, contract, include_run)
  include_manifest <- admin_pit_include_read_csv(file.path(include_run, "logs", "include_manifest.csv"))
  status <- admin_pit_include_manifest_value(include_manifest, "status")
  if (!identical(status, "all_good")) stop("Admin PIT include run is not clean; status is ", status, ".", call. = FALSE)
  mappings <- admin_pit_include_read_csv(file.path(include_run, "tables", "staged_source_mappings.csv"))
  promotion_plan <- admin_pit_include_read_csv(file.path(include_run, "tables", "promotion_plan.csv"))
  if (!nrow(promotion_plan)) stop("Admin PIT include run has no staged artifacts to promote.", call. = FALSE)
  fingerprint_check <- admin_pit_include_verify_source_fingerprints(root, include_run, mappings, contract)
  artifact_fingerprint_check <- admin_pit_include_verify_promotion_fingerprints(include_run, promotion_plan)
  confirm_id <- admin_pit_include_confirm_id()
  paths <- admin_pit_include_output_paths_for_confirm(root, contract, output_dir, confirm_id)
  report <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    to <- file.path(root, row$to_rel[[1L]])
    backup <- file.path(paths$snapshots, "original", row$to_rel[[1L]])
    backup_status <- if (file.exists(to)) admin_pit_include_copy_path(to, backup) else "destination_absent"
    promote_status <- admin_pit_include_copy_path(from, to)
    data.frame(source_id = row$source_id, country = row$country, artifact_type = row$artifact_type, from = from, to = row$to_rel, backup = if (identical(backup_status, "destination_absent")) "" else admin_pit_include_relative_path(backup, root), backup_status = if (identical(backup_status, "staged")) "backed_up" else backup_status, promote_status = promote_status, stringsAsFactors = FALSE)
  })
  promote_report <- admin_pit_include_bind(report)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(promote_report, file.path(paths$tables, "promote_report.csv"), row.names = FALSE, na = "")
  utils::write.csv(fingerprint_check, file.path(paths$tables, "source_fingerprint_check.csv"), row.names = FALSE, na = "")
  utils::write.csv(artifact_fingerprint_check, file.path(paths$tables, "staged_artifact_fingerprint_check.csv"), row.names = FALSE, na = "")
  manifest <- admin_pit_include_confirm_manifest(confirm_id, include_run, "confirmed")
  utils::write.csv(manifest, file.path(paths$logs, "confirm_manifest.csv"), row.names = FALSE, na = "")
  list(paths = paths, outputs = list(promote_report = promote_report, source_fingerprint_check = fingerprint_check, staged_artifact_fingerprint_check = artifact_fingerprint_check), manifest = manifest, contract = contract)
}

admin_pit_include_restore_sources <- function(root = admin_pit_include_repo_root(), contract_path = file.path(root, "config", "admin_pit_include.yml"), confirm_run = NULL) {
  contract <- admin_pit_include_read_contract(root, contract_path)
  confirm_run <- admin_pit_include_resolve_confirm(root, contract, confirm_run)
  promote_report <- admin_pit_include_read_csv(file.path(confirm_run, "tables", "promote_report.csv"))
  rows <- lapply(seq_len(nrow(promote_report)), function(i) {
    row <- promote_report[i, , drop = FALSE]
    dest <- file.path(root, row$to[[1L]])
    if (identical(row$backup_status[[1L]], "backed_up")) {
      backup <- file.path(root, row$backup[[1L]])
      restore_status <- admin_pit_include_copy_path(backup, dest)
    } else if (identical(row$backup_status[[1L]], "destination_absent")) {
      if (file.exists(dest)) unlink(dest, recursive = TRUE)
      restore_status <- "removed_promoted_destination"
    } else {
      restore_status <- "no_backup_available"
    }
    data.frame(source_id = row$source_id, country = row$country, artifact_type = row$artifact_type %||% "", destination = row$to, restore_status = restore_status, stringsAsFactors = FALSE)
  })
  restore_report <- admin_pit_include_bind(rows)
  utils::write.csv(restore_report, file.path(confirm_run, "tables", "restore_report.csv"), row.names = FALSE, na = "")
  list(paths = list(root = confirm_run, restore_report = file.path(confirm_run, "tables", "restore_report.csv")), outputs = list(restore_report = restore_report), contract = contract)
}
