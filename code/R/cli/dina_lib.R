dina_repo_root <- function(start = getwd()) {
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

dina_path <- function(..., root = dina_repo_root()) {
  file.path(root, ...)
}

dina_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

dina_read_yaml <- function(path, default = list()) {
  dina_need("yaml")
  if (!file.exists(path)) {
    return(default)
  }
  yaml::read_yaml(path)
}

dina_write_yaml <- function(x, path) {
  dina_need("yaml")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(x, path)
}

dina_read_json <- function(path, default = list()) {
  dina_need("jsonlite")
  if (!file.exists(path)) {
    return(default)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

dina_write_json <- function(x, path, pretty = TRUE) {
  dina_need("jsonlite")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, pretty = pretty, auto_unbox = TRUE, null = "null")
}

dina_expand_env <- function(x) {
  if (is.list(x)) {
    return(lapply(x, dina_expand_env))
  }
  if (!is.character(x)) {
    return(x)
  }
  vapply(x, dina_expand_env_scalar, character(1), USE.NAMES = FALSE)
}

dina_expand_env_scalar <- function(value) {
  matches <- gregexpr("\\$\\{([A-Za-z_][A-Za-z0-9_]*)\\}", value, perl = TRUE)
  found <- regmatches(value, matches)[[1]]
  if (!length(found) || identical(found, character(0))) {
    return(value)
  }
  replacement <- found
  for (i in seq_along(found)) {
    name <- sub("^\\$\\{", "", sub("\\}$", "", found[[i]]))
    replacement[[i]] <- Sys.getenv(name, unset = "")
  }
  regmatches(value, matches) <- list(replacement)
  value
}

dina_config_path <- function(root = dina_repo_root()) {
  dina_path("config", "dina.yml", root = root)
}

dina_pipeline_path <- function(root = dina_repo_root()) {
  dina_path("config", "pipeline.yml", root = root)
}

dina_sources_path <- function(root = dina_repo_root()) {
  dina_path("config", "sources.yml", root = root)
}

dina_pushover_local_path <- function(root = dina_repo_root()) {
  dina_path("config", "pushover.local.R", root = root)
}

dina_pushover_example_path <- function(root = dina_repo_root()) {
  dina_path("config", "pushover.local.R.example", root = root)
}

dina_pushover_template <- function() {
  c(
    "# Local Pushover credentials for DINA-LatAm.",
    "# This file is ignored by Git. Replace the placeholders with your values.",
    "pushoverr::set_pushover_user(user = \"xxxxxx\")",
    "pushoverr::set_pushover_app(token = \"xxxxxx\")"
  )
}

dina_notify_init <- function(root = dina_repo_root(), overwrite = FALSE) {
  path <- dina_pushover_local_path(root)
  if (file.exists(path) && !overwrite) {
    return(list(path = path, created = FALSE))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(dina_pushover_template(), path)
  list(path = path, created = TRUE)
}

dina_config <- function(root = dina_repo_root(), expand_env = TRUE) {
  config <- dina_read_yaml(dina_config_path(root))
  if (expand_env) {
    config <- dina_expand_env(config)
  }
  config
}

dina_pipeline <- function(root = dina_repo_root()) {
  x <- dina_read_yaml(dina_pipeline_path(root), default = list(tasks = list()))
  if (is.null(x$tasks)) {
    x$tasks <- list()
  }
  x
}

dina_sources <- function(root = dina_repo_root()) {
  x <- dina_read_yaml(dina_sources_path(root), default = list(sources = list()))
  if (is.null(x$sources)) {
    x$sources <- list()
  }
  x
}

dina_source_values <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character())
  }
  if (is.character(x)) {
    return(x[!is.na(x) & nzchar(x)])
  }
  if (is.list(x)) {
    values <- unlist(x, use.names = FALSE)
    if (is.character(values)) {
      return(values[!is.na(values) & nzchar(values)])
    }
  }
  character()
}

dina_source_field <- function(source, name, default = character()) {
  value <- source[[name, exact = TRUE]]
  if (is.null(value)) {
    default
  } else {
    value
  }
}

dina_source_urls <- function(source) {
  out <- dina_source_values(dina_source_field(source, "url"))
  urls <- dina_source_field(source, "urls", list())
  if (is.character(urls)) {
    out <- c(out, dina_source_values(urls))
  } else if (is.list(urls) && length(urls)) {
    formatted <- unlist(lapply(urls, function(item) {
      if (is.character(item) && is.null(names(item))) {
        return(dina_source_values(item))
      }
      url <- dina_source_field(item, "url", "")
      if (!is.character(url) || !length(url) || is.na(url[[1]]) || !nzchar(url[[1]])) {
        return(character())
      }
      label <- paste(dina_source_values(c(dina_source_field(item, "country"), dina_source_field(item, "label"))), collapse = " ")
      if (nzchar(label)) {
        sprintf("%s: %s", label, url[[1]])
      } else {
        url[[1]]
      }
    }), use.names = FALSE)
    out <- c(out, formatted)
  }
  unique(out[!is.na(out) & nzchar(out)])
}

dina_source_has_value <- function(x) {
  length(dina_source_values(x)) > 0
}

dina_source_country_matches <- function(source_country, country) {
  if (is.null(country) || !nzchar(country)) {
    return(TRUE)
  }
  values <- unlist(strsplit(dina_source_values(source_country), ",", fixed = TRUE), use.names = FALSE)
  values <- toupper(trimws(values))
  toupper(country) %in% values || "MULTI" %in% values
}

dina_source_registry <- function(root = dina_repo_root(), family = NULL, country = NULL) {
  registry <- dina_sources(root)$sources
  if (!is.null(family) && nzchar(family)) {
    registry <- registry[vapply(registry, function(source) identical(dina_source_field(source, "family", ""), family), logical(1))]
  }
  if (!is.null(country) && nzchar(country)) {
    registry <- registry[vapply(registry, function(source) dina_source_country_matches(dina_source_field(source, "country", ""), country), logical(1))]
  }
  registry
}

dina_source_by_id <- function(id, root = dina_repo_root()) {
  registry <- dina_sources(root)$sources
  matches <- registry[vapply(registry, function(source) identical(dina_source_field(source, "id", ""), id), logical(1))]
  if (!length(matches)) {
    stop("Unknown source id: ", id, call. = FALSE)
  }
  matches[[1]]
}

dina_bool_stata <- function(x) {
  if (isTRUE(x)) {
    "yes"
  } else {
    "no"
  }
}

dina_stata_quote_list <- function(values) {
  if (length(values) == 0) {
    return("\" \"")
  }
  paste0("\" ", paste(sprintf("\"%s\"", values), collapse = " "), " \"")
}

dina_render_config_do <- function(config = dina_config(), path = NULL) {
  lines <- c(
    "* Generated by dina. Do not edit this file by hand.",
    sprintf("global all_countries %s", dina_stata_quote_list(config$countries %||% character())),
    sprintf("global first_y %s", config$years$first %||% ""),
    sprintf("global last_y %s", config$years$last %||% ""),
    "",
    sprintf("global lang \"%s\"", config$run$lang %||% "eng"),
    sprintf("global debug \"%s\"", dina_bool_stata(config$run$debug %||% FALSE)),
    sprintf("global bfm_replace \"%s\"", dina_bool_stata(config$run$bfm_replace %||% FALSE)),
    "",
    sprintf("global all_units %s", dina_stata_quote_list(config$run$units %||% character())),
    sprintf("global all_steps %s", dina_stata_quote_list(config$run$steps %||% character()))
  )

  if (!is.null(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, path)
  }
  lines
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

dina_hash_file <- function(path) {
  dina_need("digest")
  if (!file.exists(path) || dir.exists(path)) {
    return(NA_character_)
  }
  digest::digest(file = path, algo = "sha256")
}

dina_hash_many <- function(paths, root = dina_repo_root()) {
  files <- dina_expand_paths(paths, root = root)
  files <- unique(unlist(lapply(files, function(path) {
    if (dir.exists(path)) {
      listed <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      if (length(listed)) listed[!dir.exists(listed)] else path
    } else {
      path
    }
  }), use.names = FALSE))
  stats <- file.info(files)
  data.frame(
    path = dina_relative(files, root),
    exists = file.exists(files),
    size = ifelse(is.na(stats$size), NA_real_, stats$size),
    mtime = ifelse(is.na(stats$mtime), NA_character_, format(stats$mtime, "%Y-%m-%dT%H:%M:%OS%z")),
    sha256 = vapply(files, dina_hash_file, character(1)),
    stringsAsFactors = FALSE
  )
}

dina_expand_paths <- function(paths, root = dina_repo_root()) {
  if (length(paths) == 0) {
    return(character())
  }
  out <- character()
  for (path in paths) {
    full <- if (grepl("^/", path)) path else file.path(root, path)
    matches <- Sys.glob(full)
    if (length(matches)) {
      out <- c(out, matches)
    } else {
      out <- c(out, full)
    }
  }
  unique(normalizePath(out, mustWork = FALSE))
}

dina_relative <- function(paths, root = dina_repo_root()) {
  root <- normalizePath(root, mustWork = FALSE)
  paths <- normalizePath(paths, mustWork = FALSE)
  sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", root), "/?"), "", paths)
}

dina_years_from_text <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(integer())
  }
  txt <- paste(x, collapse = " ")
  matches <- regmatches(txt, gregexpr("(?<![0-9])(19|20)[0-9]{2}(?![0-9])", txt, perl = TRUE))[[1]]
  years <- unique(as.integer(matches))
  years[!is.na(years)]
}

dina_years_from_filename <- function(path) {
  years <- dina_years_from_text(basename(path))
  csi <- regmatches(basename(path), regexec("^CSI_([0-9]{1,2})\\.", basename(path)))[[1]]
  if (length(csi) == 2) {
    years <- unique(c(years, 2000L + as.integer(csi[2])))
  }
  years
}

dina_excel_sheets_safe <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return(character())
  }
  tryCatch(readxl::excel_sheets(path), error = function(e) character())
}

dina_source_file_signature <- function(path, root = dina_repo_root(), deep = FALSE, hash = deep) {
  ext <- tolower(tools::file_ext(path))
  sheets <- if (isTRUE(deep) && ext %in% c("xls", "xlsx", "xlsb")) dina_excel_sheets_safe(path) else character()
  years <- unique(c(dina_years_from_filename(path), dina_years_from_text(sheets)))
  info <- file.info(path)
  list(
    path = dina_relative(path, root),
    exists = file.exists(path),
    size = if (file.exists(path)) unname(info$size) else NA_real_,
    mtime = if (file.exists(path)) format(info$mtime, "%Y-%m-%dT%H:%M:%OS%z") else NA_character_,
    sha256 = if (isTRUE(hash)) dina_hash_file(path) else NA_character_,
    filename_years = as.integer(dina_years_from_filename(path)),
    sheet_years = as.integer(dina_years_from_text(sheets)),
    detected_years = as.integer(sort(unique(years))),
    sheets = sheets
  )
}

dina_template_value <- function(x, values = list()) {
  if (is.null(x) || !length(x)) {
    return("")
  }
  value <- x[[1]]
  defaults <- c(values, list(date = format(Sys.Date(), "%Y-%m-%d")))
  for (name in names(defaults)) {
    value <- gsub(paste0("\\{", name, "\\}"), as.character(defaults[[name]]), value)
  }
  value
}

dina_sources_refresh <- function(session, root = dina_repo_root(), source_ids = NULL, dry_run = FALSE) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  registry <- dina_sources(root)$sources
  if (!is.null(source_ids)) {
    registry <- registry[vapply(registry, function(x) x$id %in% source_ids, logical(1))]
  }
  staging_root <- file.path(dina_update_dir(session$id, root), "source_staging")
  results <- list()
  for (source in registry) {
    method <- source$method %||% "manual"
    url <- dina_source_values(dina_source_field(source, "url"))
    url <- if (length(url)) url[[1]] else ""
    target_rel <- dina_template_value(source$staging_name %||% source$id)
    target <- file.path(staging_root, target_rel)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    supported <- method %in% c("static_url", "zip_archive")
    if (!supported || !nzchar(url)) {
      results[[source$id]] <- list(id = source$id, status = "manual_or_adapter", method = method, target = dina_relative(target, root))
      next
    }
    if (dry_run) {
      results[[source$id]] <- list(id = source$id, status = "dry_run", url = url, target = dina_relative(target, root))
      next
    }
    ok <- tryCatch({
      utils::download.file(url, target, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      results[[source$id]] <<- list(id = source$id, status = "failed", url = url, target = dina_relative(target, root), error = conditionMessage(e))
      FALSE
    })
    if (ok) {
      results[[source$id]] <- list(id = source$id, status = "staged", url = url, target = dina_relative(target, root), sha256 = dina_hash_file(target))
    }
  }
  session$source_refreshes[[dina_now()]] <- results
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  results
}

dina_sources_integrate_file <- function(session, staged_rel, dest_rel, source_id = NULL, root = dina_repo_root(), overwrite = FALSE) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  staging_root <- file.path(dina_update_dir(session$id, root), "source_staging")
  staged <- normalizePath(file.path(staging_root, staged_rel), mustWork = FALSE)
  staging_norm <- normalizePath(staging_root, mustWork = FALSE)
  if (!startsWith(staged, staging_norm)) {
    stop("Staged file must live under the active update source_staging directory.", call. = FALSE)
  }
  if (!file.exists(staged)) {
    stop("Staged file does not exist: ", staged_rel, call. = FALSE)
  }
  dest <- if (grepl("^/", dest_rel)) dest_rel else file.path(root, dest_rel)
  if (file.exists(dest) && !overwrite) {
    stop("Destination exists; pass --yes to overwrite: ", dest_rel, call. = FALSE)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(staged, dest, overwrite = overwrite, copy.date = TRUE)
  decision <- list(
    source_id = source_id %||% NA_character_,
    staged = dina_relative(staged, root),
    destination = dina_relative(dest, root),
    sha256 = dina_hash_file(dest),
    integrated_at = dina_now()
  )
  session$source_decisions[[length(session$source_decisions) + 1L]] <- decision
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  decision
}

dina_scan_sources <- function(root = dina_repo_root(), include_missing = TRUE, deep = FALSE, hash = deep) {
  registry <- dina_sources(root)$sources
  out <- list()
  for (source in registry) {
    patterns <- source$canonical %||% character()
    files <- dina_expand_paths(patterns, root = root)
    exists <- file.exists(files)
    if (!include_missing) {
      files <- files[exists]
    }
    signatures <- lapply(files, dina_source_file_signature, root = root, deep = deep, hash = hash)
    years <- sort(unique(unlist(lapply(signatures, function(x) x$detected_years))))
    out[[source$id]] <- list(
      id = source$id,
      family = source$family %||% NA_character_,
      country = source$country %||% NA_character_,
      method = source$method %||% NA_character_,
      files = signatures,
      detected_years = as.integer(years)
    )
  }
  out
}

dina_classify_source_changes <- function(current, previous = list()) {
  out <- list()
  for (id in names(current)) {
    cur <- current[[id]]
    prev <- previous[[id]] %||% list(files = list(), detected_years = integer())
    cur_years <- cur$detected_years %||% integer()
    prev_years <- prev$detected_years %||% integer()

    cur_files <- setNames(lapply(cur$files, function(x) x$sha256), vapply(cur$files, function(x) x$path, character(1)))
    prev_files <- setNames(lapply(prev$files %||% list(), function(x) x$sha256), vapply(prev$files %||% list(), function(x) x$path, character(1)))
    common <- intersect(names(cur_files), names(prev_files))
    common_hashable <- common[!is.na(unlist(cur_files[common])) & !is.na(unlist(prev_files[common]))]
    changed_common <- common_hashable[vapply(common_hashable, function(path) !identical(cur_files[[path]], prev_files[[path]]), logical(1))]

    classes <- character()
    if (length(setdiff(cur_years, prev_years))) {
      new_years <- setdiff(cur_years, prev_years)
      if (length(prev_years) && any(new_years < max(prev_years))) {
        classes <- c(classes, "backfill")
      }
      if (length(prev_years) == 0 || any(new_years > max(prev_years))) {
        classes <- c(classes, "new_year")
      }
    }
    if (length(changed_common)) {
      classes <- c(classes, "historical_revision")
    }
    if (!length(cur$files) || all(!vapply(cur$files, function(x) isTRUE(x$exists), logical(1)))) {
      classes <- c(classes, "local_missing")
    }
    if (!length(classes)) {
      classes <- "unchanged"
    }
    out[[id]] <- list(
      id = id,
      classes = unique(classes),
      current_years = as.integer(cur_years),
      previous_years = as.integer(prev_years),
      changed_files = changed_common
    )
  }
  out
}

dina_active_update_file <- function(root = dina_repo_root()) {
  dina_path("output", "updates", ".active_update", root = root)
}

dina_update_dir <- function(update_id, root = dina_repo_root()) {
  dina_path("output", "updates", update_id, root = root)
}

dina_current_update <- function(root = dina_repo_root()) {
  path <- dina_active_update_file(root)
  if (!file.exists(path)) {
    return(NULL)
  }
  id <- trimws(readLines(path, warn = FALSE)[1])
  if (!nzchar(id)) {
    return(NULL)
  }
  id
}

dina_session_manifest_path <- function(update_id, root = dina_repo_root()) {
  file.path(dina_update_dir(update_id, root), "manifest.json")
}

dina_load_session <- function(update_id = dina_current_update(root), root = dina_repo_root()) {
  if (is.null(update_id)) {
    return(NULL)
  }
  dina_read_json(dina_session_manifest_path(update_id, root), default = NULL)
}

dina_save_session <- function(session, root = dina_repo_root()) {
  dina_write_json(session, dina_session_manifest_path(session$id, root))
}

dina_update_start <- function(year = format(Sys.Date(), "%Y"), id = NULL, root = dina_repo_root()) {
  if (is.null(id) || !nzchar(id)) {
    base_id <- sprintf("%s-update-%s", year, format(Sys.Date(), "%m-%d"))
    id <- base_id
    suffix <- 1L
    while (dir.exists(dina_update_dir(id, root))) {
      suffix <- suffix + 1L
      id <- sprintf("%s-%02d", base_id, suffix)
    }
  }
  dir <- dina_update_dir(id, root)
  dir.create(file.path(dir, "source_staging"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "logs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "snapshots"), recursive = TRUE, showWarnings = FALSE)

  config <- dina_config(root)
  config_do <- file.path(dir, "config.do")
  dina_render_config_do(config, config_do)
  source_scan <- dina_scan_sources(root)
  session <- list(
    id = id,
    year = as.character(year),
    status = "initialized",
    created_at = dina_now(),
    updated_at = dina_now(),
    config_file = dina_relative(config_do, root),
    config_hash = dina_hash_file(config_do),
    source_scan = source_scan,
    source_refreshes = list(),
    source_decisions = list(),
    task_runs = list(),
    checklist = dina_default_checklist()
  )
  dina_save_session(session, root)
  dir.create(dirname(dina_active_update_file(root)), recursive = TRUE, showWarnings = FALSE)
  writeLines(id, dina_active_update_file(root))
  session
}

dina_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
}

dina_default_checklist <- function() {
  list(
    list(id = "preflight", label = "Run doctor and fix missing dependencies", status = "pending"),
    list(id = "sources_refresh", label = "Refresh staged source downloads", status = "pending"),
    list(id = "sources_review", label = "Review source coverage and changes", status = "pending"),
    list(id = "sources_integrate", label = "Integrate approved sources", status = "pending"),
    list(id = "pipeline_run", label = "Run stale pipeline tasks", status = "pending"),
    list(id = "validation", label = "Review availability reports and WID exports", status = "pending"),
    list(id = "archive", label = "Pack primary data archive", status = "pending"),
    list(id = "finalize", label = "Finalize update snapshot", status = "pending")
  )
}

dina_session_state <- function(session, root = dina_repo_root()) {
  if (is.null(session)) {
    return(list(state = "no_active_update", recommendation = "Start an update with `dina update start YEAR`."))
  }
  task_status <- dina_all_task_status(root = root, session = session)
  stale <- sum(vapply(task_status, function(x) x$status %in% c("missing_outputs", "stale", "upstream_stale", "missing_inputs"), logical(1)))
  failed <- names(session$task_runs)[vapply(session$task_runs, function(x) identical(x$status, "failed"), logical(1))]
  staged <- list.files(file.path(dina_update_dir(session$id, root), "source_staging"), recursive = TRUE, all.files = FALSE)

  if (length(failed)) {
    return(list(state = "failed", recommendation = sprintf("Inspect logs and retry `%s`.", failed[[length(failed)]]), stale_tasks = stale))
  }
  if (length(staged)) {
    return(list(state = "sources_pending", recommendation = "Review staged downloads with `dina sources review`.", stale_tasks = stale))
  }
  if (stale > 0) {
    return(list(state = "build_ready", recommendation = "Run or inspect stale tasks with `dina tasks list` and `dina tasks why TASK`.", stale_tasks = stale))
  }
  list(state = "review_ready", recommendation = "Review outputs, pack archives, then run `dina update finalize`.", stale_tasks = stale)
}

dina_task_map <- function(root = dina_repo_root()) {
  tasks <- dina_pipeline(root)$tasks
  ids <- vapply(tasks, function(x) x$id, character(1))
  names(tasks) <- ids
  tasks
}

dina_task_short_id <- function(id) {
  match <- regmatches(id, regexpr("^[0-9]{2}[A-Za-z]", id, perl = TRUE))
  if (!length(match) || identical(match, "")) id else match
}

dina_split_task_selectors <- function(x) {
  if (is.null(x) || !length(x)) {
    return(character())
  }
  selectors <- unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE)
  selectors <- trimws(selectors)
  unique(selectors[nzchar(selectors)])
}

dina_selector_candidates <- function(selector, ids) {
  block <- substr(selector, 1L, 2L)
  candidates <- ids[startsWith(tolower(ids), tolower(block))]
  if (!length(candidates)) {
    candidates <- ids
  }
  paste(utils::head(candidates, 8L), collapse = ", ")
}

dina_resolve_task_selector <- function(selector, ids, mode = c("all", "single", "from", "to")) {
  mode <- match.arg(mode)
  selector <- trimws(selector)
  if (!nzchar(selector)) {
    stop("Empty task selector.", call. = FALSE)
  }

  ids_lower <- tolower(ids)
  selector_lower <- tolower(selector)
  exact <- which(ids_lower == selector_lower)
  if (length(exact) == 1L) {
    return(ids[[exact]])
  }

  if (grepl("^[0-9]{2}[A-Za-z]$", selector, perl = TRUE)) {
    hits <- which(startsWith(ids_lower, paste0(selector_lower, "-")))
    if (length(hits) == 1L) {
      return(ids[[hits]])
    }
    if (length(hits) > 1L) {
      stop(
        sprintf("Task selector `%s` is ambiguous. Candidates: %s", selector, paste(ids[hits], collapse = ", ")),
        call. = FALSE
      )
    }
  } else if (grepl("^[0-9]{2}$", selector, perl = TRUE)) {
    hits <- which(startsWith(ids_lower, selector_lower))
    if (length(hits)) {
      if (identical(mode, "from")) {
        return(ids[[hits[[1L]]]])
      }
      if (identical(mode, "to")) {
        return(ids[[hits[[length(hits)]]]])
      }
      if (identical(mode, "single")) {
        stop(
          sprintf("Task selector `%s` matches multiple tasks. Candidates: %s", selector, paste(ids[hits], collapse = ", ")),
          call. = FALSE
        )
      }
      return(ids[hits])
    }
  }

  stop(
    sprintf("Unknown task selector `%s`. Candidates include: %s", selector, dina_selector_candidates(selector, ids)),
    call. = FALSE
  )
}

dina_resolve_task_selectors <- function(selectors, ids, mode = c("all", "single", "from", "to")) {
  mode <- match.arg(mode)
  selectors <- dina_split_task_selectors(selectors)
  if (!length(selectors)) {
    return(character())
  }
  resolved <- unlist(lapply(selectors, dina_resolve_task_selector, ids = ids, mode = mode), use.names = FALSE)
  ids[ids %in% unique(resolved)]
}

dina_select_tasks <- function(root = dina_repo_root(), task = NULL, stage = NULL, from = NULL, to = NULL) {
  tasks <- dina_pipeline(root)$tasks
  ids <- vapply(tasks, function(x) x$id, character(1))
  keep <- rep(TRUE, length(tasks))
  if (!is.null(stage)) {
    keep <- keep & vapply(tasks, function(x) identical(x$stage, stage), logical(1))
  }
  if (!is.null(task)) {
    keep <- keep & ids %in% dina_resolve_task_selectors(task, ids, mode = "all")
  }
  if (!is.null(from)) {
    from_id <- dina_resolve_task_selector(from, ids, mode = "from")
    start <- match(from_id, ids)
    keep <- keep & seq_along(tasks) >= start
  }
  if (!is.null(to)) {
    to_id <- dina_resolve_task_selector(to, ids, mode = "to")
    end <- match(to_id, ids)
    keep <- keep & seq_along(tasks) <= end
  }
  tasks[keep]
}

dina_path_mtime <- function(path) {
  if (!file.exists(path)) {
    return(as.POSIXct(NA))
  }
  file.info(path)$mtime
}

dina_latest_mtime <- function(paths, root = dina_repo_root()) {
  files <- dina_expand_paths(paths, root)
  files <- files[file.exists(files)]
  if (!length(files)) {
    return(as.POSIXct(NA))
  }
  max(file.info(files)$mtime, na.rm = TRUE)
}

dina_earliest_mtime <- function(paths, root = dina_repo_root()) {
  files <- dina_expand_paths(paths, root)
  files <- files[file.exists(files)]
  if (!length(files)) {
    return(as.POSIXct(NA))
  }
  min(file.info(files)$mtime, na.rm = TRUE)
}

dina_task_status <- function(task, root = dina_repo_root(), session = NULL, seen = character()) {
  inputs <- unique(c(task$inputs %||% character(), task$script %||% character(), "_config.do", "config/dina.yml"))
  outputs <- task$outputs %||% character()

  input_files <- dina_expand_paths(inputs, root)
  output_files <- dina_expand_paths(outputs, root)
  missing_inputs <- input_files[!file.exists(input_files)]
  existing_outputs <- output_files[file.exists(output_files)]

  reasons <- character()
  status <- "current"
  if (length(missing_inputs)) {
    status <- "missing_inputs"
    reasons <- c(reasons, sprintf("Missing input: %s", dina_relative(missing_inputs, root)))
  }
  if (!length(existing_outputs)) {
    status <- if (identical(status, "current")) "missing_outputs" else status
    reasons <- c(reasons, "No declared outputs exist.")
  }

  latest_input <- dina_latest_mtime(inputs, root)
  earliest_output <- dina_earliest_mtime(outputs, root)
  if (!is.na(latest_input) && !is.na(earliest_output) && latest_input > earliest_output) {
    status <- if (identical(status, "current")) "stale" else status
    reasons <- c(
      reasons,
      sprintf("An input/config/script changed at %s, newer than earliest output %s.", latest_input, earliest_output)
    )
  }

  if (!is.null(session)) {
    run <- session$task_runs[[task$id]] %||% NULL
    if (is.null(run)) {
      status <- if (identical(status, "current")) "never_run" else status
      reasons <- c(reasons, "No accepted run is recorded in the active update session.")
    } else if (identical(run$status, "failed")) {
      status <- "failed"
      reasons <- c(reasons, "Last recorded run failed.")
    }
  }

  if (!length(reasons)) {
    reasons <- "All declared outputs are present and not older than declared inputs."
  }
  list(id = task$id, stage = task$stage %||% NA_character_, status = status, reasons = reasons)
}

dina_all_task_status <- function(root = dina_repo_root(), session = dina_load_session(root = root)) {
  tasks <- dina_task_map(root)
  out <- lapply(tasks, dina_task_status, root = root, session = session)
  names(out) <- names(tasks)
  out
}

dina_topological_downstream <- function(task_ids, root = dina_repo_root()) {
  tasks <- dina_task_map(root)
  selected <- character()
  for (id in names(tasks)) {
    if (id %in% task_ids || any(tasks[[id]]$deps %||% character() %in% selected)) {
      selected <- c(selected, id)
    }
  }
  selected
}

dina_run_task <- function(task, root = dina_repo_root(), session = dina_load_session(root = root), dry_run = TRUE, force = FALSE) {
  status <- dina_task_status(task, root = root, session = session)
  if (!force && identical(status$status, "current")) {
    return(list(task = task$id, status = "skipped", reason = "Task is current."))
  }
  config <- dina_config(root)
  stata_cmd <- Sys.getenv("DINA_STATA_CMD", unset = config$stata$command %||% "")
  if (identical(task$type, "stata")) {
    command <- c(if (nzchar(stata_cmd)) stata_cmd else "<DINA_STATA_CMD>", config$stata$batch_args %||% c("-b", "do"), task$script)
  } else if (identical(task$type, "r")) {
    command <- c("Rscript", task$script)
  } else {
    command <- c(task$type %||% "unknown", task$script)
  }
  if (dry_run) {
    return(list(task = task$id, status = "dry_run", command = command, reasons = status$reasons))
  }
  if (!nzchar(command[[1]]) || identical(command[[1]], "<DINA_STATA_CMD>")) {
    stop("No executable configured for task ", task$id, call. = FALSE)
  }
  dina_need("processx")
  run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
  log_dir <- dina_path("output", "run_logs", run_id, root = root)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  env <- c(DINA_CONFIG_DO = if (!is.null(session)) file.path(dina_update_dir(session$id, root), "config.do") else "")
  result <- processx::run(command[[1]], command[-1], wd = root, echo = TRUE, error_on_status = FALSE, env = env)
  status_value <- if (identical(result$status, 0L)) "succeeded" else "failed"
  writeLines(result$stdout, file.path(log_dir, paste0(task$id, ".out.log")))
  writeLines(result$stderr, file.path(log_dir, paste0(task$id, ".err.log")))
  if (!is.null(session)) {
    session$task_runs[[task$id]] <- list(status = status_value, command = command, run_id = run_id, ended_at = dina_now())
    session$updated_at <- dina_now()
    dina_save_session(session, root)
  }
  list(task = task$id, status = status_value, command = command, exit_status = result$status, log_dir = dina_relative(log_dir, root))
}

dina_make_export <- function(path = dina_path("Makefile.dina", root = root), root = dina_repo_root()) {
  tasks <- dina_pipeline(root)$tasks
  lines <- c(
    "# Generated by dina. The YAML task graph remains authoritative.",
    ".PHONY: all",
    sprintf("all: %s", paste(vapply(tasks, function(x) x$id, character(1)), collapse = " ")),
    ""
  )
  for (task in tasks) {
    deps <- paste(task$deps %||% character(), collapse = " ")
    script <- task$script %||% ""
    lines <- c(
      lines,
      sprintf(".PHONY: %s", task$id),
      sprintf("%s: %s", task$id, deps),
      sprintf("\t@echo \"dina run --task %s\"", task$id),
      sprintf("\t@%s run --task %s", file.path("bin", "dina"), task$id),
      ""
    )
  }
  writeLines(lines, path)
  path
}

dina_set_nested <- function(x, key, value) {
  parts <- strsplit(key, "\\.", fixed = FALSE)[[1]]
  target <- x
  assign_recur <- function(obj, i) {
    if (i == length(parts)) {
      obj[[parts[[i]]]] <- dina_parse_scalar(value)
    } else {
      obj[[parts[[i]]]] <- assign_recur(obj[[parts[[i]]]] %||% list(), i + 1L)
    }
    obj
  }
  assign_recur(target, 1L)
}

dina_parse_scalar <- function(value) {
  if (tolower(value) %in% c("true", "false")) {
    return(tolower(value) == "true")
  }
  if (grepl("^-?[0-9]+$", value)) {
    return(as.integer(value))
  }
  if (grepl(",", value, fixed = TRUE)) {
    return(trimws(strsplit(value, ",", fixed = TRUE)[[1]]))
  }
  value
}

dina_copy_tree <- function(from, to) {
  if (!dir.exists(from)) {
    return(invisible(FALSE))
  }
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(from, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  for (file in files) {
    rel <- substr(file, nchar(from) + 2L, nchar(file))
    dest <- file.path(to, rel)
    if (dir.exists(file)) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    } else {
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      file.copy(file, dest, overwrite = TRUE, copy.date = TRUE)
    }
  }
  invisible(TRUE)
}

dina_finalize_update <- function(session, root = dina_repo_root(), force = FALSE) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  statuses <- dina_all_task_status(root, session)
  blockers <- statuses[vapply(statuses, function(x) x$status %in% c("missing_outputs", "missing_inputs", "failed", "stale"), logical(1))]
  if (length(blockers) && !force) {
    return(list(ok = FALSE, blockers = lapply(blockers, function(x) x$reasons)))
  }
  session_dir <- dina_update_dir(session$id, root)
  snapshot_dir <- file.path(session_dir, "snapshots", "final_outputs")
  config <- dina_config(root)
  for (path in config$paths$final_outputs %||% character()) {
    full <- file.path(root, path)
    if (dir.exists(full)) {
      dina_copy_tree(full, file.path(snapshot_dir, path))
    }
  }
  session$status <- "finalized"
  session$finalized_at <- dina_now()
  session$final_hashes <- dina_hash_many(config$paths$final_outputs %||% character(), root)
  dina_save_session(session, root)
  list(ok = TRUE, snapshot_dir = dina_relative(snapshot_dir, root))
}

dina_pack_data <- function(root = dina_repo_root(), archive = NULL) {
  config <- dina_config(root)
  dir.create(file.path(root, config$archives$default_dir %||% "output/archives"), recursive = TRUE, showWarnings = FALSE)
  if (is.null(archive)) {
    archive <- file.path(root, config$archives$default_dir %||% "output/archives", paste0("primary-data-", format(Sys.Date(), "%Y-%m-%d"), ".tar.gz"))
  } else if (!grepl("^/", archive)) {
    archive <- file.path(root, archive)
  }
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  paths <- config$archives$primary_paths %||% c("input_data", "previous_series")
  paths <- paths[file.exists(paths)]
  utils::tar(archive, files = paths, compression = "gzip")
  archive
}

dina_data_check <- function(root = dina_repo_root()) {
  config <- dina_config(root)
  paths <- c(config$archives$primary_paths %||% character(), config$paths$input_data %||% "input_data")
  unique(data.frame(
    path = paths,
    exists = file.exists(file.path(root, paths)),
    stringsAsFactors = FALSE
  ))
}

dina_doctor <- function(root = dina_repo_root()) {
  config_raw <- dina_read_yaml(dina_config_path(root))
  packages <- config_raw$dependencies$r_packages %||% character()
  installed <- vapply(packages, function(pkg) nzchar(system.file(package = pkg)), logical(1))
  config <- dina_config(root)
  stata <- Sys.getenv("DINA_STATA_CMD", unset = config$stata$command %||% "")
  path_checks <- data.frame(
    path = c("input_data", "output", "previous_series", "config"),
    exists = file.exists(file.path(root, c("input_data", "output", "previous_series", "config"))),
    writable = vapply(file.path(root, c("input_data", "output", "previous_series", "config")), function(path) {
      dir.exists(path) && file.access(path, 2) == 0
    }, logical(1)),
    stringsAsFactors = FALSE
  )
  list(
    root = root,
    packages = data.frame(package = packages, installed = installed, stringsAsFactors = FALSE),
    stata = list(command = stata, configured = nzchar(stata), available = nzchar(stata) && nzchar(Sys.which(strsplit(stata, " ")[[1]][1]))),
    pushover = dina_pushover_status(root),
    paths = path_checks,
    active_update = dina_current_update(root)
  )
}

dina_audit_paths <- function(root = dina_repo_root()) {
  patterns <- c("Dropbox", "setwd\\(", "capture cd", "\"Data/", "'Data/", "~/Dropbox")
  files <- list.files(file.path(root, "code"), recursive = TRUE, full.names = TRUE)
  hits <- list()
  for (file in files) {
    if (!file.exists(file) || dir.exists(file)) next
    lines <- readLines(file, warn = FALSE)
    idx <- unique(unlist(lapply(patterns, function(pattern) grep(pattern, lines, perl = TRUE))))
    if (length(idx)) {
      hits[[dina_relative(file, root)]] <- data.frame(line = idx, text = lines[idx], stringsAsFactors = FALSE)
    }
  }
  hits
}

dina_pushover_credentials <- function(root = dina_repo_root()) {
  config <- dina_config(root)
  pushover <- (config$notifications %||% list())$pushover %||% list()
  token <- Sys.getenv("PUSHOVER_APP_TOKEN", unset = "")
  user <- Sys.getenv("PUSHOVER_USER_KEY", unset = "")
  if (!nzchar(token)) {
    token <- pushover$token %||% ""
  }
  if (!nzchar(user)) {
    user <- pushover$user %||% ""
  }
  list(token = token, user = user)
}

dina_pushover_status <- function(root = dina_repo_root()) {
  config <- dina_config(root)
  pushover <- (config$notifications %||% list())$pushover %||% list()
  local_path <- dina_pushover_local_path(root)
  env_token <- Sys.getenv("PUSHOVER_APP_TOKEN", unset = "")
  env_user <- Sys.getenv("PUSHOVER_USER_KEY", unset = "")
  config_token <- pushover$token %||% ""
  config_user <- pushover$user %||% ""

  local_configured <- file.exists(local_path)
  env_configured <- nzchar(env_token) && nzchar(env_user)
  config_configured <- nzchar(config_token) && nzchar(config_user)
  source <- if (local_configured) {
    "local_file"
  } else if (env_configured) {
    "environment"
  } else if (config_configured) {
    "config"
  } else {
    "none"
  }

  list(
    enabled = isTRUE(pushover$enabled),
    configured = local_configured || env_configured || config_configured,
    source = source,
    local_path = dina_relative(local_path, root),
    local_configured = local_configured,
    env_configured = env_configured,
    config_configured = config_configured,
    token_configured = local_configured || nzchar(dina_pushover_credentials(root)$token),
    user_configured = local_configured || nzchar(dina_pushover_credentials(root)$user)
  )
}

dina_source_pushover_local <- function(root = dina_repo_root()) {
  path <- dina_pushover_local_path(root)
  if (!file.exists(path)) {
    return(FALSE)
  }
  dina_need("pushoverr")
  env <- new.env(parent = baseenv())
  env$set_pushover_user <- pushoverr::set_pushover_user
  env$set_pushover_app <- pushoverr::set_pushover_app
  sys.source(path, envir = env)
  TRUE
}

dina_notify <- function(message, root = dina_repo_root(), title = "DINA-LatAm") {
  dina_need("pushoverr")
  args <- list(message = message, title = title)
  if (file.exists(dina_pushover_local_path(root))) {
    dina_source_pushover_local(root)
  } else {
    credentials <- dina_pushover_credentials(root)
    if (!nzchar(credentials$token) || !nzchar(credentials$user)) {
      stop("Pushover is not configured. Run `dina notify init` or set PUSHOVER_APP_TOKEN and PUSHOVER_USER_KEY.", call. = FALSE)
    }
    args$app <- credentials$token
    args$user <- credentials$user
  }
  do.call(pushoverr::pushover, args)
}

dina_notify_test <- function(root = dina_repo_root()) {
  dina_notify("DINA-LatAm CLI notification test", root = root, title = "DINA-LatAm")
}
