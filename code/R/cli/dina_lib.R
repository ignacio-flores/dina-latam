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

dina_update_roadmap_path <- function(root = dina_repo_root()) {
  dina_path("config", "update_roadmap.yml", root = root)
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

dina_update_roadmap <- function(root = dina_repo_root()) {
  x <- dina_read_yaml(dina_update_roadmap_path(root), default = list(gates = list()))
  if (is.null(x$gates)) {
    x$gates <- list()
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

dina_source_url_entries <- function(source) {
  entries <- list()
  add_entry <- function(label, url) {
    url <- dina_source_values(url)
    if (!length(url)) {
      return(invisible(NULL))
    }
    for (value in url) {
      entries[[length(entries) + 1L]] <<- list(label = label %||% "", url = value)
    }
  }

  direct_label <- if ((source$method %||% "") %in% c("url", "zip")) "direct fetch" else "source"
  add_entry(direct_label, dina_source_field(source, "url"))

  urls <- dina_source_field(source, "urls", list())
  if (is.character(urls)) {
    add_entry("url", urls)
  } else if (is.list(urls) && length(urls)) {
    for (item in urls) {
      if (is.character(item) && is.null(names(item))) {
        add_entry("url", item)
        next
      }
      label <- paste(
        dina_source_values(c(
          dina_source_field(item, "country"),
          dina_source_field(item, "label"),
          dina_source_field(item, "kind")
        )),
        collapse = " "
      )
      add_entry(label, dina_source_field(item, "url", ""))
    }
  }

  if (!length(entries)) {
    return(data.frame(label = character(), url = character(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, lapply(entries, as.data.frame, stringsAsFactors = FALSE))
  out <- out[!duplicated(paste(out$label, out$url, sep = "\r")), , drop = FALSE]
  rownames(out) <- NULL
  out
}

dina_source_urls <- function(source) {
  entries <- dina_source_url_entries(source)
  if (!nrow(entries)) {
    return(character())
  }
  ifelse(nzchar(entries$label), sprintf("%s: %s", entries$label, entries$url), entries$url)
}

dina_source_has_value <- function(x) {
  length(dina_source_values(x)) > 0
}

dina_source_norm <- function(x) {
  gsub("-", "_", tolower(trimws(as.character(x))), fixed = TRUE)
}

dina_source_method_glossary <- function() {
  data.frame(
    method = c("url", "zip", "script", "manual", "wid"),
    description = c(
      "Direct URL that dina sources refresh can fetch into staging.",
      "Direct archive URL that dina sources refresh can fetch into staging.",
      "Custom acquisition script exists; not a simple direct URL fetch.",
      "Human-curated/manual input or URL index.",
      "Currently acquired through Stata/WID calls in pipeline scripts."
    ),
    refresh = c("yes", "yes", "no", "no", "no"),
    stringsAsFactors = FALSE
  )
}

dina_source_method_description <- function(method) {
  glossary <- dina_source_method_glossary()
  match <- glossary[dina_source_norm(glossary$method) == dina_source_norm(method), , drop = FALSE]
  if (!nrow(match)) {
    return("")
  }
  match$description[[1]]
}

dina_source_filter_label <- function(kind, requested, resolved) {
  if (is.null(requested) || !nzchar(requested)) {
    return(NULL)
  }
  resolved <- unique(resolved[nzchar(resolved)])
  if (!length(resolved)) {
    return(requested)
  }
  if (identical(dina_source_norm(requested), dina_source_norm(resolved[[1]])) && length(resolved) == 1L) {
    return(requested)
  }
  sprintf("%s (%s)", requested, paste(resolved, collapse = ", "))
}

dina_source_resolve_family_filter <- function(value, root = dina_repo_root()) {
  if (is.null(value) || !nzchar(value)) {
    return(character())
  }
  normalized <- dina_source_norm(value)
  alias <- switch(
    normalized,
    admin_data = c("admin_tax", "admin_tax_aux"),
    admin = c("admin_tax", "admin_tax_aux"),
    sna = c("macro_sna", "country_sna"),
    normalized
  )
  registry <- dina_sources(root)$sources
  available <- unique(vapply(registry, function(source) dina_source_field(source, "family", ""), character(1)))
  matches <- available[dina_source_norm(available) %in% dina_source_norm(alias)]
  if (!length(matches)) {
    stop("Unknown source family: ", value, "\nAvailable families: ", paste(sort(available), collapse = ", "), call. = FALSE)
  }
  matches
}

dina_source_resolve_method_filter <- function(value) {
  if (is.null(value) || !nzchar(value)) {
    return(character())
  }
  glossary <- dina_source_method_glossary()
  matches <- glossary$method[dina_source_norm(glossary$method) == dina_source_norm(value)]
  if (!length(matches)) {
    stop("Unknown source method: ", value, "\nAvailable methods: ", paste(glossary$method, collapse = ", "), call. = FALSE)
  }
  matches
}

dina_source_url_countries <- function(source) {
  urls <- dina_source_field(source, "urls", list())
  if (!is.list(urls) || !length(urls)) {
    return(character())
  }
  countries <- unlist(lapply(urls, function(item) {
    if (!is.list(item)) {
      return(character())
    }
    dina_source_values(dina_source_field(item, "country"))
  }), use.names = FALSE)
  unique(toupper(trimws(countries[nzchar(countries)])))
}

dina_source_country_values <- function(source, root = dina_repo_root()) {
  values <- unlist(strsplit(dina_source_values(dina_source_field(source, "country", "")), ",", fixed = TRUE), use.names = FALSE)
  values <- unique(toupper(trimws(values[nzchar(values)])))
  if (!length(values)) {
    return(character())
  }
  if ("MULTI" %in% values) {
    from_urls <- dina_source_url_countries(source)
    if (length(from_urls)) {
      return(from_urls)
    }
    return(unique(toupper(dina_config(root)$countries %||% character())))
  }
  values
}

dina_source_country_summary <- function(source, root = dina_repo_root()) {
  values <- dina_source_country_values(source, root)
  if (!length(values)) {
    return("")
  }
  raw <- unlist(strsplit(dina_source_values(dina_source_field(source, "country", "")), ",", fixed = TRUE), use.names = FALSE)
  raw <- unique(toupper(trimws(raw[nzchar(raw)])))
  if ("MULTI" %in% raw) {
    return(sprintf("%s %s", length(values), if (length(values) == 1L) "country" else "countries"))
  }
  if (length(values) <= 3L) {
    return(paste(values, collapse = ","))
  }
  sprintf("%s countries", length(values))
}

dina_source_country_matches <- function(source_country, country) {
  if (is.null(country) || !nzchar(country)) {
    return(TRUE)
  }
  values <- unlist(strsplit(dina_source_values(source_country), ",", fixed = TRUE), use.names = FALSE)
  values <- toupper(trimws(values))
  toupper(country) %in% values || "MULTI" %in% values
}

dina_source_registry <- function(root = dina_repo_root(), family = NULL, country = NULL, method = NULL) {
  registry <- dina_sources(root)$sources
  if (!is.null(family) && length(family) && any(nzchar(family))) {
    registry <- registry[vapply(registry, function(source) dina_source_field(source, "family", "") %in% family, logical(1))]
  }
  if (!is.null(country) && nzchar(country)) {
    registry <- registry[vapply(registry, function(source) dina_source_country_matches(dina_source_field(source, "country", ""), country), logical(1))]
  }
  if (!is.null(method) && length(method) && any(nzchar(method))) {
    registry <- registry[vapply(registry, function(source) dina_source_field(source, "method", "") %in% method, logical(1))]
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

dina_ignored_path <- function(path, root = dina_repo_root()) {
  rel <- dina_relative(path, root)
  base <- basename(rel)
  grepl("(^|/)_new(/|$)", rel) ||
    identical(base, ".DS_Store") ||
    grepl("\\.tmp$", base, ignore.case = TRUE)
}

dina_filter_ignored_paths <- function(paths, root = dina_repo_root()) {
  if (!length(paths)) {
    return(paths)
  }
  paths[!vapply(paths, dina_ignored_path, logical(1), root = root)]
}

dina_filter_inbox_paths <- function(paths, root = dina_repo_root()) {
  if (!length(paths)) {
    return(paths)
  }
  paths[!vapply(paths, function(path) {
    base <- basename(path)
    identical(base, ".DS_Store") || grepl("\\.tmp$", base, ignore.case = TRUE)
  }, logical(1))]
}

dina_dir_needs_ignore_scan <- function(path) {
  if (!dir.exists(path)) {
    return(FALSE)
  }
  direct <- list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (any(vapply(direct, dina_ignored_path, logical(1), root = dirname(path)))) {
    return(TRUE)
  }
  child_dirs <- direct[dir.exists(direct)]
  any(dir.exists(file.path(child_dirs, "_new")))
}

dina_expand_mtime_paths <- function(paths, root = dina_repo_root(), ignore = FALSE, recursive_dirs = FALSE) {
  expanded <- dina_expand_paths(paths, root)
  if (identical(recursive_dirs, FALSE)) {
    return(if (isTRUE(ignore)) dina_filter_ignored_paths(expanded, root) else expanded)
  }
  out <- character()
  for (path in expanded) {
    scan_dir <- dir.exists(path) && (identical(recursive_dirs, TRUE) || (identical(recursive_dirs, "auto") && isTRUE(ignore) && dina_dir_needs_ignore_scan(path)))
    if (scan_dir) {
      listed <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      listed <- listed[file.exists(listed) & !dir.exists(listed)]
      out <- c(out, if (length(listed)) listed else path)
    } else {
      out <- c(out, path)
    }
  }
  out <- unique(normalizePath(out, mustWork = FALSE))
  if (isTRUE(ignore)) {
    out <- dina_filter_ignored_paths(out, root)
  }
  out
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

dina_hash_mode <- function(hash = FALSE) {
  if (is.character(hash) && length(hash)) {
    mode <- match.arg(hash[[1]], c("none", "all", "changed"))
    return(mode)
  }
  if (isTRUE(hash)) "all" else "none"
}

dina_signature_has_hash <- function(signature) {
  hash <- signature$sha256 %||% NA_character_
  is.character(hash) && length(hash) && !is.na(hash[[1]]) && nzchar(hash[[1]])
}

dina_same_cheap_signature <- function(current, previous) {
  if (is.null(current) || is.null(previous)) {
    return(FALSE)
  }
  identical(isTRUE(current$exists), isTRUE(previous$exists)) &&
    identical(as.numeric(current$size %||% NA_real_), as.numeric(previous$size %||% NA_real_)) &&
    identical(as.character(current$mtime %||% NA_character_), as.character(previous$mtime %||% NA_character_))
}

dina_file_signature_map <- function(source_scan) {
  files <- source_scan$files %||% list()
  if (!length(files)) {
    return(list())
  }
  paths <- vapply(files, function(file) file$path %||% "", character(1))
  files <- files[nzchar(paths)]
  names(files) <- paths[nzchar(paths)]
  files
}

dina_source_file_signature <- function(path, root = dina_repo_root(), deep = FALSE, hash = deep, previous = NULL) {
  mode <- dina_hash_mode(hash)
  ext <- tolower(tools::file_ext(path))
  sheets <- if (isTRUE(deep) && ext %in% c("xls", "xlsx", "xlsb")) dina_excel_sheets_safe(path) else character()
  years <- unique(c(dina_years_from_filename(path), dina_years_from_text(sheets)))
  info <- file.info(path)
  signature <- list(
    path = dina_relative(path, root),
    exists = file.exists(path),
    size = if (file.exists(path)) unname(info$size) else NA_real_,
    mtime = if (file.exists(path)) format(info$mtime, "%Y-%m-%dT%H:%M:%OS%z") else NA_character_,
    sha256 = NA_character_,
    hash_status = "not_requested",
    filename_years = as.integer(dina_years_from_filename(path)),
    sheet_years = as.integer(dina_years_from_text(sheets)),
    detected_years = as.integer(sort(unique(years))),
    sheets = sheets
  )
  if (!isTRUE(signature$exists)) {
    signature$hash_status <- "missing"
    return(signature)
  }
  if (identical(mode, "all")) {
    signature$sha256 <- dina_hash_file(path)
    signature$hash_status <- "computed"
  } else if (identical(mode, "changed")) {
    if (!is.null(previous) && dina_same_cheap_signature(signature, previous) && dina_signature_has_hash(previous)) {
      signature$sha256 <- previous$sha256
      signature$hash_status <- "reused"
    } else {
      signature$sha256 <- dina_hash_file(path)
      signature$hash_status <- "computed"
    }
  }
  signature
}

dina_progress <- function(progress = NULL, message, ...) {
  if (is.function(progress)) {
    progress(sprintf(message, ...))
  }
  invisible(NULL)
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

dina_source_staging_root <- function(session, root = dina_repo_root()) {
  file.path(dina_update_dir(session$id, root), "source_staging")
}

dina_source_staging_rel <- function(source, values = list()) {
  dina_template_value(source$staging_name %||% source$id, values = c(list(source = source$id %||% "source"), values))
}

dina_source_method_status <- function(method) {
  switch(
    method,
    manual = "manual_needed",
    script = "script_needed",
    wid = "wid_pipeline",
    "skipped"
  )
}

dina_hash_path <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  if (!dir.exists(path)) {
    return(dina_hash_file(path))
  }
  dina_need("digest")
  files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) {
    return(digest::digest("", algo = "sha256"))
  }
  rel <- substring(files, nchar(normalizePath(path, mustWork = FALSE)) + 2L)
  lines <- sprintf("%s %s", rel, vapply(files, dina_hash_file, character(1)))
  digest::digest(paste(sort(lines), collapse = "\n"), algo = "sha256")
}

dina_path_size <- function(path) {
  if (!file.exists(path)) {
    return(NA_real_)
  }
  if (!dir.exists(path)) {
    return(unname(file.info(path)$size))
  }
  files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) {
    return(0)
  }
  sum(file.info(files)$size, na.rm = TRUE)
}

dina_source_canonical_state <- function(source, root = dina_repo_root()) {
  patterns <- dina_source_values(dina_source_field(source, "canonical"))
  paths <- dina_expand_paths(patterns, root = root)
  exists <- file.exists(paths)
  latest <- "none"
  if (any(exists)) {
    mtimes <- file.info(paths[exists])$mtime
    mtimes <- mtimes[!is.na(mtimes)]
    if (length(mtimes)) {
      latest <- format(max(mtimes), "%Y-%m-%d")
    }
  }
  list(
    patterns = length(patterns),
    found = sum(exists),
    latest = latest
  )
}

dina_sources_refresh <- function(session, root = dina_repo_root(), source_ids = NULL, dry_run = FALSE) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  registry <- dina_sources(root)$sources
  if (!is.null(source_ids)) {
    registry <- registry[vapply(registry, function(x) x$id %in% source_ids, logical(1))]
  }
  staging_root <- dina_source_staging_root(session, root)
  results <- list()
  for (source in registry) {
    method <- source$method %||% "manual"
    url <- dina_source_values(dina_source_field(source, "url"))
    url <- if (length(url)) url[[1]] else ""
    urls <- dina_source_urls(source)
    canonical <- dina_source_canonical_state(source, root)
    target_rel <- dina_source_staging_rel(source)
    target <- file.path(staging_root, target_rel)
    supported <- method %in% c("url", "zip")
    base_result <- list(
      id = source$id,
      family = source$family %||% "",
      country = dina_source_country_summary(source, root),
      method = method,
      url = if (nzchar(url)) url else NULL,
      urls = urls,
      url_count = length(urls),
      target = dina_relative(target, root),
      target_rel = target_rel,
      canonical_found = canonical$found,
      canonical_patterns = canonical$patterns,
      canonical_latest = canonical$latest,
      downloader = dina_source_values(dina_source_field(source, "downloader")),
      transformer = dina_source_values(dina_source_field(source, "transformer"))
    )
    if (!supported || !nzchar(url)) {
      results[[source$id]] <- c(base_result, list(
        status = dina_source_method_status(method)
      ))
      next
    }
    if (file.exists(target)) {
      results[[source$id]] <- c(base_result, list(status = "already_staged"))
      next
    }
    if (dry_run) {
      results[[source$id]] <- c(base_result, list(status = "will_fetch"))
      next
    }
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    ok <- tryCatch({
      utils::download.file(url, target, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      results[[source$id]] <<- c(base_result, list(status = "failed", error = conditionMessage(e)))
      FALSE
    })
    if (ok) {
      results[[source$id]] <- c(base_result, list(status = "staged", sha256 = dina_hash_file(target)))
    }
  }
  attempted <- results[vapply(results, function(result) result$status %in% c("staged", "failed"), logical(1))]
  if (!isTRUE(dry_run) && length(attempted)) {
    session$source_refreshes[[dina_now()]] <- attempted
    session$updated_at <- dina_now()
    dina_save_session(session, root)
  }
  results
}

dina_stage_target <- function(source, input_path, is_dir, session, root = dina_repo_root()) {
  staging_rel <- dina_source_staging_rel(source, values = list(basename = basename(input_path)))
  target <- file.path(dina_source_staging_root(session, root), staging_rel)
  if (!is_dir && !nzchar(tools::file_ext(basename(target)))) {
    target <- file.path(target, basename(input_path))
  }
  target
}

dina_sources_stage_path <- function(session, source_id, input_path, root = dina_repo_root(), overwrite = FALSE) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  source <- dina_source_by_id(source_id, root)
  input_path <- normalizePath(input_path, mustWork = FALSE)
  if (!file.exists(input_path)) {
    stop("Manual source path does not exist: ", input_path, call. = FALSE)
  }
  is_dir <- dir.exists(input_path)
  target <- dina_stage_target(source, input_path, is_dir, session, root)
  if (file.exists(target) && !isTRUE(overwrite)) {
    stop("Staged path already exists; pass --yes to overwrite: ", dina_relative(target, root), call. = FALSE)
  }
  if (file.exists(target) && isTRUE(overwrite)) {
    unlink(target, recursive = TRUE)
  }
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (is_dir) {
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    items <- list.files(input_path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(items)) {
      file.copy(items, target, recursive = TRUE, copy.date = TRUE)
    }
  } else {
    file.copy(input_path, target, overwrite = TRUE, copy.date = TRUE)
  }
  record <- list(
    source_id = source_id,
    method = source$method %||% "manual",
    original = dina_relative(input_path, root),
    staged = dina_relative(target, root),
    staged_rel = dina_relative(target, dina_source_staging_root(session, root)),
    kind = if (is_dir) "dir" else "file",
    size = dina_path_size(target),
    mtime = format(file.info(target)$mtime, "%Y-%m-%dT%H:%M:%OS%z"),
    sha256 = dina_hash_path(target),
    staged_at = dina_now()
  )
  records <- session$source_stage_records %||% list()
  records[[length(records) + 1L]] <- record
  session$source_stage_records <- records
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  record
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
  if (dir.exists(staged)) {
    if (file.exists(dest) && overwrite) {
      unlink(dest, recursive = TRUE)
    }
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    items <- list.files(staged, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(items)) {
      file.copy(items, dest, recursive = TRUE, copy.date = TRUE)
    }
  } else {
    file.copy(staged, dest, overwrite = overwrite, copy.date = TRUE)
  }
  decision <- list(
    source_id = source_id %||% NA_character_,
    staged = dina_relative(staged, root),
    destination = dina_relative(dest, root),
    sha256 = dina_hash_path(dest),
    integrated_at = dina_now()
  )
  session$source_decisions[[length(session$source_decisions) + 1L]] <- decision
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  decision
}

dina_staged_rel_from_root_relative <- function(path, session, root = dina_repo_root()) {
  staging_root_rel <- dina_relative(dina_source_staging_root(session, root), root)
  sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", staging_root_rel), "/?"), "", path)
}

dina_source_destinations <- function(source) {
  unique(c(
    dina_source_values(dina_source_field(source, "destination")),
    dina_source_values(dina_source_field(source, "destinations"))
  ))
}

dina_source_destination_for_staged <- function(source, staged_rel) {
  destinations <- dina_source_destinations(source)
  if (!length(destinations)) {
    return(list(status = "missing_destination", destination = ""))
  }
  if (length(destinations) > 1L) {
    return(list(status = "ambiguous_destination", destination = paste(destinations, collapse = ", ")))
  }
  list(
    status = "ready",
    destination = dina_template_value(
      destinations[[1]],
      values = list(
        source = source$id %||% "",
        basename = basename(staged_rel),
        staged = staged_rel
      )
    )
  )
}

dina_source_record_entries <- function(session, root = dina_repo_root()) {
  entries <- list()
  add_entry <- function(entry) {
    entries[[length(entries) + 1L]] <<- entry
  }
  refreshes <- session$source_refreshes %||% list()
  for (stamp in names(refreshes)) {
    for (result in refreshes[[stamp]]) {
      target <- result$target %||% ""
      status <- result$status %||% ""
      if (!nzchar(target) || !status %in% c("staged", "already_staged")) {
        next
      }
      if (!file.exists(file.path(root, target))) {
        next
      }
      add_entry(list(
        source_id = result$id %||% "",
        method = result$method %||% "",
        staged = target,
        staged_rel = dina_staged_rel_from_root_relative(target, session, root),
        sha256 = result$sha256 %||% dina_hash_path(file.path(root, target)),
        recorded_at = stamp,
        origin = "refresh"
      ))
    }
  }
  for (record in session$source_stage_records %||% list()) {
    add_entry(list(
      source_id = record$source_id %||% "",
      method = record$method %||% "",
      staged = record$staged %||% "",
      staged_rel = record$staged_rel %||% dina_staged_rel_from_root_relative(record$staged %||% "", session, root),
      sha256 = record$sha256 %||% NA_character_,
      recorded_at = record$staged_at %||% "",
      origin = "manual_stage"
    ))
  }
  if (length(entries)) {
    key <- vapply(entries, function(entry) {
      paste(
        entry$source_id %||% "",
        entry$staged_rel %||% entry$staged %||% "",
        sep = "\r"
      )
    }, character(1))
    entries <- entries[!duplicated(key)]
  }
  entries
}

dina_sources_review_rows <- function(session, root = dina_repo_root()) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  staging_root <- dina_source_staging_root(session, root)
  entries <- dina_source_record_entries(session, root)
  recorded_staged <- vapply(entries, function(entry) entry$staged %||% "", character(1))
  recorded_full <- normalizePath(file.path(root, recorded_staged[nzchar(recorded_staged)]), mustWork = FALSE)
  staged_files <- if (dir.exists(staging_root)) {
    list.files(staging_root, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  } else {
    character()
  }
  staged_files <- normalizePath(staged_files, mustWork = FALSE)
  staged_files <- staged_files[file.exists(staged_files) & !dir.exists(staged_files)]
  recorded_dirs <- recorded_full[dir.exists(recorded_full)]
  unrecorded <- staged_files[!vapply(staged_files, function(path) {
    any(path == recorded_full) ||
      any(vapply(recorded_dirs, function(dir) startsWith(path, paste0(dir, "/")), logical(1)))
  }, logical(1))]
  for (path in unrecorded) {
    entries[[length(entries) + 1L]] <- list(
      source_id = "",
      method = "",
      staged = dina_relative(path, root),
      staged_rel = dina_relative(path, staging_root),
      sha256 = dina_hash_path(path),
      recorded_at = "",
      origin = "unrecorded"
    )
  }
  if (!length(entries)) {
    return(data.frame(
      source_id = character(),
      method = character(),
      staged = character(),
      staged_rel = character(),
      destination = character(),
      destination_status = character(),
      sha256 = character(),
      origin = character(),
      action = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(entries, function(entry) {
    source <- if (nzchar(entry$source_id %||% "")) tryCatch(dina_source_by_id(entry$source_id, root), error = function(e) NULL) else NULL
    method <- entry$method %||% ""
    if (!nzchar(method) && !is.null(source)) {
      method <- source$method %||% ""
    }
    destination <- if (is.null(source)) {
      list(status = "unknown_source", destination = "")
    } else {
      dina_source_destination_for_staged(source, entry$staged_rel %||% "")
    }
    action <- switch(
      destination$status,
      ready = "bulk_integrate",
      missing_destination = "integrate_with_to",
      ambiguous_destination = "integrate_with_to",
      unknown_source = "integrate_with_source_and_to",
      "review"
    )
    data.frame(
      source_id = entry$source_id %||% "",
      method = method,
      staged = entry$staged %||% "",
      staged_rel = entry$staged_rel %||% "",
      destination = destination$destination,
      destination_status = destination$status,
      sha256 = entry$sha256 %||% "",
      origin = entry$origin %||% "",
      action = action,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

dina_sources_integrate_bulk <- function(session, root = dina_repo_root(), source_id = NULL, all = FALSE, overwrite = FALSE) {
  rows <- dina_sources_review_rows(session, root)
  if (!isTRUE(all)) {
    source_id <- source_id %||% ""
    if (!nzchar(source_id)) {
      stop("Pass --source ID, --all, or --staged RELPATH --to DESTINATION.", call. = FALSE)
    }
    rows <- rows[rows$source_id == source_id, , drop = FALSE]
  }
  if (!nrow(rows)) {
    return(list())
  }
  out <- list()
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    if (!identical(row$destination_status[[1]], "ready")) {
      out[[length(out) + 1L]] <- list(
        source_id = row$source_id[[1]],
        staged = row$staged_rel[[1]],
        status = "skipped",
        reason = row$destination_status[[1]],
        action = row$action[[1]]
      )
      next
    }
    if (!isTRUE(overwrite)) {
      out[[length(out) + 1L]] <- list(
        source_id = row$source_id[[1]],
        staged = row$staged_rel[[1]],
        destination = row$destination[[1]],
        status = "would_integrate"
      )
      next
    }
    current <- dina_load_session(session$id, root = root)
    decision <- dina_sources_integrate_file(
      current,
      staged_rel = row$staged_rel[[1]],
      dest_rel = row$destination[[1]],
      source_id = row$source_id[[1]],
      root = root,
      overwrite = TRUE
    )
    out[[length(out) + 1L]] <- c(decision, list(status = "integrated"))
  }
  out
}

dina_source_inbox_patterns <- function(source) {
  unique(c(
    dina_source_values(dina_source_field(source, "inbox")),
    dina_source_values(dina_source_field(source, "inboxes"))
  ))
}

dina_source_inbox_validation <- function(source, path, destination = "", root = dina_repo_root()) {
  rel <- dina_relative(path, root)
  base <- basename(path)
  ext <- tolower(tools::file_ext(base))
  issues <- character()
  warnings <- character()

  if (identical(base, ".DS_Store") || grepl("\\.tmp$", base, ignore.case = TRUE)) {
    issues <- c(issues, "hidden_or_temp")
  }
  if (!file.exists(path)) {
    issues <- c(issues, "missing")
  }
  if (!dir.exists(path)) {
    allowed <- dina_source_values(dina_source_field(source, "extensions"))
    if (!length(allowed)) {
      allowed <- switch(
        source$id %||% "",
        "chl-pit-total" = c("xlsb", "xlsx"),
        "bra-admin-tax" = "xlsx",
        c("csv", "dta", "xls", "xlsx", "xlsb", "ods", "zip")
      )
    }
    if (nzchar(ext) && !ext %in% tolower(allowed)) {
      issues <- c(issues, sprintf("unexpected_extension:%s", ext))
    }
  }
  if (nzchar(destination) && file.exists(file.path(root, destination))) {
    warnings <- c(warnings, "destination_exists")
  }

  if (identical(source$id %||% "", "col-admin-income") && dir.exists(path)) {
    xlsx <- list.files(path, pattern = "\\.xlsx$", recursive = TRUE, ignore.case = TRUE)
    dta <- list.files(path, pattern = "\\.dta$", recursive = TRUE, ignore.case = TRUE)
    if (!length(xlsx)) {
      issues <- c(issues, "no_xlsx_files")
    }
    if (length(dta)) {
      warnings <- c(warnings, "generated_dta_present")
    }
  }

  if (identical(source$id %||% "", "chl-pit-total") && !dir.exists(path)) {
    if (!ext %in% c("xlsb", "xlsx")) {
      issues <- c(issues, "expected_xlsb_or_xlsx")
    }
  }

  if (identical(source$id %||% "", "bra-admin-tax") && !dir.exists(path)) {
    if (!grepl("^gn-irpf-ac[0-9]{4}\\.xlsx$", base, ignore.case = TRUE)) {
      warnings <- c(warnings, "unexpected_bra_filename")
    }
    sheets <- dina_excel_sheets_safe(path)
    if (length(sheets) && !"Tab8" %in% sheets) {
      issues <- c(issues, "missing_Tab8")
    }
  }

  status <- if (length(issues)) "failed" else if (length(warnings)) "warning" else "ok"
  detail <- paste(c(issues, warnings), collapse = ",")
  if (!nzchar(detail)) {
    detail <- "ok"
  }
  list(status = status, detail = detail, path = rel)
}

dina_sources_inbox_rows <- function(root = dina_repo_root(), source_id = NULL) {
  registry <- dina_sources(root)$sources
  if (!is.null(source_id) && nzchar(source_id)) {
    registry <- registry[vapply(registry, function(source) identical(source$id %||% "", source_id), logical(1))]
  }
  rows <- list()
  for (source in registry) {
    patterns <- dina_source_inbox_patterns(source)
    if (!length(patterns)) {
      next
    }
    paths <- dina_filter_inbox_paths(dina_expand_paths(patterns, root), root)
    paths <- paths[file.exists(paths)]
    if (!length(paths)) {
      next
    }
    for (path in paths) {
      dest <- dina_source_destination_for_staged(source, basename(path))
      validation <- dina_source_inbox_validation(source, path, destination = dest$destination %||% "", root = root)
      rows[[length(rows) + 1L]] <- data.frame(
        source_id = source$id %||% "",
        method = source$method %||% "",
        inbox = dina_relative(path, root),
        kind = if (dir.exists(path)) "dir" else "file",
        destination = dest$destination %||% "",
        destination_status = dest$status %||% "",
        validation = validation$status,
        validation_detail = validation$detail,
        sha256 = dina_hash_path(path),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(
      source_id = character(),
      method = character(),
      inbox = character(),
      kind = character(),
      destination = character(),
      destination_status = character(),
      validation = character(),
      validation_detail = character(),
      sha256 = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

dina_sources_integrate_incoming <- function(session, root = dina_repo_root(), source_id = NULL, all = FALSE, overwrite = FALSE) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  rows <- dina_sources_inbox_rows(root, source_id = source_id)
  if (!isTRUE(all)) {
    source_id <- source_id %||% ""
    if (!nzchar(source_id)) {
      stop("Pass --source ID or --all with --incoming.", call. = FALSE)
    }
    rows <- rows[rows$source_id == source_id, , drop = FALSE]
  }
  if (!nrow(rows)) {
    return(list())
  }
  out <- list()
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    if (!identical(row$destination_status[[1]], "ready")) {
      out[[length(out) + 1L]] <- list(source_id = row$source_id[[1]], incoming = row$inbox[[1]], status = "skipped", reason = row$destination_status[[1]])
      next
    }
    if (identical(row$validation[[1]], "failed")) {
      out[[length(out) + 1L]] <- list(source_id = row$source_id[[1]], incoming = row$inbox[[1]], status = "skipped", reason = row$validation_detail[[1]])
      next
    }
    if (!isTRUE(overwrite)) {
      out[[length(out) + 1L]] <- list(source_id = row$source_id[[1]], incoming = row$inbox[[1]], destination = row$destination[[1]], validation = row$validation[[1]], status = "would_integrate")
      next
    }
    incoming <- file.path(root, row$inbox[[1]])
    destination <- file.path(root, row$destination[[1]])
    if (file.exists(destination)) {
      unlink(destination, recursive = TRUE)
    }
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(incoming)) {
      dir.create(destination, recursive = TRUE, showWarnings = FALSE)
      items <- list.files(incoming, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      if (length(items)) {
        file.copy(items, destination, recursive = TRUE, copy.date = TRUE)
      }
    } else {
      file.copy(incoming, destination, overwrite = TRUE, copy.date = TRUE)
    }
    decision <- list(
      source_id = row$source_id[[1]],
      incoming = row$inbox[[1]],
      destination = row$destination[[1]],
      sha256 = dina_hash_path(destination),
      validation = row$validation[[1]],
      integrated_at = dina_now(),
      origin = "incoming"
    )
    current <- dina_load_session(session$id, root = root)
    current$source_decisions[[length(current$source_decisions) + 1L]] <- decision
    current$updated_at <- dina_now()
    dina_save_session(current, root)
    out[[length(out) + 1L]] <- c(decision, list(status = "integrated"))
  }
  out
}

dina_scan_sources <- function(root = dina_repo_root(), include_missing = TRUE, deep = FALSE, hash = deep, previous = NULL, progress = NULL) {
  hash_mode <- dina_hash_mode(hash)
  registry <- dina_sources(root)$sources
  dina_progress(progress, "Scanning source registry: %s source entries, hash mode %s.", length(registry), hash_mode)
  out <- list()
  for (i in seq_along(registry)) {
    source <- registry[[i]]
    patterns <- source$canonical %||% character()
    files <- dina_filter_ignored_paths(dina_expand_paths(patterns, root = root), root)
    exists <- file.exists(files)
    if (!include_missing) {
      files <- files[exists]
    }
    previous_source <- previous[[source$id]] %||% list()
    previous_files <- dina_file_signature_map(previous_source)
    signatures <- lapply(files, function(path) {
      rel <- dina_relative(path, root)
      dina_source_file_signature(
        path,
        root = root,
        deep = deep,
        hash = hash_mode,
        previous = previous_files[[rel]] %||% NULL
      )
    })
    if (is.function(progress) && length(registry) > 5L && (i == length(registry) || i %% 10L == 0L)) {
      dina_progress(progress, "  scanned %s/%s source entries...", i, length(registry))
    }
    years <- sort(unique(unlist(lapply(signatures, function(x) x$detected_years))))
    out[[source$id]] <- list(
      id = source$id,
      family = source$family %||% NA_character_,
      country = dina_source_country_summary(source, root),
      country_coverage = dina_source_country_values(source, root),
      method = source$method %||% NA_character_,
      hash_mode = hash_mode,
      files = signatures,
      detected_years = as.integer(years)
    )
  }
  out
}

dina_source_scan_summary <- function(scan) {
  files <- unlist(lapply(scan, function(source) source$files %||% list()), recursive = FALSE)
  existing <- if (length(files)) vapply(files, function(file) isTRUE(file$exists), logical(1)) else logical()
  hash_modes <- unique(vapply(scan, function(source) source$hash_mode %||% "none", character(1)))
  list(
    sources = length(scan),
    files_found = sum(existing),
    missing_files = sum(!existing),
    hash_mode = paste(hash_modes, collapse = ", ")
  )
}

dina_empty_source_status_counts <- function() {
  stats::setNames(
    rep(0L, 7L),
    c(
      "unchanged",
      "content_changed",
      "timestamp_only",
      "metadata_changed_unverified",
      "new",
      "missing",
      "unverified"
    )
  )
}

dina_signature_mtime <- function(signature) {
  value <- signature$mtime %||% NA_character_
  if (!is.character(value) || !length(value) || is.na(value[[1]]) || !nzchar(value[[1]])) {
    return(as.POSIXct(NA))
  }
  parsed <- as.POSIXct(value[[1]], format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
  if (is.na(parsed)) {
    parsed <- as.POSIXct(value[[1]], tz = "UTC")
  }
  parsed
}

dina_mtime_direction <- function(current, previous) {
  cur <- dina_signature_mtime(current)
  prev <- dina_signature_mtime(previous)
  if (is.na(cur) || is.na(prev)) {
    return("unknown")
  }
  if (cur > prev) {
    return("newer_than_baseline")
  }
  if (cur < prev) {
    return("older_than_baseline")
  }
  "same"
}

dina_compare_source_file <- function(current, previous = NULL) {
  path <- current$path %||% previous$path %||% ""
  current_exists <- isTRUE(current$exists)
  previous_exists <- !is.null(previous) && isTRUE(previous$exists)
  status <- "unverified"
  if (is.null(previous)) {
    status <- if (current_exists) "new" else "unverified"
  } else if (!current_exists && previous_exists) {
    status <- "missing"
  } else if (current_exists && !previous_exists) {
    status <- "new"
  } else if (!current_exists && !previous_exists) {
    status <- "unchanged"
  } else {
    current_hashable <- dina_signature_has_hash(current)
    previous_hashable <- dina_signature_has_hash(previous)
    cheap_same <- dina_same_cheap_signature(current, previous)
    if (current_hashable && previous_hashable) {
      if (!identical(current$sha256, previous$sha256)) {
        status <- "content_changed"
      } else if (!cheap_same) {
        status <- "timestamp_only"
      } else {
        status <- "unchanged"
      }
    } else if (cheap_same) {
      status <- "unchanged"
    } else {
      status <- "metadata_changed_unverified"
    }
  }
  list(
    path = path,
    status = status,
    mtime = dina_mtime_direction(current, previous %||% list()),
    current_hash_status = current$hash_status %||% NA_character_,
    previous_hash_status = previous$hash_status %||% NA_character_,
    current_sha256 = current$sha256 %||% NA_character_,
    previous_sha256 = previous$sha256 %||% NA_character_
  )
}

dina_classify_source_changes <- function(current, previous = list()) {
  out <- list()
  for (id in names(current)) {
    cur <- current[[id]]
    prev <- previous[[id]] %||% list(files = list(), detected_years = integer())
    cur_years <- cur$detected_years %||% integer()
    prev_years <- prev$detected_years %||% integer()

    cur_files <- dina_file_signature_map(cur)
    prev_files <- dina_file_signature_map(prev)
    paths <- unique(c(names(cur_files), names(prev_files)))
    comparisons <- lapply(paths, function(path) {
      dina_compare_source_file(cur_files[[path]] %||% list(path = path, exists = FALSE), prev_files[[path]] %||% NULL)
    })
    counts <- dina_empty_source_status_counts()
    if (length(comparisons)) {
      statuses <- vapply(comparisons, function(x) x$status, character(1))
      tab <- table(statuses)
      counts[names(tab)] <- as.integer(tab)
    }
    changed <- names(counts)[counts > 0L & names(counts) != "unchanged"]
    classes <- if (length(changed)) changed else "unchanged"
    new_years <- setdiff(cur_years, prev_years)
    removed_years <- setdiff(prev_years, cur_years)
    changed_files <- vapply(
      comparisons[vapply(comparisons, function(x) !identical(x$status, "unchanged"), logical(1))],
      function(x) x$path,
      character(1)
    )
    mtime_statuses <- vapply(comparisons, function(x) x$mtime, character(1))
    mtime_counts <- table(factor(
      mtime_statuses,
      levels = c("newer_than_baseline", "older_than_baseline", "same", "unknown")
    ))
    out[[id]] <- list(
      id = id,
      classes = classes,
      counts = counts,
      mtime_counts = as.integer(mtime_counts),
      current_years = as.integer(cur_years),
      previous_years = as.integer(prev_years),
      new_years = as.integer(new_years),
      removed_years = as.integer(removed_years),
      files = comparisons,
      changed_files = unname(changed_files)
    )
    names(out[[id]]$mtime_counts) <- names(mtime_counts)
  }
  out
}

dina_active_update_file <- function(root = dina_repo_root()) {
  dina_path("output", "updates", ".active_update", root = root)
}

dina_updates_root <- function(root = dina_repo_root()) {
  dirname(dina_active_update_file(root))
}

dina_update_dir <- function(update_id, root = dina_repo_root()) {
  file.path(dina_updates_root(root), update_id)
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

dina_update_year_from_id <- function(update_id) {
  match <- regmatches(update_id, regexpr("^[0-9]{4}", update_id, perl = TRUE))
  if (length(match) && nzchar(match[[1]])) {
    return(match[[1]])
  }
  format(Sys.Date(), "%Y")
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

dina_update_default_id <- function(year = format(Sys.Date(), "%Y")) {
  sprintf("%s-update-%s", year, format(Sys.Date(), "%m-%d"))
}

dina_next_update_id <- function(year = format(Sys.Date(), "%Y"), root = dina_repo_root()) {
  base_id <- dina_update_default_id(year)
  id <- base_id
  suffix <- 1L
  while (dir.exists(dina_update_dir(id, root))) {
    suffix <- suffix + 1L
    id <- sprintf("%s-%02d", base_id, suffix)
  }
  id
}

dina_update_start_plan <- function(year = format(Sys.Date(), "%Y"), root = dina_repo_root()) {
  default_id <- dina_update_default_id(year)
  existing <- dina_load_session(default_id, root)
  exists <- dir.exists(dina_update_dir(default_id, root))
  manifest_exists <- file.exists(dina_session_manifest_path(default_id, root))
  incomplete <- exists && !manifest_exists
  finalized <- !is.null(existing) && identical(existing$status, "finalized")
  list(
    year = as.character(year),
    default_id = default_id,
    id = if (exists) dina_next_update_id(year, root) else default_id,
    exists = exists,
    manifest_exists = manifest_exists,
    incomplete = incomplete,
    existing_status = if (isTRUE(incomplete)) "missing_manifest" else if (!is.null(existing)) existing$status %||% "" else "",
    requires_confirmation = exists && !finalized,
    finalized = exists && finalized
  )
}

dina_count_files <- function(path) {
  if (!dir.exists(path)) {
    return(0L)
  }
  files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  files <- files[file.exists(files)]
  sum(!file.info(files)$isdir, na.rm = TRUE)
}

dina_update_start <- function(year = format(Sys.Date(), "%Y"), id = NULL, root = dina_repo_root(), source_hash = TRUE, progress = NULL) {
  if (is.null(id) || !nzchar(id)) {
    id <- dina_next_update_id(year, root)
  }
  dina_progress(progress, "Preparing update session %s for year %s.", id, year)
  dir <- dina_update_dir(id, root)
  dina_progress(progress, "Creating session scaffold directories.")
  dir.create(file.path(dir, "source_staging"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "logs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "snapshots"), recursive = TRUE, showWarnings = FALSE)

  config <- dina_config(root)
  config_do <- file.path(dir, "config.do")
  dina_progress(progress, "Rendering session config.")
  dina_render_config_do(config, config_do)
  started_at <- dina_now()
  source_hash_mode <- if (isTRUE(source_hash)) "all" else "none"
  if (identical(source_hash_mode, "all")) {
    dina_progress(progress, "Scanning source registry and hashing source baseline.")
  } else {
    dina_progress(progress, "Scanning source registry without source hashes.")
  }
  source_scan <- dina_scan_sources(root, hash = source_hash_mode, progress = progress)
  summary <- dina_source_scan_summary(source_scan)
  dina_progress(
    progress,
    "Source baseline summary: %s sources, %s files found, %s missing files, hash mode %s.",
    summary$sources,
    summary$files_found,
    summary$missing_files,
    summary$hash_mode
  )
  session <- list(
    id = id,
    year = as.character(year),
    status = "initialized",
    created_at = started_at,
    updated_at = started_at,
    config_file = dina_relative(config_do, root),
    config_hash = dina_hash_file(config_do),
    source_baseline = list(
      created_at = started_at,
      hash_mode = source_hash_mode
    ),
    source_scan = source_scan,
    source_refreshes = list(),
    source_stage_records = list(),
    source_decisions = list(),
    task_runs = list(),
    gate_records = list()
  )
  dina_progress(progress, "Writing manifest and active update pointer.")
  dina_save_session(session, root)
  dir.create(dirname(dina_active_update_file(root)), recursive = TRUE, showWarnings = FALSE)
  writeLines(id, dina_active_update_file(root))
  session
}

dina_update_list <- function(root = dina_repo_root()) {
  updates_root <- dina_updates_root(root)
  active <- dina_current_update(root)
  if (!dir.exists(updates_root)) {
    return(data.frame(
      active = character(),
      id = character(),
      year = character(),
      status = character(),
      created_at = character(),
      updated_at = character(),
      stringsAsFactors = FALSE
    ))
  }
  ids <- list.files(updates_root, all.files = FALSE, no.. = TRUE, full.names = FALSE)
  ids <- ids[dir.exists(file.path(updates_root, ids))]
  rows <- lapply(sort(ids), function(id) {
    session <- dina_load_session(id, root)
    missing_manifest <- is.null(session)
    data.frame(
      active = if (identical(id, active)) "*" else "",
      id = id,
      year = if (missing_manifest) dina_update_year_from_id(id) else as.character(session$year %||% ""),
      status = if (missing_manifest) "missing_manifest" else as.character(session$status %||% ""),
      created_at = if (missing_manifest) "" else as.character(session$created_at %||% ""),
      updated_at = if (missing_manifest) "" else as.character(session$updated_at %||% ""),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      active = character(),
      id = character(),
      year = character(),
      status = character(),
      created_at = character(),
      updated_at = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

dina_update_delete <- function(update_id = NULL, root = dina_repo_root(), yes = FALSE) {
  active <- dina_current_update(root)
  if (is.null(update_id) || !nzchar(update_id)) {
    update_id <- active
  }
  if (is.null(update_id) || !nzchar(update_id)) {
    stop("No active update to delete. Pass an update id.", call. = FALSE)
  }
  dir <- dina_update_dir(update_id, root)
  if (!dir.exists(dir)) {
    stop("Unknown update id: ", update_id, call. = FALSE)
  }
  result <- list(
    id = update_id,
    dir = dina_relative(dir, root),
    active = identical(update_id, active),
    deleted = FALSE,
    dry_run = !isTRUE(yes)
  )
  if (!isTRUE(yes)) {
    return(result)
  }
  unlink(dir, recursive = TRUE)
  if (identical(update_id, active)) {
    unlink(dina_active_update_file(root))
  }
  result$deleted <- TRUE
  result$dry_run <- FALSE
  result
}

dina_update_restart <- function(update_id = NULL, root = dina_repo_root(), yes = FALSE, progress = NULL) {
  active <- dina_current_update(root)
  if (is.null(update_id) || !nzchar(update_id)) {
    update_id <- active
  }
  if (is.null(update_id) || !nzchar(update_id)) {
    stop("No active update to restart. Pass an update id.", call. = FALSE)
  }
  session <- dina_load_session(update_id, root = root)
  dir <- dina_update_dir(update_id, root)
  if (is.null(session) && !dir.exists(dir)) {
    stop("Unknown update id: ", update_id, call. = FALSE)
  }
  result <- list(
    id = update_id,
    dir = dina_relative(dir, root),
    active = identical(update_id, active),
    year = if (is.null(session)) dina_update_year_from_id(update_id) else as.character(session$year %||% format(Sys.Date(), "%Y")),
    current_status = if (is.null(session)) "missing_manifest" else as.character(session$status %||% ""),
    staged_files = dina_count_files(file.path(dir, "source_staging")),
    log_files = dina_count_files(file.path(dir, "logs")),
    snapshot_files = dina_count_files(file.path(dir, "snapshots")),
    same_id = TRUE,
    dry_run = !isTRUE(yes),
    restarted = FALSE
  )
  if (!isTRUE(yes)) {
    return(result)
  }
  dina_progress(progress, "Resetting session directory %s.", result$dir)
  unlink(dir, recursive = TRUE)
  new_session <- dina_update_start(year = result$year, id = update_id, root = root, progress = progress)
  result$dry_run <- FALSE
  result$restarted <- TRUE
  result$new_session <- new_session
  result
}

dina_confirm_response <- function(answer) {
  tolower(trimws(answer %||% "")) %in% c("y", "yes")
}

dina_read_prompt <- function(prompt, input = "stdin") {
  cat(prompt)
  flush.console()
  try(flush(stdout()), silent = TRUE)
  answer <- readLines(input, n = 1L, warn = FALSE)
  if (!length(answer)) {
    return("")
  }
  answer[[1]]
}

dina_confirm_continue <- function(prompt = "Continue? [y/N] ", input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(FALSE)
  }
  dina_confirm_response(dina_read_prompt(prompt, input = input))
}

dina_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
}

dina_latest_source_refresh_time <- function(session) {
  refreshes <- session$source_refreshes %||% list()
  if (!length(refreshes)) {
    return(NA_character_)
  }
  names(refreshes)[[length(refreshes)]]
}

dina_latest_source_decision_time <- function(session) {
  decisions <- session$source_decisions %||% list()
  if (!length(decisions)) {
    return(NA_character_)
  }
  times <- vapply(decisions, function(x) x$integrated_at %||% NA_character_, character(1))
  times <- times[!is.na(times) & nzchar(times)]
  if (!length(times)) {
    return(NA_character_)
  }
  sort(times)[[length(times)]]
}

dina_gate_record_key <- function(gate_id, check_id) {
  sprintf("%s/%s", gate_id, check_id)
}

dina_find_gate <- function(gate_id, root = dina_repo_root()) {
  gates <- dina_update_roadmap(root)$gates
  matches <- gates[vapply(gates, function(gate) identical(gate$id %||% "", gate_id), logical(1))]
  if (!length(matches)) {
    available <- vapply(gates, function(gate) gate$id %||% "", character(1))
    stop("Unknown update gate: ", gate_id, "\nAvailable gates: ", paste(available, collapse = ", "), call. = FALSE)
  }
  matches[[1]]
}

dina_gate_check_ids <- function(gate) {
  vapply(gate$checks %||% list(), function(check) check$id %||% "", character(1))
}

dina_parse_gate_check <- function(value, root = dina_repo_root()) {
  value <- trimws(value %||% "")
  parts <- strsplit(value, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L || !nzchar(parts[[1]]) || !nzchar(parts[[2]])) {
    stop("Expected GATE/CHECK, for example tax-admin/raw-accepted.", call. = FALSE)
  }
  gate <- dina_find_gate(parts[[1]], root)
  checks <- dina_gate_check_ids(gate)
  if (!parts[[2]] %in% checks) {
    stop("Unknown check for gate ", parts[[1]], ": ", parts[[2]], "\nAvailable checks: ", paste(checks, collapse = ", "), call. = FALSE)
  }
  list(gate = parts[[1]], check = parts[[2]])
}

dina_gate_records <- function(session) {
  session$gate_records %||% list()
}

dina_gate_check_record <- function(session, gate_id, check_id) {
  records <- dina_gate_records(session)
  records[[dina_gate_record_key(gate_id, check_id)]] %||% NULL
}

dina_gate_check_status <- function(session, gate_id, check_id) {
  record <- dina_gate_check_record(session, gate_id, check_id)
  record$status %||% "pending"
}

dina_gate_status <- function(gate, session = NULL) {
  checks <- dina_gate_check_ids(gate)
  if (!length(checks)) {
    return("done")
  }
  statuses <- vapply(checks, function(check) dina_gate_check_status(session, gate$id %||% "", check), character(1))
  if (any(statuses == "needs-code")) {
    return("needs-code")
  }
  if (all(statuses %in% c("done", "deferred"))) {
    if (any(statuses == "deferred")) {
      return("deferred")
    }
    return("done")
  }
  if (any(statuses != "pending")) {
    return("in-progress")
  }
  "pending"
}

dina_gate_next_check <- function(gate, session = NULL) {
  checks <- gate$checks %||% list()
  for (check in checks) {
    status <- dina_gate_check_status(session, gate$id %||% "", check$id %||% "")
    if (!status %in% c("done", "deferred")) {
      return(check)
    }
  }
  NULL
}

dina_roadmap_status <- function(session = NULL, root = dina_repo_root()) {
  gates <- dina_update_roadmap(root)$gates
  lapply(gates, function(gate) {
    status <- dina_gate_status(gate, session)
    next_check <- dina_gate_next_check(gate, session)
    list(
      id = gate$id %||% "",
      label = gate$label %||% gate$id %||% "",
      status = status,
      next_check = next_check$id %||% "",
      next_check_label = next_check$label %||% "",
      gate = gate
    )
  })
}

dina_next_gate_status <- function(session = NULL, root = dina_repo_root()) {
  statuses <- dina_roadmap_status(session, root)
  for (status in statuses) {
    if (!status$status %in% c("done", "deferred")) {
      return(status)
    }
  }
  NULL
}

dina_gate_record_statuses <- function() {
  c("done", "deferred", "needs-code")
}

dina_update_mark_gate <- function(session, root = dina_repo_root(), target, status, note = "") {
  if (is.null(session)) {
    stop("No active update.", call. = FALSE)
  }
  parsed <- dina_parse_gate_check(target, root)
  status <- tolower(trimws(status %||% ""))
  if (!status %in% dina_gate_record_statuses()) {
    stop("Unknown gate status: ", status, ". Use one of: ", paste(dina_gate_record_statuses(), collapse = ", "), call. = FALSE)
  }
  note <- trimws(note %||% "")
  if (status %in% c("deferred", "needs-code") && !nzchar(note)) {
    stop("Status ", status, " requires --note so the reason is explicit.", call. = FALSE)
  }
  key <- dina_gate_record_key(parsed$gate, parsed$check)
  records <- dina_gate_records(session)
  records[[key]] <- list(
    gate = parsed$gate,
    check = parsed$check,
    status = status,
    note = note,
    marked_at = dina_now()
  )
  session$gate_records <- records
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  records[[key]]
}

dina_update_unmark_gate <- function(session, root = dina_repo_root(), target) {
  if (is.null(session)) {
    stop("No active update.", call. = FALSE)
  }
  parsed <- dina_parse_gate_check(target, root)
  key <- dina_gate_record_key(parsed$gate, parsed$check)
  records <- dina_gate_records(session)
  removed <- !is.null(records[[key]])
  records[[key]] <- NULL
  session$gate_records <- records
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  list(gate = parsed$gate, check = parsed$check, removed = removed)
}

dina_source_diff_counts <- function(diff) {
  counts <- dina_empty_source_status_counts()
  if (!length(diff)) {
    return(counts)
  }
  for (item in diff) {
    item_counts <- item$counts %||% dina_empty_source_status_counts()
    counts[names(item_counts)] <- counts[names(item_counts)] + as.integer(item_counts)
  }
  counts
}

dina_sources_status <- function(session, root = dina_repo_root(), hash = "changed", deep = FALSE) {
  if (is.null(session)) {
    stop("No active update.", call. = FALSE)
  }
  baseline <- session$source_scan %||% list()
  current <- dina_scan_sources(root, deep = deep, hash = hash, previous = baseline)
  diff <- dina_classify_source_changes(current, baseline)
  list(
    baseline_at = session$source_baseline$created_at %||% session$created_at %||% NA_character_,
    baseline_hash_mode = session$source_baseline$hash_mode %||% "none",
    scan_hash_mode = dina_hash_mode(hash),
    scanned_at = dina_now(),
    last_recorded_scan_at = session$latest_source_scan_at %||% NA_character_,
    last_refresh_at = dina_latest_source_refresh_time(session),
    last_integration_at = dina_latest_source_decision_time(session),
    review = NULL,
    counts = dina_source_diff_counts(diff),
    diff = diff
  )
}

dina_session_state <- function(session, root = dina_repo_root()) {
  if (is.null(session)) {
    return(list(state = "no_active_update", recommendation = "Start an update with `dina update start YEAR`."))
  }
  failed <- names(session$task_runs)[vapply(session$task_runs, function(x) identical(x$status, "failed"), logical(1))]
  staged <- list.files(file.path(dina_update_dir(session$id, root), "source_staging"), recursive = TRUE, all.files = FALSE)

  if (length(failed)) {
    return(list(state = "failed", recommendation = sprintf("Inspect logs and retry `%s`.", failed[[length(failed)]]), stale_tasks = NA_integer_))
  }
  if (length(staged)) {
    return(list(state = "sources_pending", recommendation = "Review staged downloads with `dina sources review`.", stale_tasks = NA_integer_))
  }
  next_gate <- dina_next_gate_status(session, root)
  if (!is.null(next_gate)) {
    return(list(
      state = sprintf("gate_%s", next_gate$status),
      recommendation = sprintf("Review `%s` with `dina update gate %s`.", next_gate$label, next_gate$id),
      stale_tasks = NA_integer_,
      next_gate = next_gate$id,
      next_check = next_gate$next_check
    ))
  }
  task_status <- dina_all_task_status(root = root, session = session)
  stale <- sum(vapply(task_status, function(x) x$status %in% c("missing_outputs", "stale", "upstream_stale", "missing_inputs"), logical(1)))
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

dina_task_active <- function(task) {
  !identical(task$active, FALSE) && !isTRUE(task$inactive)
}

dina_task_short_id <- function(id) {
  match <- regmatches(id, regexpr("^[0-9]{2}[A-Za-z]", id, perl = TRUE))
  if (!length(match) || identical(match, "")) id else match
}

dina_task_language <- function(task) {
  type <- tolower(task$type %||% "")
  if (type %in% c("stata", "r", "python", "shell", "bash")) {
    return(c(stata = "Stata", r = "R", python = "Python", shell = "Shell", bash = "Shell")[[type]])
  }
  ext <- tolower(tools::file_ext(task$script %||% ""))
  if (ext %in% c("do", "ado")) {
    return("Stata")
  }
  if (ext %in% c("r", "rscript")) {
    return("R")
  }
  if (ext %in% c("py")) {
    return("Python")
  }
  if (ext %in% c("sh", "bash")) {
    return("Shell")
  }
  if (nzchar(type)) {
    return(type)
  }
  "other"
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
  active <- vapply(tasks, dina_task_active, logical(1))
  explicit_inactive <- character()
  keep <- rep(TRUE, length(tasks))
  if (!is.null(stage)) {
    keep <- keep & vapply(tasks, function(x) identical(x$stage, stage), logical(1))
  }
  if (!is.null(task)) {
    selectors <- dina_split_task_selectors(task)
    selected <- dina_resolve_task_selectors(task, ids, mode = "all")
    short_ids <- vapply(ids, dina_task_short_id, character(1))
    for (selector in selectors) {
      selector_lower <- tolower(selector)
      direct <- ids[tolower(ids) == selector_lower | tolower(short_ids) == selector_lower]
      if (length(direct) == 1L && !active[[match(direct, ids)]]) {
        explicit_inactive <- c(explicit_inactive, direct)
      }
    }
    keep <- keep & ids %in% selected
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
  keep <- keep & (active | ids %in% explicit_inactive)
  tasks[keep]
}

dina_path_mtime <- function(path) {
  if (!file.exists(path)) {
    return(as.POSIXct(NA))
  }
  file.info(path)$mtime
}

dina_latest_mtime <- function(paths, root = dina_repo_root(), ignore = FALSE, recursive_dirs = FALSE) {
  files <- dina_expand_mtime_paths(paths, root, ignore = ignore, recursive_dirs = recursive_dirs)
  files <- files[file.exists(files)]
  if (!length(files)) {
    return(as.POSIXct(NA))
  }
  max(file.info(files)$mtime, na.rm = TRUE)
}

dina_earliest_mtime <- function(paths, root = dina_repo_root(), ignore = FALSE, recursive_dirs = FALSE) {
  files <- dina_expand_mtime_paths(paths, root, ignore = ignore, recursive_dirs = recursive_dirs)
  files <- files[file.exists(files)]
  if (!length(files)) {
    return(as.POSIXct(NA))
  }
  min(file.info(files)$mtime, na.rm = TRUE)
}

dina_task_status <- function(task, root = dina_repo_root(), session = NULL, seen = character()) {
  if (!dina_task_active(task)) {
    return(list(
      id = task$id,
      stage = task$stage %||% NA_character_,
      language = dina_task_language(task),
      status = "inactive",
      reasons = task$notes %||% "Task is inactive by default; select it explicitly when this heavy/static input needs to be rebuilt."
    ))
  }

  inputs <- unique(c(task$inputs %||% character(), task$script %||% character(), "_config.do", "config/dina.yml"))
  outputs <- task$outputs %||% character()

  input_files <- dina_filter_ignored_paths(dina_expand_paths(inputs, root), root)
  output_files <- dina_filter_ignored_paths(dina_expand_paths(outputs, root), root)
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

  latest_input <- dina_latest_mtime(inputs, root, ignore = TRUE, recursive_dirs = "auto")
  earliest_output <- dina_earliest_mtime(outputs, root, ignore = TRUE, recursive_dirs = FALSE)
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
  list(id = task$id, stage = task$stage %||% NA_character_, language = dina_task_language(task), status = status, reasons = reasons)
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
  active_tasks <- tasks[vapply(tasks, dina_task_active, logical(1))]
  lines <- c(
    "# Generated by dina. The YAML task graph remains authoritative.",
    ".PHONY: all",
    sprintf("all: %s", paste(vapply(active_tasks, function(x) x$id, character(1)), collapse = " ")),
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

dina_command_program <- function(command) {
  command <- trimws(command %||% "")
  if (!nzchar(command)) {
    return("")
  }
  parts <- strsplit(command, "[[:space:]]+")[[1]]
  gsub("^[\"']|[\"']$", "", parts[[1]])
}

dina_command_available <- function(command) {
  program <- dina_command_program(command)
  if (!nzchar(program)) {
    return(FALSE)
  }
  if (grepl("/", program, fixed = TRUE)) {
    return(file.exists(program) && file.access(program, 1) == 0)
  }
  nzchar(Sys.which(program))
}

dina_stata_path_names <- function() {
  c("stata-mp", "stata-se", "stata", "StataMP", "StataSE", "Stata")
}

dina_stata_app_dirs <- function() {
  override <- Sys.getenv("DINA_STATA_APP_DIRS", unset = "")
  if (nzchar(override)) {
    return(strsplit(override, .Platform$path.sep, fixed = TRUE)[[1]])
  }
  c("/Applications/Stata", "/Applications")
}

dina_discover_stata <- function(path_names = dina_stata_path_names(), app_dirs = dina_stata_app_dirs()) {
  for (name in path_names) {
    found <- Sys.which(name)
    if (nzchar(found)) {
      return(list(command = unname(found), source = "PATH"))
    }
  }

  executables <- c("stata-mp", "stata-se", "stata", "StataMP", "StataSE", "Stata")
  for (dir in app_dirs) {
    if (!dir.exists(dir)) {
      next
    }
    apps <- if (grepl("\\.app$", dir)) {
      dir
    } else {
      list.files(dir, pattern = "^Stata.*\\.app$", full.names = TRUE)
    }
    for (app in apps) {
      for (exe in executables) {
        candidate <- file.path(app, "Contents", "MacOS", exe)
        if (file.exists(candidate) && file.access(candidate, 1) == 0) {
          return(list(command = candidate, source = "macOS app bundle"))
        }
      }
    }
  }

  list(command = "", source = "none")
}

dina_stata_status <- function(root = dina_repo_root(), path_names = dina_stata_path_names(), app_dirs = dina_stata_app_dirs()) {
  config <- dina_config(root)
  env_command <- Sys.getenv("DINA_STATA_CMD", unset = "")
  config_command <- config$stata$command %||% ""
  command <- if (nzchar(env_command)) env_command else config_command
  source <- if (nzchar(env_command)) {
    "environment"
  } else if (nzchar(config_command)) {
    "config"
  } else {
    "none"
  }
  configured <- nzchar(command)
  available <- configured && dina_command_available(command)
  discovered <- if (available) {
    list(command = "", source = "none")
  } else {
    dina_discover_stata(path_names = path_names, app_dirs = app_dirs)
  }
  discovered_command <- discovered$command %||% ""
  list(
    command = command,
    source = source,
    configured = configured,
    available = available,
    discovered_command = discovered_command,
    discovered_source = discovered$source %||% "none",
    discovered = nzchar(discovered_command),
    suggestion = if (nzchar(discovered_command)) sprintf("export DINA_STATA_CMD=\"%s\"", discovered_command) else ""
  )
}

dina_doctor <- function(root = dina_repo_root(), stata_path_names = dina_stata_path_names(), stata_app_dirs = dina_stata_app_dirs()) {
  config_raw <- dina_read_yaml(dina_config_path(root))
  packages <- config_raw$dependencies$r_packages %||% character()
  installed <- vapply(packages, function(pkg) nzchar(system.file(package = pkg)), logical(1))
  stata <- dina_stata_status(root, path_names = stata_path_names, app_dirs = stata_app_dirs)
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
    stata = stata,
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
