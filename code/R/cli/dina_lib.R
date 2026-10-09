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

dina_records_from_df <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(list())
  }
  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    lapply(row, function(value) unname(value[[1]]))
  })
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

dina_todo_path <- function(root = dina_repo_root()) {
  dina_path("config", "todo.yml", root = root)
}

# Trust regions are Admin source-review decisions. Keeping their registry
# separate from dina.yml lets Admin Include accept them with the reviewed
# source files without turning an update override into a second source of
# truth.
dina_admin_trust_regions_path <- function(root = dina_repo_root()) {
  dina_path("config", "admin_trust_regions.yml", root = root)
}

dina_admin_trust_regions <- function(root = dina_repo_root(), path = dina_admin_trust_regions_path(root)) {
  x <- dina_read_yaml(path, default = list(version = 1L, decisions = list()))
  x$decisions <- x$decisions %||% list()
  x$profiles <- x$profiles %||% list()
  x$country_field_profiles <- x$country_field_profiles %||% list()
  x$overrides <- x$overrides %||% list()
  x
}

dina_admin_trust_profile_for <- function(registry, decision, field) {
  country <- toupper(trimws(as.character(decision$country %||% "")))
  year <- suppressWarnings(as.integer(decision$year %||% NA))
  name <- decision[[paste0(field, "_profile")]] %||% NULL
  country_profiles <- registry$country_field_profiles[[country]] %||% list()
  name <- name %||% country_profiles[[field]] %||% NULL
  if (length(registry$overrides)) for (override in registry$overrides) {
    if (!identical(toupper(as.character(override$country %||% "")), country)) next
    years <- suppressWarnings(as.integer(unlist(override$years %||% override$year %||% integer(), use.names = FALSE)))
    if (!length(years) || !year %in% years) next
    name <- override[[paste0(field, "_profile")]] %||% name
  }
  profile <- registry$profiles[[as.character(name %||% "")]] %||% list()
  list(
    name = as.character(name %||% ""),
    basis = as.character(decision[[paste0(field, "_basis")]] %||% profile$basis %||% decision$basis %||% "explicit_policy"),
    rule = as.character(decision[[paste0(field, "_rule")]] %||% profile$rule %||% decision$rule %||% ""),
    provenance_status = as.character(decision[[paste0(field, "_provenance_status")]] %||% profile$provenance_status %||% decision$provenance_status %||% "verified"),
    provenance_note = as.character(decision[[paste0(field, "_provenance_note")]] %||% profile$provenance_note %||% decision$provenance_note %||% "")
  )
}

dina_admin_trust_rows <- function(root = dina_repo_root(), path = dina_admin_trust_regions_path(root)) {
  registry <- dina_admin_trust_regions(root, path)
  decisions <- registry$decisions %||% list()
  if (!length(decisions)) {
    return(data.frame(country = character(), year = integer(), trust = numeric(), trust_wages = numeric(), basis = character(), rule = character(), provenance_status = character(), provenance_note = character(), status = character(), stringsAsFactors = FALSE))
  }
  rows <- lapply(decisions, function(x) {
    trust <- dina_admin_trust_profile_for(registry, x, "trust")
    wages <- dina_admin_trust_profile_for(registry, x, "trust_wages")
    data.frame(
      country = toupper(trimws(as.character(x$country %||% ""))),
      year = suppressWarnings(as.integer(x$year %||% NA)),
      trust = suppressWarnings(as.numeric(x$trust %||% NA)),
      trust_wages = suppressWarnings(as.numeric(x$trust_wages %||% NA)),
      basis = trimws(trust$basis), rule = trimws(trust$rule),
      provenance_status = trimws(trust$provenance_status), provenance_note = trimws(trust$provenance_note),
      trust_wages_basis = trimws(wages$basis), trust_wages_rule = trimws(wages$rule),
      trust_wages_provenance_status = trimws(wages$provenance_status),
      # `active` describes an entry in the currently selected configuration;
      # it is deliberately not a source-acceptance state.  Read legacy
      # `confirmed` entries as active so existing configurations remain valid.
      status = {
        value <- trimws(as.character(x$status %||% "active"))
        if (identical(value, "confirmed")) "active" else value
      },
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Trust regions are accepted with the Admin source family.  The runtime table is
# consequently a source snapshot, not a configuration-validation by-product.
dina_admin_trust_snapshot_path <- function(root = dina_repo_root(), session = NULL) {
  if (is.null(session)) return(file.path(root, "output", "experiments", "admin_pit_include", "admin_trust_regions.csv"))
  file.path(dina_update_dir(session$id, root), "admin_trust_regions.csv")
}

# Kept as the public internal name used by the Stata renderer and older tests.
dina_admin_trust_runtime_path <- function(root = dina_repo_root(), session = NULL) {
  dina_admin_trust_snapshot_path(root, session)
}

dina_admin_trust_snapshot_manifest_path <- function(root = dina_repo_root(), session = NULL) {
  paste0(dina_admin_trust_snapshot_path(root, session), ".manifest.json")
}

# Admin Include is the authority that turns publisher PIT workbooks into the
# interpolation-ready tables consumed by the pipeline.  Keep its handoff
# beside the selected scope, just like the trust snapshot: an update can be
# tested without changing the benchmark, and a later source edit cannot alter
# an already-included handoff silently.
dina_admin_pit_outputs_manifest_path <- function(root = dina_repo_root(), session = NULL) {
  if (is.null(session)) return(file.path(root, "output", "experiments", "admin_pit_include", "admin_pit_outputs.manifest.json"))
  file.path(dina_update_dir(session$id, root), "admin_pit_outputs.manifest.json")
}

dina_hash_path <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (!dir.exists(path)) return(dina_hash_file(path))
  dina_need("digest")
  files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) return(digest::digest("", algo = "sha256"))
  root <- normalizePath(path, winslash = "/", mustWork = FALSE)
  rel <- substring(normalizePath(files, winslash = "/", mustWork = FALSE), nchar(root) + 2L)
  entries <- paste(rel, vapply(files, dina_hash_file, character(1)), sep = " ")
  digest::digest(paste(sort(entries), collapse = "\n"), algo = "sha256")
}

dina_admin_pit_outputs_scope <- function(root = dina_repo_root(), session = NULL) {
  config <- dina_session_config(session, root, expand_env = TRUE)
  identity <- dina_config_validation_identity(root, session, dina_config_validation_receipt(root, session))
  list(
    scope = identity$scope,
    effective_config_fingerprint = identity$effective_config_fingerprint,
    countries = toupper(as.character(config$countries %||% character())),
    first_year = as.integer(config$years$first %||% NA),
    last_year = as.integer(config$years$last %||% NA)
  )
}

# Version 1 Admin inclusion manifests sorted directory entries under C.UTF-8.
# R's default sort follows the caller's LC_COLLATE; under C the unchanged COL
# publisher directory gets a different digest. Preserve the accepted digest
# algorithm while making verification independent of the launching shell.
dina_admin_pit_hash_path <- function(path) {
  original_all <- Sys.getenv("LC_ALL", unset = NA_character_)
  original_collate <- Sys.getlocale("LC_COLLATE")
  on.exit({
    suppressWarnings(Sys.setlocale("LC_COLLATE", original_collate))
    if (is.na(original_all)) Sys.unsetenv("LC_ALL") else Sys.setenv(LC_ALL = original_all)
  }, add = TRUE)
  Sys.setenv(LC_ALL = "C.UTF-8")
  selected <- suppressWarnings(Sys.setlocale("LC_COLLATE", "C.UTF-8"))
  if (is.na(selected) || !nzchar(selected)) {
    stop("Cannot verify the Admin inclusion manifest: C.UTF-8 locale is unavailable.", call. = FALSE)
  }
  dina_hash_path(path)
}

dina_admin_pit_outputs_manifest_problem <- function(root = dina_repo_root(), session = NULL) {
  path <- dina_admin_pit_outputs_manifest_path(root, session)
  remedy <- "Explore and include Admin."
  if (!file.exists(path)) {
    return(sprintf("Admin PIT handoff manifest is missing (%s). %s", dina_relative(path, root), remedy))
  }
  manifest <- tryCatch(dina_read_json(path), error = function(e) NULL)
  expected <- dina_admin_pit_outputs_scope(root, session)
  if (!is.list(manifest) || !identical(as.character(manifest$artifact %||% ""), "admin_pit_outputs")) {
    return(sprintf("Admin PIT handoff manifest is unreadable or invalid (%s). %s", dina_relative(path, root), remedy))
  }
  actual_scope <- as.character(manifest$scope %||% "")
  if (!identical(actual_scope, expected$scope)) {
    return(sprintf("Admin PIT handoff belongs to %s, but this run uses %s. %s", actual_scope %||% "an unknown scope", expected$scope, remedy))
  }
  actual_config <- as.character(manifest$effective_config_fingerprint %||% "")
  if (!identical(actual_config, expected$effective_config_fingerprint)) {
    return(sprintf("Admin PIT handoff was created for a different effective configuration. %s", remedy))
  }
  artifacts <- manifest$artifacts %||% list()
  if (!length(artifacts)) return(sprintf("Admin PIT handoff records no included artifacts. %s", remedy))
  for (artifact in artifacts) {
    rel <- as.character(artifact$path %||% "")
    target <- file.path(root, rel)
    expected_hash <- as.character(artifact$sha256 %||% "")
    if (!nzchar(rel) || !nzchar(expected_hash)) {
      return(sprintf("Admin PIT handoff has an incomplete artifact record. %s", remedy))
    }
    if (!file.exists(target)) {
      return(sprintf("Included Admin artifact is missing: %s. %s", rel, remedy))
    }
    if (!identical(dina_admin_pit_hash_path(target), expected_hash)) {
      return(sprintf("Included Admin artifact changed after inclusion: %s. %s", rel, remedy))
    }
  }
  character()
}

# A pre-manifest Include did not lack a review decision; it only lacks this
# machine-readable handoff receipt.  Backfill it only when the original
# confirmation and every currently canonical artifact still match.
dina_admin_pit_outputs_backfill_status <- function(root = dina_repo_root(), session = NULL) {
  path <- dina_admin_pit_outputs_manifest_path(root, session)
  if (file.exists(path)) return(list(eligible = FALSE, reason = "An Admin PIT output manifest already exists."))
  if (!exists("dina_review_read", mode = "function") || !exists("admin_pit_include_backfill_output_manifest", mode = "function")) {
    return(list(eligible = FALSE, reason = "The Admin source-review handoff is unavailable."))
  }
  record <- dina_review_read(root, "admin")
  if (is.null(record) || !identical(record$status, "included")) {
    return(list(eligible = FALSE, reason = "Admin sources have not been included."))
  }
  # The historical review watch deliberately covers the broad Admin inbox and
  # can change for unrelated source work.  The backfill itself verifies the
  # exact promoted raw and clean artifact hashes from the confirmation, which
  # is stricter and avoids asking for a needless repeat review.
  list(eligible = TRUE, record = record)
}

dina_admin_pit_outputs_manifest_ensure <- function(root = dina_repo_root(), session = NULL) {
  issue <- dina_admin_pit_outputs_manifest_problem(root, session)
  if (!length(issue)) return(list(created = FALSE, path = dina_admin_pit_outputs_manifest_path(root, session)))
  if (!exists("admin_pit_include_backfill_output_manifest", mode = "function")) {
    # The backfill helper is also used by the pipeline preflight, whose
    # caller frame is not visible to the status helper.  Load it in the CLI
    # environment rather than a transient function frame.
    source(file.path(root, "code", "R", "source-diagnostics", "admin_pit_include.R"), local = globalenv())
  }
  status <- dina_admin_pit_outputs_backfill_status(root, session)
  if (!isTRUE(status$eligible)) return(c(status, list(created = FALSE, issue = issue)))
  result <- tryCatch(admin_pit_include_backfill_output_manifest(root = root, session = session, confirm_run = status$record$confirmation %||% NULL), error = function(e) e)
  if (inherits(result, "error")) return(c(status, list(created = FALSE, issue = conditionMessage(result))))
  c(status, list(created = TRUE, path = result$path))
}

dina_admin_trust_snapshot_scope <- function(config, session = NULL) {
  dina_need("digest")
  countries <- toupper(as.character(config$countries %||% character()))
  scope <- list(
    scope = dina_config_scope(session)$id,
    countries = countries,
    first_year = as.integer(config$years$first %||% NA),
    last_year = as.integer(config$years$last %||% NA)
  )
  c(scope, list(scope_fingerprint = digest::digest(scope, algo = "sha256", serialize = TRUE)))
}

# Matches countries_bfm_02a in aux_general.do: these are the formatted PIT
# tables actually consumed by the one-stage BFM correction. MEX's recovered
# gpinter files feed effective-rate calculation; its separate two-stage BFM
# decision is required only if an eligible diverse/wage input is supplied.
dina_admin_trust_bfm_countries <- function() c("COL", "URY", "BRA", "DOM", "SLV", "ECU", "PER", "CHL")
dina_admin_trust_bfm_2stage_countries <- function() c("ARG", "CRI", "MEX")
dina_admin_trust_required_empty <- function() data.frame(country = character(), year = integer(), file = character(), sheet = character(), requires_trust_wages = logical(), wage_file = character(), stringsAsFactors = FALSE)

dina_admin_trust_prepared_files <- function(root = dina_repo_root()) {
  # 02e creates one gpinter workbook per country-year from these clean tables.
  # Use the same pre/post choice as 02e so validation forecasts the files that
  # the pipeline will actually consume, without treating every cleaner output
  # as a separate decision.
  countries <- dina_admin_trust_bfm_countries()
  clean <- unlist(lapply(countries, function(country) {
    component <- if (identical(country, "BRA")) "pre" else "pos"
    file.path(root, "input_data", "admin_data", country, "_clean", sprintf("total-%s-%s.xlsx", component, country))
  }), use.names = FALSE)
  gpinter <- Sys.glob(file.path(root, "input_data", "admin_data", "*", "gpinter_*.xlsx"))
  unique(c(clean[file.exists(clean)], gpinter))
}

dina_admin_trust_rows_from_file <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  country_match <- regmatches(path, regexec("/admin_data/([A-Z]{3})/", path, perl = TRUE))[[1L]]
  if (length(country_match) != 2L) return(data.frame())
  country <- country_match[[2L]]
  base <- basename(path)
  gpinter_match <- regmatches(base, regexec("^gpinter_[A-Z]{3}_([0-9]{4})\\.xlsx$", base, perl = TRUE))[[1L]]
  fixed_year <- if (length(gpinter_match) == 2L) as.integer(gpinter_match[[2L]]) else NA_integer_
  if (!requireNamespace("readxl", quietly = TRUE) || !file.exists(path)) return(data.frame())
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  if (!length(sheets)) return(data.frame())
  rows <- lapply(sheets, function(sheet) {
    # Header-only reads keep the configuration check quick even when the
    # formatted tables contain thousands of brackets. Values are read later
    # only for a publisher-derived decision or reviewer evidence.
    data <- tryCatch(readxl::read_excel(path, sheet = sheet, n_max = 0L, .name_repair = "minimal"), error = function(e) NULL)
    if (is.null(data) || !"p" %in% tolower(names(data))) return(NULL)
    year <- fixed_year
    if (!is.finite(year) && grepl("^[0-9]{4}$", sheet)) year <- as.integer(sheet)
    if (!is.finite(year) && "year" %in% tolower(names(data))) {
      year_data <- tryCatch(readxl::read_excel(path, sheet = sheet, range = "A1:ZZ2", .name_repair = "minimal"), error = function(e) data.frame())
      value <- suppressWarnings(as.integer(year_data[[which(tolower(names(year_data)) == "year")[1L]]]))
      value <- value[is.finite(value)]
      if (length(value)) year <- value[[1L]]
    }
    if (!is.finite(year)) return(NULL)
    data.frame(country = country, year = year, file = path, sheet = sheet, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

dina_admin_trust_survey_years <- function(root = dina_repo_root(), country) {
  files <- Sys.glob(file.path(root, "intermediary_data", "microdata", "raw", country, sprintf("%s_*_raw.dta", country)))
  years <- suppressWarnings(as.integer(sub(paste0("^", country, "_([0-9]{4})_raw\\.dta$"), "\\1", basename(files))))
  sort(unique(years[is.finite(years)]))
}

dina_admin_trust_required_rows <- function(root = dina_repo_root(), config = dina_config(root), files = dina_admin_trust_prepared_files(root)) {
  rows <- Filter(Negate(is.null), lapply(files, dina_admin_trust_rows_from_file))
  if (!length(rows)) return(dina_admin_trust_required_empty())
  rows <- do.call(rbind, rows)
  expected_clean <- vapply(seq_len(nrow(rows)), function(i) {
    component <- if (identical(rows$country[[i]], "BRA")) "pre" else "pos"
    grepl(paste0("/_clean/total-", component, "-", rows$country[[i]], "\\.xlsx$"), rows$file[[i]])
  }, logical(1))
  rows <- rows[grepl("/gpinter_[A-Z]{3}_[0-9]{4}\\.xlsx$", rows$file) | expected_clean, , drop = FALSE]
  countries <- toupper(as.character(config$countries %||% character()))
  rows <- rows[rows$country %in% countries & rows$country %in% dina_admin_trust_bfm_countries() &
    rows$year >= as.integer(config$years$first) & rows$year <= as.integer(config$years$last), , drop = FALSE]
  # A trust decision is meaningful only when 03a can combine the formatted
  # PIT table with a prepared survey. This prevents an unused PIT sheet from
  # becoming a configuration blocker.
  eligible <- mapply(function(country, year) year %in% dina_admin_trust_survey_years(root, country), rows$country, rows$year)
  rows <- rows[eligible, , drop = FALSE]
  rows$requires_trust_wages <- FALSE
  rows$wage_file <- NA_character_
  rows <- rows[order(rows$country, rows$year, rows$file, rows$sheet), , drop = FALSE]
  rows[!duplicated(rows[c("country", "year")]), , drop = FALSE]
}

# Two-stage BFM reads already-prepared diverse/wage tables directly, rather
# than 02e's total-pre/total-pos workbooks. These are actual overlap years;
# extrapolated BFM years deliberately do not require a trust decision.
dina_admin_trust_required_2stage_rows <- function(root = dina_repo_root(), config = dina_config(root)) {
  countries <- intersect(toupper(as.character(config$countries %||% character())), dina_admin_trust_bfm_2stage_countries())
  if (!length(countries)) return(dina_admin_trust_required_empty())
  files <- unlist(lapply(countries, function(country) c(
    Sys.glob(file.path(root, "input_data", "admin_data", country, sprintf("diverse_%s_*.xlsx", country))),
    Sys.glob(file.path(root, "input_data", "admin_data", country, sprintf("wage_%s_*.xlsx", country)))
  )), use.names = FALSE)
  if (!length(files)) return(dina_admin_trust_required_empty())
  parsed <- lapply(files, function(path) {
    base <- basename(path)
    hit <- regmatches(base, regexec("^(?:diverse|wage)_([A-Z]{3})_([0-9]{4})\\.xlsx$", base, perl = TRUE))[[1L]]
    if (length(hit) != 3L) return(NULL)
    data.frame(country = hit[[2L]], year = as.integer(hit[[3L]]), kind = sub("_.*$", "", base), file = normalizePath(path, winslash = "/", mustWork = FALSE), sheet = "prepared", stringsAsFactors = FALSE)
  })
  parsed <- Filter(Negate(is.null), parsed)
  if (!length(parsed)) return(dina_admin_trust_required_empty())
  rows <- do.call(rbind, parsed)
  # 03b starts from diverse_*; wage_* is an additional input for trust_wages.
  rows <- rows[rows$kind == "diverse", , drop = FALSE]
  if (!nrow(rows)) return(dina_admin_trust_required_empty())
  wage_path <- function(country, year) {
    path <- file.path(root, "input_data", "admin_data", country, sprintf("wage_%s_%s.xlsx", country, year))
    if (file.exists(path)) normalizePath(path, winslash = "/", mustWork = FALSE) else NA_character_
  }
  rows$wage_file <- mapply(wage_path, rows$country, rows$year, USE.NAMES = FALSE)
  rows$requires_trust_wages <- !is.na(rows$wage_file)
  rows$kind <- NULL
  rows <- rows[rows$year >= as.integer(config$years$first) & rows$year <= as.integer(config$years$last), , drop = FALSE]
  eligible <- mapply(function(country, year) year %in% dina_admin_trust_survey_years(root, country), rows$country, rows$year)
  rows <- rows[eligible, , drop = FALSE]
  rows[order(rows$country, rows$year, rows$file), , drop = FALSE]
}

dina_admin_trust_required_all_rows <- function(root = dina_repo_root(), config = dina_config(root), files = dina_admin_trust_prepared_files(root)) {
  one_stage <- dina_admin_trust_required_rows(root, config, files)
  two_stage <- dina_admin_trust_required_2stage_rows(root, config)
  rows <- rbind(one_stage, two_stage)
  if (!nrow(rows)) return(rows)
  rows <- rows[order(rows$country, rows$year, rows$file), , drop = FALSE]
  rows[!duplicated(rows[c("country", "year")]), , drop = FALSE]
}

dina_admin_trust_p_values <- function(path, sheet = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE) || !file.exists(path)) return(numeric())
  tryCatch({
    data <- readxl::read_excel(path, sheet = sheet %||% 1L, .name_repair = "minimal")
    column <- which(tolower(names(data)) == "p")
    if (!length(column)) return(numeric())
    sort(unique(suppressWarnings(as.numeric(data[[column[[1L]]]]))))
  }, error = function(e) numeric())
}

# The documented source rule is deliberately positional: choose the first
# strictly-positive bracket after a zero-starting bracket, rather than the
# first available cutoff. This avoids treating a 0-starting tax bracket as a
# meaningful top-income trust region.
dina_admin_trust_publisher_cutoff <- function(path, sheet = NULL) {
  unavailable <- list(valid = FALSE, trust = NA_real_, threshold = NA_real_, row = NA_integer_, rule = "first_positive_threshold", evidence = "prepared PIT table has no recognized threshold/p columns")
  if (!requireNamespace("readxl", quietly = TRUE) || !file.exists(path)) return(unavailable)
  data <- tryCatch(readxl::read_excel(path, sheet = sheet %||% 1L, .name_repair = "minimal"), error = function(e) NULL)
  if (is.null(data)) return(unavailable)
  names_lower <- tolower(names(data))
  p_col <- which(names_lower == "p")
  threshold_col <- which(names_lower %in% c("thr", "threshold", "thresholds"))
  if (!length(p_col) || !length(threshold_col)) return(unavailable)
  p <- suppressWarnings(as.numeric(data[[p_col[[1L]]]]))
  threshold <- suppressWarnings(as.numeric(data[[threshold_col[[1L]]]]))
  zero <- which(is.finite(threshold) & abs(threshold) <= 1e-10)
  if (!length(zero)) {
    unavailable$evidence <- "prepared PIT table has no zero-starting threshold; cannot apply first_positive_threshold"
    return(unavailable)
  }
  candidate <- which(seq_along(threshold) > min(zero) & is.finite(threshold) & threshold > 0 & is.finite(p) & p > 0 & p < 1)
  if (!length(candidate)) {
    unavailable$evidence <- "prepared PIT table has no usable positive threshold after its zero-starting bracket"
    return(unavailable)
  }
  i <- candidate[[1L]]
  list(valid = TRUE, trust = p[[i]], threshold = threshold[[i]], row = i, rule = "first_positive_threshold",
    evidence = sprintf("first positive threshold after zero bracket: threshold %s, p %s (row %s)", format(threshold[[i]], trim = TRUE, scientific = FALSE), format(p[[i]], trim = TRUE, scientific = FALSE), i))
}

dina_admin_trust_validate <- function(root = dina_repo_root(), config = dina_config(root), require_coverage = TRUE, files = dina_admin_trust_prepared_files(root), trust_path = dina_admin_trust_regions_path(root)) {
  rows <- dina_admin_trust_rows(root, trust_path)
  errors <- character()
  if (nrow(rows)) {
    if (any(!grepl("^[A-Z]{3}$", rows$country) | is.na(rows$year))) errors <- c(errors, "Trust-region decisions require an ISO3 country and year.")
    if (any(!is.finite(rows$trust) | rows$trust <= 0 | rows$trust >= 1)) errors <- c(errors, "Trust-region trust values must be strictly between 0 and 1.")
    wage_present <- !is.na(rows$trust_wages)
    if (any(wage_present & (!is.finite(rows$trust_wages) | rows$trust_wages <= 0 | rows$trust_wages >= 1))) errors <- c(errors, "Trust-region trust_wages values must be strictly between 0 and 1 when supplied.")
    allowed <- c("publisher_derived", "carried_forward", "explicit_policy")
    if (any(!rows$basis %in% allowed)) errors <- c(errors, "Trust-region basis must be publisher_derived, carried_forward, or explicit_policy.")
    if (any(!rows$provenance_status %in% c("verified", "legacy_unverified", "inferred_policy"))) errors <- c(errors, "Trust-region provenance_status must be verified, legacy_unverified, or inferred_policy.")
    if (any(!rows$status %in% c("active", "proposed"))) errors <- c(errors, "Trust-region status must be active or proposed.")
    if (anyDuplicated(paste(rows$country, rows$year))) errors <- c(errors, "Trust-region decisions must be unique by country and year.")
  }
  required <- dina_admin_trust_required_all_rows(root, config, files = files)
  published <- rows[rows$basis == "publisher_derived", , drop = FALSE]
  if (nrow(published) && nrow(required)) {
    for (i in seq_len(nrow(published))) {
      row <- published[i, , drop = FALSE]
      source <- required[required$country == row$country & required$year == row$year, , drop = FALSE]
      if (!nrow(source)) next
      # Historical directory.xlsx values did not retain the source release,
      # threshold row, or population version. Keep that gap visible, but do
      # not rewrite a historical decision merely because today's source no
      # longer reproduces it. Newly-derived values must reproduce exactly.
      if (!identical(row$provenance_status[[1L]], "legacy_unverified")) {
        cutoff <- dina_admin_trust_publisher_cutoff(source$file[[1L]], source$sheet[[1L]])
        if (!isTRUE(cutoff$valid) || abs(cutoff$trust - row$trust) > 1e-6) {
          errors <- c(errors, paste0("Publisher-derived trust for ", row$country, " ", row$year, " does not match the first positive threshold after the zero bracket."))
        }
      }
    }
  }
  missing <- required[!paste(required$country, required$year) %in% paste(rows$country, rows$year), , drop = FALSE]
  proposed <- rows$basis %in% "carried_forward" & paste(rows$country, rows$year) %in% paste(required$country, required$year)
  if (any(proposed)) {
    labels <- paste0(rows$country[proposed], " ", rows$year[proposed])
    errors <- c(errors, paste0("Trust-region carry-forward proposals still need confirmation: ", paste(labels, collapse = ", "), ". Review the Admin trust proposal and set each basis to explicit_policy or publisher_derived."))
  }
  proposed_rows <- rows$status %in% "proposed" & paste(rows$country, rows$year) %in% paste(required$country, required$year)
  if (any(proposed_rows)) {
    labels <- paste0(rows$country[proposed_rows], " ", rows$year[proposed_rows])
    errors <- c(errors, paste0("The proposed trust configuration is incomplete: ", paste(labels, collapse = ", "), ". Review every proposed row, then apply the complete proposal with `dina sources configure admin --apply`."))
  }
  if (isTRUE(require_coverage) && nrow(missing)) {
    labels <- paste0(missing$country, " ", missing$year)
    errors <- c(errors, paste0("Trust-region decisions are required for formatted PIT inputs: ", paste(labels, collapse = ", "), ". Run `dina sources explore admin`, then `dina sources configure admin`."))
  }
  wage_needed <- required$requires_trust_wages %in% TRUE
  if (any(wage_needed)) {
    keys <- paste(required$country[wage_needed], required$year[wage_needed])
    corresponding <- rows[match(keys, paste(rows$country, rows$year)), , drop = FALSE]
    missing_wages <- !is.finite(corresponding$trust_wages)
    if (any(missing_wages)) {
      errors <- c(errors, paste0("Two-stage BFM requires trust_wages decisions for: ", paste(keys[missing_wages], collapse = ", "), "."))
    }
  }
  list(valid = !length(errors), errors = unique(errors), rows = rows, required = required, missing = missing)
}

dina_admin_trust_pit_evidence <- function(path, sheet = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE) || !file.exists(path)) return("prepared PIT table unavailable")
  values <- tryCatch({
    p <- dina_admin_trust_p_values(path, sheet)
    p <- p[is.finite(p) & p > 0 & p < 1]
    if (!length(p)) return("prepared PIT table has no usable p column")
    sprintf("prepared PIT p range %s–%s (%s cutoffs)", format(min(p), trim = TRUE), format(max(p), trim = TRUE), length(p))
  }, error = function(e) paste("prepared PIT table could not be read:", conditionMessage(e)))
  values
}

dina_admin_trust_review <- function(root = dina_repo_root(), config = dina_config(root), files = dina_admin_trust_prepared_files(root), trust_path = dina_admin_trust_regions_path(root)) {
  checked <- dina_admin_trust_validate(root, config, require_coverage = FALSE, files = files, trust_path = trust_path)
  required <- checked$required
  rows <- checked$rows
  empty <- data.frame(country = character(), year = integer(), file = character(), sheet = character(), current_trust = numeric(), current_trust_wages = numeric(), basis = character(), rule = character(), provenance_status = character(), proposal_trust = numeric(), proposal_trust_wages = numeric(), status = character(), evidence = character(), next_step = character(), stringsAsFactors = FALSE)
  provenance <- data.frame(country = character(), year = integer(), field = character(), basis = character(), reason = character(), likely_explanation = character(), stringsAsFactors = FALSE)
  if (!nrow(required)) return(list(proposals = empty, decisions = empty, provenance = provenance, valid = TRUE))
  out <- lapply(seq_len(nrow(required)), function(i) {
    need <- required[i, , drop = FALSE]
    hit <- rows[rows$country == need$country & rows$year == need$year, , drop = FALSE]
    cutoff <- dina_admin_trust_publisher_cutoff(need$file, need$sheet)
    evidence <- if (isTRUE(cutoff$valid)) cutoff$evidence else dina_admin_trust_pit_evidence(need$file, need$sheet)
    if (nrow(hit)) {
      state <- if (hit$basis[[1L]] %in% "carried_forward" || hit$status[[1L]] %in% "proposed") "configuration_pending" else "configured"
      next_step <- if (identical(state, "configured")) "ready" else "Review this row with the complete proposed trust configuration."
      return(data.frame(country = need$country, year = need$year, file = dina_relative(need$file, root), sheet = need$sheet,
        current_trust = hit$trust[[1L]], current_trust_wages = hit$trust_wages[[1L]], basis = hit$basis[[1L]],
        rule = hit$rule[[1L]], provenance_status = hit$provenance_status[[1L]],
        proposal_trust = hit$trust[[1L]], proposal_trust_wages = hit$trust_wages[[1L]], status = state,
        evidence = evidence, next_step = next_step, stringsAsFactors = FALSE))
    }
    previous <- rows[rows$country == need$country & rows$year < need$year & is.finite(rows$trust), , drop = FALSE]
    if (nrow(previous) && identical(previous$basis[[which.max(previous$year)]], "publisher_derived") && isTRUE(cutoff$valid)) {
      return(data.frame(country = need$country, year = need$year, file = dina_relative(need$file, root), sheet = need$sheet,
        current_trust = NA_real_, current_trust_wages = NA_real_, basis = "publisher_derived", rule = cutoff$rule, provenance_status = "verified",
        proposal_trust = cutoff$trust, proposal_trust_wages = NA_real_, status = "configuration_pending", evidence = cutoff$evidence,
        next_step = "Review this source-derived row in the complete trust-configuration proposal.", stringsAsFactors = FALSE))
    }
    if (nrow(previous)) {
      previous <- previous[which.max(previous$year), , drop = FALSE]
      return(data.frame(country = need$country, year = need$year, file = dina_relative(need$file, root), sheet = need$sheet,
        current_trust = NA_real_, current_trust_wages = NA_real_, basis = "carried_forward", rule = "", provenance_status = "inferred_policy",
        proposal_trust = previous$trust[[1L]], proposal_trust_wages = previous$trust_wages[[1L]], status = "configuration_pending",
        evidence = paste0(evidence, "; nearest active configuration: ", previous$year[[1L]]),
        next_step = "Add this row to the complete trust-configuration proposal.", stringsAsFactors = FALSE))
    }
    data.frame(country = need$country, year = need$year, file = dina_relative(need$file, root), sheet = need$sheet,
      current_trust = NA_real_, current_trust_wages = NA_real_, basis = "", rule = "", provenance_status = "", proposal_trust = NA_real_, proposal_trust_wages = NA_real_, status = "unresolved",
      evidence = evidence, next_step = "Review the Admin trust proposal.", stringsAsFactors = FALSE)
  })
  proposals <- do.call(rbind, out)
  legacy_trust <- rows[rows$provenance_status == "legacy_unverified", , drop = FALSE]
  legacy_wages <- rows[rows$trust_wages_provenance_status == "legacy_unverified" & is.finite(rows$trust_wages), , drop = FALSE]
  provenance_parts <- Filter(Negate(is.null), list(
    if (nrow(legacy_trust)) data.frame(country = legacy_trust$country, year = legacy_trust$year, field = "trust", basis = legacy_trust$basis,
      reason = "The migrated directory.xlsx stored the chosen value but no source release, threshold row, or population version.",
      likely_explanation = "Likely derived from the then-current tax schedule using the first positive threshold after the zero bracket; it cannot be reproduced from today's prepared table without historical source provenance.", stringsAsFactors = FALSE),
    if (nrow(legacy_wages)) data.frame(country = legacy_wages$country, year = legacy_wages$year, field = "trust_wages", basis = legacy_wages$trust_wages_basis,
      reason = "The migrated directory.xlsx stored the wage trust value but no source release or threshold row.",
      likely_explanation = "Likely derived from the then-current wage schedule using the same first-positive-threshold rule; the retained value is not enough to prove that provenance today.", stringsAsFactors = FALSE)
  ))
  if (length(provenance_parts)) provenance <- do.call(rbind, provenance_parts)
  list(proposals = proposals, decisions = proposals[proposals$status == "configured", , drop = FALSE], provenance = provenance, valid = all(proposals$status == "configured"))
}

dina_write_admin_trust_runtime <- function(root = dina_repo_root(), session = NULL,
                                           config = dina_session_config(session, root, expand_env = FALSE),
                                           trust_path = dina_admin_trust_regions_path(root),
                                           output_path = dina_admin_trust_snapshot_path(root, session),
                                           manifest_path = paste0(output_path, ".manifest.json")) {
  check <- dina_admin_trust_validate(root, config, trust_path = trust_path)
  if (!isTRUE(check$valid)) stop(check$errors[[1L]], call. = FALSE)
  path <- output_path
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  countries <- toupper(as.character(config$countries %||% character()))
  scoped <- check$rows[check$rows$country %in% countries &
    check$rows$year >= as.integer(config$years$first) & check$rows$year <= as.integer(config$years$last),
    c("country", "year", "trust", "trust_wages", "basis"), drop = FALSE]
  scoped <- scoped[order(scoped$country, scoped$year), , drop = FALSE]
  utils::write.csv(scoped, path, row.names = FALSE, na = "")
  scope <- dina_admin_trust_snapshot_scope(config, session)
  dina_write_json(c(
    list(
      artifact = "admin_trust_regions",
      created_at = dina_now(),
      registry_sha256 = dina_hash_file(trust_path),
      table_sha256 = dina_hash_file(path),
      rows = nrow(scoped)
    ),
    scope
  ), manifest_path)
  path
}

dina_admin_trust_snapshot_problem <- function(root = dina_repo_root(), session = NULL,
                                              config = dina_session_config(session, root, expand_env = FALSE)) {
  path <- dina_admin_trust_snapshot_path(root, session)
  manifest_path <- dina_admin_trust_snapshot_manifest_path(root, session)
  if (!file.exists(path) || !file.exists(manifest_path)) {
    return("Admin sources have not been included for this scope; explore and include Admin.")
  }
  manifest <- tryCatch(dina_read_json(manifest_path), error = function(e) NULL)
  expected <- dina_admin_trust_snapshot_scope(config, session)
  same_scope <- is.list(manifest) &&
    identical(as.character(manifest$artifact %||% ""), "admin_trust_regions") &&
    identical(as.character(manifest$scope %||% ""), expected$scope) &&
    identical(as.character(manifest$scope_fingerprint %||% ""), expected$scope_fingerprint)
  if (!same_scope) {
    return("The included Admin trust snapshot does not match this scope; explore and include Admin again.")
  }
  if (!identical(as.character(manifest$table_sha256 %||% ""), as.character(dina_hash_file(path)))) {
    return("The included Admin trust snapshot was modified or corrupted; explore and include Admin again.")
  }
  rows <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) e)
  required <- c("country", "year", "trust", "trust_wages", "basis")
  if (inherits(rows, "error") || !all(required %in% names(rows))) {
    return("The included Admin trust snapshot is unreadable; explore and include Admin again.")
  }
  character()
}

# The scoped CSV is a derived runtime artifact of an accepted Admin review.
# Early inclusions made before this artifact was introduced are still valid:
# they have accepted sources and a valid trust registry, but no CSV yet.  Do
# not make a reviewer repeat that decision just to create a deterministic file
# the CLI can derive without new judgement.
dina_admin_trust_snapshot_backfill_status <- function(root = dina_repo_root(), session = NULL,
                                                      config = dina_session_config(session, root, expand_env = FALSE)) {
  path <- dina_admin_trust_snapshot_path(root, session)
  manifest_path <- dina_admin_trust_snapshot_manifest_path(root, session)
  if (file.exists(path) || file.exists(manifest_path)) {
    return(list(eligible = FALSE, reason = "A trust snapshot already exists."))
  }
  if (!exists("dina_review_read", mode = "function") ||
      !exists("dina_review_included_watch_changed", mode = "function")) {
    return(list(eligible = FALSE, reason = "The source-review interface is unavailable."))
  }
  record <- dina_review_read(root, "admin")
  if (is.null(record) || !identical(record$status, "included")) {
    return(list(eligible = FALSE, reason = "Admin sources have not been included."))
  }
  if (isTRUE(dina_review_included_watch_changed(record))) {
    return(list(eligible = FALSE, reason = "The included Admin review needs rechecking."))
  }
  trust <- dina_admin_trust_validate(root, config)
  if (!isTRUE(trust$valid)) {
    return(list(eligible = FALSE, reason = trust$errors[[1L]] %||% "The included Admin trust decisions are incomplete."))
  }
  list(eligible = TRUE, record = record, reason = "Accepted Admin sources have no scoped runtime trust snapshot yet.")
}

dina_admin_trust_snapshot_ensure <- function(root = dina_repo_root(), session = NULL,
                                             config = dina_session_config(session, root, expand_env = FALSE)) {
  status <- dina_admin_trust_snapshot_backfill_status(root, session, config)
  if (!isTRUE(status$eligible)) return(c(status, list(created = FALSE)))
  path <- dina_write_admin_trust_runtime(root, session = session, config = config)
  # Record the new derived artifact in the accepted review's watch.  Future
  # edits to the trust registry will correctly request a new Admin review,
  # while this one-time compatibility migration remains invisible to users.
  record <- status$record
  record$admin_trust_watch <- dina_review_watch(c(
    dina_admin_trust_regions_path(root),
    path,
    dina_admin_trust_snapshot_manifest_path(root, session)
  ))
  dina_review_save(record, root)
  c(status, list(created = TRUE, path = path))
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

# `notify init` creates a deliberately inert template.  Inspect its values
# without loading them into pushoverr, so `dina doctor` never calls a service
# or reports placeholder credentials as usable.
dina_pushover_local_credentials <- function(root = dina_repo_root()) {
  path <- dina_pushover_local_path(root)
  if (!file.exists(path)) return(list(token = "", user = "", readable = FALSE))
  lines <- paste(readLines(path, warn = FALSE), collapse = "\n")
  extract <- function(pattern) {
    hit <- regexec(pattern, lines, perl = TRUE)
    matched <- regmatches(lines, hit)[[1L]]
    if (length(matched) >= 2L) matched[[2L]] else ""
  }
  list(
    token = extract("set_pushover_app\\s*\\(\\s*token\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]"),
    user = extract("set_pushover_user\\s*\\(\\s*user\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]"),
    readable = TRUE
  )
}

dina_pushover_credential_is_usable <- function(value) {
  value <- trimws(value %||% "")
  nzchar(value) && !tolower(value) %in% c("xxxxxx", "your-token", "your-user-key", "replace-me", "placeholder")
}

dina_config <- function(root = dina_repo_root(), expand_env = TRUE, path = dina_config_path(root)) {
  config <- dina_read_yaml(path)
  if (expand_env) {
    config <- dina_expand_env(config)
  }
  config
}

dina_previous_series_dir <- function(root = dina_repo_root(), config = dina_config(root, expand_env = FALSE)) {
  path <- config$paths$previous_series %||% "input_data/_new/previous_series"
  if (grepl("^/", path)) path else file.path(root, path)
}

dina_previous_series_dir_rel <- function(root = dina_repo_root(), config = dina_config(root, expand_env = FALSE)) {
  dina_relative(dina_previous_series_dir(root, config), root)
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

dina_todo_config <- function(root = dina_repo_root()) {
  x <- dina_read_yaml(dina_todo_path(root), default = list(items = list()))
  if (is.null(x$items)) {
    x$items <- list()
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
      "Direct URL that dina sources fetch can copy into input_data/_new.",
      "Direct archive URL that dina sources fetch can copy into input_data/_new.",
      "Custom acquisition script exists; not a simple direct URL fetch.",
      "Human-curated/manual input or URL index.",
      "Acquired and promoted through the WID source workflow."
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

dina_source_public_family_map <- function() {
  list(
    sna = c("macro_sna", "country_sna"),
    admin = c("admin_tax", "admin_aux"),
    admin_aux = c("admin_aux"),
    `admin-microdata` = c("admin_microdata"),
    surveys = c("surveys"),
    wid = c("wid"),
    other = c(
      "balance_sheet",
      "ceq",
      "population",
      "prices",
      "public_spending",
      "social_security",
      "tax_composition",
      "validation"
    )
  )
}

dina_source_public_families <- function() {
  names(dina_source_public_family_map())
}

dina_source_public_family_for_internal <- function(family) {
  family <- dina_source_norm(family %||% "")
  if (identical(family, "admin_aux")) {
    return("admin_aux")
  }
  for (public in names(dina_source_public_family_map())) {
    if (family %in% dina_source_norm(dina_source_public_family_map()[[public]])) {
      return(public)
    }
  }
  family
}

dina_source_public_family <- function(source) {
  if (identical(source$integration, "sources_explore_include_wid")) return("wid")
  if (identical(source$family, "admin_aux")) return("admin")
  dina_source_public_family_for_internal(source$family %||% "")
}

dina_source_resolve_family_filter <- function(value, root = dina_repo_root()) {
  if (is.null(value) || !nzchar(value)) {
    return(character())
  }
  normalized <- dina_source_norm(value)
  public_map <- dina_source_public_family_map()
  public_match <- names(public_map)[dina_source_norm(names(public_map)) == normalized]
  alias <- if (length(public_match)) public_map[[public_match[[1L]]]] else switch(
    normalized,
    admin_data = public_map$admin,
    admin_tax_aux = "admin_aux",
    country_sna = "country_sna",
    country_sna_sources = "country_sna",
    normalized
  )
  registry <- dina_sources(root)$sources
  available <- unique(vapply(registry, function(source) dina_source_field(source, "family", ""), character(1)))
  matches <- available[dina_source_norm(available) %in% dina_source_norm(alias)]
  if (!length(matches)) {
    stop(
      "Unknown source type: ", value,
      "\nPublic source types: ", paste(dina_source_public_families(), collapse = ", "),
      "\nInternal families: ", paste(sort(available), collapse = ", "),
      call. = FALSE
    )
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
    registry <- registry[vapply(registry, function(source) dina_source_field(source, "family", "") %in% family ||
      ("wid" %in% family && identical(source$integration, "sources_explore_include_wid")), logical(1))]
  }
  if (!is.null(country) && nzchar(country)) {
    registry <- registry[vapply(registry, function(source) dina_source_country_matches(dina_source_field(source, "country", ""), country), logical(1))]
  }
  if (!is.null(method) && length(method) && any(nzchar(method))) {
    registry <- registry[vapply(registry, function(source) dina_source_field(source, "method", "") %in% method, logical(1))]
  }
  registry
}

dina_source_integration_none <- function(source) {
  identical(tolower(trimws(source$integration %||% "")), "none")
}

dina_source_has_destination <- function(source) {
  length(dina_source_destinations(source)) > 0L
}

dina_source_needs_destination <- function(source) {
  if (dina_source_integration_none(source)) {
    return(FALSE)
  }
  length(dina_source_inbox_patterns(source)) > 0L ||
    length(dina_source_values(dina_source_field(source, "fetch_target"))) > 0L ||
    (source$method %||% "") %in% c("url", "zip")
}

dina_source_fetcher_values <- function(source) {
  unique(c(
    dina_source_values(dina_source_field(source, "fetcher")),
    dina_source_values(dina_source_field(source, "downloader"))
  ))
}

dina_source_registry_warnings <- function(registry, root = dina_repo_root()) {
  rows <- list()
  add <- function(source, field, message) {
    rows[[length(rows) + 1L]] <<- data.frame(
      source_id = source$id %||% "",
      family = source$family %||% "",
      field = field,
      warning = message,
      stringsAsFactors = FALSE
    )
  }
  for (source in registry) {
    if (!nzchar(source$family %||% "")) {
      add(source, "family", "Missing family.")
    }
    if (!length(dina_source_values(dina_source_field(source, "notes")))) {
      add(source, "notes", "Missing notes.")
    }
    if (dina_source_needs_destination(source) && !dina_source_has_destination(source)) {
      add(source, "destination", "Missing destination metadata for a source that has incoming or fetch targets.")
    }
    if (!dina_source_needs_destination(source) && !dina_source_has_destination(source) && !dina_source_integration_none(source)) {
      add(source, "reference", "Reference-only source has no destination metadata.")
    }
    if (dina_source_has_destination(source) && !length(dina_source_values(dina_source_field(source, "transformer")))) {
      add(source, "transformer", "Missing transformer for a source with configured destinations.")
    }
    method <- source$method %||% ""
    if (!dina_source_integration_none(source) && identical(method, "script") && !length(dina_source_fetcher_values(source))) {
      add(source, "fetcher", "Script source has no fetcher/downloader.")
    }
    if (!dina_source_integration_none(source) && method %in% c("url", "zip") && !nzchar(dina_source_direct_fetch_url(source)) && !length(dina_source_fetcher_values(source))) {
      add(source, "fetcher", "Direct source has no URL or fetcher/downloader.")
    }
  }
  if (!length(rows)) {
    return(data.frame(source_id = character(), family = character(), field = character(), warning = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

dina_source_id_values <- function(source) {
  unique(c(dina_source_field(source, "id", ""), dina_source_values(dina_source_field(source, "aliases"))))
}

dina_source_id_matches <- function(source, id) {
  dina_source_norm(id) %in% dina_source_norm(dina_source_id_values(source))
}

dina_source_by_id <- function(id, root = dina_repo_root()) {
  registry <- dina_sources(root)$sources
  matches <- registry[vapply(registry, dina_source_id_matches, logical(1), id = id)]
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

dina_config_value_missing <- function(value) {
  is.null(value) ||
    length(value) == 0L ||
    (is.character(value) && all(!nzchar(trimws(value))))
}

dina_export_validation_config <- function(config) {
  export <- config$export_validation %||% list()
  required <- c("unit", "steps", "last_year", "previous_update_file")
  # An explicitly unselected baseline is valid runtime configuration for other
  # tasks. The export itself requires it; missing schema keys still fail here.
  missing <- required[vapply(required, function(key) {
    if (key %in% c("previous_update_file", "previous_update_date") && identical(export[[key]], "")) FALSE else dina_config_value_missing(export[[key]])
  }, logical(1))]
  if (length(missing)) {
    stop(
      "Missing required export_validation config value(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  baseline <- export$previous_update_file
  baseline_dir <- config$paths$previous_series %||% "input_data/_new/previous_series"
  baseline <- gsub("\\\\", "/", baseline)
  baseline_dir <- sub("/+$", "", gsub("\\\\", "/", baseline_dir))
  if (nzchar(baseline) && !startsWith(paste0(baseline, "/"), paste0(baseline_dir, "/"))) {
    stop("Comparison baseline must be stored in ", baseline_dir, "/.", call. = FALSE)
  }
  list(
    unit = export$unit,
    steps = dina_source_values(export$steps),
    last_year = as.integer(export$last_year),
    previous_update_date = export$previous_update_date %||% basename(export$previous_update_file),
    previous_update_file = export$previous_update_file
  )
}

dina_render_config_do <- function(config = dina_config(), path = NULL, identity = list()) {
  if (!identical(config$run$bfm_replace, FALSE)) {
    stop("run.bfm_replace must be false; only BFM noreplace is supported.", call. = FALSE)
  }
  export <- dina_export_validation_config(config)
  scope <- identity$scope %||% "unvalidated"
  fingerprint <- identity$fingerprint %||% ""
  baseline_fingerprint <- identity$baseline_fingerprint %||% ""
  validated_at <- identity$validated_at %||% ""
  lines <- c(
    "* Generated by dina. Do not edit this file by hand.",
    sprintf("global dina_config_scope \"%s\"", scope),
    sprintf("global dina_config_fingerprint \"%s\"", fingerprint),
    sprintf("global dina_baseline_fingerprint \"%s\"", baseline_fingerprint),
    sprintf("global dina_config_validated_at \"%s\"", validated_at),
    sprintf("global dina_config_countries \"%s\"", paste(config$countries %||% character(), collapse = ",")),
    sprintf("global admin_trust_regions \"%s\"", identity$admin_trust_regions %||% ""),
    sprintf("global admin_pit_outputs_manifest \"%s\"", identity$admin_pit_outputs_manifest %||% ""),
    "",
    sprintf("global all_countries %s", dina_stata_quote_list(config$countries %||% character())),
    sprintf("global first_y %s", config$years$first %||% ""),
    sprintf("global last_y %s", config$years$last %||% ""),
    "",
    sprintf("global lang \"%s\"", config$run$lang %||% "eng"),
    sprintf("global debug \"%s\"", dina_bool_stata(config$run$debug %||% FALSE)),
    "global bfm_replace \"no\"",
    "",
    sprintf("global all_units %s", dina_stata_quote_list(config$run$units %||% character())),
    sprintf("global all_steps %s", dina_stata_quote_list(config$run$steps %||% character())),
    "",
    "* Export-validation settings are centralized in the DINA config.",
    sprintf("global export_unit \"%s\"", export$unit),
    sprintf("global export_steps %s", dina_stata_quote_list(export$steps)),
    sprintf("global export_last_y %s", export$last_year),
    sprintf("global previous_update_date \"%s\"", export$previous_update_date),
    sprintf("global previous_update \"%s\"", export$previous_update_file)
  )

  if (!is.null(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, path)
  }
  lines
}

dina_write_runtime_stata_config <- function(config, identity = list()) {
  path <- tempfile("dina-runtime-config-", fileext = ".do")
  writeLines(dina_render_config_do(config, identity = identity), path)
  path
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
  number <- function(x) suppressWarnings(as.numeric(x %||% NA_real_))
  identical(isTRUE(current$exists), isTRUE(previous$exists)) &&
    identical(number(current$size), number(previous$size)) &&
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

dina_source_destinations <- function(source) {
  unique(c(
    dina_source_values(dina_source_field(source, "destination")),
    dina_source_values(dina_source_field(source, "destinations"))
  ))
}

dina_source_destination_for_incoming <- function(source, incoming_name) {
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
        basename = basename(incoming_name),
        incoming = incoming_name
      )
    )
  )
}

dina_source_inbox_patterns <- function(source) {
  unique(c(
    dina_source_values(dina_source_field(source, "inbox")),
    dina_source_values(dina_source_field(source, "inboxes"))
  ))
}

dina_source_legacy_inbox_patterns <- function(source) {
  unique(c(
    dina_source_values(dina_source_field(source, "legacy_inbox")),
    dina_source_values(dina_source_field(source, "legacy_inboxes"))
  ))
}

dina_inbox_root_rel <- function() {
  "input_data/_new"
}

dina_source_inbox_bucket_from_patterns <- function(patterns) {
  patterns <- dina_source_values(patterns)
  if (!length(patterns)) {
    return("")
  }
  inbox_root <- dina_inbox_root_rel()
  candidates <- patterns[patterns == inbox_root | startsWith(patterns, paste0(inbox_root, "/"))]
  if (!length(candidates)) {
    return("")
  }
  rel <- sub(paste0("^", inbox_root, "/?"), "", candidates)
  first <- vapply(strsplit(rel, "/", fixed = TRUE), function(parts) {
    parts <- parts[nzchar(parts)]
    if (length(parts)) parts[[1]] else ""
  }, character(1))
  first <- first[nzchar(first)]
  if (length(first)) file.path(inbox_root, sort(unique(first))[[1]]) else inbox_root
}

dina_path_has_glob <- function(path) {
  grepl("[*?]", path) || grepl("\\[", path)
}

dina_source_inbox_pattern_dir <- function(pattern) {
  pattern <- trimws(pattern %||% "")
  if (!nzchar(pattern)) {
    return("")
  }
  inbox_root <- dina_inbox_root_rel()
  if (!(pattern == inbox_root || startsWith(pattern, paste0(inbox_root, "/")))) {
    return("")
  }
  parts <- strsplit(pattern, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return("")
  }
  glob_at <- which(vapply(parts, dina_path_has_glob, logical(1)))
  if (length(glob_at)) {
    keep <- seq_len(max(1L, glob_at[[1L]] - 1L))
    return(paste(parts[keep], collapse = "/"))
  }
  last <- parts[[length(parts)]]
  if (nzchar(tools::file_ext(last)) && length(parts) > 1L) {
    return(paste(parts[-length(parts)], collapse = "/"))
  }
  paste(parts, collapse = "/")
}

dina_source_inbox_dirs_from_patterns <- function(patterns) {
  dirs <- vapply(dina_source_values(patterns), dina_source_inbox_pattern_dir, character(1))
  unique(dirs[nzchar(dirs)])
}

dina_source_inbox_dirs <- function(source) {
  dirs <- dina_source_inbox_dirs_from_patterns(dina_source_inbox_patterns(source))
  if (length(dirs)) {
    return(dirs)
  }
  bucket <- dina_source_inbox_bucket_rel(source)
  if (nzchar(bucket)) bucket else character()
}

dina_source_inbox_primary_dir <- function(source) {
  dirs <- dina_source_inbox_dirs(source)
  if (length(dirs)) dirs[[1L]] else dina_source_inbox_bucket_rel(source)
}

dina_source_inbox_bucket_rel <- function(source) {
  inbox_root <- dina_inbox_root_rel()
  explicit <- dina_source_values(dina_source_field(source, "inbox_bucket"))
  if (length(explicit) && nzchar(explicit[[1]])) {
    bucket <- explicit[[1]]
    if (!(bucket == inbox_root || startsWith(bucket, paste0(inbox_root, "/")))) {
      bucket <- file.path(inbox_root, bucket)
    }
    return(bucket)
  }
  bucket <- dina_source_inbox_bucket_from_patterns(dina_source_inbox_patterns(source))
  if (nzchar(bucket)) {
    return(bucket)
  }
  family <- source$family %||% "misc"
  if (!nzchar(family)) family <- "misc"
  file.path(inbox_root, family)
}

dina_source_inbox_pattern_labels <- function(source) {
  patterns <- dina_source_inbox_patterns(source)
  if (!length(patterns)) {
    return(character())
  }
  bucket <- dina_source_inbox_bucket_rel(source)
  labels <- sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", bucket), "/?"), "", patterns)
  labels[nzchar(labels)]
}

dina_source_is_inbox_relevant <- function(source) {
  length(dina_source_inbox_patterns(source)) > 0L ||
    length(dina_source_legacy_inbox_patterns(source)) > 0L
}

dina_source_inbox_examples <- function(source) {
  explicit <- dina_source_values(dina_source_field(source, "inbox_examples"))
  if (length(explicit)) {
    return(unique(explicit))
  }
  patterns <- dina_source_inbox_patterns(source)
  if (length(patterns)) {
    return(unique(dina_source_inbox_pattern_labels(source)))
  }
  legacy_patterns <- dina_source_legacy_inbox_patterns(source)
  if (length(legacy_patterns)) {
    return(unique(basename(legacy_patterns)))
  }
  canonical <- dina_source_values(dina_source_field(source, "canonical"))
  if (length(canonical)) {
    return(unique(basename(canonical)))
  }
  character()
}

dina_source_destination_status <- function(source) {
  destinations <- unique(c(
    dina_source_values(dina_source_field(source, "destination")),
    dina_source_values(dina_source_field(source, "destinations"))
  ))
  if (!length(destinations)) {
    return(list(status = "missing_destination", destination = ""))
  }
  if (length(destinations) > 1L) {
    return(list(status = "ambiguous_destination", destination = paste(destinations, collapse = ", ")))
  }
  list(status = "ready", destination = destinations[[1]])
}

dina_sources_inbox_registry <- function(root = dina_repo_root(), family = NULL) {
  registry <- dina_sources(root)$sources
  registry <- registry[vapply(registry, dina_source_is_inbox_relevant, logical(1))]
  if (!is.null(family) && nzchar(family)) {
    families <- dina_source_resolve_family_filter(family, root)
    registry <- registry[vapply(registry, function(source) (source$family %||% "") %in% families, logical(1))]
  }
  registry
}

dina_sources_inbox_guide_rows <- function(root = dina_repo_root(), family = NULL) {
  registry <- dina_sources_inbox_registry(root, family)
  rows <- lapply(registry, function(source) {
    dest <- dina_source_destination_status(source)
    url_entries <- dina_source_url_entries(source)
    bucket <- dina_source_inbox_bucket_rel(source)
    folders <- dina_source_inbox_dirs(source)
    examples <- dina_source_inbox_examples(source)
    patterns <- dina_source_inbox_patterns(source)
    expected <- examples
    if (!length(expected)) {
      expected <- unique(basename(patterns))
    }
    data.frame(
      family = source$family %||% "",
      bucket = bucket,
      folders = paste(folders, collapse = ", "),
      source_id = source$id %||% "",
      method = source$method %||% "",
      country = dina_source_country_summary(source, root),
      examples = paste(examples, collapse = ", "),
      expected_files = paste(expected, collapse = ", "),
      destination_status = dest$status,
      destination = dest$destination,
      url_count = nrow(url_entries),
      primary_url = if (nrow(url_entries)) url_entries$url[[1]] else "",
      url_refs = if (nrow(url_entries) == 1L) "1 url" else if (nrow(url_entries) > 1L) sprintf("%s urls", nrow(url_entries)) else "none",
      bucket_exists = dir.exists(file.path(root, bucket)),
      folder_exists = length(folders) > 0L && all(dir.exists(file.path(root, folders))),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      family = character(),
      bucket = character(),
      folders = character(),
      source_id = character(),
      method = character(),
      country = character(),
      examples = character(),
      expected_files = character(),
      destination_status = character(),
      destination = character(),
      url_count = integer(),
      primary_url = character(),
      url_refs = character(),
      bucket_exists = logical(),
      folder_exists = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

dina_copy_path_for_inbox <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(from)) {
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    items <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(items)) {
      return(file.copy(items, to, recursive = TRUE, copy.date = TRUE))
    }
    TRUE
  } else {
    file.copy(from, to, overwrite = FALSE, copy.date = TRUE)
  }
}

dina_sources_inbox_init <- function(root = dina_repo_root(), dry_run = FALSE, migrate = TRUE) {
  registry <- dina_sources_inbox_registry(root)
  bucket_rels <- sort(unique(vapply(registry, dina_source_inbox_bucket_rel, character(1))))
  folder_rels <- sort(unique(unlist(lapply(registry, dina_source_inbox_dirs), use.names = FALSE)))
  folder_rels <- folder_rels[nzchar(folder_rels)]
  bucket_rows <- lapply(bucket_rels, function(bucket) {
    path <- file.path(root, bucket)
    exists <- dir.exists(path)
    status <- if (exists) {
      "exists"
    } else if (isTRUE(dry_run)) {
      "would_create"
    } else {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      "created"
    }
    files <- if (dir.exists(path)) dina_count_files(path) else 0L
    data.frame(bucket = bucket, status = status, files = files, stringsAsFactors = FALSE)
  })
  if (!length(bucket_rows)) {
    bucket_rows <- list(data.frame(bucket = character(), status = character(), files = integer(), stringsAsFactors = FALSE))
  }
  bucket_df <- do.call(rbind, bucket_rows)
  bucket_status <- stats::setNames(bucket_df$status, bucket_df$bucket)
  folder_rows <- lapply(folder_rels, function(folder) {
    path <- file.path(root, folder)
    exists <- dir.exists(path)
    status <- if (exists) {
      "exists"
    } else if (isTRUE(dry_run)) {
      "would_create"
    } else {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      "created"
    }
    if (folder %in% names(bucket_status) && bucket_status[[folder]] %in% c("created", "would_create")) {
      status <- bucket_status[[folder]]
    }
    files <- if (dir.exists(path)) dina_count_files(path) else 0L
    data.frame(
      folder = folder,
      bucket = dina_source_inbox_bucket_from_patterns(folder),
      status = status,
      files = files,
      stringsAsFactors = FALSE
    )
  })
  if (!length(folder_rows)) {
    folder_rows <- list(data.frame(folder = character(), bucket = character(), status = character(), files = integer(), stringsAsFactors = FALSE))
  }

  migration_rows <- list()
  if (isTRUE(migrate)) {
    for (source in registry) {
      legacy_patterns <- dina_source_legacy_inbox_patterns(source)
      if (!length(legacy_patterns)) {
        next
      }
      legacy_paths <- dina_filter_inbox_paths(dina_expand_paths(legacy_patterns, root), root)
      legacy_paths <- legacy_paths[file.exists(legacy_paths)]
      if (!length(legacy_paths)) {
        next
      }
      target_dir <- dina_source_inbox_primary_dir(source)
      for (from in legacy_paths) {
        target <- file.path(root, target_dir, basename(from))
        status <- "would_copy"
        if (file.exists(target)) {
          status <- if (identical(dina_hash_path(from), dina_hash_path(target))) "already_present" else "conflict"
        } else if (!isTRUE(dry_run)) {
          ok <- dina_copy_path_for_inbox(from, target)
          status <- if (isTRUE(ok) || length(ok) && all(ok)) "copied" else "copy_failed"
        }
        migration_rows[[length(migration_rows) + 1L]] <- data.frame(
          source_id = source$id %||% "",
          from = dina_relative(from, root),
          to = dina_relative(target, root),
          status = status,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(migration_rows)) {
    migration_rows <- list(data.frame(source_id = character(), from = character(), to = character(), status = character(), stringsAsFactors = FALSE))
  }
  list(
    buckets = do.call(rbind, bucket_rows),
    folders = do.call(rbind, folder_rows),
    migrations = do.call(rbind, migration_rows),
    dry_run = isTRUE(dry_run)
  )
}

dina_sources_inbox_rows <- function(root = dina_repo_root(), source_id = NULL) {
  registry <- dina_sources(root)$sources
  if (!is.null(source_id) && nzchar(source_id)) {
    dina_source_by_id(source_id, root)
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
      dest <- dina_source_destination_for_incoming(source, basename(path))
      rows[[length(rows) + 1L]] <- data.frame(
        source_id = source$id %||% "",
        family = source$family %||% "",
        method = source$method %||% "",
        bucket = dina_source_inbox_bucket_rel(source),
        inbox = dina_relative(path, root),
        kind = if (dir.exists(path)) "dir" else "file",
        destination = dest$destination %||% "",
        destination_status = dest$status %||% "",
        transformer = paste(dina_source_values(dina_source_field(source, "transformer")), collapse = ", "),
        notes = paste(dina_source_values(dina_source_field(source, "notes")), collapse = " "),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(
      source_id = character(),
      family = character(),
      method = character(),
      bucket = character(),
      inbox = character(),
      kind = character(),
      destination = character(),
      destination_status = character(),
      transformer = character(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

dina_sources_country_sna_inbox_rows <- function(root = dina_repo_root()) {
  rows <- dina_sources_inbox_rows(root)
  rows[rows$family == "country_sna", , drop = FALSE]
}

dina_bucket_select_sources <- function(root = dina_repo_root(), family = NULL, selector = NULL, source_id = NULL) {
  registry <- dina_sources_inbox_registry(root, family = family %||% NULL)
  if (!is.null(source_id) && nzchar(source_id)) {
    source <- dina_source_by_id(source_id, root)
    registry <- registry[vapply(registry, function(item) identical(item$id %||% "", source$id %||% ""), logical(1))]
  }
  selector <- selector %||% ""
  if (!nzchar(selector)) {
    return(registry)
  }
  keep <- vapply(registry, function(source) {
    bucket <- dina_source_inbox_bucket_rel(source)
    folders <- dina_source_inbox_dirs(source)
    any(c(
      dina_source_id_values(source),
      source$family %||% "",
      bucket,
      basename(bucket),
      folders,
      basename(folders)
    ) == selector)
  }, logical(1))
  registry[keep]
}

dina_source_reference_token <- function(path) {
  path <- gsub("\\\\", "/", path)
  base <- basename(path)
  if (dina_path_has_glob(base)) {
    token <- sub("[*?\\[].*$", "", base)
    if (nzchar(token)) {
      return(token)
    }
  }
  if (nzchar(base) && !dina_path_has_glob(base)) {
    return(base)
  }
  sub("[*?\\[].*$", "", path)
}

dina_source_reference_tokens <- function(source) {
  paths <- unique(c(
    dina_source_values(dina_source_field(source, "canonical")),
    dina_source_values(dina_source_field(source, "destination"))
  ))
  paths <- paths[!grepl("\\{basename\\}", paths, fixed = TRUE)]
  tokens <- unique(vapply(paths, dina_source_reference_token, character(1)))
  tokens[nzchar(tokens)]
}

dina_task_mentions_source <- function(task, source) {
  tokens <- dina_source_reference_tokens(source)
  if (!length(tokens)) {
    return(FALSE)
  }
  inputs <- unique(c(task$inputs %||% character(), task$script %||% character()))
  if (!length(inputs)) {
    return(FALSE)
  }
  any(vapply(tokens, function(token) any(grepl(token, inputs, fixed = TRUE)), logical(1)))
}

dina_source_pipeline_users <- function(source, root = dina_repo_root()) {
  tasks <- dina_pipeline(root)$tasks
  if (!length(tasks)) {
    return(character())
  }
  ids <- vapply(tasks, function(task) task$id %||% "", character(1))
  users <- ids[vapply(tasks, dina_task_mentions_source, logical(1), source = source)]
  unique(users[nzchar(users)])
}

dina_source_code_users <- function(source, root = dina_repo_root()) {
  tokens <- dina_source_reference_tokens(source)
  if (!length(tokens)) {
    return(character())
  }
  code_root <- file.path(root, "code")
  if (!dir.exists(code_root)) {
    return(character())
  }
  files <- list.files(code_root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  files <- files[!dir.exists(files)]
  files <- files[tolower(tools::file_ext(files)) %in% c("r", "do", "ado", "py", "sh", "yml", "yaml")]
  hits <- character()
  for (file in files) {
    lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) character())
    if (!length(lines)) {
      next
    }
    if (any(vapply(tokens, function(token) any(grepl(token, lines, fixed = TRUE)), logical(1)))) {
      hits <- c(hits, dina_relative(file, root))
    }
  }
  unique(hits)
}

dina_source_user_summary <- function(source, root = dina_repo_root()) {
  transformer <- unique(basename(dina_source_values(dina_source_field(source, "transformer"))))
  tasks <- dina_source_pipeline_users(source, root)
  code <- unique(basename(dina_source_code_users(source, root)))
  parts <- character()
  if (length(transformer)) parts <- c(parts, sprintf("transformer: %s", paste(transformer, collapse = ", ")))
  if (length(tasks)) parts <- c(parts, sprintf("tasks: %s", paste(tasks, collapse = ", ")))
  if (length(code)) parts <- c(parts, sprintf("code: %s", paste(code, collapse = ", ")))
  if (!length(parts)) "not found in registered tasks/code" else paste(parts, collapse = "; ")
}

dina_buckets_uses <- function(root = dina_repo_root(), family = NULL, selector = NULL, source_id = NULL) {
  registry <- dina_bucket_select_sources(root, family = family, selector = selector, source_id = source_id)
  rows <- lapply(registry, function(source) {
    dest <- dina_source_destination_status(source)
    data.frame(
      source_id = source$id %||% "",
      family = source$family %||% "",
      bucket = dina_source_inbox_bucket_rel(source),
      folders = paste(dina_source_inbox_dirs(source), collapse = ", "),
      expected_files = paste(dina_source_inbox_examples(source), collapse = ", "),
      destination = if (identical(dest$status, "ready")) dest$destination else dest$status,
      users = dina_source_user_summary(source, root),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      source_id = character(),
      family = character(),
      bucket = character(),
      folders = character(),
      expected_files = character(),
      destination = character(),
      users = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

dina_source_fetcher_path <- function(source, root = dina_repo_root()) {
  fetcher <- dina_source_values(dina_source_field(source, "fetcher"))
  if (!length(fetcher) || !nzchar(fetcher[[1L]])) {
    return("")
  }
  if (grepl("^/", fetcher[[1L]])) fetcher[[1L]] else file.path(root, fetcher[[1L]])
}

dina_source_fetch_target_rel <- function(source, direct_url = "", for_direct = FALSE) {
  target <- dina_source_values(dina_source_field(source, "fetch_target"))
  if (length(target) && nzchar(target[[1L]])) {
    return(target[[1L]])
  }
  if (isTRUE(for_direct)) {
    bucket <- dina_source_inbox_primary_dir(source)
    url_base <- basename(strsplit(direct_url %||% "", "[?#]")[[1]][[1]] %||% "")
    if (nzchar(bucket) && nzchar(url_base) && !identical(url_base, "/") && !grepl("[/\\\\]", url_base)) {
      return(file.path(bucket, url_base))
    }
    return("")
  }
  patterns <- dina_source_inbox_patterns(source)
  non_glob <- patterns[!vapply(patterns, dina_path_has_glob, logical(1))]
  files <- non_glob[nzchar(tools::file_ext(basename(non_glob)))]
  if (length(files)) {
    return(files[[1L]])
  }
  examples <- dina_source_inbox_examples(source)
  folder <- dina_source_inbox_primary_dir(source)
  if (length(examples) && nzchar(examples[[1L]]) && nzchar(folder)) {
    return(file.path(folder, basename(examples[[1L]])))
  }
  ""
}

dina_fetch_status_without_fetcher <- function(source) {
  method <- source$method %||% "manual"
  if (method %in% c("manual", "wid")) "manual_only" else "no_fetcher"
}

dina_run_source_fetcher <- function(fetcher, target, root, source_id) {
  rscript <- file.path(R.home("bin"), "Rscript")
  args <- c(fetcher, "--source-id", source_id, "--target", target, "--repo-root", root)
  output <- tryCatch(
    system2(rscript, args = args, stdout = TRUE, stderr = TRUE),
    warning = function(w) structure(conditionMessage(w), status = 1L),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(output, "status") %||% 0L
  list(status = as.integer(status), output = paste(output, collapse = "\n"))
}

dina_source_direct_fetch_url <- function(source) {
  direct <- dina_source_values(dina_source_field(source, "url"))
  if (length(direct) && nzchar(direct[[1L]])) {
    return(direct[[1L]])
  }
  entries <- dina_source_url_entries(source)
  if (nrow(entries)) entries$url[[1L]] else ""
}

dina_download_direct_source <- function(url, target) {
  output <- tryCatch(
    utils::download.file(url, target, mode = "wb", quiet = TRUE),
    warning = function(w) structure(conditionMessage(w), status = 1L),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  status <- attr(output, "status") %||% 0L
  list(status = as.integer(status), output = paste(output, collapse = "\n"))
}

dina_buckets_fetch <- function(root = dina_repo_root(), family = NULL, selector = NULL, source_id = NULL, dry_run = FALSE) {
  registry <- dina_bucket_select_sources(root, family = family, selector = selector, source_id = source_id)
  fallback_unfetchable <- function(sources, detail) {
    rows <- lapply(sources, function(source) {
      as.data.frame(list(
        source_id = source$id %||% "",
        family = source$family %||% "",
        bucket = dina_source_inbox_bucket_rel(source),
        target = "",
        status = dina_fetch_status_without_fetcher(source),
        detail = detail,
        stringsAsFactors = FALSE
      ))
    })
    if (!length(rows)) {
      return(NULL)
    }
    do.call(rbind, rows)
  }
  if (!length(registry)) {
    if (!is.null(source_id) && nzchar(source_id)) {
      fallback <- fallback_unfetchable(
        list(dina_source_by_id(source_id, root)),
        sprintf("Use `dina sources list guide %s --urls`; no automatic fetch is configured.", source_id)
      )
      if (!is.null(fallback)) {
        return(fallback)
      }
    } else if (!is.null(family) && nzchar(family)) {
      fallback <- fallback_unfetchable(
        dina_source_registry(root, family = family),
        sprintf("Use `dina sources list guide %s --urls`; no automatic fetch is configured.", family)
      )
      if (!is.null(fallback)) {
        return(fallback)
      }
    }
  }
  rows <- lapply(registry, function(source) {
    fetcher <- dina_source_fetcher_path(source, root)
    method <- source$method %||% "manual"
    direct_url <- dina_source_direct_fetch_url(source)
    target_rel <- dina_source_fetch_target_rel(source, direct_url = direct_url, for_direct = method %in% c("url", "zip") && !nzchar(fetcher))
    target <- if (nzchar(target_rel)) file.path(root, target_rel) else ""
    base <- list(
      source_id = source$id %||% "",
      family = source$family %||% "",
      bucket = dina_source_inbox_bucket_rel(source),
      target = target_rel
    )
    inbox_root <- paste0(dina_inbox_root_rel(), "/")
    if (!nzchar(target_rel) || !(target_rel == dina_inbox_root_rel() || startsWith(target_rel, inbox_root))) {
      return(as.data.frame(c(base, list(status = "failed", detail = "Fetch target must be under input_data/_new.")), stringsAsFactors = FALSE))
    }
    if (file.exists(target)) {
      return(as.data.frame(c(base, list(status = "already_present", detail = "Target already exists.")), stringsAsFactors = FALSE))
    }
    fetchable <- nzchar(fetcher) || (method %in% c("url", "zip") && nzchar(direct_url))
    if (!isTRUE(fetchable)) {
      return(as.data.frame(c(base, list(status = dina_fetch_status_without_fetcher(source), detail = "No direct URL or fetcher configured.")), stringsAsFactors = FALSE))
    }
    if (isTRUE(dry_run)) {
      return(as.data.frame(c(base, list(status = "would_fetch", detail = "Dry-run; no file written.")), stringsAsFactors = FALSE))
    }
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (nzchar(fetcher)) {
      if (!file.exists(fetcher)) {
        return(as.data.frame(c(base, list(status = "failed", detail = sprintf("Fetcher not found: %s", dina_relative(fetcher, root)))), stringsAsFactors = FALSE))
      }
      result <- dina_run_source_fetcher(fetcher, target, root, source$id %||% "")
    } else if (method %in% c("url", "zip") && nzchar(direct_url)) {
      result <- dina_download_direct_source(direct_url, target)
    }
    if (identical(result$status, 0L) && file.exists(target)) {
      return(as.data.frame(c(base, list(status = "fetched", detail = dina_hash_path(target))), stringsAsFactors = FALSE))
    }
    detail <- result$output
    if (!nzchar(detail)) detail <- sprintf("Fetcher exited with status %s.", result$status)
    as.data.frame(c(base, list(status = "failed", detail = detail)), stringsAsFactors = FALSE)
  })
  if (!length(rows)) {
    return(data.frame(
      source_id = character(),
      family = character(),
      bucket = character(),
      target = character(),
      status = character(),
      detail = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
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

dina_session_config_path <- function(update_id, root = dina_repo_root()) {
  file.path(dina_update_dir(update_id, root), "config.override.yml")
}

dina_session_config_override_path <- function(update_id, root = dina_repo_root()) {
  dina_session_config_path(update_id, root)
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

dina_deep_merge <- function(base, override) {
  if (!is.list(base) || !is.list(override)) {
    return(override)
  }
  out <- base
  for (name in names(override)) {
    if (is.null(name) || !nzchar(name)) {
      next
    }
    if (!is.null(out[[name]]) && is.list(out[[name]]) && is.list(override[[name]]) && is.null(attr(override[[name]], "class"))) {
      out[[name]] <- dina_deep_merge(out[[name]], override[[name]])
    } else {
      out[[name]] <- override[[name]]
    }
  }
  out
}

dina_config_override <- function(session, root = dina_repo_root()) {
  if (is.null(session)) {
    return(list())
  }
  path <- dina_session_config_override_path(session$id, root)
  dina_read_yaml(path, default = list())
}

dina_session_config <- function(session, root = dina_repo_root(), expand_env = TRUE) {
  base <- dina_read_yaml(dina_config_path(root))
  if (!is.null(session)) {
    base <- dina_deep_merge(base, dina_config_override(session, root))
  }
  if (expand_env) {
    base <- dina_expand_env(base)
  }
  base
}

# A pipeline always runs in one explicit configuration scope.  An update is
# provisional: its override is authoritative for that run, but never mutates
# the benchmark configuration.
dina_config_scope <- function(session = NULL) {
  if (is.null(session)) {
    return(list(id = "benchmark", label = "Benchmark configuration", update_id = ""))
  }
  list(
    id = paste0("update:", session$id),
    label = paste("Active update", session$id),
    update_id = session$id
  )
}

dina_config_validation_receipt_path <- function(root = dina_repo_root(), session = NULL) {
  if (!is.null(session)) return(file.path(dina_update_dir(session$id, root), "config-validation.json"))
  file.path(root, "output", "experiments", "configuration_validation", "benchmark.json")
}

dina_config_validation_receipt <- function(root = dina_repo_root(), session = NULL) {
  if (!is.null(session) && length(session$config_validation %||% list())) return(session$config_validation)
  dina_read_json(dina_config_validation_receipt_path(root, session), default = NULL)
}

dina_config_override_set <- function(override, key, value) {
  dina_set_nested(override %||% list(), key, value)
}

dina_save_session_config_override <- function(session, override, root = dina_repo_root()) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  override_path <- dina_session_config_override_path(session$id, root)
  dina_write_yaml(override, override_path)
  session$config_override <- dina_relative(override_path, root)
  session$config_override_hash <- dina_hash_file(override_path)
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  session
}

dina_save_session_config <- function(session, config, root = dina_repo_root()) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  stop("Session config is stored as overrides only. Use dina_session_config_set().", call. = FALSE)
}

dina_session_config_set <- function(session, root = dina_repo_root(), key, value) {
  override <- dina_config_override(session, root)
  override <- dina_config_override_set(override, key, value)
  dina_save_session_config_override(session, override, root)
}

dina_update_extract_wid_update_date <- function(path) {
  base <- basename(path %||% "")
  match <- regexpr("[0-9]{1,2}(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[0-9]{4}", base, ignore.case = TRUE)
  if (match < 0L) {
    return("")
  }
  raw <- regmatches(base, match)
  day <- sub("^([0-9]{1,2}).*$", "\\1", raw)
  month <- sub("^[0-9]{1,2}([A-Za-z]{3})[0-9]{4}$", "\\1", raw)
  year <- sub("^[0-9]{1,2}[A-Za-z]{3}([0-9]{4})$", "\\1", raw)
  paste0(day, toupper(substr(month, 1L, 1L)), tolower(substr(month, 2L, 3L)), year)
}

dina_update_previous_update_candidates <- function(root = dina_repo_root()) {
  paths <- list.files(dina_previous_series_dir(root), pattern = "\\.dta$", full.names = TRUE, ignore.case = TRUE)
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  if (!length(paths)) {
    return(data.frame(path = character(), rel = character(), source = character(), mtime = as.POSIXct(character()), stringsAsFactors = FALSE))
  }
  info <- file.info(paths)
  rows <- data.frame(
    path = normalizePath(paths, mustWork = FALSE),
    rel = dina_relative(paths, root),
    source = "baseline",
    mtime = as.POSIXct(info$mtime, origin = "1970-01-01"),
    stringsAsFactors = FALSE
  )
  rows[order(rows$rel), , drop = FALSE]
}

dina_update_suggested_config_override <- function(root = dina_repo_root(), config = dina_config(root, expand_env = FALSE)) {
  override <- list()
  current_last <- suppressWarnings(as.integer(config$years$last %||% NA_integer_))
  if (!is.na(current_last)) {
    override <- dina_config_override_set(override, "years.last", as.character(current_last + 1L))
  }
  export_last <- suppressWarnings(as.integer(config$export_validation$last_year %||% NA_integer_))
  if (!is.na(export_last)) {
    override <- dina_config_override_set(override, "export_validation.last_year", as.character(export_last + 1L))
  }
  override$countries <- config$countries
  selected <- config$export_validation$previous_update_file
  selected_path <- if (grepl("^/", selected %||% "")) selected else file.path(root, selected %||% "")
  if (!nzchar(selected %||% "") || !file.exists(selected_path) || dir.exists(selected_path)) selected <- ""
  override <- dina_config_override_set(override, "export_validation.previous_update_file", selected)
  date <- dina_update_extract_wid_update_date(selected)
  if (!identical(selected, config$export_validation$previous_update_file) || nzchar(date)) override <- dina_config_override_set(override, "export_validation.previous_update_date", if (nzchar(date)) date else basename(selected))
  override
}

dina_write_suggested_session_config_override <- function(session_or_id, root = dina_repo_root(), overwrite = TRUE) {
  id <- if (is.list(session_or_id)) session_or_id$id else session_or_id
  path <- dina_session_config_override_path(id, root)
  if (!isTRUE(overwrite) && file.exists(path)) {
    return(path)
  }
  dina_need("yaml")
  dina_settings_write_suggestion(path, dina_update_suggested_config_override(root))
  path
}

dina_todo_items <- function(root = dina_repo_root()) {
  items <- dina_todo_config(root)$items %||% list()
  ids <- vapply(items, function(item) item$id %||% "", character(1))
  items[nzchar(ids)]
}

dina_todo_state <- function(session = NULL) {
  checked <- session$todo$checked %||% character()
  checked[!is.na(checked) & nzchar(checked)]
}

dina_todo_rows <- function(session = NULL, root = dina_repo_root()) {
  items <- dina_todo_items(root)
  checked <- dina_todo_state(session)
  if (!length(items)) {
    return(data.frame(id = character(), label = character(), checked = logical(), stringsAsFactors = FALSE))
  }
  rows <- lapply(items, function(item) {
    data.frame(
      id = item$id %||% "",
      label = item$label %||% item$note %||% "",
      checked = (item$id %||% "") %in% checked,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

dina_todo_known_ids <- function(root = dina_repo_root()) {
  rows <- dina_todo_rows(NULL, root)
  rows$id
}

dina_update_todo_state <- function(session, root = dina_repo_root(), id = NULL, checked = TRUE, reset = FALSE) {
  if (is.null(session)) {
    stop("No active update.", call. = FALSE)
  }
  known <- dina_todo_known_ids(root)
  if (isTRUE(reset)) {
    session$todo <- list(checked = character(), updated_at = dina_now())
    session$updated_at <- dina_now()
    dina_save_session(session, root)
    return(session)
  }
  id <- trimws(id %||% "")
  if (!nzchar(id) || !id %in% known) {
    stop("Unknown todo id: ", id, "\nAvailable todos: ", paste(known, collapse = ", "), call. = FALSE)
  }
  current <- dina_todo_state(session)
  if (isTRUE(checked)) {
    current <- unique(c(current, id))
  } else {
    current <- setdiff(current, id)
  }
  session$todo <- list(checked = current, updated_at = dina_now())
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  session
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
  finalized <- !is.null(existing) && existing$status %in% c("closed", "finalized")
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

dina_git_capture <- function(args, root = dina_repo_root()) {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  out <- tryCatch(
    suppressWarnings(system2("git", args, stdout = TRUE, stderr = TRUE)),
    error = function(e) {
      structure(conditionMessage(e), status = 1L)
    }
  )
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(status = status, output = paste(out, collapse = "\n"))
}

dina_repo_state_excluded <- function(rel) {
  rel <- gsub("\\\\", "/", rel)
  rel == ".git" ||
    startsWith(rel, ".git/") ||
    rel == "input_data" ||
    startsWith(rel, "input_data/") ||
    rel == "intermediary_data" ||
    startsWith(rel, "intermediary_data/") ||
    rel == "output" ||
    startsWith(rel, "output/")
}

dina_repo_state_dir <- function(session_or_id, root = dina_repo_root(), baseline = "start") {
  id <- if (is.list(session_or_id)) session_or_id$id else session_or_id
  file.path(dina_update_dir(id, root), "repo_state", baseline)
}

dina_repo_state_files_dir <- function(session_or_id, root = dina_repo_root(), baseline = "start") {
  file.path(dina_repo_state_dir(session_or_id, root, baseline), "files")
}

dina_repo_state_list_files <- function(root = dina_repo_root()) {
  paths <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  rel <- dina_relative(paths, root)
  rel <- rel[!vapply(rel, dina_repo_state_excluded, logical(1))]
  rel
}

dina_repo_state_tracked_files <- function(root = dina_repo_root()) {
  result <- dina_git_capture(c("ls-files"), root)
  if (!identical(result$status, 0L)) {
    return(character())
  }
  tracked <- result$output
  if (!nzchar(tracked)) {
    return(character())
  }
  tracked <- strsplit(tracked, "\n", fixed = TRUE)[[1]]
  tracked[nzchar(tracked)]
}

dina_repo_state_snapshot <- function(update_id, root = dina_repo_root(), baseline = "start", max_file_size = 5 * 1024^2, max_total_size = 100 * 1024^2) {
  dir <- dina_repo_state_dir(update_id, root, baseline)
  files_dir <- dina_repo_state_files_dir(update_id, root, baseline)
  dir.create(files_dir, recursive = TRUE, showWarnings = FALSE)

  branch <- dina_git_capture(c("rev-parse", "--abbrev-ref", "HEAD"), root)
  head <- dina_git_capture(c("rev-parse", "HEAD"), root)
  status <- dina_git_capture(c("status", "--short"), root)
  diff <- dina_git_capture(c("diff", "--binary"), root)
  diff_cached <- dina_git_capture(c("diff", "--cached", "--binary"), root)
  writeLines(status$output, file.path(dir, "status.txt"))
  writeLines(diff$output, file.path(dir, "diff.patch"))
  writeLines(diff_cached$output, file.path(dir, "diff_cached.patch"))

  tracked <- dina_repo_state_tracked_files(root)
  candidates <- sort(dina_repo_state_list_files(root))
  copied <- list()
  skipped <- list()
  total <- 0
  for (rel in candidates) {
    full <- file.path(root, rel)
    size <- file.info(full)$size
    reason <- ""
    if (is.na(size)) {
      reason <- "missing"
    } else if (size > max_file_size) {
      reason <- "file_too_large"
    } else if (total + size > max_total_size) {
      reason <- "total_limit"
    }
    if (nzchar(reason)) {
      skipped[[length(skipped) + 1L]] <- list(path = rel, size = ifelse(is.na(size), NA_real_, size), reason = reason)
      next
    }
    dest <- file.path(files_dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(full, dest, overwrite = TRUE, copy.date = TRUE)
    total <- total + size
    copied[[length(copied) + 1L]] <- list(
      path = rel,
      size = size,
      sha256 = dina_hash_file(full),
      tracked = rel %in% tracked
    )
  }
  metadata <- list(
    baseline = baseline,
    created_at = dina_now(),
    branch = if (identical(branch$status, 0L)) trimws(branch$output) else "unknown",
    head = if (identical(head$status, 0L)) trimws(head$output) else "unknown",
    dirty = identical(status$status, 0L) && nzchar(status$output),
    git_status = if (identical(status$status, 0L)) status$output else "",
    max_file_size = max_file_size,
    max_total_size = max_total_size,
    copied_bytes = total,
    excluded_roots = c("input_data", "intermediary_data", "output"),
    copied = copied,
    skipped = skipped
  )
  dina_write_json(metadata, file.path(dir, "metadata.json"))
  metadata
}

dina_repo_state_metadata <- function(session_or_id, root = dina_repo_root(), baseline = "start") {
  dina_read_json(file.path(dina_repo_state_dir(session_or_id, root, baseline), "metadata.json"), default = NULL)
}

dina_repo_state_summary <- function(metadata) {
  if (is.null(metadata)) {
    return(NULL)
  }
  list(
    baseline = metadata$baseline %||% "",
    created_at = metadata$created_at %||% "",
    branch = metadata$branch %||% "",
    head = metadata$head %||% "",
    dirty = isTRUE(metadata$dirty),
    copied = length(metadata$copied %||% list()),
    skipped = length(metadata$skipped %||% list())
  )
}

dina_repo_state_baselines <- function(session_or_id, root = dina_repo_root()) {
  id <- if (is.list(session_or_id)) session_or_id$id else session_or_id
  root_dir <- file.path(dina_update_dir(id, root), "repo_state")
  if (!dir.exists(root_dir)) {
    return(character())
  }
  sort(list.files(root_dir, all.files = FALSE, no.. = TRUE))
}

dina_repo_state_compare <- function(session_or_id, root = dina_repo_root(), baseline = "start") {
  metadata <- dina_repo_state_metadata(session_or_id, root, baseline)
  if (is.null(metadata)) {
    stop("Unknown repo baseline: ", baseline, call. = FALSE)
  }
  copied <- metadata$copied %||% list()
  baseline_paths <- vapply(copied, function(item) item$path %||% "", character(1))
  rows <- list()
  for (item in copied) {
    rel <- item$path %||% ""
    current <- file.path(root, rel)
    current_hash <- if (file.exists(current) && !dir.exists(current)) dina_hash_file(current) else ""
    state <- if (!file.exists(current)) {
      "deleted"
    } else if (identical(current_hash, item$sha256 %||% "")) {
      "unchanged"
    } else {
      "modified"
    }
    rows[[length(rows) + 1L]] <- data.frame(path = rel, state = state, stringsAsFactors = FALSE)
  }
  current_paths <- sort(dina_repo_state_list_files(root))
  added <- setdiff(current_paths, baseline_paths)
  if (length(added)) {
    rows <- c(rows, lapply(added, function(rel) data.frame(path = rel, state = "added", stringsAsFactors = FALSE)))
  }
  if (!length(rows)) {
    rows <- list(data.frame(path = character(), state = character(), stringsAsFactors = FALSE))
  }
  data <- do.call(rbind, rows)
  counts <- table(factor(data$state, levels = c("added", "modified", "deleted", "unchanged")))
  list(metadata = metadata, rows = data, counts = counts)
}

dina_repo_state_restore <- function(session_or_id, root = dina_repo_root(), baseline = "start", yes = FALSE) {
  comparison <- dina_repo_state_compare(session_or_id, root, baseline)
  rows <- comparison$rows[comparison$rows$state %in% c("modified", "deleted"), , drop = FALSE]
  actions <- list()
  for (i in seq_len(nrow(rows))) {
    rel <- rows$path[[i]]
    from <- file.path(dina_repo_state_files_dir(session_or_id, root, baseline), rel)
    to <- file.path(root, rel)
    actions[[length(actions) + 1L]] <- list(path = rel, status = if (isTRUE(yes)) "restored" else "would_restore")
    if (isTRUE(yes)) {
      dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
      file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
    }
  }
  added <- comparison$rows[comparison$rows$state == "added", "path", drop = TRUE]
  list(actions = actions, added_not_removed = added, dry_run = !isTRUE(yes), baseline = baseline)
}

dina_update_start <- function(year = format(Sys.Date(), "%Y"), id = NULL, root = dina_repo_root(), source_hash = TRUE, repo_snapshot = TRUE, progress = NULL) {
  if (is.null(id) || !nzchar(id)) {
    id <- dina_next_update_id(year, root)
  }
  started_at <- dina_now()
  dina_progress(progress, "Preparing update session %s for year %s.", id, year)
  dir <- dina_update_dir(id, root)
  dina_progress(progress, "Creating session scaffold directories.")
  dir.create(file.path(dir, "logs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "snapshots"), recursive = TRUE, showWarnings = FALSE)
  dina_progress(progress, "Preparing central source inbox buckets.")
  inbox_init <- dina_sources_inbox_init(root, dry_run = FALSE, migrate = FALSE)

  config_override <- dina_session_config_override_path(id, root)
  dina_progress(progress, "Writing suggested working config override.")
  dina_write_suggested_session_config_override(id, root = root, overwrite = TRUE)
  repo_state <- NULL
  if (isTRUE(repo_snapshot)) {
    dina_progress(progress, "Recording recoverable repo-state baseline.")
    repo_state <- dina_repo_state_snapshot(id, root = root, baseline = "start")
  }
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
    preferences = list(),
    config_override = dina_relative(config_override, root),
    config_override_hash = dina_hash_file(config_override),
    repo_state = list(
      baselines = if (isTRUE(repo_snapshot)) "start" else character(),
      start = dina_repo_state_summary(repo_state)
    ),
    source_baseline = list(
      created_at = started_at,
      hash_mode = source_hash_mode
    ),
    source_inbox = list(
      prepared_at = started_at,
      buckets = dina_records_from_df(inbox_init$buckets),
      folders = dina_records_from_df(inbox_init$folders)
    ),
    source_scan = source_scan,
    task_runs = list(),
    todo = list(checked = character())
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

dina_update_restart <- function(update_id = NULL, root = dina_repo_root(), yes = FALSE, repo_policy = c("preserve", "preserve_checkpoint", "replace"), progress = NULL) {
  repo_policy <- match.arg(repo_policy)
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
  inbox_state <- dina_sources_inbox_init(root, dry_run = TRUE, migrate = FALSE)
  inbox_files <- if (nrow(inbox_state$buckets)) sum(as.integer(inbox_state$buckets$files %||% 0L), na.rm = TRUE) else 0L
  result <- list(
    id = update_id,
    dir = dina_relative(dir, root),
    active = identical(update_id, active),
    year = if (is.null(session)) dina_update_year_from_id(update_id) else as.character(session$year %||% format(Sys.Date(), "%Y")),
    current_status = if (is.null(session)) "missing_manifest" else as.character(session$status %||% ""),
    log_files = dina_count_files(file.path(dir, "logs")),
    snapshot_files = dina_count_files(file.path(dir, "snapshots")),
    source_inbox = list(
      buckets = dina_records_from_df(inbox_state$buckets),
      folders = dina_records_from_df(inbox_state$folders),
      files = inbox_files,
      policy = "preserve"
    ),
    repo_baselines = dina_repo_state_baselines(update_id, root),
    repo_policy = repo_policy,
    same_id = TRUE,
    dry_run = !isTRUE(yes),
    restarted = FALSE
  )
  if (!isTRUE(yes)) {
    return(result)
  }
  repo_tmp <- NULL
  restart_snapshot <- NULL
  preserve_repo_state <- repo_policy %in% c("preserve", "preserve_checkpoint")
  if (identical(repo_policy, "preserve_checkpoint")) {
    restart_label <- paste0("restart-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    dina_progress(progress, "Recording restart repo-state snapshot %s.", restart_label)
    restart_snapshot <- dina_repo_state_snapshot(update_id, root = root, baseline = restart_label)
  }
  if (isTRUE(preserve_repo_state)) {
    repo_root <- file.path(dir, "repo_state")
    if (dir.exists(repo_root)) {
      repo_tmp <- tempfile("dina-repo-state-")
      dina_copy_tree(repo_root, repo_tmp)
    }
  }
  dina_progress(progress, "Resetting session directory %s.", result$dir)
  unlink(dir, recursive = TRUE)
  new_session <- dina_update_start(
    year = result$year,
    id = update_id,
    root = root,
    repo_snapshot = identical(repo_policy, "replace"),
    progress = progress
  )
  if (!is.null(session)) {
    checked <- intersect(dina_todo_state(session), dina_todo_known_ids(root))
    new_session$todo <- list(checked = checked)
    if (length(checked) && !is.null(session$todo$updated_at)) {
      new_session$todo$updated_at <- session$todo$updated_at
    }
  }
  if (isTRUE(preserve_repo_state) && !is.null(repo_tmp) && dir.exists(repo_tmp)) {
    unlink(file.path(dir, "repo_state"), recursive = TRUE)
    dina_copy_tree(repo_tmp, file.path(dir, "repo_state"))
  }
  baselines <- dina_repo_state_baselines(update_id, root)
  new_session$repo_state <- list(
    baselines = baselines,
    restart_policy = repo_policy,
    restart_checkpoint_saved = identical(repo_policy, "preserve_checkpoint"),
    start = dina_repo_state_summary(dina_repo_state_metadata(update_id, root, "start")),
    restart = dina_repo_state_summary(restart_snapshot)
  )
  dina_save_session(new_session, root)
  result$dry_run <- FALSE
  result$restarted <- TRUE
  result$source_inbox <- new_session$source_inbox
  result$source_inbox$policy <- "preserve"
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

dina_yes_no_response <- function(answer, default = FALSE) {
  answer <- tolower(trimws(answer %||% ""))
  if (!nzchar(answer)) {
    return(isTRUE(default))
  }
  if (answer %in% c("y", "yes")) {
    return(TRUE)
  }
  if (answer %in% c("n", "no")) {
    return(FALSE)
  }
  isTRUE(default)
}

dina_prompt_yes_no <- function(prompt, default = FALSE, input = "stdin", is_terminal = isatty(stdin())) {
  if (!isTRUE(is_terminal)) {
    return(isTRUE(default))
  }
  dina_yes_no_response(dina_read_prompt(prompt, input = input), default = default)
}

dina_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
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

dina_sources_compare <- function(session, root = dina_repo_root(), hash = "changed", deep = FALSE) {
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
    review = NULL,
    counts = dina_source_diff_counts(diff),
    diff = diff
  )
}

dina_sources_status <- function(session, root = dina_repo_root(), hash = "changed", deep = FALSE) {
  dina_sources_compare(session, root = root, hash = hash, deep = deep)
}

dina_todo_label <- function(root = dina_repo_root(), id = "") {
  id <- trimws(id %||% "")
  if (!nzchar(id)) {
    return("")
  }
  items <- dina_todo_config(root)$items %||% list()
  matches <- vapply(items, function(item) identical(item$id %||% "", id), logical(1))
  if (!any(matches)) {
    return("")
  }
  item <- items[[which(matches)[[1L]]]]
  item$label %||% item$note %||% ""
}

dina_recommendation <- function(command,
                                why = "",
                                todo_id = "",
                                todo_label = "",
                                expected_action = "",
                                next_command = "",
                                next_note = "",
                                recommendation = NULL) {
  recommendation <- recommendation %||% if (nzchar(command)) {
    sprintf("Run `%s`.", command)
  } else {
    ""
  }
  list(
    command = command,
    why = why,
    todo_id = todo_id,
    todo_label = todo_label,
    expected_action = expected_action,
    next_command = next_command,
    next_note = next_note,
    recommendation = recommendation
  )
}

dina_session_result <- function(state, proposal, stale_tasks = NA_integer_, open_todos = NULL) {
  out <- list(
    state = state,
    recommendation = proposal$recommendation %||% "",
    proposal = proposal,
    stale_tasks = stale_tasks
  )
  if (!is.null(open_todos)) {
    out$open_todos <- open_todos
  }
  out
}

dina_csv_key_value <- function(path) {
  if (!file.exists(path)) return(data.frame(key = character(), value = character(), stringsAsFactors = FALSE))
  utils::read.csv(path, stringsAsFactors = FALSE)
}

dina_manifest_value <- function(manifest, key) {
  if (!nrow(manifest) || !("key" %in% names(manifest)) || !("value" %in% names(manifest))) return("")
  hit <- manifest$key == key
  if (!any(hit)) "" else as.character(manifest$value[which(hit)[[1L]]])
}

dina_country_sna_explore_root <- function(root = dina_repo_root()) {
  file.path(root, "output", "experiments", "country_sna_explore")
}

dina_country_sna_include_root <- function(root = dina_repo_root()) {
  file.path(root, "output", "experiments", "country_sna_include")
}

dina_country_sna_inbox_signature <- function(root = dina_repo_root()) {
  rows <- dina_sources_country_sna_inbox_rows(root)
  if (!nrow(rows)) return(data.frame(rel = character(), size = numeric(), mtime = character(), stringsAsFactors = FALSE))
  rels <- if ("path" %in% names(rows)) rows$path else if ("inbox" %in% names(rows)) rows$inbox else character()
  files <- ifelse(grepl("^/", rels), rels, file.path(root, rels))
  files <- unique(files[file.exists(files)])
  info <- file.info(files)
  data.frame(
    rel = vapply(files, dina_relative, character(1), root = root),
    size = as.numeric(info$size),
    mtime = as.character(info$mtime),
    stringsAsFactors = FALSE
  )
}

dina_country_sna_matching_explore <- function(root = dina_repo_root()) {
  explore_root <- dina_country_sna_explore_root(root)
  manifest_path <- file.path(explore_root, "logs", "explore_manifest.csv")
  fingerprints_path <- file.path(explore_root, "tables", "source_fingerprints.csv")
  if (!file.exists(manifest_path) || !file.exists(fingerprints_path)) return(NULL)
  current <- dina_country_sna_inbox_signature(root)
  if (!nrow(current)) return(NULL)
  fingerprints <- utils::read.csv(fingerprints_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  fingerprints <- fingerprints[grepl("^input_data/_new/sna/", fingerprints$rel %||% ""), , drop = FALSE]
  if (!nrow(fingerprints)) return(NULL)
  key <- function(df) paste(df$rel, df$size, df$mtime, sep = "|")
  if (!all(key(current) %in% key(fingerprints))) return(NULL)
  manifest <- dina_csv_key_value(manifest_path)
  list(root = explore_root, manifest = manifest, run_id = dina_manifest_value(manifest, "run_id"))
}

dina_country_sna_include_runs <- function(root = dina_repo_root()) {
  runs_root <- file.path(dina_country_sna_include_root(root), "runs")
  if (!dir.exists(runs_root)) return(character())
  runs <- list.dirs(runs_root, full.names = TRUE, recursive = FALSE)
  runs[file.exists(file.path(runs, "logs", "include_manifest.csv"))]
}

dina_country_sna_latest_include_for_explore <- function(explore_root, root = dina_repo_root()) {
  runs <- dina_country_sna_include_runs(root)
  if (!length(runs)) return(NULL)
  matches <- lapply(runs, function(run) {
    manifest <- dina_csv_key_value(file.path(run, "logs", "include_manifest.csv"))
    prior <- normalizePath(dina_manifest_value(manifest, "exploration_run"), mustWork = FALSE)
    if (!identical(prior, normalizePath(explore_root, mustWork = FALSE))) return(NULL)
    list(root = run, manifest = manifest, status = dina_manifest_value(manifest, "status"), mtime = file.info(file.path(run, "logs", "include_manifest.csv"))$mtime)
  })
  matches <- Filter(Negate(is.null), matches)
  if (!length(matches)) return(NULL)
  matches[[order(vapply(matches, function(x) as.numeric(x$mtime), numeric(1)), decreasing = TRUE)[[1L]]]]
}

dina_country_sna_confirm_for_include <- function(include_run, root = dina_repo_root()) {
  confirms_root <- file.path(dina_country_sna_include_root(root), "confirms")
  if (!dir.exists(confirms_root)) return(NULL)
  confirms <- list.dirs(confirms_root, full.names = TRUE, recursive = FALSE)
  matches <- lapply(confirms, function(confirm) {
    manifest <- dina_csv_key_value(file.path(confirm, "logs", "confirm_manifest.csv"))
    if (!nrow(manifest)) return(NULL)
    prior <- normalizePath(dina_manifest_value(manifest, "include_run"), mustWork = FALSE)
    if (!identical(prior, normalizePath(include_run, mustWork = FALSE))) return(NULL)
    list(root = confirm, manifest = manifest, status = dina_manifest_value(manifest, "status"), mtime = file.info(file.path(confirm, "logs", "confirm_manifest.csv"))$mtime)
  })
  matches <- Filter(Negate(is.null), matches)
  if (!length(matches)) return(NULL)
  matches[[order(vapply(matches, function(x) as.numeric(x$mtime), numeric(1)), decreasing = TRUE)[[1L]]]]
}

dina_review_pointer <- function(root, family) {
  file.path(root, "output", "experiments", "source_reviews", paste0(family, ".json"))
}

dina_review_read <- function(root, family) {
  dina_read_json(dina_review_pointer(root, family), default = NULL)
}

# Pipeline products may sit beside source files in legacy directories. They are
# not source evidence and must not make an accepted review stale merely because
# a pipeline task regenerated them.
dina_review_watch_files <- function(path) {
  if (!file.exists(path)) return(character())
  files <- if (dir.exists(path)) {
    sort(list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE))
  } else path
  files <- files[!dir.exists(files)]
  generated <- "/(_clean|eff-tax-rate|gpinter_output)/|/gpinter_[^/]+\\.xlsx$|/sna_country_data/URY/cei\\.xlsx$"
  files[!grepl(generated, files, perl = TRUE)]
}

# Cheap signatures are only for guidance. Acceptance verifies full hashes.
dina_review_watch <- function(paths) {
  paths <- sort(unique(paths[!is.na(paths) & nzchar(paths)]))
  setNames(lapply(paths, function(path) {
    if (!file.exists(path)) return("absent")
    files <- dina_review_watch_files(path)
    info <- file.info(files)
    dina_need("digest")
    digest::digest(paste(files, info$size, as.numeric(info$mtime), collapse = "\n"), algo = "sha256")
  }), paths)
}

# A completed inclusion is an acceptance of source data, not an acceptance of
# the CLI implementation or its runtime configuration.  Those are separately
# validated before a pipeline run.  In particular, adding a Stata preflight or
# changing an update setting must never tell a reviewer to re-accept unchanged
# source files.
dina_review_acceptance_watch_paths <- function(paths) {
  paths <- as.character(paths %||% character())
  paths[!grepl("(^|/)(code|config|output|intermediary_data)(/|$)|config\\.override\\.yml$", paths, perl = TRUE)]
}

dina_review_included_watch_raw_changes <- function(record) {
  stored <- record$acceptance_watch %||% record$watch
  paths <- dina_review_acceptance_watch_paths(names(stored))
  if (!length(paths)) return("review evidence was not recorded")
  current <- dina_review_watch(paths)
  paths[!vapply(paths, function(path) identical(current[[path]], stored[[path]]), logical(1))]
}

# Older review records used a broader watcher and, in some cases, saved its
# signature just before the inclusion copy finished.  A differing legacy
# signature is not evidence of a later user edit.  Only call a review stale
# when a watched source was actually changed after it was included.
dina_review_paths_changed_after_inclusion <- function(paths, included_at) {
  if (!length(paths) || is.null(included_at) || !nzchar(included_at)) return(character())
  included <- as.POSIXct(included_at, format = "%Y-%m-%dT%H:%M:%S%z")
  if (is.na(included)) return(character())
  changed <- vapply(paths, function(path) {
    if (!file.exists(path)) return(TRUE)
    files <- dina_review_watch_files(path)
    if (!length(files)) return(FALSE)
    changed_at <- file.info(files)$ctime
    # `included_at` is persisted to whole seconds while a just-promoted file
    # retains sub-second filesystem time.  A small grace window prevents that
    # same inclusion from immediately invalidating itself.
    any(!is.na(changed_at) & changed_at > (included + 2))
  }, logical(1))
  paths[changed]
}

dina_review_included_watch_changes <- function(record) {
  changed <- dina_review_included_watch_raw_changes(record)
  raw_watch_missing <- identical(changed, "review evidence was not recorded")
  if (raw_watch_missing) changed <- character()
  # When a legacy acceptance snapshot disagrees, retain it only where the
  # filesystem shows a real edit after inclusion.  This makes the migration
  # safe: a subsequent source edit still produces a recheck request.
  changed <- if (length(changed)) intersect(changed, dina_review_paths_changed_after_inclusion(changed, record$included_at %||% record$reviewed_at %||% "")) else character()
  if (identical(record$family, "admin")) {
    # Newer Admin inclusions have an exact clean-output handoff manifest.  It
    # is more precise than the old broad country-directory watcher: generated
    # files inside a legacy country directory must not make an accepted source
    # look stale.  Continue watching the incoming inbox, and treat any
    # included-artifact manifest mismatch as a real recheck condition.
    session <- tryCatch(dina_load_session(), error = function(e) NULL)
    manifest_issue <- tryCatch(dina_admin_pit_outputs_manifest_problem(session = session), error = function(e) "")
    if (!length(manifest_issue)) {
      changed <- changed[grepl("/input_data/_new/admin(?:/|$)", changed, perl = TRUE)]
    } else if (file.exists(dina_admin_pit_outputs_manifest_path(session = session))) {
      changed <- unique(c(changed, "included Admin PIT artifact changed"))
    }
  }
  # Trust settings are Admin source-review evidence.  They are intentionally
  # excluded from generic source watches (which omit config/output paths), so
  # track the accepted registry and its scoped snapshot explicitly here.
  if (identical(record$family, "admin") && length(record$admin_trust_watch %||% list())) {
    trust_paths <- names(record$admin_trust_watch)
    trust_current <- dina_review_watch(trust_paths)
    trust_changed <- trust_paths[!vapply(trust_paths, function(path) identical(trust_current[[path]], record$admin_trust_watch[[path]]), logical(1))]
    if (length(trust_changed)) {
      trust_changed <- intersect(trust_changed, dina_review_paths_changed_after_inclusion(trust_changed, record$included_at %||% record$reviewed_at %||% ""))
      changed <- unique(c(changed, trust_changed))
    }
  }
  if (raw_watch_missing && !length(changed) && !length(record$admin_trust_watch %||% list())) return("review evidence was not recorded")
  changed
}

dina_review_included_watch_changed <- function(record) {
  length(dina_review_included_watch_changes(record)) > 0L
}

dina_review_included_stale_reason <- function(record) {
  changed <- dina_review_included_watch_changes(record)
  if (!length(changed)) return("The accepted review remains current.")
  if (identical(changed, "review evidence was not recorded")) {
    return("The accepted review predates tracked evidence. Re-explore once to record its current scope.")
  }
  configuration <- grepl("/(config/|config\\.override\\.yml$)", changed)
  incoming <- grepl("/input_data/_new/", changed, fixed = FALSE)
  source <- !configuration & !incoming
  parts <- c(
    if (any(configuration)) "the active update configuration changed",
    if (any(incoming)) "incoming source files changed",
    if (any(source)) "source files changed"
  )
  paste0("The accepted review is retained, but ", paste(parts, collapse = " and "), " after inclusion.")
}

dina_review_family_status <- function(root, family, record = dina_review_read(root, family)) {
  incoming <- list.files(file.path(root, "input_data", "_new", family), recursive = TRUE, all.files = TRUE, no.. = TRUE)
  answer <- function(code, label, reason, action = "explore") list(code = code, label = label, reason = reason, action = action, record = record)
  if (is.null(record)) {
    if (length(incoming)) return(answer("unexplored", "Not explored", "Local incoming files are available."))
    return(answer("empty", "No incoming files", "No local candidates; producer availability is unknown.", "list"))
  }
  if (record$status %in% c("including", "inclusion_failed")) return(answer("failed", "Inclusion incomplete", "Inspect the inclusion report and backup.", "table"))
  if (record$status == "incomplete" || is.null(record$run)) return(answer("attention", "Needs attention", "The last exploration did not complete."))
  changed <- is.null(record$watch) || !identical(dina_review_watch(names(record$watch)), record$watch)
  if (record$status == "included") changed <- dina_review_included_watch_changed(record)
  if (record$status == "included" && changed) {
    return(answer("included_stale", "Included · recheck needed",
      dina_review_included_stale_reason(record), "explore"))
  }
  if (changed || record$status == "restored") return(answer("stale", "Explore again", "Relevant evidence changed since the saved review."))
  if (record$status == "included") return(answer("included", paste("Included ·", substr(record$included_at %||% record$reviewed_at, 1, 10)), "Saved candidate accepted; pipeline status is separate.", "table"))
  if (record$status == "nothing_to_include") return(answer("nothing", "Nothing to include", "The engine found no eligible changes.", "table"))
  if (record$status == "all_good") return(answer("ready", "Review available", "Prepared evidence is ready for your decision.", "include"))
  answer("attention", "Needs attention", "The saved review has unresolved engine conditions.", "table")
}

dina_review_recommendation <- function(root) {
  families <- c("sna", "admin", "surveys", "wid")
  states <- lapply(families, function(family) dina_review_family_status(root, family))
  times <- vapply(states, function(x) x$record$reviewed_at %||% "", character(1))
  for (i in order(times, decreasing = TRUE)) {
    state <- states[[i]]; family <- families[[i]]
    if (state$code %in% c("empty", "included", "nothing")) next
    command <- paste("dina sources", state$action, family)
    return(dina_session_result(if (state$code == "ready") "sources_include_ready" else "sources_pending",
      dina_recommendation(command = command, why = state$reason,
        expected_action = "Inspect the saved family evidence before acceptance.",
        next_command = if (state$code == "ready") "dina sources" else paste("dina sources explore", family),
        next_note = "Continue one family at a time.", recommendation = command)))
  }
  NULL
}

dina_session_state <- function(session, root = dina_repo_root(), inspect_pipeline = TRUE) {
  if (is.null(session)) {
    return(dina_session_result(
      "no_active_update",
      dina_recommendation(
        command = "dina update start YEAR",
        why = "No active update workspace is available.",
        expected_action = "Create an active update workspace, source baseline, todo state, and inbox buckets.",
        next_command = "dina update status",
        next_note = "Inspect the new workspace and follow the concrete recommendation.",
        recommendation = "Start an update with `dina update start YEAR`."
      )
    ))
  }
  failed <- names(session$task_runs)[vapply(session$task_runs, function(x) identical(x$status, "failed"), logical(1))]
  if (length(failed)) {
    task <- failed[[length(failed)]]
    return(dina_session_result(
      "failed",
      dina_recommendation(
        command = sprintf("dina run why %s", task),
        why = sprintf("Task %s failed in this update session.", task),
        todo_id = "run-pipeline",
        todo_label = dina_todo_label(root, "run-pipeline"),
        expected_action = "Read the failure reason, then preview or rerun the task deliberately.",
        next_command = sprintf("dina run %s --dry-run", task),
        next_note = "If the preview looks right, rerun without --dry-run.",
        recommendation = sprintf("Inspect failed task with `dina run why %s`, then retry deliberately.", task)
      )
    ))
  }
  source_state <- dina_review_recommendation(root)
  if (!is.null(source_state)) return(source_state)

  if (!inspect_pipeline) return(dina_session_result("pipeline_uninspected",
    dina_recommendation(command = "dina run list",
      why = "Open Pipeline to inspect declared output freshness.",
      expected_action = "Inspect task records and files without running tasks.",
      recommendation = "Inspect Pipeline with `dina run list`.")))

  task_status <- dina_all_task_status(root = root, session = session)
  stale <- sum(vapply(task_status, function(x) x$status %in% c("missing_outputs", "stale", "upstream_stale", "missing_inputs"), logical(1)))
  if (stale > 0) {
    return(dina_session_result(
      "build_ready",
      dina_recommendation(
        command = "dina run stale --dry-run",
        why = sprintf("%s active task%s %s stale, missing outputs, or waiting on changed inputs.", stale, if (stale == 1L) "" else "s", if (stale == 1L) "is" else "are"),
        todo_id = "run-pipeline",
        todo_label = dina_todo_label(root, "run-pipeline"),
        expected_action = "Preview the stale task run before changing outputs.",
        next_command = "dina run stale",
        next_note = "Run without --dry-run after reviewing the preview.",
        recommendation = "Preview stale tasks with `dina run stale --dry-run`."
      ),
      stale_tasks = stale
    ))
  }
  todos <- dina_todo_rows(session, root)
  open_todos <- if (nrow(todos)) sum(!todos$checked) else 0L
  if (open_todos > 0L) {
    open_rows <- todos[!todos$checked, , drop = FALSE]
    todo_id <- if (nrow(open_rows)) open_rows$id[[1L]] else ""
    todo_label <- if (nrow(open_rows)) open_rows$label[[1L]] else ""
    return(dina_session_result(
      "todo_pending",
      dina_recommendation(
        command = "dina todo",
        why = sprintf("%s helper todo item%s still unchecked.", open_todos, if (open_todos == 1L) " is" else "s are"),
        todo_id = todo_id,
        todo_label = todo_label,
        expected_action = "Review unchecked reminders and mark completed items.",
        next_command = "dina todo check ID",
        next_note = "When the checklist is clear, preview the close report.",
        recommendation = "Review the helper checklist with `dina todo`."
      ),
      stale_tasks = stale,
      open_todos = open_todos
    ))
  }
  dina_session_result(
    "review_ready",
    dina_recommendation(
      command = "dina update close --dry-run",
      why = "No failed tasks, stale tasks, or unchecked todo items were found.",
      expected_action = "Preview closure notes before writing the final close report.",
      next_command = "dina update close",
      next_note = "Run the final close command after the dry-run report looks right.",
      recommendation = "Preview closure notes with `dina update close --dry-run`."
    ),
    stale_tasks = stale,
    open_todos = open_todos
  )
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

dina_task_effective_inputs <- function(task, root = dina_repo_root(), session = NULL) {
  inputs <- as.character(task$inputs %||% character())
  if ((task$id %||% "") %in% c("02d5-interpolate-admin-data", "02e-format-for-bfm")) {
    clean <- c(SLV = "total-SLV.xlsx", PER = "total-PER.xlsx", DOM = "total-pos-DOM.xlsx",
               BRA = "total-pre-BRA.xlsx", CHL = "total-pos-CHL.xlsx",
               COL = "total-pos-COL.xlsx", ECU = "total-pos-ECU.xlsx")
    output <- c(SLV = "total-pos-SLV.xlsx", PER = "total-pos-PER.xlsx", DOM = "total-pos-DOM.xlsx",
                BRA = "total-pre-BRA.xlsx", CHL = "total-pos-CHL.xlsx",
                COL = "total-pos-COL.xlsx", ECU = "total-pos-ECU.xlsx")
    selected <- toupper(as.character(dina_session_config(session, root, expand_env = FALSE)$countries %||% character()))
    method <- dina_read_yaml(file.path(root, "config", "admin_gpinter.yml"))
    excluded <- toupper(as.character(unlist(method$excluded_countries %||% character(), use.names = FALSE)))
    countries <- setdiff(intersect(names(clean), selected), excluded)
    if (identical(task$id, "02d5-interpolate-admin-data")) {
      inputs <- c(inputs, file.path("input_data", "admin_data", countries, "_clean", unname(clean[countries])))
    } else {
      inputs <- c(inputs, file.path("input_data", "admin_data", countries,
                                    "gpinter_output", unname(output[countries])))
    }
    if (identical(task$id, "02d5-interpolate-admin-data") && "BRA" %in% countries) {
      # The retained BRA 2000/2002/2006 _clean sheets have zero average
      # placeholders, so interpolation reads the publisher ptot workbooks.
      years <- dina_session_config(session, root, expand_env = FALSE)$years
      fallback_years <- c(2000L, 2002L, 2006L)
      fallback_years <- fallback_years[fallback_years >= as.integer(years$first) &
                                         fallback_years <= as.integer(years$last)]
      inputs <- c(inputs, file.path("input_data", "admin_data", "BRA", sprintf("ptot_%s.xlsx", fallback_years)))
    }
  }
  unique(inputs)
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

  config_inputs <- c("config/dina.yml")
  if (!is.null(session)) {
    override <- dina_session_config_override_path(session$id, root)
    if (file.exists(override)) {
      config_inputs <- c(config_inputs, dina_relative(override, root))
    }
  }
  inputs <- unique(c(dina_task_effective_inputs(task, root, session), task$script %||% character(), config_inputs))
  outputs <- task$outputs %||% character()
  trust_snapshot <- if (dina_task_requires_admin_trust_snapshot(task)) {
    dina_relative(dina_admin_trust_snapshot_path(root, session), root)
  } else character()
  freshness_inputs <- unique(c(inputs, trust_snapshot))

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
  trust_issue <- dina_task_admin_source_issues(task, root, session)
  if (length(trust_issue)) {
    status <- if (identical(status, "current")) "missing_inputs" else status
    reasons <- c(reasons, trust_issue)
  }
  if (!length(existing_outputs)) {
    status <- if (identical(status, "current")) "missing_outputs" else status
    reasons <- c(reasons, "No declared outputs exist.")
  }

  latest_input <- dina_latest_mtime(freshness_inputs, root, ignore = TRUE, recursive_dirs = "auto")
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

# These are the files that must be present before a task can safely start.
# Output freshness is deliberately not part of this check: it is a preflight
# for avoiding partial pipelines, not a reason to skip a requested task.
dina_task_missing_inputs <- function(task, root = dina_repo_root(), session = NULL) {
  config_inputs <- "config/dina.yml"
  if (!is.null(session)) {
    override <- dina_session_config_override_path(session$id, root)
    if (file.exists(override)) config_inputs <- c(config_inputs, dina_relative(override, root))
  }
  inputs <- unique(c(dina_task_effective_inputs(task, root, session), task$script %||% character(), config_inputs))
  paths <- dina_filter_ignored_paths(dina_expand_paths(inputs, root), root)
  paths[!file.exists(paths)]
}

dina_task_requires_admin_trust_snapshot <- function(task) {
  identical(task$id %||% "", "03a-run-bfm") ||
    identical(task$id %||% "", "03b-run-bfm-2stage") ||
    identical(task$id %||% "", "03d-prepare-theta-extrapolation")
}

dina_task_requires_admin_pit_outputs <- function(task) {
  # 02d is deliberately a validation-only boundary.  The later tasks are
  # listed too so running a downstream slice cannot bypass the same included
  # artifact check.
  (task$id %||% "") %in% c(
    "02d-prepare-updated-admin-data", "02d5-interpolate-admin-data", "02e-format-for-bfm",
    "03a-run-bfm", "03b-run-bfm-2stage", "03d-prepare-theta-extrapolation",
    "04a-compute-effective-rates"
  )
}

dina_task_admin_source_issues <- function(task, root = dina_repo_root(), session = NULL) {
  issues <- character()
  if (dina_task_requires_admin_pit_outputs(task)) {
    issue <- dina_admin_pit_outputs_manifest_problem(root, session)
    if (length(issue)) issues <- c(issues, issue)
  }
  if (dina_task_requires_admin_trust_snapshot(task)) {
    config <- dina_session_config(session, root, expand_env = FALSE)
    issue <- dina_admin_trust_snapshot_problem(root, session, config)
    if (length(issue)) {
      migration <- dina_admin_trust_snapshot_backfill_status(root, session, config)
      if (isTRUE(migration$eligible)) {
        issues <- c(issues, "Admin sources are included; the runtime trust snapshot will be prepared when the pipeline starts.")
      } else {
        issues <- c(issues, issue)
      }
    }
  }
  unique(issues)
}

dina_legacy_prepared_input_issues <- function(root = dina_repo_root()) {
  sources <- dina_sources(root)$sources %||% list()
  legacy <- Filter(function(source) identical(source$family %||% "", "legacy_prepared"), sources)
  if (!length(legacy)) return(character())
  unlist(lapply(legacy, function(source) {
    canonical <- as.character(source$canonical %||% character())
    checksums <- unlist(source$checksums %||% list(), use.names = TRUE)
    bad <- vapply(canonical, function(rel) {
      path <- file.path(root, rel)
      expected <- checksums[[basename(rel)]] %||% ""
      !file.exists(path) || (nzchar(expected) && !identical(dina_hash_file(path), expected))
    }, logical(1))
    if (!any(bad)) return(character())
    years <- regmatches(basename(canonical[bad]), gregexpr("[0-9]{4}", basename(canonical[bad]), perl = TRUE))
    years <- sort(unique(unlist(years, use.names = FALSE)))
    sprintf("legacy prepared %s PIT input(s) missing or altered for %s; recover the recorded %s workbooks before running", source$country %||% "", paste(years, collapse = ","), source$country %||% "")
  }), use.names = FALSE)
}

dina_pipeline_input_preflight <- function(tasks, root = dina_repo_root(), session = NULL) {
  # Compatibility migration for an Admin review that was already accepted
  # before scoped trust snapshots existed.  This writes only a deterministic
  # runtime derivative of that accepted decision; it never promotes sources or
  # alters configuration validation.
  admin_snapshot <- list(created = FALSE)
  if (any(vapply(tasks, dina_task_requires_admin_trust_snapshot, logical(1)))) {
    config <- dina_session_config(session, root, expand_env = FALSE)
    admin_snapshot <- dina_admin_trust_snapshot_ensure(root, session, config)
  }
  admin_pit_outputs <- list(created = FALSE)
  if (any(vapply(tasks, dina_task_requires_admin_pit_outputs, logical(1)))) {
    admin_pit_outputs <- dina_admin_pit_outputs_manifest_ensure(root, session)
  }
  # An input produced by an earlier selected task is expected to be absent at
  # the start of a full run.  Do not confuse that ordinary dependency with a
  # missing external/static input.
  declared_outputs <- unlist(lapply(tasks, function(task) task$outputs %||% character()), use.names = FALSE)
  output_matches <- function(path, declared) {
    rel <- dina_relative(path, root)
    any(vapply(declared, function(output) {
      grepl(utils::glob2rx(output), rel, perl = TRUE)
    }, logical(1)))
  }
  missing <- unlist(lapply(tasks, function(task) {
    paths <- dina_task_missing_inputs(task, root = root, session = session)
    paths <- paths[!vapply(paths, output_matches, logical(1), declared = declared_outputs)]
    issues <- c(
      if (length(paths)) paste0("missing input: ", dina_relative(paths, root)) else character(),
      dina_task_admin_source_issues(task, root, session)
    )
    if (!length(issues)) return(character())
    paste0(task$id, " :: ", issues)
  }), use.names = FALSE)
  # The included Admin PIT handoff is shared by several downstream tasks.
  # Report it once, with its consumers, rather than repeating the same opaque
  # rejection for every task in the chain.
  admin_pit_issue <- dina_admin_pit_outputs_manifest_problem(root, session)
  if (length(admin_pit_issue)) {
    admin_tasks <- vapply(tasks, dina_task_requires_admin_pit_outputs, logical(1))
    admin_task_ids <- vapply(tasks[admin_tasks], function(task) task$id, character(1))
    if (length(admin_task_ids)) {
      shared_pattern <- " :: .*Admin PIT (outputs|handoff|artifact)"
      missing <- missing[!grepl(shared_pattern, missing, perl = TRUE)]
      missing <- c(missing, sprintf("Admin PIT handoff (required by %s) :: %s", paste(admin_task_ids, collapse = ", "), admin_pit_issue))
    }
  }
  missing <- c(missing, dina_legacy_prepared_input_issues(root))
  if (!length(missing)) return(invisible(list(admin_trust_snapshot = admin_snapshot, admin_pit_outputs = admin_pit_outputs)))
  stop(
    "Pipeline has not started because required inputs are missing:\n  - ",
    paste(missing, collapse = "\n  - "),
    "\nResolve these inputs, then run the same command again. Legacy prepared inputs are restored from the documented pre-fresh-start snapshot; no task process was launched.",
    call. = FALSE
  )
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

dina_source_review_advisory <- function(root = dina_repo_root()) {
  families <- c("wid", "surveys", "admin", "sna")
  if (!exists("dina_review_family_status", mode = "function")) {
    return(lapply(families, function(family) list(family = family, status = "unavailable", label = "Unavailable", detail = "Source-review interface was not loaded.")))
  }
  rows <- lapply(families, function(family) {
    state <- tryCatch(dina_review_family_status(root, family), error = function(e) list(code = "unknown", label = "Unavailable", reason = conditionMessage(e)))
    list(family = family, status = state$code %||% "unknown", label = state$label %||% "Unavailable", detail = state$reason %||% "")
  })
  rows
}

dina_assert_current_config_validation <- function(root, session) {
  state <- dina_config_validation_state(root, session)
  if (!identical(state$code, "validated")) {
    stop("Configuration is ", tolower(state$label), ". ", state$detail, " Run ", state$action, " before starting pipeline tasks.", call. = FALSE)
  }
  list(state = state, receipt = dina_config_validation_receipt(root, session), identity = dina_config_validation_identity(root, session, dina_config_validation_receipt(root, session)))
}

dina_task_failure_detail <- function(result, root = dina_repo_root()) {
  if (nzchar(result$stata_log %||% "")) {
    path <- file.path(root, result$stata_log)
    if (file.exists(path)) {
      lines <- trimws(readLines(path, warn = FALSE))
      codes <- grep("^r\\([1-9][0-9]*\\);$", lines)
      if (length(codes)) {
        index <- codes[[1L]]
        prior <- if (index > 1L) lines[seq.int(max(1L, index - 12L), index - 1L)] else character()
        prior <- prior[nzchar(prior) & !grepl("^(\\.|>|end of do-file|[-=]+$)", prior)]
        detail <- if (length(prior)) tail(prior, 1L) else "Stata stopped"
        return(sprintf("%s %s", detail, sub(";$", "", lines[[index]])))
      }
    }
  }
  if (nzchar(result$log_dir %||% "") && nzchar(result$task %||% "")) {
    path <- file.path(root, result$log_dir, paste0(result$task, ".err.log"))
    if (file.exists(path)) {
      lines <- trimws(readLines(path, warn = FALSE))
      errors <- lines[grepl("^Error", lines)]
      if (length(errors)) return(tail(errors, 1L))
      lines <- lines[nzchar(lines) & lines != "Execution halted"]
      if (length(lines)) return(tail(lines, 1L))
    }
  }
  sprintf("Command exited with status %s", result$exit_status %||% "unknown")
}

dina_notification_excerpt <- function(message, max_chars = 480L) {
  lines <- trimws(strsplit(as.character(message %||% ""), "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines)]
  if (length(lines) > 1L && grepl("^Pipeline has not started", lines[[1L]])) {
    lines <- lines[-1L]
  }
  if (!length(lines)) return("No error detail was recorded.")
  value <- gsub("[[:space:]]+", " ", sub("^-[[:space:]]*", "", lines[[1L]]))
  if (nchar(value) > max_chars) paste0(substr(value, 1L, max_chars - 1L), "…") else value
}

dina_run_notification_message <- function(completed, results, failure = NULL) {
  if (isTRUE(completed)) {
    statuses <- vapply(results, function(x) x$status %||% "unknown", character(1))
    counts <- table(statuses)
    summary <- paste(sprintf("%s %s", as.integer(counts), names(counts)), collapse = ", ")
    last <- if (length(results)) tail(vapply(results, function(x) x$task %||% "?", character(1)), 1L) else "none"
    return(sprintf("DINA run finished: %s. Last task: %s.", summary, last))
  }
  if (is.null(failure)) return("DINA run stopped; no error detail was recorded.")
  detail <- dina_notification_excerpt(failure$detail %||% "")
  if (identical(failure$stage, "preflight")) {
    return(substr(sprintf("DINA preflight failed; no script started. Error: %s", detail), 1L, 900L))
  }
  lead <- if (identical(failure$stage, "interrupted")) "DINA interrupted at" else "DINA failed at"
  message <- sprintf("%s %s\nScript: %s\nError: %s", lead,
                     failure$task %||% "unknown task", failure$script %||% "unknown script", detail)
  if (nzchar(failure$log_dir %||% "")) message <- paste0(message, "\nLogs: ", failure$log_dir)
  substr(message, 1L, 900L)
}

dina_run_task <- function(task, root = dina_repo_root(), session = dina_load_session(root = root), dry_run = TRUE, force = FALSE,
                          progress = NULL) {
  validation <- if (isTRUE(dry_run)) {
    list(state = dina_config_validation_state(root, session), receipt = dina_config_validation_receipt(root, session), identity = dina_config_validation_identity(root, session, dina_config_validation_receipt(root, session)))
  } else {
    dina_assert_current_config_validation(root, session)
  }
  source_advisory <- dina_source_review_advisory(root)
  config <- dina_session_config(session, root)
  countries <- paste(config$countries %||% character(), collapse = ", ")
  validated_at <- validation$receipt$checked_at %||% "not validated"
  dina_progress(progress, "Configuration scope: %s; validated %s.", validation$identity$scope, validated_at)
  dina_progress(progress, "Effective configuration: %s–%s; countries: %s.", config$years$first %||% "?", config$years$last %||% "?", countries)
  dina_progress(progress, "Source reviews are advisory: %s.", paste(vapply(source_advisory, function(x) paste0(x$family, "=", x$status), character(1)), collapse = ", "))
  status <- dina_task_status(task, root = root, session = session)
  if (!force && identical(status$status, "current")) {
    return(list(task = task$id, status = "skipped", reason = "Task is current."))
  }
  stata_cmd <- Sys.getenv("DINA_STATA_CMD", unset = config$stata$command %||% "")
  if (identical(task$type, "stata")) {
    command <- c(if (nzchar(stata_cmd)) stata_cmd else "<DINA_STATA_CMD>", config$stata$batch_args %||% c("-b", "do"), task$script)
  } else if (identical(task$type, "r")) {
    # processx receives an explicit environment for pipeline tasks. Resolve
    # Rscript before that environment is installed, so the child does not
    # depend on PATH being preserved by the launcher.
    rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    command <- c(rscript, task$script)
  } else {
    command <- c(task$type %||% "unknown", task$script)
  }
  if (dry_run) {
    return(list(task = task$id, status = "dry_run", command = command, reasons = status$reasons,
      configuration = validation$identity, source_reviews = source_advisory))
  }
  if (dina_task_requires_admin_pit_outputs(task)) {
    dina_admin_pit_outputs_manifest_ensure(root, session)
  }
  missing_inputs <- dina_task_missing_inputs(task, root = root, session = session)
  source_issues <- dina_task_admin_source_issues(task, root, session)
  if (length(c(missing_inputs, source_issues))) {
    issues <- c(
      if (length(missing_inputs)) paste0("missing input: ", dina_relative(missing_inputs, root)) else character(),
      source_issues
    )
    stop("Task ", task$id, " cannot start:\n  - ",
      paste(issues, collapse = "\n  - "), call. = FALSE)
  }
  if (!nzchar(command[[1]]) || identical(command[[1]], "<DINA_STATA_CMD>")) {
    stop("No executable configured for task ", task$id, call. = FALSE)
  }
  dina_need("processx")
  run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
  log_dir <- dina_path("output", "run_logs", run_id, root = root)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  runtime_config_do <- ""
  stata_log <- ""
  wrapper_do <- ""
  if (identical(task$type, "stata")) {
    dina_progress(progress, "Writing the Stata runtime configuration.")
    runtime_config_do <- dina_write_runtime_stata_config(config, validation$identity)
    on.exit(unlink(runtime_config_do), add = TRUE)
    stata_log <- file.path(log_dir, paste0(task$id, ".stata.log"))
    wrapper_do <- file.path(log_dir, paste0(task$id, ".wrapper.do"))
    stata_path <- normalizePath(file.path(root, task$script), mustWork = FALSE)
    stata_quote <- function(path) gsub('"', "'", path, fixed = TRUE)
    writeLines(c(
      "capture log close _all",
      sprintf('log using "%s", text replace', stata_quote(stata_log)),
      sprintf('capture noisily do "%s"', stata_quote(stata_path)),
      "local dina_task_rc = _rc",
      "capture log close _all",
      "exit `dina_task_rc'"
    ), wrapper_do)
    command <- c(command[[1]], config$stata$batch_args %||% c("-b", "do"), wrapper_do)
  }
  override_path <- if (!is.null(session)) dina_session_config_override_path(session$id, root) else ""
  if (!file.exists(override_path)) {
    override_path <- ""
  }
  env <- c(
    DINA_CONFIG_DO = runtime_config_do,
    DINA_CONFIG_YML = dina_config_path(root),
    DINA_CONFIG_OVERRIDE_YML = override_path
  )
  dina_progress(progress, "Running %s. Live status will be reported while it works.", task$id)
  process <- processx::process$new(command[[1]], command[-1], wd = root, stdout = "|", stderr = "|", env = env,
    cleanup_tree = TRUE)
  started <- Sys.time()
  last_update <- started
  while (process$is_alive()) {
    Sys.sleep(1)
    now <- Sys.time()
    if (as.numeric(difftime(now, last_update, units = "secs")) >= 15) {
      elapsed <- round(as.numeric(difftime(now, started, units = "secs")))
      detail <- if (nzchar(stata_log) && file.exists(stata_log)) {
        sprintf("; Stata log %s KiB", round(file.info(stata_log)$size / 1024, 1))
      } else ""
      dina_progress(progress, "Still running %s (%ss%s).", task$id, elapsed, detail)
      last_update <- now
    }
  }
  process$wait()
  result <- list(status = process$get_exit_status(), stdout = process$read_all_output(), stderr = process$read_all_error())
  stata_error <- FALSE
  if (nzchar(stata_log) && file.exists(stata_log)) {
    log_lines <- readLines(stata_log, warn = FALSE)
    stata_error <- any(grepl("^r\\([1-9][0-9]*\\);$", trimws(log_lines)))
  }
  status_value <- if (identical(result$status, 0L) && !stata_error) "succeeded" else "failed"
  writeLines(result$stdout, file.path(log_dir, paste0(task$id, ".out.log")))
  writeLines(result$stderr, file.path(log_dir, paste0(task$id, ".err.log")))
  failure_detail <- if (identical(status_value, "failed")) dina_task_failure_detail(list(
    task = task$id, log_dir = dina_relative(log_dir, root),
    stata_log = if (nzchar(stata_log)) dina_relative(stata_log, root) else NULL,
    exit_status = result$status
  ), root) else NULL
  run_manifest <- list(task = task$id, status = status_value, command = command, run_id = run_id,
    ended_at = dina_now(), configuration = validation$identity, validation = validation$receipt,
    source_reviews = source_advisory, stata_log = if (nzchar(stata_log)) dina_relative(stata_log, root) else NULL,
    failure = failure_detail)
  dina_write_json(run_manifest, file.path(log_dir, "run-manifest.json"))
  if (!is.null(session)) {
    session$task_runs[[task$id]] <- c(list(status = status_value, command = command, run_id = run_id, ended_at = dina_now()),
      list(configuration = validation$identity, source_reviews = source_advisory))
    session$updated_at <- dina_now()
    dina_save_session(session, root)
  }
  dina_progress(progress, "%s; logs written to %s.", if (identical(status_value, "succeeded")) "Task completed" else "Task failed", dina_relative(log_dir, root))
  list(task = task$id, status = status_value, command = command, exit_status = result$status,
    log_dir = dina_relative(log_dir, root), stata_log = if (nzchar(stata_log)) dina_relative(stata_log, root) else NULL,
    failure = failure_detail, session = session)
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

dina_finalize_blockers <- function(session, root = dina_repo_root()) {
  statuses <- dina_all_task_status(root, session)
  statuses[vapply(statuses, function(x) x$status %in% c("missing_outputs", "missing_inputs", "failed", "stale"), logical(1))]
}

dina_finalize_update <- function(session, root = dina_repo_root(), force = FALSE, promote_config = FALSE) {
  if (is.null(session)) {
    stop("No active update session.", call. = FALSE)
  }
  blockers <- dina_finalize_blockers(session, root)
  if (length(blockers) && !force) {
    return(list(ok = FALSE, blockers = lapply(blockers, function(x) x$reasons)))
  }
  session_dir <- dina_update_dir(session$id, root)
  snapshot_dir <- file.path(session_dir, "snapshots", "final_outputs")
  config <- dina_session_config(session, root, expand_env = FALSE)
  if (isTRUE(promote_config)) {
    dina_write_yaml(config, dina_config_path(root))
  }
  final_repo_state <- dina_repo_state_snapshot(session$id, root = root, baseline = "final")
  for (path in config$paths$final_outputs %||% character()) {
    full <- file.path(root, path)
    if (dir.exists(full)) {
      dina_copy_tree(full, file.path(snapshot_dir, path))
    }
  }
  session$status <- "finalized"
  session$finalized_at <- dina_now()
  session$config_promoted <- isTRUE(promote_config)
  session$final_repo_state <- dina_repo_state_summary(final_repo_state)
  final_repo_diff <- tryCatch(dina_repo_state_compare(session$id, root = root, baseline = "start"), error = function(e) NULL)
  if (!is.null(final_repo_diff)) {
    diff_counts <- as.integer(final_repo_diff$counts)
    names(diff_counts) <- names(final_repo_diff$counts)
    session$final_repo_diff <- as.list(diff_counts)
  }
  session$repo_state$baselines <- dina_repo_state_baselines(session$id, root)
  session$final_hashes <- dina_hash_many(config$paths$final_outputs %||% character(), root)
  dina_save_session(session, root)
  list(ok = TRUE, snapshot_dir = dina_relative(snapshot_dir, root), config_promoted = isTRUE(promote_config))
}

dina_compress_dropbox_root <- function() {
  normalizePath(path.expand(Sys.getenv("DINA_DROPBOX_ROOT", unset = "~/Dropbox/DINA-LatAm")), mustWork = FALSE)
}

dina_compress_flag_values <- function(value) {
  if (is.null(value) || isTRUE(value) || !length(value)) {
    return(character())
  }
  value <- unlist(strsplit(as.character(value), ",", fixed = TRUE), use.names = FALSE)
  value <- trimws(value)
  unique(value[nzchar(value)])
}

dina_compress_source_type_paths <- function(type, root = dina_repo_root()) {
  normalized <- dina_source_norm(type)
  fallback <- if (identical(normalized, "admin_microdata")) {
    c("input_data/admin_data/MEX", "input_data/admin_data/URY")
  } else {
    character()
  }
  registry_paths <- character()
  families <- tryCatch(dina_source_resolve_family_filter(type, root), error = function(e) character())
  if (length(families)) {
    registry <- dina_source_registry(root, family = families)
    registry_paths <- unique(unlist(lapply(registry, function(source) {
      c(
        dina_source_values(source$canonical %||% character()),
        dina_source_values(source$destination %||% character()),
        dina_source_values(source$destinations %||% character())
      )
    }), use.names = FALSE))
  }
  paths <- unique(c(registry_paths, fallback))
  paths <- paths[nzchar(paths)]
  paths <- gsub("\\\\", "/", paths)
  paths <- sub("^\\./", "", paths)
  paths <- paths[paths == "input_data" | startsWith(paths, "input_data/")]
  paths
}

dina_compress_input_plan <- function(
    root = dina_repo_root(),
    dropbox = FALSE,
    output = NULL,
    all = FALSE,
    include = character(),
    exclude = character()) {
  operation_root <- if (isTRUE(dropbox)) dina_compress_dropbox_root() else normalizePath(root, mustWork = FALSE)
  if (isTRUE(dropbox) && !dir.exists(operation_root)) {
    stop("Dropbox mirror not found: ", operation_root, call. = FALSE)
  }
  input_root <- file.path(operation_root, "input_data")
  if (!dir.exists(input_root)) {
    stop("Input data folder not found: ", input_root, call. = FALSE)
  }
  archive <- output %||% file.path("output", "archives", paste0("input-data-", format(Sys.Date(), "%Y-%m-%d"), ".zip"))
  if (!grepl("^/", archive)) {
    archive <- file.path(operation_root, archive)
  }
  include <- dina_compress_flag_values(include)
  exclude <- dina_compress_flag_values(exclude)
  default_exclude <- if (isTRUE(all)) character() else "admin-microdata"
  exclude_types <- unique(c(default_exclude, exclude))
  exclude_types <- exclude_types[!dina_source_norm(exclude_types) %in% dina_source_norm(include)]
  excluded_paths <- unique(unlist(lapply(exclude_types, dina_compress_source_type_paths, root = operation_root), use.names = FALSE))
  excluded_paths <- excluded_paths[nzchar(excluded_paths)]
  excluded <- data.frame(
    path = excluded_paths,
    exists = file.exists(file.path(operation_root, excluded_paths)),
    stringsAsFactors = FALSE
  )
  zip_patterns <- unique(unlist(lapply(excluded_paths, function(path) {
    c(path, paste0(path, "/"), paste0(path, "/*"), paste0(path, "/**"))
  }), use.names = FALSE))
  list(
    target = "input",
    source_root = operation_root,
    included_root = "input_data",
    output = normalizePath(archive, mustWork = FALSE),
    dropbox = isTRUE(dropbox),
    excluded_types = exclude_types,
    excluded_paths = excluded,
    zip_exclude_patterns = zip_patterns
  )
}

dina_compress_input <- function(root = dina_repo_root(), dropbox = FALSE, output = NULL, all = FALSE, include = character(), exclude = character(), dry_run = FALSE) {
  plan <- dina_compress_input_plan(root = root, dropbox = dropbox, output = output, all = all, include = include, exclude = exclude)
  if (isTRUE(dry_run)) {
    plan$dry_run <- TRUE
    return(plan)
  }
  zip_cmd <- Sys.getenv("DINA_ZIP_CMD", unset = Sys.getenv("R_ZIPCMD", unset = ""))
  if (!nzchar(zip_cmd)) {
    zip_cmd <- if (file.exists("/usr/bin/zip")) "/usr/bin/zip" else "zip"
  }
  zip_available <- if (grepl("/", zip_cmd, fixed = TRUE)) file.exists(zip_cmd) else nzchar(Sys.which(zip_cmd))
  if (!zip_available) {
    stop("zip command not found. Set DINA_ZIP_CMD or install zip.", call. = FALSE)
  }
  dir.create(dirname(plan$output), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(plan$output)) {
    unlink(plan$output)
  }
  old <- setwd(plan$source_root)
  on.exit(setwd(old), add = TRUE)
  args <- c("-r9X", plan$output, plan$included_root)
  if (length(plan$zip_exclude_patterns)) {
    args <- c(args, "-x", plan$zip_exclude_patterns)
  }
  output_lines <- system2(zip_cmd, args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(output_lines, "status") %||% 0L
  plan$dry_run <- FALSE
  plan$status <- as.integer(status)
  plan$zip_output <- paste(output_lines, collapse = "\n")
  if (!identical(as.integer(status), 0L)) {
    stop("zip failed", if (nzchar(plan$zip_output)) paste0(":\n", plan$zip_output) else ".", call. = FALSE)
  }
  plan
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
  paths <- c("input_data", "output", dina_previous_series_dir_rel(root, config_raw), "config")
  path_checks <- data.frame(
    path = paths,
    exists = file.exists(file.path(root, paths)),
    writable = vapply(file.path(root, paths), function(path) {
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

  local <- dina_pushover_local_credentials(root)
  local_configured <- dina_pushover_credential_is_usable(local$token) && dina_pushover_credential_is_usable(local$user)
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
    local_file_present = local$readable,
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
    local <- dina_pushover_local_credentials(root)
    if (!dina_pushover_credential_is_usable(local$token) || !dina_pushover_credential_is_usable(local$user)) {
      stop("Pushover setup is incomplete. Add a real app token and user key to config/pushover.local.R, then run `dina notify test`.", call. = FALSE)
    }
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

# Small, read-only checks shared by update suggestions and the workspace UI.
dina_baseline_check <- function(path, root) {
  baseline_dir <- normalizePath(dina_previous_series_dir(root), mustWork = FALSE)
  baseline_dir_rel <- dina_relative(baseline_dir, root)
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) return(paste0("Select a comparison baseline in ", baseline_dir_rel, "/."))
  full <- if (grepl("^/", path)) path else file.path(root, path)
  full <- normalizePath(full, mustWork = FALSE)
  if (!startsWith(paste0(full, "/"), paste0(baseline_dir, "/"))) return(paste0("Baseline must be stored in ", baseline_dir_rel, "/."))
  if (!file.exists(full) || dir.exists(full)) return(paste("Baseline file is missing:", path))
  if (!requireNamespace("haven", quietly = TRUE)) return("Install haven to check the comparison baseline.")
  tryCatch({
    data <- haven::read_dta(full)
    required <- c("year", "iso", "p", "widcode", "value")
    missing <- setdiff(required, names(data))
    if (length(missing)) return(paste("Baseline lacks required fields:", paste(missing, collapse = ", ")))
    keys <- data[c("year", "iso", "p", "widcode")]
    if (!nrow(data) || anyNA(keys) || anyDuplicated(keys) || any(vapply(keys, function(x) any(!nzchar(trimws(as.character(x)))), logical(1)))) return("Baseline has empty, missing, or duplicate comparison keys.")
    if (!is.numeric(data$year) || !is.numeric(data$value) || any(!is.finite(data$year)) || any(data$year != floor(data$year))) return("Baseline year and value fields must be numeric, with integer years.")
    character()
  }, error = function(e) paste("Baseline is unreadable:", conditionMessage(e)))
}

dina_baseline_candidates <- function(root) {
  paths <- dina_update_previous_update_candidates(root)$path
  paths <- paths[vapply(paths, function(path) !length(dina_baseline_check(path, root)), logical(1))]
  sort(vapply(paths, dina_relative, character(1), root = root))
}

dina_config_validation_baseline_signature <- function(path, root = dina_repo_root(), hash = FALSE) {
  path <- trimws(as.character(path %||% "")[[1L]])
  if (is.na(path)) path <- ""
  full <- if (nzchar(path) && grepl("^/", path)) path else file.path(root, path)
  exists <- nzchar(path) && file.exists(full) && !dir.exists(full)
  info <- if (exists) file.info(full) else NULL
  list(
    path = path,
    exists = exists,
    size = if (exists) unname(info$size) else NA_real_,
    mtime = if (exists) format(info$mtime, "%Y-%m-%dT%H:%M:%OS%z") else NA_character_,
    sha256 = if (exists && isTRUE(hash)) dina_hash_file(full) else NA_character_
  )
}

dina_config_validation_snapshot <- function(root, session = dina_load_session(root = root), hash_baseline = FALSE) {
  override <- if (!is.null(session)) dina_session_config_override_path(session$id, root) else ""
  config <- tryCatch(dina_session_config(session, root, expand_env = FALSE), error = function(e) NULL)
  effective_config <- tryCatch(dina_session_config(session, root, expand_env = TRUE), error = function(e) NULL)
  baseline <- if (!is.null(config)) config$export_validation$previous_update_file %||% "" else ""
  dina_need("digest")
  list(
    receipt_version = 2L,
    scope = dina_config_scope(session)$id,
    benchmark_config_hash = dina_hash_file(dina_config_path(root)),
    update_override_hash = if (nzchar(override)) dina_hash_file(override) else NA_character_,
    effective_config_hash = if (!is.null(effective_config)) digest::digest(effective_config, algo = "sha256", serialize = TRUE) else NA_character_,
    baseline = dina_config_validation_baseline_signature(baseline, root, hash = hash_baseline)
  )
}

dina_config_validation_identity <- function(root, session = NULL, receipt = NULL) {
  snapshot <- dina_config_validation_snapshot(root, session, hash_baseline = FALSE)
  baseline <- receipt$baseline %||% snapshot$baseline
  baseline_fingerprint <- baseline$sha256 %||% ""
  if (!nzchar(baseline_fingerprint)) {
    baseline_fingerprint <- paste(baseline$path %||% "", baseline$size %||% "", baseline$mtime %||% "", sep = ":")
  }
  list(
    scope = snapshot$scope,
    fingerprint = snapshot$effective_config_hash %||% "",
    effective_config_fingerprint = snapshot$effective_config_hash %||% "",
    baseline_fingerprint = baseline_fingerprint,
    admin_trust_regions = dina_relative(dina_admin_trust_runtime_path(root, session), root),
    admin_pit_outputs_manifest = dina_relative(dina_admin_pit_outputs_manifest_path(root, session), root),
    validated_at = receipt$checked_at %||% ""
  )
}

dina_config_validation_state <- function(root, session = dina_load_session(root = root), check = NULL) {
  action <- if (is.null(session)) "dina config check" else "dina update config check"
  if (!is.null(check) && length(c(check$errors, check$baseline, check$runtime %||% character()))) {
    return(list(code = "needs_attention", label = "Needs attention", detail = c(check$errors, check$baseline, check$runtime %||% character())[[1L]], action = action))
  }
  receipt <- dina_config_validation_receipt(root, session)
  if (is.null(receipt) || !length(receipt)) return(list(code = "not_validated", label = "Not validated", detail = "The current settings and comparison baseline have not been validated.", action = action))
  snapshot <- dina_config_validation_snapshot(root, session, hash_baseline = FALSE)
  same_hash <- function(x, y) identical(as.character(x %||% NA_character_), as.character(y %||% NA_character_))
  same_baseline <- identical(receipt$baseline$path %||% "", snapshot$baseline$path %||% "") &&
    dina_same_cheap_signature(receipt$baseline, snapshot$baseline)
  current <- identical(receipt$scope %||% "", snapshot$scope) &&
    same_hash(receipt$benchmark_config_hash, snapshot$benchmark_config_hash) &&
    same_hash(receipt$update_override_hash, snapshot$update_override_hash) && same_baseline
  # Version-one receipts folded the Admin trust registry into their effective
  # fingerprint.  Configuration files and the comparison baseline still prove
  # their validity; ignore that retired coupling so an Admin inclusion does
  # not force users to repeat configuration validation.
  if (identical(as.integer(receipt$receipt_version %||% 1L), 2L)) {
    current <- current && same_hash(receipt$effective_config_hash, snapshot$effective_config_hash)
  }
  if (!current) return(list(code = "out_of_date", label = "Validation out of date", detail = "Settings or the comparison baseline changed after the last validation.", action = action))
  checked_at <- receipt$checked_at %||% "unknown time"
  if (identical(receipt$status, "passed")) return(list(code = "validated", label = paste("Validated ·", checked_at), detail = "Settings and comparison baseline match the recorded validation.", action = ""))
  list(code = "needs_attention", label = paste("Needs attention ·", checked_at), detail = receipt$errors[[1L]] %||% "The last configuration validation did not pass.", action = action)
}

dina_record_config_validation <- function(root, session, check, progress = NULL) {
  dina_progress(progress, "Fingerprinting the comparison baseline for the validation receipt.")
  snapshot <- dina_config_validation_snapshot(root, session, hash_baseline = TRUE)
  receipt <- c(
    list(status = if (isTRUE(check$valid)) "passed" else "failed", checked_at = dina_now(), errors = unname(c(check$errors, check$baseline, check$runtime %||% character()))),
    snapshot
  )
  dina_write_json(receipt, dina_config_validation_receipt_path(root, session))
  if (is.null(session)) return(invisible(NULL))
  session$config_validation <- receipt
  session$updated_at <- dina_now()
  dina_save_session(session, root)
  session
}

dina_stata_runtime_preflight <- function(root, session, config, progress = NULL) {
  command <- Sys.getenv("DINA_STATA_CMD", unset = config$stata$command %||% "")
  if (!nzchar(command) || identical(command, "${DINA_STATA_CMD}")) {
    return("Stata is not configured for this configuration scope.")
  }
  if (!dina_command_available(command)) {
    return(paste("Configured Stata command is not runnable:", command))
  }
  bootstrap <- file.path(root, "code", "Stata", "auxiliar", "dina_runtime_config.do")
  if (!file.exists(bootstrap)) return("DINA Stata runtime bootstrap is missing.")
  if (!requireNamespace("processx", quietly = TRUE)) return("The processx package is required for the Stata runtime preflight.")

  identity <- dina_config_validation_identity(root, session, dina_config_validation_receipt(root, session))
  runtime <- dina_write_runtime_stata_config(config, identity)
  check <- tempfile("dina-stata-preflight-", fileext = ".do")
  marker <- tempfile("dina-stata-preflight-result-", fileext = ".txt")
  batch_log <- file.path(root, sub("\\.do$", ".log", basename(check)))
  on.exit(unlink(c(runtime, check, marker, batch_log)), add = TRUE)
  commands <- unique(as.character(config$stata$required_ados %||% character()))
  stata_string <- function(x) gsub('"', "'", as.character(x), fixed = TRUE)
  expected_countries <- paste(config$countries %||% character(), collapse = ",")
  lines <- c(
    "clear all",
    sprintf("do \"%s\"", bootstrap),
    "foreach required in dina_config_scope dina_config_fingerprint dina_baseline_fingerprint dina_config_countries admin_trust_regions admin_pit_outputs_manifest all_countries first_y last_y export_unit export_steps export_last_y previous_update {",
    "  if `\"${`required'}\"' == \"\" exit 198",
    "}",
    sprintf("if \"${dina_config_scope}\" != \"%s\" exit 198", stata_string(identity$scope)),
    sprintf("if \"${dina_config_fingerprint}\" != \"%s\" exit 198", stata_string(identity$fingerprint)),
    sprintf("if \"${dina_baseline_fingerprint}\" != \"%s\" exit 198", stata_string(identity$baseline_fingerprint)),
    sprintf("if \"${dina_config_countries}\" != \"%s\" exit 198", stata_string(expected_countries)),
    sprintf("if $first_y != %s | $last_y != %s exit 198", config$years$first, config$years$last),
    sprintf("if \"${export_unit}\" != \"%s\" exit 198", stata_string(config$export_validation$unit)),
    sprintf("if $export_last_y != %s exit 198", config$export_validation$last_year),
    sprintf("if \"${previous_update}\" != \"%s\" exit 198", stata_string(config$export_validation$previous_update_file)),
    unlist(lapply(commands, function(ado) c(
      sprintf("capture which %s", ado),
      "if _rc {",
      sprintf("  di as error \"Missing required Stata ado: %s\"", ado),
      "  exit 499",
      "}"
    )), use.names = FALSE),
    sprintf("file open marker using \"%s\", write replace text", marker),
    "file write marker \"passed\" _n",
    "file close marker",
    "exit 0"
  )
  writeLines(lines, check)
  dina_progress(progress, "Checking the Stata runtime bootstrap and required ado commands.")
  result <- tryCatch(
    processx::run(command, c(config$stata$batch_args %||% c("-b", "do"), check), wd = root,
      error_on_status = FALSE, env = c(DINA_CONFIG_DO = runtime)),
    error = function(e) e
  )
  if (inherits(result, "error")) return(paste("Stata runtime preflight could not start:", conditionMessage(result)))
  marker_lines <- if (file.exists(marker)) readLines(marker, warn = FALSE) else character()
  if (length(marker_lines) && identical(trimws(marker_lines[[1L]]), "passed")) return(character())
  output <- trimws(paste(c(result$stdout %||% "", result$stderr %||% ""), collapse = "\n"))
  if (file.exists(batch_log)) output <- trimws(paste(c(output, readLines(batch_log, warn = FALSE)), collapse = "\n"))
  if (nchar(output) > 1200L) output <- substr(output, nchar(output) - 1199L, nchar(output))
  paste0("Stata runtime preflight failed (exit ", result$status, ").", if (nzchar(output)) paste("Output:", output) else "")
}

dina_settings_check <- function(root, session = dina_load_session(root = root), validate_baseline = TRUE,
                                validate_runtime = FALSE, progress = NULL) {
  tryCatch({
    dina_progress(progress, "Checking configuration settings.")
    base <- dina_config(root, expand_env = FALSE)
    override <- dina_config_override(session, root)
    config <- dina_session_config(session, root, expand_env = FALSE)
    errors <- character()
    supported <- function(value, schema, prefix = "") {
      if (!is.list(value) || is.null(names(value))) return(character())
      unknown <- setdiff(names(value), names(schema))
      out <- if (length(unknown)) paste0(prefix, unknown) else character()
      for (key in intersect(names(value), names(schema))) if (is.list(value[[key]]) && !is.null(names(value[[key]]))) out <- c(out, supported(value[[key]], schema[[key]], paste0(prefix, key, ".")))
      out
    }
    if (!is.list(override) || (length(override) && is.null(names(override)))) errors <- c(errors, "The update override must be a YAML mapping.")
    unknown <- supported(override, base)
    if (length(unknown)) errors <- c(errors, paste("Unsupported override settings:", paste(unknown, collapse = ", ")))
    countries <- unlist(config$countries)
    known <- unique(c(unlist(base$countries), vapply(dina_read_yaml(file.path(root, "config", "wid_include.yml"), default = list())$area_metadata %||% list(), function(x) x$iso3 %||% "", character(1))))
    if (!length(countries) || anyNA(countries) || any(!grepl("^[A-Z]{3}$", countries)) || any(!countries %in% known) || anyDuplicated(countries)) errors <- c(errors, "Countries must be unique supported ISO3 codes (see config/wid_include.yml).")
    integer_year <- function(x) length(x) == 1L && is.numeric(x) && is.finite(x) && x == floor(x) && x >= 1900 && x <= 2200
    if (!integer_year(config$years$first) || !integer_year(config$years$last) || config$years$first > config$years$last) errors <- c(errors, "Years must be ordered integer bounds between 1900 and 2200.")
    if (!integer_year(config$export_validation$last_year)) errors <- c(errors, "The comparison end year must be an integer between 1900 and 2200.")
    if (integer_year(config$years$first) && integer_year(config$export_validation$last_year) && config$years$first > config$export_validation$last_year) errors <- c(errors, "The comparison end year must not precede the first year.")
    units <- unlist(config$run$units)
    if (!is.character(units) || !length(units) || anyNA(units) || any(!units %in% c("ind", "esn", "pch", "act"))) errors <- c(errors, "Run units must be ind, esn, pch, or act.")
    export_unit <- config$export_validation$unit
    if (!is.character(export_unit) || length(export_unit) != 1L || !export_unit %in% c("ind", "esn", "pch", "act")) errors <- c(errors, "Choose one supported export validation unit: ind, esn, pch, or act.")
    export_steps <- unlist(config$export_validation$steps)
    if (!is.character(export_steps) || !length(export_steps) || anyNA(export_steps) || any(!nzchar(trimws(export_steps)))) errors <- c(errors, "Export validation steps must contain nonempty step names.")
    required_ados <- unlist(config$stata$required_ados %||% character())
    if (!is.character(required_ados) || anyNA(required_ados) || any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", required_ados))) {
      errors <- c(errors, "Stata required_ados must contain valid command names.")
    }
    # Admin Include may receive either Excel or binary Excel PIT workbooks.
    # Check the readers up front so the reviewer gets an actionable setup
    # error during configuration validation rather than a late cleaner crash.
    readers <- c("readxl", "readxlsb", "openxlsx", "haven")
    missing_readers <- readers[!vapply(readers, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_readers)) {
      errors <- c(errors, paste0("Required R reader package(s) missing: ", paste(missing_readers, collapse = ", "), ". Run `dina install` before Admin Explore or a pipeline run."))
    }
    if (!config$run$lang %in% c("eng", "esp")) errors <- c(errors, "run.lang must be eng or esp.")
    for (name in c("debug", "bfm_replace")) if (!is.logical(config$run[[name]]) || length(config$run[[name]]) != 1L || is.na(config$run[[name]])) errors <- c(errors, paste("run", name, "must be true or false."))
    if (isTRUE(config$run$bfm_replace)) errors <- c(errors, "run.bfm_replace must be false; only BFM noreplace is supported.")
    for (name in c("units", "steps")) if (!length(unlist(config$run[[name]])) || anyNA(unlist(config$run[[name]]))) errors <- c(errors, paste("run", name, "must contain values."))
    baseline <- character()
    if (isTRUE(validate_baseline)) {
      baseline_path <- config$export_validation$previous_update_file %||% ""
      baseline_label <- if (nzchar(baseline_path)) basename(baseline_path) else "no baseline selected"
      dina_progress(progress, "Reading comparison baseline: %s. Checking its fields and comparison keys; this may take a moment.", baseline_label)
      baseline <- dina_baseline_check(baseline_path, root)
      dina_progress(progress, if (length(baseline)) "Comparison baseline fields or keys need attention." else "Comparison baseline fields and keys are valid.")
    }
    runtime <- if (isTRUE(validate_runtime) && !length(c(errors, baseline))) {
      dina_stata_runtime_preflight(root, session, config, progress)
    } else {
      character()
    }
    list(config = config, errors = errors, baseline = baseline, runtime = runtime,
      valid = !length(c(errors, baseline, runtime)))
  }, error = function(e) {
    dina_progress(progress, "Configuration validation could not be completed.")
    list(config = list(), errors = paste("Invalid configuration:", conditionMessage(e)), baseline = character(), runtime = character(), valid = FALSE)
  })
}

dina_settings_write_suggestion <- function(path, config) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  comments <- c("# Settings for this update only; other values come from config/dina.yml.",
    "# Countries: keep the current list or add a supported ISO3 code.",
    "# Years: the suggested final years advance the benchmark by one year.",
    "# Run years and export_validation.last_year serve different steps.",
    "# Baseline: choose ONE alternative version in input_data/_new/previous_series/.",
    "# Changing the baseline requires rerunning the export to regenerate graphs.", "")
  writeLines(c(comments, yaml::as.yaml(config)), path)
}
