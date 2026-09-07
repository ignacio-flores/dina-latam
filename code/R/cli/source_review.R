# Shared interaction for the existing source engines. Extraction contracts and
# inclusion guards remain owned by each family.

dina_review_families <- function() {
  c(sna = "National accounts", admin = "Administrative tax data", surveys = "Household surveys", wid = "WID")
}

dina_review_save <- function(record, root) {
  path <- dina_review_pointer(root, record$family)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  pending <- tempfile("review-", tmpdir = dirname(path))
  on.exit(unlink(pending), add = TRUE)
  dina_write_json(record, pending)
  if (!file.rename(pending, path)) stop("Could not save the family review pointer.", call. = FALSE)
  invisible(record)
}

dina_review_engine <- function(root, family) {
  modules <- switch(family, sna = c("country_sna_explorer.R", "country_sna_include.R"),
    admin = c("admin_pit_explorer.R", "admin_pit_include.R"), surveys = "survey_sources_include.R", wid = "wid_include.R")
  env <- new.env(parent = environment(dina_main))
  for (module in modules) source(file.path(root, "code", "R", "source-diagnostics", module), local = env)
  env
}

dina_review_bind <- function(rows) {
  rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, rows)
  if (!length(rows)) return(data.frame())
  columns <- unique(unlist(lapply(rows, names)))
  do.call(rbind, lapply(rows, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- NA
    x[columns]
  }))
}

dina_review_compare <- function(current, candidate, keys, values, tolerance = function(old, new) 0) {
  if (!length(keys) || !all(keys %in% names(current)) || !all(keys %in% names(candidate))) {
    stop("Not checked: required comparison keys are missing.", call. = FALSE)
  }
  invalid <- function(x) anyNA(x[keys]) || any(vapply(x[keys], function(col) any(!nzchar(trimws(as.character(col)))), logical(1))) || anyDuplicated(x[keys]) > 0L
  if (invalid(current) || invalid(candidate)) stop("Not checked: comparison keys are missing or duplicated.", call. = FALSE)
  if (length(setdiff(values, union(names(current), names(candidate))))) stop("Not checked: declared value columns are missing.", call. = FALSE)
  values <- unique(values)
  if (!length(values)) stop("Not checked: no declared value columns are available.", call. = FALSE)
  dina_review_bind(lapply(values, function(measure) {
    for (x in list(current, candidate)) if (measure %in% names(x) && !is.numeric(x[[measure]])) stop("Not checked: nonnumeric comparison column ", measure, call. = FALSE)
    old <- current[keys]
    new <- candidate[keys]
    old$old_value <- if (measure %in% names(current)) current[[measure]] else rep(NA_real_, nrow(current))
    new$new_value <- if (measure %in% names(candidate)) candidate[[measure]] else rep(NA_real_, nrow(candidate))
    old$.old <- rep(TRUE, nrow(old))
    new$.new <- rep(TRUE, nrow(new))
    rows <- merge(old, new, by = keys, all = TRUE, sort = TRUE)
    if (!nrow(rows)) return(data.frame())
    rows$measure <- measure
    rows$difference <- rows$new_value - rows$old_value
    rows$percent <- ifelse(is.finite(rows$old_value) & rows$old_value != 0, 100 * rows$difference / abs(rows$old_value), NA_real_)
    rows$result <- "unchanged"
    overlap <- !is.na(rows$.old) & !is.na(rows$.new)
    finite <- is.finite(rows$old_value) & is.finite(rows$new_value)
    rows$result[overlap & finite & abs(rows$difference) > tolerance(rows$old_value, rows$new_value)] <- "revised"
    rows$result[overlap & is.na(rows$old_value) & !is.na(rows$new_value)] <- "value added"
    rows$result[overlap & !is.na(rows$old_value) & is.na(rows$new_value)] <- "value missing"
    rows$result[overlap & is.na(rows$old_value) & is.na(rows$new_value)] <- "absence unexplained"
    rows$result[is.na(rows$.old)] <- "new observation"
    rows$result[is.na(rows$.new)] <- "removed observation"
    nonfinite <- is.infinite(rows$new_value) | is.nan(rows$new_value) | is.infinite(rows$old_value) | is.nan(rows$old_value)
    rows$result[nonfinite] <- "nonfinite value"
    rows[c(keys, "measure", "old_value", "new_value", "difference", "percent", "result")]
  }))
}

dina_review_inputs <- function(root, family, engine) {
  contract_file <- switch(family, sna = "country_sna_include.yml", admin = "admin_pit_include.yml", surveys = "survey_population_include.yml", wid = "wid_include.yml")
  contract <- dina_read_yaml(file.path(root, "config", contract_file))
  paths <- switch(family,
    sna = c("input_data/sna_country_data", contract$base_dataset),
    surveys = c(contract$paths$canonical_surveys, contract$paths$population),
    wid = unlist(lapply(engine$wid_include_artifacts(contract), function(x) c(x$canonical, x$raw))),
    admin = {
      ids <- unique(c(unlist(contract$source_ids), unlist(contract$cleaners$auxiliary_sources)))
      sources <- Filter(function(x) x$id %in% ids, dina_sources(root)$sources)
      c(unlist(lapply(sources, function(x) c(x$canonical, dina_expand_paths(x$canonical, root)))), unlist(contract$cleaners$static_dependencies))
    })
  paths <- c(paths, contract$years$from_config)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  paths <- ifelse(grepl("^/", paths), paths, file.path(root, paths))
  # Track the directory containing a wildcard as well as its current matches:
  # a producer can introduce a newly named dependency after exploration.
  paths <- unique(vapply(paths, function(path) {
    if (grepl("[*?[]", path)) sub("/[^/]*[*?[].*$", "", path) else path
  }, character(1)))
  active <- dina_current_update(root)
  unique(c(paths, file.path(root, "input_data", "_new", family),
    file.path(root, "config", c("dina.yml", "sources.yml", contract_file, paste0(switch(family, sna = "country_sna", admin = "admin_pit", surveys = "survey_population", wid = "wid"), "_explorer.yml"))),
    dina_active_update_file(root),
    if (!is.null(active)) dina_session_config_override_path(active, root),
    file.path(root, "code", "R", "source-diagnostics"),
    file.path(root, "code", "R", "cli", c("source_review.R", "source_report.R")),
    if (identical(family, "admin")) c(file.path(root, "code", "R", c("admin_cleaners", "source-helpers")),
      file.path(root, "code", "Stata", "tax-data", "COL-diverse.do"))))
}

dina_review_hashes <- function(paths) {
  paths <- sort(unique(paths[!is.na(paths) & nzchar(paths)]))
  setNames(lapply(paths, function(path) if (file.exists(path)) dina_hash_path(path) else "absent"), paths)
}

dina_review_country <- function(rows, metadata = list()) {
  country <- rep(NA_character_, nrow(rows))
  for (field in intersect(c("review_country", "country", "iso"), names(rows))) {
    empty <- is.na(country) | !nzchar(country)
    country[empty] <- as.character(rows[[field]][empty])
  }
  for (area in metadata %||% list()) country[country %in% unlist(area[c("wid_area", "iso3", "country_name")])] <- area$iso3
  country
}

dina_review_values <- function(root, family, engine, explored, prepared) {
  if (identical(family, "sna")) {
    old <- engine$country_sna_include_extract_all(prepared$contract, root, prepared$years)
    current <- engine$country_sna_include_wide(old$values_long, prepared$contract, prepared$years)
    candidate <- prepared$outputs$values_wide
    # Extractors build a complete year grid. Remove empty baseline grid rows for
    # genuinely new years so they are not mistaken for overlapping values.
    coverage <- explored$outputs$extension_summary
    for (i in seq_len(nrow(coverage))) {
      added <- engine$country_sna_explorer_parse_year_list(coverage$extension_years[[i]])
      current <- current[!(current$country == coverage$country[[i]] & current$year %in% added), , drop = FALSE]
    }
    measures <- vapply(c(prepared$contract$variables$primitives, prepared$contract$variables$derived), function(x) x$name, character(1))
    rows <- dina_review_compare(current, candidate, c("country", "year"), measures,
      function(old, new) engine$country_sna_include_float_tolerance(new, old))
    rows <- dina_review_sna_absences(rows, prepared$outputs$include_detail, old$values_long, prepared$outputs$values_long)
    note <- "Country SNA values matched by country, year and variable using the same extraction rules. Other macro inputs are not checked by this review."
    if (any(rows$result == "value added")) note <- paste(note, "Revisions in cells with unavailable accepted values are not checked; these appear as value added.")
    return(list(rows = rows, note = note))
  }
  if (identical(family, "wid")) {
    notes <- character()
    rows <- lapply(engine$wid_include_artifacts(prepared$contract), function(artifact) {
      production <- engine$wid_include_artifact_paths(root, artifact)
      staged <- engine$wid_include_artifact_paths(root, artifact, staged_repo = prepared$paths$staged_repo)
      old <- engine$wid_include_read_dta(production$canonical)
      new <- engine$wid_include_read_dta(staged$canonical)
      if (!isTRUE(new$ok)) {
        notes <<- c(notes, paste(artifact$source_id, "not checked: candidate unavailable."))
        return(data.frame())
      }
      if (!isTRUE(old$ok) && file.exists(production$canonical)) stop("Not checked: unreadable accepted WID artifact ", artifact$source_id, call. = FALSE)
      schema <- engine$wid_include_schema(artifact)
      if (!isTRUE(old$ok)) {
        old$data <- new$data[FALSE, , drop = FALSE]
        notes <<- c(notes, paste(artifact$source_id, "not checked for revisions: no accepted baseline."))
      }
      compared <- dina_review_compare(old$data, new$data, schema$key_columns, schema$numeric_columns)
      if (nrow(compared)) {
        compared$source_id <- artifact$source_id
        compared$review_country <- dina_review_country(compared, prepared$contract$area_metadata)
      }
      compared
    })
    return(list(rows = dina_review_bind(rows), note = paste(c("WID observations matched by each artifact's declared keys; aggregate totals are not used to infer unchanged values.", notes), collapse = "\n")))
  }
  if (identical(family, "surveys")) {
    sources <- prepared$outputs$survey_source_comparison
    metrics <- c("row_count", "sum_fep", "adult_weight", "adult_share", "missing_fep", "missing_edad")
    rows <- if (!nrow(sources)) data.frame() else dina_review_bind(lapply(metrics, function(metric) {
      old <- sources[c("country", "year", paste0("canonical_", metric))]
      new <- sources[c("country", "year", paste0("incoming_", metric))]
      names(old)[3L] <- names(new)[3L] <- metric
      old <- old[!is.na(sources$canonical_rel) & nzchar(sources$canonical_rel), , drop = FALSE]
      dina_review_compare(old, new, c("country", "year"), metric, function(old, new) 1e-8)
    }))
    return(list(rows = rows, note = "Survey comparisons cover file changes, structure, weights and age summaries. Individual income values are not checked."))
  }
  rows <- explored$outputs$aux_comparison_detail %||% data.frame()
  if (nrow(rows)) {
    # Only compare an incoming auxiliary series when it exists. A carried-forward
    # canonical dependency is not a deletion from a proposed replacement.
    groups <- paste(rows$source_id, rows$dependency_id)
    active <- unique(groups[!is.na(rows$incoming_value)])
    rows <- rows[groups %in% active, , drop = FALSE]
    rows$measure <- rows$value_column
    rows$old_value <- rows$current_value
    rows$new_value <- rows$incoming_value
    rows$difference <- rows$new_value - rows$old_value
    rows$percent <- ifelse(!is.na(rows$old_value) & rows$old_value != 0, 100 * rows$difference / abs(rows$old_value), NA_real_)
    labels <- c(same_overlap = "unchanged", changed_overlap = "revised", extension = "new observation", dropped_current = "removed observation", missing_required = "value missing")
    rows$result <- unname(labels[rows$value_status])
    rows$result[is.na(rows$result)] <- "not checked"
    rows <- rows[c("country", "dependency_id", "year", "measure", "old_value", "new_value", "difference", "percent", "result")]
  }
  list(rows = rows, note = "Admin comparisons cover auxiliary series. Main PIT values are not checked; existing checks cover source structure, dependencies and cleaner outputs.")
}

dina_review_coverage <- function(explored, values, family, prepared = NULL) {
  if (identical(family, "sna") && nrow(values)) {
    return(dina_review_bind(lapply(split(values, values$country), function(rows) {
      old <- sort(unique(rows$year[is.finite(rows$old_value)]))
      new <- sort(unique(rows$year[is.finite(rows$new_value)]))
      data.frame(country = rows$country[[1L]], old_years = paste(old, collapse = ","), new_years = paste(new, collapse = ","), new_country = !length(old) && length(new) > 0L,
        extension_years = paste(setdiff(new, old), collapse = ","),
        overlap_years = paste(intersect(new, old), collapse = ","),
        missing_in_new_years = paste(setdiff(old, new), collapse = ","))
    })))
  }
  if (family %in% c("sna", "admin")) {
    rows <- explored$outputs$extension_summary
    if (identical(family, "admin") && nrow(rows)) {
      inventory <- explored$outputs$source_inventory
      plan <- prepared$outputs$promotion_plan
      rows$missing_in_new_years <- vapply(seq_len(nrow(rows)), function(i) {
        old <- inventory[inventory$source_id == rows$source_id[[i]] & inventory$source_set == "old" & inventory$status == "matched", , drop = FALSE]
        destinations <- plan$to_rel[plan$source_id == rows$source_id[[i]] & plan$artifact_type == "raw_source"]
        replaced <- vapply(old$rel, function(path) any(path == destinations | startsWith(path, paste0(destinations, "/"))), logical(1))
        years <- function(x) suppressWarnings(as.integer(unlist(strsplit(paste(x, collapse = ","), "[^0-9]+"))))
        missing <- setdiff(years(old$years), c(years(old$years[!replaced]), years(rows$new_years[[i]])))
        paste(sort(unique(missing[!is.na(missing)])), collapse = ",")
      }, character(1))
    }
    return(rows[intersect(c("country", "source_id", "old_years", "new_years", "extension_years", "overlap_years", "missing_in_new_years"), names(rows))])
  }
  if (identical(family, "surveys")) {
    rows <- explored$outputs$year_coverage
    if (!nrow(rows)) return(data.frame())
    parse_years <- function(x) {
      parts <- strsplit(if (is.na(x)) "" else x, ",", fixed = TRUE)[[1L]]
      unlist(lapply(parts, function(part) {
        bounds <- suppressWarnings(as.integer(strsplit(trimws(part), "[-:]")[[1L]]))
        if (length(bounds) == 2L && !anyNA(bounds)) seq.int(bounds[[1L]], bounds[[2L]]) else bounds[!is.na(bounds)]
      }))
    }
    rows$extension_years <- mapply(function(old, incoming) paste(setdiff(parse_years(incoming), parse_years(old)), collapse = ","),
      rows$canonical_discovered_years, rows$incoming_discovered_years)
    rows$overlap_years <- mapply(function(old, incoming) paste(intersect(parse_years(old), parse_years(incoming)), collapse = ","),
      rows$canonical_discovered_years, rows$incoming_discovered_years)
    # Surveys are added/replaced individually; absent incoming years are retained.
    rows$missing_in_new_years <- mapply(function(old, selected) paste(setdiff(parse_years(old), parse_years(selected)), collapse = ","),
      rows$canonical_discovered_years, rows$selected_years)
    rows$new_country <- rows$canonical_discovered_count == 0L & rows$incoming_discovered_count > 0L
    rows$old_years <- rows$canonical_discovered_years
    rows$new_years <- rows$selected_years
    return(rows[c("country", "new_country", "old_years", "new_years", "extension_years", "overlap_years", "missing_in_new_years", "blocked_years")])
  }
  # WID coverage uses candidate observations as well as accepted observations.
  if (!nrow(values)) return(explored$outputs$wid_artifact_status %||% data.frame())
  values$review_country <- dina_review_country(values, prepared$contract$area_metadata)
  groups <- split(values, paste(values$source_id, values$review_country))
  dina_review_bind(lapply(groups, function(rows) {
    years <- function(hit) paste(sort(unique(rows$year[hit & !is.na(rows$year)])), collapse = ",")
    data.frame(country = rows$review_country[[1]], source_id = rows$source_id[[1]],
      old_years = years(rows$result != "new observation"), new_years = years(rows$result != "removed observation"), new_country = all(rows$result == "new observation"),
      extension_years = years(rows$result == "new observation"),
      overlap_years = years(!rows$result %in% c("new observation", "removed observation")),
      missing_in_new_years = years(rows$result == "removed observation"))
  }))
}

dina_review_freeze_sna <- function(prepared, root) {
  rows <- prepared$outputs$staged_source_mappings
  if (!nrow(rows) || !"confirm_ready" %in% names(rows)) return(data.frame())
  ready <- which(rows$confirm_ready %in% c(TRUE, "TRUE"))
  dir <- file.path(prepared$paths$root, "reviewed_sources")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (i in ready) {
    target <- file.path(dir, paste0(i, "-", basename(rows$source[[i]])))
    if (!file.copy(rows$source[[i]], target, copy.date = TRUE)) stop("Could not save the reviewed SNA candidate.", call. = FALSE)
    rows$source[[i]] <- target
  }
  utils::write.csv(rows, file.path(prepared$paths$tables, "staged_source_mappings.csv"), row.names = FALSE, na = "")
  data.frame(from_rel = rows$source[ready], to_rel = vapply(rows$destination[ready], dina_relative, character(1), root = root))
}

dina_review_explore <- function(root, family, flags = list(), input = "stdin", is_terminal = isatty(stdin())) {
  record <- list(family = family, status = "incomplete", reviewed_at = dina_now())
  dina_review_save(record, root)
  engine <- dina_review_engine(root, family)
  dina_cli_header(paste(dina_review_families()[[family]], "Explore"))
  dina_cli_alert("Checking the family's coverage, revisions and source structure.")
  input_paths <- dina_review_inputs(root, family, engine)
  before <- dina_review_hashes(input_paths)
  output <- flags[["output-dir"]] %||% NULL
  explored <- switch(family,
    sna = engine$run_country_sna_explorer(root = root, output_dir = output),
    admin = engine$run_admin_pit_explorer(root = root, output_dir = output),
    surveys = engine$run_survey_pop_explorer(root = root, output_dir = output),
    wid = engine$run_wid_explorer(root = root, output_dir = output, fetch = isTRUE(flags$fetch)))
  if (identical(family, "wid") && !isTRUE(flags$fetch) && !isTRUE(flags[["no-fetch"]]) && isTRUE(is_terminal) && dina_wid_explore_needs_fetch(explored)) {
    dina_cli_cat("Equivalent command: dina sources explore wid --fetch")
    if (dina_menu_confirm("Fetch WID sources", "Fetch missing or stale WID candidates for this review?", input = input, is_terminal = is_terminal)) {
      flags$fetch <- TRUE
      explored <- engine$run_wid_explorer(root = root, output_dir = output, fetch = TRUE)
    }
  }
  if (identical(family, "wid") && isTRUE(flags$fetch)) {
    incoming <- file.path(root, "input_data", "_new", family)
    before[incoming] <- dina_review_hashes(incoming)
  }
  dina_cli_alert("Preparing the family review. Accepted source files remain unchanged.")
  if (identical(family, "surveys") && !nrow(explored$outputs$year_coverage)) {
    stop("No survey files are available to prepare. Follow dina sources list guide surveys, then explore again. Value comparisons are not checked.", call. = FALSE)
  }
  id <- paste0("review-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-", basename(tempfile()))
  exploration <- explored$paths$root
  prepared <- switch(family,
    sna = engine$run_country_sna_include(root = root, exploration_run = exploration, run_id = id),
    admin = engine$run_admin_pit_include(root = root, exploration_run = exploration, run_id = id),
    surveys = engine$run_survey_pop_include(root = root, exploration_run = exploration, run_id = id),
    wid = engine$run_wid_include(root = root, exploration_run = exploration, run_id = id))
  comparison_error <- ""
  values <- tryCatch(dina_review_values(root, family, engine, explored, prepared), error = function(e) {
    comparison_error <<- conditionMessage(e)
    list(rows = data.frame(), note = comparison_error)
  })
  manifest <- prepared$manifest %||% prepared$outputs$include_manifest
  status <- dina_manifest_value(manifest, "status")
  if (any(values$rows$result == "nonfinite value", na.rm = TRUE)) comparison_error <- "Value comparison found nonfinite values; inspect missing-values before exploring again."
  if (nzchar(comparison_error)) status <- "blocked"
  plan <- if (identical(family, "sna")) dina_review_freeze_sna(prepared, root) else prepared$outputs$promotion_plan
  if (!nrow(plan) && identical(status, "all_good")) status <- "nothing_to_include"
  tables <- list(review_coverage = dina_review_coverage(explored, values$rows, family, prepared), review_values = values$rows,
    review_problems = dina_review_problems(prepared), review_files = plan)
  # Copy inventory-only details into this run so later exploration cannot change
  # the evidence displayed by an older review.
  for (name in names(explored$outputs)) if (is.data.frame(explored$outputs[[name]]) && !file.exists(file.path(prepared$paths$tables, paste0(name, ".csv")))) tables[[name]] <- explored$outputs[[name]]
  for (name in names(tables)) utils::write.csv(tables[[name]], file.path(prepared$paths$tables, paste0(name, ".csv")), row.names = FALSE, na = "")
  after <- dina_review_hashes(input_paths)
  if (!identical(before, after)) {
    status <- "blocked"
    comparison_error <- "Source files or configuration changed during exploration. Explore again."
  }
  record <- list(family = family, status = status, run = prepared$paths$root, reviewed_at = dina_now(),
    files = nrow(plan), comparison_note = values$note, comparison_error = comparison_error,
    scope = "whole family", area_metadata = if (family == "wid") prepared$contract$area_metadata else NULL, inputs = after,
    candidates = dina_review_hashes(c(plan$from_rel, file.path(prepared$paths$root, "tables"), file.path(prepared$paths$root, "logs"))),
    baseline = dina_review_hashes(if (nrow(plan)) file.path(root, plan$to_rel) else character()))
  record$watch <- dina_review_watch(unique(c(input_paths, names(record$candidates), names(record$baseline))))
  dina_write_json(record, file.path(record$run, "review.json"))
  dina_review_save(record, root)
  dina_review_print(record, country = flags$country %||% NULL)
  invisible(record)
}

dina_review_table <- function(record, table) {
  aliases <- c(coverage = "review_coverage", revisions = "review_values", `added-values` = "review_values", `missing-values` = "review_values", problems = "review_problems", files = "review_files")
  selection <- table
  if (table %in% names(aliases)) table <- aliases[[table]]
  if (!grepl("^[A-Za-z0-9_-]+$", table)) stop("Invalid review table name.", call. = FALSE)
  path <- file.path(record$run, "tables", paste0(gsub("-", "_", table), ".csv"))
  if (!file.exists(path)) stop("This table is not part of the saved review: ", table, call. = FALSE)
  rows <- dina_read_csv_table_or_empty(path)
  # Adapt old saved evidence in memory only; never rewrite a reviewed candidate.
  if (identical(record$family, "sna") && table == "review_values" && nrow(rows) && !"absence_reason" %in% names(rows)) {
    detail <- dina_read_csv_table_or_empty(file.path(record$run, "tables", "include_detail.csv"))
    rows <- dina_review_sna_absences(rows, detail)
  }
  if (record$family == "sna" && table == "review_coverage" && !"old_years" %in% names(rows)) {
    rows <- dina_review_coverage(NULL, dina_review_table(record, "review_values"), "sna")
  }
  if (table == "review_problems") {
    outputs <- lapply(dina_review_problem_tables(), function(name) { path <- file.path(record$run, "tables", paste0(name, ".csv")); if (file.exists(path)) dina_read_csv_table_or_empty(path) else data.frame() })
    names(outputs) <- dina_review_problem_tables()
    rows <- dina_review_problems(list(outputs = outputs))
    if (nrow(rows)) rows$country <- dina_review_country(rows, record$area_metadata)
    if (nzchar(record$comparison_error %||% "")) rows <- dina_review_bind(list(data.frame(severity = "Blocker", reason = record$comparison_error, status = "comparison_failed"), rows))
  }
  if (selection == "revisions") rows <- rows[rows$result == "revised", , drop = FALSE]
  if (selection == "added-values") rows <- rows[rows$result == "value added", , drop = FALSE]
  if (selection == "missing-values") rows <- rows[rows$result %in% c("value missing", "removed observation", "expected value missing", "absence unexplained", "nonfinite value"), , drop = FALSE]
  rows
}

dina_review_verify <- function(record) {
  for (kind in c("inputs", "candidates", "baseline")) {
    if (!identical(dina_review_hashes(names(record[[kind]])), record[[kind]])) stop("Reviewed ", kind, " changed. Explore the family again before inclusion.", call. = FALSE)
  }
  invisible(TRUE)
}

dina_review_include <- function(root, family, flags = list(), input = "stdin", is_terminal = isatty(stdin()), record = dina_review_read(root, family)) {
  if (is.null(record) || identical(record$status, "incomplete")) stop("Explore this family first: dina sources explore ", family, call. = FALSE)
  if (identical(record$status, "included")) {
    if (is.null(record$watch) || !identical(dina_review_watch(names(record$watch)), record$watch)) stop("Evidence changed since inclusion. Explore this family again: dina sources explore ", family, call. = FALSE)
    dina_cli_ok("This family review has already been included.")
    return(invisible(record))
  }
  if (!identical(record$status, "all_good")) stop("The family review has unresolved checks. Inspect dina sources table ", family, " before exploring again.", call. = FALSE)
  dina_review_verify(record)
  dina_cli_header(paste("Include", dina_review_families()[[family]]))
  dina_cli_cat(sprintf("Accept the whole reviewed family: %s files, reviewed %s.", record$files, record$reviewed_at))
  values <- dina_review_table(record, "review_values")
  dina_cli_cat(sprintf("Evidence: %s revised values, %s new observations, %s lost values.",
    sum(values$result == "revised"), sum(values$result == "new observation"), sum(values$result %in% c("value missing", "removed observation"))))
  dina_cli_cat(record$comparison_note)
  dina_review_show_rows(dina_review_table(record, "review_coverage"), c("country", "source_id", "extension_years", "overlap_years", "missing_in_new_years"))
  confirmed <- isTRUE(flags$confirm)
  if (!confirmed && isTRUE(is_terminal)) confirmed <- dina_menu_confirm("Include family", "Accept this reviewed family?", input = input, is_terminal = is_terminal)
  if (!confirmed) {
    dina_cli_cat(sprintf("No files changed. Scripts can accept this review with: dina sources include %s --confirm", family))
    return(invisible(record))
  }
  engine <- dina_review_engine(root, family)
  dina_review_verify(record)
  record$status <- "including"
  record$backup_root <- file.path(dirname(dirname(record$run)), "confirms")
  dina_review_save(record, root)
  result <- tryCatch(switch(family,
    sna = engine$country_sna_include_confirm_sources(root = root, include_run = record$run),
    admin = engine$admin_pit_include_confirm_sources(root = root, include_run = record$run),
    surveys = engine$survey_pop_confirm_sources(root = root, include_run = record$run),
    wid = engine$wid_include_confirm_sources(root = root, include_run = record$run)), error = function(e) {
      record$status <- "inclusion_failed"
      record$error <- conditionMessage(e)
      manifests <- list.files(record$backup_root, pattern = "^confirm_manifest\\.csv$", full.names = TRUE, recursive = TRUE)
      manifests <- manifests[order(file.info(manifests)$mtime, decreasing = TRUE)]
      for (path in manifests) {
        manifest <- dina_read_csv_table_or_empty(path)
        if (identical(dina_manifest_value(manifest, "include_run"), record$run)) {
          record$confirmation <- dirname(dirname(path))
          break
        }
      }
      dina_review_save(record, root)
      stop(e)
    })
  manifest <- result$manifest %||% result$outputs$confirm_manifest
  if (!identical(dina_manifest_value(manifest, "status"), "confirmed")) {
    record$status <- "inclusion_failed"
    record$confirmation <- result$paths$root
    dina_review_save(record, root)
    stop("Inclusion did not complete. Inspect the backup and inclusion report at ", result$paths$root, call. = FALSE)
  }
  record$status <- "included"
  record$included_at <- dina_now()
  record$confirmation <- result$paths$root
  record$watch <- dina_review_watch(names(record$inputs))
  dina_review_save(record, root)
  dina_cli_ok("Family included. The pipeline has not been run.")
  dina_cli_cat(sprintf("Next family: dina sources\nRestore backup: dina sources include %s --restore %s", family, result$paths$root))
  invisible(result)
}

dina_review_choose_family <- function(input = "stdin", is_terminal = isatty(stdin()), root = dina_repo_root()) {
  families <- dina_review_families()
  if (!isTRUE(is_terminal)) stop("Choose a family: sna, admin, surveys, or wid. Example: dina sources explore surveys", call. = FALSE)
  dina_menu_select("Source families", lapply(names(families), function(family) dina_menu_action(family, paste(families[[family]], "—", dina_review_family_status(root, family)$label), description = dina_review_family_status(root, family)$reason, command = paste("dina sources explore", family))),
    default = "quit", input = input, is_terminal = is_terminal)
}

dina_review_details <- function(record, input = "stdin", is_terminal = isatty(stdin())) {
  choices <- c(coverage = "Coverage", revisions = "Numerical revisions", `added-values` = "Newly available values", `missing-values` = "Missing values", problems = "Extraction problems", files = "Files to include")
  details <- switch(record$family, surveys = c(survey_source_comparison = "Survey files and schema"),
    sna = c(include_detail = "Extraction checks"), wid = c(include_detail = "Artifact checks"),
    admin = c(aux_validation_report = "Auxiliary series checks", static_dependency_report = "Dependencies", cleaner_summary = "Cleaner outputs"))
  choices <- c(choices, details[file.exists(file.path(record$run, "tables", paste0(names(details), ".csv")))])
  repeat {
    selected <- dina_menu_select("Review details", lapply(names(choices), function(table) dina_menu_action(table, choices[[table]],
      command = sprintf("dina sources table %s %s", record$family, table))), default = "quit", input = input, is_terminal = is_terminal)
    if (is.null(selected) || identical(selected, "quit")) return(invisible(NULL))
    dina_review_show_rows(dina_review_table(record, selected), dina_review_detail_columns(selected, dina_review_table(record, selected)), limit = 20L)
  }
}

dina_review_family_menu <- function(root, family, input = "stdin", is_terminal = isatty(stdin())) {
  repeat {
    state <- dina_review_family_status(root, family)
    dina_cli_cat(paste(state$label, "—", state$reason))
    actions <- c(list = "Find sources", explore = "Explore", table = "Review details", include = "Include", back = "Choose another family")
    selected <- dina_menu_select(dina_review_families()[[family]], lapply(names(actions), function(action) dina_menu_action(action, actions[[action]],
      command = if (action == "back") "dina sources" else sprintf("dina sources %s %s", action, family))), default = "quit", input = input, is_terminal = is_terminal)
    if (is.null(selected) || selected %in% c("back", "quit")) return(invisible(selected))
    tryCatch(if (selected == "include") dina_review_include(root, family, input = input, is_terminal = is_terminal) else if (selected == "table") {
      record <- dina_review_read(root, family)
      if (is.null(record$run)) dina_cli_cat("Explore this family first.") else {
        dina_review_print(record)
        dina_review_details(record, input, is_terminal)
      }
    } else if (selected == "list") {
      registry <- dina_print_source_list(root, list(family = family, `no-menu` = TRUE))
      if (length(registry)) dina_source_list_actions_menu(registry, root, list(family = family), input, is_terminal)
    } else dina_review_explore(root, family, input = input, is_terminal = is_terminal), error = function(e) dina_cli_warn(conditionMessage(e)))
  }
}

dina_cmd_sources <- function(root, args, input = "stdin", is_terminal = isatty(stdin())) {
  args <- dina_drop_leading_separator(args)
  if (!length(args)) {
    if (!is_terminal) {
      dina_cli_header("Source families")
      for (family in names(dina_review_families())) {
        state <- dina_review_family_status(root, family)
        dina_cli_cat(sprintf("%s — %s: %s\n  %s\n  dina sources explore %s", family, dina_review_families()[[family]], state$label, state$reason, family))
      }
      dina_cli_cat("\nFind sources, Explore, then Include one family at a time.\nAll registered inputs: dina sources list")
      return(invisible(NULL))
    }
    repeat {
      family <- dina_review_choose_family(input, is_terminal, root = root)
      if (is.null(family) || identical(family, "quit")) return(invisible(NULL))
      if (!identical(dina_review_family_menu(root, family, input, is_terminal), "back")) return(invisible(NULL))
    }
  }
  sub <- args[[1]]
  flags <- dina_parse_flags(args[-1])
  if (sub %in% c("explore", "include", "table") && !length(flags$positional)) {
    family <- dina_review_choose_family(input, is_terminal, root = root)
    if (is.null(family) || identical(family, "quit")) return(invisible(NULL))
    args <- c(sub, family, args[-1])
    flags <- dina_parse_flags(args[-1])
  }
  if (identical(sub, "include") && isTRUE(flags$confirm) && !is.null(flags$restore)) stop("Choose acceptance or restore, not both.", call. = FALSE)
  if (identical(sub, "include") && isTRUE(flags[["dry-run"]]) && (isTRUE(flags$confirm) || !is.null(flags$restore))) stop("An assessment cannot be combined with acceptance or restore.", call. = FALSE)
  if (identical(sub, "include") && isTRUE(flags$apply)) stop("--apply is retired. Use dina sources explore FAMILY, then dina sources include FAMILY.", call. = FALSE)
  if (identical(sub, "include") && !is.null(flags[["include-run"]]) && is.null(flags$restore) && !isTRUE(flags[["dry-run"]])) {
    run <- flags[["include-run"]]
    if (!grepl("^/", run)) run <- file.path(root, run)
    if (file.exists(file.path(run, "review.json"))) {
      record <- dina_read_json(file.path(run, "review.json"))
      family <- dina_source_workflow_family(dina_arg(flags$positional, 1L, record$family))
      if (!identical(record$family, family)) stop("The selected review belongs to another family.", call. = FALSE)
      current <- dina_review_read(root, family)
      if (!identical(current$run, record$run)) stop("This review was superseded. Explore the family again before inclusion.", call. = FALSE)
      return(dina_review_include(root, family, flags, input, is_terminal))
    }
  }
  legacy <- !sub %in% c("explore", "include", "table") || isTRUE(flags[["dry-run"]]) || !is.null(flags[["include-run"]]) || !is.null(flags[["exploration-run"]]) || !is.null(flags$restore)
  if (legacy) {
    result <- dina_cmd_sources_legacy(root, args)
    if (identical(sub, "include") && !is.null(flags$restore)) {
      family <- dina_source_workflow_family(dina_arg(flags$positional, 1L, "sna"))
      record <- dina_review_read(root, family)
      if (!is.null(record)) {
        record$status <- "restored"
        dina_review_save(record, root)
      }
    }
    return(invisible(result))
  }
  family <- dina_arg(flags$positional, 1L, NULL)
  if (is.null(family)) family <- dina_review_choose_family(input, is_terminal, root = root)
  if (is.null(family) || identical(family, "quit")) return(invisible(NULL))
  family <- dina_source_workflow_family(family, command = sub)
  if (sub == "explore") return(dina_review_explore(root, family, flags, input, is_terminal))
  if (sub == "include") return(dina_review_include(root, family, flags, input, is_terminal))
  record <- dina_review_read(root, family)
  table <- dina_arg(flags$positional, 2L, NULL)
  if (!is.null(flags$run)) {
    run <- flags$run
    if (!grepl("^/", run)) run <- file.path(root, run)
    if (!file.exists(file.path(run, "review.json"))) return(dina_cmd_sources_legacy(root, args))
    record <- dina_read_json(file.path(run, "review.json"))
  }
  if (is.null(record$run)) {
    if (!is.null(record)) stop("The latest exploration did not complete. Explore this family again: dina sources explore ", family, call. = FALSE)
    return(dina_cmd_sources_legacy(root, args))
  }
  if (!identical(record$family, family)) stop("The selected review belongs to another family.", call. = FALSE)
  if (is.null(table)) {
    dina_review_print(record, flags$country %||% NULL, flags$limit %||% 20L)
    if (is_terminal) dina_review_details(record, input, is_terminal)
  } else {
    dina_cli_header(paste(dina_review_families()[[family]], "Table:", table))
    dina_review_show_rows(dina_review_table(record, table), dina_review_detail_columns(table, dina_review_table(record, table)), country = flags$country %||% NULL, limit = flags$limit %||% 20L)
  }
  invisible(record)
}
