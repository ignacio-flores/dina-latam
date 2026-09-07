# Presentation of saved evidence. None of these helpers prepares or accepts data.
dina_review_sna_absences <- function(rows, detail = data.frame(), old = data.frame(), new = data.frame()) {
  key <- function(x, variable = "variable") paste(x$country, x$year, x[[variable]], sep = "\r")
  rows$absence_reason <- rep("", nrow(rows))
  for (side in c("old", "new")) {
    source <- if (side == "old") old else new
    hit <- if (nrow(source)) match(key(rows, "measure"), key(source)) else rep(NA_integer_, nrow(rows))
    for (field in c("extract_status", "source_file", "sheet")) {
      rows[[paste(side, field, sep = "_")]] <- if (field %in% names(source)) source[[field]][hit] else NA_character_
    }
  }
  hit <- if (nrow(detail)) match(key(rows, "measure"), key(detail)) else rep(NA_integer_, nrow(rows))
  status <- if ("status" %in% names(detail)) detail$status[hit] else rep(NA_character_, nrow(rows))
  empty <- is.na(rows$old_value) & is.na(rows$new_value)
  rows$result[empty] <- "absence unexplained"
  rows$absence_reason[empty] <- "No value extracted on either side; applicability is undetermined."
  expected_absence <- empty & status %in% c("ok_contract_missing")
  rows$result[expected_absence] <- "expected absence"
  rows$absence_reason[expected_absence] <- "Excluded or absent under the existing extraction contract."
  years_with_values <- unique(paste(rows$country[!empty], rows$year[!empty]))
  outside <- empty & is.na(status) & !paste(rows$country, rows$year) %in% years_with_values
  rows$result[outside] <- "outside coverage"
  rows$absence_reason[outside] <- "Outside extracted year coverage on both sides."
  failed <- empty & status %in% c("warning_missing_expected_value", "warning_ambiguous_match", "blocked_adapter_required")
  rows$result[failed] <- "expected value missing"
  extraction <- if ("extract_status" %in% names(detail)) detail$extract_status[hit] else rep(NA_character_, nrow(rows))
  extraction[is.na(extraction)] <- "extraction reason unavailable"
  rows$absence_reason[failed] <- paste("Expected value unavailable:", gsub("_", " ", extraction[failed]))
  rows
}

dina_review_problem_tables <- function() c("include_detail", "staged_source_mappings", "static_dependency_report",
  "aux_validation_report", "validation_report", "candidate_source_status", "candidate_country_status", "cleaner_summary")

dina_review_problems <- function(prepared) {
  # Explicit engine conditions: successful statuses containing 'missing' are not problems.
  warnings <- c("warning", "check_following", "warning_structure_review", "warning_ambiguous_match",
    "warning_missing_expected_value", "revision_detected")
  blockers <- c("blocked", "failed", "ambiguous_destination", "ambiguous_registry", "ambiguous_stem",
    "stage_failed", "blocked_adapter_required", "blocked_dependency", "blocked_destination_missing",
    "blocked_missing_aux", "blocked_missing_incoming_source", "blocked_structure_mismatch",
    "blocked_aux_ambiguous_candidates", "blocked_aux_canonical_invalid", "blocked_aux_missing_canonical_years",
    "blocked_aux_missing_required_years", "blocked_aux_overlap_changed", "blocked_ambiguous_canonical",
    "blocked_ambiguous_incoming", "blocked_canonical_unreadable_file", "blocked_no_observed_surveys",
    "blocked_unreadable_file", "blocked_copy_failed", "blocked_fetch_fingerprint_mismatch",
    "blocked_missing_fetch_fingerprint", "blocked_missing_fetched_candidate", "blocked_read_failed",
    "missing_file", "read_failed", "missing_required_columns", "missing_fep", "missing_edad",
    "unsupported_directory_source", "copy_failed", "missing_source", "unreadable_file", "missing_arg_population_target", "missing_static_dependency", "missing_dependency", "missing_clean_output",
    "missing_aux_dependency", "missing_aux_registry", "ambiguous_incoming_primary", "ambiguous_identity", "missing_identity")
  result <- dina_review_bind(lapply(dina_review_problem_tables(), function(table) {
    rows <- prepared$outputs[[table]] %||% data.frame()
    if (!nrow(rows)) return(data.frame())
    fields <- intersect(c("status", "source_status", "severity", "blocker", "cleaner_status", "action", "copy_status"), names(rows))
    if (!length(fields)) return(data.frame())
    bad <- vapply(seq_len(nrow(rows)), function(i) any(unlist(rows[i, fields]) %in% blockers), logical(1))
    warn <- vapply(seq_len(nrow(rows)), function(i) any(unlist(rows[i, fields]) %in% warnings), logical(1))
    # A nonempty engine blocker is an explicit failure, including future engine codes.
    if ("blocker" %in% names(rows)) bad <- bad | (!is.na(rows$blocker) & !rows$blocker %in% c("", "none", "FALSE", "false", "0"))
    conditions <- vapply(seq_len(nrow(rows)), function(i) {
      codes <- as.character(unlist(rows[i, fields]))
      hit <- codes[codes %in% c(blockers, warnings)]
      explicit <- as.character(rows$status[i] %||% NA_character_)
      if (length(explicit) == 1L && !is.na(explicit) && nzchar(explicit)) explicit else if (length(hit)) hit[[1]] else if ("blocker" %in% names(rows)) as.character(rows$blocker[i]) else "engine_condition"
    }, character(1))
    rows$status <- conditions
    rows$severity <- ifelse(bad, "Blocker", "Warning")
    rows <- rows[bad | warn, , drop = FALSE]
    if (!nrow(rows)) return(data.frame())
    if (!"status" %in% names(rows)) rows$status <- vapply(seq_len(nrow(rows)), function(i) {
      values <- unlist(rows[i, intersect(fields, names(rows))]); paste(values[!is.na(values) & nzchar(values)], collapse = "; ")
    }, character(1))
    if (!"reason" %in% names(rows)) rows$reason <- NA_character_
    missing_reason <- is.na(rows$reason) | !nzchar(rows$reason)
    rows$reason[missing_reason] <- gsub("_", " ", rows$status[missing_reason])
    labels <- c(ambiguous_destination = "Incoming file matches more than one destination",
      ambiguous_registry = "Incoming file matches more than one registered source",
      stage_failed = "Could not prepare the incoming file", warning_missing_expected_value = "Expected value was not extracted",
      warning_ambiguous_match = "Extraction found conflicting matches", blocked_adapter_required = "Source layout needs an extraction adapter")
    labeled <- rows$status %in% names(labels)
    rows$reason[labeled] <- unname(labels[rows$status[labeled]])
    rows[intersect(c("country", "source_id", "year", "variable", "severity", "status", "reason", "detail",
      "extract_status", "source_file", "sheet", "source", "path", "rel", "source_rel", "next_command"), names(rows))]
  }))
  if (!nrow(result)) return(result)
  result <- unique(result)
  result[order(match(result$severity, c("Blocker", "Warning")), result$country %||% rep("", nrow(result)), result$reason), , drop = FALSE]
}

dina_review_show_rows <- function(rows, columns = names(rows), country = NULL, limit = 12L) {
  if (!is.null(country) && any(c("country", "review_country", "iso") %in% names(rows))) rows <- rows[toupper(dina_review_country(rows)) %in% toupper(country), , drop = FALSE]
  rows <- rows[intersect(columns, names(rows))]
  if (!nrow(rows) || !ncol(rows)) { dina_cli_cat("None reported."); return(invisible(rows)) }
  zero <- if (all(c("old_value", "new_value") %in% names(rows))) is.finite(rows$old_value) & rows$old_value == 0 & is.finite(rows$new_value) else rep(FALSE, nrow(rows))
  for (name in intersect(c("old_value", "new_value", "difference", "percent"), names(rows))) rows[[name]] <- signif(rows[[name]], 6L)
  if ("percent" %in% names(rows)) {
    missing <- is.na(rows$percent)
    rows$percent <- as.character(rows$percent)
    rows$percent[missing] <- if ("result" %in% names(rows)) paste("N/A —", rows$result[missing]) else "N/A — values unavailable"
    rows$percent[zero] <- "N/A — baseline is zero"
  }
  for (name in names(rows)[grepl("years$", names(rows))]) rows[[name]] <- vapply(rows[[name]], function(x) {
    if (is.na(x) || !nzchar(as.character(x))) {
      if (name %in% c("old_years", "new_years")) "No extracted data" else "None"
    } else dina_admin_pit_year_label(x)
  }, character(1))
  labels <- c(old_years = "Accepted years", new_years = "Candidate years", extension_years = "Added years",
    overlap_years = "Years compared", missing_in_new_years = "Years missing from candidate", old_value = "Accepted", new_value = "Candidate", percent = "Change %")
  names(rows) <- ifelse(names(rows) %in% names(labels), labels[names(rows)], gsub("_", " ", names(rows)))
  rows[] <- lapply(rows, function(x) { x <- as.character(x); x[is.na(x) | !nzchar(x)] <- "Unavailable"; x })
  widths <- vapply(seq_along(rows), function(i) max(nchar(c(names(rows)[i], rows[[i]])), na.rm = TRUE), integer(1))
  limit <- max(1L, as.integer(limit))
  visible <- head(rows, limit)
  if (sum(widths) + 2L * (length(widths) - 1L) <= min(100L, getOption("width", 80L))) {
    dina_print_data_frame_compact(visible, limit)
  } else {
    for (i in seq_len(nrow(visible))) {
      for (j in seq_along(visible)) cat(paste(strwrap(paste0(names(visible)[j], ": ", visible[[j]][i]), width = min(80L, getOption("width", 80L)), exdent = 2L), collapse = "\n"), "\n", sep = "")
      dina_cli_cat("")
    }
  }
  if (nrow(rows) > limit) dina_cli_cat(sprintf("Showing %s of %s. Open the detail table with --limit N for more.", limit, nrow(rows)))
  invisible(rows)
}

dina_review_print <- function(record, country = NULL, limit = 12L) {
  stale <- !is.null(record$watch) && !identical(dina_review_watch(names(record$watch)), record$watch)
  label <- switch(record$status, all_good = "Review available", included = "Included", blocked = "Needs attention",
    check_following = "Needs attention", nothing_to_include = "Nothing to include", inclusion_failed = "Inclusion incomplete", including = "Inclusion incomplete", "Needs attention")
  dina_cli_cat(paste("Status:", label))
  if (stale) dina_cli_warn(paste("Evidence changed. Explore again:", "dina sources explore", record$family))
  if (!is.null(record$error)) dina_cli_warn(record$error)
  if (!is.null(record$confirmation)) dina_cli_cat(sprintf("Inclusion report: %s\nRestore: dina sources include %s --restore %s", record$confirmation, record$family, record$confirmation))
  if (is.null(record$confirmation) && !is.null(record$backup_root)) dina_cli_cat(paste("Backups:", record$backup_root))
  filter_country <- function(x) if (!is.null(country) && any(c("country", "review_country", "iso") %in% names(x))) x[toupper(dina_review_country(x)) %in% toupper(country), , drop = FALSE] else x
  coverage <- filter_country(dina_review_table(record, "review_coverage"))
  values <- filter_country(dina_review_table(record, "review_values"))
  problems <- filter_country(dina_review_table(record, "review_problems"))
  areas <- dina_review_country(values)
  countries <- sort(unique(na.omit(c(coverage$country, areas, problems$country))))
  countries <- countries[nzchar(countries)]
  dina_cli_cat("\nCountry summary")
  summary <- dina_review_bind(lapply(countries, function(ct) {
    v <- values[areas %in% ct, , drop = FALSE]; p <- problems[problems$country %in% ct, , drop = FALSE]
    data.frame(Country = ct, Revisions = sum(v$result == "revised"), Lost = sum(v$result %in% c("value missing", "removed observation")),
      Blockers = sum(p$severity == "Blocker"), Warnings = sum(p$severity == "Warning"))
  }))
  dina_review_show_rows(summary, limit = limit)
  dina_cli_cat("\n1. Coverage")
  # Historical reviews did not store extracted year ranges; derive from saved values.
  if (record$family == "sna" && nrow(values)) coverage <- dina_review_coverage(NULL, values, "sna")
  if (any(c("old_years", "new_years") %in% names(coverage))) dina_review_show_rows(coverage, c("country", "source_id", "old_years", "new_years"), limit = limit)
  dina_review_show_rows(coverage, c("country", "source_id", "extension_years", "missing_in_new_years"), limit = limit)
  dina_cli_cat("Year coverage does not imply complete variable coverage.")
  dina_cli_cat("\n2. Value changes")
  dina_cli_cat(sprintf("%s unchanged overlaps; %s numerical revisions; %s newly available values; %s lost values.",
    sum(values$result == "unchanged"), sum(values$result == "revised"), sum(values$result == "value added"), sum(values$result %in% c("value missing", "removed observation"))))
  display <- c("country", "source_id", "year", "measure", "old_value", "new_value", "difference", "percent", "result")
  for (kind in c("revised", "value added", "value missing", "removed observation")) {
    selected <- values[values$result %in% kind, , drop = FALSE]
    if (nrow(selected)) { dina_cli_cat(paste("\n", switch(kind, revised = "Numerical revisions", `value added` = "Newly available values", "Lost values"), sep = "")); dina_review_show_rows(selected, display, limit = min(limit, 3L)) }
  }
  dina_cli_cat(record$comparison_note %||% "Comparison scope unavailable.")
  if (record$family == "surveys") dina_review_show_rows(filter_country(dina_review_table(record, "survey_source_comparison")), c("country", "year", "comparison_status", "added_columns", "dropped_columns"), limit = limit)
  dina_cli_cat("\n3. Problems")
  if (nrow(problems)) {
    areas <- problems$country %||% rep(NA_character_, nrow(problems))
    areas[is.na(areas) | !nzchar(areas)] <- "Family"
    problems$country <- areas
    groups <- split(problems, paste(problems$severity, problems$country, problems$reason))
    for (group in groups) {
      dina_cli_cat(paste(strwrap(sprintf("%s · %s · %s (%s)", group$severity[1], group$country[1], group$reason[1], nrow(group)), width = 80L), collapse = "\n"))
      for (field in intersect(c("source_file", "source", "path", "rel"), names(group))) group[[field]] <- basename(group[[field]])
      columns <- intersect(c("year", "variable", "extract_status", "source_file", "source", "path", "rel", "detail"), names(group))
      columns <- columns[vapply(group[columns], function(x) any(!is.na(x) & nzchar(as.character(x))), logical(1))]
      if (length(columns)) dina_review_show_rows(group, columns, limit = min(limit, 2L))
      commands <- unique(na.omit(group$next_command %||% character()))
      for (command in commands[nzchar(commands)]) dina_cli_cat(sub("sources include ([a-z]+) --dry-run", "sources explore \\1", command))
    }
  } else dina_cli_cat("No engine problems reported.")
  unexplained <- sum(values$result %in% "absence unexplained")
  if (unexplained) dina_cli_cat(sprintf("%s absent values have undetermined applicability; inspect missing-values.", unexplained))
  if (nzchar(record$comparison_error %||% "")) dina_cli_warn(record$comparison_error)
  if (record$status == "blocked" && !any(problems$severity == "Blocker") && !nzchar(record$comparison_error %||% "")) dina_cli_warn("The engine blocked inclusion; inspect its detailed assessment for the remaining condition.")
  dina_cli_cat(sprintf("\nDetails: dina sources table %s revisions|missing-values|coverage|problems|files", record$family))
  if (record$status == "all_good" && !stale) dina_cli_cat(paste("After review: dina sources include", record$family))
  if (!record$status %in% c("included", "including", "inclusion_failed")) dina_cli_cat("Explore has not changed accepted sources or run the pipeline.")
  invisible(record)
}


dina_review_detail_columns <- function(table, rows) {
  if (table %in% c("revisions", "added-values")) return(setdiff(names(rows), c("absence_reason", "old_extract_status", "new_extract_status", "old_source_file", "new_source_file", "old_sheet", "new_sheet")))
  names(rows)
}
