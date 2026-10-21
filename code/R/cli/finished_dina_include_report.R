# Candidate reports remain separate from the original source exploration.
dina_finished_candidate_read <- function(record) {
  run <- record$include_candidate
  if (is.list(run)) run <- run$run
  if (is.null(run)) stop("Prepare a candidate first: dina sources include dina --dry-run", call. = FALSE)
  candidate <- dina_read_json(file.path(run, "candidate.json"), default = NULL)
  if (is.null(candidate) || !identical(candidate$family, "dina")) {
    stop("The saved finished DINA candidate cannot be read. Prepare a new dry run.", call. = FALSE)
  }
  candidate
}

dina_finished_candidate_table <- function(candidate, table) {
  if (!table %in% c("summary", "changes", "impact", "checks", "problems", "coverage")) {
    stop("Unknown prepared-source table.", call. = FALSE)
  }
  path <- file.path(candidate$run, "tables", paste0(table, ".csv"))
  if (!file.exists(path)) return(data.frame())
  dina_read_csv_table_or_empty(path)
}

dina_finished_prepared_filter <- function(rows, filters) {
  dina_finished_filter(rows, filters[intersect(names(filters), c("country", "year", "step", "unit"))])
}

dina_finished_boundary <- function(value) {
  ifelse(is.na(value), "-", paste0(formatC(100 * value, format = "f", digits = 6L), "%"))
}

dina_finished_include_print <- function(record, candidate, root = dina_repo_root(), filters = list()) {
  dina_cli_section("Prepared source")
  label <- switch(candidate$status, ready = "Ready for acceptance", accepted = "Accepted",
    blocked = "Preparation blocked", preparing = "Preparation incomplete", candidate$status)
  if (identical(record$status, "included") && identical(record$accepted_candidate, candidate$run)) label <- "Accepted"
  dina_cli_cat("Status: ", dina_cli_emphasis(label))
  for (source in candidate$sources) {
    dina_cli_cat(source$id, " · ", source$version, " · publication ", source$publication)
    dina_finished_note("  Source: ", source$citation, " ", source$doi)
    dina_finished_note("  Processing: ", source$processing)
  }
  summary <- dina_finished_prepared_filter(dina_finished_candidate_table(candidate, "summary"), filters)
  if (nrow(summary)) {
    keys <- unique(summary[c("country", "step", "unit")])
    display <- lapply(seq_len(nrow(keys)), function(i) {
      key <- keys[i, , drop = FALSE]
      part <- summary[summary$country == key$country & summary$step == key$step & summary$unit == key$unit, , drop = FALSE]
      data.frame(Country = key$country, Series = paste(key$step, key$unit, sep = "/"),
        Years = nrow(part), Brackets = paste(range(part$prepared_brackets), collapse = "–"),
        Thresholds = sum(part$threshold_repairs), Nudges = sum(part$threshold_nudges),
        Pooled = sum(part$pooled_bins), Boundaries = sum(part$boundary_changes))
    })
    display <- do.call(rbind, display)
    display$Brackets <- sub("^([0-9]+)–\\1$", "\\1", display$Brackets)
    dina_finished_table(display)
    dina_finished_note("Thresholds = repaired values; nudges = numerical margins; pooled = original bins pooled.")
    dina_finished_note("Boundaries = population cut points changed from nominal labels; these are not income corrections.")
    if ("max_mean_preservation_relative_error" %in% names(summary)) {
      dina_cli_cat("Maximum original-bin mean preservation error: ",
        dina_finished_number(max(summary$max_mean_preservation_relative_error) * 100, "%"))
    }
  }
  checks <- dina_finished_prepared_filter(dina_finished_candidate_table(candidate, "checks"), filters)
  if (nrow(checks)) {
    bad <- checks$status %in% c("fail", "review")
    dina_cli_cat("Prepared checks: ", sum(checks$status == "pass"), " passed; ",
      sum(bad), " failed; ", sum(checks$status == "warning"), " warning categories.")
  }
  problems <- dina_finished_prepared_filter(dina_finished_candidate_table(candidate, "problems"), filters)
  if (nrow(problems) && "detail" %in% names(problems)) {
    for (detail in unique(problems$detail)) dina_finished_note("  ", detail)
  }
  if (length(candidate$failures)) {
    for (failure in candidate$failures) {
      dina_cli_cat(failure$source_id %||% failure$id %||% "Source", ": ",
        failure$error %||% failure$message %||% failure$detail %||% "Preparation failed.")
    }
  }
  dina_finished_note("Changes: dina sources table dina changes [--year YYYY --step nat --unit esn]")
  dina_finished_note("Impact:  dina sources table dina impact [--year YYYY --step nat --unit esn]")
  dina_finished_note("Checks:  dina sources table dina prepared-checks")
  dina_finished_note("Original exploration remains available through checks, problems and coverage.")
  dina_finished_note("Prepared source only; continuation, WID scaling and publication activation remain deferred.")
  dina_cli_cat("Candidate: ", candidate$run)
  invisible(candidate)
}

dina_finished_include_show_table <- function(record, table, limit = 20L, filters = list()) {
  candidate <- dina_finished_candidate_read(record)
  if (table == "preparation") return(dina_finished_include_print(record, candidate, filters = filters))
  alias <- c(`prepared-checks` = "checks", `prepared-problems` = "problems", `prepared-coverage` = "coverage")
  name <- if (table %in% names(alias)) alias[[table]] else table
  rows <- dina_finished_prepared_filter(dina_finished_candidate_table(candidate, name), filters)
  limit <- suppressWarnings(as.integer(limit))
  if (length(limit) != 1L || is.na(limit) || limit < 1L) stop("--limit must be a positive integer.", call. = FALSE)
  if (!is.null(filters$size) || !is.null(filters$check)) {
    stop("Prepared tables use --year, --step, --unit and --country. Use --rule for changes.", call. = FALSE)
  }
  if (!is.null(filters$rule)) {
    if (name != "changes" || !is.character(filters$rule) || !filters$rule %in% unique(rows$rule)) {
      stop("--rule must identify a rule in the changes table.", call. = FALSE)
    }
    rows <- rows[rows$rule == filters$rule, , drop = FALSE]
  }
  if (!nrow(rows)) {
    dina_cli_cat("No entries match this view.")
    return(invisible(rows))
  }
  if (name == "changes") {
    priority <- match(rows$rule, c("threshold_repair", "tiny_mean_pooling", "threshold_numerical_margin",
      "inferred_population", "precision_normalization", "export_grid_interpolation"))
    rows <- rows[order(priority, -abs(rows$relative_percent), rows$country, rows$year, na.last = TRUE), , drop = FALSE]
  }
  total <- nrow(rows)
  rows <- head(rows, limit)
  if (isTRUE(filters$exact) || !name %in% c("changes", "impact")) {
    view <- rows
    for (field in intersect(c("before", "after", "difference", "relative_percent",
      "original_population_boundary", "target_population_boundary"), names(view))) {
      view[[field]] <- vapply(view[[field]], function(x) {
        if (is.na(x)) "-" else format(x, digits = 15L, trim = TRUE)
      }, character(1))
    }
    if ("difference" %in% names(view)) {
      names(view)[names(view) == "difference"] <- "difference (after - before)"
    }
    dina_review_show_rows(view, limit = limit, dim_headers = FALSE,
      dim_columns = c("reason", "detail"))
  } else {
    by <- if (name == "changes") c("country", "year", "step", "unit", "rule") else c("country", "year", "step", "unit", "basis")
    key <- do.call(paste, c(rows[by], sep = " · "))
    for (group in unique(key)) {
      part <- rows[key == group, , drop = FALSE]
      dina_cli_section(group)
      if (name == "changes") {
        view <- data.frame(Bracket = part$percentile, Field = part$field,
          Before = dina_finished_number(part$before), After = dina_finished_number(part$after),
          Difference = dina_finished_number(part$difference),
          `Change %` = dina_finished_number(part$relative_percent, "%"), check.names = FALSE)
      } else {
        share <- part$statistic %in% c("bottom50", "top10", "top1", "top0_1")
        before <- dina_finished_number(part$before)
        after <- dina_finished_number(part$after)
        difference <- dina_finished_number(part$difference)
        before[share] <- dina_finished_number(100 * part$before[share], "%")
        after[share] <- dina_finished_number(100 * part$after[share], "%")
        difference[share] <- dina_finished_number(100 * part$difference[share], " pp")
        view <- data.frame(Statistic = part$statistic, Before = before, After = after, Difference = difference,
          `Source p` = dina_finished_boundary(part$original_population_boundary),
          `Target p` = dina_finished_boundary(part$target_population_boundary), check.names = FALSE)
      }
      dina_finished_table(view)
      for (reason in unique(part$reason)) dina_finished_note("  ", reason)
    }
  }
  dina_finished_note(sprintf("Showing %s of %s entries. Use --limit N or --exact for full precision.", nrow(rows), total))
  if (name == "changes") {
    dina_finished_note("Threshold repairs appear first. Filter a correction with --rule RULE.")
  } else if (name == "impact") {
    dina_finished_note("Share differences are percentage points. Gini uses the 0–1 scale.")
    dina_finished_note("Source p is the recovered population boundary; compare same_population_boundary rows to assess preservation.")
  }
  invisible(rows)
}
