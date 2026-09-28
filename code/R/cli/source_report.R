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
  excluded_on_both_sides <- empty &
    rows$old_extract_status %in% "excluded_by_contract" &
    rows$new_extract_status %in% "excluded_by_contract"
  rows$result[excluded_on_both_sides] <- "expected absence"
  rows$absence_reason[excluded_on_both_sides] <- "Excluded on both sides under the existing extraction contract."
  expected_absence <- empty & status %in% c("ok_contract_missing")
  rows$result[expected_absence] <- "expected absence"
  rows$absence_reason[expected_absence] <- "Excluded or absent under the existing extraction contract."
  years_with_values <- unique(paste(rows$country[!empty], rows$year[!empty]))
  outside <- empty & is.na(status) & !excluded_on_both_sides & !paste(rows$country, rows$year) %in% years_with_values
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

dina_review_plain_number <- function(value, digits = 2L) {
  if (is.na(value)) return(NA_character_)
  if (!is.finite(value)) return(as.character(value))
  out <- formatC(value, format = "f", digits = digits, big.mark = ",")
  if (digits > 0L) {
    out <- sub("0+$", "", out)
    out <- sub("\\.$", "", out)
  }
  out
}

dina_review_amount <- function(value) {
  if (is.na(value)) return(NA_character_)
  magnitude <- abs(value)
  divisor <- 1
  suffix <- ""
  if (magnitude >= 1e9) {
    divisor <- 1e9
    suffix <- "B"
  } else if (magnitude >= 1e6) {
    divisor <- 1e6
    suffix <- "M"
  } else if (magnitude >= 1e3) {
    divisor <- 1e3
    suffix <- "k"
  }
  digits <- if (divisor == 1) 2L else if (magnitude / divisor < 100) 1L else 0L
  paste0(dina_review_plain_number(value / divisor, digits), suffix)
}

dina_review_percent <- function(value) {
  if (is.na(value)) return(NA_character_)
  if (value != 0 && abs(value) < 0.01) return(if (value < 0) "<0.01% decrease" else "<0.01%")
  paste0(dina_review_plain_number(value, 2L), "%")
}

dina_review_compact_coverage <- function(coverage) {
  if (!nrow(coverage)) return(coverage)
  rows <- lapply(seq_len(nrow(coverage)), function(i) {
    no_evidence <- "scope_empty" %in% names(coverage) && isTRUE(coverage$scope_empty[[i]])
    old <- coverage$old_years[[i]] %||% ""
    new <- coverage$new_years[[i]] %||% ""
    added <- coverage$extension_years[[i]] %||% ""
    missing <- coverage$missing_in_new_years[[i]] %||% ""
    label <- function(years) {
      if (is.na(years) || !nzchar(years)) "none" else dina_admin_pit_year_label(years, max_chars = 24L)
    }
    changes <- c(
      if (!is.na(added) && nzchar(added)) paste0("+", dina_admin_pit_year_label(added, max_chars = 16L)),
      if (!is.na(missing) && nzchar(missing)) paste0("missing ", dina_admin_pit_year_label(missing, max_chars = 16L))
    )
    data.frame(
      country = coverage$country[[i]] %||% "",
      coverage = if (no_evidence) "not assessed" else label(old),
      change = if (no_evidence) "no SNA evidence" else if (length(changes)) paste(changes, collapse = "; ") else "no change",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

dina_review_wid_compact_coverage <- function(coverage, scope = list()) {
  required <- c("country", "source_id", "old_years", "new_years", "extension_years", "missing_in_new_years")
  if (!nrow(coverage) || !all(required %in% names(coverage))) return(dina_review_compact_coverage(coverage))
  allowed_years <- unique(suppressWarnings(as.integer(scope$years %||% integer())))
  allowed_years <- allowed_years[is.finite(allowed_years)]
  years <- function(x) {
    found <- suppressWarnings(as.integer(unlist(strsplit(paste(x, collapse = ","), "[^0-9]+"))))
    found <- sort(unique(found[is.finite(found)]))
    if (length(allowed_years)) found <- intersect(found, allowed_years)
    found
  }
  rows <- lapply(split(coverage, as.character(coverage$country)), function(part) {
    summarized <- data.frame(
      country = part$country[[1L]],
      old_years = paste(years(part$old_years), collapse = ","),
      new_years = paste(years(part$new_years), collapse = ","),
      extension_years = paste(years(part$extension_years), collapse = ","),
      missing_in_new_years = paste(years(part$missing_in_new_years), collapse = ","),
      stringsAsFactors = FALSE
    )
    compact <- dina_review_compact_coverage(summarized)
    checks <- length(unique(part$source_id))
    compact$artifacts <- sprintf("%s benchmark check%s", checks, if (checks == 1L) "" else "s")
    compact
  })
  out <- dina_review_bind(rows)
  if (nrow(out)) out[order(out$country), , drop = FALSE] else out
}

dina_review_sna_filter_scope <- function(rows, scope = list()) {
  if (!nrow(rows)) return(rows)
  countries <- toupper(as.character(scope$countries %||% character()))
  countries <- unique(countries[!is.na(countries) & nzchar(countries)])
  years <- suppressWarnings(as.integer(scope$years %||% integer()))
  years <- unique(years[is.finite(years)])
  keep <- rep(TRUE, nrow(rows))
  if (length(countries) && "country" %in% names(rows)) {
    row_countries <- toupper(trimws(as.character(rows$country)))
    # An unscoped condition applies to the complete configured review, so it
    # remains visible. Country-specific rows must belong to the configuration.
    keep <- keep & (is.na(row_countries) | !nzchar(row_countries) | row_countries %in% countries)
  }
  if (length(years) && "year" %in% names(rows)) {
    row_years <- suppressWarnings(as.integer(rows$year))
    # File- or country-level conditions do not have a single year and remain
    # visible; dated evidence is limited to the effective run-year scope.
    keep <- keep & (is.na(row_years) | row_years %in% years)
  }
  rows[keep, , drop = FALSE]
}

dina_review_years_in_scope <- function(label, years) {
  years <- sort(unique(suppressWarnings(as.integer(years))))
  years <- years[is.finite(years)]
  if (is.na(label) || !nzchar(as.character(label))) return("")
  if (!length(years)) return(as.character(label))
  parts <- strsplit(as.character(label), ",", fixed = TRUE)[[1L]]
  values <- unlist(lapply(parts, function(part) {
    bounds <- suppressWarnings(as.integer(strsplit(trimws(part), "[-:]")[[1L]]))
    if (length(bounds) == 2L && !anyNA(bounds)) seq.int(bounds[[1L]], bounds[[2L]]) else bounds[is.finite(bounds)]
  }))
  values <- sort(unique(intersect(values, years)))
  if (!length(values)) return("")
  groups <- split(values, cumsum(c(TRUE, diff(values) != 1L)))
  paste(vapply(groups, function(group) {
    if (length(group) == 1L) as.character(group[[1L]]) else sprintf("%s-%s", group[[1L]], group[[length(group)]])
  }, character(1)), collapse = ",")
}

dina_review_survey_scope_coverage <- function(coverage, scope = list()) {
  if (!nrow(coverage)) return(coverage)
  coverage <- dina_review_sna_scope_coverage(coverage, scope)
  countries <- toupper(as.character(scope$countries %||% character()))
  countries <- unique(countries[!is.na(countries) & nzchar(countries)])
  years <- suppressWarnings(as.integer(scope$years %||% integer()))
  years <- years[is.finite(years)]
  if (length(countries) && "country" %in% names(coverage)) {
    coverage <- coverage[toupper(as.character(coverage$country)) %in% countries, , drop = FALSE]
  }
  for (field in intersect(c("old_years", "new_years", "extension_years", "overlap_years", "missing_in_new_years", "blocked_years"), names(coverage))) {
    coverage[[field]] <- vapply(coverage[[field]], dina_review_years_in_scope, character(1), years = years)
  }
  coverage
}

dina_review_sna_scope_coverage <- function(coverage, scope = list()) {
  if (!is.data.frame(coverage)) coverage <- data.frame(stringsAsFactors = FALSE)
  countries <- toupper(as.character(scope$countries %||% character()))
  countries <- unique(countries[!is.na(countries) & nzchar(countries)])
  if (!length(countries)) return(coverage)
  fields <- c("country", "old_years", "new_years", "new_country", "extension_years", "overlap_years", "missing_in_new_years")
  for (field in setdiff(fields, names(coverage))) {
    coverage[[field]] <- if (identical(field, "new_country")) rep(FALSE, nrow(coverage)) else rep("", nrow(coverage))
  }
  coverage <- coverage[toupper(as.character(coverage$country)) %in% countries, , drop = FALSE]
  coverage$scope_empty <- rep(FALSE, nrow(coverage))
  observed <- toupper(as.character(coverage$country))
  absent <- countries[!countries %in% observed]
  if (length(absent)) {
    empty <- data.frame(
      country = absent, old_years = "", new_years = "", new_country = FALSE,
      extension_years = "", overlap_years = "", missing_in_new_years = "",
      scope_empty = TRUE, stringsAsFactors = FALSE
    )
    for (field in setdiff(names(coverage), names(empty))) empty[[field]] <- NA
    empty <- empty[names(coverage)]
    coverage <- rbind(coverage, empty)
  }
  coverage[match(countries, toupper(as.character(coverage$country))), , drop = FALSE]
}

dina_review_config_display_scope <- function(scope, root, family = "Source") {
  current <- dina_source_explore_scope(root, family = family)
  if (!length(scope$years %||% integer())) scope$years <- current$years
  if (!length(scope$countries %||% character())) scope$countries <- current$countries
  scope
}

dina_review_filter_display_scope <- function(rows, scope = list(), metadata = list()) {
  if (!nrow(rows)) return(rows)
  countries <- unique(toupper(as.character(scope$countries %||% character())))
  countries <- countries[!is.na(countries) & nzchar(countries)]
  years <- unique(suppressWarnings(as.integer(scope$years %||% integer())))
  years <- years[is.finite(years)]
  keep <- rep(TRUE, nrow(rows))
  if (length(countries)) {
    mapped <- toupper(dina_review_country(rows, metadata))
    keep <- keep & !is.na(mapped) & mapped %in% countries
  }
  if (length(years) && "year" %in% names(rows)) {
    keep <- keep & suppressWarnings(as.integer(rows$year)) %in% years
  }
  rows[keep, , drop = FALSE]
}

dina_review_sna_display_scope <- function(scope, root) {
  dina_review_config_display_scope(scope, root, family = "National accounts")
}

dina_review_sna_audit_rows <- function(values, include_detail = data.frame(stringsAsFactors = FALSE)) {
  if (!nrow(values) || !"result" %in% names(values)) return(values[FALSE, , drop = FALSE])
  audit <- values[!is.na(values$result) & values$result != "unchanged", , drop = FALSE]
  required <- c("country", "year", "variable")
  if (!nrow(audit) || !all(c("country", "year", "measure") %in% names(audit)) ||
      !nrow(include_detail) || !all(required %in% names(include_detail))) return(audit)
  detail <- include_detail[!duplicated(include_detail[required]), , drop = FALSE]
  audit_key <- paste(audit$country, audit$year, audit$measure, sep = "\r")
  detail_key <- paste(detail$country, detail$year, detail$variable, sep = "\r")
  matched <- match(audit_key, detail_key)
  for (field in intersect(c("source_file", "sheet", "extract_status", "expected_status"), names(detail))) {
    audit[[field]] <- detail[[field]][matched]
  }
  audit
}

dina_review_sna_year_span <- function(years) {
  years <- sort(unique(as.integer(years[is.finite(years)])))
  if (!length(years)) return("—")
  if (length(years) == 1L) return(as.character(years[[1L]]))
  sprintf("%s–%s (%s years)", years[[1L]], years[[length(years)]], length(years))
}

dina_review_sna_variable_summary <- function(rows, revisions = FALSE) {
  if (!nrow(rows)) return(data.frame(stringsAsFactors = FALSE))
  variables <- sort(unique(as.character(rows$measure[!is.na(rows$measure) & nzchar(rows$measure)])))
  out <- lapply(variables, function(variable) {
    part <- rows[rows$measure == variable, , drop = FALSE]
    base <- data.frame(
      variable = variable,
      years = dina_review_sna_year_span(part$year),
      rows = nrow(part),
      stringsAsFactors = FALSE
    )
    if (!revisions) return(base)
    finite <- part[is.finite(part$percent), , drop = FALSE]
    if (!nrow(finite)) {
      base[["mean change"]] <- "—"
      base[["min change"]] <- "—"
      base[["max change"]] <- "—"
      base[["no %"]] <- nrow(part)
      return(base)
    }
    base[["mean change"]] <- dina_review_percent(mean(finite$percent))
    base[["min change"]] <- dina_review_percent(min(finite$percent))
    base[["max change"]] <- dina_review_percent(max(finite$percent))
    base[["no %"]] <- nrow(part) - nrow(finite)
    base
  })
  do.call(rbind, out)
}

dina_review_sna_pipeline_inputs <- function(script) {
  if (!file.exists(script)) stop("Missing configured SNA pipeline task: ", script, call. = FALSE)
  lines <- readLines(script, warn = FALSE, encoding = "UTF-8")
  start <- grep("^[[:space:]]*qui[[:space:]]+use[[:space:]]+iso[[:space:]]+year[[:space:]]+uprofits_hh_ni", lines, perl = TRUE)
  if (length(start) != 1L) stop("Could not identify the CEI import in ", basename(script), call. = FALSE)
  end <- start[[1L]] - 1L + which(grepl("using", lines[start[[1L]]:length(lines)], fixed = TRUE))[[1L]]
  imported <- unlist(regmatches(lines[start[[1L]]:end], gregexpr("[[:alnum:]_]+_cei", lines[start[[1L]]:end], perl = TRUE)), use.names = FALSE)
  unique(imported[nzchar(imported)])
}

dina_review_sna_review_focus <- function(root = dina_repo_root()) {
  config <- dina_read_yaml(file.path(root, "config", "country_sna_explorer.yml"))
  focus <- config$review_focus %||% list()
  groups <- focus$groups %||% list()
  if (!identical(focus$scope %||% "", "direct_pipeline_inputs") || !length(groups) || !nzchar(focus$script %||% "")) {
    stop("Country-SNA direct-pipeline review focus is not configured.", call. = FALSE)
  }
  entries <- dina_review_bind(lapply(seq_along(groups), function(i) {
    group <- groups[[i]]
    measures <- as.character(group$measures %||% character())
    if (!length(measures) || !nzchar(group$title %||% "")) stop("Each SNA review-focus group needs a title and measures.", call. = FALSE)
    data.frame(measure = measures, group = group$title, group_order = i, measure_order = seq_along(measures), stringsAsFactors = FALSE)
  }))
  if (anyDuplicated(entries$measure)) stop("Country-SNA review-focus measures must be unique.", call. = FALSE)
  actual <- sort(dina_review_sna_pipeline_inputs(file.path(root, focus$script)))
  declared <- sort(entries$measure)
  if (!identical(declared, actual)) {
    stop("Country-SNA review focus does not match the CEI inputs in ", focus$script,
      ". Update config/country_sna_explorer.yml before reviewing revisions.", call. = FALSE)
  }
  list(task_id = focus$task_id %||% "pipeline task", entries = entries)
}

dina_review_sna_focus_rows <- function(rows, focus, field = "measure") {
  if (!nrow(rows) || !field %in% names(rows)) return(rows[FALSE, , drop = FALSE])
  rows[rows[[field]] %in% focus$entries$measure, , drop = FALSE]
}

dina_review_sna_focus_problems <- function(rows, focus) {
  if (!nrow(rows)) return(rows)
  direct <- if ("variable" %in% names(rows)) rows$variable %in% focus$entries$measure else rep(FALSE, nrow(rows))
  # A blocker without a variable is structural: it prevents extraction for the
  # source as a whole and therefore also for every direct pipeline input.
  global_blocker <- (!"variable" %in% names(rows) | is.na(rows$variable) | !nzchar(rows$variable)) &
    "severity" %in% names(rows) & rows$severity == "Blocker"
  rows[direct | global_blocker, , drop = FALSE]
}

dina_review_sna_focused_revision_summary <- function(rows, focus) {
  if (!nrow(rows)) return(data.frame(stringsAsFactors = FALSE))
  entries <- focus$entries[focus$entries$measure %in% unique(rows$measure), , drop = FALSE]
  entries <- entries[order(entries$group_order, entries$measure_order), , drop = FALSE]
  out <- lapply(seq_len(nrow(entries)), function(i) {
    entry <- entries[i, , drop = FALSE]
    part <- rows[rows$measure == entry$measure[[1L]], , drop = FALSE]
    finite <- part[is.finite(part$percent), , drop = FALSE]
    mean_change <- "N/A"
    range <- ""
    note <- ""
    if (nrow(finite)) {
      mean_change <- dina_review_percent(mean(finite$percent))
      if (nrow(finite) > 1L) {
        low <- dina_review_percent(min(finite$percent))
        high <- dina_review_percent(max(finite$percent))
        if (!identical(low, high)) range <- paste(low, "to", high)
      }
      omitted <- nrow(part) - nrow(finite)
      if (omitted) note <- sprintf("%s zero baseline%s", omitted, if (omitted == 1L) "" else "s")
    } else {
      note <- sprintf("%s zero baseline%s", nrow(part), if (nrow(part) == 1L) "" else "s")
    }
    data.frame(
      group = entry$group[[1L]], group_order = entry$group_order[[1L]], measure_order = entry$measure_order[[1L]],
      variable = sub("_cei$", "", entry$measure[[1L]]), years = dina_review_sna_year_span(part$year), rows = nrow(part),
      `mean change` = mean_change, range = range, note = note, stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  do.call(rbind, out)
}

dina_review_whole_year_additions <- function(values) {
  if (!nrow(values) || !all(c("country", "year", "result") %in% names(values))) {
    return(data.frame(country = character(), year = integer(), change = character(), observations = integer(), unresolved = integer(), stringsAsFactors = FALSE))
  }
  available_results <- c("value added", "new observation")
  available <- values[values$result %in% available_results & !is.na(values$country) & !is.na(values$year), , drop = FALSE]
  if (!nrow(available)) return(data.frame(country = character(), year = integer(), change = character(), observations = integer(), unresolved = integer(), stringsAsFactors = FALSE))
  keys <- unique(available[c("country", "year")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    country <- keys$country[[i]]
    year <- keys$year[[i]]
    all_rows <- values[values$country == country & values$year == year, , drop = FALSE]
    comparable_other <- all_rows$result %in% c("unchanged", "revised", "value missing", "removed observation")
    # Extraction warnings (for example, a code that could not be read) do not
    # turn a complete availability change into dozens of misleading rows. They
    # are counted explicitly instead, so the summary never claims full data
    # coverage when there are unresolved values.
    if (!nrow(all_rows) || any(comparable_other, na.rm = TRUE)) return(NULL)
    added <- all_rows[all_rows$result %in% available_results, , drop = FALSE]
    unresolved <- nrow(all_rows) - nrow(added)
    change <- if (all(added$result == "new observation")) {
      "whole year added"
    } else if (all(added$result == "value added")) {
      "year newly available"
    } else {
      "year availability expanded"
    }
    data.frame(country = country, year = as.integer(year), change = change,
      observations = nrow(added), unresolved = unresolved, stringsAsFactors = FALSE)
  })
  dina_review_bind(rows)
}

dina_review_without_whole_year_additions <- function(rows, whole_years) {
  if (!nrow(rows) || !nrow(whole_years)) return(rows)
  keys <- paste(whole_years$country, whole_years$year, sep = "\r")
  rows[!paste(rows$country, rows$year, sep = "\r") %in% keys, , drop = FALSE]
}

dina_review_change_year_label <- function(years) {
  years <- sort(unique(suppressWarnings(as.integer(years))))
  years <- years[is.finite(years)]
  if (!length(years)) return("—")
  dina_admin_pit_year_label(paste(years, collapse = ","), max_chars = 24L)
}

# A source review can contain one observation for every artifact, measure, and
# year. The terminal review is for decisions, not a raw observation dump: this
# helper keeps the complete rows in review_values.csv and presents a compact
# country/artifact/measure summary instead.
dina_review_artifact_change_summary <- function(rows, revisions = FALSE) {
  required <- c("country", "year", "measure")
  if (!nrow(rows) || !all(required %in% names(rows))) return(data.frame(stringsAsFactors = FALSE))
  country <- as.character(rows$country)
  country[is.na(country) | !nzchar(country)] <- "Unspecified"
  artifact <- if ("source_id" %in% names(rows)) as.character(rows$source_id) else rep("source", nrow(rows))
  artifact[is.na(artifact) | !nzchar(artifact)] <- "source"
  component <- if ("component" %in% names(rows)) as.character(rows$component) else rep("", nrow(rows))
  component[is.na(component)] <- ""
  measure <- as.character(rows$measure)
  measure[is.na(measure) | !nzchar(measure)] <- "value"
  keys <- paste(country, artifact, component, measure, sep = "\r")
  parts <- split(seq_len(nrow(rows)), keys)
  out <- lapply(parts, function(index) {
    part <- rows[index, , drop = FALSE]
    answer <- data.frame(
      country = country[[index[[1L]]]], artifact = artifact[[index[[1L]]]],
      component = component[[index[[1L]]]], measure = measure[[index[[1L]]]],
      years = dina_review_change_year_label(part$year), rows = nrow(part), stringsAsFactors = FALSE
    )
    if (isTRUE(revisions)) {
      finite <- part[is.finite(part$percent), , drop = FALSE]
      answer[["mean change"]] <- if (nrow(finite)) dina_review_percent(mean(finite$percent)) else "N/A"
      if (nrow(finite) > 1L) {
        low <- dina_review_percent(min(finite$percent))
        high <- dina_review_percent(max(finite$percent))
        answer$range <- if (identical(low, high)) "" else paste(low, "to", high)
      } else answer$range <- ""
      if (nrow(finite) < nrow(part)) answer$note <- sprintf("%s zero baseline", nrow(part) - nrow(finite)) else answer$note <- ""
    }
    answer
  })
  out <- dina_review_bind(out)
  # A one-artifact review (notably WID population) does not need to repeat the
  # same artifact name in every row.
  one_artifact <- nrow(out) && length(unique(out$artifact)) == 1L
  if (one_artifact) out$artifact <- NULL
  if (!any(nzchar(out$component))) out$component <- NULL
  if (nrow(out)) {
    ord <- if (one_artifact) order(out$country, out$component %||% "", out$measure) else order(out$country, out$artifact, out$component %||% "", out$measure)
    out[ord, , drop = FALSE]
  } else out
}

dina_review_compact_whole_year_additions <- function(whole_years) {
  if (!nrow(whole_years)) return(data.frame(stringsAsFactors = FALSE))
  parts <- split(whole_years, as.character(whole_years$country))
  out <- lapply(parts, function(part) {
    data.frame(
      country = part$country[[1L]], years = dina_review_change_year_label(part$year),
      `new values` = sum(part$observations, na.rm = TRUE), unresolved = sum(part$unresolved, na.rm = TRUE),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  out <- dina_review_bind(out)
  if (nrow(out)) out[order(out$country), , drop = FALSE] else out
}

dina_review_print_artifact_value_changes <- function(record, values, family) {
  if (!nrow(values) || !"result" %in% names(values)) {
    dina_cli_cat(dina_cli_dim("No direct value comparison was produced; see the review status and Problems for the blocking reason."))
    audit_path <- file.path(record$run, "tables", "review_values.csv")
    dina_cli_cat(dina_cli_dim("Complete row-level audit: "), audit_path)
    return(invisible(NULL))
  }
  tiny <- values$result == "revised" & is.finite(values$percent) & abs(values$percent) < 1
  whole_added <- dina_review_whole_year_additions(values)
  revised <- values[values$result == "revised", , drop = FALSE]
  partial_added <- values[values$result %in% c("value added", "new observation"), , drop = FALSE]
  partial_added <- dina_review_without_whole_year_additions(partial_added, whole_added)
  lost <- values[values$result %in% c("value missing", "removed observation"), , drop = FALSE]
  revision_note <- if (sum(tiny)) sprintf("%s numerical revisions (%s below 1%%, summarized only)", nrow(revised), sum(tiny)) else sprintf("%s numerical revisions", nrow(revised))
  dina_cli_cat(sprintf("%s unchanged overlaps; %s; %s newly available values; %s lost values.",
    sum(values$result == "unchanged"), revision_note,
    sum(values$result %in% c("value added", "new observation")), nrow(lost)))

  if (nrow(whole_added)) {
    unresolved <- dina_review_compact_whole_year_additions(whole_added)
    unresolved <- unresolved[unresolved$unresolved > 0L, , drop = FALSE]
    if (nrow(unresolved)) {
      dina_cli_section("Unresolved values in newly available years")
      dina_cli_cat(dina_cli_dim("Coverage already lists the added years; only unresolved values are shown here."))
      dina_print_data_frame_compact(unresolved, limit = nrow(unresolved))
    } else {
      dina_cli_cat(dina_cli_dim("Whole-year additions are already summarized in Coverage; no unresolved values were found."))
    }
  }
  if (nrow(revised)) {
    dina_cli_section("Revisions")
    dina_print_data_frame_compact(dina_review_artifact_change_summary(revised, revisions = TRUE), limit = nrow(revised))
  }
  if (nrow(partial_added)) {
    dina_cli_section("Partially newly available values")
    dina_print_data_frame_compact(dina_review_artifact_change_summary(partial_added), limit = nrow(partial_added))
  }
  if (nrow(lost)) {
    dina_cli_section("Lost values")
    dina_print_data_frame_compact(dina_review_artifact_change_summary(lost), limit = nrow(lost))
  }
  audit_path <- file.path(record$run, "tables", "review_values.csv")
  dina_cli_cat(dina_cli_dim("Complete row-level audit: "), audit_path)
  dina_cli_cat(dina_cli_dim("Preview: "), dina_cli_command(paste("dina sources table", family, "review_values")))
  invisible(NULL)
}

dina_review_print_admin_other_inputs <- function(record) {
  path <- file.path(record$run, "tables", "other_admin_inputs.csv")
  if (!file.exists(path)) return(invisible(NULL))
  rows <- dina_read_csv_table_or_empty(path)
  if (!nrow(rows)) return(invisible(NULL))
  dina_cli_section("Other administrative inputs")
  dina_cli_cat(dina_cli_dim("Non-PIT inputs used by the PIT cleaners. Incoming confirms that a replacement file was supplied; “unchanged in overlap” means accepted and incoming values were compared and matched."))
  dina_print_data_frame_compact(rows, limit = nrow(rows))
  dina_cli_cat(dina_cli_dim("Value audit: "), dina_cli_command("dina sources table admin aux_comparison_detail"))
  invisible(rows)
}

dina_review_print_wid_population_changes <- function(record, values) {
  dina_cli_cat(dina_cli_emphasis("Population estimates"))
  baseline <- unique(as.character(values$baseline %||% character()))
  baseline <- baseline[!is.na(baseline) & nzchar(baseline)]
  baseline_text <- if (identical(baseline, "legacy population input")) {
    "the legacy population input used by the preceding pipeline"
  } else {
    "the accepted WID population input"
  }
  dina_cli_cat(dina_cli_dim("Direct pipeline input. Incoming total- and adult-population estimates are compared with "), baseline_text,
    dina_cli_dim(", not with the final DINA series."))
  has_accepted <- nrow(values) && "old_value" %in% names(values) && any(is.finite(values$old_value))
  if (!has_accepted) {
    dina_cli_cat(dina_cli_dim("No revision can be calculated yet: Include accepts this first candidate as the WID population baseline; a later refresh will be compared with it."))
    audit_path <- file.path(record$run, "tables", "review_values.csv")
    dina_cli_cat(dina_cli_dim("Complete row-level audit: "), audit_path)
    dina_cli_cat(dina_cli_dim("Preview: "), dina_cli_command("dina sources table wid review_values"))
    return(invisible(NULL))
  }
  dina_review_print_artifact_value_changes(record, values, "wid")
  invisible(NULL)
}

dina_review_sna_unresolved_issue <- function(status) {
  labels <- c(
    duplicate_conflict = "Conflicting source cells",
    no_code_match = "Account not published separately",
    non_numeric = "Source cell is blank",
    missing_role = "Sector column not found",
    missing_sheet = "Expected sheet not found",
    missing_file = "Source file unavailable"
  )
  unname(labels[[as.character(status)]]) %||% "Source value unresolved"
}

dina_review_sna_unresolved_additions <- function(include_detail, whole_years) {
  required <- c("country", "year", "variable", "expected_status", "extract_status")
  if (!nrow(include_detail) || !nrow(whole_years) || !all(required %in% names(include_detail))) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  keys <- paste(whole_years$country, whole_years$year, sep = "\r")
  statuses <- c("duplicate_conflict", "no_code_match", "non_numeric", "missing_role", "missing_sheet", "missing_file")
  rows <- include_detail[
    paste(include_detail$country, include_detail$year, sep = "\r") %in% keys &
      include_detail$expected_status == "expected_value" &
      include_detail$extract_status %in% statuses,
    , drop = FALSE
  ]
  if (!nrow(rows)) return(data.frame(stringsAsFactors = FALSE))
  by_year <- dina_review_bind(lapply(split(rows, paste(rows$country, rows$year, rows$extract_status, sep = "\r")), function(part) {
    variables <- sort(unique(as.character(part$variable)))
    data.frame(
      country = part$country[[1L]], year = as.integer(part$year[[1L]]),
      issue = dina_review_sna_unresolved_issue(part$extract_status[[1L]]),
      inputs = paste(sub("_cei$", "", variables), collapse = ", "),
      signature = paste(variables, collapse = "\r"), values = nrow(part),
      stringsAsFactors = FALSE
    )
  }))
  grouped <- lapply(split(by_year, paste(by_year$country, by_year$issue, by_year$signature, sep = "\r")), function(part) {
    data.frame(
      country = part$country[[1L]], years = dina_review_sna_year_span(part$year),
      issue = part$issue[[1L]], inputs = part$inputs[[1L]], values = sum(part$values),
      stringsAsFactors = FALSE
    )
  })
  out <- dina_review_bind(grouped)
  out[order(out$country, out$issue, out$years), , drop = FALSE]
}

dina_review_sna_compact_country_column <- function(rows) {
  if (!nrow(rows)) return(rows)
  country <- as.character(rows$country)
  country[c(FALSE, country[-1L] == country[-length(country)])] <- ""
  rows$country <- country
  rows
}

dina_review_sna_material_revision_variables <- function(rows) {
  if (!nrow(rows)) return(character())
  variables <- unique(as.character(rows$measure[!is.na(rows$measure) & nzchar(rows$measure)]))
  variables[vapply(variables, function(variable) {
    changes <- rows$percent[rows$measure == variable]
    changes <- changes[is.finite(changes)]
    # Keep zero/unavailable baselines visible. For ordinary percentage changes,
    # a variable earns a row through a material average or an individual large
    # revision; routine sub-1% noise remains in the audit only.
    !length(changes) || abs(mean(changes)) >= 1 || any(abs(changes) > 5)
  }, logical(1))]
}

dina_review_sna_compact_category_cells <- function(summary) {
  if (!nrow(summary)) return(data.frame(stringsAsFactors = FALSE))
  summary <- summary[order(summary$group_order, summary$measure_order), , drop = FALSE]
  category <- as.character(summary$group)
  repeated <- c(FALSE, category[-1L] == category[-length(category)])
  category[repeated] <- ""
  out <- data.frame(
    category = category,
    variable = summary$variable,
    years = summary$years,
    rows = summary$rows,
    `mean change` = summary[["mean change"]],
    range = summary$range,
    note = summary$note,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (all(!nzchar(out$range))) out$range <- NULL
  if (all(!nzchar(out$note))) out$note <- NULL
  out
}

dina_review_sna_revision_summary <- function(rows, focus) {
  countries <- sort(unique(as.character(rows$country[!is.na(rows$country) & nzchar(rows$country)])))
  summaries <- lapply(seq_along(countries), function(i) {
    country <- countries[[i]]
    country_rows <- rows[rows$country == country, , drop = FALSE]
    shown <- dina_review_sna_material_revision_variables(country_rows)
    summary <- dina_review_sna_focused_revision_summary(country_rows[country_rows$measure %in% shown, , drop = FALSE], focus)
    if (!nrow(summary)) return(NULL)
    summary$country <- country
    summary$country_order <- i
    summary
  })
  dina_review_bind(summaries)
}

dina_review_sna_compact_revision_cells <- function(summary) {
  if (!nrow(summary)) return(data.frame(stringsAsFactors = FALSE))
  summary <- summary[order(summary$country_order, summary$group_order, summary$measure_order), , drop = FALSE]
  country <- as.character(summary$country)
  new_country <- c(TRUE, country[-1L] != country[-length(country)])
  category <- as.character(summary$group)
  repeated_category <- c(FALSE, !new_country[-1L] & category[-1L] == category[-length(category)])
  category[repeated_category] <- ""
  country[!new_country] <- ""
  out <- data.frame(
    country = country,
    category = category,
    variable = summary$variable,
    years = summary$years,
    rows = summary$rows,
    `mean change` = summary[["mean change"]],
    range = summary$range,
    note = summary$note,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (all(!nzchar(out$range))) out$range <- NULL
  if (all(!nzchar(out$note))) out$note <- NULL
  out
}

dina_review_print_sna_change_block <- function(country, kind, rows, focus = NULL) {
  if (!nrow(rows)) return(invisible(NULL))
  title <- switch(kind,
    revised = "Revisions",
    available = "Newly available",
    lost = "Lost values"
  )
  if (identical(kind, "revised")) {
    shown_variables <- dina_review_sna_material_revision_variables(rows)
    summary <- dina_review_sna_focused_revision_summary(rows[rows$measure %in% shown_variables, , drop = FALSE], focus)
    # A country with no material direct input is deliberately absent from the
    # normal review. Its complete evidence remains in the audit table.
    if (!nrow(summary)) return(invisible(NULL))
    dina_cli_cat(dina_cli_emphasis(sprintf("%s — %s", country, title)))
    dina_print_data_frame_compact(dina_review_sna_compact_category_cells(summary), limit = nrow(summary))
  } else {
    dina_cli_cat(dina_cli_emphasis(sprintf("%s — %s", country, title)))
    dina_cli_cat("  ", dina_cli_dim("Affected observations by variable."))
    summary <- dina_review_sna_variable_summary(rows)
    dina_print_data_frame_compact(summary, limit = nrow(summary))
  }
  invisible(NULL)
}

dina_review_print_sna_value_changes <- function(values, confirmed_missing, include_detail, record, root = dina_repo_root()) {
  focus <- dina_review_sna_review_focus(root)
  revised <- values[values$result == "revised", , drop = FALSE]
  focused_revised <- revised[revised$measure %in% focus$entries$measure, , drop = FALSE]
  available <- values[values$result %in% c("value added", "new observation"), , drop = FALSE]
  lost <- values[values$result %in% c("value missing", "removed observation"), , drop = FALSE]
  whole_added <- dina_review_whole_year_additions(values)
  available_total <- nrow(available)
  available <- dina_review_without_whole_year_additions(available, whole_added)
  if (nrow(confirmed_missing) && nrow(lost)) {
    confirmed_key <- paste(confirmed_missing$country, confirmed_missing$year, sep = "\r")
    lost <- lost[!paste(lost$country, lost$year, sep = "\r") %in% confirmed_key, , drop = FALSE]
  }
  dina_cli_cat(
    dina_cli_dim("Direct-input changes — unchanged overlaps: "), sum(values$result == "unchanged"),
    dina_cli_dim("  revisions: "), nrow(revised),
    dina_cli_dim("  Newly available: "), available_total,
    dina_cli_dim("  Lost: "), nrow(lost)
  )
  dina_cli_cat("  ", dina_cli_dim(sprintf("Direct pipeline inputs (%s): ", focus$task_id)), nrow(focused_revised), " revisions across ", nrow(focus$entries), " measures.")
  dina_cli_cat("  ", dina_cli_dim("Configured in config/country_sna_explorer.yml → review_focus. Revision tables show |mean change| ≥1% or any individual change above 5%."))
  unresolved_additions <- dina_review_sna_unresolved_additions(include_detail, whole_added)
  if (nrow(unresolved_additions)) {
    dina_cli_section("Unresolved source values in newly available years")
    dina_cli_cat("  ", dina_cli_dim("Coverage already lists the added years; this table shows only source values that could not be resolved."))
    dina_print_data_frame_compact(dina_review_sna_compact_country_column(unresolved_additions), limit = nrow(unresolved_additions))
    unresolved_table <- if (file.exists(file.path(record$run, "tables", "unresolved_new_values.csv"))) "unresolved_new_values" else "include_detail"
    dina_cli_cat("  ", dina_cli_dim("Review table: "), dina_cli_command(paste("dina sources table sna", unresolved_table)))
  }
  revision_summary <- dina_review_sna_revision_summary(focused_revised, focus)
  if (nrow(revision_summary)) {
    dina_cli_section("Revisions")
    dina_print_data_frame_compact(dina_review_sna_compact_revision_cells(revision_summary), limit = nrow(revision_summary))
  }
  countries <- sort(unique(c(available$country, lost$country)))
  countries <- countries[!is.na(countries) & nzchar(countries)]
  for (country in countries) {
    for (kind in c("available", "lost")) {
      rows <- switch(kind,
        available = available[available$country == country, , drop = FALSE],
        lost = lost[lost$country == country, , drop = FALSE]
      )
      dina_review_print_sna_change_block(country, kind, rows, focus = focus)
    }
  }
  if (nrow(confirmed_missing)) {
    dina_cli_section("Confirmed whole-year absences")
    for (i in seq_len(nrow(confirmed_missing))) {
      dina_cli_cat("  ", dina_cli_emphasis(sprintf("%s %s", confirmed_missing$country[[i]], confirmed_missing$year[[i]])),
        " ", dina_cli_dim(confirmed_missing$reason[[i]]))
    }
  }
  has_direct_audit <- file.exists(file.path(record$run, "tables", "direct_input_changes_audit.csv"))
  audit_table <- if (has_direct_audit) "direct_input_changes_audit" else "value_changes_audit"
  audit_path <- file.path(record$run, "tables", paste0(audit_table, ".csv"))
  audit_label <- if (has_direct_audit) "Direct-input audit: " else "Complete audit (saved review predates the direct-input audit): "
  dina_cli_cat("  ", dina_cli_dim(audit_label), audit_path)
  dina_cli_cat("  ", dina_cli_dim("Preview: "), dina_cli_command(paste("dina sources table sna", audit_table)))
  invisible(NULL)
}

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
    "missing_pipeline_columns",
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

dina_review_problem_triage_kind <- function(severity, status, extract_status = NA_character_) {
  status <- as.character(status %||% "")
  extract_status <- as.character(extract_status %||% "")
  if (is.na(status)) status <- ""
  if (is.na(extract_status)) extract_status <- ""
  if (identical(severity, "Blocker")) {
    return(switch(status,
      ambiguous_destination = list(section = "Blocks inclusion", issue = "No safe destination", next_step = "Add an explicit destination mapping."),
      ambiguous_registry = list(section = "Blocks inclusion", issue = "Ambiguous source match", next_step = "Resolve the source registry match."),
      stage_failed = list(section = "Blocks inclusion", issue = "Could not stage incoming file", next_step = "Check the file and staging path."),
      missing_static_dependency = list(section = "Blocks inclusion", issue = "Required source data is not accepted", next_step = "Complete the required source review, then explore this family again."),
      missing_dependency = list(section = "Blocks inclusion", issue = "Required source data is not accepted", next_step = "Complete the required source review, then explore this family again."),
      stata_not_configured = list(section = "Blocks inclusion", issue = "Stata is not configured", next_step = "Open Configuration, review the detected Stata command, confirm it, then explore this family again."),
      stata_cleaner_failed = list(section = "Blocks inclusion", issue = "Stata cleaner failed", next_step = "Inspect the Stata cleaner log, correct the reported error, then explore this family again."),
      stata_completed_without_expected_output = list(section = "Blocks inclusion", issue = "Stata produced no cleaned workbook", next_step = "Inspect the Stata cleaner log and generated paths, then explore this family again."),
      missing_clean_output = list(section = "Blocks inclusion", issue = "Staged cleaner did not produce its output", next_step = "Resolve the preceding source or Stata check, then explore this family again."),
      missing_pipeline_columns = list(section = "Blocks inclusion", issue = "Direct pipeline input is missing", next_step = "Use the configured survey input audit to identify the missing field."),
      source_checks_blocked = list(section = "Blocks inclusion", issue = "Required checks have not run", next_step = "Resolve the more specific block listed above, then explore this family again."),
      list(section = "Blocks inclusion", issue = "Source review could not complete", next_step = "Inspect the full problems audit.")))
  }
  switch(extract_status,
    zero_treated_missing = list(section = "Needs a decision", issue = "Published zero policy", next_step = "Confirm the zero-as-missing policy."),
    no_code_match = list(section = "Recorded source limitations", issue = "Account not published separately", next_step = "No action unless another source provides the breakdown."),
    non_numeric = list(section = "Recorded source limitations", issue = "Source cell is blank", next_step = "No action unless a usable source value is available."),
    missing_file = list(section = "Needs investigation", issue = "Source file unavailable", next_step = "Check the incoming source set."),
    missing_sheet = list(section = "Needs investigation", issue = "Expected sheet not found", next_step = "Check the source layout and sheet rule."),
    no_role_match = list(section = "Needs investigation", issue = "Sector column not found", next_step = "Check the source layout and role mapping."),
    ratio_missing_input = list(section = "Recorded source limitations", issue = "Derived input unavailable", next_step = "Follows the missing primitive input."),
    list(section = "Needs investigation",
      issue = if (identical(status, "warning_structure_review")) "Incoming source structure was not fully checked" else "Source check needs review",
      next_step = if (identical(status, "warning_structure_review")) "Check the source layout and reader support." else "Inspect the full problems audit."))
}

dina_review_problem_period <- function(years) {
  years <- suppressWarnings(as.integer(years))
  years <- sort(unique(years[is.finite(years)]))
  if (!length(years)) return("file-level")
  if (length(years) == 1L) return(as.character(years[[1L]]))
  sprintf("%s–%s (%s years)", years[[1L]], years[[length(years)]], length(years))
}

dina_review_problem_triage <- function(problems) {
  if (!nrow(problems)) return(data.frame(stringsAsFactors = FALSE))
  if (!"country" %in% names(problems)) problems$country <- NA_character_
  if (!"year" %in% names(problems)) problems$year <- NA_integer_
  if (!"extract_status" %in% names(problems)) problems$extract_status <- NA_character_
  problems$country[is.na(problems$country) | !nzchar(problems$country)] <- "Family"
  condition <- as.character(problems$status %||% rep("", nrow(problems)))
  reason <- as.character(problems$reason %||% rep("", nrow(problems)))
  condition[is.na(condition)] <- ""
  reason[is.na(reason)] <- ""
  reason_code <- gsub("[^a-z0-9]+", "_", tolower(reason))
  reason_code <- sub("^_|_$", "", reason_code)
  reason_code[is.na(reason_code)] <- ""
  # Several engines use a generic `blocked` status and put the actionable
  # condition in `reason`. Present that condition instead of the wrapper.
  generic <- condition %in% c("", "blocked", "failed", "warning", "check_following", "source_checks_blocked")
  use_reason <- generic & nzchar(reason_code)
  condition[use_reason] <- reason_code[use_reason]
  # Engines can emit a generic source_checks_blocked row in addition to the
  # dependency that caused it. Keep the generic row only when it is the sole
  # explanation for that country.
  specific <- condition %in% c("missing_static_dependency", "missing_dependency", "stata_not_configured", "stata_cleaner_failed", "stata_completed_without_expected_output", "missing_clean_output")
  redundant <- condition %in% "source_checks_blocked" & problems$country %in% problems$country[specific]
  if (any(redundant, na.rm = TRUE)) {
    problems <- problems[!redundant, , drop = FALSE]
    condition <- condition[!redundant]
  }
  kinds <- lapply(seq_len(nrow(problems)), function(i) {
    dina_review_problem_triage_kind(problems$severity[[i]], condition[[i]], problems$extract_status[[i]] %||% NA_character_)
  })
  problems$.section <- vapply(kinds, `[[`, character(1), "section")
  problems$.issue <- vapply(kinds, `[[`, character(1), "issue")
  problems$.next_step <- vapply(kinds, `[[`, character(1), "next_step")
  command <- as.character(problems$next_command %||% rep("", nrow(problems)))
  command[is.na(command)] <- ""
  problems$.next_step[nzchar(command)] <- paste("Run", command[nzchar(command)])
  groups <- split(problems, paste(problems$.section, problems$country, problems$.issue, problems$.next_step, sep = "\r"))
  rows <- lapply(groups, function(group) {
    data.frame(
      country = group$country[[1L]],
      issue = group$.issue[[1L]],
      period = dina_review_problem_period(group$year %||% integer()),
      checks = nrow(group),
      next_step = group$.next_step[[1L]],
      section = group$.section[[1L]],
      stringsAsFactors = FALSE
    )
  })
  out <- dina_review_bind(rows)
  out[order(match(out$section, c("Blocks inclusion", "Needs a decision", "Needs investigation", "Recorded source limitations")), out$country, out$issue), , drop = FALSE]
}

dina_review_survey_problem_triage <- function(problems) {
  if (!nrow(problems)) return(dina_review_problem_triage(problems))
  status <- tolower(gsub("[^a-z0-9]+", "_", as.character(problems$status %||% rep("", nrow(problems)))))
  reason <- tolower(gsub("[^a-z0-9]+", "_", as.character(problems$reason %||% rep("", nrow(problems)))))
  target <- status == "missing_arg_population_target" | reason == "missing_arg_population_target"
  if (!any(target, na.rm = TRUE)) return(dina_review_problem_triage(problems))
  cascade <- status == "blocked_no_observed_surveys" | reason == "blocked_no_observed_surveys"
  other <- problems[!(target | cascade), , drop = FALSE]
  target_rows <- problems[target, , drop = FALSE]
  target_rows$country[is.na(target_rows$country) | !nzchar(target_rows$country)] <- "ARG"
  target_summary <- dina_review_bind(lapply(split(target_rows, target_rows$country), function(rows) {
    data.frame(
      country = rows$country[[1L]],
      issue = "Population benchmark is not accepted",
      period = dina_review_problem_period(rows$year),
      checks = nrow(rows),
      next_step = "Review and include WID population, then explore surveys again.",
      section = "Blocks inclusion",
      stringsAsFactors = FALSE
    )
  }))
  rownames(target_summary) <- NULL
  out <- dina_review_bind(list(dina_review_problem_triage(other), target_summary))
  rownames(out) <- NULL
  out[order(match(out$section, c("Blocks inclusion", "Needs a decision", "Needs investigation", "Recorded source limitations")), out$country, out$issue), , drop = FALSE]
}

dina_review_survey_population_problem_note <- function(problems, root = NULL) {
  if (!nrow(problems)) return(invisible(NULL))
  status <- tolower(gsub("[^a-z0-9]+", "_", as.character(problems$status %||% rep("", nrow(problems)))))
  reason <- tolower(gsub("[^a-z0-9]+", "_", as.character(problems$reason %||% rep("", nrow(problems)))))
  rows <- problems[status == "missing_arg_population_target" | reason == "missing_arg_population_target", , drop = FALSE]
  if (!nrow(rows)) return(invisible(NULL))
  details <- as.character(rows$detail %||% "")
  matches <- regmatches(details, regexpr("input_data/[[:alnum:]_./-]+\\.dta", details))
  canonical <- matches[nzchar(matches)]
  canonical <- if (length(canonical)) canonical[[1L]] else "input_data/wid/population_total_adult_npopul.dta"
  canonical_exists <- !is.null(root) && file.exists(file.path(root, canonical))
  staged <- if (!is.null(root)) file.path(root, "input_data", "_new", "wid", basename(canonical)) else ""
  dina_cli_cat(dina_cli_dim(sprintf(
    "ARG's survey files and direct inputs are valid. Their population normalization needs the accepted WID series at %s, which is %s.",
    canonical, if (canonical_exists) "missing Argentina targets for these years" else "not present"
  )))
  if (!canonical_exists && nzchar(staged) && file.exists(staged)) {
    dina_cli_cat(dina_cli_dim("A staged WID replacement is available, but is deliberately not used to validate another unaccepted source. Review/include WID first, then re-run Surveys."))
  }
  invisible(NULL)
}

dina_review_print_problem_triage <- function(problems, family, triage = NULL, root = NULL) {
  triage <- triage %||% if (identical(family, "surveys")) dina_review_survey_problem_triage(problems) else dina_review_problem_triage(problems)
  if (!nrow(triage)) {
    dina_cli_cat("No review issues require attention.")
    return(invisible(triage))
  }
  for (section in c("Blocks inclusion", "Needs a decision", "Needs investigation", "Recorded source limitations")) {
    rows <- triage[triage$section == section, , drop = FALSE]
    if (!nrow(rows)) next
    dina_cli_section(section)
    dina_print_data_frame_compact(rows[c("country", "issue", "period", "checks", "next_step")], limit = nrow(rows))
  }
  if (identical(family, "surveys")) dina_review_survey_population_problem_note(problems, root)
  dina_cli_cat(dina_cli_dim("Full technical evidence: "), dina_cli_command(sprintf("dina sources table %s problems", family)))
  invisible(triage)
}

dina_review_survey_missing_input_summary <- function(audit) {
  rows <- audit[audit$source_set == "incoming" & audit$status %in% c("missing", "unresolved"), , drop = FALSE]
  if (!nrow(rows)) return(rows)
  dina_review_bind(lapply(split(rows, paste(rows$country, rows$input_group, rows$status, sep = "\r")), function(part) {
    data.frame(
      country = part$country[[1L]], category = part$input_group[[1L]],
      variables = paste(sort(unique(part$variable)), collapse = ", "),
      years = dina_review_sna_year_span(part$year), files = length(unique(part$rel)),
      issue = if (part$status[[1L]] == "missing") "missing" else "could not inspect",
      stringsAsFactors = FALSE
    )
  }))
}

dina_review_survey_input_table <- function(summary, expected_inputs = NA_integer_) {
  if (!nrow(summary)) return(summary)
  expected <- suppressWarnings(as.integer(expected_inputs[[1L]]))
  checked <- suppressWarnings(as.integer(summary$inputs_checked))
  missing <- suppressWarnings(as.integer(summary$missing))
  unresolved <- suppressWarnings(as.integer(summary$unresolved))
  status <- ifelse(
    missing > 0L & unresolved > 0L,
    sprintf("%s missing; %s unresolved", missing, unresolved),
    ifelse(missing > 0L, sprintf("%s missing", missing),
      ifelse(unresolved > 0L, sprintf("%s unresolved", unresolved), "ready"))
  )
  data.frame(
    country = summary$country,
    years = vapply(summary$years, function(years) dina_admin_pit_year_label(years, max_chars = 18L), character(1)),
    files = summary$files,
    inputs = if (!is.na(expected) && is.finite(expected) && expected > 0L) sprintf("%s/%s", checked, expected) else as.character(checked),
    status = status,
    stringsAsFactors = FALSE
  )
}

dina_review_survey_change_range <- function(values, suffix, digits = 1L) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (!length(values)) return("not comparable")
  render <- function(value) {
    sign <- if (value > 0) "+" else ""
    paste0(sign, formatC(value, format = "f", digits = digits, big.mark = ","), suffix)
  }
  low <- min(values)
  high <- max(values)
  if (identical(round(low, digits), round(high, digits))) return(render(low))
  paste(render(low), render(high), sep = " to ")
}

dina_review_survey_source_change_table <- function(comparison, scope = list()) {
  countries <- toupper(as.character(scope$countries %||% character()))
  comparison <- dina_review_sna_filter_scope(comparison, scope)
  if (!length(countries) && nrow(comparison) && "country" %in% names(comparison)) {
    countries <- sort(unique(toupper(as.character(comparison$country))))
  }
  if (!length(countries)) return(data.frame(stringsAsFactors = FALSE))
  dina_review_bind(lapply(countries, function(country) {
    rows <- comparison[toupper(as.character(comparison$country)) == country, , drop = FALSE]
    overlap <- rows[rows$comparison_status %in% c("retro_overlap_same", "retro_overlap_changed"), , drop = FALSE]
    changed <- overlap[overlap$comparison_status == "retro_overlap_changed", , drop = FALSE]
    weight_change <- if (nrow(changed)) {
      100 * changed$sum_fep_diff / changed$canonical_sum_fep
    } else numeric()
    adult_share_change <- if (nrow(changed)) changed$adult_share_diff else numeric()
    row_changes <- if (nrow(changed)) sum(
      is.finite(changed$incoming_row_count) & is.finite(changed$canonical_row_count) &
        changed$incoming_row_count != changed$canonical_row_count
    ) else 0L
    added_columns <- as.character(changed$added_columns %||% character())
    dropped_columns <- as.character(changed$dropped_columns %||% character())
    added_columns[is.na(added_columns)] <- ""
    dropped_columns[is.na(dropped_columns)] <- ""
    layout_changes <- if (nrow(changed)) sum(nzchar(added_columns) | nzchar(dropped_columns)) else 0L
    data.frame(
      country = country,
      compared = if (nrow(overlap)) nrow(overlap) else "—",
      changed = if (nrow(overlap)) nrow(changed) else "—",
      `row changes` = if (nrow(overlap)) row_changes else "—",
      `layout changes` = if (nrow(overlap)) layout_changes else "—",
      `weight Δ` = dina_review_survey_change_range(weight_change, "%"),
      `adult share Δ` = dina_review_survey_change_range(adult_share_change, " pp"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}

dina_review_print_survey_value_changes <- function(record, values, scope = list()) {
  read_saved <- function(table) {
    path <- file.path(record$run, "tables", paste0(table, ".csv"))
    if (file.exists(path)) dina_review_table(record, table) else data.frame(stringsAsFactors = FALSE)
  }
  summary <- read_saved("pipeline_input_summary")
  audit <- read_saved("pipeline_input_audit")
  focus <- read_saved("pipeline_input_focus")
  source_comparison <- read_saved("survey_source_comparison")
  countries <- toupper(as.character(scope$countries %||% character()))
  years <- suppressWarnings(as.integer(scope$years %||% integer()))
  if (length(countries) && nrow(summary) && "country" %in% names(summary)) summary <- summary[toupper(summary$country) %in% countries, , drop = FALSE]
  audit <- dina_review_sna_filter_scope(audit, scope)
  dina_cli_cat(dina_cli_dim("Survey microdata cannot be compared record-by-record safely. This review compares overlapping files' structure, sample size, weights and adult coverage."))
  dina_cli_section("Survey-file changes")
  changes <- dina_review_survey_source_change_table(source_comparison, scope)
  if (nrow(changes)) {
    dina_print_data_frame_compact(changes, limit = nrow(changes))
    dina_cli_cat(dina_cli_dim("Compared and changed refer only to years present in both incoming and accepted sources; new years are shown in Coverage."))
  } else {
    dina_cli_cat("No overlapping incoming survey files were available for comparison.")
  }
  dina_cli_cat(dina_cli_dim("Full file comparison: "), dina_cli_command("dina sources table surveys survey_source_comparison"))
  dina_cli_cat("")
  dina_cli_cat(dina_cli_emphasis("Direct pipeline inputs (01e-clean-survey-data)"))
  if (nrow(focus)) {
    dina_cli_cat("  ", dina_cli_dim(sprintf("%s declared inputs across %s groups.",
      length(unique(focus$variable)), length(unique(focus$input_group)))))
  }
  if (!nrow(summary)) {
    dina_cli_cat("No incoming primary survey files were available for direct-input inspection.")
  }
  missing <- dina_review_survey_missing_input_summary(audit)
  if (nrow(missing)) {
    dina_cli_section("Direct inputs needing attention")
    dina_print_data_frame_compact(missing[c("country", "category", "variables", "years", "files", "issue")], limit = nrow(missing))
  } else if (nrow(summary)) {
    dina_cli_cat(dina_cli_dim(sprintf(
      "All %s declared direct inputs are available in %s incoming survey files.",
      length(unique(focus$variable)), sum(suppressWarnings(as.integer(summary$files)), na.rm = TRUE)
    )))
  }
  if (nrow(audit)) {
    audit_path <- file.path(record$run, "tables", "pipeline_input_audit.csv")
    dina_cli_cat("  ", dina_cli_dim("Direct-input audit: "), audit_path)
    dina_cli_cat("  ", dina_cli_dim("Preview: "), dina_cli_command("dina sources table surveys pipeline-input-audit"))
  } else {
    dina_cli_cat(dina_cli_dim("This saved review predates the direct-input audit; explore surveys again to create it."))
  }
  invisible(NULL)
}

dina_review_show_rows <- function(rows, columns = names(rows), country = NULL, limit = 12L) {
  if (!is.null(country) && any(c("country", "review_country", "iso") %in% names(rows))) rows <- rows[toupper(dina_review_country(rows)) %in% toupper(country), , drop = FALSE]
  rows <- rows[intersect(columns, names(rows))]
  if (!nrow(rows) || !ncol(rows)) { dina_cli_cat("None reported."); return(invisible(rows)) }
  zero <- if (all(c("old_value", "new_value") %in% names(rows))) is.finite(rows$old_value) & rows$old_value == 0 & is.finite(rows$new_value) else rep(FALSE, nrow(rows))
  for (name in intersect(c("old_value", "new_value", "difference"), names(rows))) {
    rows[[name]] <- vapply(rows[[name]], dina_review_amount, character(1))
  }
  if ("percent" %in% names(rows)) {
    missing <- is.na(rows$percent)
    rows$percent <- vapply(rows$percent, dina_review_percent, character(1))
    rows$percent[missing] <- if ("result" %in% names(rows)) paste("N/A —", rows$result[missing]) else "N/A — values unavailable"
    rows$percent[zero] <- "N/A — baseline is zero"
  }
  for (name in names(rows)[grepl("years$", names(rows))]) rows[[name]] <- vapply(rows[[name]], function(x) {
    if (is.na(x) || !nzchar(as.character(x))) {
      if (name %in% c("old_years", "new_years")) "No extracted data" else "None"
    } else dina_admin_pit_year_label(x)
  }, character(1))
  numeric_columns <- names(rows)[vapply(rows, is.numeric, logical(1))]
  for (name in numeric_columns) {
    rows[[name]] <- vapply(rows[[name]], function(value) {
      if (is.na(value)) return(NA_character_)
      if (identical(name, "year")) return(formatC(value, format = "f", digits = 0L, big.mark = ""))
      if (abs(value - round(value)) < 1e-9) return(dina_review_plain_number(value, 0L))
      dina_review_plain_number(value, 3L)
    }, character(1))
  }
  labels <- c(old_years = "Accepted years", new_years = "Incoming years", extension_years = "Added years",
    coverage = "Accepted coverage", change = "Incoming change",
    overlap_years = "Years compared", missing_in_new_years = "Years missing from incoming data", old_value = "Accepted", new_value = "Incoming", percent = "Change %")
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

dina_review_print <- function(record, country = NULL, limit = 12L, root = dina_repo_root()) {
  stale <- !is.null(record$watch) && !identical(dina_review_watch(names(record$watch)), record$watch)
  label <- switch(record$status, all_good = "Review available", included = "Included", blocked = "Needs attention",
    check_following = "Needs attention", nothing_to_include = "Nothing to include", inclusion_failed = "Inclusion incomplete", including = "Inclusion incomplete", "Needs attention")
  dina_cli_cat(dina_cli_dim("Status: "), dina_cli_emphasis(label))
  dina_cli_cat(dina_cli_dim("Explore reviews incoming files. Include accepts a clean review; accepted source files stay unchanged until then."))
  scope <- if (is.list(record$scope)) record$scope else list()
  if (record$family %in% c("sna", "surveys", "wid")) {
    scope <- dina_review_config_display_scope(scope, root, family = dina_review_families()[[record$family]])
    if (!is.null(country) && length(scope$countries %||% character())) {
      selected <- toupper(as.character(country))
      scope$countries <- scope$countries[toupper(scope$countries) %in% selected]
    }
  }
  scope_years <- suppressWarnings(as.integer(unlist(scope$years %||% integer(), use.names = FALSE)))
  scope_years <- scope_years[is.finite(scope_years)]
  if (record$family %in% c("sna", "surveys", "wid") && length(scope_years)) {
    dina_cli_cat(dina_cli_dim("Scope: "), sprintf("%s · %s–%s · %s configured countries", scope$config_source %||% "effective configuration",
      min(scope_years), max(scope_years), length(scope$countries %||% character())))
  }
  if (stale) dina_cli_warn(paste("Evidence changed. Explore again:", "dina sources explore", record$family))
  if (!is.null(record$error)) dina_cli_warn(record$error)
  if (!is.null(record$confirmation)) dina_cli_cat(sprintf("Inclusion report: %s\nRestore: dina sources include %s --restore %s", record$confirmation, record$family, record$confirmation))
  if (is.null(record$confirmation) && !is.null(record$backup_root)) dina_cli_cat(paste("Backups:", record$backup_root))
  filter_country <- function(x) if (!is.null(country) && any(c("country", "review_country", "iso") %in% names(x))) x[toupper(dina_review_country(x)) %in% toupper(country), , drop = FALSE] else x
  coverage <- filter_country(dina_review_table(record, "review_coverage"))
  values <- filter_country(dina_review_table(record, "review_values"))
  problems <- filter_country(dina_review_table(record, "review_problems"))
  include_detail <- filter_country(dina_review_table(record, "include_detail"))
  focus <- if (identical(record$family, "sna")) dina_review_sna_review_focus(root) else NULL
  if (identical(record$family, "surveys")) {
    values <- dina_review_sna_filter_scope(values, scope)
    include_detail <- dina_review_sna_filter_scope(include_detail, scope)
    problems <- dina_review_sna_filter_scope(problems, scope)
    coverage <- dina_review_survey_scope_coverage(coverage, scope)
  }
  if (identical(record$family, "wid")) {
    metadata <- record$area_metadata %||% list()
    values <- dina_review_filter_display_scope(values, scope, metadata)
    include_detail <- dina_review_filter_display_scope(include_detail, scope, metadata)
    problems <- dina_review_filter_display_scope(problems, scope, metadata)
    if (nrow(coverage)) {
      coverage$country <- dina_review_country(coverage, metadata)
      coverage <- dina_review_filter_display_scope(coverage, scope, metadata)
    }
  }
  if (!is.null(focus)) {
    values <- dina_review_sna_filter_scope(values, scope)
    include_detail <- dina_review_sna_filter_scope(include_detail, scope)
    problems <- dina_review_sna_filter_scope(problems, scope)
    values <- dina_review_sna_focus_rows(values, focus)
    include_detail <- dina_review_sna_focus_rows(include_detail, focus, field = "variable")
    problems <- dina_review_sna_focus_problems(problems, focus)
    coverage <- dina_review_sna_scope_coverage(dina_review_coverage(NULL, values, "sna"), scope)
  }
  confirmed_path <- file.path(record$run, "tables", "confirmed_missing_years.csv")
  confirmed_missing <- if (identical(record$family, "sna") && file.exists(confirmed_path)) {
    filter_country(dina_read_csv_table_or_empty(confirmed_path))
  } else data.frame()
  if (identical(record$family, "sna")) confirmed_missing <- dina_review_sna_filter_scope(confirmed_missing, scope)
  areas <- dina_review_country(values)
  problem_triage <- if (identical(record$family, "surveys")) {
    dina_review_survey_problem_triage(problems)
  } else {
    dina_review_problem_triage(problems)
  }
  countries <- if (record$family %in% c("sna", "surveys", "wid") && length(scope$countries %||% character())) {
    toupper(as.character(scope$countries))
  } else {
    sort(unique(na.omit(c(coverage$country, areas, problem_triage$country))))
  }
  countries <- countries[!is.na(countries) & nzchar(countries)]
  wid_has_accepted <- identical(record$family, "wid") && nrow(values) && "old_value" %in% names(values) && any(is.finite(values$old_value))
  if (identical(record$family, "wid") && !wid_has_accepted) {
    dina_cli_section("What is new")
    dina_cli_cat(dina_cli_emphasis("First WID population candidate"))
    dina_cli_cat(dina_cli_dim("New to this repository: total- and adult-population estimates for every configured country and run year. There is no accepted WID population input to compare with yet."))
  } else {
    dina_cli_section(if (identical(record$family, "wid")) {
      "Population input summary"
    } else if (identical(record$family, "admin")) {
      "PIT — Country summary"
    } else {
      "Country summary"
    })
  }
  summary <- if (identical(record$family, "wid")) {
    dina_review_bind(lapply(countries, function(ct) {
      v <- values[areas %in% ct, , drop = FALSE]
      t <- problem_triage[problem_triage$country %in% ct, , drop = FALSE]
      if (!wid_has_accepted) {
        data.frame(Country = ct, `Candidate values` = nrow(v),
          Blocks = sum(t$section == "Blocks inclusion"), Limits = sum(t$section == "Recorded source limitations"), check.names = FALSE)
      } else {
        data.frame(Country = ct,
          New = sum(v$result %in% c("value added", "new observation")),
          Revised = sum(v$result == "revised"),
          Lost = sum(v$result %in% c("value missing", "removed observation")),
          Blocks = sum(t$section == "Blocks inclusion"), Limits = sum(t$section == "Recorded source limitations"), check.names = FALSE)
      }
    }))
  } else {
    dina_review_bind(lapply(countries, function(ct) {
      v <- values[areas %in% ct, , drop = FALSE]
      t <- problem_triage[problem_triage$country %in% ct, , drop = FALSE]
      data.frame(Country = ct, New = sum(v$result %in% c("value added", "new observation")), Revisions = sum(v$result == "revised"), Lost = sum(v$result %in% c("value missing", "removed observation")),
        Blocks = sum(t$section == "Blocks inclusion"), Decisions = sum(t$section %in% c("Needs a decision", "Needs investigation")),
        Limitations = sum(t$section == "Recorded source limitations"))
    }))
  }
  if (!identical(record$family, "wid") || wid_has_accepted) dina_review_show_rows(summary, limit = limit)
  dina_cli_section(if (identical(record$family, "admin")) "1. PIT coverage" else "1. Coverage")
  if (identical(record$family, "wid")) {
    if (!wid_has_accepted) {
      dina_cli_cat(dina_cli_dim("Incoming coverage for that first candidate."))
      coverage_display <- coverage[intersect(c("country", "new_years"), names(coverage))]
      names(coverage_display)[names(coverage_display) == "new_years"] <- "Candidate population coverage"
      coverage_display[["Candidate population coverage"]] <- vapply(
        coverage_display[["Candidate population coverage"]], dina_admin_pit_year_label, character(1), max_chars = 24L
      )
    } else {
      coverage_display <- dina_review_wid_compact_coverage(coverage, scope)
      names(coverage_display)[names(coverage_display) == "coverage"] <- "Accepted population"
      names(coverage_display)[names(coverage_display) == "change"] <- "Incoming population change"
      coverage_display <- coverage_display[intersect(c("country", "Accepted population", "Incoming population change"), names(coverage_display))]
    }
    dina_review_show_rows(coverage_display, limit = limit)
  } else if (any(c("old_years", "new_years") %in% names(coverage))) {
    coverage_display <- if (identical(record$family, "wid")) dina_review_wid_compact_coverage(coverage, scope) else dina_review_compact_coverage(coverage)
    coverage_columns <- c("country", "coverage", "change")
    dina_review_show_rows(coverage_display, coverage_columns, limit = limit)
  } else {
    dina_review_show_rows(coverage, c("country", "source_id", "extension_years", "missing_in_new_years"), limit = limit)
  }
  dina_cli_cat(dina_cli_dim("Year coverage does not imply complete variable coverage."))
  dina_cli_section(if (identical(record$family, "admin")) "2. PIT value changes" else "2. Value changes")
  if (identical(record$family, "sna")) {
    dina_review_print_sna_value_changes(values, confirmed_missing, include_detail, record, root = root)
  } else if (identical(record$family, "surveys")) {
    dina_review_print_survey_value_changes(record, values, scope)
  } else if (identical(record$family, "wid")) {
    dina_review_print_wid_population_changes(record, values)
  } else {
    if (identical(record$family, "admin")) {
      dina_cli_cat(dina_cli_dim("Harmonized PIT inputs are compared after applying the same cleaner to accepted and incoming sources. These are the bracket tables ready for interpolation, not raw publisher tables."))
    }
    dina_review_print_artifact_value_changes(record, values, record$family)
  }
  if (identical(record$family, "admin")) dina_review_print_admin_other_inputs(record)
  if (identical(record$family, "sna")) {
    dina_cli_cat(dina_cli_dim("Review scope: direct CEI inputs declared in config/country_sna_explorer.yml → review_focus."))
  } else dina_cli_cat(record$comparison_note %||% "Comparison scope unavailable.")
  dina_cli_section("3. Problems")
  dina_review_print_problem_triage(problems, record$family, triage = problem_triage, root = root)
  unexplained <- sum(values$result %in% "absence unexplained")
  if (unexplained && identical(record$family, "admin")) {
    absent <- values[values$result == "absence unexplained", , drop = FALSE]
    groups <- split(absent, paste(absent$country, absent$component, absent$measure, sep = "\r"))
    summary <- dina_review_bind(lapply(groups, function(part) data.frame(
      country = part$country[[1L]], component = part$component[[1L]], measure = part$measure[[1L]],
      years = dina_review_change_year_label(part$year), stringsAsFactors = FALSE
    )))
    dina_cli_cat(dina_cli_dim("Unavailable in both accepted and incoming harmonized PIT inputs (does not block inclusion):"))
    dina_print_data_frame_compact(summary, limit = nrow(summary))
    dina_cli_cat(dina_cli_dim("Details: missing-values."))
  } else if (unexplained) dina_cli_cat(sprintf("%s absent values have undetermined applicability; inspect missing-values.", unexplained))
  if (nzchar(record$comparison_error %||% "")) dina_cli_warn(record$comparison_error)
  if (record$status == "blocked" && !any(problems$severity == "Blocker") && !nzchar(record$comparison_error %||% "")) dina_cli_warn("The engine blocked inclusion; inspect its detailed assessment for the remaining condition.")
  if (identical(record$family, "sna")) {
    dina_cli_cat(dina_cli_dim("Details: coverage, audit, problems, and files are saved with this review."))
  } else if (identical(record$family, "wid")) {
    dina_cli_cat("\nDetails: dina sources table wid revisions|coverage|problems|files")
  } else dina_cli_cat(sprintf("\nDetails: dina sources table %s revisions|missing-values|coverage|problems|files", record$family))
  if (record$status == "all_good" && !stale) dina_cli_cat(paste("Ready to include:", "dina sources include", record$family))
  if (!record$status %in% c("included", "including", "inclusion_failed")) dina_cli_cat("Explore has not accepted sources or run the pipeline.")
  invisible(record)
}


dina_review_detail_columns <- function(table, rows) {
  if (table %in% c("revisions", "added-values")) return(setdiff(names(rows), c("absence_reason", "old_extract_status", "new_extract_status", "old_source_file", "new_source_file", "old_sheet", "new_sheet")))
  names(rows)
}
