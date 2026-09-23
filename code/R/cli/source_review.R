# Shared interaction for the existing source engines. Extraction contracts and
# inclusion guards remain owned by each family.

dina_review_families <- function() {
  c(sna = "National accounts", admin = "Administrative tax data", surveys = "Household surveys", wid = "WID")
}

dina_review_source_family_order <- function() c("wid", "surveys", "admin", "sna")

dina_review_source_prerequisites <- function(family) {
  switch(family,
    surveys = c(wid = "WID population"),
    admin = c(surveys = "Household surveys"),
    character()
  )
}

dina_review_source_prerequisite_rows <- function(root, family) {
  prerequisites <- dina_review_source_prerequisites(family)
  if (!length(prerequisites)) return(data.frame(stringsAsFactors = FALSE))
  dina_review_bind(lapply(names(prerequisites), function(required_family) {
    state <- dina_review_family_status(root, required_family)
    state_text <- switch(state$code,
      included = "Included and current",
      included_stale = "Included; recheck needed",
      ready = "Review ready; not included",
      unexplored = "Not yet explored",
      empty = "No local candidate",
      stale = "Explore again",
      failed = "Inclusion incomplete",
      attention = "Needs attention",
      nothing = "Nothing to include",
      state$label
    )
    action <- switch(state$code,
      included = "Ready",
      included_stale = sprintf("Re-explore %s", required_family),
      ready = sprintf("Include %s first", required_family),
      unexplored = sprintf("Explore then include %s", required_family),
      empty = sprintf("Prepare %s first", required_family),
      stale = sprintf("Explore %s again", required_family),
      failed = sprintf("Resolve %s first", required_family),
      attention = sprintf("Resolve %s first", required_family),
      nothing = "No action required",
      sprintf("Review %s", required_family)
    )
    data.frame(
      required = unname(prerequisites[[required_family]]),
      status = state_text,
      next_step = action,
      detail = state$reason,
      stringsAsFactors = FALSE
    )
  }))
}

dina_review_state_label <- function(state) {
  label <- as.character(state$label %||% "")
  if (!(state$code %in% c("included", "included_stale")) || !startsWith(label, "Included")) return(label)
  paste0(dina_cli_success("Included"), substr(label, nchar("Included") + 1L, nchar(label)))
}

dina_review_print_source_prerequisites <- function(root, family) {
  for (line in dina_review_source_prerequisite_context(root, family)) dina_cli_cat(line)
  invisible(dina_review_source_prerequisite_rows(root, family))
}

dina_review_source_prerequisite_context <- function(root, family) {
  rows <- dina_review_source_prerequisite_rows(root, family)
  if (!nrow(rows)) return(character())
  lines <- c(
    dina_cli_emphasis(paste("Before including", dina_review_families()[[family]])),
    dina_cli_dim("You can explore now. The required source must be included and current before this family can be included.")
  )
  for (i in seq_len(nrow(rows))) {
    lines <- c(lines, paste0("  ", dina_cli_emphasis(rows$required[[i]]), ": ", rows$status[[i]], " — ", dina_cli_dim(rows$next_step[[i]])))
  }
  lines
}

dina_review_family_menu_context <- function(root, family, state) {
  lines <- c(
    dina_review_state_label(state), dina_cli_dim(state$reason)
  )
  if (identical(family, "wid")) {
    snapshot <- dina_wid_local_snapshot(root)
    snapshot_lines <- if (isTRUE(snapshot$available)) {
      c(
        dina_cli_emphasis("Local WID snapshot"),
        paste0(dina_cli_dim("  Downloaded: "), snapshot$version),
        paste0(dina_cli_dim("  Data coverage: "), snapshot$coverage, dina_cli_dim(sprintf(" · %s artifact files", snapshot$files))),
        paste0(dina_cli_dim("  Compared with: "), snapshot$population_baseline$label),
        paste0(dina_cli_dim("  Refresh from WID API: "), dina_cli_command("dina sources refresh wid"))
      )
    } else {
      c(dina_cli_emphasis("Local WID snapshot"), dina_cli_dim("  No incoming WID snapshot is stored in input_data/_new/wid."))
    }
    lines <- c(lines, "", snapshot_lines)
  }
  prerequisites <- dina_review_source_prerequisite_context(root, family)
  if (length(prerequisites)) lines <- c(lines, "", prerequisites)
  lines
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

# WID's source review centers on population, a direct pipeline input. It is
# compared against the accepted WID input—not against a final DINA release.
dina_review_wid_pipeline_input_values <- function(root, engine, contract, staged_repo) {
  declared <- (contract$review_focus %||% list())$pipeline_inputs %||% list()
  if (!length(declared)) stop("WID contract declares no direct pipeline inputs for review.", call. = FALSE)
  notes <- character()
  rows <- lapply(names(declared), function(source_id) {
    spec <- declared[[source_id]] %||% list()
    artifact <- tryCatch(engine$wid_include_artifact(contract, source_id), error = function(e) NULL)
    if (is.null(artifact)) stop("WID review focus refers to an unknown artifact: ", source_id, call. = FALSE)
    candidate_paths <- engine$wid_include_artifact_paths(root, artifact, staged_repo = staged_repo)
    baseline <- engine$wid_include_comparison_baseline(root, artifact)
    current <- engine$wid_include_read_dta(baseline$path)
    candidate <- engine$wid_include_read_dta(candidate_paths$canonical)
    keys <- as.character(spec$keys %||% character())
    labels <- unlist(spec$values %||% list(), use.names = TRUE)
    measures <- names(labels)
    if (!isTRUE(candidate$ok)) {
      notes <<- c(notes, paste(source_id, "not checked: reviewed direct input is unavailable."))
      return(data.frame())
    }
    if (!all(c(keys, measures) %in% names(candidate$data))) {
      stop("WID review-focus columns are missing from candidate ", source_id, ".", call. = FALSE)
    }
    current_data <- if (isTRUE(current$ok)) current$data else candidate$data[FALSE, , drop = FALSE]
    if (!all(c(keys, measures) %in% names(current_data))) {
      notes <<- c(notes, paste(source_id, "has no compatible accepted input; incoming observations are listed as new."))
      current_data <- candidate$data[FALSE, c(keys, measures), drop = FALSE]
    }
    normalize_keys <- function(data) {
      for (key in keys) if (is.character(data[[key]])) data[[key]] <- trimws(data[[key]])
      data
    }
    current_data <- normalize_keys(current_data)
    candidate_data <- normalize_keys(candidate$data)
    compared <- dina_review_compare(
      current_data, candidate_data, keys, measures,
      function(old, new) 1e-8 * pmax(1, abs(old), abs(new))
    )
    if (!nrow(compared)) return(compared)
    compared$source_id <- source_id
    compared$input_title <- as.character(spec$title %||% source_id)
    compared$measure <- unname(labels[compared$measure])
    compared$baseline <- switch(baseline$kind,
      legacy_population = "legacy population input",
      accepted_wid = "accepted WID population input",
      "no accepted population input"
    )
    compared$review_country <- dina_review_country(compared, contract$area_metadata)
    if (identical(baseline$kind, "legacy_population")) {
      notes <<- c(notes, paste(source_id, "is compared with the legacy population input at", baseline$rel, "until this WID candidate is included."))
    }
    compared
  })
  list(rows = dina_review_bind(rows), notes = notes)
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
    if (identical(family, "sna")) c(file.path(root, "config", "pipeline.yml"), file.path(root, "code", "Stata", "05b-rescale-and-impute.do")),
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

# Admin candidate cleaners emit the harmonized bracket tables consumed by the
# interpolation tooling. Read them as a stable review interface: published
# threshold values identify bracket rows, while annual population and mean
# values occur once per country/year/component.
dina_review_admin_harmonized_output <- function(path, country, source_id) {
  if (!file.exists(path)) return(list(brackets = data.frame(), annual = data.frame(), note = paste(source_id, "has no harmonized output.")))
  if (!requireNamespace("readxl", quietly = TRUE)) return(list(brackets = data.frame(), annual = data.frame(), note = "readxl is required to compare harmonized PIT outputs."))
  component <- if (grepl("total-pre-", basename(path), fixed = TRUE)) "pretax" else if (grepl("total-pos-", basename(path), fixed = TRUE)) "posttax" else "unknown"
  normalize_names <- function(x) {
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- tolower(gsub("[^a-z0-9]+", "_", x))
    gsub("(^_+|_+$)", "", x)
  }
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  if (!length(sheets)) return(list(brackets = data.frame(), annual = data.frame(), note = paste(source_id, "output cannot be read.")))
  issues <- character()
  rows <- lapply(sheets, function(sheet) {
    raw <- tryCatch(readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"), error = function(e) e)
    if (inherits(raw, "error")) { issues <<- c(issues, paste(sheet, conditionMessage(raw))); return(NULL) }
    names(raw) <- normalize_names(names(raw))
    required <- c("p", "thr", "bracketavg")
    if (!all(required %in% names(raw))) { issues <<- c(issues, paste(sheet, "is missing", paste(setdiff(required, names(raw)), collapse = ", "))); return(NULL) }
    year <- suppressWarnings(as.integer(sheet))
    if (!is.finite(year) && "year" %in% names(raw)) year <- suppressWarnings(as.integer(raw$year[which(!is.na(raw$year))[1L]]))
    if (!is.finite(year)) { issues <<- c(issues, paste(sheet, "does not identify a year")); return(NULL) }
    data.frame(
      country = country, source_id = source_id, year = year, component = component,
      p = suppressWarnings(as.numeric(raw$p)), thr = suppressWarnings(as.numeric(raw$thr)),
      bracketavg = suppressWarnings(as.numeric(raw$bracketavg)),
      average = if ("average" %in% names(raw)) suppressWarnings(as.numeric(raw$average)) else NA_real_,
      popsize = if ("popsize" %in% names(raw)) suppressWarnings(as.numeric(raw$popsize)) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  rows <- dina_review_bind(rows)
  rows <- rows[is.finite(rows$p), , drop = FALSE]
  annual <- if (!nrow(rows)) data.frame() else dina_review_bind(lapply(split(rows, paste(rows$country, rows$source_id, rows$year, rows$component, sep = "\r")), function(part) {
    first_finite <- function(x) { x <- x[is.finite(x)]; if (length(x)) x[[1L]] else NA_real_ }
    data.frame(country = part$country[[1L]], source_id = part$source_id[[1L]], year = part$year[[1L]], component = part$component[[1L]],
      average = first_finite(part$average), popsize = first_finite(part$popsize), stringsAsFactors = FALSE)
  }))
  list(brackets = rows[c("country", "source_id", "year", "component", "p", "thr", "bracketavg")], annual = annual,
    note = paste(issues, collapse = "; "))
}

dina_review_admin_harmonized_values <- function(prepared) {
  focus <- (prepared$contract$cleaners %||% list())$review_harmonized_outputs %||% list()
  bracket_keys <- as.character(focus$bracket_keys %||% c("country", "source_id", "year", "component", "thr"))
  bracket_values <- as.character(focus$bracket_values %||% "bracketavg")
  annual_keys <- as.character(focus$annual_keys %||% c("country", "source_id", "year", "component"))
  annual_values <- as.character(focus$annual_values %||% c("average", "popsize"))
  candidate_outputs <- prepared$outputs$cleaner_outputs %||% data.frame()
  required <- c("source_id", "country", "rel", "staged_to", "exists")
  if (!all(required %in% names(candidate_outputs))) return(list(rows = data.frame(), note = "Harmonized PIT comparison was not produced."))
  candidate_outputs <- candidate_outputs[candidate_outputs$exists %in% TRUE, , drop = FALSE]
  notes <- character(); candidate_brackets <- list(); accepted_brackets <- list(); candidate_annual <- list(); accepted_annual <- list()
  for (i in seq_len(nrow(candidate_outputs))) {
    output <- candidate_outputs[i, , drop = FALSE]
    source_id <- output$source_id[[1L]]; country <- output$country[[1L]]
    candidate <- dina_review_admin_harmonized_output(output$staged_to[[1L]], country, source_id)
    baseline_path <- file.path(prepared$paths$baseline_repo, output$rel[[1L]])
    accepted <- dina_review_admin_harmonized_output(baseline_path, country, source_id)
    notes <- c(notes, candidate$note, accepted$note)
    candidate_brackets[[length(candidate_brackets) + 1L]] <- candidate$brackets
    accepted_brackets[[length(accepted_brackets) + 1L]] <- accepted$brackets
    candidate_annual[[length(candidate_annual) + 1L]] <- candidate$annual
    accepted_annual[[length(accepted_annual) + 1L]] <- accepted$annual
  }
  candidate_brackets <- dina_review_bind(candidate_brackets); accepted_brackets <- dina_review_bind(accepted_brackets)
  candidate_annual <- dina_review_bind(candidate_annual); accepted_annual <- dina_review_bind(accepted_annual)
  compare <- function(old, new, keys, measures, level) {
    if (!nrow(new)) return(data.frame())
    if (!nrow(old)) old <- new[FALSE, c(keys, measures), drop = FALSE]
    rows <- dina_review_compare(old, new, keys, measures, function(old, new) pmax(1e-8, abs(old) * 1e-10))
    rows$comparison_level <- level
    rows
  }
  bracket_rows <- compare(accepted_brackets, candidate_brackets, bracket_keys, bracket_values, "bracket")
  annual_rows <- compare(accepted_annual, candidate_annual, annual_keys, annual_values, "annual")
  rows <- dina_review_bind(list(bracket_rows, annual_rows))
  labels <- c(thr = "threshold", bracketavg = "bracket average", average = "annual average", popsize = "population")
  if (nrow(rows)) rows$measure <- unname(labels[rows$measure])
  notes <- unique(notes[nzchar(notes)])
  list(rows = rows, note = paste(c(
    "Harmonized PIT inputs are compared after the same cleaner is run on accepted and incoming sources. Published thresholds align equivalent brackets; annual values use country, year and component.",
    notes
  ), collapse = "\n"))
}

dina_review_values <- function(root, family, engine, explored, prepared) {
  if (identical(family, "sna")) {
    old <- engine$country_sna_include_extract_all(prepared$contract, root, prepared$years)
    current <- engine$country_sna_include_wide(old$values_long, prepared$contract, prepared$years)
    candidate <- prepared$outputs$values_wide
    # The wide extractor deliberately builds a complete grid.  Remove a row
    # only when the canonical extractor itself could not resolve a source for
    # that country-year.  Do not use explorer coverage for this: its file
    # discovery is descriptive and must never erase values that were already
    # accepted into the canonical source directory.
    source_matches <- old$source_matches %||% data.frame()
    unavailable <- source_matches[
      !is.na(source_matches$country) & !is.na(source_matches$year) &
        !is.na(source_matches$source_status) & source_matches$source_status != "matched",
      c("country", "year"), drop = FALSE
    ]
    if (nrow(unavailable)) {
      unavailable_key <- paste(unavailable$country, unavailable$year, sep = "\r")
      current_key <- paste(current$country, current$year, sep = "\r")
      current <- current[!current_key %in% unavailable_key, , drop = FALSE]
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
    pipeline <- dina_review_wid_pipeline_input_values(root, engine, prepared$contract, prepared$paths$staged_repo)
    scope <- dina_source_explore_scope(root, family = "WID")
    pipeline$rows <- dina_review_filter_display_scope(pipeline$rows, scope, prepared$contract$area_metadata)
    baseline_note <- if (any(pipeline$rows$baseline %in% "legacy population input", na.rm = TRUE)) {
      "Population changes compare the incoming WID input with the legacy population input; Include will establish the new WID canonical input for future refreshes."
    } else {
      "Population changes compare the incoming WID input with the accepted WID input; they are not revisions of the final DINA series."
    }
    note_parts <- c(
      baseline_note,
      "Final-series revisions are checked only after the pipeline runs, against export_validation.previous_update_file.",
      pipeline$notes
    )
    return(list(rows = pipeline$rows, note = paste(note_parts[nzchar(note_parts)], collapse = "\n")))
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
  if (identical(family, "admin")) return(dina_review_admin_harmonized_values(prepared))
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
  list(rows = rows, note = "Admin coverage is reviewed before acceptance. Direct auxiliary inputs are compared here; primary PIT files are validated by staging their cleaners. Individual PIT values are not compared row by row in this review.")
}

dina_review_sna_focus_status <- function(values, problems, fallback = "all_good") {
  if (identical(fallback, "blocked")) return("blocked")
  blockers <- nrow(problems) && any(problems$severity %in% "Blocker", na.rm = TRUE)
  unresolved <- nrow(values) && any(values$result %in% c("absence unexplained", "expected value missing", "nonfinite value"), na.rm = TRUE)
  if (blockers) return("blocked")
  if (unresolved) return("check_following")
  "all_good"
}

dina_review_manifest_set_value <- function(manifest, key, value) {
  hit <- which(manifest$key %in% key)
  if (length(hit)) {
    manifest$value[hit[[1L]]] <- as.character(value)
  } else {
    manifest <- rbind(manifest, data.frame(key = key, value = as.character(value), stringsAsFactors = FALSE))
  }
  manifest
}

dina_review_sna_sync_include_manifest <- function(prepared, review_status, full_contract_status) {
  manifest <- prepared$manifest %||% prepared$outputs$include_manifest
  if (is.null(manifest) || !nrow(manifest)) return(prepared)
  manifest <- dina_review_manifest_set_value(manifest, "full_contract_status", full_contract_status)
  manifest <- dina_review_manifest_set_value(manifest, "status", review_status)
  manifest <- dina_review_manifest_set_value(manifest, "review_scope", "direct_pipeline_inputs")
  prepared$manifest <- manifest
  prepared$outputs$include_manifest <- manifest
  for (path in c(
    file.path(prepared$paths$logs, "include_manifest.csv"),
    file.path(prepared$paths$tables, "include_manifest.csv")
  )) {
    utils::write.csv(manifest, path, row.names = FALSE, na = "")
  }
  prepared
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

# SNA source promotion is deliberately provisional during an update.  A new
# exploration starts from the pre-include canonical source state, so it can
# assess the current inbox against the same baseline the reviewer first saw.
# The confirm backup is the sole rollback authority; saved review pointers and
# explorer caches never determine this state.
dina_review_sna_confirmation_to_reset <- function(root) {
  base <- file.path(root, "output", "experiments", "country_sna_include", "confirms")
  manifests <- list.files(base, pattern = "^confirm_manifest\\.csv$", recursive = TRUE, full.names = TRUE)
  if (!length(manifests)) return(NULL)
  manifests <- manifests[order(file.info(manifests)$mtime, decreasing = TRUE)]
  for (manifest_path in manifests) {
    manifest <- dina_read_csv_table_or_empty(manifest_path)
    if (!identical(dina_manifest_value(manifest, "status"), "confirmed")) next
    confirm_root <- dirname(dirname(manifest_path))
    # A completed restore makes the confirmation's rollback idempotent and
    # prevents every later Explore from touching the same source files again.
    restored <- list.files(file.path(confirm_root, "logs"), pattern = "^restore_.*\\.csv$", full.names = TRUE)
    if (!length(restored)) return(confirm_root)
  }
  NULL
}

dina_review_sna_reset_before_explore <- function(root, engine, progress = function(...) invisible(NULL)) {
  confirm_root <- dina_review_sna_confirmation_to_reset(root)
  if (is.null(confirm_root)) return(invisible(NULL))
  progress("Resetting the prior SNA source inclusion to its confirmed backup.")
  restored <- engine$country_sna_include_restore_sources(root = root, confirm_run = confirm_root)
  report <- restored$outputs$restore_report %||% data.frame()
  if (nrow(report) && any(report$status %in% c("backup_missing_or_copy_failed", "remove_failed"), na.rm = TRUE)) {
    stop("Could not reset the prior SNA source inclusion completely. Inspect ", restored$paths$restore_report, call. = FALSE)
  }
  progress(sprintf("Prior SNA inclusion reset: %s source file%s returned to the pre-include state.",
    nrow(report), if (nrow(report) == 1L) "" else "s"))
  invisible(restored)
}

dina_review_explore <- function(root, family, flags = list(), input = "stdin", is_terminal = isatty(stdin())) {
  operation <- dina_cli_operation(paste(dina_review_families()[[family]], "source exploration"))
  completed <- FALSE
  on.exit(if (!completed) operation$finish("Failed"), add = TRUE)
  engine <- dina_review_engine(root, family)
  dina_cli_header(paste(dina_review_families()[[family]], "Explore"))
  dina_cli_alert("Checking the family's coverage, revisions and source structure.")
  if (identical(family, "wid")) {
    snapshot <- dina_print_wid_local_snapshot(root)
    dina_cli_cat(dina_cli_dim("Explore reviews this snapshot and prepares the standard WID inclusion decision. It never changes accepted WID files."))
    if (isTRUE(flags$fetch)) dina_cli_cat(dina_cli_dim("Refresh requested: the snapshot will be replaced from the WID API before review."))
    if (!isTRUE(snapshot$available) && !isTRUE(flags$fetch)) {
      dina_cli_warn("There is no local WID snapshot to review.")
      dina_cli_cat(dina_cli_dim("Next: "), dina_cli_command("dina sources refresh wid"), dina_cli_dim(" to download one; then run Explore."))
      completed <- TRUE
      operation$finish("Needs refresh", "No local WID snapshot is available.")
      return(invisible(NULL))
    }
  }
  operation$progress("Inspecting source inputs and saved review evidence.")
  if (identical(family, "sna")) dina_review_sna_reset_before_explore(root, engine, operation$progress)
  record <- list(family = family, status = "incomplete", reviewed_at = dina_now())
  dina_review_save(record, root)
  input_paths <- dina_review_inputs(root, family, engine)
  before <- dina_review_hashes(input_paths)
  output <- flags[["output-dir"]] %||% NULL
  review_scope <- if (family %in% c("sna", "surveys")) dina_source_explore_scope(root, family = dina_review_families()[[family]]) else NULL
  sna_scope <- if (identical(family, "sna")) review_scope else NULL
  if (!is.null(review_scope)) {
    operation$progress(sprintf("Using %s for configured countries and run years %s-%s.", review_scope$config_source, min(review_scope$years), max(review_scope$years)))
  }
  explored <- switch(family,
    sna = engine$run_country_sna_explorer(root = root, output_dir = output, years = sna_scope$years,
      countries = sna_scope$countries %||% NULL, scope = sna_scope, progress = operation$progress),
    admin = engine$run_admin_pit_explorer(root = root, output_dir = output),
    surveys = engine$run_survey_pop_explorer(root = root, output_dir = output,
      countries = review_scope$countries, years = review_scope$years, scope = review_scope),
    wid = engine$run_wid_explorer(root = root, output_dir = output, fetch = isTRUE(flags$fetch)))
  if (identical(family, "wid") && isTRUE(flags$fetch)) {
    incoming <- file.path(root, "input_data", "_new", family)
    before[incoming] <- dina_review_hashes(incoming)
  }
  if (!is.null(review_scope)) review_scope <- explored$scope
  if (identical(family, "sna")) sna_scope <- review_scope
  if (identical(family, "sna") && nrow(explored$outputs$cache_status %||% data.frame())) {
    cache <- explored$outputs$cache_status
    operation$progress(sprintf("Fresh SNA inspection complete: %s configured countries analyzed.",
      sum(cache$status == "computed")))
  }
  dina_cli_alert("Preparing the family review. Accepted source files remain unchanged.")
  operation$progress("Preparing validation tables and candidate comparisons.")
  if (identical(family, "surveys") && !nrow(explored$outputs$year_coverage)) {
    stop("No survey files are available to prepare. Follow dina sources list guide surveys, then explore again. Value comparisons are not checked.", call. = FALSE)
  }
  id <- paste0("review-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-", basename(tempfile()))
  exploration <- explored$paths$root
  prepared <- switch(family,
    sna = engine$run_country_sna_include(root = root, exploration_run = exploration, run_id = id, expected_scope = sna_scope),
    admin = engine$run_admin_pit_include(
      root = root, exploration_run = exploration, run_id = id,
      progress = operation$progress
    ),
    surveys = engine$run_survey_pop_include(root = root, exploration_run = exploration, run_id = id),
    wid = engine$run_wid_include(root = root, exploration_run = exploration, run_id = id))
  comparison_error <- ""
  if (identical(family, "admin")) operation$progress("Comparing harmonized PIT inputs ready for interpolation.")
  values <- tryCatch(dina_review_values(root, family, engine, explored, prepared), error = function(e) {
    comparison_error <<- conditionMessage(e)
    list(rows = data.frame(), note = comparison_error)
  })
  manifest <- prepared$manifest %||% prepared$outputs$include_manifest
  status <- dina_manifest_value(manifest, "status")
  full_contract_status <- status
  if (any(values$rows$result == "nonfinite value", na.rm = TRUE)) comparison_error <- "Value comparison found nonfinite values; inspect missing-values before exploring again."
  if (nzchar(comparison_error)) status <- "blocked"
  plan <- if (identical(family, "sna")) dina_review_freeze_sna(prepared, root) else prepared$outputs$promotion_plan
  if (!nrow(plan) && identical(status, "all_good")) status <- "nothing_to_include"
  coverage <- dina_review_coverage(explored, values$rows, family, prepared)
  tables <- list(review_coverage = coverage, review_values = values$rows,
    review_problems = dina_review_problems(prepared), review_files = plan)
  if (identical(family, "sna")) {
    focus <- dina_review_sna_review_focus(root)
    scoped_values <- dina_review_sna_filter_scope(values$rows, sna_scope)
    scoped_detail <- dina_review_sna_filter_scope(prepared$outputs$include_detail %||% data.frame(), sna_scope)
    scoped_problems <- dina_review_sna_filter_scope(tables$review_problems, sna_scope)
    direct_values <- dina_review_sna_focus_rows(scoped_values, focus)
    direct_detail <- dina_review_sna_focus_rows(scoped_detail, focus, field = "variable")
    direct_problems <- dina_review_sna_focus_problems(scoped_problems, focus)
    tables$review_coverage <- dina_review_sna_scope_coverage(dina_review_coverage(NULL, direct_values, "sna"), sna_scope)
    tables$review_problems <- scoped_problems
    tables$confirmed_missing_years <- dina_review_sna_filter_scope(prepared$outputs$confirmed_missing_years %||% data.frame(), sna_scope)
    tables$value_changes_audit <- dina_review_sna_audit_rows(values$rows, prepared$outputs$include_detail %||% data.frame())
    tables$direct_input_changes_audit <- dina_review_sna_audit_rows(direct_values, direct_detail)
    tables$direct_input_checks <- direct_detail
    tables$direct_input_problems <- direct_problems
    tables$unresolved_new_values <- dina_review_sna_unresolved_additions(
      direct_detail, dina_review_whole_year_additions(direct_values)
    )
    # The normal SNA workflow intentionally reviews the inputs consumed by the
    # active pipeline task.  Its inclusion gate uses that same declared scope;
    # unscoped source-level blockers are retained by the focus filter.
    status <- dina_review_sna_focus_status(direct_values, direct_problems, status)
    prepared <- dina_review_sna_sync_include_manifest(prepared, status, full_contract_status)
    manifest <- prepared$manifest
  }
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
    scope = if (!is.null(review_scope)) review_scope else "whole family", area_metadata = if (family == "wid") prepared$contract$area_metadata else NULL, inputs = after,
    candidates = dina_review_hashes(c(plan$from_rel, file.path(prepared$paths$root, "tables"), file.path(prepared$paths$root, "logs"))),
    baseline = dina_review_hashes(if (nrow(plan)) file.path(root, plan$to_rel) else character()))
  record$watch <- dina_review_watch(unique(c(input_paths, names(record$candidates), names(record$baseline))))
  dina_write_json(record, file.path(record$run, "review.json"))
  dina_review_save(record, root)
  dina_review_print(record, country = flags$country %||% NULL, root = root)
  completed <- TRUE
  operation$finish(if (status %in% c("all_good", "nothing_to_include")) "Completed" else "Needs attention", paste("Review:", record$run))
  invisible(record)
}

dina_review_wid_refresh <- function(root, flags = list()) {
  operation <- dina_cli_operation("WID source refresh")
  completed <- FALSE
  on.exit(if (!completed) operation$finish("Failed"), add = TRUE)
  engine <- dina_review_engine(root, "wid")
  dina_cli_header("Refresh WID data")
  dina_cli_cat(dina_cli_dim("Downloads a fresh WID API snapshot into input_data/_new/wid. It does not explore, include, or change accepted WID files."))
  dina_print_wid_local_snapshot(root)
  scope <- dina_source_explore_scope(root, family = "WID")
  dina_cli_cat(dina_cli_dim("Requesting the active update's configured range: "), sprintf("%s–%s", min(scope$years), max(scope$years)))
  operation$progress("Downloading and validating WID API artifacts.")
  output <- flags[["output-dir"]] %||% NULL
  result <- engine$run_wid_explorer(root = root, output_dir = output, fetch = TRUE)
  if (!identical(result$status, "fetched")) {
    validation <- result$outputs$validation_report %||% data.frame(stringsAsFactors = FALSE)
    missing <- validation[validation$status %in% "blocked_missing_required_years", , drop = FALSE]
    if (nrow(missing)) {
      dina_cli_warn("The WID API did not supply every year requested by the active update. The existing local snapshot was kept unchanged.")
      dina_print_data_frame_compact(missing[c("source_id", "detail")], limit = nrow(missing))
    }
    stop("WID refresh did not produce a usable local snapshot. Inspect the WID fetch report.", call. = FALSE)
  }
  dina_cli_ok("Fresh WID snapshot downloaded.")
  dina_print_wid_local_snapshot(root)
  dina_cli_cat(dina_cli_dim("Next: "), dina_cli_command("dina sources explore wid"), dina_cli_dim(" to review this snapshot."))
  completed <- TRUE
  operation$finish("Completed", "Local snapshot is ready for Explore.")
  invisible(result)
}

dina_review_table <- function(record, table) {
  aliases <- c(coverage = "review_coverage", revisions = "review_values", `added-values` = "review_values", `missing-values` = "review_values", problems = "review_problems", files = "review_files", `pipeline-input-audit` = "pipeline_input_audit", `pipeline-input-summary` = "pipeline_input_summary")
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

dina_review_assert_config_scope <- function(record, root) {
  if (!record$family %in% c("sna", "surveys")) return(invisible(TRUE))
  saved <- record$scope %||% list()
  if (!is.list(saved)) saved <- list()
  current <- dina_source_explore_scope(root, family = dina_review_families()[[record$family]])
  saved_years <- sort(unique(as.integer(saved$years %||% integer())))
  current_years <- sort(unique(as.integer(current$years %||% integer())))
  saved_countries <- sort(unique(toupper(as.character(saved$countries %||% character()))))
  current_countries <- sort(unique(toupper(as.character(current$countries %||% character()))))
  same_countries <- !length(saved_countries) || identical(saved_countries, current_countries)
  same_identity <- nzchar(saved$identity %||% "") && identical(saved$identity, current$identity %||% "")
  same_fields <- identical(saved$config_source %||% "", current$config_source %||% "") &&
    identical(saved_years, current_years) &&
    identical(saved$effective_config_hash %||% "", current$effective_config_hash %||% "") && same_countries
  if (!isTRUE(same_identity || same_fields)) {
    saved_range <- if (length(saved_years)) sprintf("%s–%s", min(saved_years), max(saved_years)) else "unknown"
    current_range <- if (length(current_years)) sprintf("%s–%s", min(current_years), max(current_years)) else "unknown"
    stop(sprintf("This %s review was saved for %s; the current effective configuration requires %s. Explore %s again before inclusion.",
      dina_review_families()[[record$family]], saved_range, current_range, record$family), call. = FALSE)
  }
  invisible(TRUE)
}

dina_review_assert_sna_scope <- function(record, root) {
  dina_review_assert_config_scope(record, root)
}

dina_review_include <- function(root, family, flags = list(), input = "stdin", is_terminal = isatty(stdin()), record = dina_review_read(root, family)) {
  if (is.null(record) || identical(record$status, "incomplete")) stop("Explore this family first: dina sources explore ", family, call. = FALSE)
  if (identical(record$status, "included")) {
    if (dina_review_included_watch_changed(record)) stop("Source data or configuration changed since inclusion. Explore this family again: dina sources explore ", family, call. = FALSE)
    dina_cli_ok("This family review has already been included.")
    return(invisible(record))
  }
  if (!identical(record$status, "all_good")) stop("The family review has unresolved checks. Inspect dina sources table ", family, " before exploring again.", call. = FALSE)
  dina_review_verify(record)
  dina_review_assert_config_scope(record, root)
  dina_cli_header(paste("Include", dina_review_families()[[family]]))
  dina_cli_cat(dina_cli_dim("Include accepts the complete saved review. It does not run the pipeline."))
  dina_cli_cat(sprintf("Accept the whole reviewed family: %s files, reviewed %s.", record$files, record$reviewed_at))
  values <- dina_review_table(record, "review_values")
  dina_cli_cat(sprintf("Evidence: %s revised values, %s new observations, %s lost values.",
    sum(values$result == "revised"), sum(values$result == "new observation"), sum(values$result %in% c("value missing", "removed observation"))))
  dina_cli_cat(record$comparison_note)
  coverage <- dina_review_table(record, "review_coverage")
  if (any(c("old_years", "new_years") %in% names(coverage))) {
    dina_review_show_rows(dina_review_compact_coverage(coverage), c("country", "coverage", "change"))
  } else {
    dina_review_show_rows(coverage, c("country", "source_id", "extension_years", "overlap_years", "missing_in_new_years"))
  }
  confirmed <- isTRUE(flags$confirm)
  if (!confirmed && isTRUE(is_terminal)) {
    dina_cli_section("Final confirmation")
    dina_cli_cat(dina_cli_dim("No files have changed. Choose Yes below to begin a recoverable inclusion; choosing No returns to the source menu."))
    confirmed <- dina_menu_confirm(
      "Include family", "Accept this reviewed family?",
      input = input, is_terminal = is_terminal, preserve_screen = TRUE
    )
  }
  if (!confirmed) {
    dina_cli_cat(sprintf("No files changed. Scripts can accept this review with: dina sources include %s --confirm", family))
    return(invisible(record))
  }
  operation <- dina_cli_operation(paste(dina_review_families()[[family]], "source inclusion"))
  completed <- FALSE
  on.exit(if (!completed) operation$finish("Failed"), add = TRUE)
  engine <- dina_review_engine(root, family)
  operation$progress("Rechecking reviewed evidence before accepting source files.")
  dina_review_verify(record)
  dina_review_assert_config_scope(record, root)
  record$status <- "including"
  record$backup_root <- file.path(dirname(dirname(record$run)), "confirms")
  dina_review_save(record, root)
  operation$progress(sprintf("Review accepted. Inclusion is starting for %s staged file%s; recoverable backups will be created first.",
    record$files, if (record$files == 1L) "" else "s"))
  result <- tryCatch(switch(family,
    sna = engine$country_sna_include_confirm_sources(root = root, include_run = record$run),
    admin = engine$admin_pit_include_confirm_sources(
      root = root, include_run = record$run, progress = operation$progress
    ),
    surveys = engine$survey_pop_confirm_sources(root = root, include_run = record$run, progress = operation$progress),
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
  # Once accepted, source/configuration changes matter; CLI implementation
  # changes do not.  This avoids falsely showing an accepted family as stale
  # after a presentation-only improvement.
  record$acceptance_watch <- dina_review_watch(dina_review_acceptance_watch_paths(names(record$inputs)))
  record$watch <- record$acceptance_watch
  dina_review_save(record, root)
  dina_cli_ok("Family included. The pipeline has not been run.")
  dina_cli_cat(sprintf("Next family: dina sources\nRestore backup: dina sources include %s --restore %s", family, result$paths$root))
  completed <- TRUE
  operation$finish("Completed", paste("Backup and report:", result$paths$root))
  invisible(result)
}

dina_review_choose_family <- function(input = "stdin", is_terminal = isatty(stdin()), root = dina_repo_root()) {
  families <- dina_review_families()
  if (!isTRUE(is_terminal)) stop("Choose a family: sna, admin, surveys, or wid. Example: dina sources explore surveys", call. = FALSE)
  dina_menu_select("Source families", lapply(dina_review_source_family_order(), function(family) {
    state <- dina_review_family_status(root, family)
    dina_menu_action(family, paste(families[[family]], "—", dina_review_state_label(state)), description = state$reason,
      command = paste("dina sources explore", family))
  }),
    default = "quit", input = input, is_terminal = is_terminal)
}

dina_review_details <- function(record, input = "stdin", is_terminal = isatty(stdin())) {
  choices <- c(coverage = "Coverage", revisions = "Numerical revisions", `added-values` = "Newly available values", `missing-values` = "Missing values", problems = "Extraction problems", files = "Files to include")
  details <- switch(record$family, surveys = c(pipeline_input_summary = "Direct pipeline-input summary", pipeline_input_audit = "Direct pipeline-input audit", survey_source_comparison = "Survey files and schema"),
    sna = c(direct_input_changes_audit = "Direct-input change audit", unresolved_new_values = "Unresolved new-year values",
      direct_input_checks = "Direct-input extraction checks", direct_input_problems = "Direct-input problems"), wid = c(include_detail = "Artifact checks"),
    admin = c(aux_validation_report = "Auxiliary series checks", static_dependency_report = "Dependencies", cleaner_summary = "Cleaner outputs"))
  choices <- c(choices, details[file.exists(file.path(record$run, "tables", paste0(names(details), ".csv")))])
  repeat {
    selected <- dina_menu_select("Review details", lapply(names(choices), function(table) dina_menu_action(table, choices[[table]],
      command = sprintf("dina sources table %s %s", record$family, table))), default = "quit", input = input, is_terminal = is_terminal)
    if (is.null(selected) || identical(selected, "quit")) return(invisible(NULL))
    dina_review_show_rows(dina_review_table(record, selected), dina_review_detail_columns(selected, dina_review_table(record, selected)), limit = 20L)
    dina_cli_hold_result("Press Enter to return to review details: ", input, is_terminal)
  }
}

dina_review_family_menu <- function(root, family, input = "stdin", is_terminal = isatty(stdin())) {
  repeat {
    state <- dina_review_family_status(root, family)
    actions <- if (identical(family, "wid")) {
      c(refresh = "Refresh/download WID data", explore = "Explore", table = "Review details", include = "Include", back = "Choose another family")
    } else {
      c(list = "Find sources", explore = "Explore", table = "Review details", include = "Include", back = "Choose another family")
    }
    selected <- dina_menu_select(dina_review_families()[[family]], lapply(names(actions), function(action) dina_menu_action(action, actions[[action]],
      command = if (action == "back") "dina sources" else if (action == "refresh") "dina sources refresh wid" else sprintf("dina sources %s %s", action, family))),
      default = "quit", input = input, is_terminal = is_terminal,
      context = dina_review_family_menu_context(root, family, state))
    if (is.null(selected) || selected %in% c("back", "quit")) return(invisible(selected))
    tryCatch(if (selected == "include") {
      dina_review_include(root, family, input = input, is_terminal = is_terminal)
      dina_cli_hold_result(sprintf("Press Enter to return to %s sources: ", dina_review_families()[[family]]), input, is_terminal)
    } else if (selected == "table") {
      record <- dina_review_read(root, family)
      if (is.null(record$run)) dina_cli_cat("Explore this family first.") else {
        dina_review_print(record, root = root)
        dina_review_details(record, input, is_terminal)
      }
    } else if (selected == "refresh") {
      dina_review_wid_refresh(root)
      dina_cli_hold_result("Press Enter to return to WID sources: ", input, is_terminal)
    } else if (selected == "list") {
      registry <- dina_print_source_list(root, list(family = family, `no-menu` = TRUE))
      if (length(registry)) dina_source_list_actions_menu(registry, root, list(family = family), input, is_terminal)
    } else {
      dina_review_explore(root, family, input = input, is_terminal = is_terminal)
      dina_cli_hold_result(sprintf("Press Enter to return to %s sources: ", dina_review_families()[[family]]), input, is_terminal)
    }, error = function(e) {
      dina_cli_warn(conditionMessage(e))
      dina_cli_hold_result(sprintf("Press Enter to return to %s sources: ", dina_review_families()[[family]]), input, is_terminal)
    })
  }
}

dina_cmd_sources <- function(root, args, input = "stdin", is_terminal = isatty(stdin())) {
  args <- dina_drop_leading_separator(args)
  if (!length(args)) {
    if (!is_terminal) {
      dina_cli_header("Source families")
      for (family in dina_review_source_family_order()) {
        state <- dina_review_family_status(root, family)
        dina_cli_cat(paste0(family, " — ", dina_review_families()[[family]], ": ", dina_review_state_label(state),
          "\n  ", state$reason, "\n  dina sources explore ", family))
      }
      dina_cli_cat("\nRefresh WID when needed; Explore, then Include one family at a time.\nAll registered inputs: dina sources list")
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
  if (identical(sub, "refresh")) {
    family <- dina_source_workflow_family(dina_arg(flags$positional, 1L, "wid"), command = sub)
    if (!identical(family, "wid")) stop("Refresh is currently available only for WID. Use `dina sources refresh wid`.", call. = FALSE)
    return(dina_review_wid_refresh(root, flags))
  }
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
    dina_review_print(record, flags$country %||% NULL, flags$limit %||% 20L, root = root)
    if (is_terminal) dina_review_details(record, input, is_terminal)
  } else {
    dina_cli_header(paste(dina_review_families()[[family]], "Table:", table))
    dina_review_show_rows(dina_review_table(record, table), dina_review_detail_columns(table, dina_review_table(record, table)), country = flags$country %||% NULL, limit = flags$limit %||% 20L)
  }
  invisible(record)
}
