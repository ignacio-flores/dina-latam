# Review supplied distributions without depending on, or changing, pipeline
# products. Continuation and publication deliberately belong to a later step.

dina_finished_specification <- function(source) {
  spec <- source$finished_series %||% list()
  list(
    countries = unlist(source$country, use.names = FALSE),
    years = list(first = spec$first_year, last = spec$last_year),
    steps = unlist(spec$steps, use.names = FALSE),
    units = unlist(spec$units, use.names = FALSE),
    publication = list(step = spec$publication_step, unit = spec$publication_unit),
    definitions = list(
      population_age = spec$population,
      currency_price_basis = paste(spec$currency %||% "unconfirmed", spec$price_basis %||% "unconfirmed", sep = "; "),
      bracket_semantics = spec$bracket_semantics %||% "unconfirmed"
    )
  )
}

dina_finished_output_root <- function(root, path = NULL) {
  path <- path %||% file.path(root, "output", "experiments", "finished_dina_explore")
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      ".." %in% strsplit(path, "/", fixed = TRUE)[[1L]]) {
    stop("--output-dir must name a directory inside output/experiments, without '..'.", call. = FALSE)
  }
  if (!grepl("^/", path)) path <- file.path(root, path)
  # Resolve the existing parent too: normalizePath alone leaves a nonexistent
  # leaf unresolved, including any symlink in its parents.
  canonical <- function(p) {
    suffix <- character()
    while (!file.exists(p)) {
      suffix <- c(basename(p), suffix)
      p <- dirname(p)
    }
    paste(c(normalizePath(p, mustWork = TRUE), suffix), collapse = "/")
  }
  allowed <- canonical(file.path(root, "output", "experiments"))
  path <- canonical(path)
  if (!startsWith(paste0(path, "/"), paste0(allowed, "/"))) stop("--output-dir must be inside output/experiments.", call. = FALSE)
  path
}

dina_finished_explore <- function(root, flags = list()) {
  unsupported <- intersect(names(flags), c("fetch", "dry-run", "exploration-run", "include-run", "restore"))
  if (length(unsupported)) stop("Finished DINA exploration supports review only; omit --", unsupported[[1]], ".", call. = FALSE)
  base <- dina_finished_output_root(root, flags[["output-dir"]])
  operation <- dina_cli_operation("Finished DINA series source exploration")
  completed <- FALSE
  on.exit(if (!completed) operation$finish("Failed"), add = TRUE)
  dina_cli_header("Finished DINA series Explore")
  operation$progress("Reading supplied series and checking coverage and consistency.")
  registry <- dina_source_registry(root, family = "dina")
  if (!length(registry)) stop("No finished DINA sources are registered in config/sources.yml.", call. = FALSE)
  engine <- dina_review_engine(root, "dina")
  inbox <- file.path(root, "input_data", "_new", "dina")
  inputs <- c(inbox, file.path(root, "config", "sources.yml"),
    file.path(root, "code", "R", "source-diagnostics", "finished_dina_explorer.R"),
    file.path(script_dir, c("finished_dina_review.R", "finished_dina_summary.R")))
  before <- dina_review_hashes(inputs)
  dina_review_save(list(family = "dina", status = "incomplete", reviewed_at = dina_now()), root)

  tables <- list(review_coverage = data.frame(), checks = data.frame(),
    review_problems = data.frame(), review_files = data.frame())
  add <- function(name, rows, id, file) {
    if (!nrow(rows)) return(invisible(NULL))
    rows$source_id <- id
    rows$file <- file
    tables[[name]] <<- dina_review_bind(list(tables[[name]], rows))
  }
  problem <- function(id, file, country, check, detail) {
    add("checks", data.frame(check_id = check, status = "fail", affected_rows = 0L,
      affected_cases = 0L, detail = detail), id, file)
    add("review_problems", data.frame(check_id = check, status = "fail",
      country = country, year = NA_integer_, step = "", unit = "", percentile = "",
      row = NA_integer_, field = "", observed = "", expected = "", difference = NA_real_,
      detail = detail), id, file)
  }
  matched <- character()
  for (source in registry) {
    paths <- dina_expand_paths(source$inbox, root)
    paths <- paths[file.exists(paths) & !dir.exists(paths)]
    spec <- source$finished_series %||% list()
    id <- source$id
    country <- paste(unlist(source$country), collapse = ",")
    file <- if (length(paths)) paste(vapply(paths, dina_relative, character(1), root = root), collapse = ", ") else paste(unlist(source$inbox), collapse = ", ")
    matched <- c(matched, paths)
    file_row <- data.frame(country = country, version = spec$version %||% "",
      publication = paste(spec$publication_step %||% "", spec$publication_unit %||% "", sep = "/"),
      citation = spec$citation %||% "", doi = spec$doi %||% "",
      currency = spec$currency %||% "unconfirmed", price_basis = spec$price_basis %||% "unconfirmed",
      population = spec$population %||% "unconfirmed", status = "reviewed",
      stringsAsFactors = FALSE)
    if (length(paths) != 1L) {
      file_row$status <- "fail"
      problem(id, file, country, "source_file", if (!length(paths))
        "Registered source file is missing; place the supplied file at the registered inbox path."
        else "Multiple files match this source; register the intended version separately before review.")
    } else {
      metadata <- c("version", "first_year", "last_year", "steps", "units", "publication_step", "publication_unit", "citation", "doi")
      missing <- metadata[vapply(metadata, function(name) {
        value <- unlist(spec[[name]], use.names = FALSE)
        !length(value) || anyNA(value) || any(!nzchar(trimws(as.character(value))))
      }, logical(1))]
      if (length(missing)) {
        problem(id, file, country, "source_metadata", paste("Complete the source registry fields:", paste(missing, collapse = ", ")))
        file_row$status <- "fail"
      }
      year_value <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x)
      if (!year_value(spec$first_year) || !year_value(spec$last_year) ||
          spec$first_year > spec$last_year || spec$last_year - spec$first_year >= 10000) {
        problem(id, file, country, "source_metadata", "first_year and last_year must be finite integer years in ascending order.")
        file_row$status <- "fail"
      }
      if (length(spec$publication_step) != 1L || length(spec$publication_unit) != 1L ||
          !(spec$publication_step %in% unlist(spec$steps)) || !(spec$publication_unit %in% unlist(spec$units))) {
        problem(id, file, country, "source_metadata", "publication_step and publication_unit must identify a declared concept/unit combination.")
        file_row$status <- "fail"
      }
      if (!length(source$country) || anyNA(source$country) || any(!grepl("^[A-Z]{3}$", unlist(source$country)))) {
        problem(id, file, country, "source_metadata", "Declare the supplied country coverage using uppercase ISO3 codes.")
        file_row$status <- "fail"
      }
      result <- tryCatch({
        dina_need("haven")
        data <- as.data.frame(haven::read_dta(paths[[1]]))
        engine$finished_dina_validate(data, dina_finished_specification(source))
      }, error = function(e) e)
      if (inherits(result, "error")) {
        file_row$status <- "fail"
        problem(id, file, country, "source_read", paste("Could not validate the supplied file:", conditionMessage(result)))
      } else {
        add("review_coverage", result$coverage, id, file)
        add("checks", result$checks, id, file)
        add("review_problems", result$issues, id, file)
        if (any(result$checks$status == "fail")) file_row$status <- "fail"
        else if (file_row$status != "fail" && any(result$checks$status %in% c("review", "warning"))) file_row$status <- "needs_review"
      }
    }
    add("review_files", file_row, id, file)
  }
  supplied <- list.files(inbox, recursive = TRUE, full.names = TRUE)
  supplied <- supplied[!dir.exists(supplied)]
  unregistered <- setdiff(supplied, matched)
  for (path in unregistered) problem("", dina_relative(path, root), "", "unregistered_file",
    "This incoming file has no registered source/version. Register its coverage and attribution before review.")

  status <- if (any(tables$checks$status == "fail")) "blocked" else if (any(tables$checks$status %in% c("review", "warning"))) "needs_review" else "reviewed"
  after <- dina_review_hashes(inputs)
  if (!identical(before, after)) {
    problem("", "", "", "source_changed", "Source files or review code changed during exploration. Explore again.")
    status <- "blocked"
  }
  # Each review gets its own directory; no source or publication can be used as
  # an output destination. Only small review tables are written.
  run <- file.path(base, "runs", paste0("review-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-", basename(tempfile())))
  dir.create(file.path(run, "tables"), recursive = TRUE, showWarnings = FALSE)
  for (name in names(tables)) utils::write.csv(tables[[name]], file.path(run, "tables", paste0(name, ".csv")), row.names = FALSE, na = "")
  record <- list(family = "dina", status = status, run = run, reviewed_at = dina_now(),
    scope = "all supplied concepts, units and years", review_only = TRUE,
    continuation = "Deferred until internal estimates are ready, after 07c and before publication through 07d.",
    inputs = after, watch = dina_review_watch(c(inputs, file.path(run, "tables"))))
  dina_write_json(record, file.path(run, "review.json"))
  dina_review_save(record, root)
  dina_finished_print(record, country = flags$country %||% NULL, root = root, filters = flags)
  completed <- TRUE
  operation$finish(if (status == "reviewed") "Completed" else "Needs attention", paste("Review:", run))
  invisible(record)
}

dina_finished_filter <- function(rows, filters = list(), country = NULL) {
  filters$country <- country %||% filters$country
  for (field in c("country", "year", "step", "unit")) {
    value <- filters[[field]]
    if (is.null(value)) next
    if (isTRUE(value) || length(value) != 1L) stop("Supply a value for --", field, ".", call. = FALSE)
    if (field == "country") value <- toupper(value)
    if (field %in% names(rows)) {
      # Source-wide definition notes apply to every case, including filtered views.
      keep <- is.na(rows[[field]]) | as.character(rows[[field]]) == "" | as.character(rows[[field]]) == as.character(value)
      rows <- rows[keep, , drop = FALSE]
    }
  }
  if (!is.null(filters$check)) {
    if (!filters$check %in% names(finished_dina_check_labels())) stop("Unknown --check. Use a check ID shown in dina sources table dina checks.", call. = FALSE)
    if ("check_id" %in% names(rows)) rows <- rows[rows$check_id == filters$check, , drop = FALSE]
  }
  if (!is.null(filters$size)) {
    if (!filters$size %in% c("small", "larger")) stop("--size must be small or larger.", call. = FALSE)
    if ("category" %in% names(rows)) rows <- rows[rows$category == filters$size, , drop = FALSE]
  }
  rows
}

dina_finished_number <- function(value, suffix = "") {
  vapply(value, function(x) {
    if (is.na(x)) return("-")
    if (is.infinite(x)) return(paste0("undefined", suffix))
    paste0(trimws(formatC(x, format = "g", digits = 5L)), suffix)
  }, character(1))
}

dina_finished_note <- function(...) dina_cli_cat(dina_cli_dim(paste0(...)))

dina_finished_table <- function(rows, limit = nrow(rows)) {
  dina_print_data_frame_compact(rows, limit = limit, dim_headers = FALSE,
    dim_columns = c("Action", "detail", "citation", "doi"))
}

dina_finished_checks_table <- function(checks, issues, show_ids = FALSE) {
  summary <- finished_dina_check_summary(checks, issues)
  numeric <- summary[summary$small + summary$larger > 0L, , drop = FALSE]
  if (nrow(numeric)) {
    share <- !is.na(numeric$max_absolute_pp)
    typical <- dina_finished_number(numeric$median_relative_percent, "%")
    largest <- dina_finished_number(numeric$max_relative_percent, "%")
    typical[share] <- dina_finished_number(numeric$median_absolute_pp[share], " pp")
    largest[share] <- dina_finished_number(numeric$max_absolute_pp[share], " pp")
    view <- data.frame(Check = numeric$label, Cases = numeric$cases,
      Small = numeric$small, Larger = numeric$larger, Typical = typical, Largest = largest)
    for (field in c("Cases", "Small", "Larger")) view[[field]] <- format(view[[field]], big.mark = ",", trim = TRUE)
    dina_finished_table(view)
  } else dina_cli_cat("No numerical discrepancies in this view.")
  other <- summary[!summary$category %in% c("small", "larger", "pass") &
    summary$status != "pass" & summary$small + summary$larger == 0L, , drop = FALSE]
  # A filtered view may contain no findings for a globally failing check.
  other <- other[other$check_id %in% issues$check_id, , drop = FALSE]
  if (nrow(other)) {
    dina_cli_section("Definitions, format and structural checks")
    category_labels <- c(definition = "Definition", format = "Format", structural = "Blocker", decision = "Decision")
    view <- data.frame(Check = other$label, Type = unname(category_labels[other$category]),
      Cases = ifelse(other$cases == 0L, "source", as.character(other$cases)), Action = other$action)
    dina_finished_table(view)
  }
  dina_finished_note("\nSmall: at most 0.1% of the reference value; shares also at most 0.01 pp.")
  dina_finished_note("Size guides review; it does not approve a correction. pp = percentage points.")
  dina_finished_note("Cases = country/year/concept/unit; counts are findings. Typical = median of flagged findings.")
  passed <- checks$check_id[checks$status == "pass"]
  if (length(passed)) dina_cli_cat(sprintf("%s source checks passed.", length(unique(passed))),
    dina_cli_dim(" Full-file validation is unchanged by display filters."))
  if (isTRUE(show_ids)) {
    dina_cli_section("Check IDs for filtering")
    view <- data.frame(Check = summary$label, ID = summary$check_id)
    dina_finished_table(view)
  }
  invisible(summary)
}

dina_finished_show_table <- function(record, table, country = NULL, limit = 12L, filters = list()) {
  if (table %in% c("changes", "impact", "preparation", "prepared-checks", "prepared-problems", "prepared-coverage")) {
    filters$country <- country %||% filters$country
    return(dina_finished_include_show_table(record, table, limit = limit, filters = filters))
  }
  if (table %in% c("checks", "summary")) {
    checks <- dina_review_table(record, "checks")
    issues <- dina_finished_filter(finished_dina_issue_sizes(dina_review_table(record, "problems")), filters, country)
    checks <- dina_finished_filter(checks, filters)
    return(dina_finished_checks_table(checks, issues, show_ids = TRUE))
  }
  problem_view <- table %in% c("problems", "review_problems", "small", "larger", "decisions")
  rows <- dina_review_table(record, if (problem_view) "problems" else table)
  if (problem_view) {
    rows <- finished_dina_issue_sizes(rows)
    if (table %in% c("small", "larger")) filters$size <- table
    if (table == "decisions") rows <- rows[!rows$category %in% c("small", "larger"), , drop = FALSE]
  }
  rows <- dina_finished_filter(rows, filters, country)
  if (problem_view && nrow(rows)) {
    rank <- match(rows$category, c("structural", "larger", "decision", "format", "definition", "small"))
    gap <- ifelse(!is.na(rows$absolute_pp), rows$absolute_pp, rows$relative_percent)
    column <- function(name) if (name %in% names(rows)) rows[[name]] else rep(NA, nrow(rows))
    rows <- rows[order(rank, column("check_id"), -gap, column("country"), column("year"), na.last = TRUE), , drop = FALSE]
  }
  total <- nrow(rows)
  limit <- suppressWarnings(as.integer(limit))
  if (length(limit) != 1L || is.na(limit) || limit < 1L) stop("--limit must be a positive integer.", call. = FALSE)
  # Detailed discrepancies can have many rows. Read once and format only the
  # requested preview; the complete precision-preserving CSV remains available.
  rows <- head(rows, limit)
  if (problem_view && !isTRUE(filters$exact)) {
    labels <- finished_dina_check_labels()
    groups <- split(seq_len(nrow(rows)), factor(paste(rows$country, rows$check_id, rows$source_id, sep = "|"),
      levels = unique(paste(rows$country, rows$check_id, rows$source_id, sep = "|"))))
    for (indices in groups) {
      part <- rows[indices, , drop = FALSE]
      id <- part$check_id[[1L]]
      label <- if (id %in% names(labels)) labels[[id]] else id
      dina_cli_section(paste(part$country[[1L]], label, paste0("(", id, ")")))
      number <- suppressWarnings(as.numeric(part$observed))
      expected <- suppressWarnings(as.numeric(part$expected))
      if (all(part$category %in% c("small", "larger")) && all(is.finite(number)) && all(is.finite(expected))) {
        gap <- dina_finished_number(part$relative_percent, "%")
        shares <- !is.na(part$absolute_pp)
        gap[shares] <- dina_finished_number(part$absolute_pp[shares], " pp")
        dina_finished_table(data.frame(
          Year = part$year, Series = paste(part$step, part$unit, sep = "/"),
          Fractile = ifelse(is.na(part$percentile) | part$percentile == "", "whole", part$percentile),
          Supplied = dina_finished_number(number), Reference = dina_finished_number(expected),
          Gap = gap, Size = part$category
        ), limit = nrow(part))
      } else {
        dina_finished_table(data.frame(Year = part$year,
          Series = ifelse(part$step == "" | is.na(part$step), "source", paste(part$step, part$unit, sep = "/")),
          Field = part$field, Type = part$category), limit = nrow(part))
        for (detail in unique(part$detail)) dina_finished_note("  ", detail)
      }
    }
    if (!nrow(rows)) dina_cli_cat("No findings match this view.")
    dina_finished_note(sprintf("\nShowing %s of %s findings (largest gaps first within each check).", nrow(rows), total))
    dina_finished_note("Narrow with --check ID, --year YYYY, --step nat, --unit esn, or --size small|larger.")
    dina_finished_note("Use --exact for full precision and the check explanation.")
    return(invisible(rows))
  }
  for (field in intersect(c("observed", "expected"), names(rows))) {
    if (is.numeric(rows[[field]])) rows[[field]] <- vapply(rows[[field]], function(x) {
      if (is.na(x)) return(NA_character_)
      format(x, digits = 12L, trim = TRUE)
    }, character(1))
  }
  if ("difference" %in% names(rows)) {
    rows[["difference (observed - expected)"]] <- vapply(rows$difference, function(x) {
      if (is.na(x)) return(NA_character_)
      format(x, digits = 9L, trim = TRUE)
    }, character(1))
    rows$difference <- NULL
  }
  columns <- if (table %in% c("problems", "review_problems")) {
    c("country", "year", "step", "unit", "percentile", "check_id", "status", "field", "observed", "expected", "difference (observed - expected)", "detail")
  } else names(rows)
  dina_review_show_rows(rows, columns = columns, limit = limit, dim_headers = FALSE,
    dim_columns = c("detail", "citation", "doi"))
  if (total > limit) dina_finished_note(sprintf("Showing %s of %s. Use --limit N for more.", limit, total))
  invisible(rows)
}

dina_finished_print <- function(record, country = NULL, limit = 12L, root = dina_repo_root(), filters = list()) {
  state <- dina_review_family_status(root, "dina", record)
  if (!is.null(record$include_candidate)) {
    filters$country <- country %||% filters$country
    if (state$code %in% c("stale", "included_stale")) dina_cli_cat(state$label, ": ", state$reason)
    return(dina_finished_include_print(record, dina_finished_candidate_read(record), root = root, filters = filters))
  }
  dina_cli_cat("Status: ", dina_cli_emphasis(state$label), dina_cli_dim(" — supplied-source review"))
  if (state$code == "stale") dina_cli_cat("Evidence changed. Run dina sources explore dina again.")
  files <- dina_review_table(record, "files")
  coverage <- dina_review_table(record, "coverage")
  checks <- dina_review_table(record, "checks")
  issues <- finished_dina_issue_sizes(dina_review_table(record, "problems"))
  select <- function(rows) dina_finished_filter(rows, filters, country)
  files <- select(files)
  coverage <- select(coverage)
  issues <- select(issues)
  if (!is.null(country)) checks <- checks[checks$source_id %in% unique(c(files$source_id, issues$source_id)), , drop = FALSE]
  for (i in seq_len(nrow(files))) {
    dina_cli_cat(files$file[[i]], " (", files$version[[i]], "; publication ", files$publication[[i]], ")")
    dina_finished_note("  Source: ", files$citation[[i]])
    dina_finished_note("  DOI: ", files$doi[[i]])
  }
  dina_cli_section("Coverage")
  if (nrow(coverage)) {
    view <- data.frame(country = coverage$country, series = paste(coverage$step, coverage$unit, sep = "/"),
      period = paste(coverage$first_year, coverage$last_year, sep = "–"),
      groups = coverage$n_groups, rows = coverage$rows,
      brackets = ifelse(coverage$min_brackets == coverage$max_brackets, coverage$min_brackets,
        paste(coverage$min_brackets, coverage$max_brackets, sep = "–")))
    dina_review_show_rows(view, limit = limit, dim_headers = FALSE)
  } else dina_cli_cat("No readable series.")
  dina_cli_section("Consistency review")
  dina_finished_checks_table(checks, issues)
  dina_finished_note("\nSmall findings:  dina sources table dina small")
  dina_finished_note("Larger findings: dina sources table dina larger")
  dina_finished_note("Other decisions: dina sources table dina decisions")
  dina_finished_note("Publication view: dina sources table dina --step nat --unit esn")
  dina_finished_note("Details: dina sources table dina problems --check bracket_bounds --year 1992")
  dina_finished_note("Continuation: deferred until internal estimates are ready (after 07c, before 07d publication).")
  dina_finished_note("No continuation preview, replacement activation or published-result changes.")
  dina_finished_note("Prepare corrections: dina sources include dina --dry-run")
  invisible(record)
}
