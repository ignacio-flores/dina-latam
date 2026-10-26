# Presentation-only triage for saved source-review findings. These display
# cutoffs do not replace validation tolerances, approve a source or repair data.
finished_dina_check_labels <- function() {
  c(
    metadata = "Definitions",
    schema = "Fields and types",
    key_values = "Case identifiers",
    finite_values = "Finite values",
    percentile_intervals = "Percentile intervals",
    percentile_alignment = "Percentile labels",
    unique_keys = "Duplicate rows",
    expected_coverage = "Expected cases",
    percentile_coverage = "Population coverage",
    export_grid = "Export bracket grid",
    group_constants = "Group constants",
    gini_bounds = "Gini bounds",
    mean_domain = "Positive overall mean",
    threshold_order = "Threshold ordering",
    bracket_average_order = "Bracket mean ordering",
    bracket_bounds = "Mean within thresholds",
    share_bounds = "Share bounds",
    share_total = "Shares sum to 100%",
    share_identity = "Bracket share identity",
    overall_mean_identity = "Overall mean identity",
    top_share_identity = "Cumulative top shares",
    top_average_identity = "Cumulative top means",
    source_file = "Registered source file",
    source_metadata = "Source registration",
    source_read = "Readable source",
    unregistered_file = "Unregistered file",
    source_changed = "Source changed",
    grouped_coverage = "Supplied coverage gaps", top_share_order = "Nested top shares",
    grouped_mean_identity = "Shares versus averages"
  )
}

finished_dina_issue_sizes <- function(issues) {
  stopifnot(is.data.frame(issues))
  n <- nrow(issues)
  text <- function(name) {
    if (name %in% names(issues)) as.character(issues[[name]]) else rep("", n)
  }
  number <- function(name) suppressWarnings(as.numeric(text(name)))
  observed <- number("observed")
  expected <- number("expected")
  difference <- number("difference")
  comparable <- is.finite(observed) & is.finite(expected)
  calculated <- observed - expected
  fallback <- comparable & !is.finite(difference)
  difference[fallback] <- calculated[fallback]
  comparable <- comparable & is.finite(difference)

  relative <- rep(NA_real_, n)
  nonzero <- comparable & expected != 0
  relative[nonzero] <- abs(difference[nonzero]) / abs(expected[nonzero]) * 100
  zero <- comparable & expected == 0
  relative[zero] <- ifelse(observed[zero] == 0 & difference[zero] == 0, 0, Inf)
  share <- text("check_id") %in% c(
    "share_identity", "top_share_identity", "share_total", "share_bounds", "gini_bounds", "top_share_order"
  )
  absolute_pp <- rep(NA_real_, n)
  absolute_pp[share & comparable] <- 100 * abs(difference[share & comparable])

  # Accommodate arithmetic noise at the DISPLAY boundary only. Non-scalar
  # references such as "[0,1]" are not coerced to an invented reference value.
  within <- function(value, cutoff) !is.na(value) & value <= cutoff + 1e-12
  small <- comparable & within(relative, 0.1) & (!share | within(absolute_pp, 0.01))
  category <- rep("decision", n)
  category[comparable] <- "larger"
  category[small] <- "small"
  category[text("check_id") %in% c("metadata", "source_metadata")] <- "definition"
  category[text("check_id") == "export_grid"] <- "format"
  category[text("status") == "fail"] <- "structural"
  issues$relative_percent <- relative
  issues$absolute_pp <- absolute_pp
  issues$category <- category
  issues
}

finished_dina_check_actions <- function() {
  c(
    metadata = "Confirm source definitions.",
    schema = "Supply the required fields/types.",
    key_values = "Correct missing or invalid keys.",
    finite_values = "Resolve missing/nonfinite values.",
    percentile_intervals = "Correct interval definitions.",
    percentile_alignment = "Reconcile labels and stored p.",
    unique_keys = "Resolve duplicate case rows.",
    expected_coverage = "Resolve absent/unexpected cases.",
    percentile_coverage = "Resolve population gaps/overlaps.",
    export_grid = "Agree how to align wider brackets.",
    group_constants = "Resolve conflicting group values.",
    gini_bounds = "Confirm Gini definition and values.",
    mean_domain = "Confirm mean and income concept.",
    threshold_order = "Confirm ordering; review adjustments.",
    bracket_average_order = "Confirm ordering; review adjustments.",
    bracket_bounds = "Confirm bracket definitions first.",
    share_bounds = "Confirm share definition and values.",
    share_total = "Review totals and normalization.",
    share_identity = "Confirm bracket definitions first.",
    overall_mean_identity = "Choose the authoritative mean.",
    top_share_identity = "Review cumulative-share derivation.",
    top_average_identity = "Review cumulative-mean derivation.",
    source_file = "Supply one registered source file.",
    source_metadata = "Complete the source registration.",
    source_read = "Resolve the file/read error.",
    unregistered_file = "Register the source and version.",
    source_changed = "Explore the changed source again.",
    grouped_coverage = "Retain gaps for later harmonization.", top_share_order = "Review nested shares before harmonization.",
    grouped_mean_identity = "Shares govern; monetary scale is deferred."
  )
}

finished_dina_check_summary <- function(checks, issues) {
  stopifnot(is.data.frame(checks), is.data.frame(issues))
  empty <- data.frame(
    check_id = character(), label = character(), status = character(),
    category = character(), small = integer(), larger = integer(),
    cases = integer(), rows = integer(), median_relative_percent = numeric(),
    max_relative_percent = numeric(), median_absolute_pp = numeric(), max_absolute_pp = numeric(),
    action = character(), stringsAsFactors = FALSE
  )
  ids <- unique(c(as.character(checks$check_id), as.character(issues$check_id)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return(empty)
  sizes <- finished_dina_issue_sizes(issues)
  labels <- finished_dina_check_labels()
  actions <- finished_dina_check_actions()
  text <- function(rows, name) {
    if (name %in% names(rows)) as.character(rows[[name]]) else rep("", nrow(rows))
  }
  statistic <- function(values, fun) {
    values <- values[!is.na(values)]
    if (length(values)) fun(values) else NA_real_
  }
  key_count <- function(rows, columns, valid) {
    if (!any(valid)) return(0L)
    keys <- as.data.frame(lapply(columns, function(name) text(rows, name)[valid]),
      stringsAsFactors = FALSE)
    nrow(unique(keys))
  }
  result <- lapply(ids, function(id) {
    part <- sizes[!is.na(sizes$check_id) & sizes$check_id == id, , drop = FALSE]
    check <- checks[!is.na(checks$check_id) & checks$check_id == id, , drop = FALSE]
    severity <- c(pass = 0L, warning = 1L, review = 2L, fail = 3L)
    statuses <- c(text(check, "status"), text(part, "status"))
    ranks <- severity[statuses]
    status <- if (any(!is.na(ranks))) names(severity)[max(ranks, na.rm = TRUE) + 1L] else "review"
    priority <- c("structural", "definition", "format", "decision", "larger", "small")
    categories <- priority[priority %in% part$category]
    category <- if (status == "fail") "structural" else if (length(categories)) categories[[1L]] else if (
      status == "pass") "pass" else if (id %in% c("metadata", "source_metadata")) "definition" else if (
      id == "export_grid") "format" else "decision"
    case_columns <- c("country", "year", "step", "unit")
    complete <- rep(TRUE, nrow(part))
    for (column in case_columns) {
      value <- text(part, column)
      complete <- complete & !is.na(value) & nzchar(trimws(value))
    }
    complete <- complete & is.finite(suppressWarnings(as.numeric(text(part, "year"))))
    row_values <- suppressWarnings(as.numeric(text(part, "row")))
    valid_rows <- is.finite(row_values) & row_values >= 1 & row_values == floor(row_values)
    # A check can emit more than one finding for one input row. small/larger
    # count findings; rows/cases below count unique affected source rows/cases.
    rows <- key_count(part, c("source_id", "file", "row"), valid_rows)
    cases <- key_count(part, c("source_id", "file", case_columns), complete)
    label <- if (id %in% names(labels)) unname(labels[[id]]) else gsub("_", " ", id)
    action <- if (status == "pass") "No action." else if (id %in% names(actions)) unname(actions[[id]]) else "Review the reported finding."
    data.frame(
      check_id = id, label = label, status = status, category = category,
      small = sum(part$category == "small"), larger = sum(part$category == "larger"),
      cases = cases, rows = rows,
      median_relative_percent = statistic(part$relative_percent, stats::median),
      max_relative_percent = statistic(part$relative_percent, max),
      median_absolute_pp = statistic(part$absolute_pp, stats::median),
      max_absolute_pp = statistic(part$absolute_pp, max),
      action = action, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  out
}
