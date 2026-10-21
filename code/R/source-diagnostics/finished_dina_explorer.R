# Review supplied, finished distributions without changing them or reading any
# calculated pipeline output. Identity checks use the conventional meanings of
# bracket averages, population widths and cumulative top shares; discrepancies
# request review rather than silently reinterpret or repair the supplied data.

finished_dina_export_grid <- function() {
  c(seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
    seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001))
}

finished_dina_validate <- function(data, specification = list()) {
  required <- c("percentile", "step", "unit", "country", "year", "gini",
                "average", "p", "thr", "avg", "bckt_sh", "topavg", "topsh")
  numeric_fields <- c("year", "gini", "average", "p", "thr", "avg", "bckt_sh", "topavg", "topsh")
  # Stata float storage can shift p by about 3e-8. Fraction tolerance also
  # accommodates single precision shares; monetary comparisons are scale-aware.
  tol <- list(percentile = 6e-8, fraction = 1e-6,
              money_relative = 1e-6, money_absolute = 1e-6)
  for (name in intersect(names(specification$tolerances), names(tol))) {
    value <- specification$tolerances[[name]]
    if (is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0) tol[[name]] <- value
  }
  empty_issues <- data.frame(
    check_id = character(), status = character(), country = character(),
    year = numeric(), step = character(), unit = character(), percentile = character(),
    row = integer(), field = character(), observed = character(), expected = character(),
    difference = numeric(), detail = character(), stringsAsFactors = FALSE
  )
  empty_coverage <- data.frame(
    country = character(), step = character(), unit = character(),
    first_year = numeric(), last_year = numeric(), n_years = integer(), n_groups = integer(),
    rows = integer(), min_brackets = integer(), max_brackets = integer(),
    publication = logical(), stringsAsFactors = FALSE
  )
  issues <- list()
  descriptions <- list()
  coverage <- empty_coverage
  d <- if (is.data.frame(data)) as.data.frame(data, stringsAsFactors = FALSE) else data.frame()
  describe <- function(check, detail) descriptions[[check]] <<- detail
  issue <- function(check, status, detail, rows = NA_integer_, field = "",
                    observed = "", expected = "", difference = NA_real_, keys = NULL) {
    n <- max(length(rows), nrow(keys), 1L)
    rows <- rep_len(rows, n)
    key <- function(name, default) {
      if (!is.null(keys) && name %in% names(keys)) return(rep_len(keys[[name]], n))
      out <- rep(default, n)
      valid <- !is.na(rows) & rows >= 1L & rows <= nrow(d)
      if (name %in% names(d)) out[valid] <- d[[name]][rows[valid]]
      out
    }
    issues[[length(issues) + 1L]] <<- data.frame(
      check_id = rep(check, n), status = rep(status, n),
      country = as.character(key("country", "")), year = suppressWarnings(as.numeric(key("year", NA_real_))),
      step = as.character(key("step", "")), unit = as.character(key("unit", "")),
      percentile = as.character(key("percentile", "")), row = rows,
      field = rep_len(field, n), observed = as.character(rep_len(observed, n)),
      expected = as.character(rep_len(expected, n)), difference = rep_len(difference, n),
      detail = rep_len(detail, n), stringsAsFactors = FALSE
    )
  }
  result <- function() {
    findings <- if (length(issues)) do.call(rbind, issues) else empty_issues
    ids <- unique(c(names(descriptions), findings$check_id))
    checks <- lapply(ids, function(id) {
      part <- findings[findings$check_id == id, , drop = FALSE]
      ranks <- c(pass = 0L, warning = 1L, review = 2L, fail = 3L)
      status <- if (nrow(part)) names(ranks)[max(ranks[part$status]) + 1L] else "pass"
      cases <- part[nzchar(part$country) & is.finite(part$year), c("country", "year", "step", "unit"), drop = FALSE]
      detail <- descriptions[[id]]
      if (is.null(detail)) detail <- if (nrow(part)) part$detail[[1L]] else ""
      data.frame(check_id = id, status = status,
                 affected_rows = length(unique(part$row[!is.na(part$row)])),
                 affected_cases = nrow(unique(cases)), detail = detail, stringsAsFactors = FALSE)
    })
    list(coverage = coverage, checks = do.call(rbind, checks), issues = findings)
  }
  describe("metadata", "Population age coverage, currency/price basis and bracket semantics must be confirmed before publication.")
  metadata <- specification$definitions
  if (!is.list(metadata)) metadata <- list()
  for (name in c("population_age", "currency_price_basis", "bracket_semantics")) {
    value <- metadata[[name]]
    if (is.null(value) || !length(value) || anyNA(value) ||
        any(!nzchar(trimws(as.character(value))) | grepl("\\b(unknown|unconfirmed)\\b", tolower(as.character(value))))) {
      issue("metadata", "warning", paste("Unconfirmed definition:", name), field = name, expected = "confirmed definition")
    }
  }
  describe("schema", "Required fields must exist and numerical fields must be numeric.")
  missing <- setdiff(required, names(d))
  if (length(missing)) {
    issue("schema", "fail", "Required field is absent.", field = missing, rows = rep(NA_integer_, length(missing)), expected = "present")
    return(result())
  }
  wrong_type <- numeric_fields[!vapply(d[numeric_fields], is.numeric, logical(1))]
  if (length(wrong_type)) {
    issue("schema", "fail", "Numerical field has a non-numeric storage type.", field = wrong_type,
          rows = rep(NA_integer_, length(wrong_type)), observed = vapply(d[wrong_type], function(x) class(x)[[1L]], character(1)), expected = "numeric")
    return(result())
  }
  if (!nrow(d)) {
    issue("schema", "fail", "Source contains no observations.", observed = "0 rows", expected = "nonempty distribution")
    return(result())
  }
  for (name in c("country", "step", "unit", "percentile")) d[[name]] <- as.character(d[[name]])
  describe("key_values", "Country, year, concept and unit keys must be present; years must be integers.")
  bad_key <- !is.finite(d$year) | (is.finite(d$year) & d$year != floor(d$year))
  for (name in c("country", "step", "unit")) {
    bad <- is.na(d[[name]]) | !nzchar(trimws(d[[name]]))
    bad_key <- bad_key | bad
    if (any(bad)) issue("key_values", "fail", "Missing case key.", which(bad), name, d[[name]][bad], "nonempty")
  }
  bad_country <- !is.na(d$country) & !grepl("^[A-Z]{3}$", d$country)
  if (any(bad_country)) issue("key_values", "fail", "Country must be an uppercase ISO3 code.", which(bad_country), "country", d$country[bad_country], "ISO3")
  bad_key <- bad_key | bad_country
  bad_year <- !is.finite(d$year) | (is.finite(d$year) & d$year != floor(d$year))
  if (any(bad_year)) issue("key_values", "fail", "Year must be a finite integer.", which(bad_year), "year", d$year[bad_year], "integer year")
  describe("finite_values", "All supplied numerical values must be finite and nonmissing.")
  for (name in numeric_fields) {
    bad <- !is.finite(d[[name]])
    if (any(bad)) issue("finite_values", "fail", "Nonfinite or missing numerical value.", which(bad), name, d[[name]][bad], "finite number")
  }
  # Bounds come from the actual labels, not the export grid: wider supplied
  # brackets can cover the population completely and are not missing cases.
  describe("percentile_intervals", "Percentile labels must define positive intervals within p0–p100.")
  labels_ok <- !is.na(d$percentile) & grepl("^p[0-9]+(?:\\.[0-9]+)?p[0-9]+(?:\\.[0-9]+)?$", d$percentile, perl = TRUE)
  lower <- upper <- rep(NA_real_, nrow(d))
  lower[labels_ok] <- as.numeric(sub("^p([^p]+)p.*$", "\\1", d$percentile[labels_ok])) / 100
  upper[labels_ok] <- as.numeric(sub("^p[^p]+p", "", d$percentile[labels_ok])) / 100
  intervals_ok <- labels_ok & is.finite(lower) & is.finite(upper) & lower >= 0 & upper <= 1 & upper > lower
  if (any(!intervals_ok)) issue("percentile_intervals", "fail", "Malformed or out-of-range percentile interval.", which(!intervals_ok), "percentile", d$percentile[!intervals_ok], "0 <= lower < upper <= 100")
  describe("percentile_alignment", sprintf("Stored p must match the label's lower bound within %.1g.", tol$percentile))
  bad <- intervals_ok & is.finite(d$p) & abs(d$p - lower) > tol$percentile
  if (any(bad)) issue("percentile_alignment", "fail", "Stored p does not match the interval label.", which(bad), "p", d$p[bad], lower[bad], d$p[bad] - lower[bad])

  keys <- c("country", "year", "step", "unit")
  valid_keys <- d[!bad_key, keys, drop = FALSE]
  group_key <- function(x) do.call(paste, c(x, sep = "\r"))
  describe("unique_keys", "Each country/year/concept/unit must have unique percentile lower bounds.")
  row_keys <- paste(group_key(d[keys]), ifelse(intervals_ok, sprintf("%.12f", lower), d$percentile), sep = "\r")
  duplicate <- duplicated(row_keys) | duplicated(row_keys, fromLast = TRUE)
  if (any(duplicate)) issue("unique_keys", "fail", "Duplicate case/percentile key.", which(duplicate), "percentile", d$percentile[duplicate], "one row per lower bound")
  describe("expected_coverage", "All declared country × year × concept × unit cases must be present.")
  years <- specification$years
  if (is.list(years)) {
    endpoints <- suppressWarnings(as.numeric(c(years$first, years$last)))
    years <- if (length(endpoints) == 2L && all(is.finite(endpoints)) && endpoints[[2L]] >= endpoints[[1L]] && diff(endpoints) < 10000) seq(endpoints[[1L]], endpoints[[2L]]) else numeric()
  }
  spec_values <- list(country = specification$countries, year = years, step = specification$steps, unit = specification$units)
  for (name in names(spec_values)) {
    allowed <- unlist(spec_values[[name]], use.names = FALSE)
    if (length(allowed)) {
      bad <- !bad_key & !(d[[name]] %in% allowed)
      if (any(bad)) issue("expected_coverage", "fail", "Case lies outside declared source coverage.", which(bad), name, d[[name]][bad], paste(allowed, collapse = ","))
    }
  }
  if (nrow(valid_keys)) {
    values <- lapply(names(spec_values), function(name) {
      supplied <- unlist(spec_values[[name]], use.names = FALSE)
      if (length(supplied)) supplied else if (name == "year") seq(min(valid_keys$year), max(valid_keys$year)) else unique(valid_keys[[name]])
    })
    names(values) <- names(spec_values)
    expected <- do.call(expand.grid, c(values, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
    absent <- expected[!(group_key(expected[keys]) %in% group_key(valid_keys)), , drop = FALSE]
    if (nrow(absent)) issue("expected_coverage", "fail", "Expected distribution is absent.", keys = absent, field = "case", observed = "absent", expected = "present")
  }
  groups <- split(which(!bad_key), group_key(d[!bad_key, keys, drop = FALSE]))
  money_diff <- function(a, b) abs(a - b) > tol$money_absolute + tol$money_relative * pmax(abs(a), abs(b))
  for (indices in groups) {
    i <- indices[order(lower[indices], na.last = TRUE)]
    x <- d[i, , drop = FALSE]
    if (!all(intervals_ok[i])) next
    lo <- lower[i]
    hi <- upper[i]
    width <- hi - lo
    describe("percentile_coverage", "Actual intervals must cover p0–p100 without gaps or overlaps.")
    previous <- c(0, head(hi, -1L))
    gap <- lo - previous
    bad <- abs(gap) > tol$percentile
    if (any(bad)) issue("percentile_coverage", "fail", ifelse(gap[bad] > 0, "Gap before this interval.", "Overlap before this interval."),
                       i[bad], "lower bound", lo[bad], previous[bad], gap[bad])
    if (abs(tail(hi, 1L) - 1) > tol$percentile) issue("percentile_coverage", "fail", "Final interval does not reach p100.", tail(i, 1L), "upper bound", tail(hi, 1L), 1, tail(hi, 1L) - 1)
    describe("export_grid", "Current exporter requires its 127-bracket grid; wider valid intervals need an explicit alignment decision.")
    grid <- finished_dina_export_grid()
    match_grid <- length(lo) == length(grid) && all(abs(lo - grid) <= tol$percentile) && all(abs(hi - c(tail(grid, -1L), 1)) <= tol$percentile)
    if (!match_grid) {
      missing_bounds <- grid[!vapply(grid, function(p) any(abs(lo - p) <= tol$percentile), logical(1))]
      issue("export_grid", "warning", paste0("Complete coverage and export compatibility are separate checks. Missing export lower bounds: ",
            paste(format(missing_bounds * 100, trim = TRUE, scientific = FALSE), collapse = ", "), "."),
            rows = NA_integer_, keys = x[1L, keys, drop = FALSE], field = "bracket count",
            observed = length(lo), expected = length(grid), difference = length(lo) - length(grid))
    }
    if (!all(vapply(x[numeric_fields], function(v) all(is.finite(v)), logical(1)))) next
    describe("group_constants", "Overall mean and Gini must be constant within each distribution.")
    for (field in c("average", "gini")) {
      bad <- if (field == "gini") abs(x[[field]] - x[[field]][[1L]]) > tol$fraction else money_diff(x[[field]], x[[field]][[1L]])
      if (any(bad)) issue("group_constants", "fail", "Group-level statistic varies across brackets.", i[bad], field, x[[field]][bad], x[[field]][[1L]], x[[field]][bad] - x[[field]][[1L]])
    }
    describe("gini_bounds", "Supplied Gini must be within [0,1]; grouped approximations are not treated as exact Gini values.")
    bad <- x$gini < -tol$fraction | x$gini > 1 + tol$fraction
    if (any(bad)) issue("gini_bounds", "review", "Gini is outside conventional [0,1] bounds.", i[bad], "gini", x$gini[bad], "[0,1]")
    describe("mean_domain", "Overall mean must be positive for normalized income/share identities.")
    if (any(x$average <= 0)) {
      issue("mean_domain", "review", "Nonpositive overall mean prevents standard normalization.", i[x$average <= 0], "average", x$average[x$average <= 0], ">0")
      next
    }
    for (field in c("thr", "avg")) {
      check <- if (field == "thr") "threshold_order" else "bracket_average_order"
      describe(check, paste(field, "must be nondecreasing across ordered population brackets."))
      prev <- c(x[[field]][[1L]], head(x[[field]], -1L))
      bad <- x[[field]] < prev & money_diff(x[[field]], prev)
      if (any(bad)) issue(check, "review", "Value decreases from the preceding bracket.", i[bad], field, x[[field]][bad], prev[bad], x[[field]][bad] - prev[bad])
    }
    describe("bracket_bounds", "A bracket mean should lie between its threshold and the next threshold under conventional bracket definitions.")
    bad <- x$avg < x$thr & money_diff(x$avg, x$thr)
    if (any(bad)) issue("bracket_bounds", "review", "Bracket average is below its lower threshold; confirm supplied definitions.", i[bad], "avg", x$avg[bad], x$thr[bad], x$avg[bad] - x$thr[bad])
    next_thr <- c(tail(x$thr, -1L), Inf)
    bad <- is.finite(next_thr) & x$avg > next_thr & money_diff(x$avg, next_thr)
    if (any(bad)) issue("bracket_bounds", "review", "Bracket average exceeds the next threshold; confirm supplied definitions.", i[bad], "avg", x$avg[bad], next_thr[bad], x$avg[bad] - next_thr[bad])
    describe("share_bounds", "For nonnegative distributions, bracket and top shares must be in [0,1].")
    if (all(x$avg >= 0)) for (field in c("bckt_sh", "topsh")) {
      bad <- x[[field]] < -tol$fraction | x[[field]] > 1 + tol$fraction
      if (any(bad)) issue("share_bounds", "review", "Share is outside [0,1] for a nonnegative distribution.", i[bad], field, x[[field]][bad], "[0,1]")
    }
    describe("share_total", "Bracket shares must sum to one.")
    if (abs(sum(x$bckt_sh) - 1) > tol$fraction) issue("share_total", "review", "Bracket shares do not sum to one.", keys = x[1L, keys, drop = FALSE], field = "sum(bckt_sh)", observed = sum(x$bckt_sh), expected = 1, difference = sum(x$bckt_sh) - 1)
    describe("share_identity", "Expected bracket share = bracket average × actual population width / overall mean.")
    share <- x$avg * width / x$average
    bad <- abs(x$bckt_sh - share) > tol$fraction
    if (any(bad)) issue("share_identity", "review", "Bracket share differs from the mean/width identity; confirm bracket semantics.", i[bad], "bckt_sh", x$bckt_sh[bad], share[bad], x$bckt_sh[bad] - share[bad])
    describe("overall_mean_identity", "Overall mean must equal the population-weighted sum of bracket means.")
    mean <- sum(x$avg * width)
    if (money_diff(x$average[[1L]], mean)) issue("overall_mean_identity", "review", "Overall mean differs from the bracket-weighted mean.", keys = x[1L, keys, drop = FALSE], field = "average", observed = x$average[[1L]], expected = mean, difference = x$average[[1L]] - mean)
    describe("top_share_identity", "Top share must equal the reverse cumulative sum of supplied bracket shares.")
    top_share <- rev(cumsum(rev(x$bckt_sh)))
    bad <- abs(x$topsh - top_share) > tol$fraction
    if (any(bad)) issue("top_share_identity", "review", "Top share differs from cumulative bracket shares.", i[bad], "topsh", x$topsh[bad], top_share[bad], x$topsh[bad] - top_share[bad])
    describe("top_average_identity", "Top average must equal cumulative bracket income divided by the remaining population.")
    top_mean <- rev(cumsum(rev(x$avg * width))) / (1 - lo)
    bad <- money_diff(x$topavg, top_mean)
    if (any(bad)) issue("top_average_identity", "review", "Top average differs from cumulative bracket income/width.", i[bad], "topavg", x$topavg[bad], top_mean[bad], x$topavg[bad] - top_mean[bad])
  }
  if (nrow(valid_keys)) {
    series <- split(which(!bad_key), group_key(d[!bad_key, c("country", "step", "unit"), drop = FALSE]))
    publication_step <- if (is.null(specification$publication$step)) "nat" else specification$publication$step
    publication_unit <- if (is.null(specification$publication$unit)) "esn" else specification$publication$unit
    coverage <- do.call(rbind, lapply(series, function(i) {
      x <- d[i, , drop = FALSE]
      counts <- table(x$year)
      data.frame(country = x$country[[1L]], step = x$step[[1L]], unit = x$unit[[1L]],
                 first_year = min(x$year), last_year = max(x$year), n_years = length(counts),
                 n_groups = length(counts), rows = nrow(x), min_brackets = min(counts),
                 max_brackets = max(counts), publication = x$step[[1L]] == publication_step && x$unit[[1L]] == publication_unit,
                 stringsAsFactors = FALSE)
    }))
    rownames(coverage) <- NULL
  }
  result()
}
