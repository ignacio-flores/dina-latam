# Explore preserves grouped WID observations. Include selects complete observed
# years and fits the same standard grid as other finished DINA sources.
finished_dina_is_grouped <- function(source) {
  identical(source$finished_series$format, "wid_grouped_xlsx")
}

finished_dina_grouped_read <- function(path, source) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Grouped DINA input requires readxl.")
  spec <- source$finished_series
  sheets <- readxl::excel_sheets(path)
  declared <- vapply(spec$series, function(s) s$sheet, character(1))
  if (!setequal(sheets, declared) || anyDuplicated(declared)) stop("Workbook sheets do not match the registered series.")
  percentiles <- unlist(spec$percentiles, use.names = FALSE)
  if (!length(percentiles) || anyDuplicated(percentiles) ||
      any(!grepl("^p[0-9.]+p[0-9.]+$", percentiles))) stop("Register distinct percentile columns.")
  rows <- lapply(spec$series, function(series) {
    x <- as.data.frame(readxl::read_excel(path, sheet = series$sheet, col_types = "text", .name_repair = "minimal"))
    columns <- c("WID code", "Unit", "Year", percentiles, "Data Quality",
      if (series$measure == "average") "Deflator (2016 =100)")
    if (anyDuplicated(names(x)) || !setequal(names(x), columns) || !nrow(x)) {
      stop("Unexpected or missing columns in ", series$sheet, ".")
    }
    if (anyNA(x[["WID code"]]) || any(x[["WID code"]] != series$code) ||
        anyNA(x$Unit) || any(x$Unit != series$unit)) stop("WID code or unit differs from registration in ", series$sheet, ".")
    number <- function(name) {
      raw <- x[[name]]
      value <- suppressWarnings(as.numeric(raw))
      if (any(!is.na(raw) & (!is.finite(value) | !nzchar(trimws(raw))))) {
        stop("Invalid numerical value in ", series$sheet, " column ", name, ".")
      }
      value
    }
    year <- number("Year")
    quality <- number("Data Quality")
    deflator <- if (series$measure == "average") number("Deflator (2016 =100)") else rep(NA_real_, nrow(x))
    do.call(rbind, lapply(percentiles, function(percentile) data.frame(
      row_type = "supplied", country = source$country, year = year,
      step = "ptinc", unit = series$unit, wid_code = series$code, measure = series$measure,
      percentile = percentile,
      p_lower = as.numeric(sub("^p([^p]+)p.*$", "\\1", percentile)) / 100,
      p_upper = as.numeric(sub("^p[^p]+p", "", percentile)) / 100,
      value = number(percentile), data_quality = quality, deflator = deflator,
      source_sheet = series$sheet, source_row = seq_len(nrow(x)) + 1L,
      source_column = percentile, requires_harmonization = 1L, publication_ready = 0L,
      stringsAsFactors = FALSE)))
  })
  do.call(rbind, rows)
}

finished_dina_grouped_brackets <- function(data) {
  shares <- data[data$row_type == "supplied" & data$measure == "share" & !is.na(data$value), , drop = FALSE]
  out <- list()
  label <- function(p) sub("\\.?0+$", "", sprintf("%.6f", 100 * p))
  label <- local({format_p <- label; function(p) ifelse(p == 0, "0", format_p(p))})
  for (year in unique(shares$year)) {
    x <- shares[shares$year == year, , drop = FALSE]
    x <- x[order(x$p_lower), , drop = FALSE]
    for (i in seq_len(nrow(x))) {
      row <- x[i, , drop = FALSE]
      nested <- which(x$p_lower > row$p_lower & x$p_upper == row$p_upper)
      j <- if (length(nested)) nested[which.min(x$p_lower[nested])] else NA_integer_
      row$row_type <- "bracket"
      row$basis_percentile <- row$percentile
      row$basis_share <- row$value
      row$subtract_percentile <- if (is.na(j)) "" else x$percentile[j]
      row$subtract_share <- if (is.na(j)) 0 else x$value[j]
      if (!is.na(j)) row$p_upper <- x$p_lower[j]
      row$percentile <- paste0("p", label(row$p_lower), "p", label(row$p_upper))
      row$bracket_share <- row$basis_share - row$subtract_share
      row$avg_norm <- row$bracket_share / (row$p_upper - row$p_lower)
      row$value <- NA_real_
      row$unit <- "mean_one"
      row$measure <- "derived_bracket"
      out[[length(out) + 1L]] <- row
    }
  }
  if (length(out)) do.call(rbind, out) else data.frame()
}

finished_dina_grouped_years <- function(data, source, brackets = finished_dina_grouped_brackets(data)) {
  policy <- source$finished_series$harmonization
  first_full <- if (is.null(policy$first_full_year)) source$finished_series$first_year else policy$first_full_year
  if ((!is.null(policy$require_full_population) && !isTRUE(policy$require_full_population)) ||
      isTRUE(policy$complete_partial_population) || isTRUE(policy$interpolate_missing_years) || isTRUE(policy$extrapolate_years)) {
    stop("Grouped harmonization supports observed full-population years only; population completion, missing-year interpolation and temporal extrapolation are not implemented.")
  }
  do.call(rbind, lapply(seq(source$finished_series$first_year, source$finished_series$last_year), function(year) {
    b <- brackets[brackets$year == year, , drop = FALSE]
    b <- b[order(b$p_lower), , drop = FALSE]
    start <- c(0, b$p_upper); end <- c(b$p_lower, 1)
    gaps <- which(end - start > 6e-8)
    complete <- nrow(b) > 0L && !length(gaps) && all(end - start >= -6e-8)
    eligible <- complete && year >= first_full
    scope <- if (!nrow(b)) "missing" else if (complete) "full_population" else if (min(b$p_lower) > 0 && max(b$p_upper) == 1) "top_tail_only" else "partial_population"
    data.frame(country = source$country, year = year, step = "ptinc", unit = "mean_one",
      representation = "coarse", prepared_brackets = nrow(b),
      population_covered = sum(b$p_upper - b$p_lower), share_total = sum(b$bracket_share),
      uncovered_ranges = if (length(gaps)) paste(paste0(start[gaps] * 100, "–", end[gaps] * 100), collapse = ", ") else "",
      population_scope = scope, eligible_for_gpinter = as.integer(eligible),
      eligibility_reason = if (eligible) "Full-population observations; share-total decisions and harmonization review still required"
        else if (!nrow(b)) "No supplied distribution; year remains missing"
        else if (!complete) "Incomplete population; tail data cannot fill uncovered ranges"
        else "Before the registered full-distribution period",
      interpolate_missing_years = 0L, extrapolate_years = 0L,
      requires_harmonization = TRUE, publication = FALSE)
  }))
}

finished_dina_grouped_validate <- function(data, source) {
  d <- as.data.frame(data)
  x <- d[d$row_type == "supplied", , drop = FALSE]
  issues <- list()
  descriptions <- c(schema = "Grouped observation fields and source identifiers.",
    key_values = "Declared country, series, years and percentile bounds.",
    unique_keys = "One observation per code, year and percentile.",
    finite_values = "Supplied values must be finite; blanks represent missing coverage.",
    grouped_coverage = "Missing supplied observations are retained as coverage gaps.",
    share_bounds = "Shares must lie in [0,1].", top_share_order = "Nested top shares must not increase.",
    share_total = "Disjoint broad-group shares should sum to one.",
    grouped_mean_identity = "Matching shares and averages should imply a common overall mean.",
    metadata = "Population, monetary definitions and full attribution require confirmation.")
  add <- function(check, status, detail, row = x[1L, , drop = FALSE], field = "", observed = "", expected = "", difference = NA_real_) {
    issues[[length(issues) + 1L]] <<- data.frame(check_id = check, status = status,
      country = row$country, year = row$year, step = row$step, unit = row$unit,
      wid_code = row$wid_code, percentile = row$percentile, row = row$source_row,
      field = field, observed = as.character(observed), expected = as.character(expected),
      difference = difference, detail = detail, stringsAsFactors = FALSE)
  }
  required <- c("row_type", "country", "year", "step", "unit", "wid_code", "measure", "percentile",
    "p_lower", "p_upper", "value", "data_quality", "deflator", "source_sheet", "source_row", "source_column",
    "requires_harmonization", "publication_ready")
  if (!nrow(x) || !all(required %in% names(d))) stop("Grouped artifact has missing fields or no supplied observations.")
  spec <- source$finished_series
  expected_years <- seq(spec$first_year, spec$last_year)
  if (any(!is.finite(x$year) | x$year != floor(x$year) | !x$year %in% expected_years) ||
      anyNA(x$country) || any(x$country != source$country) ||
      any(!is.finite(x$p_lower) | !is.finite(x$p_upper) | x$p_lower < 0 | x$p_upper > 1 | x$p_lower >= x$p_upper)) {
    add("key_values", "fail", "Invalid country, year or percentile bounds.")
  }
  for (series in spec$series) {
    part <- x[x$wid_code == series$code, , drop = FALSE]
    expected <- expand.grid(year = expected_years, percentile = unlist(spec$percentiles), stringsAsFactors = FALSE)
    if (nrow(part) != nrow(expected) || !setequal(paste(part$year, part$percentile), paste(expected$year, expected$percentile)) ||
        any(part$measure != series$measure | part$unit != series$unit | part$source_sheet != series$sheet)) {
      add("key_values", "fail", paste("Missing or unexpected source keys for", series$code))
    }
  }
  if (anyNA(x$wid_code) || any(!x$wid_code %in% vapply(spec$series, function(s) s$code, character(1)))) add("key_values", "fail", "Unregistered WID code.")
  if (anyDuplicated(x[c("country", "year", "wid_code", "percentile")])) add("unique_keys", "fail", "Duplicate code/year/percentile observations.")
  if (any(is.infinite(x$value) | is.nan(x$value))) add("finite_values", "fail", "Nonfinite supplied observation.")
  if (any(!is.finite(x$deflator[x$measure == "average"]) | x$deflator[x$measure == "average"] <= 0)) add("finite_values", "fail", "Average-sheet deflators must be positive and finite.")
  absent <- which(is.na(x$value))
  for (i in absent) add("grouped_coverage", "warning", "No value supplied for this percentile and year; retained as missing.", x[i, ])
  for (name in c("population", "currency", "price_basis", "citation", "doi")) {
    value <- spec[[name]]
    if (is.null(value) || !nzchar(value) || grepl("unconfirmed", value, ignore.case = TRUE)) {
      row <- x[1L, ]; row$year <- NA_real_; row$percentile <- ""; row$wid_code <- ""; row$unit <- ""
      add("metadata", "warning", paste("Unconfirmed definition:", name), row, field = name)
    }
  }
  for (i in which(x$measure == "share" & !is.na(x$value) & (x$value < 0 | x$value > 1))) {
    add("share_bounds", "review", "Supplied share outside [0,1]; retained for harmonization.", x[i, ], "value", x$value[i], "[0,1]")
  }
  # Structural failures must be reported before attempting scientific arithmetic.
  structural <- length(issues) && any(vapply(issues, function(i) i$status == "fail", logical(1)))
  if (!structural) for (year in expected_years) {
    s <- x[x$year == year & x$measure == "share" & !is.na(x$value), , drop = FALSE]
    top <- s[s$p_upper == 1, , drop = FALSE]; top <- top[order(top$p_lower), , drop = FALSE]
    for (i in which(diff(top$value) > 1e-6) + 1L) add("top_share_order", "review", "Nested top share exceeds its enclosing share.", top[i, ], "value", top$value[i], top$value[i - 1L], top$value[i] - top$value[i - 1L])
    broad <- s[s$percentile %in% c("p0p50", "p50p90", "p90p100"), , drop = FALSE]
    if (nrow(broad) == 3L && abs(sum(broad$value) - 1) > 1e-6) add("share_total", "review", "Broad shares do not sum to one; no normalization applied.", broad[1L, ], "sum(shares)", sum(broad$value), 1, sum(broad$value) - 1)
    a <- x[x$year == year & x$measure == "average", , drop = FALSE]
    pairs <- merge(s, a, by = "percentile", suffixes = c(".s", ".a"))
    pairs <- pairs[!is.na(pairs$value.a) & pairs$value.s > 0, , drop = FALSE]
    if (nrow(pairs) > 1L) {
      mean <- pairs$value.a * (pairs$p_upper.s - pairs$p_lower.s) / pairs$value.s
      reference <- stats::median(mean)
      for (i in which(abs(mean - reference) > 1e-6 + 1e-6 * pmax(abs(mean), abs(reference)))) {
        row <- s[s$percentile == pairs$percentile[i], , drop = FALSE]
        add("grouped_mean_identity", "review", "Implied mean differs from the median implied mean; shares take precedence and monetary scale is deferred.", row, "implied_mean", mean[i], reference, mean[i] - reference)
      }
    }
  }
  if (any(!d$requires_harmonization | d$publication_ready) || anyNA(d$requires_harmonization) || anyNA(d$publication_ready)) add("schema", "fail", "Coarse observations must remain flagged as requiring harmonization and ineligible for publication.")
  if (any(!d$row_type %in% c("supplied", "bracket"))) add("schema", "fail", "Unknown coarse artifact row type.")
  if (any(d$row_type == "bracket") && !structural) {
    expected <- finished_dina_grouped_brackets(x)
    actual <- d[d$row_type == "bracket", , drop = FALSE]
    fields <- c("country", "year", "percentile", "p_lower", "p_upper", "bracket_share", "avg_norm", "basis_percentile", "subtract_percentile", "basis_share", "subtract_share")
    if (!all(fields %in% names(actual)) || !isTRUE(all.equal(actual[fields], expected[fields], check.attributes = FALSE, tolerance = 1e-12))) add("schema", "fail", "Derived brackets differ from deterministic share subtraction.")
  }
  eligibility_fields <- c("eligible_for_gpinter", "population_scope", "interpolate_missing_years", "extrapolate_years")
  if (any(eligibility_fields %in% names(d)) && !structural) {
    years <- finished_dina_grouped_years(x, source)
    expected <- years[match(d$year, years$year), eligibility_fields, drop = FALSE]
    if (!all(eligibility_fields %in% names(d)) || !isTRUE(all.equal(d[eligibility_fields], expected, check.attributes = FALSE))) {
      add("schema", "fail", "Harmonization eligibility differs from observed population coverage or the registered policy.")
    }
  }
  findings <- if (length(issues)) do.call(rbind, issues) else data.frame()
  checks <- do.call(rbind, lapply(names(descriptions), function(id) {
    statuses <- findings$status[findings$check_id == id]
    ranks <- c(pass = 0L, warning = 1L, review = 2L, fail = 3L)
    data.frame(check_id = id, status = names(ranks)[max(c(0L, ranks[statuses])) + 1L],
      affected_rows = sum(findings$check_id == id), affected_cases = length(unique(findings$year[findings$check_id == id & !is.na(findings$year)])), detail = descriptions[[id]])
  }))
  coverage <- do.call(rbind, lapply(split(x, paste(x$wid_code, x$percentile)), function(part) {
    years <- part$year[!is.na(part$value) & is.finite(part$year)]
    data.frame(country = source$country, step = "ptinc", unit = part$unit[1L], wid_code = part$wid_code[1L],
      percentile = part$percentile[1L], first_year = if (length(years)) min(years) else NA_real_,
      last_year = if (length(years)) max(years) else NA_real_, n_years = length(years), n_groups = length(years),
      rows = length(years), min_brackets = NA_integer_, max_brackets = NA_integer_, publication = FALSE,
      missing_years = paste(setdiff(expected_years, years), collapse = ","),
      internal_gaps = if (length(years)) paste(setdiff(seq(min(years), max(years)), years), collapse = ",") else "")
  }))
  list(checks = checks, issues = findings, coverage = coverage, values = x)
}

finished_dina_grouped_coarse <- function(data, source) {
  validation <- finished_dina_grouped_validate(data, source)
  if (any(validation$checks$status == "fail")) stop("Grouped source has structural failures.")
  brackets <- finished_dina_grouped_brackets(data)
  columns <- unique(c(names(data), names(brackets)))
  fill <- function(x) {for (name in setdiff(columns, names(x))) x[[name]] <- rep(NA, nrow(x)); x[columns]}
  prepared <- rbind(fill(data), fill(brackets))
  # Preserve text storage even for columns whose supplied rows are all missing.
  for (name in c("basis_percentile", "subtract_percentile")) if (name %in% names(prepared)) {
    prepared[[name]] <- as.character(prepared[[name]])
    prepared[[name]][is.na(prepared[[name]])] <- ""
  }
  for (name in c("requires_harmonization", "publication_ready")) prepared[[name]] <- as.integer(prepared[[name]])
  summary <- finished_dina_grouped_years(data, source, brackets)
  for (name in c("eligible_for_gpinter", "population_scope", "interpolate_missing_years", "extrapolate_years")) {
    prepared[[name]] <- summary[[name]][match(prepared$year, summary$year)]
  }
  changes <- if (nrow(brackets)) data.frame(country = brackets$country, year = brackets$year,
    step = brackets$step, unit = brackets$unit, percentile = brackets$percentile,
    field = "bracket_share", before = brackets$basis_share, after = brackets$bracket_share,
    difference = -brackets$subtract_share, relative_percent = ifelse(brackets$basis_share == 0, NA_real_, -100 * brackets$subtract_share / brackets$basis_share),
    rule = ifelse(brackets$subtract_percentile == "", "retain_share", "nested_share_subtraction"),
    reason = paste0(brackets$basis_percentile, ifelse(brackets$subtract_percentile == "", " retained", paste0(" minus ", brackets$subtract_percentile)), "; avg_norm = share / population width; monetary scale deferred.")) else data.frame()
  list(data = prepared, summary = summary, changes = changes, impact = data.frame(),
    validation = finished_dina_grouped_validate(prepared, source),
    recipe = list(id = "wid-grouped-shares", version = 2L, authority = "shares",
      income_scale = "mean_one", normalization = "none", requires_harmonization = TRUE,
      require_full_population = TRUE, complete_partial_population = FALSE,
      interpolate_missing_years = FALSE, extrapolate_years = FALSE))
}

finished_dina_grouped_prepared_spec <- function(source, supplied) {
  coverage <- finished_dina_grouped_years(supplied, source)
  meta <- source$finished_series
  if (is.null(meta$prepared_step) || is.null(meta$prepared_unit)) stop("Register the grouped prepared concept and population unit.")
  list(countries = source$country, years = coverage$year[coverage$eligible_for_gpinter == 1],
    steps = meta$prepared_step, units = meta$prepared_unit,
    definitions = list(population_age = meta$population,
      currency_price_basis = "Mean-one shares; supplied monetary scale is not used",
      bracket_semantics = "GPinter estimates from normalized disjoint population-bracket shares"))
}

finished_dina_grouped_validate_prepared <- function(data, source, supplied) {
  spec <- finished_dina_grouped_prepared_spec(source, supplied)
  if (!length(spec$years)) stop("No observed full-population years are eligible for GPinter.")
  result <- finished_dina_validate(data, spec)
  # Publication is separately approved; source preparation must not silently
  # introduce time interpolation, tail-only years, or monetary scaling.
  flags <- c("requires_harmonization", "publication_ready", "interpolate_missing_years", "extrapolate_years")
  bad <- !all(flags %in% names(data)) || any(vapply(flags, function(f) anyNA(data[[f]]) || any(data[[f]] != 0), logical(1))) ||
    !"income_scale" %in% names(data) || anyNA(data$income_scale) || any(data$income_scale != "mean_one") ||
    !"average" %in% names(data) || any(!is.finite(data$average) | abs(data$average - 1) > 1e-12)
  if (bad) {
    check <- data.frame(check_id = "preparation", status = "fail", affected_rows = nrow(data),
      affected_cases = length(unique(data$year)), detail = "Prepared grouped data violate the observed-year, mean-one or publication-approval policy.")
    result$checks <- rbind(result$checks, check)
  }
  result
}

finished_dina_grouped_prepare <- function(data, source, progress = NULL) {
  if (!requireNamespace("gpinter", quietly = TRUE)) stop("Grouped DINA preparation requires gpinter.")
  coarse <- finished_dina_grouped_coarse(data, source)
  share_errors <- coarse$validation$issues
  if (any(share_errors$check_id %in% c("share_bounds", "top_share_order") & share_errors$status == "review")) {
    stop("Invalid or nonnested supplied shares cannot be repaired by proportional normalization.")
  }
  coverage <- coarse$summary
  years <- coverage$year[coverage$eligible_for_gpinter == 1]
  if (!length(years)) stop("No observed full-population years are eligible for GPinter.")
  policy <- source$finished_series$harmonization
  if (!identical(policy$share_normalization, "proportional")) stop("Register the reviewed grouped share normalization policy: proportional.")
  spec <- finished_dina_grouped_prepared_spec(source, data)
  brackets <- coarse$data[coarse$data$row_type == "bracket", , drop = FALSE]
  grid <- finished_dina_export_grid()
  end <- c(grid[-1L], 1)
  label <- function(x) ifelse(x == 0, "0", sub("\\.?0+$", "", sprintf("%.6f", x * 100)))
  output <- changes <- impacts <- summaries <- list()
  for (year in years) {
    if (is.function(progress)) progress(length(output) + 1L, length(years), paste(source$country, year, "grouped shares"))
    b <- brackets[brackets$year == year, , drop = FALSE]
    b <- b[order(b$p_lower), , drop = FALSE]
    abort <- function(message) stop(source$country, " ", year, ": ", message, call. = FALSE)
    width <- b$p_upper - b$p_lower
    total <- sum(b$bracket_share)
    if (!is.finite(total) || total <= 0 || any(!is.finite(b$bracket_share) | b$bracket_share <= 0)) abort("GPinter requires finite positive disjoint bracket shares.")
    shares <- b$bracket_share / total
    means <- shares / width
    if (any(diff(means) < -1e-6)) abort("Normalized bracket means are reversed; no pooling is authorized for this source.")
    fit <- tryCatch(gpinter::shares_fit(p = b$p_lower, average = 1,
      bracketshare = shares, first_threshold = 0), error = function(e) abort(conditionMessage(e)))
    tabulate <- function(p) gpinter::generate_tabulation(fit, fractiles = p, threshold = TRUE,
      bracketavg = TRUE, bracketshare = TRUE, topavg = TRUE, topshare = TRUE)
    knots <- tabulate(b$p_lower)
    error <- max(abs(knots$bracket_share - shares))
    if (!is.finite(error) || error > 1e-6) abort("GPinter did not preserve normalized source bracket shares.")
    tab <- tabulate(grid)
    gini <- as.numeric(gpinter::gini(fit))
    output[[length(output) + 1L]] <- data.frame(country = source$country, year = year,
      step = spec$steps, unit = spec$units, percentile = paste0("p", label(grid), "p", label(end)),
      p = grid, average = 1, gini = gini, fitted_gini = gini,
      thr = tab$threshold, avg = tab$bracket_average, bckt_sh = tab$bracket_share,
      topavg = tab$top_average, topsh = tab$top_share, income_scale = "mean_one",
      source_share_total = total, share_normalization = "proportional", source_id = source$id,
      source_version = source$finished_series$version, requires_harmonization = 0L,
      publication_ready = 0L, interpolate_missing_years = 0L, extrapolate_years = 0L,
      stringsAsFactors = FALSE)
    normalization <- data.frame(country = source$country, year = year, step = spec$steps, unit = spec$units,
      percentile = b$percentile, field = "bracket_share", before = b$bracket_share, after = shares,
      difference = shares - b$bracket_share, relative_percent = 100 * (shares / b$bracket_share - 1),
      rule = "proportional_share_normalization", reason = "Scale all disjoint shares by their sum; retain relative proportions and use overall mean one.")
    changes[[length(changes) + 1L]] <- normalization
    impacts[[length(impacts) + 1L]] <- data.frame(country = source$country, year = year, step = spec$steps,
      unit = spec$units, statistic = "share_total", before = total, after = 1, difference = 1 - total,
      basis = "source_normalization", reason = "Proportional normalization before GPinter; supplied monetary averages are not used.")
    impacts[[length(impacts) + 1L]] <- data.frame(country = source$country, year = year, step = spec$steps,
      unit = spec$units, statistic = "max_source_share_error", before = 0, after = error, difference = error,
      basis = "same_population_boundary", reason = "Maximum absolute share error at the original observed bracket boundaries.")
    summaries[[length(summaries) + 1L]] <- data.frame(country = source$country, year = year, step = spec$steps,
      unit = spec$units, representation = "gpinter_from_grouped_shares", source_brackets = nrow(b),
      prepared_brackets = length(grid), threshold_repairs = 0L, threshold_nudges = 0L, pooled_bins = 0L,
      boundary_changes = 0L, max_source_share_error = error, source_share_total = total,
      share_normalization = "proportional", publication = FALSE)
  }
  prepared <- do.call(rbind, output)
  validation <- finished_dina_grouped_validate_prepared(prepared, source, data)
  if (any(validation$checks$status %in% c("fail", "review"))) stop("GPinter grouped distributions failed consistency validation.")
  # Keep exclusions visible in candidate reports while the prepared artifact
  # contains only fitted full-population years.
  coverage$included <- coverage$eligible_for_gpinter
  coverage$source_share_total <- coverage$share_total
  coverage$share_total[coverage$included == 1] <- 1
  coverage$prepared_brackets[coverage$included == 1] <- length(grid)
  coverage$representation[coverage$included == 1] <- "gpinter_from_grouped_shares"
  coverage$requires_harmonization[coverage$included == 1] <- FALSE
  source_changes <- coarse$changes[coarse$changes$year %in% years, , drop = FALSE]
  source_changes$step <- spec$steps; source_changes$unit <- spec$units
  list(data = prepared, validation = validation, coverage = coverage,
    summary = do.call(rbind, summaries), changes = rbind(source_changes, do.call(rbind, changes)),
    impact = do.call(rbind, impacts),
    recipe = list(id = "grouped-dina-shares-gpinter", version = 1L, authority = "shares",
      gpinter_version = as.character(utils::packageVersion("gpinter")), income_scale = "mean_one",
      normalization = "proportional", require_full_population = TRUE,
      complete_partial_population = FALSE, interpolate_missing_years = FALSE, extrapolate_years = FALSE))
}
