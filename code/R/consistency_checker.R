#!/usr/bin/env Rscript
# Small, deterministic corrections to the delivery representation. Scientific
# source files and GPinter fits are never rewritten by this module.
dina_consistency_grid <- function() {
  p <- round(c(seq(0, .99, .01), seq(.991, .999, .001),
    seq(.9991, .9999, .0001), seq(.99991, .99999, .00001)), 5)
  fmt <- function(x) vapply(x, format, character(1), scientific = FALSE, trim = TRUE, digits = 8)
  data.frame(p = p, end = c(p[-1L], 1), mass = diff(c(p, 1)),
    label = paste0("p", fmt(p * 100), "p", fmt(c(p[-1L], 1) * 100)),
    top = paste0("p", fmt(p * 100), "p100"), stringsAsFactors = FALSE)
}

dina_consistency_headline_names <- function() c(p0p20 = "Bottom 20%", p0p50 = "Bottom 50%",
  p50p90 = "Middle 40%", p90p100 = "Top 10%", p99p100 = "Top 1%",
  p99.9p100 = "Top 0.1%", p99.99p100 = "Top 0.01%")

dina_consistency_key <- function(x) paste(x$country, x$year, x$step, sep = "/")
dina_consistency_bind <- function(x) {
  x <- Filter(function(z) is.data.frame(z) && nrow(z), x)
  if (!length(x)) return(dina_consistency_empty())
  out <- do.call(rbind, x); rownames(out) <- NULL; out
}
dina_consistency_empty <- function() data.frame(record_type = character(), phase = character(),
  country = character(), year = integer(), step = character(), p = character(), field = character(),
  before = double(), after = double(), absolute_difference = double(), relative_difference = double(),
  difference_pp = double(), rule = character(), reason = character(), count = integer(),
  status = character(), stringsAsFactors = FALSE)

dina_consistency_record <- function(case, type, phase, field, rule, reason,
    before = NA_real_, after = NA_real_, p = "", count = 1L, status = "corrected") {
  n <- max(length(before), length(after), length(p), length(count))
  before <- rep_len(before, n); after <- rep_len(after, n)
  delta <- after - before
  data.frame(record_type = type, phase = phase, country = case$country[1L],
    year = as.integer(case$year[1L]), step = case$step[1L], p = rep_len(p, n), field = field,
    before = before, after = after, absolute_difference = abs(delta),
    relative_difference = ifelse(is.finite(before) & before != 0, delta / abs(before), NA_real_),
    difference_pp = if (type == "headline") delta * 100 else NA_real_,
    rule = rule, reason = reason, count = as.integer(rep_len(count, n)), status = status,
    stringsAsFactors = FALSE)
}

dina_consistency_equality_tolerance <- function(left, right) {
  1e-12 * pmax(.Machine$double.xmin, abs(left), abs(right))
}

dina_consistency_ties <- function(x, numerical = FALSE) {
  if (length(x) < 2L) return(0L)
  tolerance <- if (numerical) dina_consistency_equality_tolerance(head(x, -1L), tail(x, -1L)) else 0
  pair <- is.finite(head(x, -1L)) & is.finite(tail(x, -1L)) & abs(diff(x)) <= tolerance
  sum(is.finite(x) & x > 0 & (c(FALSE, pair) | c(pair, FALSE)))
}

dina_consistency_derived <- function(avg, mass, p = NULL) {
  if (is.null(p)) p <- c(0, head(cumsum(mass), -1L))
  income <- avg * mass; overall <- sum(income)
  share <- income / overall
  bottom_income <- c(0, head(cumsum(income), -1L))
  list(s = share, topavg = rev(cumsum(rev(income))) / rev(cumsum(rev(mass))),
    topsh = rev(cumsum(rev(share))), bottomsh = c(0, head(cumsum(share), -1L)),
    bottomavg = c(0, bottom_income[-1L] / head(cumsum(mass), -1L)), mean = overall)
}

dina_consistency_headlines <- function(avg, mass) {
  grid <- dina_consistency_grid()
  if (length(avg) != nrow(grid) || any(!is.finite(avg)) || any(!is.finite(mass)) || sum(avg * mass) <= 0)
    return(setNames(rep(NA_real_, 7L), names(dina_consistency_headline_names())))
  s <- avg * mass / sum(avg * mass)
  c(p0p20 = sum(s[grid$p < .2]), p0p50 = sum(s[grid$p < .5]),
    p50p90 = sum(s[grid$p >= .5 & grid$p < .9]), p90p100 = sum(s[grid$p >= .9]),
    p99p100 = sum(s[grid$p >= .99]), p99.9p100 = sum(s[grid$p >= .999]),
    p99.99p100 = sum(s[grid$p >= .9999]))
}

dina_consistency_repair <- function(avg, thr, mass, relative_change = 1e-6) {
  n <- length(avg)
  if (n < 2L || length(thr) != n || length(mass) != n ||
      any(!is.finite(c(avg, thr, mass))) || any(mass <= 0) || any(avg < 0))
    stop("Incomplete, non-finite or negative bracket inputs; no correction applied.", call. = FALSE)
  mu <- sum(avg * mass)
  if (!is.finite(mu) || mu <= 0) stop("A positive overall income is required; no correction applied.", call. = FALSE)
  a <- avg / mu; t <- thr / mu; original <- a; original_t <- t
  # Share the delivery equality criterion with the audit. Keep the separate
  # bounds on numerical decreases and neighboring plateau expansion explicit.
  strict_tolerance <- dina_consistency_equality_tolerance(head(a, -1L), tail(a, -1L))
  tolerance <- 4096 * .Machine$double.eps * pmax(.Machine$double.xmin,
    abs(head(a, -1L)), abs(tail(a, -1L)))
  if (any(diff(a) < -tolerance)) stop("Decreasing means exceed the permitted numerical tie tolerance; original means and thresholds retained.", call. = FALSE)
  # Include almost-flat neighbors in a defective plateau. Otherwise their tiny
  # gaps can cap its new slope so tightly that the delivery still reads as flat.
  # This 16384-epsilon envelope is only run expansion; the decrease bound above
  # stays at 4096 epsilon, and valid increasing runs remain untouched below.
  runs <- split(seq_along(a), cumsum(c(TRUE, abs(diff(a)) > 4 * tolerance)))
  for (ii in runs) {
    if (length(ii) < 2L || any(a[ii] <= 0)) next
    if (all(diff(a[ii]) > strict_tolerance[head(ii, -1L)])) next
    center <- sum(a[ii] * mass[ii]) / sum(mass[ii])
    midpoint <- cumsum(mass[ii]) - mass[ii] / 2
    z <- midpoint - sum(midpoint * mass[ii]) / sum(mass[ii])
    slope <- center * relative_change / max(abs(z))
    # Bound each change against its original mean, including the small move
    # to the weighted center. Leave room for the final floating-point scaling.
    budget <- max(0, relative_change - 16 * .Machine$double.eps)
    lower_mean <- original[ii] * (1 - budget)
    upper_mean <- original[ii] * (1 + budget)
    slope <- min(slope, (upper_mean[z > 0] - center) / z[z > 0],
      (center - lower_mean[z < 0]) / -z[z < 0])
    if (min(ii) > 1L) slope <- min(slope, (center - a[min(ii) - 1L]) * .25 / (-min(z)))
    if (max(ii) < n) slope <- min(slope, (a[max(ii) + 1L] - center) * .25 / max(z))
    if (!is.finite(slope) || slope <= 0) stop("No feasible local positive slope; original group retained.", call. = FALSE)
    a[ii] <- center + slope * z
  }
  for (i in seq_along(t)) {
    lower <- if (i > 1L) a[i - 1L] else 0
    repeated <- i > 1L && t[i] > 0 && t[i] <= t[i - 1L] +
      dina_consistency_equality_tolerance(t[i], t[i - 1L])
    if (t[i] < lower || t[i] > a[i] || repeated) {
      if (t[i] == 0 && a[i] == 0) next
      if (a[i] <= lower) stop("No feasible threshold interior; original group retained.", call. = FALSE)
      margin <- min(1e-7 * max(abs(a[i]), 1), (a[i] - lower) / 4)
      t[i] <- max(lower + margin, min(a[i] - margin, t[i]))
    }
  }
  # Do not round-trip untouched values through normalization: this preserves
  # their exact bits and makes repeated preparation idempotent.
  outa <- avg; outt <- thr
  outa[a != original] <- (a * mu)[a != original]
  outt[t != original_t] <- (t * mu)[t != original_t]
  if (any(diff(outa) < 0) || any(diff(outt) < 0) || dina_consistency_ties(outa, numerical = TRUE) ||
      dina_consistency_ties(outt, numerical = TRUE) || any(outa < outt) || any(head(outa, -1L) > tail(outt, -1L)) ||
      any(abs(outa[avg > 0] - avg[avg > 0]) / avg[avg > 0] > relative_change) ||
      abs(sum(outa * mass) - mu) > 128 * .Machine$double.eps * max(abs(mu), .Machine$double.xmin))
    stop("Local correction could not preserve income and ordered bounds; original group retained.", call. = FALSE)
  list(avg = outa, thr = outt, derived = dina_consistency_derived(outa, mass))
}

dina_consistency_prepare <- function(data, phase = "assembled") {
  required <- c("country", "year", "step", "p", "bracketavg_norm", "thr", "bracketpop")
  if (!all(required %in% names(data))) stop("Missing preparation fields: ", paste(setdiff(required, names(data)), collapse = ", "), call. = FALSE)
  data <- as.data.frame(data); grid <- dina_consistency_grid()
  for (v in c("bracketavg", "s", "topavg", "topsh", "bottomsh", "bottomavg"))
    if (!v %in% names(data)) data[[v]] <- NA_real_
  details <- list(); snapshots <- list(); metrics <- list()
  groups <- split(seq_len(nrow(data)), dina_consistency_key(data))
  for (key in names(groups)) {
    ii <- groups[[key]]; ii <- ii[order(data$p[ii])]
    d <- data[ii, ]; case <- d[1L, c("country", "year", "step"), drop = FALSE]
    eligible <- if ("consistency_publish" %in% names(d)) all(d$consistency_publish == 1, na.rm = TRUE) else TRUE
    avg <- as.numeric(d$bracketavg_norm); thr <- as.numeric(d$thr); mass <- as.numeric(d$bracketpop) / 100000
    # This is a normalized handoff even when the group's correction is not
    # feasible. Retaining a raw-currency bracketavg would scale it a second time.
    data$bracketavg[ii] <- avg
    result <- tryCatch({
      if (nrow(d) != nrow(grid) || any(!is.finite(d$p)) ||
          any(abs(d$p / 100000 - grid$p) > 1e-10) || any(!is.finite(mass)) ||
          any(abs(mass - grid$mass) > 1e-10)) stop("Incomplete grid or changed population weights; original group retained.")
      dina_consistency_repair(avg, thr, mass)
    }, error = identity)
    before <- dina_consistency_headlines(avg, mass)
    if (inherits(result, "error")) {
      details[[length(details) + 1L]] <- dina_consistency_record(case, "issue", phase,
        "distribution", "unresolved_preparation", conditionMessage(result), status = "unresolved")
      after_avg <- avg; after_thr <- thr; after <- before
      # A primitive ordering problem does not prevent arithmetic identities.
      # Rebuild aggregates from the available unchanged means and masses;
      # otherwise newly introduced bottom statistics would remain missing.
      complete <- nrow(d) == nrow(grid) && all(is.finite(c(d$p, avg, mass))) &&
        all(abs(d$p / 100000 - grid$p) <= 1e-10) &&
        all(abs(mass - grid$mass) <= 1e-10) && all(avg >= 0) && all(mass > 0) &&
        is.finite(sum(avg * mass)) && sum(avg * mass) > 0
      if (complete) {
        derived <- dina_consistency_derived(avg, mass)
        for (v in c("s", "topavg", "topsh", "bottomsh", "bottomavg")) data[[v]][ii] <- derived[[v]]
      }
    } else {
      after_avg <- result$avg; after_thr <- result$thr
      data$bracketavg_norm[ii] <- after_avg; data$bracketavg[ii] <- after_avg
      data$thr[ii] <- after_thr
      for (v in c("s", "topavg", "topsh", "bottomsh", "bottomavg")) data[[v]][ii] <- result$derived[[v]]
      for (field in c("mean", "threshold")) {
        old <- if (field == "mean") avg else thr
        new <- if (field == "mean") after_avg else after_thr
        changed <- which(old != new)
        if (length(changed)) details[[length(details) + 1L]] <- dina_consistency_record(case,
          "correction", phase, field,
          if (field == "mean") "local_income_preserving_slope" else "nearest_feasible_threshold",
          if (field == "mean") "Small increasing slope; population and the affected run's total income preserved." else
            "Nearest feasible threshold interior; unchanged bracket population.",
          old[changed], new[changed], grid$label[changed])
      }
      after <- dina_consistency_headlines(after_avg, mass)
      changed <- which(abs(after - before) > 128 * .Machine$double.eps)
      if (length(changed)) details[[length(details) + 1L]] <- dina_consistency_record(case,
        "headline", phase, "share", "derived_from_corrected_means", "Income share recomputed from corrected bracket means.",
        before[changed], after[changed], names(after)[changed],
        status = ifelse(abs(after[changed] - before[changed]) * 100 > .01, "review", "corrected"))
    }
    metrics[[key]] <- cbind(case, data.frame(eligible = eligible,
      repeated_averages_before = dina_consistency_ties(avg), repeated_thresholds_before = dina_consistency_ties(thr),
      repeated_averages_after = dina_consistency_ties(after_avg), repeated_thresholds_after = dina_consistency_ties(after_thr),
      mean_before = if (all(is.finite(c(avg, mass)))) sum(avg * mass) else NA_real_,
      mean_after = if (all(is.finite(c(after_avg, mass)))) sum(after_avg * mass) else NA_real_))
    # Numeric-only snapshots, without the long repeated citation strings.
    snapshots[[key]] <- list(case = case, eligible = eligible, p = d$p,
      mass = mass, avg_before = avg, thr_before = thr, avg_after = after_avg,
      thr_after = after_thr, headline_before = before, headline_after = after)
  }
  list(data = data, state = list(version = 1L, phase = phase, snapshots = snapshots,
    metrics = if (length(metrics)) do.call(rbind, metrics) else data.frame(),
    details = dina_consistency_bind(details)))
}

dina_consistency_prepare_file <- function(input, output, work_dir, phase) {
  if (!phase %in% c("sources", "assembled")) stop("Preparation phase must be sources or assembled.", call. = FALSE)
  result <- dina_consistency_prepare(haven::read_dta(input), phase)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("consistency-", tmpdir = dirname(output), fileext = ".dta")
  on.exit(unlink(temporary), add = TRUE)
  haven::write_dta(result$data, temporary, version = 14)
  saveRDS(result$state, file.path(work_dir, paste0(phase, ".rds")))
  if (!file.rename(temporary, output)) stop("Could not install the prepared consistency output.", call. = FALSE)
  issues <- sum(result$state$details$status == "unresolved")
  cat("Consistency preparation:", phase, "-", length(result$state$snapshots), "distributions,", issues, "unresolved findings.\n")
  invisible(result$state)
}

dina_consistency_read_states <- function(work_dir) {
  if (is.null(work_dir) || !nzchar(work_dir)) return(list())
  paths <- file.path(work_dir, c("sources.rds", "assembled.rds"))
  states <- setNames(lapply(paths[file.exists(paths)], readRDS), sub("[.]rds$", "", basename(paths[file.exists(paths)])))
  donors <- file.path(work_dir, "regional_donors.csv")
  if (file.exists(donors)) states <- dina_consistency_regional_reference(states,
    utils::read.csv(donors, stringsAsFactors = FALSE))
  states
}

dina_consistency_share_change <- function(before, after) {
  difference <- after - before
  difference[is.finite(difference) & abs(difference) <= 128 * .Machine$double.eps] <- 0
  difference
}

# Reporting only: reconstruct the uncorrected regional reference from the
# actual donor map written by Stata. Never infer or duplicate country policies.
dina_consistency_regional_reference <- function(states, donors) {
  if (!all(c("sources", "assembled") %in% names(states))) return(states)
  if (!all(c("country", "step", "donor") %in% names(donors)))
    stop("Regional donor map requires country, step and donor.", call. = FALSE)
  donors <- unique(donors[c("country", "step", "donor")])
  pools <- split(as.character(donors$donor), paste(donors$country, donors$step, sep = "/"))
  source <- states$sources$snapshots; assembled <- states$assembled$snapshots
  reference <- list(); details <- list(); cache <- new.env(parent = emptyenv())
  for (key in setdiff(names(assembled), names(source))) {
    snap <- assembled[[key]]; case <- snap$case
    pool <- pools[[paste(case$country, case$step, sep = "/")]]
    if (!length(pool)) next
    result <- tryCatch({
      cache_key <- paste(case$year, case$step, paste(sort(pool), collapse = ","), sep = "/")
      if (!exists(cache_key, cache, inherits = FALSE)) {
        donor_keys <- paste(pool, case$year, case$step, sep = "/")
        if (!all(donor_keys %in% names(source))) stop("A recorded regional donor snapshot is unavailable.")
        inputs <- source[donor_keys]
        if (any(vapply(inputs, function(d) length(d$mass) != length(snap$mass) ||
            any(!is.finite(d$mass)) || any(abs(d$mass - snap$mass) > 1e-12), logical(1))))
          stop("Regional donor population grids do not agree.")
        values <- lapply(c("avg_before", "avg_after", "thr_before", "thr_after"), function(field)
          rowMeans(vapply(inputs, `[[`, numeric(length(snap$mass)), field)))
        names(values) <- c("avg_before", "avg_after", "thr_before", "thr_after")
        assign(cache_key, values, cache)
      }
      values <- get(cache_key, cache, inherits = FALSE)
      for (field in c("avg", "thr")) {
        reconstructed <- values[[paste0(field, "_after")]]; observed <- snap[[paste0(field, "_before")]]
        if (any(!is.finite(c(reconstructed, observed))) ||
            any(abs(reconstructed - observed) > 128 * .Machine$double.eps *
              pmax(.Machine$double.xmin, abs(reconstructed), abs(observed))))
          stop("Recorded donor means do not reproduce the assembled regional distribution.")
      }
      values
    }, error = identity)
    if (inherits(result, "error")) {
      details[[length(details) + 1L]] <- dina_consistency_record(case, "issue", "regional",
        "reference", "unresolved_regional_reference", conditionMessage(result), status = "unresolved")
      next
    }
    snap$avg_before <- result$avg_before; snap$thr_before <- result$thr_before
    snap$headline_before <- dina_consistency_headlines(snap$avg_before, snap$mass)
    delta <- dina_consistency_share_change(snap$headline_before, snap$headline_after)
    changed <- which(is.finite(delta) & delta != 0)
    if (length(changed)) details[[length(details) + 1L]] <- dina_consistency_record(case,
      "headline", "regional", "share", "inherited_donor_correction",
      "Regional income share reflects the same corrected source distributions used for their own exports.",
      snap$headline_before[changed], snap$headline_after[changed], names(delta)[changed],
      status = ifelse(abs(delta[changed]) * 100 > .01, "review", "corrected"))
    reference[[key]] <- snap
  }
  states$regional <- list(snapshots = reference, details = dina_consistency_bind(details))
  states
}

dina_consistency_step <- function(code) unname(c(ptinc = "nat", diinc = "pon", cainc = "pod")[substr(code, 2L, 6L)])
dina_consistency_code <- function(step) unname(c(nat = "ptinc", pon = "diinc", pod = "cainc")[step])

dina_consistency_audit <- function(data, expected = NULL, states = list(), wide = NULL) {
  required <- c("country", "year", "p", "widcode", "value")
  if (!all(required %in% names(data))) stop("Missing export fields: ", paste(setdiff(required, names(data)), collapse = ", "), call. = FALSE)
  data <- as.data.frame(data); data$step <- dina_consistency_step(data$widcode)
  data$kind <- substr(data$widcode, 1L, 1L); data$key <- dina_consistency_key(data)
  grid <- dina_consistency_grid(); issue <- list(); audited <- list()
  if (is.character(expected)) expected <- utils::read.csv(expected, stringsAsFactors = FALSE)
  scope_source <- "provided"
  if (is.null(expected) && "assembled" %in% names(states)) {
    expected <- states$assembled$metrics
    expected <- expected[expected$eligible, c("country", "year", "step"), drop = FALSE]
    scope_source <- "pre-scaling assembly"
  }
  if (is.null(expected)) {
    expected <- unique(data[!is.na(data$step), c("country", "year", "step"), drop = FALSE])
    scope_source <- "observed export cases only"
    issue[[length(issue) + 1L]] <- dina_consistency_record(data.frame(country = "ALL", year = NA_integer_, step = ""),
      "issue", "audit", "coverage", "scope_unverified",
      "No independent expected-case inventory was available; complete absence of a country or year cannot be certified.", status = "unresolved")
  }
  if (!all(c("country", "year", "step") %in% names(expected))) stop("Expected cases require country, year and step.", call. = FALSE)
  expected <- unique(as.data.frame(expected[c("country", "year", "step")]))
  expected <- expected[order(expected$country, expected$year, expected$step), , drop = FALSE]
  expected$key <- dina_consistency_key(expected)
  split_data <- split(seq_len(nrow(data)), data$key)
  split_wide <- if (!is.null(wide)) split(seq_len(nrow(wide)), dina_consistency_key(wide)) else list()
  add <- function(case, field, rule, reason, count = 1L, before = NA_real_, after = NA_real_, p = "") {
    issue[[length(issue) + 1L]] <<- dina_consistency_record(case, "issue", "audit", field, rule,
      reason, before, after, p, count, "unresolved")
  }
  extra <- setdiff(unique(data$key), expected$key)
  for (key in extra) {
    d <- data[split_data[[key]][1L], ]
    add(d, "coverage", "unexpected_case", "Export contains a case outside the expected publication scope.")
  }
  close <- function(a, b, relative = 1e-8, absolute = 1e-12)
    abs(a - b) <= absolute + relative * pmax(abs(a), abs(b))
  for (j in seq_len(nrow(expected))) {
    case <- expected[j, ]; key <- case$key
    rows <- split_data[[key]]; d <- if (length(rows)) data[rows, , drop = FALSE] else data[FALSE, ]
    if (!nrow(d)) {
      add(case, "distribution", "missing_distribution", "Expected country-year-concept is absent from the saved export.", 381L)
      audited[[key]] <- list(case = case, averages = rep(NA_real_, 127), thresholds = rep(NA_real_, 127),
        shares = rep(NA_real_, 127), repeated_averages = 0L, repeated_thresholds = 0L,
        missing_averages = 127L, missing_thresholds = 127L)
      next
    }
    dupe <- duplicated(paste(d$widcode, d$p))
    if (any(dupe)) add(case, "keys", "duplicate_records", "Conflicting or repeated export keys are retained for review, not silently discarded.", sum(dupe))
    read_kind <- function(kind) {
      z <- d[d$kind == kind, , drop = FALSE]
      index <- match(grid$label, z$p)
      list(values = z$value[index], absent = sum(is.na(index)), missing = sum(!is.finite(z$value[index])))
    }
    a <- read_kind("a"); t <- read_kind("t"); s <- read_kind("s")
    w <- if (length(split_wide[[key]])) wide[split_wide[[key]], , drop = FALSE] else NULL
    benchmark <- NA_real_
    if (!is.null(w)) {
      benchmark_field <- if (case$step == "pod" && "income_mean" %in% names(w)) "income_mean" else "anninc992i"
      if (benchmark_field %in% names(w)) {
        available <- unique(w[[benchmark_field]][is.finite(w[[benchmark_field]])])
        if (length(available) == 1L && available > 0) benchmark <- available
        else add(case, "benchmark", if (!length(available)) "missing_benchmark" else "invalid_benchmark",
          "A unique positive WID-scaled income mean is unavailable; monetary values remain explicitly missing.")
      }
    }
    for (field in c("averages", "thresholds", "shares")) {
      z <- switch(field, averages = a, thresholds = t, shares = s)
      if (z$absent) add(case, field, "missing_bracket_records", "Required bracket records are absent from the saved file.", z$absent)
      if (z$missing > z$absent) add(case, field, "missing_values", "Required bracket records have missing or non-finite values.", z$missing - z$absent)
    }
    if (!s$missing) {
      if (any(s$values < 0)) add(case, "shares", "negative_share", "Negative bracket income shares.", sum(s$values < 0))
      if (abs(sum(s$values) - 1) > 1e-8) add(case, "shares", "share_sum", "Bracket shares do not sum to one.", before = sum(s$values), after = 1)
    }
    if (!a$missing) {
      if (any(a$values < 0)) add(case, "averages", "negative_mean", "Negative bracket means are outside the current export preparation rules.", sum(a$values < 0))
      if (any(diff(a$values) < 0)) add(case, "averages", "decreasing_means", "Bracket means decrease with percentile.", sum(diff(a$values) < 0))
      if (dina_consistency_ties(a$values)) add(case, "averages", "repeated_positive_means", "Repeated positive means remain after preparation.", dina_consistency_ties(a$values))
      near <- dina_consistency_ties(a$values, numerical = TRUE) - dina_consistency_ties(a$values)
      if (near > 0L) add(case, "averages", "indistinguishable_positive_means", "Additional positive means differ only within numerical equality tolerance.", near)
      mu <- sum(a$values * grid$mass)
      if (is.finite(benchmark) && !close(mu, benchmark, 1e-10)) add(case, "mean", "benchmark_identity", "Weighted monetary mean differs from its WID-scaled benchmark.", before = mu, after = benchmark)
      if (!s$missing && is.finite(mu) && mu > 0 && any(!close(s$values, a$values * grid$mass / mu)))
        add(case, "shares", "mean_share_identity", "Detailed shares disagree with bracket means and total income.", sum(!close(s$values, a$values * grid$mass / mu)))
    }
    if (!t$missing) {
      if (any(diff(t$values) < 0)) add(case, "thresholds", "decreasing_thresholds", "Thresholds decrease with percentile.", sum(diff(t$values) < 0))
      if (dina_consistency_ties(t$values)) add(case, "thresholds", "repeated_positive_thresholds", "Repeated positive thresholds remain after preparation.", dina_consistency_ties(t$values))
      near <- dina_consistency_ties(t$values, numerical = TRUE) - dina_consistency_ties(t$values)
      if (near > 0L) add(case, "thresholds", "indistinguishable_positive_thresholds", "Additional positive thresholds differ only within numerical equality tolerance.", near)
      if (any(t$values < 0)) add(case, "thresholds", "negative_threshold", "Negative thresholds remain.", sum(t$values < 0))
    }
    if (!a$missing && !t$missing) {
      bad <- a$values < t$values | a$values > c(t$values[-1L], Inf)
      if (any(bad)) add(case, "bounds", "mean_threshold_bounds", "Bracket means lie outside their threshold interval.", sum(bad))
    }
    check_aggregates <- function(kind, labels, values, field) {
      z <- d[d$kind == kind, ]; index <- match(labels, z$p)
      absent <- is.na(index); missing <- !is.finite(z$value[index])
      if (any(absent)) add(case, field, "missing_aggregate_records", "Required cumulative or headline aggregate records are absent.", sum(absent))
      if (any(missing & !absent)) add(case, field, "missing_aggregate_values", "Required aggregate values are missing or non-finite.", sum(missing & !absent))
      valid <- !missing & is.finite(values)
      bad <- valid & !close(z$value[index], values)
      if (any(bad)) add(case, field, "aggregate_identity", "Aggregates disagree with their detailed bracket income or shares.", sum(bad))
    }
    top_s <- if (!s$missing) rev(cumsum(rev(s$values))) else rep(NA_real_, 127L)
    top_a <- if (!a$missing) rev(cumsum(rev(a$values * grid$mass))) / (1 - grid$p) else rep(NA_real_, 127L)
    check_aggregates("s", grid$top, top_s, "top_shares")
    check_aggregates("a", grid$top, top_a, "top_averages")
    lower <- list(p0p20 = grid$p < .2, p0p50 = grid$p < .5,
      p0p90 = grid$p < .9, p50p90 = grid$p >= .5 & grid$p < .9)
    bottom_s <- vapply(lower, function(ii) if (!s$missing) sum(s$values[ii]) else NA_real_, numeric(1))
    bottom_a <- vapply(lower[-1L], function(ii) if (!a$missing) sum(a$values[ii] * grid$mass[ii]) / sum(grid$mass[ii]) else NA_real_, numeric(1))
    check_aggregates("s", names(bottom_s), bottom_s, "bottom_shares")
    check_aggregates("a", names(bottom_a), bottom_a, "bottom_averages")
    if (!is.null(w) && all(c("p", "bracketavg", "thr", "s") %in% names(w))) {
      wp <- if (max(w$p, na.rm = TRUE) <= 100) w$p / 100 else w$p / 100000
      index <- match(round(grid$p * 100000), round(wp * 100000))
      for (field in c("bracketavg", "thr", "s")) {
        v <- switch(field, bracketavg = a$values, thr = t$values, s = s$values)
        target <- w[[field]][index]
        bad <- xor(is.finite(v), is.finite(target)) | (is.finite(v) & is.finite(target) & !close(v, target, 1e-12))
        if (any(bad)) add(case, field, "wide_long_disagreement", "The saved wide and long exports disagree.", sum(bad))
      }
    }
    audited[[key]] <- list(case = case, averages = a$values, thresholds = t$values, shares = s$values,
      repeated_averages = dina_consistency_ties(a$values), repeated_thresholds = dina_consistency_ties(t$values),
      missing_averages = a$missing, missing_thresholds = t$missing)
  }
  list(expected = expected, scope_source = scope_source, details = dina_consistency_bind(issue), groups = audited)
}

# The display filter does not alter corrections, diagnostics or export values.
dina_consistency_review <- function(audit, details, threshold = .1) {
  relative <- function(before, after) {
    out <- abs(after - before) / abs(before) * 100
    out[before == 0 & after == 0] <- 0
    out[before == 0 & is.finite(after) & after != 0] <- Inf
    out
  }
  h <- details[details$record_type == "headline", , drop = FALSE]
  h$change_percent <- relative(h$before, h$after)
  h <- h[dina_consistency_key(h) %in% audit$expected$key, , drop = FALSE]
  material <- h[!is.na(h$change_percent) & h$change_percent >= threshold, , drop = FALSE]
  issues <- details[details$status == "unresolved", , drop = FALSE]
  keys <- intersect(unique(dina_consistency_key(issues)), audit$expected$key)
  grid <- dina_consistency_grid()
  rows <- lapply(keys, function(key) {
    z <- audit$groups[[key]]; a <- z$averages; t <- z$thresholds
    decrease <- gap <- NA_real_
    if (length(a) == 127L && all(is.finite(a))) {
      base <- pmax(abs(head(a, -1L)), abs(tail(a, -1L)))
      ratios <- pmax(-diff(a), 0) / base * 100; ratios[base == 0] <- 0
      decrease <- max(ratios)
      mu <- sum(a * grid$mass)
      if (length(t) == 127L && all(is.finite(t)) && mu > 0)
        gap <- max(pmax(t - a, 0), pmax(head(a, -1L) - tail(t, -1L), 0)) / mu * 100
    }
    case <- audit$expected[audit$expected$key == key, c("country", "year", "step"), drop = FALSE]
    cbind(case, mean_decrease_percent = decrease, threshold_gap_percent = gap)
  })
  cases <- if (length(rows)) do.call(rbind, rows) else data.frame(country = character(), year = integer(),
    step = character(), mean_decrease_percent = double(), threshold_gap_percent = double())
  numeric_rules <- c("unresolved_preparation", "decreasing_means", "decreasing_thresholds",
    "mean_threshold_bounds", "repeated_positive_means", "repeated_positive_thresholds",
    "indistinguishable_positive_means", "indistinguishable_positive_thresholds")
  structural <- unique(dina_consistency_key(issues[!issues$rule %in% numeric_rules, , drop = FALSE]))
  # Missing or unmeasurable findings always stay visible in the review table.
  important <- cases$mean_decrease_percent >= threshold | cases$threshold_gap_percent >= threshold |
    !is.finite(cases$mean_decrease_percent) | !is.finite(cases$threshold_gap_percent) |
    dina_consistency_key(cases) %in% structural
  list(threshold_percent = threshold, headline = h, material = material, cases = cases,
    important_cases = cases[which(important), , drop = FALSE])
}

dina_consistency_plot <- function(path, audit, states, details, threshold = .1) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  review <- dina_consistency_review(audit, details, threshold)
  snapshots <- list()
  for (phase in c("assembled", "sources", "regional")) if (phase %in% names(states))
    for (key in names(states[[phase]]$snapshots)) snapshots[[key]] <- states[[phase]]$snapshots[[key]]
  # Saved detail rows are sufficient to redraw a report after temporary work
  # files have been removed. The exported distributions are read only.
  grid <- dina_consistency_grid()
  for (key in audit$expected$key) {
    z <- audit$groups[[key]]
    after <- dina_consistency_headlines(z$shares / grid$mass, grid$mass)
    before <- if (!is.null(snapshots[[key]])) snapshots[[key]]$headline_before else after
    original <- review$headline[dina_consistency_key(review$headline) == key, , drop = FALSE]
    original <- original[!duplicated(original$p), , drop = FALSE]
    for (i in seq_len(nrow(original))) before[original$p[i]] <- original$before[i]
    snapshots[[key]] <- list(headline_before = before, headline_after = after)
  }
  grDevices::pdf(path, width = 11.7, height = 8.3, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  colors <- c(Before = "#777777", After = "#0072B2")
  graphics::par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
  graphics::plot.new(); graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  txt <- function(x, y, label, ...) graphics::text(x, y, label, adj = c(0, 1), ...)
  max_safe <- function(x) if (any(is.finite(x))) max(x[is.finite(x)]) else 0
  fmt <- function(x) if (!is.finite(x)) "Unavailable" else format(x, digits = 3, scientific = x != 0 && abs(x) < .001, trim = TRUE)
  txt(.05, .95, "Consistency review", cex = 1.45, font = 2)
  txt(.05, .885, paste0("Plot filter: at least ", threshold, "% relative change in an income share."), col = "#666666", cex = .9)
  txt(.05, .85, "Small corrections remain in the detail CSV. This filter does not change the data or diagnostic results.", col = "#666666", cex = .9)
  missing_a <- sum(vapply(audit$groups, function(z) z$missing_averages, numeric(1)))
  missing_t <- sum(vapply(audit$groups, function(z) z$missing_thresholds, numeric(1)))
  stats <- c(paste(nrow(audit$expected), "distributions checked"),
    paste("Missing averages / thresholds:", missing_a, "/", missing_t),
    paste("Largest headline-share change:", if (any(is.infinite(review$headline$change_percent)))
      "undefined relative to an initial zero share" else paste(fmt(max_safe(review$headline$change_percent)), "% relative")),
    paste("Cases reaching the plot filter:", length(unique(dina_consistency_key(review$material)))))
  for (i in seq_along(stats)) txt(.05, .79 - (i-1) * .045, stats[i], cex = 1)
  txt(.05, .575, if (!nrow(review$material) && !nrow(review$cases))
    "No unresolved consistency findings. All headline-share changes are below the plot filter." else if (!nrow(review$material) && !nrow(review$important_cases))
    "No plots: headline-share changes and remaining numerical discrepancies are below the display threshold." else
    "The table locates unresolved findings; plots below show changes that meet the display threshold.", cex = .95, font = 2)
  # Compact country table: cases are country-year-concept distributions.
  xs <- c(.05, .15, .46, .58, .66, .81)
  headers <- c("Country", "Years", "Concepts", "Cases", "Mean decline (%)", "Threshold gap (%)")
  graphics::rect(.045, .475, .955, .53, col = "#eeeeee", border = NA)
  for (i in seq_along(xs)) txt(xs[i], .515, headers[i], cex = .88, font = 2)
  years_text <- function(y) {
    y <- sort(unique(y)); runs <- split(y, cumsum(c(TRUE, diff(y) != 1)))
    paste(vapply(runs, function(z) if (length(z) == 1L) as.character(z) else
      paste0(min(z), "-", substr(as.character(max(z)), 3, 4)), character(1)), collapse = ", ")
  }
  by_country <- split(review$cases, review$cases$country)
  if (!length(by_country)) txt(.05, .45, "No unresolved cases.", cex = .95)
  for (i in seq_along(by_country)) {
    # Keep continuation pages only if a future review has many affected countries.
    if (i > 8L && (i-1L) %% 8L == 0L) {
      graphics::plot.new(); graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
      txt(.05, .95, "Unresolved findings - continued", cex = 1.3, font = 2)
    }
    d <- by_country[[i]]; row <- c(d$country[1], years_text(d$year), paste(sort(unique(d$step)), collapse = "/"),
      nrow(d), if (all(!is.finite(d$mean_decrease_percent))) "Unavailable" else fmt(max_safe(d$mean_decrease_percent)),
      if (all(!is.finite(d$threshold_gap_percent))) "Unavailable" else fmt(max_safe(d$threshold_gap_percent)))
    y <- if (i <= 8L) .45 - (i-1) * .04 else .86 - ((i-1) %% 8L) * .07
    for (j in seq_along(xs)) txt(xs[j], y, row[j], cex = .85)
  }
  txt(.05, .15, "Mean decline is relative to adjacent incomes. Threshold gap is measured against the overall income mean.", col = "#666666", cex = .86)
  txt(.05, .11, if (nrow(review$cases))
    "Tiny ordering and equality findings remain recorded; they are not treated as large distributional changes." else
    "All checked distributions pass the consistency audit; the detail CSV records the applied corrections.", col = "#666666", cex = .86)
  txt(.05, .07, "Concepts: nat = pretax; pod = disposable. Exact cases and correction reasons: details.csv.", col = "#666666", cex = .86)
  # One country-panel comparison per affected concept and income group. Identical
  # regional trajectories are represented once, never averaged for presentation.
  regions <- unique(details$country[details$phase == "regional"])
  titles <- dina_consistency_headline_names()
  concepts <- c(nat = "Pretax", pod = "Disposable", pon = "Post-tax national")
  selections <- split(review$material, paste(review$material$step, review$material$p, sep = "/"))
  for (selection in selections) {
    step <- selection$step[1]; p <- selection$p[1]
    series <- lapply(sort(unique(selection$country)), function(country) {
      cases <- audit$expected[audit$expected$country == country & audit$expected$step == step, ]
      cases <- cases[order(cases$year), ]
      data.frame(year = cases$year, before = unname(vapply(cases$key, function(k) snapshots[[k]]$headline_before[[p]], numeric(1))),
        after = unname(vapply(cases$key, function(k) snapshots[[k]]$headline_after[[p]], numeric(1))))
    })
    names(series) <- sort(unique(selection$country))
    panels <- series[setdiff(names(series), regions)]
    regional <- list(); members <- list()
    for (country in intersect(names(series), regions)) {
      same <- which(vapply(regional, function(z) isTRUE(all.equal(z, series[[country]], tolerance = 128 * .Machine$double.eps)), logical(1)))
      if (length(same)) members[[same[1]]] <- c(members[[same[1]]], country)
      else {regional[[length(regional)+1L]] <- series[[country]]; members[[length(members)+1L]] <- country}
    }
    if (length(regional)) panels$Regional <- regional
    for (difference in c(FALSE, TRUE)) {
      nc <- min(3L, length(panels)); nr <- ceiling(length(panels) / nc)
      graphics::par(mfrow = c(nr, nc), mar = c(3.8, 4.5, 2.3, 1), oma = c(1, 0, 3, 0), mgp = c(2.7, .8, 0))
      for (label in names(panels)) {
        z <- if (label == "Regional") panels[[label]] else list(panels[[label]])
        values <- unlist(lapply(z, function(q) if (difference) dina_consistency_share_change(q$before, q$after) * 100 else c(q$before, q$after) * 100))
        ylim <- range(values[is.finite(values)]); if (!all(is.finite(ylim))) ylim <- c(0, 1)
        if (diff(ylim) == 0) ylim <- ylim + c(-.1, .1)
        xlim <- range(unlist(lapply(z, `[[`, "year"))); if (diff(xlim) == 0) xlim <- xlim + c(-.5, .5)
        graphics::plot(NA, xlim = xlim, ylim = ylim, xlab = "Year", main = label,
          ylab = if (difference) "Change (percentage points)" else "Income share (%)")
        for (j in seq_along(z)) {
          q <- z[[j]]
          if (difference) {graphics::abline(h = 0, col = "#bbbbbb", lty = 3); graphics::lines(q$year, (q$after-q$before)*100, col = colors[["After"]], lty = j, lwd = 1.5)}
          else {graphics::lines(q$year, q$before*100, col = colors[["Before"]], lty = j, lwd = 1.5); graphics::lines(q$year, q$after*100, col = colors[["After"]], lty = j, lwd = 1.5)}
        }
        if (!difference) graphics::legend("topleft", names(colors), col = colors, lty = 1, lwd = 1.5, bty = "n", cex = .7)
        if (label == "Regional") graphics::legend("bottomleft", vapply(members, function(m) if (length(m) == 1L) m else paste(length(m), "countries"), character(1)), lty = seq_along(z), bty = "n", cex = .7)
      }
      if (length(panels) < nr*nc) for (i in seq_len(nr*nc-length(panels))) graphics::plot.new()
      graphics::mtext(paste(concepts[[step]], titles[[p]], if (difference) "- change" else ""), outer = TRUE, side = 3, line = 1, cex = 1.1)
    }
  }
  invisible(list(threshold_percent = threshold,
    plotted_cases = length(unique(dina_consistency_key(review$material))),
    unresolved_cases = nrow(review$cases), substantial_or_unmeasured_cases = nrow(review$important_cases)))
}

dina_consistency_public_path <- function(path) {
  original <- path
  path <- normalizePath(path, mustWork = FALSE)
  stage <- Sys.getenv("DINA_OUTPUT_ROOT", unset = "")
  if (nzchar(stage)) {
    for (prefix in paste0(unique(c(stage, normalizePath(stage, mustWork = FALSE))), "/"))
      for (candidate in unique(c(original, path))) if (startsWith(candidate, prefix)) {
        relative <- substring(candidate, nchar(prefix) + 1L)
        if (startsWith(relative, "output/")) return(relative)
      }
  }
  root <- Sys.getenv("DINA_PROJECT_ROOT", unset = getwd())
  prefix <- paste0(normalizePath(root, mustWork = FALSE), "/")
  if (startsWith(path, prefix)) {
    relative <- substring(path, nchar(prefix) + 1L)
    if (startsWith(relative, "output/")) return(relative)
  }
  path
}

dina_consistency_write_report <- function(audit, states, report_dir, figure, file) {
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  stage_details <- lapply(states, `[[`, "details")
  details <- dina_consistency_bind(c(stage_details, list(audit$details)))
  primitive <- details[details$record_type == "correction", , drop = FALSE]
  headlines <- details[details$record_type == "headline", , drop = FALSE]
  inherited <- headlines[headlines$phase == "regional", , drop = FALSE]
  unresolved <- details[details$status == "unresolved", , drop = FALSE]
  warning <- details[details$status == "review", , drop = FALSE]
  baseline <- list()
  for (phase in c("assembled", "sources", "regional")) if (phase %in% names(states))
    for (key in names(states[[phase]]$snapshots)) baseline[[key]] <- states[[phase]]$snapshots[[key]]
  baseline <- baseline[intersect(names(baseline), audit$expected$key)]
  get_sum <- function(field) sum(vapply(audit$groups, function(g) g[[field]], numeric(1)))
  max_or_zero <- function(x) if (any(is.finite(x))) max(x[is.finite(x)]) else 0
  counts <- list(distributions = nrow(audit$expected),
    corrected_distributions = length(unique(dina_consistency_key(primitive))),
    inherited_distributions = length(unique(dina_consistency_key(inherited))),
    mean_corrections = sum(primitive$field == "mean"), threshold_corrections = sum(primitive$field == "threshold"),
    repeated_averages_before = if (length(baseline)) sum(vapply(baseline, function(g) dina_consistency_ties(g$avg_before), numeric(1))) else NA_integer_,
    repeated_thresholds_before = if (length(baseline)) sum(vapply(baseline, function(g) dina_consistency_ties(g$thr_before), numeric(1))) else NA_integer_,
    repeated_averages_after = get_sum("repeated_averages"), repeated_thresholds_after = get_sum("repeated_thresholds"),
    missing_averages = get_sum("missing_averages"), missing_thresholds = get_sum("missing_thresholds"),
    unresolved = nrow(unresolved), unresolved_cases = length(unique(dina_consistency_key(unresolved))),
    large_share_changes = nrow(warning),
    max_mean_change_percent = max_or_zero(abs(primitive$relative_difference[primitive$field == "mean"])) * 100,
    max_threshold_change_percent = max_or_zero(abs(primitive$relative_difference[primitive$field == "threshold"])) * 100,
    max_threshold_change_normalized = max_or_zero(primitive$absolute_difference[primitive$field == "threshold"]),
    threshold_corrections_near_zero = sum(primitive$field == "threshold" &
      abs(primitive$before) <= sqrt(.Machine$double.eps) * pmax(abs(primitive$after), 1), na.rm = TRUE),
    max_headline_change_pp = max_or_zero(abs(headlines$difference_pp)))
  status <- if (nrow(unresolved) || nrow(warning)) "review_needed" else "clean"
  detail_path <- file.path(report_dir, "details.csv"); summary_path <- file.path(report_dir, "summary.txt")
  utils::write.csv(details, detail_path, row.names = FALSE, na = "")
  review <- dina_consistency_plot(figure, audit, states, details)
  f <- function(x) if (length(x) && is.finite(x)) format(x, scientific = FALSE, trim = TRUE, digits = 8) else "not recorded"
  summary <- c("CONSISTENCY CHECKER", paste("Export:", dina_consistency_public_path(file)),
    paste("Status:", if (status == "clean") "completed, no unresolved issues" else "completed, review needed"),
    paste("Coverage:", counts$distributions, "country-year-concept distributions;", audit$scope_source),
    paste("Review PDF:", review$threshold_percent, "% relative headline-share filter;", review$plotted_cases, "cases plotted."), "",
    sprintf("Repeated positive averages: %s before; %s after", f(counts$repeated_averages_before), f(counts$repeated_averages_after)),
    sprintf("Repeated positive thresholds: %s before; %s after", f(counts$repeated_thresholds_before), f(counts$repeated_thresholds_after)),
    sprintf("Local corrections: %s distributions; %s means; %s thresholds", counts$corrected_distributions, counts$mean_corrections, counts$threshold_corrections),
    paste("Regional distributions with inherited headline changes:", counts$inherited_distributions),
    sprintf("Missing bracket values/records: %s averages; %s thresholds", counts$missing_averages, counts$missing_thresholds),
    sprintf("Largest mean / threshold adjustment: %s%% / %s%%", f(counts$max_mean_change_percent), f(counts$max_threshold_change_percent)),
    paste("Largest absolute threshold adjustment:", f(counts$max_threshold_change_normalized), "in units of overall income"),
    paste("Largest headline-share change:", f(counts$max_headline_change_pp), "percentage points"),
    paste("Share changes above 0.01 percentage points:", counts$large_share_changes),
    paste("Unresolved findings:", counts$unresolved, "in", counts$unresolved_cases, "cases"))
  if (counts$threshold_corrections_near_zero > 0)
    summary <- c(summary, paste("Threshold corrections from near zero:", counts$threshold_corrections_near_zero,
      "(relative percentages have tiny denominators; consult absolute changes)."))
  if (nrow(unresolved)) {
    grouped <- split(unresolved, paste(unresolved$country, unresolved$step, unresolved$rule, sep = "/"))
    lines <- vapply(grouped, function(d) {
      years <- sort(unique(d$year[is.finite(d$year)]))
      span <- if (!length(years)) "" else if (length(years) == 1L) as.character(years) else
        paste0(min(years), "–", max(years), " (", length(years), " years)")
      paste(d$country[1L], d$step[1L], span, d$rule[1L], sep = " | ")
    }, character(1))
    summary <- c(summary, "", "Unresolved cases (country | concept | years | finding):", lines)
  }
  changed_cases <- dina_consistency_bind(list(primitive, inherited))
  if (nrow(changed_cases)) {
    grouped <- split(changed_cases, changed_cases$country)
    lines <- vapply(grouped, function(d) {
      years <- sort(unique(d$year))
      span <- if (length(years) == 1L) as.character(years) else
        paste0(min(years), "–", max(years), " (", length(years), " years)")
      paste(d$country[1L], paste(sort(unique(d$step)), collapse = ","), span,
        paste(length(unique(dina_consistency_key(d))), "distributions"), sep = " | ")
    }, character(1))
    summary <- c(summary, "", "Local or inherited changes (country | concepts | years | count):", lines)
  }
  summary <- c(summary, "", "Corrections preserve population and total income. Zero-income groups are retained.",
    "Source citations are unchanged; these corrections are our export-consistency processing.",
    "Unavailable monetary benchmarks are not invented or carried forward.",
    paste("Details:", dina_consistency_public_path(detail_path)),
    paste("Review figures:", dina_consistency_public_path(figure)))
  writeLines(summary, summary_path, useBytes = TRUE)
  receipt <- list(version = 1L, status = status, review = review, generated_at = format(Sys.time(), "%FT%T%z"),
    export = dina_consistency_public_path(file), scope_source = audit$scope_source, counts = counts,
    reports = list(summary = dina_consistency_public_path(summary_path), details = dina_consistency_public_path(detail_path),
      figure = dina_consistency_public_path(figure)),
    expected_cases = lapply(seq_len(nrow(audit$expected)), function(i) as.list(audit$expected[i, c("country", "year", "step")])) )
  jsonlite::write_json(receipt, file.path(report_dir, "receipt.json"), pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  cat(if (status == "clean") "Consistency checker completed, no unresolved issues." else
    paste("Consistency checker completed, review needed:", counts$unresolved, "unresolved findings."), "\n")
  cat("Report:", dina_consistency_public_path(summary_path), "\nFigures:", dina_consistency_public_path(figure), "\n")
  invisible(receipt)
}

dina_consistency_audit_file <- function(file, work_dir = NULL, report_dir, figure, expected = NULL, wide = NULL) {
  states <- dina_consistency_read_states(work_dir)
  if (is.null(expected) && !is.null(work_dir)) {
    scope <- file.path(work_dir, "publication_scope.csv")
    if (file.exists(scope)) expected <- scope
  }
  columns <- c("country", "year", "p", "widcode", "value")
  data <- haven::read_dta(file, col_select = tidyselect::any_of(columns))
  wide_data <- if (is.character(wide) && length(wide) == 1L) as.data.frame(haven::read_dta(wide,
    col_select = tidyselect::any_of(c("country", "year", "step", "p", "bracketavg", "thr", "s", "anninc992i", "income_mean")))) else wide
  audit <- dina_consistency_audit(data, expected, states, wide_data)
  dina_consistency_write_report(audit, states, report_dir, figure, file)
}

dina_consistency_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) == 5L && args[1L] == "prepare")
    return(dina_consistency_prepare_file(args[2L], args[3L], args[4L], args[5L]))
  if (length(args) %in% c(5L, 6L) && args[1L] == "audit")
    return(dina_consistency_audit_file(args[2L], args[3L], args[4L], args[5L],
      wide = if (length(args) == 6L) args[6L] else NULL))
  stop("Usage: consistency_checker.R prepare INPUT OUTPUT WORK_DIR sources|assembled\n       consistency_checker.R audit EXPORT WORK_DIR REPORT_DIR FIGURE [WIDE]", call. = FALSE)
}

if (sys.nframe() == 0L) dina_consistency_main()
