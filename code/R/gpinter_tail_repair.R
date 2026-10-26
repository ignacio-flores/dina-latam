# Explicit fallback for the approved top-tail adjustment. It does not fit a
# distribution, change the tail multipliers, or alter the bottom-90 income bins.
gpinter_tail_empty_adjustments <- function() {
  data.frame(p = double(), field = character(), before = double(), after = double(),
    absolute_difference = double(), relative_difference = double(), difference_pp = double(),
    rule = character(), reason = character(), lower_bound = double(), upper_bound = double())
}

gpinter_tail_stop <- function(message, adjustments = gpinter_tail_empty_adjustments()) {
  stop(structure(list(message = message, call = NULL, tail_adjustments = adjustments),
    class = c("gpinter_tail_error", "error", "condition")))
}

gpinter_tail_repair <- function(d, relative_gap = 1e-10) {
  needed <- c("p", "avg", "thr")
  if (!all(needed %in% names(d)) || !nrow(d) ||
      !all(vapply(d[needed], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(d[needed]))) || d$p[1L] != 0 ||
      any(d$p < 0 | d$p >= 1) || any(diff(d$p) <= 0) || any(d$avg < 0)) {
    gpinter_tail_stop("Tail repair requires a complete ordered population grid and finite nonnegative bracket means.")
  }
  if (length(relative_gap) != 1L || !is.finite(relative_gap) ||
      relative_gap <= 0 || relative_gap >= .01) {
    gpinter_tail_stop("Tail repair requires a small positive relative interior margin.")
  }
  original <- d
  substantive_reversals <- function(avg) {
    envelope <- 128 * .Machine$double.eps * pmax(1, abs(head(avg, -1L)), abs(tail(avg, -1L)))
    which(diff(avg) < -envelope)
  }
  width <- diff(c(d$p, 1))
  income <- width * d$avg
  overall <- sum(income)
  if (!is.finite(overall) || overall <= 0) gpinter_tail_stop("Tail repair requires positive total income.")
  cutoffs <- c(.9, .99, .999, .9999)
  positions <- vapply(cutoffs, function(p) {
    found <- which(abs(d$p - p) < 1e-12)
    if (length(found) != 1L) return(NA_integer_)
    found
  }, integer(1))
  if (anyNA(positions) || positions[4L] != nrow(d) ||
      positions[3L] + 1L != positions[4L] || positions[2L] + 1L != positions[3L]) {
    gpinter_tail_stop("Tail repair requires the existing p90, p99, p99.9 and p99.99 fitting grid.")
  }
  tail_income <- rev(cumsum(rev(income)))
  targets <- tail_income[positions] / overall
  adjustments <- gpinter_tail_empty_adjustments()
  add <- function(p, field, before, after, rule, reason, lower = NA_real_, upper = NA_real_) {
    delta <- after - before
    adjustments <<- rbind(adjustments, data.frame(p = p, field = field, before = before,
      after = after, absolute_difference = abs(delta),
      relative_difference = if (is.na(delta) || before == 0) NA_real_ else delta / abs(before),
      difference_pp = if (field == "top_share") 100 * delta else NA_real_,
      rule = rule, reason = reason, lower_bound = lower, upper_bound = upper))
  }

  # The bounds are 0.1*S(top .1%) and 1.1*S(top .1%)-0.1*S(top 1%).
  # Use actual grid widths to avoid creating reversals through decimal rounding.
  a <- positions[2L]; b <- positions[3L]; z <- positions[4L]
  lower <- targets[3L] * width[z] / (width[b] + width[z])
  upper <- targets[3L] - width[b] / width[a] * (targets[2L] - targets[3L])
  target <- targets[4L]
  substantive_tail <- length(substantive_reversals(d$avg[c(a, b, z)])) > 0L
  tail_means <- d$avg[c(a, b, z)]
  equal_tail_pairs <- abs(diff(tail_means)) <= 128 * .Machine$double.eps *
    pmax(1, abs(head(tail_means, -1L)), abs(tail(tail_means, -1L)))
  # A positive-width feasible interval permits a tiny inward margin at an
  # exact bound. A terminal atom with a zero-width interval is not silently
  # converted to a smooth distribution by this helper.
  if (substantive_tail || any(equal_tail_pairs) || target <= lower || target >= upper) {
    span <- upper - lower
    if (!is.finite(span) || span <= 0 || all(equal_tail_pairs)) {
      gpinter_tail_stop("No strictly feasible top-0.01% share exists without changing the protected top-1% or top-0.1% share.", adjustments)
    }
    projected <- min(max(target, lower), upper)
    if (equal_tail_pairs[2L]) projected <- lower
    if (equal_tail_pairs[1L]) projected <- upper
    if (projected != target && substantive_tail) {
      add(.9999, "top_share", target, projected, "tail_top001_feasibility",
        "Nearest feasible top-0.01% share; top-1% and top-0.1% income are held fixed.", lower, upper)
    }
    gap <- min(relative_gap * max(abs(lower), abs(upper), abs(targets[3L])), span / 4)
    adjusted <- projected
    if (projected <= lower) adjusted <- lower + gap
    if (projected >= upper) adjusted <- upper - gap
    if (!(adjusted > lower && adjusted < upper)) {
      gpinter_tail_stop("The feasible top-0.01% interval is too narrow for a representable interior margin.", adjustments)
    }
    margin_before <- if (substantive_tail) projected else target
    if (adjusted != margin_before) {
      add(.9999, "top_share", margin_before, adjusted, "tail_top001_interior_margin",
        "Numerical inward margin for strictly increasing GPinter tail means; separate from the substantive target correction.", lower, upper)
    }
    target <- adjusted
    d$avg[z] <- overall * target / width[z]
    d$avg[b] <- (tail_income[b] - overall * target) / width[b]
  }

  # Collapse only when an original inversion touches the p90--p99 block.
  # This includes p89--p90 and p98--p99, but never permits altering p<90.
  reversals <- substantive_reversals(original$avg)
  block_conflict <- reversals[original$p[reversals] < .99 - 1e-12 &
    original$p[reversals + 1L] >= .9 - 1e-12]
  if (length(block_conflict)) {
    block <- which(d$p >= .9 - 1e-12 & d$p < .99 - 1e-12)
    combined <- sum(income[block]) / sum(width[block])
    first <- block[1L]
    add(.9, "bracket_mean", original$avg[first], combined, "tail_p90_p99_consolidation",
      "Replace p90--p99 by its population-weighted mean; the before and after means describe different population widths.")
    for (i in block[-1L]) {
      add(original$p[i], "population_boundary", original$p[i], NA_real_,
        "tail_p90_p99_consolidation",
        "Remove this interior fitting boundary; preserve total p90--p99 income and interpolate its detailed allocation.")
    }
    d$avg[first] <- combined
    if (length(block) > 1L) d <- d[-block[-1L], , drop = FALSE]
  }
  remaining <- substantive_reversals(d$avg)
  if (length(remaining)) {
    i <- remaining[1L]
    gpinter_tail_stop(sprintf(
      "Tail repair leaves decreasing means at p=%.8g to p=%.8g; resolving them would require changing another protected income anchor.",
      d$p[i], d$p[i + 1L]), adjustments)
  }
  # Interior equal means and arithmetic-roundoff ties are deliberately left
  # unchanged for the separate exact-atom handling.
  actual_income <- diff(c(d$p, 1)) * d$avg
  actual_overall <- sum(actual_income)
  actual_tail <- rev(cumsum(rev(actual_income))) / actual_overall
  retained_positions <- vapply(cutoffs, function(p) which(abs(d$p - p) < 1e-12), integer(1))
  expected <- c(targets[1:3], target)
  bottom <- original$p < .9 - 1e-12
  if (!identical(d$avg[d$p < .9 - 1e-12], original$avg[bottom]) ||
      abs(actual_overall - overall) > 1e-12 * max(1, overall) ||
      any(abs(actual_tail[retained_positions] - expected) > 1e-12)) {
    gpinter_tail_stop("Tail repair did not preserve the protected population and income anchors.", adjustments)
  }
  d$average <- rep(overall, nrow(d))
  list(data = d, original = original, adjustments = adjustments, applied = nrow(adjustments) > 0L,
    overall = overall, anchors = data.frame(p = d$p, avg = d$avg),
    targets = data.frame(p = cutoffs, top_share = expected))
}

gpinter_tail_validate_tab <- function(tab, prepared, tolerance = 1e-8) {
  # A tabulation on the union of the export grid and retained anchors also
  # supports the exact-atom adapter, without requiring a GPinter fit object.
  fields <- c("fractile", "bracket_average", "top_share", "top_average")
  if (!all(fields %in% names(tab)) || !length(tab$fractile) ||
      any(vapply(tab[fields], length, integer(1)) != length(tab$fractile)) ||
      !all(vapply(tab[fields], function(x) all(is.finite(x)), logical(1))) ||
      tab$fractile[1L] != 0 || any(diff(tab$fractile) <= 0) ||
      any(tab$fractile < 0 | tab$fractile >= 1)) {
    gpinter_tail_stop("Tail validation requires a complete finite ordered tabulation.", prepared$adjustments)
  }
  locate <- function(p) {
    index <- which(abs(tab$fractile - p) < 1e-12)
    if (length(index) != 1L) gpinter_tail_stop("Tail validation is missing a retained population boundary.", prepared$adjustments)
    index
  }
  positions <- vapply(prepared$anchors$p, locate, integer(1))
  cell_income <- diff(c(tab$fractile, 1)) * tab$bracket_average
  ends <- c(positions[-1L] - 1L, length(cell_income))
  means <- vapply(seq_along(positions), function(i) {
    sum(cell_income[seq.int(positions[i], ends[i])]) / diff(c(prepared$anchors$p, 1))[i]
  }, numeric(1))
  bottom <- prepared$original$p < .9 - 1e-12
  bottom_positions <- vapply(prepared$original$p[bottom], function(p) {
    found <- which(abs(prepared$anchors$p - p) < 1e-12)
    if (length(found) != 1L) gpinter_tail_stop("Tail repair discarded a protected bottom-90 boundary.", prepared$adjustments)
    found
  }, integer(1))
  if (any(abs(means - prepared$anchors$avg) > tolerance * pmax(1, abs(prepared$anchors$avg))) ||
      any(abs(means[bottom_positions] - prepared$original$avg[bottom]) >
        tolerance * pmax(1, abs(prepared$original$avg[bottom]))) ||
      abs(sum(cell_income) - prepared$overall) > tolerance * max(1, prepared$overall) ||
      abs(tab$top_average[1L] - prepared$overall) > tolerance * max(1, prepared$overall)) {
    gpinter_tail_stop("GPinter did not preserve the repaired tail's retained bracket-income anchors.", prepared$adjustments)
  }
  positions <- vapply(prepared$targets$p, locate, integer(1))
  if (any(abs(tab$top_share[positions] - prepared$targets$top_share) > tolerance)) {
    gpinter_tail_stop("GPinter did not preserve the protected broad tail shares and reviewed top-0.01% target.", prepared$adjustments)
  }
  if (exists("gpinter_threshold_check_tab", mode = "function")) {
    gpinter_threshold_check_tab(tab, prepared$overall, "Tail-repaired export grid")
  }
  invisible(TRUE)
}

gpinter_tail_validate_fit <- function(dist, tab, prepared, tolerance = 1e-8) {
  grid <- sort(unique(c(tab$fractile, prepared$anchors$p)))
  fitted <- gpinter::generate_tabulation(dist, fractiles = grid,
    threshold = TRUE, bracketavg = TRUE, bracketshare = TRUE, topavg = TRUE, topshare = TRUE)
  gpinter_tail_validate_tab(fitted, prepared, tolerance)
}
