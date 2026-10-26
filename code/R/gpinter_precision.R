# Precision handling for generated statistics. This does not alter income means,
# population boundaries, tail multipliers, or the established interpolation grid.
gpinter_precision_empty_adjustments <- function() {
  data.frame(p = double(), threshold_before = double(),
                            threshold_after = double(), bracket_mean = double(),
                            difference = double(), precision_envelope = double(),
                            anchor_p = double(), rule = character())
}

gpinter_precision_prepare <- function(d, tol, margin, collapse, sanitize) {
  d <- d[order(d$p), , drop = FALSE]
  adjustments <- gpinter_precision_empty_adjustments()
  fail <- function(message) {
    stop(structure(list(message = message, call = NULL,
                        precision_adjustments = adjustments),
                   class = c("gpinter_precision_error", "error", "condition")))
  }
  if (anyDuplicated(d$p) || any(!is.finite(d$p)) ||
      any(diff(d$p) <= 0) || any(d$p < 0 | d$p >= 1) ||
      any(!is.finite(d$avg)) || any(!is.finite(d$thr))) {
    fail("Invalid percentile, mean, or threshold before GPinter cleaning.")
  }
  width <- diff(c(d$p, 1))
  envelope <- pmax(0.005, 2^-23 * pmax(abs(d$avg), abs(d$thr), 1))
  # Zero-income ties at the bottom retain the existing tie-collapse treatment.
  needs_margin <- d$avg <= d$thr + margin(d$thr) & !(d$avg == 0 & d$thr == 0)
  if (any(needs_margin & d$thr - d$avg > envelope)) {
    i <- which(needs_margin & d$thr - d$avg > envelope)[[1L]]
    fail(sprintf("Substantive mean/threshold inconsistency at p=%.8g: mean %.12g, threshold %.12g, precision envelope %.12g.",
                 d$p[i], d$avg[i], d$thr[i], envelope[i]))
  }
  original <- d
  anchors <- data.frame(p = double(), tail_income = double())
  # Keep equal-threshold bins together. Moving their common boundary preserves
  # the first population anchor; later tied boundaries may still be collapsed.
  for (threshold in unique(original$thr[needs_margin])) {
    indices <- which(original$thr == threshold)
    affected <- indices[needs_margin[indices]]
    target <- min(original$avg[affected] - 2 * margin(original$thr[affected]))
    if (!is.finite(target) || target < 0) {
      fail(sprintf("No feasible precision threshold at p=%.8g without changing income means.",
                   original$p[min(indices)]))
    }
    anchor <- min(original$p[indices])
    d$thr[indices] <- target
    adjustments <- rbind(adjustments, data.frame(
      p = original$p[indices], threshold_before = original$thr[indices],
      threshold_after = target, bracket_mean = original$avg[indices],
      difference = target - original$thr[indices], precision_envelope = envelope[indices],
      anchor_p = anchor, rule = "storage_precision_threshold_margin"))
    anchors <- rbind(anchors, data.frame(p = anchor,
      tail_income = sum(width[original$p >= anchor] * original$avg[original$p >= anchor])))
  }
  # The threshold-only repair must never change a mean or population boundary.
  if (!identical(d$p, original$p) || !identical(d$avg, original$avg)) {
    fail("Precision repair unexpectedly changed an income mean or population boundary.")
  }
  cleaned <- sanitize(collapse(d))
  if (nrow(anchors)) {
    next_threshold <- c(cleaned$thr[-1L], Inf)
    if (any(!is.finite(cleaned$thr)) || any(diff(cleaned$thr) <= 0) ||
        any(!is.finite(cleaned$avg)) || any(cleaned$avg <= cleaned$thr) ||
        any(cleaned$avg >= next_threshold)) {
      fail("Precision thresholds are infeasible after tie handling.")
    }
    cleaned_width <- diff(c(cleaned$p, 1))
    original_income <- sum(width * original$avg)
    cleaned_income <- sum(cleaned_width * cleaned$avg)
    if (abs(cleaned_income - original_income) > 1e-10 * max(1, abs(original_income))) {
      fail("Precision handling would change total income during tie handling; review the thresholds instead.")
    }
    # Tie collapse may combine original bins, but the sanitizer must not alter
    # their income to make a moved boundary feasible. Check each resulting bin
    # as well as the total, so offsetting changes cannot cancel in the check.
    for (i in seq_len(nrow(cleaned))) {
      upper_p <- if (i < nrow(cleaned)) cleaned$p[i + 1L] else 1
      rows <- original$p >= cleaned$p[i] & original$p < upper_p
      expected_income <- sum(width[rows] * original$avg[rows])
      actual_income <- cleaned_width[i] * cleaned$avg[i]
      if (abs(actual_income - expected_income) > 1e-10 * max(1, abs(expected_income))) {
        fail(sprintf("Precision handling would change bracket income at p=%.8g; review the thresholds instead.",
                     cleaned$p[i]))
      }
    }
    for (i in seq_len(nrow(anchors))) {
      anchor <- anchors$p[i]
      if (!any(abs(cleaned$p - anchor) < 1e-12)) {
        fail(sprintf("Precision handling would remove the population anchor p=%.8g.", anchor))
      }
      income <- sum(cleaned_width[cleaned$p >= anchor] * cleaned$avg[cleaned$p >= anchor])
      if (abs(income - anchors$tail_income[i]) > 1e-10 * max(1, abs(anchors$tail_income[i]))) {
        fail(sprintf("Precision handling would change tail income above p=%.8g; review the thresholds instead.", anchor))
      }
    }
  }
  cleaned$average <- rep(stats::weighted.mean(cleaned$avg, diff(c(cleaned$p, 1))), nrow(cleaned))
  list(data = cleaned, adjustments = adjustments, anchors = anchors)
}

gpinter_precision_validate_fit <- function(tab, prepared) {
  if (!nrow(prepared$anchors)) return(invisible(TRUE))
  for (i in seq_len(nrow(prepared$anchors))) {
    anchor <- prepared$anchors$p[i]
    row <- which(abs(tab$fractile - anchor) < 1e-12)
    expected <- prepared$anchors$tail_income[i] / prepared$data$average[1L]
    if (length(row) != 1L || !is.finite(tab$top_share[row]) ||
        abs(tab$top_share[row] - expected) > 1e-8) {
      stop(structure(list(message = sprintf("GPinter did not preserve the repaired tail anchor p=%.8g.", anchor),
                          call = NULL, precision_adjustments = prepared$adjustments),
                     class = c("gpinter_precision_error", "error", "condition")))
    }
  }
  invisible(TRUE)
}
