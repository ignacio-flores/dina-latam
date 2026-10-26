# Preserve the approved income adjustments while reconstructing incompatible
# thresholds. gpinter_precision.R must be loaded before this helper.
gpinter_threshold_stop <- function(message, adjustments = gpinter_precision_empty_adjustments()) {
  stop(structure(list(message = message, call = NULL, precision_adjustments = adjustments),
                 class = c("gpinter_threshold_error", "error", "condition")))
}

gpinter_threshold_conserves <- function(cleaned, original) {
  if (!nrow(cleaned) || cleaned$p[1L] != 0 || any(diff(cleaned$p) <= 0) ||
      any(!is.finite(cleaned$avg))) return(FALSE)
  widths <- diff(c(original$p, 1))
  for (i in seq_len(nrow(cleaned))) {
    upper <- if (i < nrow(cleaned)) cleaned$p[i + 1L] else 1
    if (!any(abs(original$p - cleaned$p[i]) < 1e-12)) return(FALSE)
    rows <- original$p >= cleaned$p[i] & original$p < upper
    # Existing consolidation is allowed only at exactly tied thresholds.
    # Combining bins with distinct thresholds would discard an income anchor
    # even when the resulting larger bin preserves aggregate income.
    if (sum(rows) > 1L && length(unique(original$thr[rows])) != 1L) return(FALSE)
    expected <- sum(widths[rows] * original$avg[rows])
    actual <- (upper - cleaned$p[i]) * cleaned$avg[i]
    if (abs(actual - expected) > 1e-10 * max(1, abs(expected))) return(FALSE)
  }
  TRUE
}

gpinter_threshold_prepare <- function(d, tol, margin, collapse, sanitize) {
  needed <- c("p", "avg", "thr")
  if (!all(needed %in% names(d)) || !nrow(d) ||
      !all(vapply(d[needed], is.numeric, logical(1))) ||
      any(!is.finite(as.matrix(d[needed])))) {
    gpinter_threshold_stop("GPinter requires finite percentile boundaries, bracket means, and thresholds.")
  }
  if (d$p[1L] != 0 || any(d$p < 0 | d$p >= 1) || any(diff(d$p) <= 0)) {
    gpinter_threshold_stop("GPinter requires a complete, strictly increasing population grid beginning at p=0; boundaries are not reordered.")
  }
  if (any(d$avg < 0)) gpinter_threshold_stop("Negative bracket means require review; income means are not changed.")
  original <- d
  widths <- diff(c(original$p, 1))
  overall <- sum(widths * original$avg)
  if (!is.finite(overall) || overall <= 0) gpinter_threshold_stop("GPinter requires a positive finite population-weighted overall mean.")
  original$average <- rep(overall, nrow(original))
  candidate <- tryCatch(gpinter_precision_prepare(original, tol, margin, collapse, sanitize),
                        error = identity)
  reversal <- which(diff(original$avg) < 0)
  # Preserve the already approved precision path for tiny reversed means in
  # exactly tied threshold bins. This invokes existing consolidation only;
  # shares_fit never pools or reorders original means.
  precision_ties <- !length(reversal) || (!inherits(candidate, "error") &&
    nrow(candidate$adjustments) > 0 && all(vapply(reversal, function(i) {
      envelope <- max(.005, 2^-23 * max(abs(original$avg[c(i, i + 1L)]),
                                       abs(original$thr[c(i, i + 1L)]), 1))
      original$thr[i] == original$thr[i + 1L] &&
        original$avg[i] - original$avg[i + 1L] <= envelope
    }, logical(1))))
  if (!inherits(candidate, "error") && precision_ties &&
      gpinter_threshold_conserves(candidate$data, original)) {
    candidate$method <- "tabulation_fit"
    candidate$original <- original
    candidate$reason <- "Existing threshold preparation preserves bracket income."
    return(candidate)
  }
  if (length(reversal)) {
    i <- reversal[1L]
    gpinter_threshold_stop(sprintf(
      "Decreasing bracket means at p=%.8g to p=%.8g (%.12g to %.12g); threshold reconstruction cannot fix income ordering without changing means.",
      original$p[i], original$p[i + 1L], original$avg[i], original$avg[i + 1L]))
  }
  fitting <- original
  leading_zeros <- sum(original$avg == 0)
  if (leading_zeros > 1L) {
    # shares_fit requires strictly increasing means. Consecutive leading
    # zero-income bins describe one atom at zero, with no income allocation
    # to alter. Retain the original boundaries for validation and reporting.
    fitting <- original[c(1L, seq.int(leading_zeros + 1L, nrow(original))), , drop = FALSE]
  }
  if (any(diff(fitting$avg) == 0)) {
    gpinter_threshold_stop("GPinter shares_fit requires strictly increasing original bracket means; tied means require review without automatic pooling.")
  }
  if (nrow(fitting) < 3L) gpinter_threshold_stop("GPinter shares_fit requires at least three fitting population boundaries after representing any leading zero-income atom.")
  reason <- if (inherits(candidate, "error")) conditionMessage(candidate) else
    "Legacy threshold cleaning would change bracket income or discard an original population boundary."
  if (leading_zeros > 1L) reason <- paste(reason,
    "Leading exactly zero-income bins are represented as one zero-income atom for fitting; all original population boundaries are retained for validation.")
  list(data = fitting, adjustments = gpinter_precision_empty_adjustments(),
       anchors = data.frame(p = original$p, tail_income = rev(cumsum(rev(widths * original$avg)))),
       method = "shares_fit", original = original, reason = reason)
}

gpinter_threshold_check_tab <- function(tab, overall, label) {
  fields <- c("fractile", "threshold", "bracket_average", "bracket_share", "top_average", "top_share")
  if (!all(fields %in% names(tab)) || !length(tab$fractile) ||
      any(vapply(tab[fields], length, integer(1)) != length(tab$fractile)) ||
      !all(vapply(tab[fields], function(x) all(is.finite(x)), logical(1)))) {
    gpinter_threshold_stop(paste(label, "contains missing or non-finite fitted statistics."))
  }
  p <- tab$fractile
  if (p[1L] != 0 || any(p < 0 | p >= 1) || any(diff(p) <= 0)) {
    gpinter_threshold_stop(paste(label, "has invalid population ordering or coverage."))
  }
  mean_tol <- 1e-8 * pmax(1, abs(tab$bracket_average))
  threshold_tol <- 1e-8 * pmax(1, abs(tab$threshold))
  upper <- c(tab$threshold[-1L], Inf)
  if (any(tab$threshold < -threshold_tol) ||
      any(diff(tab$threshold) < -pmax(threshold_tol[-1L], threshold_tol[-length(p)])) ||
      any(diff(tab$bracket_average) < -pmax(mean_tol[-1L], mean_tol[-length(p)])) ||
      any(tab$bracket_average < tab$threshold - mean_tol) ||
      any(tab$bracket_average > upper + mean_tol) || any(tab$bracket_share < -1e-8)) {
    gpinter_threshold_stop(paste(label, "violates fitted distribution ordering or threshold bounds."))
  }
  shares <- tab$bracket_average * diff(c(p, 1)) / overall
  top <- rev(cumsum(rev(shares)))
  if (abs(sum(shares) - 1) > 1e-8 || max(abs(tab$bracket_share - shares)) > 1e-8 ||
      max(abs(tab$top_share - top)) > 1e-8 ||
      any(abs(tab$top_average - top * overall / (1 - p)) >
          1e-8 * pmax(1, abs(tab$top_average)))) {
    gpinter_threshold_stop(paste(label, "does not preserve the overall mean or consistent bracket and cumulative income."))
  }
  invisible(TRUE)
}

# Keep a known zero-income mass explicit when fitting the positive distribution.
gpinter_threshold_fit_shares <- function(d) {
  if (!all(c("p", "avg") %in% names(d)) || !nrow(d) ||
      any(!is.finite(d$p)) || any(!is.finite(d$avg)) ||
      d$p[1L] != 0 || any(d$p < 0 | d$p >= 1) ||
      any(diff(d$p) <= 0) || any(d$avg < 0)) {
    gpinter_threshold_stop("Shares fitting requires a complete ordered population grid and finite nonnegative bracket means.")
  }
  overall <- sum(diff(c(d$p, 1)) * d$avg)
  first_positive <- which(d$avg > 0)[1L]
  if (is.na(first_positive) || !is.finite(overall) || overall <= 0)
    gpinter_threshold_stop("Shares fitting requires positive total income.")
  fitting <- d[seq.int(first_positive, nrow(d)), , drop = FALSE]
  if (any(fitting$avg <= 0))
    gpinter_threshold_stop("Zero-income rows must form a leading population mass before shares fitting.")
  has_zero_mass <- first_positive > 1L
  bottom_model <- if (has_zero_mass) "dirac" else NULL
  notes <- if (has_zero_mass)
    "Represent the original leading zero-income population as an explicit Dirac mass; retain all positive-income population boundaries and bracket incomes." else character()
  # Keeping an artificial p=0 zero row can let the package's spline and bottom
  # extrapolation put positive/negative incomes inside that known zero mass.
  # Fit the positive rows at their original p values and declare the mass.
  dist <- gpinter::shares_fit(p = fitting$p, bracketavg = fitting$avg,
    average = overall, first_threshold = 0, bottom_model = bottom_model)
  thresholds <- gpinter::fitted_quantile(dist, fitting$p)
  if (any(!is.finite(thresholds)))
    gpinter_threshold_stop("Shares fitting produced non-finite retained thresholds.")
  if (thresholds[1L] < 0) {
    # The package's generalized-inverse spline solve can miss its requested
    # zero lower boundary. Refit that constraint with exactly the same means;
    # clipping the exported threshold alone would leave the distribution wrong.
    thresholds[1L] <- 0
    dist <- gpinter::tabulation_fit(p = fitting$p, threshold = thresholds,
      bracketavg = fitting$avg, average = overall, bottom_model = bottom_model)
    notes <- c(notes,
      "Refit the requested zero lower boundary after shares_fit returned a negative first positive-income threshold; preserve all bracket incomes.")
  }
  attr(dist, "dina_fit_notes") <- notes
  dist
}

# GPinter evaluates cumulative income numerically, so subtraction can create
# tiny positive/negative means inside a known zero-income population mass.
# Restore only that exact identity; positive-income bins are left untouched.
gpinter_threshold_zero_atom <- function(tab, original) {
  if (!nrow(original) || original$p[1L] != 0 || original$avg[1L] != 0) return(tab)
  first_positive <- which(original$avg != 0)[1L]
  if (is.na(first_positive)) return(tab)
  end <- original$p[first_positive]
  fields <- c("fractile", "threshold", "bracket_average", "bracket_share", "top_average", "top_share")
  if (!all(fields %in% names(tab)) || any(!is.finite(unlist(tab[fields]))))
    gpinter_threshold_stop("The zero-income atom identity cannot repair missing or non-finite fitted statistics.")
  overall <- sum(diff(c(original$p, 1)) * original$avg)
  p <- tab$fractile
  widths <- diff(c(p, 1))
  through_end <- p <= end + 1e-12
  inside <- p < end - 1e-12
  zero_bins <- inside & c(p[-1L], 1) <= end + 1e-12
  expected_top <- overall / (1 - p)
  # Allow accumulation across the fitted population bins (observed errors are
  # below 8e-14 of total income); the distribution checks stay at 1e-8.
  roundoff <- 512 * .Machine$double.eps
  if (any(abs(tab$top_share[through_end] - 1) > roundoff) ||
      any(abs(tab$top_average[through_end] - expected_top[through_end]) >
          roundoff * pmax(1, abs(expected_top[through_end]))) ||
      any(abs(tab$bracket_share[zero_bins]) > roundoff) ||
      any(abs(tab$bracket_average[zero_bins]) > roundoff * max(1, abs(overall)) / widths[zero_bins]))
    gpinter_threshold_stop("The fitted zero-income mass differs substantively from its exact original identity.")
  tab$threshold[inside] <- 0
  tab$bracket_average[zero_bins] <- 0
  tab$bracket_share[zero_bins] <- 0
  tab$top_share[through_end] <- 1
  tab$top_average[through_end] <- expected_top[through_end]
  tab
}

gpinter_threshold_tabulate <- function(dist, p, original) {
  tab <- gpinter::generate_tabulation(dist, fractiles = p,
    threshold = TRUE, bracketavg = TRUE, bracketshare = TRUE, topavg = TRUE, topshare = TRUE)
  overall <- sum(diff(c(original$p, 1)) * original$avg)
  first <- which(original$avg > 0)[1L]
  if (!is.na(first)) {
    upper <- if (first < nrow(original)) original$p[first + 1L] else 1
    widths <- diff(c(p, 1))
    # Subtracting two cumulative shares near one loses precision in a very
    # small first positive bin. Integrate the fitted quantile locally instead.
    small <- which(p >= original$p[first] - 1e-12 & c(p[-1L], 1) <= upper + 1e-12 &
      tab$bracket_share > 0 & tab$bracket_share < 1e-5)
    for (i in small) {
      # GPinter can insert a very narrow interpolation segment near a bin's
      # endpoint. Integrate each actual segment so quadrature cannot miss it.
      end <- p[i] + widths[i]
      cuts <- sort(unique(c(p[i], dist$pk[dist$pk > p[i] & dist$pk < end], end)))
      value <- sum(vapply(seq_len(length(cuts) - 1L), function(j)
        stats::integrate(function(x) gpinter::fitted_quantile(dist, x),
          lower = cuts[j], upper = cuts[j + 1L], rel.tol = 1e-10,
          abs.tol = 1e-12 * max(1, abs(tab$bracket_average[i] * widths[i])),
          subdivisions = 1000L)$value, numeric(1)))
      if (abs(value - tab$bracket_average[i] * widths[i]) >
          128 * .Machine$double.eps * max(1, abs(overall))) {
        gpinter_threshold_stop("Local quantile integration disagrees with fitted cumulative income beyond arithmetic precision.")
      }
      tab$bracket_average[i] <- value / widths[i]
      tab$bracket_share[i] <- value / overall
    }
    if (length(small)) {
      tab$top_share <- rev(cumsum(rev(tab$bracket_share)))
      tab$top_average <- tab$top_share * overall / (1 - p)
    }
  }
  gpinter_threshold_zero_atom(tab, original)
}

gpinter_threshold_validate_fit <- function(dist, tab, prepared) {
  if (identical(prepared$method, "tabulation_fit")) {
    return(gpinter_precision_validate_fit(tab, prepared))
  }
  if (!identical(prepared$method, "shares_fit")) gpinter_threshold_stop("Unknown GPinter threshold preparation method.")
  original <- prepared$original
  overall <- sum(diff(c(original$p, 1)) * original$avg)
  gpinter_threshold_check_tab(tab, overall, "Export grid")
  original_tab <- gpinter_threshold_tabulate(dist, original$p, original)
  gpinter_threshold_check_tab(original_tab, overall, "Original population grid")
  if (length(original_tab$fractile) != nrow(original) ||
      any(abs(original_tab$fractile - original$p) > 1e-12)) {
    gpinter_threshold_stop("GPinter did not preserve every original population boundary.")
  }
  expected_income <- diff(c(original$p, 1)) * original$avg
  actual_income <- diff(c(original$p, 1)) * original_tab$bracket_average
  expected_top <- rev(cumsum(rev(expected_income))) / overall
  if (any(abs(original_tab$bracket_average - original$avg) > 1e-8 * pmax(1, abs(original$avg))) ||
      any(abs(actual_income - expected_income) > 1e-8 * pmax(1, abs(expected_income))) ||
      any(abs(original_tab$top_share - expected_top) > 1e-8) ||
      abs(original_tab$top_average[1L] - overall) > 1e-8 * max(1, abs(overall))) {
    gpinter_threshold_stop("GPinter threshold reconstruction did not preserve original bracket means, income, or population-boundary tail shares.")
  }
  invisible(TRUE)
}

gpinter_threshold_changes <- function(dist, prepared) {
  empty <- data.frame(p = double(), threshold_before = double(), threshold_after = double(),
                      bracket_mean = double(), difference = double(), rule = character(), reason = character())
  if (!identical(prepared$method, "shares_fit")) return(empty)
  original <- prepared$original
  tab <- gpinter::generate_tabulation(dist, fractiles = original$p, threshold = TRUE)
  if (length(tab$fractile) != nrow(original) || any(!is.finite(tab$threshold)) ||
      any(abs(tab$fractile - original$p) > 1e-12)) {
    gpinter_threshold_stop("Cannot report reconstructed thresholds on the original population grid.")
  }
  changed <- tab$threshold != original$thr
  data.frame(p = original$p[changed], threshold_before = original$thr[changed],
    threshold_after = tab$threshold[changed], bracket_mean = original$avg[changed],
    difference = tab$threshold[changed] - original$thr[changed],
    rule = rep("gpinter_threshold_reconstruction", sum(changed)),
    reason = rep(prepared$reason, sum(changed)))
}
