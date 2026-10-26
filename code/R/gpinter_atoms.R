# Exact constant-income population masses around a GPinter continuous fit.
# Equal incomes are never made strictly increasing or spread across a range.
gpinter_atoms_tabulate <- function(d, p_grid, validation_tolerance = 1e-8) {
  fail <- function(message) stop(paste("GPinter atom fit:", message), call. = FALSE)
  if (!all(c("p", "avg", "thr") %in% names(d)) || !nrow(d) ||
      any(!is.finite(as.matrix(d[c("p", "avg", "thr")]))))
    fail("finite original boundaries, means and thresholds are required.")
  if (d$p[1L] != 0 || any(d$p < 0 | d$p >= 1) || any(diff(d$p) <= 0))
    fail("original population boundaries must begin at zero and increase strictly.")
  if (any(d$avg < 0)) fail("negative means require review.")
  widths <- diff(c(d$p, 1))
  original_income <- widths * d$avg
  overall <- sum(original_income)
  if (!is.finite(overall) || overall <= 0) fail("a positive overall mean is required.")
  # Only arithmetic roundoff counts as an equal income here. This is much
  # narrower than the separate float-storage tolerance used for thresholds.
  equality_tol <- 128 * .Machine$double.eps * pmax(1, abs(d$avg[-nrow(d)]), abs(d$avg[-1L]))
  change <- diff(d$avg)
  if (any(change < -equality_tol)) fail("decreasing bracket means require review before atom fitting.")
  same <- abs(change) <= equality_tol
  run <- cumsum(c(TRUE, !same))
  runs <- split(seq_len(nrow(d)), run)
  atom_runs <- runs[vapply(runs, function(i) length(i) > 1L || all(d$avg[i] == 0), logical(1))]
  if (!length(atom_runs)) fail("no constant-income population mass was found.")
  atoms <- do.call(rbind, lapply(atom_runs, function(i) {
    value <- if (length(unique(d$avg[i])) == 1L) d$avg[i[1L]] else
      sum(original_income[i]) / sum(widths[i])
    data.frame(p_start = d$p[min(i)], p_end = d$p[max(i)] + widths[max(i)],
      income = value, population = sum(widths[i]),
      first_row = min(i), last_row = max(i),
      mean_roundoff = max(abs(d$avg[i] - value)))
  }))
  rownames(atoms) <- NULL
  if (any(atoms$p_end >= 1)) fail("a terminal constant-income mass needs a bounded-tail model; it is not smoothed automatically.")
  if (nrow(atoms) > 1L && any(atoms$p_end[-nrow(atoms)] == atoms$p_start[-1L]))
    fail("adjacent masses at different income levels need an explicit income-gap model.")
  atom_rows <- unlist(atom_runs, use.names = FALSE)
  keep <- setdiff(seq_len(nrow(d)), atom_rows)
  if (length(keep) < 3L) fail("at least three continuous brackets must remain for GPinter.")
  continuous_mass <- sum(widths[keep])
  continuous_p <- c(0, head(cumsum(widths[keep]), -1L)) / continuous_mass
  continuous_avg <- d$avg[keep]
  continuous_mean <- sum(widths[keep] * continuous_avg) / continuous_mass
  lower <- if (atoms$p_start[1L] == 0) atoms$income[1L] else 0
  seed <- gpinter::shares_fit(p = continuous_p, bracketavg = continuous_avg,
    average = continuous_mean, first_threshold = lower)
  thresholds <- gpinter::fitted_quantile(seed, continuous_p)
  # Pin each compressed join to its constant income. This makes the GPinter
  # quantile approach the exact atom value from either side of the removed mass.
  for (i in seq_len(nrow(atoms))) {
    after <- match(atoms$last_row[i] + 1L, keep)
    if (is.na(after)) fail("an atom has no following continuous bracket.")
    thresholds[after] <- atoms$income[i]
  }
  if (any(!is.finite(thresholds))) fail("continuous threshold reconstruction was not finite.")
  continuous <- gpinter::tabulation_fit(p = continuous_p, threshold = thresholds,
    bracketavg = continuous_avg, average = continuous_mean)
  evaluate <- function(p) {
    if (!length(p) || p[1L] != 0 || any(!is.finite(p)) || any(p < 0 | p >= 1) || any(diff(p) <= 0))
      fail("tabulation grid must begin at zero and increase strictly below one.")
    if (any(diff(p) < 1e-12)) fail("tabulation grid contains numerically duplicate population boundaries.")
    removed_below <- vapply(p, function(x) sum(pmin(pmax(x - atoms$p_start, 0), atoms$population)), numeric(1))
    compressed_p <- pmin(1, pmax(0, (p - removed_below) / continuous_mass))
    threshold <- gpinter::fitted_quantile(continuous, compressed_p)
    tail_income <- continuous_mass * continuous$average * gpinter::top_share(continuous, compressed_p)
    for (i in seq_len(nrow(atoms))) {
      inside <- p >= atoms$p_start[i] & p < atoms$p_end[i]
      threshold[inside] <- atoms$income[i]
      tail_income <- tail_income + atoms$income[i] *
        pmax(atoms$p_end[i] - pmax(p, atoms$p_start[i]), 0)
    }
    bracket_income <- -diff(c(tail_income, 0))
    # A tiny first positive bin can lose digits when two large tail incomes
    # are subtracted. Evaluate that same fitted quantile locally, retaining
    # exact atom masses, as in gpinter_threshold_tabulate().
    first <- which(d$avg > 0)[1L]
    upper <- if (first < nrow(d)) d$p[first + 1L] else 1
    ends <- c(p[-1L], 1)
    small <- which(p >= d$p[first] - 1e-12 & ends <= upper + 1e-12 &
      bracket_income > 0 & bracket_income / overall < 1e-5)
    for (i in small) {
      end_removed <- sum(pmin(pmax(ends[i] - atoms$p_start, 0), atoms$population))
      end_compressed <- min(1, max(0, (ends[i] - end_removed) / continuous_mass))
      cuts <- sort(unique(c(compressed_p[i], continuous$pk[
        continuous$pk > compressed_p[i] & continuous$pk < end_compressed], end_compressed)))
      value <- if (length(cuts) > 1L) continuous_mass * sum(vapply(
        seq_len(length(cuts) - 1L), function(j) stats::integrate(
          function(x) gpinter::fitted_quantile(continuous, x),
          lower = cuts[j], upper = cuts[j + 1L], rel.tol = 1e-10,
          abs.tol = 1e-12 * max(1, abs(bracket_income[i])) / continuous_mass,
          subdivisions = 1000L)$value, numeric(1))) else 0
      value <- value + sum(atoms$income * pmax(0,
        pmin(ends[i], atoms$p_end) - pmax(p[i], atoms$p_start)))
      if (abs(value - bracket_income[i]) >
          128 * .Machine$double.eps * max(1, abs(overall)))
        fail("local quantile integration disagrees with cumulative income beyond arithmetic precision.")
      bracket_income[i] <- value
    }
    if (length(small)) tail_income <- rev(cumsum(rev(bracket_income)))
    tab <- list(fractile = p, threshold = threshold,
      bracket_average = bracket_income / diff(c(p, 1)),
      bracket_share = bracket_income / overall,
      top_average = tail_income / (1 - p), top_share = tail_income / overall)
    if (exists("gpinter_threshold_zero_atom", mode = "function"))
      tab <- gpinter_threshold_zero_atom(tab, d)
    if (any(!is.finite(unlist(tab)))) fail("the fitted tabulation contains non-finite values.")
    mean_tol <- validation_tolerance * pmax(1, abs(tab$bracket_average))
    threshold_tol <- validation_tolerance * pmax(1, abs(tab$threshold))
    if (any(diff(tab$threshold) < -pmax(head(threshold_tol, -1L), tail(threshold_tol, -1L))) ||
        any(diff(tab$bracket_average) < -pmax(head(mean_tol, -1L), tail(mean_tol, -1L))) ||
        any(tab$bracket_average < tab$threshold - mean_tol) ||
        any(tab$bracket_average > c(tab$threshold[-1L], Inf) + mean_tol) ||
        any(tab$bracket_share < -validation_tolerance) ||
        abs(sum(tab$bracket_share) - 1) > validation_tolerance)
      fail("the fitted tabulation violates ordering, threshold bounds or total income.")
    tab
  }
  original_tab <- evaluate(d$p)
  expected_tail <- rev(cumsum(rev(original_income))) / overall
  if (any(abs(original_tab$bracket_average - d$avg) > validation_tolerance * pmax(1, abs(d$avg))) ||
      any(abs(original_tab$bracket_share - original_income / overall) > validation_tolerance) ||
      any(abs(original_tab$top_share - expected_tail) > validation_tolerance))
    fail("an original population boundary or bracket income was not preserved.")
  tab <- evaluate(p_grid)
  attr(tab, "atoms") <- atoms
  attr(tab, "original_tabulation") <- original_tab
  attr(tab, "original") <- d
  tab
}
