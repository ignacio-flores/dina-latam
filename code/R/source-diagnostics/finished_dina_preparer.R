# Prepare a reviewed supplied distribution. This module has no filesystem or
# pipeline dependencies: the caller owns staging, confirmation and publication.
# finished_dina_explorer.R supplies the validator and the standard export grid.

finished_dina_preparation_recipe <- function() {
  list(
    id = "finished-dina-inferred-population-gpinter", version = 1L,
    precision_tolerance = 1e-6, money_absolute_tolerance = 1e-6,
    pooling_relative_limit = 1e-5, threshold_margin_relative = 1e-7,
    threshold_margin_gap_fraction = 0.25,
    gpinter_version = if (requireNamespace("gpinter", quietly = TRUE))
      as.character(utils::packageVersion("gpinter")) else NA_character_
  )
}

finished_dina_prepare <- function(data, specification = list(), progress = NULL) {
  if (!requireNamespace("gpinter", quietly = TRUE)) {
    stop("Finished DINA preparation requires the local gpinter R package.", call. = FALSE)
  }
  recipe <- finished_dina_preparation_recipe()
  tol <- recipe$precision_tolerance
  abs_tol <- recipe$money_absolute_tolerance
  close_money <- function(a, b) abs(a - b) <= abs_tol + tol * pmax(abs(a), abs(b))
  raw_validation <- finished_dina_validate(data, specification)
  # These issues cannot be explained by population weights or repaired by the
  # explicitly approved threshold, pooling and export-grid rules.
  blocked <- raw_validation$issues$status == "fail" |
    raw_validation$issues$check_id %in% c(
      "gini_bounds", "mean_domain", "share_bounds", "share_total", "top_share_identity"
    )
  if (any(blocked)) {
    bad <- raw_validation$issues[which(blocked)[[1L]], , drop = FALSE]
    stop(sprintf("Finished DINA preparation: %s %s %s/%s %s: %s (%s).",
      bad$country, bad$year, bad$step, bad$unit, bad$percentile, bad$detail, bad$check_id),
      call. = FALSE)
  }
  d <- as.data.frame(data, stringsAsFactors = FALSE)
  key_fields <- c("country", "year", "step", "unit")
  for (name in c("country", "step", "unit", "percentile")) d[[name]] <- as.character(d[[name]])
  group_key <- do.call(paste, c(d[key_fields], sep = "\r"))
  groups <- split(seq_len(nrow(d)), group_key)
  grid <- finished_dina_export_grid()
  grid_end <- c(tail(grid, -1L), 1)
  label_number <- function(x) sub("\\.?0+$", "", sprintf("%.6f", x * 100))
  grid_labels <- paste0("p", label_number(grid), "p", label_number(grid_end))
  # The special zero representation should remain p0, not an empty string.
  grid_labels <- sub("^pp", "p0p", grid_labels)
  changes <- impacts <- summaries <- prepared <- list()
  change_count <- impact_count <- 0L
  for (group in groups) {
    x <- d[group, , drop = FALSE]
    original_lower <- as.numeric(sub("^p([^p]+)p.*$", "\\1", x$percentile)) / 100
    x <- x[order(original_lower), , drop = FALSE]
    original_lower <- sort(original_lower)
    original_upper <- as.numeric(sub("^p[^p]+p", "", x$percentile)) / 100
    key <- x[1L, key_fields, drop = FALSE]
    name <- paste(x$country[[1L]], x$year[[1L]], paste(x$step[[1L]], x$unit[[1L]], sep = "/"))
    if (is.function(progress)) progress(length(prepared) + 1L, length(groups), name)
    abort <- function(reason, rows = integer()) {
      labels <- if (length(rows)) paste(x$percentile[head(rows, 4L)], collapse = ", ") else "distribution"
      stop(sprintf("Finished DINA preparation: %s %s: %s", name, labels, reason), call. = FALSE)
    }
    add_change <- function(field, before, after, rule, reason,
                           rows = seq_len(nrow(x)), labels = x$percentile[rows],
                           retain_equal = FALSE) {
      n <- max(length(before), length(after), length(labels))
      if (!n) return(invisible(NULL))
      before <- rep_len(as.numeric(before), n)
      after <- rep_len(as.numeric(after), n)
      labels <- rep_len(labels, n)
      keep <- if (retain_equal) rep(TRUE, n) else is.na(before) | is.na(after) | before != after
      if (!any(keep)) return(invisible(NULL))
      relative <- 100 * (after - before) / abs(before)
      relative[before == 0 & after == 0] <- 0
      relative[before == 0 & after != 0] <- NA_real_
      change_count <<- change_count + 1L
      changes[[change_count]] <<- data.frame(
        key[rep(1L, sum(keep)), , drop = FALSE], percentile = labels[keep],
        field = rep_len(field, n)[keep], before = before[keep], after = after[keep],
        difference = (after - before)[keep], relative_percent = relative[keep],
        rule = rep_len(rule, n)[keep], reason = rep_len(reason, n)[keep],
        stringsAsFactors = FALSE, row.names = NULL
      )
      invisible(NULL)
    }
    add_impact <- function(statistic, before, after, basis, reason,
                           original_boundary = NA_real_, target_boundary = NA_real_) {
      impact_count <<- impact_count + 1L
      impacts[[impact_count]] <<- data.frame(
        key, statistic = statistic, before = before, after = after,
        difference = after - before, basis = basis, reason = reason,
        original_population_boundary = original_boundary,
        target_population_boundary = target_boundary, stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
    if (any(x$avg <= 0)) abort("Nonpositive bracket means do not identify positive population masses.", which(x$avg <= 0))
    weights_raw <- x$bckt_sh * x$average / x$avg
    if (any(!is.finite(weights_raw) | weights_raw <= 0)) {
      abort("Inferred bracket population must be finite and strictly positive.",
        which(!is.finite(weights_raw) | weights_raw <= 0))
    }
    total_weight <- sum(weights_raw)
    if (abs(total_weight - 1) > tol) {
      abort(sprintf("Inferred population sums to %.12g; normalization exceeds the approved 1e-6 tolerance.", total_weight))
    }
    # Validate independent supplied top means before normalizing. A common
    # population scaling cancels in this ratio.
    inferred_top_mean <- rev(cumsum(rev(weights_raw * x$avg))) / rev(cumsum(rev(weights_raw)))
    bad <- !close_money(x$topavg, inferred_top_mean)
    if (any(bad)) abort("Supplied cumulative top mean is incompatible with inferred population weights.", which(bad))
    weight <- weights_raw / total_weight
    effective_p <- c(0, head(cumsum(weight), -1L))
    if (any(diff(effective_p) <= 0) || tail(effective_p, 1L) >= 1) {
      abort("Inferred population boundaries cannot be represented as distinct numbers.")
    }
    average <- sum(weight * x$avg)
    if (!is.finite(average) || average <= 0) abort("Normalized overall mean is invalid.")
    if (abs(average / x$average[[1L]] - 1) > tol) {
      abort("Recomputed overall mean exceeds the approved 1e-6 precision adjustment.")
    }
    normalized_share <- weight * x$avg / average
    add_change("population_mass", original_upper - original_lower, weights_raw,
      "inferred_population", "Population recovered from supplied share, overall mean and bracket mean.")
    add_change("population_mass", weights_raw, weight, "precision_normalization",
      "Normalize the inferred total population to one within the approved 1e-6 tolerance.")
    add_change("p", original_lower, effective_p, "inferred_population",
      "Effective cumulative population boundary; original percentile labels need not equal exact population weights.")
    add_change("average", x$average[[1L]], average, "precision_normalization",
      "Overall mean recomputed from normalized population and unchanged bracket means.",
      rows = 1L, labels = "all")
    add_change("bckt_sh", x$bckt_sh, normalized_share, "precision_normalization",
      "Normalize supplied bracket shares consistently with the normalized population and overall mean.")

    # Weighted pool-adjacent-violators. Only reversed/equal adjoining means are
    # pooled, and every original mean is checked against the agreed tiny limit.
    blocks <- list()
    for (j in seq_len(nrow(x))) {
      blocks[[length(blocks) + 1L]] <- list(rows = j, weight = weight[[j]], income = weight[[j]] * x$avg[[j]])
      while (length(blocks) >= 2L) {
        n <- length(blocks)
        left <- blocks[[n - 1L]]
        right <- blocks[[n]]
        if (left$income / left$weight < right$income / right$weight) break
        blocks[[n - 1L]] <- list(rows = c(left$rows, right$rows),
          weight = left$weight + right$weight, income = left$income + right$income)
        blocks[[n]] <- NULL
      }
    }
    starts <- vapply(blocks, function(block) block$rows[[1L]], integer(1))
    pooled_weight <- vapply(blocks, `[[`, numeric(1), "weight")
    means <- vapply(blocks, function(block) block$income / block$weight, numeric(1))
    # Avoid arithmetic round-off being recorded as an income correction in
    # unpooled bins.
    singleton <- lengths(lapply(blocks, `[[`, "rows")) == 1L
    means[singleton] <- x$avg[starts[singleton]]
    pooled_rows <- integer()
    for (j in which(!singleton)) {
      rows <- blocks[[j]]$rows
      relative <- abs(means[[j]] / x$avg[rows] - 1)
      if (any(relative > recipe$pooling_relative_limit)) {
        abort(sprintf("Pooling requires a bracket mean change of %.8g%%, above the approved 0.001%% limit.",
          max(relative) * 100), rows)
      }
      pooled_rows <- c(pooled_rows, rows)
      add_change("avg", x$avg[rows], means[[j]], "tiny_mean_pooling",
        "Pool adjacent nonincreasing means using inferred population weights; preserve combined population and income.",
        rows = rows)
      # Every discarded threshold is recorded explicitly.
      removed <- tail(rows, -1L)
      add_change("thr", x$thr[removed], NA_real_, "tiny_mean_pooling",
        "Internal threshold removed when adjacent bins are pooled.", rows = removed)
    }
    if (length(blocks) < 2L) abort("Fewer than two distinct ordered mean blocks remain for GPinter.")
    fit_p <- effective_p[starts]
    thresholds <- x$thr[starts]
    repair_count <- margin_count <- 0L
    for (j in seq_along(thresholds)) {
      # No preceding bracket exists at p0. Retain a valid lower support,
      # including the supplied zero; project only an invalid first threshold.
      lower <- if (j == 1L) -Inf else means[[j - 1L]]
      upper <- means[[j]]
      gap <- upper - lower
      margin <- min(recipe$threshold_margin_relative * max(abs(upper), 1),
                    recipe$threshold_margin_gap_fraction * gap)
      feasible_lower <- if (is.finite(lower)) lower + margin else lower
      feasible_upper <- upper - margin
      old <- thresholds[[j]]
      projected <- min(max(old, feasible_lower), feasible_upper)
      if (!is.finite(projected) || !(projected < upper) ||
          (is.finite(lower) && !(projected > lower))) abort("Threshold projection has no representable feasible interior.", starts[[j]])
      if (projected != old) {
        repair <- old < lower || old > upper
        rule <- if (repair) "threshold_repair" else "threshold_numerical_margin"
        repair_count <- repair_count + as.integer(repair)
        margin_count <- margin_count + as.integer(!repair)
        add_change("thr", old, projected, rule,
          if (repair) "Project erroneous threshold to the nearest feasible boundary between adjacent bracket means, with the agreed interior margin."
          else "Move a boundary threshold by the agreed numerical margin required by GPinter.",
          rows = starts[[j]])
        thresholds[[j]] <- projected
      }
    }
    fit <- tryCatch(gpinter::tabulation_fit(
      p = fit_p, threshold = thresholds, average = average, bracketavg = means),
      error = function(e) abort(paste("GPinter fitting failed:", conditionMessage(e))))
    tabulate <- function(p) tryCatch(
      gpinter::generate_tabulation(fit, fractiles = p, threshold = TRUE,
        bracketavg = TRUE, bracketshare = TRUE, topavg = TRUE, topshare = TRUE),
      error = function(e) abort(paste("GPinter tabulation failed:", conditionMessage(e))))
    # Validate preserved input knots, not just the generated export grid. The
    # only legitimate loss of an original internal boundary is approved pooling.
    knot_tab <- tabulate(fit_p)
    knot_income <- knot_tab$bracket_average * pooled_weight
    supplied_income <- means * pooled_weight
    if (length(knot_tab$fractile) != length(fit_p) ||
        any(!is.finite(knot_tab$bracket_average)) ||
        any(!close_money(knot_tab$bracket_average, means)) ||
        any(!close_money(knot_income, supplied_income)) ||
        !close_money(sum(knot_income), average)) {
      abort("GPinter did not preserve the approved original/pooled bracket means and income totals.")
    }
    tab <- tabulate(grid)
    fitted_gini <- tryCatch(as.numeric(gpinter::gini(fit)),
      error = function(e) abort(paste("GPinter Gini calculation failed:", conditionMessage(e))))
    numeric_output <- c("threshold", "bracket_average", "bracket_share", "top_average", "top_share")
    if (length(tab$fractile) != length(grid) || any(tab$fractile != grid) ||
        any(!vapply(tab[numeric_output], function(v) length(v) == length(grid) && all(is.finite(v)), logical(1))) ||
        length(fitted_gini) != 1L || !is.finite(fitted_gini) || fitted_gini < -tol || fitted_gini > 1 + tol) {
      abort("GPinter produced incomplete, nonfinite or out-of-bounds export statistics.")
    }
    out <- data.frame(
      percentile = grid_labels, step = x$step[[1L]], unit = x$unit[[1L]],
      country = x$country[[1L]], year = x$year[[1L]], gini = x$gini[[1L]],
      average = average, p = grid, thr = tab$threshold, avg = tab$bracket_average,
      bckt_sh = tab$bracket_share, topavg = tab$top_average, topsh = tab$top_share,
      fitted_gini = fitted_gini, stringsAsFactors = FALSE
    )
    prepared[[length(prepared) + 1L]] <- out
    add_change("bracket_count", nrow(x), length(grid), "export_grid_interpolation",
      "Fit approved bins on inferred population boundaries and tabulate the exact standard 127-bracket grid.",
      rows = 1L, labels = "all", retain_equal = TRUE)
    # Matching labels identify report comparisons only. They do not imply
    # unchanged population boundaries; the reason is explicit on each row.
    source_match <- match(out$percentile, x$percentile)
    for (field in c("thr", "avg", "bckt_sh", "topavg", "topsh")) {
      add_change(field, x[[field]][source_match], out[[field]], "export_grid_interpolation",
        "Re-evaluate the fitted distribution on exact export boundaries. Same labels may describe different populations; this difference is not a direct income correction.",
        rows = integer(), labels = out$percentile)
    }
    add_impact("overall_mean", x$average[[1L]], average, "same_population",
      "Precision normalization only; threshold fitting and grid conversion preserve the normalized overall mean.")
    add_impact("gini", x$gini[[1L]], fitted_gini, "supplied_vs_fitted",
      "Supplied Gini is retained. GPinter Gini describes the fitted distribution and is reported separately.")
    targets <- c(bottom50 = 0.5, top10 = 0.9, top1 = 0.99, top0_1 = 0.999)
    for (statistic in names(targets)) {
      target <- targets[[statistic]]
      original_row <- which(abs(original_lower - target) < 1e-10)
      output_row <- which(abs(grid - target) < 1e-10)[[1L]]
      before <- if (length(original_row)) x$topsh[[original_row[[1L]]]] else NA_real_
      after <- out$topsh[[output_row]]
      original_boundary <- if (length(original_row)) effective_p[[original_row[[1L]]]] else NA_real_
      if (statistic == "bottom50") {
        before <- 1 - before
        after <- 1 - after
      }
      add_impact(statistic, before, after, "nominal_label_to_exact_population",
        if (length(original_row)) "Source share at the labelled boundary versus fitted share at the exact population boundary; includes regridding."
        else "Source has no such boundary because its wider bracket spans it; no before value is invented.",
        original_boundary = original_boundary, target_boundary = target)
      if (length(original_row)) {
        same <- tabulate(original_boundary)$top_share[[1L]]
        if (statistic == "bottom50") same <- 1 - same
        add_impact(statistic, before, same, "same_population_boundary",
          "Compare at the source's recovered population boundary; isolates normalization and any approved pooling from export regridding.",
          original_boundary = original_boundary, target_boundary = original_boundary)
      }
    }
    summaries[[length(summaries) + 1L]] <- data.frame(
      key, source_brackets = nrow(x), fitted_brackets = length(blocks),
      prepared_brackets = length(grid), threshold_repairs = repair_count,
      threshold_nudges = margin_count, pooled_bins = length(pooled_rows),
      boundary_changes = sum(abs(effective_p - original_lower) > 6e-8),
      precision_changes = sum(weights_raw != weight) + sum(x$bckt_sh != normalized_share) +
        as.integer(x$average[[1L]] != average),
      population_normalization = total_weight - 1,
      max_mean_preservation_relative_error = max(abs(knot_tab$bracket_average / means - 1)),
      publication = x$step[[1L]] == if (is.null(specification$publication$step)) "nat" else specification$publication$step,
      stringsAsFactors = FALSE
    )
    summaries[[length(summaries)]]$publication <- summaries[[length(summaries)]]$publication &&
      x$unit[[1L]] == if (is.null(specification$publication$unit)) "esn" else specification$publication$unit
  }
  output <- do.call(rbind, prepared)
  rownames(output) <- NULL
  validation <- finished_dina_validate(output, specification)
  unacceptable <- validation$issues$status %in% c("fail", "review")
  if (any(unacceptable)) {
    bad <- validation$issues[which(unacceptable)[[1L]], , drop = FALSE]
    stop(sprintf("Prepared DINA validation failed: %s %s %s/%s %s: %s (%s).",
      bad$country, bad$year, bad$step, bad$unit, bad$percentile, bad$detail, bad$check_id),
      call. = FALSE)
  }
  bind <- function(rows) {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }
  list(data = output, changes = bind(changes), impact = bind(impacts),
    summary = bind(summaries), validation = validation,
    checks = validation$checks, issues = validation$issues, recipe = recipe,
    raw_validation = raw_validation)
}
