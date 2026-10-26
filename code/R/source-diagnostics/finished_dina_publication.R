# Pure preparation of a supplied-series publication candidate. No files are
# read or written and no candidate is approved here. The group-join alternative
# is explicitly a review variant, distinct from source-inclusion corrections.

finished_dina_publication_recipe <- function() {
  list(id = "finished-dina-relative-internal-continuation", version = 3L,
    grid_denominator = 100000L, validation_tolerance = 1e-6,
    threshold_margin_relative = 1e-7, threshold_margin_gap_fraction = 0.25,
    group_boundaries = c(0, 0.5, 0.9, 0.99, 1),
    group_income_tolerance = 1e-12, join_max_iterations = 2000L,
    adjustment = "Local population-weighted least squares preserving every group income; review required",
    gini = "Source Gini retains supplied or GPinter-fitted provenance; continuation uses grouped Lorenz trapezoids")
}

# Solve a bounded local join in sqrt(population)-scaled coordinates. The
# active set contains independent rows only, so equal means are permissible.
# This is a projection onto linear group-income equalities and ordering bounds.
finished_dina_join_projection <- function(avg, mass, group, lower = 0, upper = Inf,
    max_iterations = 2000L) {
  n <- length(avg)
  if (n < 2L || length(mass) != n || length(group) != n ||
      any(!is.finite(c(avg, mass))) || any(avg <= 0 | mass <= 0) ||
      !is.finite(lower) || !is.numeric(upper) || length(upper) != 1L || is.na(upper)) {
    stop("Invalid local join inputs.", call. = FALSE)
  }
  gids <- unique(group)
  E <- t(vapply(gids, function(g) ifelse(group == g, mass, 0), numeric(n)))
  target <- as.vector(E %*% avg)
  local_mean <- target / rowSums(E)
  start <- local_mean[match(group, gids)]
  if (any(diff(local_mean) < -1e-12) || min(start) < lower - 1e-12 ||
      max(start) > upper + 1e-12) stop("Local join window is infeasible; expand it.", call. = FALSE)
  root_mass <- sqrt(mass)
  y <- avg * root_mass
  z <- start * root_mass
  A <- matrix(0, nrow = n - 1L, ncol = n)
  for (j in seq_len(n - 1L)) A[j, c(j, j + 1L)] <- c(-1, 1)
  A <- rbind(A, c(1, rep(0, n - 1L)))
  bound <- c(rep(0, n - 1L), lower)
  if (is.finite(upper)) {
    A <- rbind(A, c(rep(0, n - 1L), -1)); bound <- c(bound, -upper)
  }
  # Normalize constraints to reduce conditioning differences in tail widths.
  E_scaled <- sweep(E, 2L, root_mass, "/")
  e_norm <- sqrt(rowSums(E_scaled ^ 2))
  E_scaled <- E_scaled / e_norm; equality <- target / e_norm
  A_scaled <- sweep(A, 2L, root_mass, "/")
  a_norm <- sqrt(rowSums(A_scaled ^ 2))
  A_scaled <- A_scaled / a_norm; bound_scaled <- bound / a_norm
  rank <- function(x) qr(t(x), tol = 1e-10)$rank
  active <- integer()
  slack <- as.vector(A_scaled %*% z - bound_scaled)
  for (j in which(slack <= 1e-12)) {
    current <- rbind(E_scaled, A_scaled[active, , drop = FALSE])
    if (rank(rbind(current, A_scaled[j, ])) > nrow(current)) active <- c(active, j)
  }
  for (iteration in seq_len(max_iterations)) {
    C <- rbind(E_scaled, A_scaled[active, , drop = FALSE])
    target_C <- c(equality, bound_scaled[active])
    projection <- tryCatch({
      decomposition <- qr(t(C), tol = 1e-12)
      if (decomposition$rank != nrow(C)) stop("Dependent constraints")
      Q <- qr.Q(decomposition); R <- qr.R(decomposition)
      residual <- as.vector(C %*% y - target_C)[decomposition$pivot]
      coordinates <- forwardsolve(t(R), residual)
      multipliers <- numeric(nrow(C))
      multipliers[decomposition$pivot] <- backsolve(R, coordinates)
      list(destination = y - as.vector(Q %*% coordinates), multipliers = multipliers)
    }, error = function(e) stop("Local join projection became numerically singular.", call. = FALSE))
    multipliers <- projection$multipliers
    destination <- projection$destination
    direction <- destination - z
    scale <- max(1, abs(y), abs(z))
    if (max(abs(direction)) <= 1e-12 * scale) {
      wrong_sign <- if (length(active)) which(tail(multipliers, length(active)) > 1e-11 * scale) else integer()
      if (!length(wrong_sign)) {
        result <- destination / root_mass
        # Snap only floating-point differences on an active plateau. This does
        # not replace substantive reversals or perturb strictly ordered bins.
        for (block in split(seq_len(n), cumsum(c(1L, as.integer(abs(diff(result)) > 1e-12 * pmax(1, head(abs(result), -1L), tail(abs(result), -1L))))))) {
          if (length(block) > 1L) result[block] <- sum(mass[block] * result[block]) / sum(mass[block])
        }
        error <- max(abs(as.vector(E %*% result) - target))
        if (error > 1e-12 || any(diff(result) < -1e-12) ||
            min(result) < lower - 1e-12 || max(result) > upper + 1e-12 || any(result <= 0)) {
          stop("Local join projection failed income, ordering or bound validation.", call. = FALSE)
        }
        return(list(avg = result, iterations = iteration, max_income_error = error))
      }
      # Deterministic lowest-index removal also resolves tied multipliers.
      remove <- wrong_sign[which.min(active[wrong_sign])]
      active <- active[-remove]
      next
    }
    inactive <- setdiff(seq_len(nrow(A_scaled)), active)
    change <- as.vector(A_scaled %*% direction)
    candidates <- inactive[change[inactive] < -1e-13 * scale]
    alpha <- 1; blocker <- NA_integer_
    if (length(candidates)) {
      slack <- as.vector(A_scaled %*% z - bound_scaled)
      steps <- pmax(0, slack[candidates]) / -change[candidates]
      if (min(steps) < 1 - 1e-12) {
        alpha <- min(steps)
        blocker <- min(candidates[steps <= alpha + 1e-12])
      }
    }
    z <- z + alpha * direction
    if (!is.na(blocker)) {
      C <- rbind(E_scaled, A_scaled[active, , drop = FALSE])
      if (rank(rbind(C, A_scaled[blocker, ])) <= nrow(C)) {
        stop("Local join projection encountered a dependent blocking constraint.", call. = FALSE)
      }
      active <- c(active, blocker)
    }
  }
  stop(sprintf("Local join projection did not converge within %d iterations.", max_iterations), call. = FALSE)
}

finished_dina_group_join <- function(avg, mass, group, max_iterations = 2000L) {
  n <- length(avg)
  if (length(mass) != n || length(group) != n || any(!is.finite(c(avg, mass))) ||
      any(avg <= 0 | mass <= 0) || any(diff(group) < 0)) stop("Invalid group join inputs.", call. = FALSE)
  target <- as.numeric(rowsum(avg * mass, group))
  total_mass <- as.numeric(rowsum(mass, group))
  if (any(diff(target / total_mass) < -1e-12)) {
    stop("Group target means are reversed; no ordered distribution can preserve these group incomes.", call. = FALSE)
  }
  crossings <- which(diff(avg) < -1e-12)
  empty <- data.frame(first = integer(), last = integer(), iterations = integer())
  if (!length(crossings)) return(list(avg = avg, windows = empty))
  # Inputs are already ordered within each scientific group; only joins are
  # permitted to trigger repair. Unexpected interior reversals require review.
  if (any(group[crossings] == group[crossings + 1L])) {
    stop("A reversal inside a scientific group cannot be repaired as a group join.", call. = FALSE)
  }
  windows <- lapply(crossings, function(i) c(first = i, last = i + 1L))
  merge_windows <- function(windows) {
    result <- list()
    for (window in windows) {
      if (length(result) && window[[1L]] <= tail(result, 1L)[[1L]][[2L]] + 1L) {
        j <- length(result); result[[j]][[2L]] <- max(result[[j]][[2L]], window[[2L]])
      } else result[[length(result) + 1L]] <- window
    }
    result
  }
  feasible <- function(window) {
    rows <- seq.int(window[[1L]], window[[2L]])
    means <- as.numeric(rowsum(avg[rows] * mass[rows], group[rows])) /
      as.numeric(rowsum(mass[rows], group[rows]))
    lower <- if (min(rows) > 1L) avg[min(rows) - 1L] else 0
    upper <- if (max(rows) < n) avg[max(rows) + 1L] else Inf
    all(diff(means) >= -1e-12) && min(means) >= lower - 1e-12 && max(means) <= upper + 1e-12
  }
  for (expansion in seq_len(n)) {
    windows <- merge_windows(windows)
    ok <- vapply(windows, feasible, logical(1))
    if (all(ok)) break
    for (j in which(!ok)) windows[[j]] <- c(max(1L, windows[[j]][[1L]] - 1L), min(n, windows[[j]][[2L]] + 1L))
  }
  if (!all(vapply(windows, feasible, logical(1)))) stop("No feasible bounded join window was found.", call. = FALSE)
  out <- avg; records <- list()
  for (window in windows) {
    rows <- seq.int(window[[1L]], window[[2L]])
    lower <- if (min(rows) > 1L) avg[min(rows) - 1L] else 0
    upper <- if (max(rows) < n) avg[max(rows) + 1L] else Inf
    fitted <- finished_dina_join_projection(avg[rows], mass[rows], group[rows], lower, upper, max_iterations)
    out[rows] <- fitted$avg
    records[[length(records) + 1L]] <- data.frame(first = min(rows), last = max(rows), iterations = fitted$iterations)
  }
  error <- max(abs(as.numeric(rowsum(out * mass, group)) - target))
  if (error > 1e-12 || any(diff(out) < -1e-12)) stop("Local joins did not preserve ordered group totals.", call. = FALSE)
  list(avg = out, windows = do.call(rbind, records))
}

finished_dina_publication_build <- function(prepared, internal, specification,
    first_year, last_year, export_last_year, adjustment = c("none", "group_join")) {
  if (length(adjustment) == 1L && identical(adjustment, "isotonic")) {
    stop("The obsolete isotonic alternative does not preserve four-group incomes. Use group_join or none and prepare a new candidate.", call. = FALSE)
  }
  adjustment <- match.arg(adjustment)
  recipe <- finished_dina_publication_recipe()
  tol <- recipe$validation_tolerance
  grid <- c(seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
    seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001))
  ticks <- as.integer(round(grid * recipe$grid_denominator))
  # Integer lattice also removes harmless Stata float storage differences.
  grid <- ticks / recipe$grid_denominator
  mass <- diff(c(grid, 1))
  group_labels <- c("p0p50", "p50p90", "p90p99", "p99p100")
  group_id <- findInterval(grid, recipe$group_boundaries)
  group_sum <- function(values) as.numeric(rowsum(mass * values, group_id, reorder = TRUE))
  group_mass <- as.numeric(rowsum(mass, group_id, reorder = TRUE))
  format_p <- function(p) sub("\\.?0+$", "", sprintf("%.6f", p * 100))
  labels <- paste0("p", format_p(grid), "p", format_p(c(tail(grid, -1L), 1)))
  labels <- sub("^pp", "p0p", labels)
  key_fields <- c("country", "year", "step", "unit")
  empty_checks <- data.frame(country = character(), year = integer(), step = character(),
    unit = character(), check_id = character(), status = character(),
    percentile = character(), detail = character(), stringsAsFactors = FALSE)
  empty_changes <- data.frame(country = character(), year = integer(), step = character(),
    unit = character(), percentile = character(), field = character(), before = numeric(),
    after = numeric(), difference = numeric(), relative_percent = numeric(), rule = character(),
    reason = character(), stringsAsFactors = FALSE)
  empty_impact <- data.frame(country = character(), year = integer(), step = character(),
    unit = character(), statistic = character(), before = numeric(), after = numeric(),
    difference = numeric(), basis = character(), stringsAsFactors = FALSE)
  empty_groups <- data.frame(country = character(), year = integer(), step = character(),
    unit = character(), group = character(), p_lower = numeric(), p_upper = numeric(),
    population_mass = numeric(), source_share = numeric(), internal_anchor_share = numeric(),
    internal_current_share = numeric(), growth_factor = numeric(), normalizer = numeric(),
    target_share = numeric(), achieved_share = numeric(), income_difference = numeric(),
    status = character(), stringsAsFactors = FALSE)
  checks <- changes <- impacts <- output <- group_rows <- list()
  bind <- function(rows, empty) if (length(rows)) do.call(rbind, rows) else empty
  record <- function(country = "", year = NA_integer_, check_id, status, detail,
      percentile = "") {
    checks[[length(checks) + 1L]] <<- data.frame(country = country, year = year,
      step = as.character(specification$publication$step %||publication% ""),
      unit = as.character(specification$publication$unit %||publication% ""),
      check_id = check_id, status = status, percentile = percentile,
      detail = detail, stringsAsFactors = FALSE)
    invisible(identical(status, "pass"))
  }
  finish <- function() {
    check_table <- bind(checks, empty_checks)
    list(data = bind(output, data.frame()), changes = bind(changes, empty_changes),
      checks = check_table, impact = bind(impacts, empty_impact),
      groups = bind(group_rows, empty_groups),
      problems = check_table[check_table$status != "pass", , drop = FALSE],
      valid = nrow(check_table) > 0L && !any(check_table$status == "fail"),
      adjustment = adjustment, review_required = TRUE, recipe = recipe)
  }
  `%||publication%` <- function(x, y) if (is.null(x) || !length(x)) y else x
  scalar_year <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x)
  years <- specification$years
  if (is.list(years)) {
    source_first <- years$first
    source_last <- years$last
    source_years <- if (scalar_year(source_first) && scalar_year(source_last) &&
      source_first <= source_last && source_last - source_first < 10000) seq.int(source_first, source_last) else numeric()
  } else {
    source_years <- sort(unique(as.numeric(years)))
  }
  countries <- as.character(unlist(specification$countries, use.names = FALSE))
  step <- as.character(specification$publication$step)
  unit <- as.character(specification$publication$unit)
  good_config <- length(countries) > 0L && !anyNA(countries) &&
    all(grepl("^[A-Z]{3}$", countries)) && !anyDuplicated(countries) &&
    length(step) == 1L && !is.na(step) && nzchar(step) &&
    length(unit) == 1L && !is.na(unit) && nzchar(unit) &&
    length(source_years) > 0L && all(is.finite(source_years)) &&
    all(source_years == floor(source_years)) &&
    all(vapply(list(first_year, last_year, export_last_year), scalar_year, logical(1)))
  if (!good_config) {
    # Safe diagnostic strings when metadata itself is malformed.
    specification$publication <- list(step = paste(step, collapse = ","), unit = paste(unit, collapse = ","))
    record(check_id = "configuration", status = "fail",
      detail = "Declare countries, publication step/unit, supplied years and finite integer calculation/export years.")
    return(finish())
  }
  anchor <- max(source_years)
  policy <- specification$publication_policy
  if ((!is.null(policy$continuation) && !identical(policy$continuation, "four_group_relative_changes")) ||
      (!is.null(policy$carry_forward) && !isTRUE(policy$carry_forward)) ||
      (!is.null(policy$include_history) && !isTRUE(policy$include_history))) {
    record(check_id = "publication_policy", status = "fail", detail = "This recipe requires explicitly reviewed four-group continuation, observed history and final-year carry-forward.")
    return(finish())
  }
  if (first_year > last_year || last_year < anchor || export_last_year < last_year ||
      export_last_year - min(source_years) > 10000) {
    record(check_id = "configuration", status = "fail",
      detail = "Calculation years must be ordered and reach the supplied anchor; export must reach the last calculation year.")
    return(finish())
  }
  required <- c(key_fields, "p", "avg", "thr", "average")
  for (name in c("prepared", "internal")) {
    value <- as.data.frame(get(name), stringsAsFactors = FALSE)
    # No internal table is required if there is no continuation to calculate.
    if (name == "internal" && last_year == anchor && !nrow(value)) {
      internal <- value
      next
    }
    missing <- setdiff(required, names(value))
    if (length(missing)) record(check_id = paste0(name, "_fields"), status = "fail",
      detail = paste("Missing fields:", paste(missing, collapse = ", ")))
    else {
      for (field in c("country", "step", "unit")) value[[field]] <- as.character(value[[field]])
      bad_numeric <- required[required %in% c("year", "p", "avg", "thr", "average")]
      if (!all(vapply(value[bad_numeric], is.numeric, logical(1)))) {
        record(check_id = paste0(name, "_fields"), status = "fail", detail = "Year, boundaries and monetary fields must be numeric.")
      }
      assign(name, value)
    }
  }
  if (any(vapply(checks, function(x) x$status == "fail", logical(1)))) return(finish())
  prepared <- prepared[prepared$country %in% countries & prepared$step == step & prepared$unit == unit, , drop = FALSE]
  if (nrow(internal)) internal <- internal[internal$country %in% countries & internal$step == step & internal$unit == unit, , drop = FALSE]
  add_change <- function(country, year, field, before, after, rule, reason, percentile = labels) {
    n <- max(length(before), length(after), length(percentile))
    before <- rep_len(before, n); after <- rep_len(after, n); percentile <- rep_len(percentile, n)
    keep <- is.na(before) != is.na(after) | (!is.na(before) & !is.na(after) & before != after)
    if (!any(keep)) return(invisible(NULL))
    relative <- (after - before) / abs(before) * 100
    relative[before == 0] <- NA_real_
    changes[[length(changes) + 1L]] <<- data.frame(country = country, year = year,
      step = step, unit = unit, percentile = percentile[keep], field = field,
      before = before[keep], after = after[keep], difference = (after - before)[keep],
      relative_percent = relative[keep], rule = rule, reason = reason, stringsAsFactors = FALSE)
  }
  get_case <- function(d, country, year, kind) {
    x <- d[!is.na(d$country) & !is.na(d$year) & d$country == country & d$year == year, , drop = FALSE]
    # Growth is measured for four broad groups, not individual brackets.
    # Internal zero-income bins are valid; only the group denominators below
    # and the overall mean must be positive. Do not smooth away zero income.
    supplied_case <- identical(kind, "supplied")
    finite <- nrow(x) > 0L && all(is.finite(x$p)) && all(is.finite(x$avg)) &&
      all(is.finite(x$thr)) && all(is.finite(x$average)) &&
      all(if (supplied_case) x$avg > 0 else x$avg >= 0) && all(x$average > 0)
    if (!finite) {
      record(country, year, paste0(kind, "_values"), "fail",
        if (supplied_case) "Required supplied distribution is missing, nonfinite, or has a nonpositive bracket/overall mean."
        else "Required internal distribution is missing, nonfinite, has a negative bracket mean, or has a nonpositive overall mean.")
      return(NULL)
    }
    order <- order(x$p); x <- x[order, , drop = FALSE]
    tick <- as.integer(round(x$p * recipe$grid_denominator))
    if (nrow(x) != length(ticks) || anyDuplicated(tick) || !identical(tick, ticks) ||
        any(abs(x$p - grid) > 6e-8)) {
      record(country, year, paste0(kind, "_grid"), "fail", "Expected exactly 127 unique standard population brackets.")
      return(NULL)
    }
    supplied_average <- x$average[[1L]]
    mean <- sum(mass * x$avg)
    if (any(abs(x$average / supplied_average - 1) > tol) || abs(mean / supplied_average - 1) > tol) {
      record(country, year, paste0(kind, "_mean_identity"), "fail", "Bracket means and standard population masses do not match the stated overall mean within 1e-6.")
      return(NULL)
    }
    if (kind == "supplied") {
      for (field in intersect(c("bckt_sh", "topsh", "topavg"), names(x))) {
        wanted <- switch(field, bckt_sh = mass * x$avg / mean,
          topsh = rev(cumsum(rev(mass * x$avg / mean))),
          topavg = rev(cumsum(rev(mass * x$avg))) / (1 - grid))
        supplied <- x[[field]]
        if (!is.numeric(supplied) || any(!is.finite(supplied)) ||
            any(abs(supplied - wanted) > 1e-8 + tol * pmax(abs(supplied), abs(wanted)))) {
          record(country, year, paste0(kind, "_", field, "_identity"), "fail",
            paste("Prepared", field, "is inconsistent with bracket income and population."))
          return(NULL)
        }
      }
      if (!"gini" %in% names(x) || !is.numeric(x$gini) || any(!is.finite(x$gini)) ||
          any(x$gini < 0 | x$gini > 1) || max(x$gini) - min(x$gini) > tol) {
        record(country, year, "supplied_gini", "fail", "A finite, common supplied Gini in [0,1] is required.")
        return(NULL)
      }
      if (identical(specification$gini_kind, "gpinter_fitted") &&
          (!"fitted_gini" %in% names(x) || !is.numeric(x$fitted_gini) || any(!is.finite(x$fitted_gini)) ||
           any(abs(x$fitted_gini - x$gini) > tol))) {
        record(country, year, "source_gini_provenance", "fail", "A GPinter-derived source must retain its finite fitted Gini without relabelling it as supplied.")
        return(NULL)
      }
    }
    record(country, year, paste0(kind, "_input"), "pass",
      if (supplied_case) "Complete compatible grid, finite positive means and consistent population-weighted total."
      else sprintf("Complete compatible grid, nonnegative bracket means and positive consistent overall mean; %d zero-income brackets retained.", sum(x$avg == 0)))
    list(raw = x, avg = x$avg / mean, thr = x$thr / mean, mean = mean)
  }
  grouped_gini <- function(avg) {
    share <- mass * avg / sum(mass * avg)
    lorenz <- c(0, cumsum(share))
    1 - sum(mass * (head(lorenz, -1L) + tail(lorenz, -1L)))
  }
  stats <- function(avg) {
    share <- mass * avg / sum(mass * avg)
    c(average = sum(mass * avg), bottom50 = sum(share[grid < 0.5]),
      top10 = sum(share[grid >= 0.9]), top1 = sum(share[grid >= 0.99]),
      top0_1 = sum(share[grid >= 0.999]), top0_01 = sum(share[grid >= 0.9999]),
      grouped_gini = grouped_gini(avg))
  }
  make_case <- function(country, year, avg, thr, origin, source, before = avg,
      carried_from = NA_integer_) {
    norm <- sum(mass * avg)
    if (!all(is.finite(c(avg, thr, norm))) || any(avg <= 0) || norm <= 0) {
      record(country, year, "continuation_values", "fail", "Relative changes generated invalid or nonpositive income; inspect anchor denominators.")
      return(NULL)
    }
    avg <- avg / norm; thr <- thr / norm
    raw_avg <- avg; raw_thr <- thr
    if (origin == "continuation" && adjustment == "group_join") {
      joined <- tryCatch(finished_dina_group_join(avg, mass, group_id,
        max_iterations = recipe$join_max_iterations), error = function(e) e)
      if (inherits(joined, "error")) {
        record(country, year, "group_join_feasibility", "fail", conditionMessage(joined))
        return(NULL)
      }
      avg <- joined$avg
      if (nrow(joined$windows)) for (j in seq_len(nrow(joined$windows))) {
        rows <- seq.int(joined$windows$first[[j]], joined$windows$last[[j]])
        add_change(country, year, "avg", raw_avg[rows], avg[rows], "review_group_join_means",
          sprintf("Local weighted least-squares join, window %s to %s; exact group income constraints; %d solver iterations.",
            labels[min(rows)], labels[max(rows)], joined$windows$iterations[[j]]), labels[rows])
      }
      record(country, year, "group_join_feasibility", "pass",
        sprintf("%d local join windows; unselected bins and each group income retained.", nrow(joined$windows)))
      for (i in seq_along(thr)) {
        lower <- if (i == 1L) -Inf else avg[[i - 1L]]
        upper <- avg[[i]]
        gap <- max(0, upper - lower)
        margin <- min(recipe$threshold_margin_relative * max(abs(upper), 1),
          recipe$threshold_margin_gap_fraction * gap)
        # A pooled plateau has a valid single boundary, without an artificial
        # strictly increasing perturbation or any extra income adjustment.
        lo <- if (is.finite(lower)) lower + margin else lower
        hi <- upper - margin
        thr[[i]] <- min(max(thr[[i]], lo), hi)
      }
      repair <- c(-Inf, head(avg, -1L)) > raw_thr | raw_thr > avg
      if (any(repair)) add_change(country, year, "thr", raw_thr[repair], thr[repair],
        "review_threshold_repair", "Review alternative: nearest feasible threshold between adjacent means.", labels[repair])
      if (any(!repair)) add_change(country, year, "thr", raw_thr[!repair], thr[!repair],
        "review_threshold_margin", "Review alternative: numerical interior margin; zero width for a pooled plateau.", labels[!repair])
      preserved <- abs(sum(mass * avg) - sum(mass * raw_avg)) <= 1e-12
      record(country, year, "adjustment_income_preserved", if (preserved) "pass" else "fail",
        sprintf("Population-weighted income difference after local group-join correction: %.12g.", sum(mass * (avg - raw_avg))))
    }
    reversal <- which(diff(avg) < -1e-12 * pmax(1, abs(head(avg, -1L)), abs(tail(avg, -1L)))) + 1L
    record(country, year, "mean_ordering", if (length(reversal)) "fail" else "pass",
      sprintf("%d adjacent bracket-mean reversals.", length(reversal)), paste(labels[reversal], collapse = ", "))
    lower <- c(-Inf, head(avg, -1L))
    # There is no previous-mean constraint on the first threshold.
    bad_thr <- which((is.finite(lower) & thr < lower - 1e-12 * pmax(1, abs(lower))) |
      thr > avg + 1e-12 * pmax(1, abs(avg)))
    record(country, year, "threshold_bounds", if (length(bad_thr)) "fail" else "pass",
      sprintf("%d thresholds outside adjacent bracket means.", length(bad_thr)), paste(labels[bad_thr], collapse = ", "))
    share <- mass * avg
    top_share <- rev(cumsum(rev(share)))
    bottom_share <- c(0, head(cumsum(share), -1L))
    bottomavg <- numeric(length(grid)); bottomavg[grid > 0] <- bottom_share[grid > 0] / grid[grid > 0]
    discrete <- grouped_gini(avg)
    source_gini <- source$raw$gini[[1L]]
    fitted_source <- identical(specification$gini_kind, "gpinter_fitted")
    supplied_gini <- if (fitted_source) NA_real_ else source_gini
    supplied_fitted <- if ("fitted_gini" %in% names(source$raw)) source$raw$fitted_gini[[1L]] else NA_real_
    out <- data.frame(country = country, year = year, step = step, unit = unit,
      percentile = labels, p = grid, average = 1, thr = thr, avg = avg,
      bckt_sh = share, topsh = top_share, bottomsh = bottom_share,
      topavg = top_share / (1 - grid), bottomavg = bottomavg,
      gini = if (origin == "supplied") source_gini else discrete,
      supplied_gini = if (origin == "supplied") supplied_gini else NA_real_,
      fitted_gini = if (origin == "supplied") supplied_fitted else NA_real_,
      grouped_gini = discrete,
      gini_method = if (origin == "supplied") if (fitted_source) "gpinter_fitted" else "supplied" else "grouped_lorenz_trapezoid",
      origin = origin, anchor_year = anchor, carried_from = carried_from,
      adjustment = if (origin == "supplied") "none" else adjustment,
      source_id = as.character(specification$source_id %||publication% ""),
      source_version = as.character(specification$version %||publication% ""),
      citation = as.character(specification$citation %||publication% ""),
      doi = as.character(specification$doi %||publication% ""), stringsAsFactors = FALSE)
    identities <- all(is.finite(as.matrix(out[c("avg", "thr", "bckt_sh", "topsh", "bottomsh", "topavg", "bottomavg")]))) &&
      abs(sum(share) - 1) <= 1e-12 && all(abs(top_share + bottom_share - 1) <= 1e-12) &&
      discrete >= -1e-12 && discrete <= 1 + 1e-12
    record(country, year, "derived_statistics", if (identities) "pass" else "fail",
      "Normalized mean, bracket/top/bottom shares and cumulative means; grouped Gini calculated without assuming a fitted continuous tail.")
    before_stats <- stats(before); after_stats <- stats(avg)
    impacts[[length(impacts) + 1L]] <<- data.frame(country = country, year = year,
      step = step, unit = unit, statistic = names(after_stats),
      before = unname(before_stats), after = unname(after_stats),
      difference = unname(after_stats - before_stats),
      basis = if (origin == "supplied") "supplied_normalized_distribution" else "unadjusted_normalized_continuation",
      stringsAsFactors = FALSE)
    out
  }
  for (country in countries) {
    extra <- setdiff(unique(prepared$year[prepared$country == country]), source_years)
    if (length(extra)) record(country, check_id = "supplied_years", status = "fail",
      detail = paste("Prepared source contains years outside registered coverage:", paste(extra, collapse = ", ")))
    source_cases <- list()
    for (year in source_years) {
      source <- get_case(prepared, country, year, "supplied")
      if (is.null(source)) next
      source_cases[[as.character(year)]] <- source
      out <- make_case(country, year, source$avg, source$thr, "supplied", source, before = source$avg)
      if (!is.null(out)) output[[length(output) + 1L]] <- out
    }
    source_anchor <- source_cases[[as.character(anchor)]]
    if (is.null(source_anchor)) next
    if (last_year > anchor) {
      internal_anchor <- get_case(internal, country, anchor, "internal_anchor")
      # Still inspect every expected future input if the anchor is missing.
      for (year in seq.int(anchor + 1L, last_year)) {
        current <- get_case(internal, country, year, "internal_current")
        if (is.null(internal_anchor) || is.null(current)) next
        source_share <- group_sum(source_anchor$avg)
        internal_anchor_share <- group_sum(internal_anchor$avg)
        internal_current_share <- group_sum(current$avg)
        group_factors <- internal_current_share / internal_anchor_share
        if (any(!is.finite(group_factors) | group_factors <= 0)) {
          record(country, year, "group_growth_factors", "fail", "Each internal anchor/current group income must be finite and strictly positive.")
          next
        }
        factors <- group_factors[group_id]
        avg <- source_anchor$avg * factors
        thr <- source_anchor$thr * factors
        norm <- sum(mass * avg)
        if (!all(is.finite(c(factors, avg, thr, norm))) || norm <= 0) {
          record(country, year, "continuation_values", "fail", "Anchor ratios overflow or produce invalid normalized incomes.")
          next
        }
        avg <- avg / norm; thr <- thr / norm
        add_change(country, year, "avg", source_anchor$avg, avg, "internal_group_relative_change",
          "Supplied anchor normalized bracket income multiplied by its four-group internal income-share ratio, then normalized once across all groups.")
        add_change(country, year, "thr", source_anchor$thr, thr, "internal_group_relative_change",
          "The same four-group factor and common normalization are applied to the supplied anchor threshold.")
        out <- make_case(country, year, avg, thr, "continuation", source_anchor, before = avg)
        target_share <- source_share * group_factors / norm
        achieved_share <- if (is.null(out)) rep(NA_real_, 4L) else group_sum(out$avg)
        gap <- achieved_share - target_share
        group_status <- ifelse(is.finite(gap) & abs(gap) <= recipe$group_income_tolerance, "pass", "fail")
        group_rows[[length(group_rows) + 1L]] <- data.frame(country = country, year = year,
          step = step, unit = unit, group = group_labels,
          p_lower = head(recipe$group_boundaries, -1L), p_upper = tail(recipe$group_boundaries, -1L),
          population_mass = group_mass, source_share = source_share,
          internal_anchor_share = internal_anchor_share, internal_current_share = internal_current_share,
          growth_factor = group_factors, normalizer = norm, target_share = target_share,
          achieved_share = achieved_share, income_difference = gap, status = group_status)
        record(country, year, "group_income_preservation", if (all(group_status == "pass")) "pass" else "fail",
          if (all(is.finite(gap))) sprintf("Maximum absolute group income difference: %.12g; tolerance 1e-12.", max(abs(gap)))
          else "Join correction could not produce all four required group incomes.")
        if (!is.null(out)) output[[length(output) + 1L]] <- out
      }
    }
    if (export_last_year > last_year) {
      latest <- Filter(function(x) identical(x$country[[1L]], country) && x$year[[1L]] == last_year, output)
      if (!length(latest)) {
        record(country, last_year, "carry_forward_source", "fail", "The last calculation year is unavailable; carry-forward cannot skip it.")
      } else for (year in seq.int(last_year + 1L, export_last_year)) {
        out <- latest[[1L]]
        out$year <- year; out$origin <- "carry_forward"; out$carried_from <- last_year
        output[[length(output) + 1L]] <- out
        last_groups <- Filter(function(x) x$country[[1L]] == country && x$year[[1L]] == last_year, group_rows)
        if (length(last_groups)) {
          carried_groups <- last_groups[[1L]]; carried_groups$year <- year
          group_rows[[length(group_rows) + 1L]] <- carried_groups
        }
        record(country, year, "carry_forward", "pass", paste("Distribution carried unchanged from", last_year, "without implying a new observation."))
      }
    }
    available <- vapply(Filter(function(x) x$country[[1L]] == country, output), function(x) x$year[[1L]], numeric(1))
    expected <- c(source_years, if (export_last_year > anchor) seq.int(anchor + 1L, export_last_year) else integer())
    missing <- setdiff(expected, available)
    record(country, check_id = "complete_coverage", status = if (length(missing)) "fail" else "pass",
      detail = if (length(missing)) paste("Missing expected years:", paste(missing, collapse = ", ")) else
        sprintf("All %d supplied/continued/carried years retain 127 brackets.", length(expected)))
  }
  finish()
}
