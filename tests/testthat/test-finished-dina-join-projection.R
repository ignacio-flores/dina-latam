source(file.path(repo_root_for_tests, "code", "R", "source-diagnostics", "finished_dina_publication.R"))

# Independent small-window optimum: enumerate every contiguous plateau partition.
# No active-set optimizer or production implementation is used here.
finished_join_partition_oracle <- function(original, mass, group, lower = -Inf, upper = Inf) {
  n <- length(original)
  groups <- unique(group)
  target <- vapply(groups, function(g) sum(mass[group == g] * original[group == g]), numeric(1))
  best <- NULL
  endpoint_sets <- expand.grid(lower = if (is.finite(lower)) c(FALSE, TRUE) else FALSE,
    upper = if (is.finite(upper)) c(FALSE, TRUE) else FALSE)
  for (code in 0:(2^(n - 1L) - 1L)) {
    starts <- as.integer(intToBits(code))[seq_len(n - 1L)]
    block <- cumsum(c(1L, starts)); k <- max(block)
    block_mass <- vapply(seq_len(k), function(j) sum(mass[block == j]), numeric(1))
    center <- vapply(seq_len(k), function(j) sum(mass[block == j] * original[block == j]), numeric(1)) / block_mass
    C <- do.call(rbind, lapply(groups, function(g) vapply(seq_len(k), function(j) sum(mass[group == g & block == j]), numeric(1))))
    for (e in seq_len(nrow(endpoint_sets))) {
      A <- C; rhs <- target
      if (endpoint_sets$lower[e]) { A <- rbind(A, as.numeric(seq_len(k) == 1)); rhs <- c(rhs, lower) }
      if (endpoint_sets$upper[e]) { A <- rbind(A, as.numeric(seq_len(k) == k)); rhs <- c(rhs, upper) }
      M <- A %*% (t(A) / block_mass)
      eig <- eigen(M, symmetric = TRUE)
      use <- eig$values > max(eig$values, 1e-30) * 1e-12
      lambda <- if (any(use)) as.vector(eig$vectors[, use, drop = FALSE] %*%
        ((t(eig$vectors[, use, drop = FALSE]) %*% (rhs - A %*% center)) / eig$values[use])) else rep(0, length(rhs))
      z <- as.vector(center + crossprod(A, lambda) / block_mass)
      if (max(abs(A %*% z - rhs)) > 1e-8 || any(diff(z) < -1e-8) ||
          z[1] < lower - 1e-8 || z[k] > upper + 1e-8) next
      result <- z[block]
      objective <- sum(mass * (result - original)^2)
      if (is.null(best) || objective < best$objective) best <- list(avg = result, objective = objective)
    }
  }
  best
}


test_that("local join projection matches an independent exhaustive partition optimum", {
  cases <- list(
    list(avg = c(1, 3, 2, 4), mass = rep(.25, 4), group = c(1, 1, 2, 2), lower = 0, upper = Inf),
    list(avg = c(.1, .2, .5, .7, .4, .6, .8, .9), mass = rep(1 / 8, 8),
      group = rep(1:2, each = 4), lower = 0, upper = 1),
    # Equal group means imply a fully tied solution with dependent constraints.
    list(avg = c(1, 1, 2, 1, 1, 2), mass = rep(1 / 6, 6),
      group = rep(1:2, each = 3), lower = 1, upper = 2),
    # The population-width ratio spans the full standard export grid.
    list(avg = c(1, 2, 1, 2), mass = c(1e-5, .01, 1e-5, .01),
      group = c(1, 1, 2, 2), lower = 0, upper = 3),
    # Protect the QR projection against large tail means and severe weight
    # imbalance, both of which lose precision through normal equations.
    list(avg = c(1, 10000, 2, 20000), mass = c(.01, 1e-5, .01, 1e-5),
      group = c(1, 1, 2, 2), lower = 0, upper = Inf),
    list(avg = c(1, 2, 1, 2), mass = c(1e-5, .3, 1e-5, .3),
      group = c(1, 1, 2, 2), lower = 0, upper = 3),
    list(avg = c(1, 3, 2, 4, 3, 5), mass = rep(1 / 6, 6),
      group = rep(1:3, each = 2), lower = 1, upper = 5),
    list(avg = c(.1, .3, .7, .4, .6, 1.2), mass = c(.02, .2, .1, .03, .25, .4),
      group = rep(1:2, each = 3), lower = 0, upper = 2)
  )
  for (case in cases) {
    expected <- finished_join_partition_oracle(case$avg, case$mass, case$group, case$lower, case$upper)
    expect_false(is.null(expected))
    fitted <- do.call(finished_dina_join_projection, case)
    expect_equal(fitted$avg, expected$avg, tolerance = 1e-10)
    expect_equal(sum(case$mass * (fitted$avg - case$avg)^2), expected$objective, tolerance = 1e-12)
    expect_equal(as.numeric(rowsum(case$mass * fitted$avg, case$group)),
      as.numeric(rowsum(case$mass * case$avg, case$group)), tolerance = 1e-12)
    expect_true(all(diff(fitted$avg) >= -1e-12))
    expect_identical(fitted, do.call(finished_dina_join_projection, case))
  }
  expect_error(finished_dina_join_projection(c(1, 3, 2, 4), rep(.25, 4), c(1, 1, 2, 2),
    max_iterations = 1L), "did not converge")
})
