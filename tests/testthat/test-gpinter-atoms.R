gpinter_atoms_test_engine <- function() {
  e <- new.env(parent = globalenv())
  source(file.path(repo_root_for_tests, "code/R/gpinter_atoms.R"), local = e)
  e
}

gpinter_atoms_fixture <- function() {
  data.frame(p = c(0, .2, .35, .5, .6, .8, .95),
    avg = c(1, 2, 3, 3, 5, 10, 30), thr = c(0, 1, 3, 3, 4, 8, 20), average = 1)
}

test_that("an interior atom retains exact incomes and all original population anchors", {
  e <- gpinter_atoms_test_engine()
  d <- gpinter_atoms_fixture()
  grid <- sort(unique(round(c(seq(0, .99, .01), .999, d$p), 12)))
  tab <- e$gpinter_atoms_tabulate(d, grid)
  atom <- attr(tab, "atoms")
  expect_equal(atom[c("p_start", "p_end", "income")],
               data.frame(p_start = .35, p_end = .6, income = 3))
  expect_equal(tab$threshold[grid >= .35 & grid < .6], rep(3, sum(grid >= .35 & grid < .6)))
  inside <- grid >= .35 & c(grid[-1L], 1) <= .6
  expect_equal(tab$bracket_average[inside], rep(3, sum(inside)), tolerance = 1e-10)
  original <- attr(tab, "original_tabulation")
  expect_equal(original$bracket_average, d$avg, tolerance = 1e-8)
  income <- diff(c(d$p, 1)) * d$avg
  expect_equal(original$bracket_share, income / sum(income), tolerance = 1e-10)
  expect_equal(original$top_share, rev(cumsum(rev(income))) / sum(income), tolerance = 1e-10)
  expect_equal(tab$top_share[grid == .5], sum(income[d$p >= .5]) / sum(income), tolerance = 1e-10)
  expect_identical(attr(tab, "original"), d)
  expect_error(e$gpinter_atoms_tabulate(d, c(0, .35, .35 + 1e-15, .5)),
               "numerically duplicate population boundaries")
})

test_that("roundoff equalities do not turn into an artificial income slope", {
  e <- gpinter_atoms_test_engine()
  d <- gpinter_atoms_fixture()
  d$avg[4] <- d$avg[4] - 5e-15
  tab <- e$gpinter_atoms_tabulate(d, d$p)
  expect_equal(attr(tab, "atoms")$population, .25)
  expect_true(attr(tab, "atoms")$mean_roundoff < 1e-13)
  expect_equal(tab$threshold[3], tab$threshold[4], tolerance = 0)
  expect_equal(tab$bracket_average, d$avg, tolerance = 1e-8)
})

test_that("zero and positive atoms can coexist without changing either mass", {
  e <- gpinter_atoms_test_engine()
  d <- gpinter_atoms_fixture()
  d <- rbind(data.frame(p = 0, avg = 0, thr = 0, average = 1), transform(d, p = .05 + .95 * p))
  tab <- e$gpinter_atoms_tabulate(d, sort(unique(c(d$p, .01, .02, .5))))
  expect_equal(nrow(attr(tab, "atoms")), 2L)
  expect_equal(tab$threshold[1:3], c(0, 0, 0))
  expect_equal(attr(tab, "original_tabulation")$bracket_average, d$avg, tolerance = 1e-8)
})

test_that("a tiny positive bin retains its income beside large tails and atoms", {
  e <- gpinter_atoms_test_engine()
  # CHL2024 exposed cancellation after a zero mass: the first positive bin's
  # income is below one currency unit, while cumulative income is millions.
  # This smaller independent fixture also failed the original anchor check.
  d <- data.frame(p = c(0, .24, .25, .35, .5, .51, .52, .8, .95),
    avg = c(0, 50, 141347, 1e6, 8649938, 8649938, 1e7, 2e7, 1e9), thr = 0)
  grid <- sort(c(d$p, .245))
  tab <- e$gpinter_atoms_tabulate(d, grid)
  original <- attr(tab, "original_tabulation")
  expect_true(all(abs(original$bracket_average - d$avg) <=
    1e-8 * pmax(1, abs(d$avg))))
  expect_lt(abs(original$bracket_average[2] - 50), 1e-7)
  income <- diff(c(d$p, 1)) * d$avg
  expect_equal(original$top_share, rev(cumsum(rev(income))) / sum(income), tolerance = 1e-12)
  tiny <- grid >= .24 & grid < .25
  expect_equal(sum(tab$bracket_average[tiny] * diff(c(grid, 1))[tiny]), .5, tolerance = 1e-8)
  expect_equal(sum(tab$bracket_share), 1, tolerance = 1e-12)
  expect_equal(attr(tab, "atoms")$income, c(0, 8649938))
  expect_identical(attr(tab, "original"), d)
})

test_that("substantive reversals and unsupported atom arrangements fail explicitly", {
  e <- gpinter_atoms_test_engine()
  d <- gpinter_atoms_fixture()
  d$avg[4] <- 2.99
  expect_error(e$gpinter_atoms_tabulate(d, d$p), "decreasing bracket means")
  d <- gpinter_atoms_fixture()
  d$avg[6:7] <- 30
  expect_error(e$gpinter_atoms_tabulate(d, d$p), "terminal constant-income mass")
  d <- gpinter_atoms_fixture()
  d$avg[1:2] <- 1
  expect_error(e$gpinter_atoms_tabulate(d, d$p), "adjacent masses")
  d <- gpinter_atoms_fixture()
  d$avg <- c(1, 2, 3, 4, 5, 10, 30)
  expect_error(e$gpinter_atoms_tabulate(d, d$p), "no constant-income population mass")
})
