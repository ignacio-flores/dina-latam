gpinter_threshold_test_engine <- function() {
  env <- new.env(parent = globalenv())
  suppressPackageStartupMessages(library(dplyr))
  source(file.path(repo_root_for_tests, "code/R/gpinter_precision.R"), local = env)
  source(file.path(repo_root_for_tests, "code/R/gpinter_thresholds.R"), local = env)
  definitions <- c("thr_digits", "avg_digits", "tol", "p_grid", "collapse_and_clean",
                   "sanitize_eps", "strict_margin", "enforce_avg_strictly_inside")
  for (expr in parse(file.path(repo_root_for_tests, "code/R/07c_interpolate_ineqstats.R"))) {
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        is.symbol(expr[[2L]]) && as.character(expr[[2L]]) %in% definitions) eval(expr, env)
  }
  env$prepare <- function(d) env$gpinter_threshold_prepare(d, env$tol, env$strict_margin,
    env$collapse_and_clean, env$enforce_avg_strictly_inside)
  env$fit_shares <- function(prepared) env$gpinter_threshold_fit_shares(prepared$data)
  env$tabulate <- function(dist, p) gpinter::generate_tabulation(dist, fractiles = p,
    threshold = TRUE, bracketavg = TRUE, bracketshare = TRUE, topavg = TRUE, topshare = TRUE)
  env
}

gpinter_threshold_fixture <- function() {
  # Original boundaries deliberately differ from the export grid.
  data.frame(p = c(0, .4, .73, .991), thr = c(0, 3, 3.9, 9),
             avg = c(.5, 2, 4, 10), average = 999)
}

test_that("GPinter reconstructs thresholds without changing original means or populations", {
  e <- gpinter_threshold_test_engine()
  d <- gpinter_threshold_fixture()
  prepared <- e$prepare(d)
  expect_equal(prepared$method, "shares_fit")
  expect_equal(prepared$data$p, d$p)
  expect_equal(prepared$data$avg, d$avg)
  expected_income <- d$avg * diff(c(d$p, 1))
  expect_equal(prepared$data$average, rep(sum(expected_income), nrow(d)))
  dist <- e$fit_shares(prepared)
  tab <- e$tabulate(dist, e$p_grid)
  expect_true(e$gpinter_threshold_validate_fit(dist, tab, prepared))
  original_tab <- e$tabulate(dist, d$p)
  expect_true(max(abs(original_tab$bracket_average - d$avg)) < 1e-8)
  expect_true(max(abs(original_tab$top_share - rev(cumsum(rev(expected_income))) / sum(expected_income))) < 1e-8)
  changes <- e$gpinter_threshold_changes(dist, prepared)
  expect_true(nrow(changes) > 0)
  expect_true(all(changes$rule == "gpinter_threshold_reconstruction"))
  expect_equal(changes$bracket_mean, d$avg[match(changes$p, d$p)])
  expect_true(all(nzchar(changes$reason)))
})

test_that("legacy mean sanitation triggers reconstruction instead of changing income", {
  e <- gpinter_threshold_test_engine()
  d <- data.frame(p = c(0, .5, .9, .99), thr = c(0, 1, 1.5, 10),
                  avg = c(.5, 2, 8, 20), average = 1)
  prepared <- e$prepare(d)
  expect_equal(prepared$method, "shares_fit")
  expect_equal(prepared$data$avg, d$avg)
  expect_match(prepared$reason, "would change bracket income")
})

test_that("collapsing distinct descending thresholds cannot discard an original income anchor", {
  e <- gpinter_threshold_test_engine()
  d <- data.frame(p = c(0, .5, .9), thr = c(0, 5, 4),
                  avg = c(2, 6, 10), average = 1)
  prepared <- e$prepare(d)
  expect_equal(prepared$method, "shares_fit")
  expect_equal(prepared$data$p, d$p)
  expect_equal(prepared$data$avg, d$avg)
  expect_match(prepared$reason, "discard an original population boundary")
  dist <- e$fit_shares(prepared)
  expect_true(e$gpinter_threshold_validate_fit(dist, e$tabulate(dist, e$p_grid), prepared))
  original_tab <- e$tabulate(dist, d$p)
  expect_equal(original_tab$bracket_average, d$avg, tolerance = 1e-8)
})

test_that("ordinary valid thresholds and PER2003 precision preparation retain their method", {
  e <- gpinter_threshold_test_engine()
  ordinary <- data.frame(p = c(0, .5, .9, .99), thr = c(0, 1, 5, 10),
                         avg = c(.5, 2, 8, 20), average = 1)
  ordinary_prepared <- e$prepare(ordinary)
  expect_equal(ordinary_prepared$method, "tabulation_fit")
  expect_equal(ordinary_prepared$data$avg, ordinary$avg)
  expect_equal(ordinary_prepared$data$thr, ordinary$thr)
  d <- data.frame(p = c(0, .99, .999, .9999),
    thr = c(0, 100000, 1536133.273634, 1536133.273634),
    avg = c(10000, 122000, 1536133.25, 1536133.25), average = 1)
  prepared <- e$prepare(d)
  expect_equal(prepared$method, "tabulation_fit")
  expect_equal(nrow(prepared$adjustments), 2L)
  expect_true(.999 %in% prepared$data$p)
  # The existing precision path also consolidates this float-storage reversal
  # at exactly tied thresholds; shares_fit must not introduce new pooling.
  d$thr[3:4] <- 1604604.26
  d$avg[3:4] <- c(1604604.388888889, 1604604.25)
  prepared <- e$prepare(d)
  expect_equal(prepared$method, "tabulation_fit")
  expect_equal(sum(prepared$data$avg * diff(c(prepared$data$p, 1))),
               sum(d$avg * diff(c(d$p, 1))))
})

test_that("reversed or tied means cannot be silently changed for shares fitting", {
  e <- gpinter_threshold_test_engine()
  d <- gpinter_threshold_fixture()
  d$avg[2:3] <- c(4, 2)
  expect_error(e$prepare(d), "Decreasing bracket means")
  d <- gpinter_threshold_fixture()
  d$avg[2:3] <- c(2, 2)
  expect_error(e$prepare(d), "strictly increasing original bracket means")
  d <- gpinter_threshold_fixture()
  d$avg[2:3] <- c(2, 2 - 1e-10)
  expect_error(e$prepare(d), "Decreasing bracket means")
})

test_that("leading exact-zero bins retain every original income anchor", {
  e <- gpinter_threshold_test_engine()
  # Actual PER2005 nat/pch inputs after the unchanged tail rescaling.
  # Keep this small numerical fixture independent of experimental output files.
  d <- data.frame(
    p = c(0, 0.01, 0.02, 0.03, 0.04,
      0.05, 0.06, 0.07, 0.08, 0.09,
      0.1, 0.11, 0.12, 0.13, 0.14,
      0.15, 0.16, 0.17, 0.18, 0.19,
      0.2, 0.21, 0.22, 0.23, 0.24,
      0.25, 0.26, 0.27, 0.28, 0.29,
      0.3, 0.31, 0.32, 0.33, 0.34,
      0.35, 0.36, 0.37, 0.38, 0.39,
      0.4, 0.41, 0.42, 0.43, 0.44,
      0.45, 0.46, 0.47, 0.48, 0.49,
      0.5, 0.51, 0.52, 0.53, 0.54,
      0.55, 0.56, 0.57, 0.58, 0.59,
      0.6, 0.61, 0.62, 0.63, 0.64,
      0.65, 0.66, 0.67, 0.68, 0.69,
      0.7, 0.71, 0.72, 0.73, 0.74,
      0.75, 0.76, 0.77, 0.78, 0.79,
      0.8, 0.81, 0.82, 0.83, 0.84,
      0.85, 0.86, 0.87, 0.88, 0.89,
      0.9, 0.91, 0.92, 0.93, 0.94,
      0.95, 0.96, 0.97, 0.98, 0.99,
      0.999, 0.9999),
    thr = c(0, 0, 0, 132.525291134121, 293.92761156296,
      390.226764361615, 466.131981449872, 549.626922744672, 627.306235501183, 696.183099678278,
      767.506822463563, 843.973790498137, 918.188184140752, 980.62315849635, 1038.51142897288,
      1104.36586139141, 1169.98417259307, 1227.73384690922, 1295.74461239287, 1357.24326266551,
      1423.44954686914, 1495.89480178884, 1556.35335430268, 1621.27436806325, 1678.15458104522,
      1745.84038501911, 1804.66437178883, 1873.90813961302, 1930.72477772429, 2005.40267979021,
      2069.12381814191, 2135.40966054294, 2206.86545159259, 2268.70367272752, 2325.9938788396,
      2404.50039129339, 2463.33515586779, 2526.82757097988, 2611.9346892184, 2696.33667158444,
      2770.33937492045, 2846.43152310076, 2934.92906344131, 3030.33264796391, 3110.83555303641,
      3182.54783216473, 3270.76814438858, 3366.46940416398, 3456.27393112156, 3538.03724212404,
      3639.08431694879, 3751.52168359064, 3848.39228487242, 3952.92488282702, 4057.248808543,
      4157.39175323394, 4272.82669273017, 4414.75848574942, 4522.63084818317, 4614.19867876489,
      4751.43991664935, 4861.26212946977, 4987.09153121789, 5141.29997411942, 5278.67416486975,
      5408.40643619327, 5583.85197870476, 5712.27979037179, 5869.06205124122, 6032.49987276467,
      6201.63870421844, 6381.00213085617, 6591.05952307247, 6826.07362119664, 7058.84126334597,
      7288.21419961582, 7548.85522758704, 7861.67632644905, 8142.75420264694, 8475.70333600018,
      8832.18635772616, 9263.28127097032, 9595.08155330932, 10044.4068039461, 10645.8096905025,
      11235.4452217174, 11900.7529640321, 12674.8096587524, 13478.745833723, 14559.6867550439,
      16027.5683904727, 17930.0713886652, 19826.5433936043, 22547.3893319939, 26219.7909203515,
      31130.2358966598, 36154.1554102508, 43884.5392408174, 57915.8665762687, 84106.0670928767,
      233742.217685221, 380626.933925744),
    avg = c(0, 0, 14.7273468175791, 233.949990197881, 342.244962265261,
      432.723348247476, 511.54609259281, 586.301062633154, 665.167402198066, 732.174304327294,
      806.875649926494, 880.61490020078, 949.772061040855, 1010.29838285101, 1075.93352722324,
      1133.80273796096, 1197.49197682601, 1260.8534837529, 1326.26284908728, 1389.36834957455,
      1459.55550342208, 1525.20070091256, 1590.40865830526, 1646.75817539476, 1713.30137499616,
      1777.69227556566, 1839.61177089297, 1900.58551354089, 1967.61219880127, 2035.10918850344,
      2103.66066293997, 2175.24247269166, 2239.00333489773, 2297.90518656657, 2363.64654059709,
      2436.08526322451, 2495.50161336695, 2568.45492923128, 2655.10830388839, 2729.38507643327,
      2812.2525842042, 2890.154620546, 2984.00960388266, 3069.41242192421, 3148.72783597964,
      3224.90343932258, 3317.91856043539, 3413.15971802182, 3499.87275035868, 3583.23109338698,
      3695.9885962864, 3803.18903831004, 3902.37695780411, 4003.8589508311, 4107.68134870016,
      4219.28289482288, 4347.37894786553, 4468.93900985959, 4569.95258175319, 4683.06601918373,
      4806.63460475594, 4925.96539885588, 5060.17221239971, 5208.20099600566, 5340.577710403699,
      5495.78628459727, 5657.83013249638, 5780.25246499276, 5944.93062647421, 6112.36789455371,
      6288.82960855885, 6489.66749903267, 6707.46435242014, 6929.10521924172, 7174.13448973601,
      7420.07445265101, 7721.0546337524, 7999.47424427064, 8307.43797120105, 8665.55524594631,
      9049.44238741954, 9423.30911308633, 9821.39262380587, 10328.1599056325, 10957.576244059,
      11550.1834205623, 12250.2786521242, 13039.2024222481, 13974.6337009662, 15188.2851367665,
      15574.59898414855, 17398.161265981926, 19429.10531247438, 22358.3365780613, 26468.870804384045,
      30978.201004942057, 35954.14296268345, 45850.29086916418, 65418.83398412849, 129100.45815788956,
      325476.7538352173, 3266676.3046761593),
    average = 1)
  prepared <- e$prepare(d)
  expect_equal(prepared$method, "shares_fit")
  expect_equal(prepared$original$p, d$p)
  expect_equal(prepared$original$avg, d$avg)
  expect_equal(prepared$data$p, d$p[-2L])
  expect_equal(prepared$data$avg, d$avg[-2L])
  expect_match(prepared$reason, "one zero-income atom")
  dist <- e$fit_shares(prepared)
  expect_true(e$gpinter_threshold_validate_fit(dist, e$tabulate(dist, e$p_grid), prepared))
  original_tab <- e$tabulate(dist, d$p)
  expect_equal(original_tab$threshold[1:2], c(0, 0))
  expect_true(all(abs(original_tab$bracket_average[1:2]) < 1e-8))
  expect_true(all(abs(original_tab$bracket_share[1:2]) < 1e-8))
  expect_equal(original_tab$bracket_average, d$avg, tolerance = 1e-8)
  # Representing the zero atom cannot bypass the package's minimum grid.
  d <- data.frame(p = c(0, .01, .02), thr = c(0, 0, 11),
                  avg = c(0, 0, 10), average = 1)
  expect_error(e$prepare(d), "at least three fitting population boundaries")
})

test_that("invalid population or income inputs fail before fitting", {
  e <- gpinter_threshold_test_engine()
  d <- gpinter_threshold_fixture()
  d$p[1] <- .1
  expect_error(e$prepare(d), "beginning at p=0")
  d <- gpinter_threshold_fixture()
  d$avg[1] <- -1
  expect_error(e$prepare(d), "Negative bracket means")
  d <- gpinter_threshold_fixture()
  d$thr[2] <- Inf
  expect_error(e$prepare(d), "finite percentile boundaries")
  d <- gpinter_threshold_fixture()[c(1, 3, 2, 4), ]
  expect_error(e$prepare(d), "boundaries are not reordered")
})

test_that("validation rejects changed income at a non-export original boundary", {
  e <- gpinter_threshold_test_engine()
  prepared <- e$prepare(gpinter_threshold_fixture())
  altered <- prepared
  altered$data$avg[2] <- altered$data$avg[2] + .01
  altered$data$avg[3] <- altered$data$avg[3] - .01 * .33 / .261
  dist <- e$fit_shares(altered)
  tab <- e$tabulate(dist, e$p_grid)
  expect_error(e$gpinter_threshold_validate_fit(dist, tab, prepared),
               "did not preserve original bracket means")
})

test_that("validation rejects corrupted or non-finite exported statistics", {
  e <- gpinter_threshold_test_engine()
  prepared <- e$prepare(gpinter_threshold_fixture())
  dist <- e$fit_shares(prepared)
  tab <- e$tabulate(dist, e$p_grid)
  tab$top_share[2] <- tab$top_share[2] + .01
  expect_error(e$gpinter_threshold_validate_fit(dist, tab, prepared), "consistent bracket and cumulative income")
  tab <- e$tabulate(dist, e$p_grid)
  tab$threshold[2] <- NA_real_
  expect_error(e$gpinter_threshold_validate_fit(dist, tab, prepared), "non-finite fitted statistics")
})

test_that("known zero-income atoms remove cumulative subtraction noise only", {
  e <- gpinter_threshold_test_engine()
  original <- data.frame(p = c(0, .01, .02, .5, .9),
    avg = c(0, 0, 500000, 1000000, 3000000), thr = c(0, 0, 0, 600000, 2000000))
  overall <- sum(diff(c(original$p, 1)) * original$avg)
  dist <- gpinter::shares_fit(p = original$p[-2L], bracketavg = original$avg[-2L],
                             average = overall, first_threshold = 0)
  tab <- e$tabulate(dist, original$p)
  # Reproduce a two-ULP cumulative-share error and its implied negative mean.
  tab$top_share[2L] <- 1 + 2 * .Machine$double.eps
  tab$bracket_share <- -diff(c(tab$top_share, 0))
  tab$bracket_average <- tab$bracket_share * overall / diff(c(original$p, 1))
  tab$top_average <- tab$top_share * overall / (1 - original$p)
  expect_lt(tab$bracket_average[1L], 0)
  fixed <- e$gpinter_threshold_zero_atom(tab, original)
  expect_identical(fixed$bracket_average[1:2], c(0, 0))
  expect_identical(fixed$bracket_share[1:2], c(0, 0))
  expect_identical(fixed$top_share[1:3], c(1, 1, 1))
  expect_identical(fixed$bracket_average[3:5], tab$bracket_average[3:5])
  expect_identical(fixed$bracket_share[3:5], tab$bracket_share[3:5])
  expect_true(e$gpinter_threshold_check_tab(fixed, overall, "Test grid"))
  corrupt <- tab; corrupt$top_share[2L] <- .9
  expect_error(e$gpinter_threshold_zero_atom(corrupt, original), "differs substantively")
  corrupt <- tab; corrupt$bracket_average[1L] <- NA_real_
  expect_error(e$gpinter_threshold_zero_atom(corrupt, original), "non-finite")
  no_atom <- original; no_atom$avg[1L] <- 1
  expect_identical(e$gpinter_threshold_zero_atom(tab, no_atom), tab)
})
