consistency_engine <- function() {
  e <- new.env(parent = globalenv())
  sys.source(file.path(repo_root_for_tests, "code/R/consistency_checker.R"), e)
  e
}

consistency_fixture <- function(e, country = "AAA", step = "nat", shape = "valid") {
  g <- e$dina_consistency_grid(); a <- 1 + 4 * g$p
  if (shape == "interior") a[g$p >= .34 & g$p < .38] <- 2.42
  if (shape == "join") a[g$p >= .48 & g$p < .52] <- 3
  if (shape == "tail") a[g$p >= .999] <- 5
  if (shape == "zeros") a[g$p < .2] <- 0
  t <- c(0, (head(a, -1L) + tail(a, -1L)) / 2)
  if (shape %in% c("interior", "join", "tail")) {
    same <- c(FALSE, diff(a) == 0)
    t[same] <- a[same]
  }
  t[a == 0] <- 0
  mu <- sum(a * g$mass); a <- a / mu; t <- t / mu
  v <- e$dina_consistency_derived(a, g$mass)
  data.frame(country, year = 2024L, step, p = round(g$p * 100000), bracketpop = g$mass * 100000,
    bracketavg_norm = a, bracketavg = a, thr = t, s = v$s, topavg = v$topavg, topsh = v$topsh,
    bottomsh = v$bottomsh, bottomavg = v$bottomavg, consistency_publish = 1L,
    pub_source = "citation retained", stringsAsFactors = FALSE)
}

consistency_long_fixture <- function(e, wide, benchmark = 12345) {
  g <- e$dina_consistency_grid(); output <- list(); scaled <- list()
  for (key in unique(e$dina_consistency_key(wide))) {
    d <- wide[e$dina_consistency_key(wide) == key, ]; d <- d[order(d$p), ]
    income <- benchmark * d$bracketavg_norm
    derived <- e$dina_consistency_derived(income, g$mass)
    code <- e$dina_consistency_code(d$step[1L])
    row <- function(kind, labels, values) data.frame(country = d$country[1L], year = d$year[1L],
      p = labels, widcode = paste0(kind, code, "992j"), value = values, stringsAsFactors = FALSE)
    shares <- derived$s
    bottom <- list(p0p20 = g$p < .2, p0p50 = g$p < .5, p0p90 = g$p < .9,
      p50p90 = g$p >= .5 & g$p < .9)
    z <- rbind(row("a", g$label, income), row("s", g$label, shares), row("t", g$label, d$thr * benchmark),
      row("a", g$top, derived$topavg), row("s", g$top, derived$topsh),
      row("s", names(bottom), vapply(bottom, function(ii) sum(shares[ii]), numeric(1))),
      row("a", names(bottom)[-1L], vapply(bottom[-1L], function(ii) sum(income[ii] * g$mass[ii]) / sum(g$mass[ii]), numeric(1))))
    z <- z[!duplicated(z[c("country", "year", "p", "widcode")]), ]
    output[[key]] <- z
    d$p <- d$p / 1000; d$bracketavg <- income; d$thr <- d$thr * benchmark
    d$s <- shares; d$anninc992i <- benchmark; d$income_mean <- benchmark
    scaled[[key]] <- d
  }
  list(long = do.call(rbind, output), wide = do.call(rbind, scaled))
}

test_that("local smoothing preserves population, income and valid or zero-income groups", {
  e <- consistency_engine(); g <- e$dina_consistency_grid()
  for (shape in c("valid", "interior", "join", "tail", "zeros")) {
    input <- consistency_fixture(e, shape = shape)
    r <- e$dina_consistency_prepare(input, "sources")
    out <- r$data
    expect_identical(out$bracketpop, input$bracketpop)
    expect_equal(sum(out$bracketavg_norm * g$mass), sum(input$bracketavg_norm * g$mass), tolerance = 1e-14)
    expect_identical(out$pub_source, input$pub_source)
    expect_equal(e$dina_consistency_ties(out$bracketavg_norm), 0)
    expect_equal(e$dina_consistency_ties(out$thr), 0)
    expect_true(all(diff(out$bracketavg_norm) >= 0))
    expect_true(all(diff(out$thr) >= 0))
    expect_true(all(out$bracketavg_norm >= out$thr))
    expect_true(all(head(out$bracketavg_norm, -1L) <= tail(out$thr, -1L)))
    expect_lte(max(abs(out$bracketavg_norm - input$bracketavg_norm) /
      pmax(abs(input$bracketavg_norm), .Machine$double.xmin)), 1e-6 + 1e-14)
    twice <- e$dina_consistency_prepare(out, "assembled")
    expect_identical(twice$data$bracketavg_norm, out$bracketavg_norm)
    expect_identical(twice$data$thr, out$thr)
    if (shape %in% c("valid", "zeros")) {
      expect_identical(out$bracketavg_norm, input$bracketavg_norm)
      expect_identical(out$thr, input$thr)
    }
  }
})

test_that("tiny distinct positive incomes are not incorrectly classified as equal", {
  e <- consistency_engine()
  a <- c(1e-20, 2e-20, 4e-20, 1, 2)
  t <- c(0, 1.5e-20, 3e-20, .5, 1.5)
  r <- e$dina_consistency_repair(a, t, rep(.2, 5))
  expect_identical(r$avg, a)
  expect_identical(r$thr, t)
})

test_that("noisy observed plateaus are repaired within the per-mean cap and survive serialization", {
  skip_if_not_installed("haven")
  e <- consistency_engine()
  # Representative consecutive brackets from the 27 October delivery. These
  # contain floating-point declines, not merely perfectly repeated means.
  cases <- list(
    BRA_2003 = list(
      a = c(17741.4802058731, 17741.6740258104, 17741.6740258104,
        17741.6740258097, 17741.6740258104, 18004.0476156102),
      t = c(17578.393532077, rep(17741.6740258101, 5)), m = rep(.01, 6)),
    BRA_2005 = list(
      a = c(20024.20314729679, 20024.252852794525, 20024.252852795147, 20033.307897088824),
      t = c(0, 20024.246737174526, 20024.2528527947, 20024.258968415146),
      m = rep(.01, 4)),
    CRI_2016 = list(
      a = c(907387603.528568, 992019951.615707, 992019951.615594,
        992019951.615767, 992019951.615594, 992019951.615654,
        992019951.615707, 992019951.615594, 992019951.615654,
        992019951.615707, 992019951.615654),
      t = c(830233722.356008, 992019949.631619, rep(992019951.615659, 9)),
      m = c(.0001, rep(.00001, 10))),
    PER_2003 = list(
      a = c(2452121.32079195, 3085589.12985729, 3085589.12985822,
        3085589.12985824, 3085589.12985822, 3085589.12985824,
        3085589.12985822, 3085589.12985824, 3085589.12985822,
        3085589.12985822, 3085589.1298585, 3085589.1298582,
        3085589.129858, 3085589.1298585, 3085589.129858,
        3085589.1298582, 3085589.1298582, 3085589.12985831,
        3085589.1298582, 3085589.12985831),
      t = c(1826129.72555109, 3085588.83267397, rep(3085589.12985822, 18)),
      m = c(.001, rep(.0001, 9), rep(.00001, 10))),
    ARG_2003 = list(
      a = c(1003046856.7657772, 1003046857.2821498, 1003046857.2847052,
        1003046857.2847462, 1003046857.2845447, 1003046857.2847462),
      t = c(1003046854.7451982, 1003046857.2698947, 1003046857.2846477,
        rep(1003046857.2846752, 3)), m = rep(.00001, 6)),
    ARG_2007 = list(
      a = c(1005052843.6757238, 1005052843.9547408, 1005052843.9560541,
        1005052843.9560763, 1005052843.9560392, 1005052843.9560392,
        1005052843.9560243, 1005052843.9560392, 1005052843.9560243,
        1005052843.9560392, 1005052843.9562618, 1005052843.9560243,
        1005052843.9560392, 1005052843.9560243),
      t = c(1005052842.603359, 1005052843.9481012, 1005052843.9560467,
        rep(1005052843.9560616, 11)), m = c(rep(.0001, 4), rep(.00001, 10))))
  path <- tempfile(fileext = ".dta"); on.exit(unlink(path), add = TRUE)
  for (x in cases) {
    expect_true(any(diff(x$a) < 0) || e$dina_consistency_ties(x$a, numerical = TRUE) > 0)
    r <- e$dina_consistency_repair(x$a, x$t, x$m)
    expect_equal(e$dina_consistency_ties(r$avg, numerical = TRUE), 0)
    expect_equal(e$dina_consistency_ties(r$thr, numerical = TRUE), 0)
    for (values in list(r$avg, r$thr)) {
      expect_true(all(diff(values) > 1e-12 * pmax(abs(head(values, -1L)), abs(tail(values, -1L)))))
      expect_true(all(diff(round(values, 5)) > 0))
    }
    expect_true(all(diff(r$avg) > 0))
    expect_true(all(diff(r$thr) > 0))
    expect_true(all(r$thr <= r$avg))
    expect_true(all(head(r$avg, -1L) <= tail(r$thr, -1L)))
    expect_lte(max(abs(r$avg - x$a) / x$a), 1e-6)
    expect_lte(abs(sum(r$avg * x$m) / sum(x$a * x$m) - 1), 128 * .Machine$double.eps)
    expect_identical(r$avg[1L], x$a[1L])
    haven::write_dta(data.frame(a = r$avg, t = r$thr, m = x$m), path)
    saved <- haven::read_dta(path)
    expect_identical(as.numeric(saved$a), r$avg)
    expect_identical(as.numeric(saved$t), r$thr)
    expect_identical(as.numeric(saved$m), x$m)
    twice <- e$dina_consistency_repair(as.numeric(saved$a), as.numeric(saved$t), as.numeric(saved$m))
    expect_identical(twice$avg, r$avg)
    expect_identical(twice$thr, r$thr)
  }
})

test_that("noisy plateau recognition preserves valid close means and rejects real decreases", {
  e <- consistency_engine(); eps <- .Machine$double.eps
  a <- c(1, 1 + 8192 * eps, 2)
  t <- c(0, (a[1] + a[2]) / 2, 1.5)
  valid <- e$dina_consistency_repair(a, t, rep(1 / 3, 3))
  expect_identical(valid$avg, a)
  expect_identical(valid$thr, t)
  # A neighboring almost-equal mean must join the defective plateau; otherwise
  # the neighbor cap can leave a slope too small to pass the strict audit.
  a <- c(1, 2, 2 + 256 * eps, 2 + 256 * eps, 4)
  t <- c(0, 1.5, 2 + 128 * eps, 2 + 256 * eps, 3)
  repaired <- e$dina_consistency_repair(a, t, rep(.2, 5))
  expect_equal(e$dina_consistency_ties(repaired$avg, numerical = TRUE), 0)
  expect_equal(e$dina_consistency_ties(repaired$thr, numerical = TRUE), 0)
  expect_lte(max(abs(repaired$avg - a) / a), 1e-6)
  expect_identical(e$dina_consistency_repair(repaired$avg, repaired$thr, rep(.2, 5))$avg, repaired$avg)
  expect_error(e$dina_consistency_repair(c(1, 2, 2 * (1 - 1e-10), 3),
    c(0, 1.5, 2, 2.5), rep(.25, 4)), "Decreasing means exceed")
})

test_that("unsupported groups are reported without preventing other corrections", {
  e <- consistency_engine()
  bad <- consistency_fixture(e, "BAD"); bad$bracketavg_norm[50] <- bad$bracketavg_norm[48]
  good <- consistency_fixture(e, "FIX", shape = "tail")
  r <- e$dina_consistency_prepare(rbind(bad, good))
  expect_identical(r$data$bracketavg_norm[r$data$country == "BAD"], bad$bracketavg_norm)
  expect_identical(r$data$bracketavg[r$data$country == "BAD"], bad$bracketavg_norm)
  expect_identical(r$data$thr[r$data$country == "BAD"], bad$thr)
  derived <- e$dina_consistency_derived(bad$bracketavg_norm, bad$bracketpop / 100000)
  expect_identical(r$data$topavg[r$data$country == "BAD"], derived$topavg)
  expect_true(any(r$state$details$status == "unresolved" & r$state$details$country == "BAD"))
  expect_equal(e$dina_consistency_ties(r$data$bracketavg_norm[r$data$country == "FIX"]), 0)
})

test_that("unresolved primitive ordering retains values but still completes valid derived statistics", {
  e <- consistency_engine(); bad <- consistency_fixture(e)
  bad$bracketavg_norm[50] <- bad$bracketavg_norm[48]
  bad$bottomsh <- NULL; bad$bottomavg <- NULL
  bad$bracketavg <- bad$bracketavg * 10000
  r <- e$dina_consistency_prepare(bad, "sources")
  expect_identical(r$data$bracketavg_norm, bad$bracketavg_norm)
  expect_identical(r$data$bracketavg, bad$bracketavg_norm)
  expect_identical(r$data$thr, bad$thr)
  expected <- e$dina_consistency_derived(bad$bracketavg_norm, bad$bracketpop / 100000)
  for (v in c("s", "topavg", "topsh", "bottomsh", "bottomavg"))
    expect_identical(r$data[[v]], expected[[v]])
  expect_true(any(r$state$details$status == "unresolved"))
  bad$bracketavg_norm[1L] <- NA_real_
  incomplete <- e$dina_consistency_prepare(bad)
  expect_true(all(is.na(incomplete$data$bottomsh)))
  expect_true(all(is.na(incomplete$data$bottomavg)))
})

test_that("audit checks complete serialized concepts, aggregates and monetary benchmarks", {
  e <- consistency_engine()
  d <- rbind(consistency_fixture(e, "AAA", "nat"), consistency_fixture(e, "AAA", "pod"))
  files <- consistency_long_fixture(e, d)
  expected <- unique(d[c("country", "year", "step")])
  audit <- e$dina_consistency_audit(files$long, expected = expected, wide = files$wide)
  expect_equal(nrow(audit$details), 0)
  expect_length(audit$groups, 2L)
  altered <- files$long
  altered$value[altered$widcode == "sptinc992j" & altered$p == "p0p50"] <- .99
  audit <- e$dina_consistency_audit(altered, expected = expected, wide = files$wide)
  expect_true("aggregate_identity" %in% audit$details$rule)
  altered <- files$wide; altered$anninc992i <- altered$anninc992i * 1.01
  audit <- e$dina_consistency_audit(files$long, expected = expected, wide = altered)
  expect_true("benchmark_identity" %in% audit$details$rule)
  duplicate <- rbind(files$long, files$long[1L, ])
  audit <- e$dina_consistency_audit(duplicate, expected = expected)
  expect_true("duplicate_records" %in% audit$details$rule)
  almost <- files$long
  means <- which(almost$widcode == "aptinc992j" & almost$p %in% c("p98p99", "p99p99.1"))
  almost$value[means[2L]] <- almost$value[means[1L]] * (1 + 2 * .Machine$double.eps)
  audit <- e$dina_consistency_audit(almost, expected = expected)
  expect_true("indistinguishable_positive_means" %in% audit$details$rule)
})

test_that("missing records and unavailable benchmark values remain visible without aborting", {
  e <- consistency_engine()
  d <- rbind(consistency_fixture(e, "AAA"), consistency_fixture(e, "BBB"))
  files <- consistency_long_fixture(e, d)
  files$long$value[files$long$country == "BBB" & substr(files$long$widcode, 1L, 1L) %in% c("a", "t")] <- NA_real_
  files$wide$anninc992i[files$wide$country == "BBB"] <- NA_real_
  files$wide$bracketavg[files$wide$country == "BBB"] <- NA_real_
  files$wide$thr[files$wide$country == "BBB"] <- NA_real_
  expected <- unique(d[c("country", "year", "step")])
  audit <- e$dina_consistency_audit(files$long, expected = expected, wide = files$wide)
  expect_true("missing_benchmark" %in% audit$details$rule)
  expect_equal(audit$groups[["BBB/2024/nat"]]$missing_averages, 127)
  expect_equal(audit$groups[["BBB/2024/nat"]]$missing_thresholds, 127)
  expect_false(any(audit$details$country == "AAA"))
  removed <- files$long[files$long$country == "AAA", ]
  audit <- e$dina_consistency_audit(removed, expected = expected)
  expect_true("missing_distribution" %in% audit$details$rule)
})

test_that("saved preparation and audit keep values exact and produce concise review artifacts", {
  skip_if_not_installed("haven"); skip_if_not_installed("jsonlite")
  e <- consistency_engine(); temp <- tempfile("consistency-test-")
  dir.create(temp); on.exit(unlink(temp, recursive = TRUE), add = TRUE)
  input <- consistency_fixture(e, shape = "join")
  input$thr[1L] <- -1e-20
  haven::write_dta(input, file.path(temp, "input.dta"))
  work <- file.path(temp, "work")
  e$dina_consistency_prepare_file(file.path(temp, "input.dta"), file.path(temp, "prepared.dta"), work, "sources")
  prepared <- as.data.frame(haven::read_dta(file.path(temp, "prepared.dta")))
  again <- e$dina_consistency_prepare(prepared)$data
  expect_equal(as.numeric(prepared$bracketavg_norm), as.numeric(again$bracketavg_norm), tolerance = 0)
  expect_equal(as.numeric(prepared$thr), as.numeric(again$thr), tolerance = 0)
  e$dina_consistency_prepare_file(file.path(temp, "prepared.dta"), file.path(temp, "assembled.dta"), work, "assembled")
  files <- consistency_long_fixture(e, prepared)
  haven::write_dta(files$long, file.path(temp, "export.dta"))
  haven::write_dta(files$wide, file.path(temp, "wide.dta"))
  before <- tools::md5sum(file.path(temp, "export.dta"))
  receipt <- e$dina_consistency_audit_file(file.path(temp, "export.dta"), work,
    file.path(temp, "reports"), file.path(temp, "review.pdf"), wide = file.path(temp, "wide.dta"))
  expect_identical(receipt$status, "clean")
  expect_identical(tools::md5sum(file.path(temp, "export.dta")), before)
  expect_equal(receipt$counts$repeated_averages_after, 0)
  expect_true(receipt$counts$mean_corrections > 0)
  expect_gt(receipt$counts$max_threshold_change_normalized, 0)
  expect_equal(receipt$counts$threshold_corrections_near_zero, 1)
  expect_true(any(grepl("tiny denominators", readLines(file.path(temp, "reports/summary.txt")))))
  expect_true(all(file.exists(unlist(receipt$reports))))
  expect_true(file.exists(file.path(temp, "reports/receipt.json")))
  expect_lte(length(readLines(file.path(temp, "reports/summary.txt"))), 33)
  expect_true(grepl("%PDF", readChar(file.path(temp, "review.pdf"), 5L)))
})

test_that("regional review reconstructs inherited changes using the recorded donor map only", {
  e <- consistency_engine()
  source <- rbind(consistency_fixture(e, "AAA", shape = "join"), consistency_fixture(e, "BBB"))
  prepared <- e$dina_consistency_prepare(source, "sources")
  regional <- consistency_fixture(e, "RRG")
  regional$bracketavg_norm <- (prepared$data$bracketavg_norm[prepared$data$country == "AAA"] +
    prepared$data$bracketavg_norm[prepared$data$country == "BBB"]) / 2
  regional$thr <- (prepared$data$thr[prepared$data$country == "AAA"] +
    prepared$data$thr[prepared$data$country == "BBB"]) / 2
  assembled <- e$dina_consistency_prepare(rbind(prepared$data, regional))
  states <- list(sources = prepared$state, assembled = assembled$state)
  map <- data.frame(country = "RRG", step = "nat", donor = c("AAA", "BBB"))
  reviewed <- e$dina_consistency_regional_reference(states, map)
  expect_identical(reviewed$assembled, states$assembled)
  expect_identical(reviewed$sources, states$sources)
  snapshot <- reviewed$regional$snapshots[["RRG/2024/nat"]]
  original <- (source$bracketavg_norm[source$country == "AAA"] + source$bracketavg_norm[source$country == "BBB"]) / 2
  expect_identical(snapshot$avg_before, original)
  expect_true(any(reviewed$regional$details$rule == "inherited_donor_correction"))
  expect_false(any(reviewed$regional$details$status == "unresolved"))
  mismatch <- states
  mismatch$assembled$snapshots[["RRG/2024/nat"]]$avg_before[1L] <- 10
  rejected <- e$dina_consistency_regional_reference(mismatch, map)
  expect_true(any(rejected$regional$details$rule == "unresolved_regional_reference"))
  expect_length(rejected$regional$snapshots, 0)
  expect_equal(e$dina_consistency_share_change(.3, .3 + 1e-15), 0)
  expect_gt(e$dina_consistency_share_change(.3, .3 + 1e-10), 0)
  files <- consistency_long_fixture(e, assembled$data)
  audit <- e$dina_consistency_audit(files$long, unique(assembled$data[c("country", "year", "step")]), reviewed)
  temp <- tempfile("regional-review-"); dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE), add = TRUE)
  receipt <- e$dina_consistency_write_report(audit, reviewed, temp, file.path(temp, "review.pdf"), "fixture.dta")
  expect_equal(receipt$counts$inherited_distributions, 1)
  details <- read.csv(file.path(temp, "details.csv"))
  expect_true(any(details$country == "RRG" & details$phase == "regional"))
  expect_true(file.exists(file.path(temp, "review.pdf")))
})

test_that("report links refer to published outputs rather than discarded worker stages", {
  e <- consistency_engine()
  old <- Sys.getenv("DINA_OUTPUT_ROOT", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("DINA_OUTPUT_ROOT") else Sys.setenv(DINA_OUTPUT_ROOT = old), add = TRUE)
  stage <- tempfile("consistency-stage-")
  dir.create(stage); on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  Sys.setenv(DINA_OUTPUT_ROOT = stage)
  expect_identical(e$dina_consistency_public_path(file.path(stage, "output/data_reports/consistency/test/summary.txt")),
    "output/data_reports/consistency/test/summary.txt")
})

test_that("review plots filter small relative changes without changing diagnostics", {
  e <- consistency_engine()
  d <- consistency_fixture(e)
  files <- consistency_long_fixture(e, d)
  audit <- e$dina_consistency_audit(files$long, unique(d[c("country", "year", "step")]))
  case <- d[1, c("country", "year", "step")]
  tiny <- e$dina_consistency_record(case, "headline", "sources", "share", "test", "fixture", .2, .2001, "p0p50")
  large <- e$dina_consistency_record(case, "headline", "sources", "share", "test", "fixture", .2, .2003, "p50p90")
  details <- rbind(tiny, large)
  review <- e$dina_consistency_review(audit, details)
  expect_equal(review$threshold_percent, .1)
  expect_equal(nrow(review$headline), 2L)
  expect_equal(review$material$p, "p50p90")
  expect_equal(review$material$change_percent, .15, tolerance = 1e-12)
  expect_equal(details, rbind(tiny, large))
  expect_equal(nrow(audit$details), 0L)
  zero <- e$dina_consistency_record(case, "headline", "sources", "share", "test", "fixture", 0, .001, "p0p50")
  expect_equal(nrow(e$dina_consistency_review(audit, zero)$material), 1L)
  temp <- tempfile(fileext = ".pdf"); on.exit(unlink(temp), add = TRUE)
  result <- e$dina_consistency_plot(temp, audit, list(), tiny)
  expect_equal(result$plotted_cases, 0L)
  expect_true(file.info(temp)$size > 0)
  result <- e$dina_consistency_plot(temp, audit, list(), details)
  expect_equal(result$plotted_cases, 1L)
})

