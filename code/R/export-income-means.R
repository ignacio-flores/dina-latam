#!/usr/bin/env Rscript
# Export-only monetary benchmarks. Distribution shapes remain untouched.
# Disposable income uses its constructed aggregate relative to pretax national
# income, then applies the common WID national-income benchmark once.
export_income_mean_ratios <- function(grouped, targets, unit, countries, observed = NULL) {
  required <- c("country", "year", "step", "unit", "variable", "group", "value")
  if (!all(required %in% names(grouped)) ||
      !all(c("country", "year", "step") %in% names(targets))) stop("Missing income-mean fields.")
  targets <- unique(as.data.frame(targets[c("country", "year", "step")]))
  if (anyNA(targets) || any(!targets$step %in% c("nat", "pon", "pod"))) stop("Invalid export income concepts.")
  targets$income_mean_ratio <- 1
  if (!any(targets$step == "pod")) return(targets)
  x <- grouped[grouped$unit == unit & grouped$variable == "average" &
    grouped$group == "tot" & grouped$step %in% c("nat", "pod"), , drop = FALSE]
  key <- function(z) paste(z$country, z$year, z$step, sep = "/")
  if (anyDuplicated(key(x))) stop("Duplicate country-year income means.")
  x$value <- suppressWarnings(as.numeric(x$value))
  if (any(!is.finite(x$value) | x$value <= 0)) stop("Income means must be finite and positive.")
  national <- x[x$step == "nat", c("country", "year", "value")]
  disposable <- x[x$step == "pod", c("country", "year", "value")]
  if (!is.null(observed)) {
    observed <- observed[observed$step == "pod", c("country", "year"), drop = FALSE]
    disposable <- merge(observed, disposable, by = c("country", "year"), all.x = TRUE)
    if (any(!is.finite(disposable$value) | disposable$value <= 0)) stop("Missing disposable mean for an exported observation.")
  }
  pairs <- merge(disposable, national, by = c("country", "year"), all.x = TRUE,
    suffixes = c("_pod", "_nat"))
  if (!nrow(pairs) || any(!is.finite(pairs$value_nat) | pairs$value_nat <= 0)) {
    stop("Disposable income requires a matching internal national-income mean for every calculated year.")
  }
  pairs$ratio <- pairs$value_pod / pairs$value_nat
  if (any(!is.finite(pairs$ratio) | pairs$ratio <= 0)) stop("Invalid disposable-to-national income ratio.")
  years <- sort(unique(targets$year[targets$step == "pod"]))
  donors <- list()
  for (country in countries) {
    z <- pairs[pairs$country == country, ]; z <- z[order(z$year), ]
    if (!nrow(z)) stop("Missing disposable/national means for ", country, ". Run its pod statistics first.")
    # Match 07d's linear interpolation and constant endpoint carry-forward.
    donors[[country]] <- if (nrow(z) == 1L) rep(z$ratio, length(years)) else
      stats::approx(z$year, z$ratio, xout = years, rule = 2, ties = "ordered")$y
  }
  for (country in unique(targets$country[targets$step == "pod"])) {
    pool <- if (country %in% countries) country else if (country == "CUB")
      c("ECU", "URY", "ARG") else setdiff(countries, "CRI")
    if (!length(pool) || !all(pool %in% names(donors))) stop("Incomplete regional income-mean donor pool for ", country)
    ratio <- rowMeans(do.call(cbind, donors[pool]))
    take <- targets$country == country & targets$step == "pod"
    targets$income_mean_ratio[take] <- ratio[match(targets$year[take], years)]
  }
  if (any(!is.finite(targets$income_mean_ratio) | targets$income_mean_ratio <= 0)) stop("Incomplete export income-mean ratios.")
  targets
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 5L) stop("Expected grouped statistics, target keys, export request, output DTA and observed cases.")
  request <- readLines(args[3L], warn = FALSE)
  if (length(request) != 7L) stop("Incomplete export settings request.")
  countries <- strsplit(trimws(gsub('"', '', request[5L], fixed = TRUE)), "[[:space:]]+")[[1L]]
  result <- export_income_mean_ratios(utils::read.csv(args[1L], stringsAsFactors = FALSE),
    utils::read.csv(args[2L], stringsAsFactors = FALSE), request[2L], countries, utils::read.csv(args[5L], stringsAsFactors = FALSE))
  haven::write_dta(result, args[4L], version = 14)
  cat("Validated", sum(result$step == "pod"), "disposable income mean ratios.\n")
}
