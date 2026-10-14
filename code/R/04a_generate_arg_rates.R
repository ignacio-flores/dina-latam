#!/usr/bin/env Rscript

# Rebuild ARG effective PIT rates from the local AFIP tabulations and gpinter.
# Run from the repository root. --output-dir can stage a comparison outside
# the canonical directory; the pipeline uses that directory by default.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) %% 2L || (length(args) &&
    any(!args[seq(1L, length(args), by = 2L)] %in% c("--output-dir", "--compare-ref")))) {
  stop("Usage: Rscript code/R/04a_generate_arg_rates.R [--output-dir DIR] [--compare-ref GIT_REF]", call. = FALSE)
}
options <- if (length(args)) {
  setNames(as.list(args[seq(2L, length(args), by = 2L)]),
           args[seq(1L, length(args), by = 2L)])
} else list()
if (!is.null(options[["--output-dir"]]) && !nzchar(options[["--output-dir"]])) {
  stop("--output-dir must not be empty.", call. = FALSE)
}
for (package in c("gpinter", "haven", "readxl", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

source("code/R/admin_cleaners/gpinter_admin.R")

arg_f32 <- function(x) {
  readBin(writeBin(as.double(x), raw(), size = 4L), what = "numeric",
          n = length(x), size = 4L)
}

arg_source_file <- function(year, name) {
  file.path("input_data", "admin_data", "ARG",
            sprintf("EstadisticasTributarias%d", year), name)
}

arg_distribution <- function(year, grid) {
  path <- arg_source_file(year + 1L, sprintf("tabulation_%d.dta", year))
  if (!file.exists(path)) stop("Missing ARG tabulation: ", path, call. = FALSE)
  input <- haven::read_dta(path)
  p <- as.numeric(input$p)
  bracketavg <- as.numeric(input$bracketavg)
  average <- as.numeric(input$average[[1L]])
  if (length(p) < 3L || any(!is.finite(p)) || any(diff(p) <= 0) ||
      any(!is.finite(bracketavg)) || !is.finite(average) || average <= 0) {
    stop("Invalid ARG tabulation: ", path, call. = FALSE)
  }

  # The 2013-2018 tabulations have low brackets that the current gpinter
  # package rejects. Retry with the smallest omitted prefix, capped at two
  # brackets so future input changes cannot silently alter the method.
  failures <- character()
  for (drop in 0:2) {
    keep <- seq.int(drop + 1L, length(p))
    fit <- tryCatch(
      gpinter::shares_fit(p = p[keep], average = average,
                           bracketavg = bracketavg[keep]),
      error = function(e) {
        message <- conditionMessage(e)
        if (!startsWith(message, "The average below the first bracket")) {
          stop(sprintf("gpinter failed for ARG %d: %s", year, message), call. = FALSE)
        }
        failures <<- c(failures, message)
        NULL
      }
    )
    if (!is.null(fit)) {
      return(list(topavg = gpinter::generate_tabulation(fit, grid)$top_average,
                  dropped = drop, input = path, fit_errors = failures))
    }
  }
  stop(sprintf("gpinter could not fit ARG %d after at most two low brackets were omitted: %s",
               year, tail(failures, 1L)), call. = FALSE)
}

arg_tax_table <- function(year) {
  source_year <- year + 1L
  if (source_year == 2009L) {
    # The derived 2009 .dta is absent, but the original AFIP workbook exists.
    path <- arg_source_file(source_year, "2.2.2.1.11.xlsx")
    if (!file.exists(path)) stop("Missing ARG AFIP tax table: ", path, call. = FALSE)
    table <- readxl::read_excel(path, sheet = "Hoja1", range = "B18:H35",
                                col_names = FALSE, .name_repair = "minimal")
    names(table) <- c("desde", "hasta", "declarantes_tax", "totinc_tax",
                      "casos_gan", "importe_gan", "impuesto")
    # AFIP stored thousands separators and nonbreaking spaces as text.
    table[] <- lapply(table, function(x) as.numeric(gsub("[^0-9-]", "", as.character(x))))
  } else {
    path <- arg_source_file(source_year, "2.2.2.1.11.dta")
    if (!file.exists(path)) stop("Missing ARG AFIP tax table: ", path, call. = FALSE)
    table <- haven::read_dta(path)
  }
  expected <- if (year == 2019L) 30L else 18L
  if (nrow(table) != expected ||
      any(!is.finite(table$desde)) || any(diff(table$desde) <= 0) ||
      any(!is.finite(table$totinc_tax)) || any(table$totinc_tax <= 0) ||
      any(!is.finite(table$impuesto)) || any(table$impuesto < 0)) {
    stop(sprintf("Invalid ARG %d tax table: %s", year, path), call. = FALSE)
  }
  list(table = table, input = path)
}

arg_linear_ipolate <- function(p, rates) {
  known <- which(!is.na(rates))
  if (length(known) < 2L) stop("ARG needs at least two tax-rate anchors.", call. = FALSE)
  result <- stats::approx(p[known], rates[known], xout = p, rule = 2L)$y
  outside_low <- which(p < p[known[[1L]]])
  outside_high <- which(p > p[tail(known, 1L)])
  for (outside in list(outside_low, outside_high)) {
    if (!length(outside)) next
    pair <- if (identical(outside, outside_low)) known[1:2] else tail(known, 2L)
    result[outside] <- rates[pair[[1L]]] +
      (p[outside] - p[pair[[1L]]]) *
      diff(rates[pair]) / diff(p[pair])
  }
  result
}

arg_year_rates <- function(year, grid) {
  distribution <- arg_distribution(year, grid)
  tax <- arg_tax_table(year)
  table <- tax$table
  n <- nrow(table)
  bracket_income <- c((table$desde[-n] + table$desde[-1L]) / 2,
                      table$desde[[n]] * 1.5)
  tax_rate <- table$impuesto / table$totinc_tax
  if (any(!is.finite(tax_rate)) || any(tax_rate > 1)) {
    stop(sprintf("ARG %d tax rates are outside [0, 1].", year), call. = FALSE)
  }

  anchors <- numeric(length(grid))
  previous <- NA_integer_
  for (i in seq_len(n)) {
    index <- which.min(abs(distribution$topavg - bracket_income[[i]]))
    # Match the old adjacent-tie rule: move the second anchor one grid row up.
    if (!is.na(previous) && index == previous) index <- min(index + 1L, length(grid))
    anchors[[index]] <- anchors[[index]] + tax_rate[[i]]
    previous <- index
  }
  if (year == 2019L) anchors[anchors > 0.4] <- 0 # historical 2020-table rule
  anchors[-1L][anchors[-1L] == 0] <- NA_real_
  interpolated <- arg_linear_ipolate(grid, anchors)
  if (any(!is.finite(interpolated)) || any(interpolated < 0 | interpolated >= 1)) {
    stop(sprintf("ARG %d interpolated rates are invalid.", year), call. = FALSE)
  }

  # Stata stores p_merge as float after round(p, .00001) * 10000. Recreate
  # those float values exactly so merges with Stata survey keys remain exact.
  p_merge <- arg_f32(arg_f32(round(grid, 5L)) * 10000)
  # Only the merge key and rate enter the pipeline. The fitted top averages
  # are used to place tax anchors but are not downstream income estimates.
  result <- data.frame(p_merge = p_merge, eff_tax_rate_ipol = interpolated)
  result <- result[!duplicated(result$p_merge) & result$p_merge <= 9999, ]
  rownames(result) <- NULL
  if (nrow(result) != 118L || anyDuplicated(result$p_merge)) {
    stop(sprintf("ARG %d rates have an unexpected percentile grid.", year), call. = FALSE)
  }
  list(data = result, distribution = distribution, tax = tax$input)
}

arg_historical <- function(ref, path, extension) {
  destination <- tempfile(fileext = extension)
  on.exit(unlink(destination), add = TRUE)
  status <- suppressWarnings(system2("git", c("show", paste0(ref, ":", path)),
                                     stdout = destination, stderr = FALSE))
  if (!identical(status, 0L)) stop("Missing historical comparison file: ", path, call. = FALSE)
  if (identical(extension, ".dta")) haven::read_dta(destination)
  else readxl::read_excel(destination)
}

arg_compare <- function(year, result, ref) {
  base <- "input_data/admin_data/ARG"
  historical <- arg_historical(
    ref, sprintf("%s/eff-tax-rate/eff-tax-rate/ARG_effrates_%d.dta", base, year), ".dta")
  prior_gpinter <- arg_historical(ref, sprintf("%s/diverse_ARG_%d.xlsx", base, year), ".xlsx")
  matched <- merge(result$data, historical[, c("p_merge", "eff_tax_rate_ipol")],
                   by = "p_merge", suffixes = c("_new", "_old"))
  if (nrow(matched) != 118L || nrow(prior_gpinter) != 127L) {
    stop(sprintf("ARG %d comparison grids do not match.", year), call. = FALSE)
  }
  rate_difference <- abs(matched$eff_tax_rate_ipol_new - matched$eff_tax_rate_ipol_old)
  topavg_difference <- abs(result$distribution$topavg - as.numeric(prior_gpinter$topavg))
  data.frame(year = year,
             dropped_low_brackets = result$distribution$dropped,
             max_abs_rate_difference = max(rate_difference),
             mean_abs_rate_difference = mean(rate_difference),
             max_relative_gpinter_topavg_difference =
               max(topavg_difference / pmax(abs(prior_gpinter$topavg), 1)),
             stringsAsFactors = FALSE)
}

output_dir <- options[["--output-dir"]] %||%
  file.path("input_data", "admin_data", "ARG", "eff-tax-rate")
grid <- dina_admin_gpinter_grid()
results <- vector("list", 18L)
summaries <- vector("list", 18L)
for (year in 2002:2019) {
  result <- arg_year_rates(year, grid)
  results[[year - 2001L]] <- result
  cat(sprintf("ARG %d: %d rates; omitted %d low gpinter brackets\n",
              year, nrow(result$data), result$distribution$dropped))
  if (!is.null(options[["--compare-ref"]])) {
    summaries[[year - 2001L]] <- arg_compare(year, result, options[["--compare-ref"]])
  }
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
manifest <- vector("list", 18L)
for (year in 2002:2019) {
  output <- file.path(output_dir, sprintf("ARG_effrates_%d.dta", year))
  result <- results[[year - 2001L]]
  haven::write_dta(result$data, output)
  manifest[[year - 2001L]] <- data.frame(
    year = year,
    tabulation = result$distribution$input,
    tax_table = result$tax,
    dropped_low_brackets = result$distribution$dropped,
    gpinter_version = as.character(utils::packageVersion("gpinter")),
    output_sha256 = digest::digest(file = output, algo = "sha256"),
    stringsAsFactors = FALSE
  )
}
utils::write.csv(do.call(rbind, manifest),
                 file.path(output_dir, "ARG_rates_manifest.csv"), row.names = FALSE)
if (!is.null(options[["--compare-ref"]])) {
  comparison <- do.call(rbind, summaries)
  path <- file.path(output_dir, "ARG_historical_comparison.csv")
  utils::write.csv(comparison, path, row.names = FALSE)
  cat(sprintf("Comparison: %s\nMax absolute rate difference: %.9g\n",
              path, max(comparison$max_abs_rate_difference)))
}
