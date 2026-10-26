# Bounded BFM review helpers. These never call the production publisher.
# Source from the repository root; all generated files stay under `review`.
correction_review_setup <- function(review) {
  root <- normalizePath(getwd(), mustWork = TRUE)
  dir.create(review, recursive = TRUE, showWarnings = FALSE)
  review <- normalizePath(review, mustWork = TRUE)
  if (!startsWith(review, paste0(root, "/output/experiments/"))) {
    stop("Correction reviews must be isolated under output/experiments/.")
  }
  source(file.path(root, "code/R/country_run.R"), local = environment())
  country_run_load_cli(root)
  session <- dina_load_session(root = root)
  config <- dina_session_config(session, root)
  identity <- dina_config_validation_identity(root, session, dina_config_validation_receipt(root, session))
  stage <- file.path(review, "work/stage")
  if (!dir.exists(stage)) stage <- country_run_stage(root, file.path(review, "work"), list())
  list(root = root, review = review, config = config, identity = identity,
       stage = stage,
       runtime_writer = dina_write_runtime_stata_config)
}

correction_review_run <- function(context, task, countries, label = task, review_inputs = FALSE) {
  if (!task %in% c("03a-run-bfm", "03b-run-bfm-2stage", "03d-prepare-theta-extrapolation", "03e-extrapolate-bfm-correction")) {
    stop("Only correction-stage tasks are permitted in this review.")
  }
  stopifnot(all(countries %in% unlist(context$config$countries)))
  directory <- file.path(context$review, "logs", label)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  coverage <- file.path(context$stage, "_coverage.csv")
  if (file.exists(coverage)) {
    file.copy(coverage, file.path(directory, "coverage-before.csv"), overwrite = TRUE)
    unlink(coverage)
  }
  runtime <- context$runtime_writer(context$config, context$identity)
  on.exit(unlink(runtime), add = TRUE)
  log <- file.path(directory, "stata.log")
  marker <- file.path(directory, "return-code.txt")
  if (file.exists(marker)) unlink(marker)
  wrapper <- file.path(directory, "review.do")
  writeLines(c("set more off", sprintf('cd "%s"', context$root), "capture log close _all",
    sprintf('log using "%s", text replace', log),
    sprintf('capture noisily do "%s/code/Stata/%s.do"', context$root, task),
    "local result = _rc", sprintf('file open result using "%s", write replace', marker),
    'file write result "`result\'"', "file close result", "capture log close _all", "exit `result'"), wrapper)
  env <- c(DINA_CONFIG_DO = runtime, DINA_COUNTRY_WORKER = task,
    DINA_RUN_COUNTRIES = paste(countries, collapse = " "), DINA_OUTPUT_ROOT = context$stage,
    DINA_RSCRIPT = file.path(R.home("bin"), "Rscript"),
    DINA_REVIEW_ROOT = if (review_inputs) context$stage else "")
  cat(sprintf("Review %s: %s; outputs %s\n", label, paste(countries, collapse = ","), context$stage))
  process <- processx::process$new(context$config$stata$command, c("-b", "do", wrapper),
    wd = directory, env = env, stdout = file.path(directory, "stdout.log"),
    stderr = file.path(directory, "stderr.log"), cleanup_tree = TRUE)
  on.exit(if (process$is_alive()) process$kill_tree(), add = TRUE)
  last <- Sys.time()
  while (process$is_alive()) {
    process$wait(1000)
    if (as.numeric(difftime(Sys.time(), last, units = "secs")) >= 20) {
      cat(sprintf("%s running; log %.0f KiB\n", label, if (file.exists(log)) file.info(log)$size/1024 else 0))
      last <- Sys.time()
    }
  }
  if (!file.exists(marker) || !identical(trimws(readLines(marker, warn = FALSE)), "0")) {
    stop("Review calculation failed; inspect ", log)
  }
  country_output_coverage <- get("country_output_coverage", envir = environment(context$runtime_writer))
  country_output_coverage(context$stage)
  file.copy(coverage, file.path(directory, "coverage.csv"), overwrite = TRUE)
  invisible(log)
}

correction_review_panel <- function(context, countries) {
  path <- "output/bfm_summary/all_thetas.xlsx"
  original <- openxlsx::read.xlsx(file.path(context$root, path), sheet = "panel")
  keys <- unique(original[original$country %in% countries, c("country", "year")])
  fresh <- lapply(seq_len(nrow(keys)), function(i) {
    country <- keys$country[i]; year <- as.integer(keys$year[i])
    donor <- haven::read_dta(file.path(context$stage, "output/bfm_summary/observed_donors", paste0(country, year, ".dta")))
    stopifnot(nrow(donor) == 127L, all(donor$country == country), all(donor$year == year))
    candidates <- file.path(context$stage, "output/bfm_summary", c("bfm_norep_pos", "bfm_norep_pre"), "merging_points.xlsx")
    sheet <- paste0(country, year)
    candidates <- candidates[vapply(candidates, function(p) file.exists(p) && sheet %in% openxlsx::getSheetNames(p), logical(1))]
    if (length(candidates) != 1L) stop("Expected one independent observed merging sheet for ", sheet)
    data <- openxlsx::read.xlsx(candidates, sheet = sheet)
    data$country <- country
    data$year <- if (is.character(original$year)) as.character(year) else year
    data$mpoint <- round(data$mpoint[1] * 100, 4)
    data$p <- round(data$p * 100, 4)
    data
  })
  data <- dplyr::bind_rows(original[!original$country %in% countries, ], dplyr::bind_rows(fresh))
  selected <- data$country %in% countries
  data$avg_t[selected] <- ave(data$antitonic[selected], data$country[selected], data$p[selected],
    FUN = function(x) median(x, na.rm = TRUE))
  data <- data[order(data$country, data$year, data$p), ]
  stopifnot(nrow(unique(data[c("country", "year")])) == nrow(unique(original[c("country", "year")])) )
  unchanged <- function(x) {
    x <- x[!x$country %in% countries, names(original), drop = FALSE]
    x <- x[order(x$country, x$year, x$p), ]; rownames(x) <- NULL
    x
  }
  stopifnot(isTRUE(all.equal(unchanged(original), unchanged(data), check.attributes = FALSE, tolerance = 0)))
  destination <- file.path(context$stage, path)
  openxlsx::write.xlsx(list(panel = data), destination)
  invisible(data)
}

correction_review_summary <- function(root, countries) {
  result <- list()
  for (country in countries) {
    for (kind in c("pos", "pre")) for (suffix in c("", "_extrap")) {
      path <- file.path(root, "output/bfm_summary", paste0("bfm_norep_", kind), paste0("bfm_summary_", country, suffix, ".xlsx"))
      if (!file.exists(path)) next
      for (sheet in openxlsx::getSheetNames(path)) {
        if (!grepl("^sum[0-9]{4}$", sheet)) next
        data <- openxlsx::read.xlsx(path, sheet)
        if (!all(c("p", "topshare", "average") %in% names(data))) next
        rows <- vapply(c(.9, .99, .999), function(p) which.min(abs(data$p - p)), integer(1))
        year <- as.integer(sub("sum", "", sheet))
        result[[paste(country, year)]] <- data.frame(country = country, year = year,
          mean = data$average[1], top10 = data$topshare[rows[1]],
          top1 = data$topshare[rows[2]], top01 = data$topshare[rows[3]],
          source = substring(path, nchar(root) + 2L), sheet = sheet)
      }
    }
  }
  dplyr::bind_rows(result)
}

# Exact weighted empirical shares for a like-for-like comparison of accepted
# and candidate microdata. Fractional boundary weights handle point masses.
# Income definitions are supplied explicitly by the diagnostic caller.
correction_review_microdata <- function(root, cases, income_variables, kinds) {
  result <- lapply(seq_len(nrow(cases)), function(i) {
    country <- cases$country[i]; year <- as.integer(cases$year[i])
    income <- income_variables[[country]]; kind <- kinds[[country]]
    path <- file.path(root, "intermediary_data/microdata", paste0("bfm_norep_", kind),
      sprintf("%s_%d_bfm_norep_%s.dta", country, year, kind))
    if (!file.exists(path)) stop("Missing correction for comparison: ", path)
    data <- haven::read_dta(path, col_select = tidyselect::all_of(c("_weight", income)))
    valid <- is.finite(data[[income]]) & is.finite(data$`_weight`) & data$`_weight` > 0
    y <- as.numeric(data[[income]][valid]); w <- as.numeric(data$`_weight`[valid])
    order <- order(y, decreasing = TRUE); y <- y[order]; w <- w[order]
    population <- sum(w); income_total <- sum(w * y)
    if (population <= 0 || income_total <= 0) stop("Invalid totals for ", country, " ", year)
    before <- c(0, head(cumsum(w), -1L))
    shares <- vapply(c(.1, .01, .001), function(fraction) {
      sum(pmax(0, pmin(w, fraction * population - before)) * y) / income_total
    }, 0)
    data.frame(country = country, year = year, income_variable = income,
      population = population, mean = income_total/population,
      top10 = shares[1], top1 = shares[2], top01 = shares[3])
  })
  dplyr::bind_rows(result)
}
