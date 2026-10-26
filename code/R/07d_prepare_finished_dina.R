#!/usr/bin/env Rscript
# Integrate included finished sources automatically; internal 07c results stay intact.
dina_finished_stata_files <- function(active, config, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  data <- active$data
  overlay <- data.frame(country = character(), year = double(), step = character(),
    p = double(), bracketavg = double(), bracketavg_norm = double(), s = double(),
    thr = double(), thr_norm = double(), topavg = double(), topavg_norm = double(),
    topsh = double(), bottomsh = double(), bottomavg = double(),
    imputed = double(), extrap = double(), finished_dina = double())
  attribution <- data.frame(country = character(), year = double(), step = character(),
    pub_source = character(), pub_origin = character(), pub_citation = character(),
    pub_doi = character(), pub_version = character(), pub_adjustment = character(),
    pub_anchor = double(), pub_supplied_gini = double(), pub_grouped_gini = double(),
    pub_fitted_gini = double(), pub_gini_method = character())
  if (is.data.frame(data) && nrow(data)) {
    if (any(!data$country %in% unlist(config$countries)) ||
        any(!data$step %in% unlist(config$export_validation$steps)) ||
        any(data$unit != config$export_validation$unit)) {
      stop("An active finished-series publication is outside the export country/concept/unit configuration.")
    }
    if (any(!data$origin %in% c("supplied", "continuation", "carry_forward"))) stop("Unknown publication origin.")
    key <- paste(data$country, data$year, data$step, round(data$p * 1e5))
    if (anyDuplicated(key)) stop("Active finished-series contributions overlap.")
    overlay <- data.frame(country = data$country, year = as.double(data$year), step = data$step,
      p = round(data$p * 1e5), bracketavg = data$avg, bracketavg_norm = data$avg,
      s = data$bckt_sh, thr = data$thr, thr_norm = data$thr,
      topavg = data$topavg, topavg_norm = data$topavg, topsh = data$topsh,
      bottomsh = data$bottomsh, bottomavg = data$bottomavg, imputed = 0,
      extrap = ifelse(data$origin == "supplied", NA_real_, 1), finished_dina = 1)
    numeric <- c("p", "bracketavg", "s", "thr", "topavg", "topsh", "bottomsh", "bottomavg")
    if (!all(vapply(overlay[numeric], function(x) all(is.finite(x)), logical(1)))) stop("Included-source integration contains nonfinite statistics.")
    attribution <- unique(data.frame(country = data$country, year = as.double(data$year),
      step = data$step, pub_source = data$source_id, pub_origin = data$origin,
      pub_citation = data$citation, pub_doi = data$doi, pub_version = data$source_version,
      pub_adjustment = data$adjustment, pub_anchor = as.double(data$anchor_year),
      pub_supplied_gini = data$supplied_gini, pub_grouped_gini = data$grouped_gini,
      pub_fitted_gini = if ("fitted_gini" %in% names(data)) data$fitted_gini else NA_real_,
      pub_gini_method = if ("gini_method" %in% names(data)) data$gini_method else "unspecified"))
    if (anyDuplicated(attribution[c("country", "year", "step")])) stop("Publication attribution is inconsistent within a distribution.")
  }
  haven::write_dta(overlay, file.path(directory, "overlay.dta"), version = 14)
  haven::write_dta(attribution, file.path(directory, "attribution.dta"), version = 14)
  invisible(list(overlay = overlay, attribution = attribution))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) stop("Expected export settings request and temporary output directory.")
  root <- normalizePath(getwd())
  Sys.setenv(DINA_CLI_SOURCE_ONLY = "1", DINA_CLI_SOURCE_FILE = file.path(root, "code/R/cli/dina.R"))
  source(Sys.getenv("DINA_CLI_SOURCE_FILE"))
  request <- readLines(args[1L], warn = FALSE)
  if (length(request) != 7L) stop("Incomplete export settings request.")
  config <- dina_session_config(dina_load_session(root = root), root, expand_env = FALSE)
  config$export_validation$previous_update_file <- request[1L]
  config$export_validation$unit <- request[2L]
  words <- function(x) strsplit(trimws(gsub('"', '', x, fixed = TRUE)), "[[:space:]]+")[[1L]]
  config$export_validation$steps <- words(request[3L])
  config$export_validation$last_year <- as.integer(request[4L])
  config$countries <- words(request[5L])
  config$years <- list(first = as.integer(request[6L]), last = as.integer(request[7L]))
  active <- dina_finished_publication_ensure(root, config)
  out <- dina_finished_stata_files(active, config, args[2L])
  cat("Validated finished-series integration:", nrow(out$attribution), "country-year-concept contributions.\n")
}
