#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1L]] >= length(args)) default else args[[hit[[1L]] + 1L]]
}

target <- arg_value("--target")
repo_root <- arg_value("--repo-root", getwd())
if (!nzchar(target)) {
  stop("Missing --target.", call. = FALSE)
}

need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

normalize_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "_", x)
}

parse_number <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^0-9,.-]", "", x)
  has_comma <- grepl(",", x, fixed = TRUE)
  x[has_comma] <- gsub("\\.", "", x[has_comma])
  x[has_comma] <- gsub(",", ".", x[has_comma], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

canonicalize_thresholds <- function(data) {
  names(data) <- normalize_name(names(data))
  aliases <- list(
    year = c("year", "ano", "ano_calendario"),
    avg_ui = c("avg_ui", "avgui", "average_ui", "media_ui", "seguro_desemprego_medio"),
    maxlimit_inss = c("maxlimit_inss", "maxlimitinss", "teto_inss", "limite_inss", "inss_ceiling"),
    exempt_dirpf = c("exempt_dirpf", "exemptdirpf", "isencao_dirpf", "limite_isencao_dirpf", "irpf_exempt")
  )
  find_col <- function(target) {
    hit <- intersect(aliases[[target]], names(data))
    if (length(hit)) hit[[1L]] else ""
  }
  cols <- vapply(names(aliases), find_col, character(1))
  missing <- names(cols)[!nzchar(cols)]
  if (length(missing)) {
    stop("Brazil admin-threshold source is missing columns: ", paste(missing, collapse = ","), call. = FALSE)
  }
  out <- data.frame(
    year = suppressWarnings(as.integer(data[[cols[["year"]]]])),
    avg_ui = parse_number(data[[cols[["avg_ui"]]]]),
    maxlimit_inss = parse_number(data[[cols[["maxlimit_inss"]]]]),
    exempt_dirpf = parse_number(data[[cols[["exempt_dirpf"]]]]),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$year), , drop = FALSE]
  if (!nrow(out)) stop("Brazil admin-threshold source has no year rows.", call. = FALSE)
  duplicated_years <- unique(out$year[duplicated(out$year)])
  if (length(duplicated_years)) {
    stop("Brazil admin-threshold source has duplicate years: ", paste(duplicated_years, collapse = ","), call. = FALSE)
  }
  out[order(out$year), , drop = FALSE]
}

read_html_thresholds <- function(url) {
  need("rvest")
  content <- rvest::read_html(url)
  tables <- rvest::html_table(content, fill = TRUE)
  if (!length(tables)) {
    stop("No HTML tables found for Brazil admin thresholds.", call. = FALSE)
  }
  errors <- character()
  for (table in tables) {
    parsed <- tryCatch(canonicalize_thresholds(table), error = function(e) e)
    if (!inherits(parsed, "error")) return(parsed)
    errors <- c(errors, conditionMessage(parsed))
  }
  stop("Could not identify a Brazil admin-threshold table. Last parser error: ", tail(errors, 1L), call. = FALSE)
}

source_csv <- Sys.getenv("DINA_FETCH_BRA_ADMIN_THRESHOLDS_SOURCE", unset = "")
source_url <- Sys.getenv("DINA_FETCH_BRA_ADMIN_THRESHOLDS_URL", unset = "")
canonical <- file.path(repo_root, "input_data", "admin_data", "BRA", "downloads", "admin_thresholds.csv")

if (nzchar(source_csv)) {
  out <- canonicalize_thresholds(utils::read.csv(source_csv, stringsAsFactors = FALSE, na.strings = c("", "NA")))
} else if (nzchar(source_url)) {
  out <- read_html_thresholds(source_url)
} else if (file.exists(canonical)) {
  out <- canonicalize_thresholds(utils::read.csv(canonical, stringsAsFactors = FALSE, na.strings = c("", "NA")))
} else {
  stop("Set DINA_FETCH_BRA_ADMIN_THRESHOLDS_URL or DINA_FETCH_BRA_ADMIN_THRESHOLDS_SOURCE.", call. = FALSE)
}

dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, target, row.names = FALSE, na = "")
cat(sprintf("wrote %s Brazil admin-threshold rows to %s\n", nrow(out), target))
