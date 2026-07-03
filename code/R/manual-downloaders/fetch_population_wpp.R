#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1L]] >= length(args)) default else args[[hit[[1L]] + 1L]]
}

target <- arg_value("--target")
if (!nzchar(target)) {
  stop("Missing --target.", call. = FALSE)
}

need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

source_url <- Sys.getenv(
  "DINA_FETCH_POPULATION_WPP_URL",
  unset = "https://population.un.org/wpp/Download/Standard/Population/"
)

country_map <- c(
  Argentina = "ARG",
  Bolivia = "BOL",
  Brazil = "BRA",
  Chile = "CHL",
  Colombia = "COL",
  `Costa Rica` = "CRI",
  Ecuador = "ECU",
  Mexico = "MEX",
  Peru = "PER",
  Uruguay = "URY",
  `Venezuela (Bolivarian Republic of)` = "VEN",
  Venezuela = "VEN",
  `Dominican Republic` = "DOM",
  `El Salvador` = "SLV"
)

copy_or_download <- function(from, to) {
  if (file.exists(from)) {
    file.copy(from, to, overwrite = TRUE)
  } else {
    utils::download.file(from, to, mode = "wb", quiet = TRUE)
    TRUE
  }
}

discover_candidate <- function(page_url) {
  lines <- if (file.exists(page_url)) {
    readLines(page_url, warn = FALSE)
  } else {
    readLines(page_url, warn = FALSE)
  }
  html <- paste(lines, collapse = "\n")
  hits <- regmatches(html, gregexpr("href=[\"'][^\"']+[\"']", html, perl = TRUE))[[1]]
  hrefs <- gsub("^href=[\"']|[\"']$", "", hits)
  hrefs <- hrefs[grepl("\\.(xlsx|xls|csv|zip)(\\?|$)", hrefs, ignore.case = TRUE)]
  hrefs <- hrefs[grepl("population|age|wpp", hrefs, ignore.case = TRUE)]
  hrefs <- unique(hrefs)
  if (!length(hrefs)) {
    stop("No WPP population-age download link was discovered. Download manually into the population bucket.", call. = FALSE)
  }
  if (length(hrefs) > 1L) {
    stop("Multiple WPP population-age links were discovered; manual choice is required.", call. = FALSE)
  }
  if (grepl("^https?://", hrefs[[1L]])) {
    hrefs[[1L]]
  } else {
    paste0(sub("/?$", "/", page_url), hrefs[[1L]])
  }
}

transform_population <- function(path, target) {
  need("readxl")
  need("haven")
  raw <- readxl::read_excel(path)
  names_norm <- tolower(gsub("[^A-Za-z0-9]+", "_", names(raw)))
  location_col <- grep("location|country", names_norm, value = FALSE)[1]
  age_col <- grep("^age$|age_grp|age_group", names_norm, value = FALSE)[1]
  year_cols <- grep("^[0-9]{4}$", names(raw), value = TRUE)
  if (is.na(location_col) || is.na(age_col) || !length(year_cols)) {
    stop("WPP workbook was downloaded but not recognized as an age-by-year table.", call. = FALSE)
  }
  location <- as.character(raw[[location_col]])
  country <- unname(country_map[location])
  keep_country <- !is.na(country)
  raw <- raw[keep_country, , drop = FALSE]
  country <- country[keep_country]
  age <- as.character(raw[[age_col]])
  adult <- !grepl("^0|^1-4|^5-9|^10-14|^15-19", age)

  long <- do.call(rbind, lapply(year_cols, function(year) {
    value <- suppressWarnings(as.numeric(raw[[year]])) * 1000
    data.frame(country = country, year = as.integer(year), age = age, population = value, adult = adult, stringsAsFactors = FALSE)
  }))
  total <- stats::aggregate(population ~ country + year, data = long, sum, na.rm = TRUE)
  adult_total <- stats::aggregate(population ~ country + year, data = long[long$adult, , drop = FALSE], sum, na.rm = TRUE)
  names(total)[names(total) == "population"] <- "totalpop"
  names(adult_total)[names(adult_total) == "population"] <- "adultpop"
  out <- merge(total, adult_total, by = c("country", "year"), all.x = TRUE)
  out$region <- ""
  out <- out[, c("country", "region", "year", "totalpop", "adultpop")]
  out <- out[order(out$country, out$year), , drop = FALSE]
  if (!nrow(out)) {
    stop("WPP workbook parsed, but no configured LatAm country rows were found.", call. = FALSE)
  }
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  haven::write_dta(out, target)
}

candidate <- source_url
if (!file.exists(candidate) && !grepl("\\.(dta|xlsx|xls|csv|zip)(\\?|$)", candidate, ignore.case = TRUE)) {
  candidate <- discover_candidate(candidate)
}

ext_match <- regmatches(candidate, regexpr("\\.(dta|xlsx|xls|zip)", candidate, ignore.case = TRUE, perl = TRUE))
tmp <- tempfile("wpp-population-", fileext = if (length(ext_match)) tolower(ext_match[[1L]]) else "")
copy_or_download(candidate, tmp)

if (grepl("\\.dta$", candidate, ignore.case = TRUE)) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  file.copy(tmp, target, overwrite = FALSE)
  cat(sprintf("copied %s\n", target))
  quit(status = 0)
}

if (grepl("\\.zip($|\\?)", candidate, ignore.case = TRUE)) {
  unzip_dir <- tempfile("wpp-population-unzip-")
  dir.create(unzip_dir)
  utils::unzip(tmp, exdir = unzip_dir)
  candidates <- list.files(unzip_dir, pattern = "\\.(xlsx|xls)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  candidates <- candidates[grepl("population|age|wpp", basename(candidates), ignore.case = TRUE)]
  if (length(candidates) != 1L) {
    stop("WPP archive did not contain exactly one population-age workbook; manual choice is required.", call. = FALSE)
  }
  tmp <- candidates[[1L]]
}

if (!grepl("\\.(xlsx|xls)$", candidate, ignore.case = TRUE) && !grepl("\\.(xlsx|xls)$", tmp, ignore.case = TRUE)) {
  stop("WPP candidate was not a DTA or Excel workbook; download manually into the population bucket.", call. = FALSE)
}

transform_population(tmp, target)
cat(sprintf("wrote %s\n", target))
