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

need("haven")

url <- Sys.getenv(
  "DINA_FETCH_WB_INFLATION_URL",
  unset = "https://api.worldbank.org/v2/en/indicator/FP.CPI.TOTL.ZG?downloadformat=csv"
)

copy_or_download <- function(from, to) {
  if (file.exists(from)) {
    file.copy(from, to, overwrite = TRUE)
  } else {
    utils::download.file(from, to, mode = "wb", quiet = TRUE)
    TRUE
  }
}

countries <- strsplit(Sys.getenv(
  "DINA_FETCH_WB_COUNTRIES",
  unset = "ARG,BOL,BRA,CHL,COL,CRI,DOM,ECU,MEX,PER,SLV,URY,VEN"
), ",", fixed = TRUE)[[1]]
countries <- trimws(toupper(countries))

tmp <- tempfile("wb-inflation-")
copy_or_download(url, tmp)

if (grepl("\\.dta$", url, ignore.case = TRUE) && file.exists(tmp)) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  file.copy(tmp, target, overwrite = FALSE)
  cat(sprintf("copied %s\n", target))
  quit(status = 0)
}

csv <- tmp
zip_list <- tryCatch(utils::unzip(tmp, list = TRUE), error = function(e) NULL)
if (!is.null(zip_list)) {
  unzip_dir <- tempfile("wb-inflation-unzip-")
  dir.create(unzip_dir)
  utils::unzip(tmp, exdir = unzip_dir)
  candidates <- list.files(unzip_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  candidates <- candidates[!grepl("Metadata", basename(candidates), ignore.case = TRUE)]
  api_candidates <- candidates[grepl("FP\\.CPI\\.TOTL\\.ZG|API_", basename(candidates), ignore.case = TRUE)]
  if (length(api_candidates)) {
    candidates <- api_candidates
  }
  if (!length(candidates)) {
    stop("World Bank inflation archive did not contain a usable CSV.", call. = FALSE)
  }
  csv <- candidates[[1L]]
}

raw <- utils::read.csv(csv, skip = 4, check.names = FALSE, stringsAsFactors = FALSE)
country_col <- grep("^Country Code$", names(raw), value = FALSE)[1]
if (is.na(country_col)) {
  stop("World Bank inflation CSV is missing `Country Code`.", call. = FALSE)
}
year_cols <- grep("^[0-9]{4}$", names(raw), value = TRUE)
if (!length(year_cols)) {
  stop("World Bank inflation CSV has no year columns.", call. = FALSE)
}

rows <- lapply(year_cols, function(year) {
  data.frame(
    country = toupper(raw[[country_col]]),
    year = as.integer(year),
    inflation = suppressWarnings(as.numeric(raw[[year]])),
    stringsAsFactors = FALSE
  )
})
out <- do.call(rbind, rows)
out <- out[out$country %in% countries & !is.na(out$inflation), , drop = FALSE]
out <- out[order(out$country, out$year), , drop = FALSE]
if (!nrow(out)) {
  stop("World Bank inflation CSV parsed, but no configured LatAm rows were found.", call. = FALSE)
}

dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
haven::write_dta(out, target)
cat(sprintf("wrote %s rows to %s\n", nrow(out), target))
