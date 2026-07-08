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

url <- Sys.getenv(
  "DINA_FETCH_BRA_MINWAGE_URL",
  unset = "https://es.wikipedia.org/wiki/Anexo:Salario_m%C3%ADnimo_en_Brasil"
)

need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

need("rvest")

normalize_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "_", x)
}

parse_money <- function(x) {
  x <- gsub("[^0-9,.-]", "", as.character(x))
  has_comma <- grepl(",", x, fixed = TRUE)
  x[has_comma] <- gsub("\\.", "", x[has_comma])
  x[has_comma] <- gsub(",", ".", x[has_comma], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

parse_year <- function(x) {
  x <- as.character(x)
  hit <- regexpr("(19|20)[0-9]{2}", x, perl = TRUE)
  out <- rep(NA_integer_, length(x))
  ok <- hit > 0L
  out[ok] <- suppressWarnings(as.integer(regmatches(x, hit)))
  out
}

candidate_from_table <- function(tbl) {
  best <- NULL
  for (year_col in seq_along(tbl)) {
    years <- parse_year(tbl[[year_col]])
    for (value_col in seq_along(tbl)) {
      if (identical(year_col, value_col)) next
      minwage <- parse_money(tbl[[value_col]])
      valid <- !is.na(years) & !is.na(minwage) & years >= 1900L & years <= 2100L & minwage > 0
      score <- sum(valid)
      if (!is.null(best) && score <= best$score) next
      best <- list(
        score = score,
        data = data.frame(row_id = seq_along(years), year = years, minwage = minwage, stringsAsFactors = FALSE)
      )
    }
  }
  best
}

content <- rvest::read_html(url)
tables <- rvest::html_table(content, fill = TRUE)
if (!length(tables)) {
  stop("No HTML tables found on the Brazil minimum wage page.", call. = FALSE)
}

candidate <- NULL
for (tbl in tables) {
  current <- candidate_from_table(tbl)
  if (!is.null(current) && (is.null(candidate) || current$score > candidate$score)) {
    candidate <- current
  }
}
if (is.null(candidate) || candidate$score == 0L) {
  stop("Could not identify year and minimum-wage columns.", call. = FALSE)
}

out <- candidate$data
out <- out[!is.na(out$year) & !is.na(out$minwage), , drop = FALSE]
if (!nrow(out)) {
  stop("Parsed the page but found no year/minimum-wage rows.", call. = FALSE)
}
out <- out[order(out$year, out$row_id), , drop = FALSE]
out <- aggregate(minwage ~ year, data = out, FUN = function(x) tail(x[!is.na(x)], 1L))

dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, target, row.names = FALSE, na = "")
cat(sprintf("wrote %s rows to %s\n", nrow(out), target))
