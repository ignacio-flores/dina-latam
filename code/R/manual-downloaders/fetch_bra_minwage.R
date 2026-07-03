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

content <- rvest::read_html(url)
tables <- rvest::html_table(content, fill = TRUE)
if (!length(tables)) {
  stop("No HTML tables found on the Brazil minimum wage page.", call. = FALSE)
}

score_table <- function(tbl) {
  names_norm <- normalize_name(names(tbl))
  sum(grepl("vig|ano|year", names_norm)) + sum(grepl("valor|salario|minimum", names_norm))
}

scores <- vapply(tables, score_table, integer(1))
tbl <- tables[[which.max(scores)]]
names_norm <- normalize_name(names(tbl))
vig_col <- grep("vig|ano|year", names_norm, value = FALSE)[1]
value_col <- grep("valor|salario|minimum", names_norm, value = FALSE)[1]
if (is.na(vig_col) || is.na(value_col)) {
  stop("Could not identify year and minimum-wage columns.", call. = FALSE)
}

year <- regmatches(as.character(tbl[[vig_col]]), regexpr("(19|20)[0-9]{2}", as.character(tbl[[vig_col]]), perl = TRUE))
year <- suppressWarnings(as.integer(year))
minwage <- parse_money(tbl[[value_col]])
out <- data.frame(year = year, minwage = minwage)
out <- out[!is.na(out$year) & !is.na(out$minwage), , drop = FALSE]
if (!nrow(out)) {
  stop("Parsed the page but found no year/minimum-wage rows.", call. = FALSE)
}
out <- out[order(out$year), , drop = FALSE]
out <- aggregate(minwage ~ year, data = out, FUN = function(x) tail(x[!is.na(x)], 1L))

dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(out, target, row.names = FALSE, na = "")
cat(sprintf("wrote %s rows to %s\n", nrow(out), target))
