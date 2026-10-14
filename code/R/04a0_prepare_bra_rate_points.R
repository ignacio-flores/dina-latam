#!/usr/bin/env Rscript

# Build the bracket effective PIT rates consumed by BRA's Stata interpolation.
# The publisher changes the layout and monetary units of the Tab9/Tab8 tables,
# so keep the source column choices explicit for each release period.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) && (length(args) != 2L || args[[1L]] != "--output" || !nzchar(args[[2L]]))) {
  stop("Usage: Rscript code/R/04a0_prepare_bra_rate_points.R [--output PATH]", call. = FALSE)
}
for (package in c("readxl", "openxlsx", "yaml")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}
source("code/R/functions/read_dina_config.R")
config <- read_dina_config()
last_year <- as.integer(config$last_y)
if (!is.finite(last_year) || last_year < 2007L || last_year > 2024L) {
  stop("BRA rate preparation supports run end years from 2007 through 2024.", call. = FALSE)
}
output <- if (length(args)) args[[2L]] else
  "intermediary_data/tax_rates/BRA/eff-tax-rates-BRA.xlsx"

bra_rate_layout <- function(year) {
  if (year <= 2013L) {
    list(sheet = "P14_P15_T9", range = "C12:P22", columns = c(label = 1L, n = 2L,
      taxable = 3L, exclusive = 4L, exempt = 5L, livro = 10L, tax = 14L), scale = 1e6)
  } else if (year <= 2015L) {
    list(sheet = if (year == 2014L) "T9_AC2014" else "T9", range = "C12:P22",
      columns = c(label = 1L, n = 2L, taxable = 3L, exclusive = 4L,
                  exempt = 5L, livro = 10L, tax = 14L), scale = 1e6)
  } else if (year == 2016L) {
    list(sheet = "faixa SM RTT+RTE+RTI", range = "C10:P20", columns = c(label = 1L,
      n = 2L, taxable = 3L, exclusive = 4L, exempt = 5L, livro = 10L, tax = 14L),
      scale = 1e6)
  } else if (year <= 2018L) {
    list(sheet = "faixa SM RTT+RTE+RTI", range = "B9:O19", columns = c(label = 1L,
      n = 2L, taxable = 3L, exclusive = 4L, exempt = 5L, livro = 10L, tax = 14L),
      scale = 1e6)
  } else if (year <= 2020L) {
    list(sheet = if (year == 2020L) "Tab9_Fx Rend Total" else "faixa SM RTT+RTE+RTI",
      range = "C12:T22", columns = c(label = 1L, n = 2L, taxable = 3L,
      exclusive = 4L, exempt = 5L, livro = 12L, tax = 18L), scale = 1e6)
  } else if (year == 2021L) {
    list(sheet = "Tab8", range = "B8:AB959", columns = c(form = 1L, state = 2L,
      label = 3L, n = 4L, taxable = 5L, exclusive = 13L, exempt = 14L,
      livro = 21L, tax = 27L), scale = 1e6)
  } else if (year == 2022L) {
    list(sheet = "Tab8", range = "A3:AA954", columns = c(form = 1L, state = 2L,
      label = 3L, n = 4L, taxable = 5L, exclusive = 13L, exempt = 14L,
      livro = 21L, tax = 27L), scale = 1)
  } else {
    list(sheet = "Tab8", range = "A3:AS954", columns = c(form = 1L, state = 2L,
      label = 3L, n = 4L, taxable = 5L, exclusive = 13L, exempt = 22L,
      livro = 39L, tax = 45L), scale = 1)
  }
}

bra_brackets <- c("At\u00e9 1/2", "De 1/2 a 1", "De 1 a 2", "De 2 a 3", "De 3 a 5",
                  "De 5 a 7", "De 7 a 10", "De 10 a 15", "De 15 a 20",
                  "De 20 a 30", "De 30 a 40", "De 40 a 60", "De 60 a 80",
                  "De 80 a 160", "De 160 a 240", "De 240 a 320", "Mais de 320")

bra_rate_points <- function(year) {
  path <- sprintf("input_data/admin_data/BRA/gn-irpf-ac%d.xlsx", year)
  if (!file.exists(path)) stop("Missing Brazilian tax workbook: ", path, call. = FALSE)
  layout <- bra_rate_layout(year)
  raw <- readxl::read_excel(path, sheet = layout$sheet, range = layout$range,
                            col_names = FALSE, col_types = "text", .name_repair = "minimal")
  col <- layout$columns
  labels <- as.character(raw[[col[["label"]]]])
  values <- lapply(col[c("n", "taxable", "exclusive", "exempt", "livro", "tax")],
                   function(index) suppressWarnings(as.numeric(raw[[index]])))
  names(values) <- c("n", "taxable", "exclusive", "exempt", "livro", "tax")

  if (year <= 2020L) {
    if (nrow(raw) != 11L) stop(sprintf("BRA %d must have 11 historical rate brackets.", year), call. = FALSE)
    points <- data.frame(label = labels, values, check.names = FALSE)
  } else {
    forms <- as.character(raw[[col[["form"]]]])
    states <- as.character(raw[[col[["state"]]]])
    eligible <- forms %in% c("Completo", "Simplificado") &
      !is.na(states) & nzchar(states) & labels %in% bra_brackets
    if (sum(eligible) < 900L || !setequal(unique(labels[eligible]), bra_brackets)) {
      stop(sprintf("BRA %d Tab8 has unexpected forms or income brackets.", year), call. = FALSE)
    }
    keep <- eligible & labels %in% bra_brackets[seq_len(11L)]
    if (anyNA(values[["n"]][keep])) stop(sprintf("BRA %d has missing declarant counts.", year), call. = FALSE)
    points <- data.frame(label = labels[keep], lapply(values, function(x) {
      x <- x[keep]
      x[is.na(x)] <- 0
      x
    }), check.names = FALSE)
    points <- aggregate(. ~ label, data = points, FUN = sum)
    points <- points[match(bra_brackets[seq_len(11L)], points$label), , drop = FALSE]
    # The historical interpolation deliberately used the first 11 of the
    # 17 published brackets. Keep that selection across the layout change.
  }

  if (anyNA(points) || any(points$n <= 0) || any(points$tax < 0)) {
    stop(sprintf("BRA %d has invalid bracket counts or taxes.", year), call. = FALSE)
  }
  income_total <- points$taxable + points$exclusive + points$exempt - points$livro
  income <- income_total * layout$scale / points$n
  rate <- points$tax / income_total
  if (any(!is.finite(income)) || any(diff(income) <= 0) ||
      any(!is.finite(rate)) || any(rate < 0 | rate >= 1)) {
    stop(sprintf("BRA %d has invalid effective-rate anchors.", year), call. = FALSE)
  }
  data.frame(year = year, bracket = points$label, n = points$n,
             income = income, eff_tax_rate = rate, source = path,
             stringsAsFactors = FALSE)
}

years <- seq.int(2007L, last_year)
points <- lapply(years, bra_rate_points)
workbook <- openxlsx::createWorkbook()
for (i in seq_along(years)) {
  sheet <- as.character(years[[i]])
  openxlsx::addWorksheet(workbook, sheet)
  openxlsx::writeData(workbook, sheet, points[[i]][, c("income", "eff_tax_rate")],
                      startCol = 14L, startRow = 25L)
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
staged <- tempfile("bra-rate-points-", tmpdir = dirname(output), fileext = ".xlsx")
on.exit(unlink(staged), add = TRUE)
openxlsx::saveWorkbook(workbook, staged)
if (!identical(readxl::excel_sheets(staged), as.character(years))) {
  stop("Could not verify the Brazilian rate-point workbook.", call. = FALSE)
}
if (!file.copy(staged, output, overwrite = TRUE)) stop("Could not write ", output, call. = FALSE)
manifest <- do.call(rbind, points)
utils::write.csv(manifest, sub("\\.xlsx$", "_manifest.csv", output), row.names = FALSE)
cat(sprintf("Prepared BRA rate inputs for %d–%d from publisher workbooks.\n",
            years[[1L]], tail(years, 1L)))
