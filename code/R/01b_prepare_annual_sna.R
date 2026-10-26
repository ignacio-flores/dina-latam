#!/usr/bin/env Rscript
# Assemble accepted annual CEI tables for the existing Stata 01b extractor.
# Source selection, sheets, ranges and units come from the source-family contract.
# Accepted source workbooks are read only; this file is a disposable derived input.
dina_prepare_annual_sna <- function(country, first, last, output, root = getwd()) {
  engine <- new.env(parent = globalenv())
  sys.source(file.path(root, "code/R/source-diagnostics/country_sna_include.R"), engine)
  contract <- engine$country_sna_include_read_contract(root)
  rule <- contract$country_rules[[country]]
  if (is.null(rule) || !identical(rule$adapter, "workbook_table") ||
      !identical(rule$source$type, "year_pattern")) {
    stop("Annual CEI preparation requires a workbook_table/year_pattern contract: ", country)
  }
  years <- seq.int(as.integer(first), as.integer(last))
  wb <- openxlsx::createWorkbook()
  supplied <- list()
  coverage <- integer()
  for (year in years) {
    layout <- engine$country_sna_include_layout_for_year(rule, year)
    source <- engine$country_sna_include_resolve_source(layout$source %||% rule$source, year, root)
    if (identical(source$status, "missing_file")) next
    sheet <- engine$country_sna_include_resolve_sheet(source$file, year, layout$sheet)
    if (!identical(sheet$status, "matched")) stop("Missing CEI sheet: ", country, " ", year)
    part <- engine$country_sna_include_extract_year(country, year, contract, root)
    if (!identical(part$source_match$table_health, "read_ok") ||
        any(part$values$extract_status %in% c("duplicate_conflict", "non_numeric"))) {
      stop("Invalid accepted CEI table: ", country, " ", year)
    }
    missing <- setdiff(part$values$variable[!is.finite(part$values$value_standardized)],
      unlist(rule$assembly_allow_missing %||% character()))
    if (length(missing)) stop("Missing CEI components: ", country, " ", year, ": ", paste(missing, collapse = ", "))
    raw <- suppressMessages(readxl::read_excel(source$file, sheet = sheet$sheet,
      range = layout$range, col_names = FALSE, .name_repair = "unique_quiet"))
    origin <- engine$country_sna_include_parse_cell_range(layout$range)
    openxlsx::addWorksheet(wb, as.character(year))
    openxlsx::writeData(wb, as.character(year), as.data.frame(raw),
      startRow = origin$first_row, startCol = origin$first_col,
      colNames = FALSE, rowNames = FALSE, keepNA = FALSE)
    supplied[[as.character(year)]] <- part$values
    coverage <- c(coverage, year)
  }
  if (!length(coverage)) stop("No accepted annual CEI tables found for ", country)
  gaps <- setdiff(seq.int(min(coverage), max(coverage)), coverage)
  if (length(gaps)) stop("Missing annual CEI tables inside coverage: ", paste(gaps, collapse = ", "))
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  candidate <- tempfile(".cei-", tmpdir = dirname(output), fileext = ".xlsx")
  on.exit(unlink(candidate), add = TRUE)
  openxlsx::saveWorkbook(wb, candidate, overwrite = TRUE)
  # Re-extract every consumed component, including sector values and ratios.
  # The assembled workbook must preserve the accepted source table exactly.
  copied_contract <- contract
  copied_contract$country_rules[[country]]$source <- list(type = "single_stem", stem = candidate)
  for (i in seq_along(copied_contract$country_rules[[country]]$layouts)) {
    copied_contract$country_rules[[country]]$layouts[[i]]$source <- NULL
    copied_contract$country_rules[[country]]$layouts[[i]]$sheet <- list(type = "year_variants", variants = "{year}")
  }
  for (year in coverage) {
    expected <- supplied[[as.character(year)]]
    copied <- engine$country_sna_include_extract_year(country, year, copied_contract, root)$values
    if (!identical(expected$variable, copied$variable) ||
        !isTRUE(all.equal(expected$value_standardized, copied$value_standardized,
          tolerance = 1e-12, check.attributes = FALSE))) {
      stop("Assembled CEI values differ from accepted input: ", country, " ", year)
    }
  }
  if (file.exists(output) && !file.copy(output, paste0(output, ".previous"), overwrite = TRUE)) {
    stop("Could not back up the previous derived CEI workbook.")
  }
  if (!file.rename(candidate, output)) stop("Could not publish the validated derived CEI workbook.")
  cat(country, "CEI:", length(coverage), "annual tables,", min(coverage), "–", max(coverage),
    "; all consumed components preserved.\n")
  invisible(list(years = coverage, values = do.call(rbind, supplied), output = output))
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 5L) stop("Expected country, first year, last year, output workbook and completion file.")
  dina_prepare_annual_sna(args[1L], as.integer(args[2L]), as.integer(args[3L]), args[4L])
  writeLines("complete", args[5L])
}
