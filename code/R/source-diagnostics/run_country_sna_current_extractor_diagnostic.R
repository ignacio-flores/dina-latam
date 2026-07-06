#!/usr/bin/env Rscript

cmd <- commandArgs(trailingOnly = FALSE)
file_arg <- cmd[grepl("^--file=", cmd)]
script_path <- if (length(file_arg)) {
  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
  file.path(getwd(), "code", "R", "source-diagnostics", "run_country_sna_current_extractor_diagnostic.R")
}
source(file.path(dirname(script_path), "country_sna_current_extractor_diagnostic.R"))

result <- run_country_sna_current_extractor_diagnostic()
cat("Country-SNA diagnostic written to:\n")
cat(result$workbook_path, "\n")
