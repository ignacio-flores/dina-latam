# Internal Stata bridge: retain the inherited country worker and staged output.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !nzchar(args[[1L]])) {
  stop("Expected a fresh 07c completion-marker path.", call. = FALSE)
}
if (!identical(Sys.getenv("DINA_COUNTRY_WORKER"), "07c-interpolate-ineqstats")) {
  stop("07c interpolation bridge requires its country worker.", call. = FALSE)
}
completion_file <- args[[1L]]
if (file.exists(completion_file)) unlink(completion_file)
# Stata may launch R in the C locale. source() can otherwise truncate UTF-8
# comments with only a warning, so establish a UTF-8 parser locale explicitly.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (locale in c("C.UTF-8", "en_US.UTF-8", "UTF-8")) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (isTRUE(l10n_info()[["UTF-8"]])) break
  }
  if (!isTRUE(l10n_info()[["UTF-8"]])) stop("07c requires a UTF-8 R locale.", call. = FALSE)
}
source("code/R/07c_interpolate_ineqstats.R", encoding = "UTF-8")
# The interpolation script validates selected group coverage before returning.
required <- file.path(Sys.getenv("DINA_OUTPUT_ROOT"),
  c("_coverage.csv", "_groups.csv", "output/ineqstats/_gpinter_all_topcorrected.csv"))
if (!all(file.exists(required))) stop("07c did not finish writing its staged results.", call. = FALSE)
writeLines("07c interpolation completed", completion_file)
