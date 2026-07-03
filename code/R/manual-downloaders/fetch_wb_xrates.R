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
  "DINA_FETCH_WB_XRATES_URL",
  unset = "https://api.worldbank.org/v2/en/indicator/PA.NUS.FCRF?downloadformat=excel"
)

dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
if (file.exists(url)) {
  ok <- file.copy(url, target, overwrite = FALSE)
} else {
  ok <- tryCatch({
    utils::download.file(url, target, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    message(conditionMessage(e))
    FALSE
  })
}

if (!isTRUE(ok) || !file.exists(target)) {
  stop("World Bank exchange-rate download failed.", call. = FALSE)
}
cat(sprintf("wrote %s\n", target))
