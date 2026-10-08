# Interpolate the Admin PIT tabulations locally with the same gpinter functions
# and g-percentile grid used by gpinter's Excel application.

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

dina_admin_gpinter_specs <- function() {
  data.frame(
    country = c("SLV", "PER", "DOM", "BRA", "CHL", "COL", "ECU"),
    clean = c("total-SLV.xlsx", "total-PER.xlsx", "total-pos-DOM.xlsx",
              "total-pre-BRA.xlsx", "total-pos-CHL.xlsx", "total-pos-COL.xlsx",
              "total-pos-ECU.xlsx"),
    output = c("total-pos-SLV.xlsx", "total-pos-PER.xlsx", "total-pos-DOM.xlsx",
               "total-pre-BRA.xlsx", "total-pos-CHL.xlsx", "total-pos-COL.xlsx",
               "total-pos-ECU.xlsx"),
    stringsAsFactors = FALSE
  )
}

dina_admin_gpinter_grid <- function() {
  c(seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
    seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001))
}

dina_admin_gpinter_fit <- function(input, country, year, clean_path = NULL) {
  required <- c("p", "bracketavg", "average")
  missing <- setdiff(required, names(input))
  if (length(missing)) stop(sprintf("%s %s _clean sheet lacks %s.", country, year, paste(missing, collapse = ", ")), call. = FALSE)
  p <- as.numeric(input$p)
  bracketavg <- as.numeric(input$bracketavg)
  average <- as.numeric(input$average[which(!is.na(input$average))[1L]])
  if (length(average) == 1L && is.finite(average) && average == 0 && identical(country, "BRA")) {
    # The retained 2000/2002/2006 BRA tables carry a placeholder zero in
    # _clean. Their publisher workbooks contain the genuine total average.
    source <- file.path(dirname(dirname(clean_path)), sprintf("ptot_%s.xlsx", year))
    if (!file.exists(source)) stop(sprintf("BRA %s needs %s for its total average.", year, source), call. = FALSE)
    original <- readxl::read_excel(source)
    average <- as.numeric(original$average[[1L]])
  }
  if (!length(p) || any(!is.finite(p)) || any(diff(p) <= 0) || any(p < 0 | p >= 1) ||
      any(!is.finite(bracketavg)) || length(average) != 1L || !is.finite(average) || average <= 0) {
    stop(sprintf("%s %s _clean sheet has invalid gpinter inputs.", country, year), call. = FALSE)
  }
  if ("thr" %in% names(input)) {
    threshold <- as.numeric(input$thr)
    if (any(!is.finite(threshold))) stop(sprintf("%s %s has invalid thresholds.", country, year), call. = FALSE)
    gpinter::tabulation_fit(p = p, threshold = threshold, average = average, bracketavg = bracketavg)
  } else {
    gpinter::shares_fit(p = p, average = average, bracketavg = bracketavg)
  }
}

dina_admin_gpinter_sheet <- function(input, fit, tabulation, country, year, grid) {
  n <- length(grid)
  component <- if ("component" %in% names(input)) {
    value <- as.character(input$component[which(!is.na(input$component) & nzchar(as.character(input$component)))[1L]])
    if (length(value)) value else NULL
  } else NULL
  out <- data.frame(year = c(as.integer(year), rep(NA_integer_, n - 1L)),
                    country = c(country, rep(NA_character_, n - 1L)),
                    stringsAsFactors = FALSE)
  if (!is.null(component)) out$component <- c(component, rep(NA_character_, n - 1L))
  out$average <- c(fit$average, rep(NA_real_, n - 1L))
  out$p <- grid
  out$thr <- tabulation$threshold
  out$topsh <- tabulation$top_share
  out$topavg <- tabulation$top_average
  out$bracketavg <- tabulation$bracket_average
  out$b <- tabulation$invpareto
  if (any(!is.finite(out$thr)) || any(!is.finite(out$topavg)) || any(!is.finite(out$bracketavg))) {
    stop(sprintf("%s %s gpinter returned non-finite thresholds or averages.", country, year), call. = FALSE)
  }
  label <- paste(c(component, country, year), collapse = ", ")
  list(label = label, data = out, component = component)
}

dina_admin_gpinter_series_row <- function(fit, tabulation, country, year, component) {
  data.frame(
    "Country" = country, "Component" = component %||% "n.a.", "Year" = as.integer(year),
    "Average" = fit$average,
    "Bottom 50%" = gpinter::bottom_share(fit, 0.5),
    "Middle 40%" = gpinter::bracket_share(fit, 0.5, 0.9),
    "Top 10%" = gpinter::top_share(fit, 0.9),
    "Top 1%" = gpinter::top_share(fit, 0.99),
    "Gini" = gpinter::gini(fit),
    "P10/average" = tabulation$p10_average,
    "P50/average" = tabulation$p50_average,
    "P90/average" = tabulation$p90_average,
    "P99/average" = tabulation$p99_average,
    "b(10%)" = tabulation$b10, "b(50%)" = tabulation$b50,
    "b(90%)" = tabulation$b90, "b(99%)" = tabulation$b99,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

dina_admin_gpinter_workbook <- function(clean_path, output_path, country, first_year, last_year,
                                         excluded_years = integer()) {
  sheets <- readxl::excel_sheets(clean_path)
  years <- sheets[grepl("^[0-9]{4}$", sheets) & as.integer(sheets) >= first_year & as.integer(sheets) <= last_year]
  years <- setdiff(years, as.character(excluded_years))
  if (!length(years)) stop(sprintf("%s _clean workbook has no sheets in %s–%s.", country, first_year, last_year), call. = FALSE)
  grid <- dina_admin_gpinter_grid()
  wb <- openxlsx::createWorkbook()
  results <- vector("list", length(years))
  series <- vector("list", length(years))
  auxiliary_sources <- character()
  for (i in seq_along(years)) {
    year <- years[[i]]
    input <- readxl::read_excel(clean_path, sheet = year)
    if (identical(country, "BRA") && isTRUE(as.numeric(input$average[[1L]]) == 0)) {
      auxiliary_sources <- c(auxiliary_sources, file.path(dirname(dirname(clean_path)), sprintf("ptot_%s.xlsx", year)))
    }
    fit <- tryCatch(dina_admin_gpinter_fit(input, country, year, clean_path),
                    error = function(e) stop(sprintf("gpinter failed for %s %s: %s", country, year, conditionMessage(e)), call. = FALSE))
    tabulation <- gpinter::generate_tabulation(fit, grid)
    result <- dina_admin_gpinter_sheet(input, fit, tabulation, country, year, grid)
    results[[i]] <- result
    series[[i]] <- dina_admin_gpinter_series_row(fit, tabulation, country, year, result$component)
  }
  openxlsx::addWorksheet(wb, "series")
  openxlsx::writeData(wb, "series", do.call(rbind, series))
  for (result in results) {
    openxlsx::addWorksheet(wb, result$label)
    openxlsx::writeData(wb, result$label, result$data)
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  expected <- vapply(results, function(x) x$label, character(1))
  if (!identical(readxl::excel_sheets(output_path), c("series", expected))) {
    stop(sprintf("Could not verify the generated %s gpinter workbook.", country), call. = FALSE)
  }
  attr(years, "auxiliary_sources") <- unique(auxiliary_sources)
  invisible(years)
}

dina_admin_gpinter_run <- function(root = getwd(), config, output_root = file.path(root, "input_data", "admin_data")) {
  for (package in c("gpinter", "readxl", "openxlsx", "digest", "jsonlite", "yaml")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Required R package is missing: ", package, call. = FALSE)
  }
  root <- normalizePath(root, mustWork = TRUE)
  selected <- toupper(as.character(config$countries %||% character()))
  method_path <- file.path(root, "config", "admin_gpinter.yml")
  if (!file.exists(method_path)) stop("Admin gpinter method config is missing: ", method_path, call. = FALSE)
  method <- yaml::read_yaml(method_path)
  excluded <- toupper(as.character(unlist(method$excluded_countries %||% character(), use.names = FALSE)))
  specs <- dina_admin_gpinter_specs()
  specs <- specs[specs$country %in% selected, , drop = FALSE]
  country_status <- data.frame(country = specs$country,
                               status = ifelse(specs$country %in% excluded, "excluded", "generated"),
                               stringsAsFactors = FALSE)
  specs <- specs[!specs$country %in% excluded, , drop = FALSE]
  first_year <- as.integer(config$years$first)
  last_year <- as.integer(config$years$last)
  if (!is.finite(first_year) || !is.finite(last_year) || first_year > last_year) stop("Invalid gpinter year range.", call. = FALSE)
  specs$clean_path <- file.path(root, "input_data", "admin_data", specs$country, "_clean", specs$clean)
  missing <- specs$clean_path[!file.exists(specs$clean_path)]
  if (length(missing)) {
    stop("Admin gpinter needs current _clean input workbook(s):\n  - ",
         paste(sub(paste0("^", root, "/"), "", missing), collapse = "\n  - "), call. = FALSE)
  }
  stage <- tempfile("dina-admin-gpinter-")
  dir.create(stage, recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  rows <- vector("list", nrow(specs))
  for (i in seq_len(nrow(specs))) {
    country <- specs$country[[i]]
    source <- specs$clean_path[[i]]
    staged <- file.path(stage, country, specs$output[[i]])
    cat(sprintf("Interpolating %s from %s\n", country, source))
    excluded_years <- as.integer(unlist(method$excluded_years[[country]] %||% integer(), use.names = FALSE))
    years <- dina_admin_gpinter_workbook(source, staged, country, first_year, last_year, excluded_years)
    auxiliary_sources <- attr(years, "auxiliary_sources")
    rows[[i]] <- list(country = country, clean = source, staged = staged,
                      output = file.path(output_root, country, "gpinter_output", specs$output[[i]]),
                      years = as.integer(years), excluded_years = excluded_years,
                      auxiliary_sources = lapply(auxiliary_sources, function(path) list(
                        path = substring(path, nchar(root) + 2L),
                        sha256 = digest::digest(file = path, algo = "sha256")
                      )),
                      clean_sha256 = digest::digest(file = source, algo = "sha256"))
  }
  # Fit and verify every country before writing any canonical workbook.
  for (row in rows) {
    dir.create(dirname(row$output), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(row$staged, row$output, overwrite = TRUE)) stop("Could not promote ", row$output, call. = FALSE)
  }
  manifest <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    gpinter_version = as.character(utils::packageVersion("gpinter")),
    first_year = first_year, last_year = last_year,
    excluded_countries = intersect(selected, excluded),
    workbooks = lapply(rows, function(row) list(
      country = row$country,
      clean = sub(paste0("^", root, "/"), "", row$clean),
      clean_sha256 = row$clean_sha256,
      output = sub(paste0("^", root, "/"), "", row$output),
      output_sha256 = digest::digest(file = row$output, algo = "sha256"),
      years = row$years, excluded_years = row$excluded_years,
      auxiliary_sources = row$auxiliary_sources
    ))
  )
  manifest_path <- file.path(root, "output", "data_reports", "admin_gpinter_manifest.json")
  country_status_path <- file.path(root, "output", "data_reports", "admin_gpinter_countries.csv")
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE)
  utils::write.csv(country_status, country_status_path, row.names = FALSE)
  cat(sprintf("Wrote %s\n", manifest_path))
  invisible(manifest)
}
