# Shared publication scope for the WID handoff and final export checks.
# Calculation countries and regional donors remain separate from recipients.
dina_export_scope_read_yaml <- function(path) {
  text <- readLines(path, encoding = "UTF-8")
  yaml::yaml.load(paste(text, collapse = "\n"), error.label = path)
}

dina_export_publication_scope <- function(root, config, contract = NULL, history = NULL) {
  if (is.null(contract)) contract <- dina_export_scope_read_yaml(file.path(root, "config/wid_include.yml"))
  policy <- contract$publication
  if (is.null(policy)) stop("Missing export publication-scope policy.", call. = FALSE)
  map <- do.call(rbind, lapply(contract$area_metadata, function(x)
    data.frame(country = x$iso3, iso = x$wid_area, name = x$country_name, stringsAsFactors = FALSE)))
  direct <- as.character(unlist(config$countries))
  population_path <- file.path(root, contract$artifacts$population$canonical)
  if (file.exists(population_path)) {
    population <- as.data.frame(haven::read_dta(population_path, col_select = tidyselect::any_of(c("country", "year"))))
    if ("year" %in% names(population)) population <- population[population$year >= policy$population_first_year, , drop = FALSE]
    regional <- map$country[match(unique(population$country), map$name)]
    if (anyNA(regional)) stop("Unmapped country names in the accepted export population input.", call. = FALSE)
  } else {
    # Initial source acquisition can precede inclusion of the population file.
    regional <- map$country[match(unlist(contract$area_sets$population_legacy), map$iso)]
    if (anyNA(regional)) stop("Unmapped population areas in the WID contract.", call. = FALSE)
  }
  regional <- setdiff(regional, unlist(policy$regional_exclusions))
  countries <- sort(unique(c(direct, regional)))
  if (any(!countries %in% map$country)) stop("Unmapped publication country code.", call. = FALSE)
  steps <- as.character(unlist(config$export_validation$steps))
  years <- seq.int(as.integer(config$years$first), as.integer(config$export_validation$last_year))
  cases <- expand.grid(country = countries, year = years, step = steps,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (country in names(policy$first_publication_year)) {
    first <- as.integer(policy$first_publication_year[[country]])
    cases <- cases[!(cases$country == country & cases$year < first), , drop = FALSE]
  }
  if (is.null(history)) {
    env <- new.env(parent = globalenv())
    sys.source(file.path(root, "code/R/source-diagnostics/wid_common.R"), env)
    artifact <- env$wid_include_artifact(contract, "wid-export-scaling")
    history <- env$wid_include_publication_history(root, artifact)
  }
  if (nrow(history)) {
    extra <- data.frame(country = map$country[match(history$iso, map$iso)], year = as.integer(history$year),
      step = if ("step" %in% names(history)) history$step else steps[1L], stringsAsFactors = FALSE)
    extra <- extra[extra$step %in% steps & extra$year <= max(years), , drop = FALSE]
    cases <- unique(rbind(cases, extra))
  }
  cases$iso <- map$iso[match(cases$country, map$country)]
  cases <- cases[order(cases$country, cases$year, cases$step), c("country", "iso", "year", "step")]
  rownames(cases) <- NULL
  cases
}

dina_export_scope_cli <- function(args) {
  if (length(args) != 2L) stop("Expected export request file and output directory.")
  root <- normalizePath(getwd())
  request <- readLines(args[1L], warn = FALSE)
  if (length(request) != 7L) stop("Incomplete export settings request.")
  words <- function(x) strsplit(trimws(gsub('"', '', x, fixed = TRUE)), "[[:space:]]+")[[1L]]
  config <- list(countries = words(request[5L]), years = list(first = as.integer(request[6L]), last = as.integer(request[7L])),
    export_validation = list(steps = words(request[3L]), last_year = as.integer(request[4L])))
  contract <- dina_export_scope_read_yaml(file.path(root, "config/wid_include.yml"))
  cases <- dina_export_publication_scope(root, config, contract)
  dir.create(args[2L], recursive = TRUE, showWarnings = FALSE)
  write.csv(cases, file.path(args[2L], "publication_scope.csv"), row.names = FALSE)
  cases$consistency_publish <- 1L
  haven::write_dta(cases, file.path(args[2L], "publication_scope.dta"))
  writeLines(paste0('global dina_export_regional_exclusions "',
    paste(unlist(contract$publication$regional_exclusions), collapse = " "), '"'), file.path(args[2L], "scope.do"))
  invisible(cases)
}

if (sys.nframe() == 0L) dina_export_scope_cli(commandArgs(trailingOnly = TRUE))
