#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Fetchers are called by `dina sources fetch` with simple flag/value pairs.
arg_value <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit) || hit[[1L]] >= length(args)) default else args[[hit[[1L]] + 1L]]
}

target <- arg_value("--target")
if (!nzchar(target)) {
  stop("Missing --target.", call. = FALSE)
}
source_id <- arg_value("--source-id", "population")
repo_root <- arg_value("--repo-root", getwd())

# Keep dependency failures explicit; this script is intentionally standalone.
need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

parse_years <- function(value) {
  value <- trimws(value %||% "")
  if (!nzchar(value)) {
    return(default_years(repo_root))
  }
  parts <- trimws(unlist(strsplit(value, ",", fixed = TRUE), use.names = FALSE))
  years <- unlist(lapply(parts, function(part) {
    bounds <- suppressWarnings(as.integer(trimws(unlist(strsplit(part, ":", fixed = TRUE), use.names = FALSE))))
    if (length(bounds) == 2L && !any(is.na(bounds))) {
      return(seq.int(bounds[[1L]], bounds[[2L]]))
    }
    suppressWarnings(as.integer(part))
  }), use.names = FALSE)
  years <- sort(unique(years[!is.na(years)]))
  if (!length(years)) {
    stop("DINA_FETCH_WID_POPULATION_YEARS did not contain any valid years.", call. = FALSE)
  }
  years
}

need("haven")
need("yaml")
need("wid")

# The canonical PopulationLatAm file starts in 1990. By default, extend the WID
# fetch through the project run horizon so final exports have population rows
# for every configured pipeline year. Validation reruns can still force a
# legacy window with DINA_FETCH_WID_POPULATION_YEARS=1990:2020.
default_years <- function(root) {
  config_path <- Sys.getenv(
    "DINA_CONFIG_YML",
    unset = file.path(root, "config", "dina.yml")
  )
  config <- tryCatch(
    yaml::read_yaml(config_path),
    error = function(e) {
      stop("Could not read DINA config for default WID population years: ", conditionMessage(e), call. = FALSE)
    }
  )
  last_year <- suppressWarnings(as.integer(config$years$last))
  if (is.na(last_year) || last_year < 1990L) {
    stop("DINA config must define years.last >= 1990 for the WID population fetcher.", call. = FALSE)
  }
  1990:last_year
}

years <- parse_years(Sys.getenv("DINA_FETCH_WID_POPULATION_YEARS", unset = ""))

# Start from the legacy PopulationLatAm country/region coverage and map each
# name to WID's two-letter area code. Three territories are filtered below
# because WID currently returns no npopul rows for them.
area_map <- data.frame(
  area = c(
    "AG", "AR", "AW", "BS", "BB", "BZ", "BO", "BR", "CL", "CO",
    "CR", "CU", "CW", "DO", "EC", "SV", "GF", "GD", "GP", "GT",
    "GY", "HT", "HN", "JM", "MQ", "MX", "NI", "PA", "PY", "PE",
    "PR", "LC", "VC", "SR", "TT", "VI", "UY", "VE"
  ),
  country = c(
    "Antigua and Barbuda",
    "Argentina",
    "Aruba",
    "Bahamas",
    "Barbados",
    "Belize",
    "Bolivia (Plurinational State of)",
    "Brazil",
    "Chile",
    "Colombia",
    "Costa Rica",
    "Cuba",
    "Curaçao",
    "Dominican Republic",
    "Ecuador",
    "El Salvador",
    "French Guiana",
    "Grenada",
    "Guadeloupe",
    "Guatemala",
    "Guyana",
    "Haiti",
    "Honduras",
    "Jamaica",
    "Martinique",
    "Mexico",
    "Nicaragua",
    "Panama",
    "Paraguay",
    "Peru",
    "Puerto Rico",
    "Saint Lucia",
    "Saint Vincent and the Grenadines",
    "Suriname",
    "Trinidad and Tobago",
    "United States Virgin Islands",
    "Uruguay",
    "Venezuela (Bolivarian Republic of)"
  ),
  region = c(
    "Caribbean",
    "South America",
    "Caribbean",
    "Caribbean",
    "Caribbean",
    "Central America",
    "South America",
    "South America",
    "South America",
    "South America",
    "Central America",
    "Caribbean",
    "Caribbean",
    "Caribbean",
    "South America",
    "Central America",
    "South America",
    "Caribbean",
    "Caribbean",
    "Central America",
    "South America",
    "Caribbean",
    "Central America",
    "Caribbean",
    "Caribbean",
    "Central America",
    "Central America",
    "Central America",
    "South America",
    "South America",
    "Caribbean",
    "Caribbean",
    "Caribbean",
    "South America",
    "Caribbean",
    "Caribbean",
    "South America",
    "South America"
  ),
  stringsAsFactors = FALSE
)

# Do not collect all-missing WID output for these legacy population territories.
excluded_areas <- c("GF", "GP", "MQ")
area_map <- area_map[!area_map$area %in% excluded_areas, , drop = FALSE]
area_map$order <- seq_len(nrow(area_map))

# A narrow area override is useful for smoke tests and targeted rechecks.
area_override <- Sys.getenv("DINA_FETCH_WID_POPULATION_AREAS", unset = "")
if (nzchar(area_override)) {
  requested <- trimws(toupper(unlist(strsplit(area_override, ",", fixed = TRUE), use.names = FALSE)))
  area_map <- area_map[area_map$area %in% requested, , drop = FALSE]
  if (!nrow(area_map)) {
    stop("DINA_FETCH_WID_POPULATION_AREAS did not match any configured LatAm WID areas.", call. = FALSE)
  }
}

# WID age 999 is total population; age 992 is adults, matching the existing
# WID macro-data convention in code/Stata/01a-clean-macro-data.do.
required_vars <- c(totalpop = "npopul999i", adultpop = "npopul992i")

# Download the two population series in one call so total/adult coverage is
# validated from the same WID snapshot.
raw <- tryCatch(
  wid::download_wid(
    indicators = "npopul",
    areas = area_map$area,
    years = years,
    ages = c("999", "992"),
    pop = "i",
    metadata = FALSE,
    verbose = FALSE
  ),
  error = function(e) {
    stop("WID population download failed: ", conditionMessage(e), call. = FALSE)
  }
)

if (is.null(raw) || !nrow(raw)) {
  stop("WID returned no population rows for npopul and the configured LatAm areas.", call. = FALSE)
}

missing_columns <- setdiff(c("country", "variable", "year", "value"), names(raw))
if (length(missing_columns)) {
  stop("WID response is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

raw$country <- toupper(as.character(raw$country))
raw$variable <- as.character(raw$variable)
raw$year <- suppressWarnings(as.integer(raw$year))
raw$value <- suppressWarnings(as.numeric(raw$value))

# Fail if the API shape changed enough that one of the requested age concepts
# is not present at all.
present_required <- intersect(unname(required_vars), unique(raw$variable))
missing_required <- setdiff(unname(required_vars), present_required)
if (length(missing_required)) {
  stop(
    "WID response is missing required age/population series: ",
    paste(missing_required, collapse = ", "),
    call. = FALSE
  )
}

raw <- raw[
  raw$country %in% area_map$area &
    raw$year %in% years &
    raw$variable %in% unname(required_vars),
  c("country", "year", "variable", "value"),
  drop = FALSE
]
if (!nrow(raw)) {
  stop("WID returned population rows, but none matched the configured LatAm areas and years.", call. = FALSE)
}

# There should be a single value per area/year/series, but collapse defensively
# in case WID ever returns duplicate rows.
collapse_first <- function(x) {
  x <- x[!is.na(x)]
  if (length(x)) x[[1L]] else NA_real_
}
raw <- stats::aggregate(value ~ country + year + variable, data = raw, FUN = collapse_first)

# Build the full validation grid so any remaining WID coverage gaps are visible
# as missing cells instead of disappearing from the output.
grid <- expand.grid(
  country = area_map$area,
  year = years,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

total <- raw[raw$variable == required_vars[["totalpop"]], c("country", "year", "value"), drop = FALSE]
names(total)[names(total) == "value"] <- "totalpop"
adult <- raw[raw$variable == required_vars[["adultpop"]], c("country", "year", "value"), drop = FALSE]
names(adult)[names(adult) == "value"] <- "adultpop"

out <- merge(grid, total, by = c("country", "year"), all.x = TRUE, sort = FALSE)
out <- merge(out, adult, by = c("country", "year"), all.x = TRUE, sort = FALSE)
out <- merge(out, area_map, by.x = "country", by.y = "area", all.x = TRUE, sort = FALSE)
out <- out[order(out$order, out$year), , drop = FALSE]
out <- out[, c("country.y", "region", "year", "totalpop", "adultpop")]
names(out)[names(out) == "country.y"] <- "country"
rownames(out) <- NULL

# Preserve the same core Stata labels as the old PopulationLatAm.dta.
attr(out$country, "label") <- "Location"
attr(out$region, "label") <- "region in Latin America & Caribbean"
attr(out$totalpop, "label") <- "Total Population"
attr(out$adultpop, "label") <- "Adult Population (20+)"

missing_total <- sum(is.na(out$totalpop))
missing_adult <- sum(is.na(out$adultpop))
if (missing_total || missing_adult) {
  total_keys <- paste(out$country[is.na(out$totalpop)], out$year[is.na(out$totalpop)], sep = " ")
  adult_keys <- paste(out$country[is.na(out$adultpop)], out$year[is.na(out$adultpop)], sep = " ")
  sample_missing <- paste(head(unique(c(total_keys, adult_keys)), 10L), collapse = "; ")
  stop(
    sprintf(
      "WID output is missing %s totalpop and %s adultpop area-year values. Examples: %s",
      missing_total,
      missing_adult,
      sample_missing
    ),
    call. = FALSE
  )
}

dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
haven::write_dta(out, target)
cat(sprintf("wrote %s rows to %s for %s\n", nrow(out), target, source_id))
