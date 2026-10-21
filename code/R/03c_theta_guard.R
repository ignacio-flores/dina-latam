# Validate the observed BFM cases and publish 03c only after its complete
# panel and figures have been checked.  The existing panel supplies provenance
# for retained observations absent from the latest 03a/03b index sheets.

theta03c_key <- function(country, year) {
  if (!length(country) || !length(year)) return(character())
  paste0(country, "_", year)
}

theta03c_sha <- function(path) {
  if (!file.exists(path)) stop("Missing 03c input: ", path, call. = FALSE)
  digest::digest(file = path, algo = "sha256")
}

# The same workbook supplies many country-year sheets. Repeated file-based
# reads unzip the whole archive each time. Reuse its parsed workbook and sheets
# only while its exact contents match; timestamps never establish validity.
.theta03c_workbook_cache <- new.env(parent = emptyenv())
theta03c_book <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  sha <- theta03c_sha(path)
  entry <- if (exists(path, .theta03c_workbook_cache, inherits = FALSE)) get(path, .theta03c_workbook_cache) else NULL
  if (is.null(entry) || !identical(entry$sha256, sha)) {
    entry <- list(sha256 = sha, workbook = openxlsx::loadWorkbook(path), sheets = new.env(parent = emptyenv()))
    assign(path, entry, .theta03c_workbook_cache)
  }
  entry
}

theta03c_sheet_names <- function(path) names(theta03c_book(path)$workbook)

theta03c_read <- function(path, sheet, ...) {
  entry <- theta03c_book(path)
  arguments <- list(...)
  key <- digest::digest(list(sheet = sheet, arguments = arguments), algo = "sha256")
  if (!exists(key, entry$sheets, inherits = FALSE)) {
    data <- do.call(openxlsx::read.xlsx, c(list(xlsxFile = entry$workbook, sheet = sheet), arguments))
    assign(key, data, entry$sheets)
  }
  get(key, entry$sheets, inherits = FALSE)
}

theta03c_panel <- function(path, allow_empty = FALSE) {
  if (!file.exists(path)) stop("Missing theta panel: ", path, call. = FALSE)
  if (!"panel" %in% theta03c_sheet_names(path)) {
    stop("Theta workbook has no panel sheet: ", path, call. = FALSE)
  }
  data <- theta03c_read(path, sheet = "panel", colNames = TRUE,
                              skipEmptyRows = FALSE)
  needed <- c("country", "year", "p", "small_t", "antitonic", "mpoint")
  if (!all(needed %in% names(data)) || (!allow_empty && !nrow(data))) {
    stop("Theta panel is empty or lacks required columns: ", path, call. = FALSE)
  }
  data$country <- as.character(data$country)
  data$year <- suppressWarnings(as.integer(data$year))
  if (anyNA(data$country) || anyNA(data$year) || any(!grepl("^[A-Z]{3}$", data$country))) {
    stop("Theta panel has invalid country/year identifiers: ", path, call. = FALSE)
  }
  data$key <- theta03c_key(data$country, data$year)
  data
}

theta03c_workbooks <- function(root) {
  base <- file.path(root, "output", "bfm_summary")
  files <- c(pos = file.path(base, "bfm_norep_pos", "merging_points.xlsx"),
             pre = file.path(base, "bfm_norep_pre", "merging_points.xlsx"))
  invisible(lapply(files, theta03c_sha))
  files
}

theta03c_index <- function(files) {
  out <- list()
  available <- lapply(files, theta03c_sheet_names)
  for (kind in names(files)) {
    sheets <- available[[kind]]
    for (index in c("country_years", "country_years_2stage")) {
      if (!index %in% sheets) next
      values <- theta03c_read(files[[kind]], sheet = index, colNames = TRUE)
      if (!"country_year" %in% names(values)) {
        stop("Missing country_year column in ", files[[kind]], ":", index, call. = FALSE)
      }
      ids <- trimws(as.character(values$country_year))
      ids <- ids[!is.na(ids) & nzchar(ids)]
      if (any(!grepl("^[A-Z]{3}_[0-9]{4}$", ids))) {
        stop("Invalid observed-case identifier in ", files[[kind]], ":", index, call. = FALSE)
      }
      for (id in ids) {
        if (!is.null(out[[id]])) stop("Observed case occurs in multiple index sheets: ", id, call. = FALSE)
        sheet <- gsub("_", "", id, fixed = TRUE)
        sources <- names(available)[vapply(available, function(x) sheet %in% x, logical(1))]
        if (length(sources) != 1L) {
          stop("Listed observed case has no merging-point sheet: ", id, call. = FALSE)
        }
        parts <- strsplit(id, "_", fixed = TRUE)[[1L]]
        out[[id]] <- list(country = parts[[1L]], year = as.integer(parts[[2L]]),
                          source_file = files[[sources]], index = index,
                          provenance = "current_index")
      }
    }
  }
  out
}

theta03c_eligible <- function(root, trust, country, year, index = NULL) {
  match <- trust[trust$country == country & trust$year == year, , drop = FALSE]
  if (nrow(match) != 1L) return(list(ok = FALSE, reason = "no unique trust-region entry"))
  survey <- file.path(root, "intermediary_data", "microdata", "raw", country,
                      sprintf("%s_%s_raw.dta", country, year))
  if (!file.exists(survey)) return(list(ok = FALSE, reason = "no raw survey"))
  admin <- file.path(root, "input_data", "admin_data", country)
  normal <- file.exists(file.path(admin, sprintf("gpinter_%s_%s.xlsx", country, year))) &&
    !is.na(match$trust[[1L]])
  twostage <- any(file.exists(file.path(admin, sprintf("%s_%s_%s.xlsx",
                                              c("diverse", "wage"), country, year)))) &&
    !is.na(match$trust_wages[[1L]])
  if (identical(index, "country_years")) twostage <- FALSE
  if (identical(index, "country_years_2stage")) normal <- FALSE
  if (!normal && !twostage) {
    return(list(ok = FALSE, reason = "no matching tax input and trust region"))
  }
  list(ok = TRUE, reason = if (twostage) "two-stage" else "regular")
}

theta03c_source <- function(path, country, year) {
  sheet <- paste0(country, year)
  if (!sheet %in% theta03c_sheet_names(path)) {
    stop("Missing merging-point sheet ", sheet, " in ", path, call. = FALSE)
  }
  data <- theta03c_read(path, sheet = sheet, colNames = TRUE,
                              skipEmptyRows = FALSE)
  needed <- c("p", "small_t", "antitonic", "mpoint")
  if (!all(needed %in% names(data)) || !nrow(data) || is.na(data$mpoint[[1L]]) ||
      all(is.na(data$p))) {
    stop("Invalid merging-point data for ", country, " ", year, " in ", path,
         call. = FALSE)
  }
  data
}

theta03c_signature <- function(data, source = FALSE) {
  scale <- if (source) 100 else 1
  mpoint <- if (source) rep(as.numeric(data$mpoint[[1L]]) * scale, nrow(data)) else
    as.numeric(data$mpoint)
  as_text <- function(x) ifelse(is.na(x), "<NA>", sprintf("%.4f", round(as.numeric(x), 4)))
  sort(paste(as_text(as.numeric(data$p) * scale), as_text(data$small_t),
             as_text(data$antitonic), as_text(mpoint), sep = "|"))
}

theta03c_prepare <- function(root, stage, trust_path, first_year, last_year) {
  if (dir.exists(stage)) stop("03c staging directory already exists: ", stage, call. = FALSE)
  dir.create(file.path(stage, "figures"), recursive = TRUE, showWarnings = FALSE)
  files <- theta03c_workbooks(root)
  old_path <- file.path(root, "output", "bfm_summary", "all_thetas.xlsx")
  old <- theta03c_panel(old_path, allow_empty = TRUE)
  previous_receipt <- file.path(root, "output", "bfm_summary", "03c_validation.json")
  if (file.exists(previous_receipt)) {
    receipt <- jsonlite::fromJSON(previous_receipt)
    if (!identical(receipt$panel_sha256, theta03c_sha(old_path))) {
      stop("Published theta panel changed after its last validation", call. = FALSE)
    }
  }
  trust_file <- if (grepl("^/", trust_path)) trust_path else file.path(root, trust_path)
  trust <- utils::read.csv(trust_file, stringsAsFactors = FALSE)
  if (!all(c("country", "year", "trust", "trust_wages") %in% names(trust))) {
    stop("Admin trust snapshot has missing columns: ", trust_file, call. = FALSE)
  }
  selected <- theta03c_index(files)
  listed_count <- length(selected)
  for (id in names(selected)) {
    row <- selected[[id]]
    if (row$year < first_year || row$year > last_year) {
      stop("Listed observed case is outside the configured years: ", id, call. = FALSE)
    }
    check <- theta03c_eligible(root, trust, row$country, row$year, row$index)
    if (!check$ok) stop("Listed observed case ", id, " is ineligible: ", check$reason, call. = FALSE)
    theta03c_source(row$source_file, row$country, row$year)
  }
  excluded <- list()
  for (id in setdiff(unique(old$key), names(selected))) {
    rows <- old[old$key == id, , drop = FALSE]
    country <- rows$country[[1L]]
    year <- rows$year[[1L]]
    check <- if (year >= first_year && year <= last_year) {
      theta03c_eligible(root, trust, country, year)
    } else list(ok = FALSE, reason = "outside configured years")
    if (!check$ok) {
      excluded[[id]] <- check$reason
      next
    }
    matching <- character()
    for (path in files) {
      if (!paste0(country, year) %in% theta03c_sheet_names(path)) next
      source <- theta03c_source(path, country, year)
      if (identical(theta03c_signature(source, source = TRUE),
                    theta03c_signature(rows))) matching <- c(matching, path)
    }
    if (!length(matching)) {
      stop("Historical observed case ", id,
           " is eligible, but its merging-point sheet no longer matches the old panel",
           call. = FALSE)
    }
    selected[[id]] <- list(country = country, year = year, source_file = matching[[1L]],
                            index = "historical", provenance = "historical_panel")
  }
  ids <- sort(names(selected))
  cases <- data.frame(country = vapply(selected[ids], `[[`, "", "country"),
                      year = vapply(selected[ids], `[[`, 0L, "year"),
                      source_file = vapply(selected[ids], `[[`, "", "source_file"),
                      provenance = vapply(selected[ids], `[[`, "", "provenance"),
                      stringsAsFactors = FALSE)
  if (!nrow(cases)) stop("No eligible observed theta cases were found.", call. = FALSE)
  utils::write.csv(cases, file.path(stage, "cases.csv"), row.names = FALSE, na = "")
  context <- list(cases = cases, excluded = excluded, old_path = old_path,
                  old_sha = theta03c_sha(old_path),
                  input_sha = vapply(c(files, trust = trust_file), theta03c_sha, ""),
                  input_paths = c(files, trust = trust_file),
                  listed_count = listed_count)
  saveRDS(context, file.path(stage, "context.rds"))
  writeLines("ready", file.path(stage, "prepare.ok"))
  cat(sprintf("03c cases: %s current, %s retained historical, %s excluded historical\n",
              listed_count, nrow(cases) - listed_count, length(excluded)))
  if (length(excluded)) {
    for (id in names(excluded)) cat(sprintf("  excluded %s: %s\n", id, excluded[[id]]))
  }
}

theta03c_diff <- function(old, new) {
  fields <- intersect(c("p", "small_t", "antitonic", "mpoint", "big_t",
                        "extrap", "thr", "avg_t"), intersect(names(old), names(new)))
  fingerprint <- function(data, field) {
    x <- data[[field]]
    if (is.numeric(x)) x <- ifelse(is.na(x), "<NA>", sprintf("%.6f", round(x, 6)))
    sort(as.character(x))
  }
  ids <- sort(union(unique(old$key), unique(new$key)))
  lapply(ids, function(id) {
    a <- old[old$key == id, , drop = FALSE]
    b <- new[new$key == id, , drop = FALSE]
    changed <- if (!nrow(a) || !nrow(b)) fields else
      fields[!vapply(fields, function(field) identical(fingerprint(a, field),
                                                       fingerprint(b, field)), logical(1))]
    list(case = id, status = if (!nrow(a)) "added" else if (!nrow(b)) "removed" else
           if (length(changed)) "changed" else "unchanged",
         old_rows = nrow(a), new_rows = nrow(b), changed_fields = changed)
  })
}

theta03c_validate <- function(root, stage, context) {
  if (!identical(theta03c_sha(context$old_path), context$old_sha) ||
      !identical(vapply(context$input_paths, theta03c_sha, ""), context$input_sha)) {
    stop("03c inputs changed while the staged result was being built", call. = FALSE)
  }
  new_path <- file.path(stage, "all_thetas.xlsx")
  new <- theta03c_panel(new_path, allow_empty = TRUE)
  old <- theta03c_panel(context$old_path, allow_empty = TRUE)
  if (!all(c("avg_t", "big_t", "extrap", "thr") %in% names(new))) {
    stop("Staged theta panel lacks expected BFM or average-theta columns", call. = FALSE)
  }
  expected <- theta03c_key(context$cases$country, context$cases$year)
  if (!identical(sort(unique(new$key)), sort(expected))) {
    stop("Staged theta panel has missing or unregistered observed cases", call. = FALSE)
  }
  for (i in seq_len(nrow(context$cases))) {
    case <- context$cases[i, ]
    id <- theta03c_key(case$country, case$year)
    source <- theta03c_source(case$source_file, case$country, case$year)
    rows <- new[new$key == id, , drop = FALSE]
    if (!identical(theta03c_signature(source, source = TRUE),
                   theta03c_signature(rows))) {
      stop("Staged theta panel does not match merging-point sheet for ", id, call. = FALSE)
    }
  }
  groups <- split(seq_len(nrow(new)), paste(new$country, new$p))
  for (rows in groups) {
    values <- as.numeric(new$antitonic[rows])
    expected_median <- if (all(is.na(values))) NA_real_ else stats::median(values, na.rm = TRUE)
    actual <- as.numeric(new$avg_t[rows])
    if (any(is.na(actual) != is.na(expected_median)) ||
        any(abs(actual[!is.na(actual)] - expected_median) > 0.0001)) {
      stop("Staged average theta does not match the country-fractile median", call. = FALSE)
    }
  }
  for (country in unique(context$cases$country)) {
    for (suffix in c("", "_esp")) {
      pdf <- file.path(stage, "figures", paste0(country, suffix, ".pdf"))
      if (!file.exists(pdf) || file.info(pdf)$size < 100L ||
          !identical(rawToChar(readBin(pdf, "raw", n = 4L)), "%PDF")) {
        stop("Missing or invalid staged theta figure: ", pdf, call. = FALSE)
      }
    }
  }
  legend <- file.path(stage, "figures", "legend_esp.pdf")
  if (nrow(new) && (!file.exists(legend) || file.info(legend)$size < 100L)) {
    stop("Missing staged theta legend", call. = FALSE)
  }
  changes <- theta03c_diff(old, new)
  for (change in changes) if (change$status != "unchanged") {
    cat(sprintf("  %s %s (%s -> %s rows; %s)\n", change$status, change$case,
                change$old_rows, change$new_rows,
                paste(change$changed_fields, collapse = ",")))
  }
  list(new_path = new_path, new = new, old = old, changes = changes,
       panel_sha = theta03c_sha(new_path))
}

theta03c_publish <- function(root, stage) {
  if (!file.exists(file.path(stage, "prepare.ok"))) stop("03c preparation was not completed", call. = FALSE)
  context <- readRDS(file.path(stage, "context.rds"))
  # Recheck the small set of observed-panel inputs before publication.
  if (!identical(theta03c_sha(context$old_path), context$old_sha) ||
      !identical(vapply(context$input_paths, theta03c_sha, ""), context$input_sha)) {
    stop("03c inputs changed while the staged result was being built", call. = FALSE)
  }
  checked <- theta03c_validate(root, stage, context)
  figures <- file.path(stage, "figures")
  staged_pdfs <- list.files(figures, pattern = "\\.pdf$", full.names = TRUE)
  old_countries <- unique(checked$old$country)
  new_countries <- unique(checked$new$country)
  retired <- unlist(lapply(setdiff(old_countries, new_countries), function(country) {
    file.path(Sys.getenv("DINA_OUTPUT_ROOT", unset = root), "output", "figures", "thetas", paste0(country, c("", "_esp"), ".pdf"))
  }), use.names = FALSE)
  receipt <- list(version = 2L, generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                  panel_sha256 = checked$panel_sha,
                  countries_updated = unique(context$cases$country),
                  input_sha256 = as.list(context$input_sha),
                  cases = lapply(seq_len(nrow(context$cases)), function(i) as.list(context$cases[i, ])),
                  excluded_historical = context$excluded,
                  differences_from_prior = checked$changes)
  receipt_stage <- file.path(stage, "03c_validation.json")
  jsonlite::write_json(receipt, receipt_stage, auto_unbox = TRUE, pretty = TRUE, null = "null")
  target_base <- file.path(Sys.getenv("DINA_OUTPUT_ROOT", unset = root), "output")
  panel_target <- file.path(target_base, "bfm_summary", "all_thetas.xlsx")
  sources <- c(staged_pdfs, receipt_stage, checked$new_path)
  targets <- c(file.path(target_base, "figures", "thetas", basename(staged_pdfs)),
               file.path(target_base, "bfm_summary", "03c_validation.json"),
               panel_target)
  touched <- unique(c(targets, retired))
  backups <- file.path(stage, "backup", seq_along(touched))
  dir.create(file.path(stage, "backup"), showWarnings = FALSE)
  existed <- file.exists(touched)
  for (i in which(existed)) {
    if (!file.copy(touched[[i]], backups[[i]], overwrite = TRUE)) {
      stop("Could not back up 03c output: ", touched[[i]], call. = FALSE)
    }
  }
  committed <- FALSE
  on.exit(if (!committed) {
    for (i in seq_along(touched)) {
      if (existed[[i]]) file.copy(backups[[i]], touched[[i]], overwrite = TRUE)
      else if (file.exists(touched[[i]])) unlink(touched[[i]])
    }
  }, add = TRUE)
  for (i in seq_along(targets)[-length(targets)]) {
    dir.create(dirname(targets[[i]]), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(sources[[i]], targets[[i]], overwrite = TRUE)) {
      stop("Could not publish 03c output: ", targets[[i]], call. = FALSE)
    }
  }
  if (length(retired)) unlink(retired)
  # Install the panel last. A process interrupted during figure publication
  # leaves the previous panel in place; the final rename is atomic on macOS.
  panel_temp <- tempfile(pattern = ".03c_panel_", tmpdir = dirname(panel_target))
  on.exit(if (file.exists(panel_temp)) unlink(panel_temp), add = TRUE)
  if (!file.copy(checked$new_path, panel_temp) ||
      !identical(theta03c_sha(panel_temp), checked$panel_sha) ||
      !file.rename(panel_temp, panel_target)) {
    stop("Could not install the validated 03c panel", call. = FALSE)
  }
  if (!identical(theta03c_sha(panel_target), checked$panel_sha)) {
    stop("Published theta panel failed its checksum check", call. = FALSE)
  }
  committed <- TRUE
  writeLines("published", file.path(stage, "publish.ok"))
  cat(sprintf("Published 03c: %s observed cases, %s changed cases\n",
              nrow(context$cases), sum(vapply(checked$changes, function(x) x$status != "unchanged", logical(1)))))
}

theta03c_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) < 2L) stop("Usage: 03c_theta_guard.R prepare|publish|cleanup STAGE [TRUST FIRST_YEAR LAST_YEAR]", call. = FALSE)
  mode <- args[[1L]]
  stage <- args[[2L]]
  root <- normalizePath(getwd(), mustWork = TRUE)
  if (!grepl("dina03c", basename(stage), fixed = TRUE)) {
    stop("Unsafe 03c staging path: ", stage, call. = FALSE)
  }
  if (identical(mode, "prepare")) {
    if (length(args) != 5L) stop("03c prepare needs trust snapshot and year range", call. = FALSE)
    theta03c_prepare(root, stage, args[[3L]], as.integer(args[[4L]]), as.integer(args[[5L]]))
  } else if (identical(mode, "publish")) {
    theta03c_publish(root, stage)
  } else if (identical(mode, "cleanup")) {
    if (dir.exists(stage)) unlink(stage, recursive = TRUE, force = TRUE)
  } else stop("Unknown 03c guard operation: ", mode, call. = FALSE)
}

if (sys.nframe() == 0L) theta03c_main()
