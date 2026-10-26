# Publication helpers for one explicit country run. Only changed outputs are staged.
country_output_country <- function(data) {
  column <- intersect(c("country", "iso3", "iso", "country_year"), tolower(names(data)))
  if (!length(column)) return(NULL)
  value <- as.character(data[[match(column[[1L]], tolower(names(data)))]])
  toupper(substr(value, 1L, 3L))
}

country_output_path_owned <- function(paths, country) {
  pattern <- paste0("(^|/)", country, "(/|[_.-])|[_-]", country, "([_.-]|$)|",
    "(^|/)", country, "[0-9]{4}([_.-]|$)")
  grepl(pattern, paths, perl = TRUE)
}

country_output_rows <- function(old, new, countries, income_scope = NULL) {
  strip_blank <- function(data) {
    ids <- country_output_country(data)
    if (is.null(ids)) return(data)
    blank <- is.na(ids) | !nzchar(trimws(ids))
    if (any(blank)) {
      rows <- data[blank, , drop = FALSE]
      empty <- apply(rows, 1L, function(row) all(is.na(row) | trimws(as.character(row)) %in% c("", ".")))
      if (any(!empty)) stop("A shared country table has data without a country identifier", call. = FALSE)
      data <- data[!blank, , drop = FALSE]
    }
    data
  }
  old <- strip_blank(old)
  new <- strip_blank(new)
  a <- country_output_country(old)
  b <- country_output_country(new)
  if (is.null(b)) stop("A shared country table has no country identifier", call. = FALSE)
  if (any(!b %in% countries)) stop("Worker produced an unselected country's rows", call. = FALSE)
  if (nrow(old) && is.null(a)) stop("Existing table has no country identifier", call. = FALSE)
  if (is.null(income_scope)) {
    old <- old[!a %in% countries, , drop = FALSE]
  } else {
    dimensions <- c("country", "unit", if ("step" %in% names(new)) "step")
    if (!all(dimensions %in% names(income_scope)) ||
        !all(dimensions %in% names(new)) ||
        (nrow(old) && !all(dimensions %in% names(old)))) {
      stop("Synthetic income publication needs country, unit and applicable step identifiers")
    }
    if (anyNA(income_scope[, dimensions, drop = FALSE]) ||
        any(!income_scope$country %in% countries)) stop("Invalid synthetic income publication scope")
    key <- function(data) do.call(paste, c(data[, dimensions, drop = FALSE], sep = "|"))
    selected <- key(income_scope)
    if (any(!key(new) %in% selected)) stop("Worker produced an unselected income step or unit")
    if (nrow(old)) old <- old[!key(old) %in% selected, , drop = FALSE]
  }
  columns <- union(names(old), names(new))
  for (column in setdiff(columns, names(old))) old[[column]] <- rep(NA, nrow(old))
  for (column in setdiff(columns, names(new))) new[[column]] <- rep(NA, nrow(new))
  rbind(old[, columns, drop = FALSE], new[, columns, drop = FALSE])
}

country_output_excel_attributes <- function(wb) {
  # openxlsx can load omitted Stata display settings as NA and a default
  # font colour as #RRGGBB. Its writer then emits invalid OOXML attributes,
  # which Stata rejects even though the worksheet cells are intact.
  for (i in seq_along(wb$worksheets)) {
    wb$worksheets[[i]]$sheetViews <- gsub(' showGridLines="NA"', "",
      wb$worksheets[[i]]$sheetViews, fixed = TRUE)
  }
  wb$styles$fonts <- gsub('rgb="#([[:xdigit:]]{6})"', 'rgb="FF\\1"', wb$styles$fonts)
  if (length(wb$styles$fonts) && !grepl("<color", wb$styles$fonts[[1L]], fixed = TRUE)) {
    # Without an explicit base-font colour, openxlsx synthesizes #000000
    # later during save, after the existing styles have been normalized.
    wb$styles$fonts[[1L]] <- sub("</font>", '<color rgb="FF000000"/></font>',
      wb$styles$fonts[[1L]], fixed = TRUE)
  }
  for (i in seq_along(wb$styleObjects)) {
    colour <- wb$styleObjects[[i]]$style$fontColour
    if ("rgb" %in% names(colour) && grepl("^#[[:xdigit:]]{6}$", colour[["rgb"]])) {
      colour[["rgb"]] <- paste0("FF", substring(colour[["rgb"]], 2L))
      wb$styleObjects[[i]]$style$fontColour <- colour
    }
  }
  invisible(wb)
}

country_output_workbook <- function(old, new, destination, countries, task_id) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("openxlsx is required")
  wb <- if (file.exists(old)) openxlsx::loadWorkbook(old) else openxlsx::createWorkbook()
  incoming <- openxlsx::getSheetNames(new)
  path_owned <- any(vapply(countries, function(country) country_output_path_owned(old, country), logical(1)))
  if (path_owned) {
    if (!file.copy(new, destination, overwrite = TRUE)) stop("Could not publish country-specific workbook")
    return(invisible(destination))
  }
  detail_country <- function(sheet) {
    if (grepl("^[A-Z]{3}[_]?[0-9]{4}", sheet)) substr(sheet, 1L, 3L) else ""
  }
  # A new statistics or synthetic contribution replaces that country's complete
  # eligible contribution. BFM observed sheets retain approved historical cases.
  retire <- startsWith(task_id, "06a") || startsWith(task_id, "07a") || startsWith(task_id, "03e")
  if (retire) for (sheet in names(wb)) {
    selected <- detail_country(sheet) %in% countries
    if (startsWith(task_id, "03e")) selected <- selected && grepl("_extrap$", sheet)
    if (selected && !sheet %in% incoming) openxlsx::removeWorksheet(wb, sheet)
  }
  for (sheet in incoming) {
    data <- openxlsx::read.xlsx(new, sheet = sheet, check.names = FALSE)
    detail <- detail_country(sheet)
    if (nzchar(detail) && !detail %in% countries) stop("Unselected sheet in staged workbook: ", sheet)
    if (startsWith(task_id, "03e") && nzchar(detail) && !grepl("_extrap$", sheet)) {
      stop("03e attempted to replace an observed merging-point sheet: ", sheet)
    }
    # Full exchange-rate inputs are intentionally included with every export.
    full_table <- identical(sheet, "inflation-xrates")
    if (!nzchar(detail) && !full_table && !is.null(country_output_country(data))) {
      previous <- if (sheet %in% names(wb)) openxlsx::read.xlsx(wb, sheet, check.names = FALSE) else data[FALSE, , drop = FALSE]
      data <- country_output_rows(previous, data, countries)
    } else if (!nzchar(detail) && !full_table && sheet %in% names(wb) && is.null(country_output_country(data))) {
      previous <- openxlsx::read.xlsx(wb, sheet, check.names = FALSE)
      if (!isTRUE(all.equal(previous, data, check.attributes = FALSE))) {
        stop("Cannot safely merge shared sheet without a country key: ", sheet)
      }
    }
    if (sheet %in% names(wb)) openxlsx::removeWorksheet(wb, sheet)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, data, keepNA = FALSE)
  }
  if (!length(names(wb))) stop("Refusing to publish an empty workbook")
  country_output_excel_attributes(wb)
  openxlsx::saveWorkbook(wb, destination, overwrite = TRUE)
}

country_output_coverage <- function(stage, required = TRUE) {
  path <- file.path(stage, "_coverage.csv")
  if (!file.exists(path)) {
    if (required) stop("Worker did not write expected-output coverage; nothing was published", call. = FALSE)
    return(data.frame())
  }
  coverage <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("country", "year", "step", "unit", "status") %in% names(coverage))) stop("Invalid worker coverage report")
  key <- do.call(paste, c(coverage[c("country", "year", "step", "unit")], sep = "/"))
  missing <- setdiff(key[coverage$status == "expected"], key[coverage$status %in% c("success", "excluded", "external")])
  failed <- key[coverage$status %in% c("failed", "failure")]
  if (length(c(missing, failed))) stop("Incomplete selected outputs: ", paste(unique(c(missing, failed)), collapse = ", "), call. = FALSE)
  if (!nrow(coverage)) stop("Empty expected-output coverage report", call. = FALSE)
  coverage
}

country_output_recover <- function(journal, complete = FALSE) {
  if (!file.exists(journal)) return(invisible(NULL))
  rows <- readRDS(journal)
  for (i in rev(seq_len(nrow(rows)))) {
    if (!complete) {
      if (file.exists(rows$backup[i])) {
        if (file.exists(rows$target[i])) unlink(rows$target[i])
        if (!file.rename(rows$backup[i], rows$target[i])) stop("Could not restore ", rows$target[i])
      } else if (!rows$existed[i] && file.exists(rows$target[i])) unlink(rows$target[i])
    } else if (file.exists(rows$backup[i])) unlink(rows$backup[i])
  }
  unlink(journal)
  invisible(NULL)
}

country_output_transaction <- function(sources, targets, journal, commit = function() NULL) {
  if (length(sources) != length(targets) || anyDuplicated(targets)) stop("Invalid output transaction")
  dir.create(dirname(journal), recursive = TRUE, showWarnings = FALSE)
  rows <- data.frame(source = sources, target = targets,
                     backup = paste0(targets, ".dina-backup-", Sys.getpid()), existed = file.exists(targets))
  saveRDS(rows, journal)
  completed <- FALSE
  on.exit(if (!completed) country_output_recover(journal), add = TRUE)
  for (i in seq_len(nrow(rows))) {
    dir.create(dirname(rows$target[i]), recursive = TRUE, showWarnings = FALSE)
    if (rows$existed[i] && !file.rename(rows$target[i], rows$backup[i])) stop("Cannot stage publication backup: ", rows$target[i])
    if (!file.rename(rows$source[i], rows$target[i])) stop("Cannot publish staged output: ", rows$target[i])
  }
  commit() # Completion is recorded only after the complete contribution is installed.
  completed <- TRUE
  country_output_recover(journal, complete = TRUE)
  invisible(targets)
}

country_output_lock_recover <- function(root) {
  lock <- file.path(root, "output", "run_logs", ".country-publication-lock")
  if (!dir.exists(lock)) return(invisible(list(recovered = FALSE)))
  path <- file.path(lock, "owner.rds")
  if (!file.exists(path)) stop("Publication lock has no owner record: ", lock, ". Inspect the interrupted run before removing it.")
  owner <- readRDS(path)
  alive <- tryCatch(isTRUE(tools::pskill(as.integer(owner$pid), signal = 0L)), error = function(e) FALSE)
  if (alive) stop("Another publication is active (PID ", owner$pid, "): ", owner$journal)
  journal <- owner$journal
  complete <- FALSE
  record_path <- file.path(dirname(dirname(dirname(journal))), "country-run.json")
  if (file.exists(record_path)) {
    record <- jsonlite::read_json(record_path, simplifyVector = FALSE)
    key <- basename(dirname(journal))
    complete <- identical(record$chunks[[key]]$status, "succeeded")
    if (grepl("--assembly$", key)) {
      task <- sub("--assembly$", "", key)
      chunks <- record$chunks[startsWith(names(record$chunks), paste0(task, "--"))]
      complete <- length(chunks) > 0L && all(vapply(chunks, function(chunk) identical(chunk$status, "succeeded"), logical(1)))
    }
  }
  country_output_recover(journal, complete = complete)
  unlink(lock, recursive = TRUE)
  invisible(list(recovered = TRUE, complete = complete, journal = journal))
}

country_output_prepare_empty_workbooks <- function(stage) {
  path <- file.path(stage, "_empty_workbooks.csv")
  if (!file.exists(path)) return(invisible(NULL))
  intents <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!all(c("path", "sheet") %in% names(intents))) stop("Invalid empty-workbook intent file")
  if (any(!grepl("^output/.*[.]xlsx$", intents$path) | grepl("(^|/)[.][.](/|$)", intents$path))) stop("Invalid empty-workbook output path")
  for (relative in unique(intents$path)) {
    destination <- file.path(stage, relative)
    wb <- if (file.exists(destination)) openxlsx::loadWorkbook(destination) else openxlsx::createWorkbook()
    for (sheet in unique(intents$sheet[intents$path == relative])) {
      if (!nzchar(sheet)) stop("Empty shared worksheet name")
      if (sheet %in% names(wb)) next
      openxlsx::addWorksheet(wb, sheet)
      openxlsx::writeData(wb, sheet, data.frame(country = character(), year = integer(), step = character(), unit = character()))
    }
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    openxlsx::saveWorkbook(wb, destination, overwrite = TRUE)
  }
  invisible(NULL)
}

country_output_empty_statistics <- function(stage, root, task) {
  if (!startsWith(task$id, "06a") || !file.exists(file.path(stage, "_coverage.csv"))) return(invisible(NULL))
  coverage <- country_output_coverage(stage)
  pairs <- unique(coverage[c("step", "unit")])
  for (i in seq_len(nrow(pairs))) {
    rows <- coverage[coverage$step == pairs$step[i] & coverage$unit == pairs$unit[i], , drop = FALSE]
    if (any(rows$status %in% c("success", "external"))) next
    relative <- file.path("output", "ineqstats", paste0("ineqstats_", pairs$step[i], "_", pairs$unit[i], ".xlsx"))
    source <- file.path(root, relative)
    destination <- file.path(stage, relative)
    if (file.exists(destination) || !file.exists(source)) next
    wb <- openxlsx::createWorkbook()
    for (sheet in openxlsx::getSheetNames(source)) {
      if (grepl("^[A-Z]{3}[_]?[0-9]{4}", sheet)) next
      data <- openxlsx::read.xlsx(source, sheet, check.names = FALSE)
      if (is.null(country_output_country(data))) next
      openxlsx::addWorksheet(wb, sheet)
      openxlsx::writeData(wb, sheet, data[FALSE, , drop = FALSE])
    }
    if (length(names(wb))) {
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      openxlsx::saveWorkbook(wb, destination, overwrite = TRUE)
    }
  }
  invisible(NULL)
}

country_output_publish <- function(stage, root, task, countries, universe, journal,
                                   external = character(), baselines = list(), commit = function() NULL) {
  country_output_prepare_empty_workbooks(stage)
  country_output_empty_statistics(stage, root, task)
  files <- list.files(stage, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  files <- files[grepl("^(output|intermediary_data|input_data)/", substring(files, nchar(stage) + 2L))]
  files <- files[!grepl("\\.(smcl|log)$", files)]
  relative <- substring(files, nchar(stage) + 2L)
  if (any(relative %in% external)) stop("Approved external input cannot be replaced: ", paste(intersect(relative, external), collapse = ", "))
  if (!identical(task$country_scope, "shared")) {
    unselected <- setdiff(universe, countries)
    for (country in unselected) {
      bad <- relative[country_output_path_owned(relative, country)]
      if (length(bad)) stop("Worker staged an unselected country's files: ", paste(bad, collapse = ", "))
    }
  }
  if (any(grepl("(^|/)\\.\\.(/|$)", relative))) stop("Invalid staged path")
  lock <- file.path(root, "output", "run_logs", ".country-publication-lock")
  if (dir.exists(lock)) country_output_lock_recover(root)
  dir.create(dirname(lock), recursive = TRUE, showWarnings = FALSE)
  if (!dir.create(lock, showWarnings = FALSE)) stop("Another publication is active: ", lock, ". Finish or recover that run first.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  saveRDS(list(pid = Sys.getpid(), journal = journal, started_at = format(Sys.time(), "%FT%T%z")), file.path(lock, "owner.rds"))
  candidates <- file.path(dirname(journal), "publish", relative)
  targets <- file.path(root, relative)
  scope_path <- file.path(stage, "_income_scope.csv")
  income_scope <- if (startsWith(task$id, "07a") && file.exists(scope_path)) {
    read.csv(scope_path, stringsAsFactors = FALSE)
  } else NULL
  for (i in seq_along(files)) {
    dir.create(dirname(candidates[i]), recursive = TRUE, showWarnings = FALSE)
    baseline <- targets[i]
    if (!file.exists(baseline) && !is.null(baselines[[basename(baseline)]])) baseline <- file.path(root, baselines[[basename(baseline)]])
    scoped <- !identical(task$country_scope, "shared")
    if (scoped && grepl("\\.xlsx$", files[i], ignore.case = TRUE)) {
      country_output_workbook(baseline, files[i], candidates[i], countries, task$id)
    } else if (scoped && grepl("\\.csv$", files[i], ignore.case = TRUE)) {
      new <- read.csv(files[i], stringsAsFactors = FALSE, check.names = FALSE)
      if (!is.null(country_output_country(new))) {
        old <- if (file.exists(baseline)) read.csv(baseline, stringsAsFactors = FALSE, check.names = FALSE) else new[FALSE, , drop = FALSE]
        selected_income <- NULL
        if (!is.null(income_scope) && grepl("/_smicrofile_long_(detailed|grouped|efftax)\\.csv$", relative[i])) {
          selected_income <- income_scope
          if (endsWith(relative[i], "_efftax.csv")) selected_income <- subset(income_scope, step == "nat")
        }
        merged <- country_output_rows(old, new, countries, selected_income)
        if (startsWith(task$id, "07c") && identical(relative[i], "output/ineqstats/_gpinter_all_topcorrected.csv")) {
          country_output_gpinter_validate(stage, merged)
        }
        write.csv(merged, candidates[i], row.names = FALSE, na = "")
      } else if (!file.copy(files[i], candidates[i], overwrite = TRUE)) stop("Could not stage CSV")
    } else {
      # Changed microdata and figures are already isolated worker outputs.
      # Move them within this filesystem instead of making another full copy.
      if (file.exists(candidates[i])) unlink(candidates[i])
      if (!file.rename(files[i], candidates[i])) stop("Could not move staged output for publication: ", relative[i])
    }
  }
  country_output_transaction(candidates, targets, journal, commit)
  invisible(relative)
}

country_output_gpinter_validate <- function(stage, data) {
  groups_path <- file.path(stage, "_groups.csv")
  if (!file.exists(groups_path)) stop("Missing complete GPinter group inventory")
  groups <- read.csv(groups_path, stringsAsFactors = FALSE)
  columns <- c("country", "year", "step", "unit")
  if (!all(columns %in% names(groups)) || !all(c(columns, "p") %in% names(data))) stop("Invalid GPinter group schema")
  key <- function(x) do.call(paste, c(x[columns], sep = "/"))
  expected <- unique(key(groups))
  actual <- key(data)
  missing <- setdiff(expected, actual)
  if (length(missing)) stop("GPinter publication needs existing contributions for: ", paste(missing, collapse = ", "), ". Run 07c for those countries first.")
  extra <- setdiff(actual, expected)
  if (length(extra)) stop("GPinter publication contains obsolete groups; refresh their countries: ", paste(extra, collapse = ", "))
  counts <- table(actual)
  if (any(counts != 127L)) stop("GPinter groups must each contain 127 fractiles: ", paste(names(counts)[counts != 127L], collapse = ", "))
  grid <- c(seq(0, .99, by = .01), seq(.991, .999, by = .001), seq(.9991, .9999, by = .0001), seq(.99991, .99999, by = .00001))
  bad_grid <- vapply(split(data$p, actual), function(p) !is.numeric(p) || any(!is.finite(p)) ||
    length(p) != length(grid) || any(abs(sort(p) - grid) > 1e-10), logical(1))
  if (any(bad_grid)) stop("GPinter group has missing, duplicated, or invalid fractiles: ", paste(names(bad_grid)[bad_grid], collapse = ", "))
  numeric_columns <- intersect(c("p", "thr", "avg", "bckt_sh", "topavg", "topsh"), names(data))
  if (any(!vapply(data[numeric_columns], function(value) is.numeric(value) && all(is.finite(value)), logical(1)))) stop("GPinter statistics contain non-finite values")
  invisible(TRUE)
}

# Country chunks stay private until the entire required table can be published.
# The caller may reuse these same stage folders when resuming this explicit run.
country_output_gpinter_assemble <- function(stages, destination, baseline, countries) {
  if (!length(stages)) stop("No completed GPinter chunks are available")
  columns <- c("country", "year", "step", "unit")
  key <- function(x) do.call(paste, c(x[columns], sep = "/"))
  pieces <- list()
  groups <- NULL
  coverage <- list()
  relative <- "output/ineqstats/_gpinter_all_topcorrected.csv"
  for (stage in stages) {
    coverage[[length(coverage) + 1L]] <- country_output_coverage(stage)
    group_path <- file.path(stage, "_groups.csv")
    path <- file.path(stage, relative)
    if (!file.exists(group_path) || !file.exists(path)) stop("Incomplete saved GPinter chunk: ", stage)
    incoming_groups <- read.csv(group_path, stringsAsFactors = FALSE)
    if (!all(columns %in% names(incoming_groups))) stop("Invalid saved GPinter group inventory")
    if (is.null(groups)) groups <- incoming_groups else if (!identical(sort(unique(key(groups))), sort(unique(key(incoming_groups))))) {
      stop("GPinter inputs changed between saved chunks; restart 07c for the selected countries")
    }
    chunk <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    if (!all(columns %in% names(chunk)) || any(!chunk$country %in% countries)) stop("Unexpected country or schema in saved GPinter chunk")
    pieces[[length(pieces) + 1L]] <- chunk
  }
  new <- do.call(rbind, pieces)
  selected_expected <- unique(key(groups[groups$country %in% countries, , drop = FALSE]))
  if (length(setdiff(selected_expected, key(new)))) stop("GPinter selected chunks are not complete: ", paste(setdiff(selected_expected, key(new)), collapse = ", "))
  old <- if (file.exists(baseline)) read.csv(baseline, stringsAsFactors = FALSE, check.names = FALSE) else new[FALSE, , drop = FALSE]
  merged <- country_output_rows(old, new, countries)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  write.csv(groups, file.path(destination, "_groups.csv"), row.names = FALSE)
  country_output_gpinter_validate(destination, merged)
  output <- file.path(destination, relative)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  # Keep only selected rows in the candidate; the locked publisher merges
  # against the latest canonical contribution and validates again.
  write.csv(new, output, row.names = FALSE, na = "")
  write.csv(do.call(rbind, coverage), file.path(destination, "_coverage.csv"), row.names = FALSE, na = "")
  reports <- file.path("output/data_reports", c("gpinter_precision_adjustments.csv",
    "gpinter_threshold_reconstruction.csv", "gpinter_fitting_methods.csv",
    "gpinter_tail_adjustments.csv", "gpinter_atoms.csv"))
  for (report in reports) {
    paths <- file.path(stages, report)
    paths <- paths[file.exists(paths)]
    if (!length(paths)) next
    parts <- lapply(paths, read.csv, stringsAsFactors = FALSE, check.names = FALSE)
    data <- do.call(rbind, parts)
    ids <- country_output_country(data)
    if (is.null(ids) || any(!ids %in% countries)) stop("Unexpected country or schema in saved GPinter report: ", report)
    target <- file.path(destination, report)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    # Even an empty report must retain its columns: publication removes the
    # selected countries' previous repairs while preserving other countries.
    write.csv(data, target, row.names = FALSE, na = "")
  }
  invisible(list(stage = destination, path = output, countries = countries))
}
