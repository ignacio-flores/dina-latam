# Explicit country execution and recovery of a single invocation.
# No historical inference, input copying, or cross-run calculation cache.
if (!exists("%||%", mode = "function")) `%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path("code", "R", "country_outputs.R"), local = environment())
country_run_inventory_cache <- new.env(parent = emptyenv())

country_run_load_cli <- function(root) {
  target <- environment(country_run_load_cli)
  if (!exists("dina_session_config", envir = target, inherits = TRUE)) {
    for (file in c("dina_lib.R", "workspace.R", "source_review.R")) {
      source(file.path(root, "code", "R", "cli", file), local = target)
    }
  }
  if (!exists("dina_finished_publication_state", envir = target, inherits = TRUE)) {
    for (file in c("finished_dina_review.R", "finished_dina_include.R", "finished_dina_publication.R")) {
      source(file.path(root, "code", "R", "cli", file), local = target)
    }
  }
}

country_run_session <- function(root) {
  # A child of the ordinary runner must retain its benchmark/update choice.
  # A fresh standalone invocation uses the active update unless its generated
  # runtime configuration explicitly selected another validated scope.
  override <- Sys.getenv("DINA_CONFIG_OVERRIDE_YML", unset = NA_character_)
  if (!is.na(override)) {
    if (!nzchar(override)) return(NULL)
    id <- basename(dirname(override))
    session <- dina_load_session(update_id = id, root = root)
    expected <- dina_session_config_override_path(id, root)
    if (is.null(session) || normalizePath(override, mustWork = FALSE) != normalizePath(expected, mustWork = FALSE)) {
      stop("The standalone configuration override must belong to a DINA update: ", override)
    }
    return(session)
  }
  runtime <- Sys.getenv("DINA_CONFIG_DO", unset = "")
  if (nzchar(runtime) && file.exists(runtime)) {
    line <- readLines(runtime, warn = FALSE)
    line <- line[grepl('^global dina_config_scope "', line)]
    if (length(line) == 1L) {
      scope <- sub('^global dina_config_scope "([^"]+)".*$', '\\1', line)
      if (identical(scope, "benchmark")) return(NULL)
      if (startsWith(scope, "update:")) {
        session <- dina_load_session(update_id = sub("^update:", "", scope), root = root)
        if (is.null(session)) stop("Runtime configuration refers to a missing update: ", scope)
        return(session)
      }
    }
  }
  dina_load_session(root = root)
}

country_run_codes <- function(countries, config) {
  universe <- as.character(unlist(config$countries))
  if (is.null(countries)) return(universe)
  countries <- unique(toupper(unlist(strsplit(paste(countries, collapse = ","), "[,[:space:]]+"))))
  countries <- countries[nzchar(countries)]
  if (!length(countries) || any(!grepl("^[A-Z]{3}$", countries)) || any(!countries %in% universe)) {
    stop("Countries must be ISO3 codes in the effective configuration: ", paste(universe, collapse = ","), call. = FALSE)
  }
  countries
}

country_run_scope <- function(task, config, countries) {
  if (!grepl("^0[3-7]", task$id)) stop("Country execution supports tasks 03a through 07d only")
  requested <- country_run_codes(countries, config)
  if (identical(task$country_scope, "shared")) return(as.character(unlist(config$countries)))
  intersect(requested, as.character(unlist(task$country_applicability %||% config$countries)))
}

country_run_income_selection <- function(selection, config, saved = NULL) {
  if (is.null(selection) && is.null(saved)) return(NULL)
  resolved <- lapply(c("steps", "units"), function(name) {
    universe <- as.character(unlist(config$run[[name]], use.names = FALSE))
    value <- selection[[name]] %||% saved[[name]] %||% universe
    if (isTRUE(value) || !length(value)) stop("Missing value for --", name, ".", call. = FALSE)
    value <- unique(unlist(strsplit(paste(unlist(value), collapse = ","), "[,[:space:]]+"), use.names = FALSE))
    value <- value[nzchar(value)]
    if (!length(value)) stop("Missing value for --", name, ".", call. = FALSE)
    unknown <- setdiff(value, universe)
    if (length(unknown)) stop("--", name, " must use the effective configuration: ",
      paste(universe, collapse = ", "), ". Unknown: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    universe[universe %in% value]
  })
  stats::setNames(resolved, c("steps", "units"))
}

country_run_income_tasks <- function(tasks, selection) {
  if (is.null(selection)) return(invisible(NULL))
  ids <- vapply(tasks, `[[`, "", "id")
  unsupported <- ids[!grepl("^(06[ab]|07[a-d])(-|$)", ids)]
  if (length(unsupported)) stop("--steps and --units support tasks 06a through 07d only. Unsupported selection: ",
    paste(unsupported, collapse = ", "), ".", call. = FALSE)
  invisible(NULL)
}

country_run_income_env <- function(task, selection) {
  if (is.null(selection) || !grepl("^(06b|07a)(-|$)", task$id)) return(character())
  c(DINA_RUN_STEPS = paste(selection$steps, collapse = " "),
    DINA_RUN_UNITS = paste(selection$units, collapse = " "))
}

country_run_read <- function(root, id) {
  if (length(id) != 1L || !grepl("^[A-Za-z0-9_-]+$", id)) stop("Invalid run ID")
  path <- file.path(root, "output", "run_logs", id, "country-run.json")
  if (!file.exists(path)) stop("No resumable country invocation: ", id)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

country_run_save <- function(context) {
  if (isTRUE(context$dry_run)) return(invisible(NULL))
  dir.create(context$directory, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(context$directory, "country-run.json")
  temp <- paste0(file, ".tmp")
  jsonlite::write_json(context$record, temp, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA)
  if (!file.rename(temp, file)) stop("Could not record country run progress")
}

country_run_begin <- function(root, tasks, config, countries, resume = NULL, dry_run = FALSE, selection = NULL) {
  country_run_load_cli(root)
  ids <- vapply(tasks, `[[`, "", "id")
  context <- new.env(parent = emptyenv())
  context$root <- normalizePath(root, mustWork = TRUE)
  context$dry_run <- dry_run
  config <- jsonlite::fromJSON(jsonlite::toJSON(config, auto_unbox = TRUE, null = "null"), simplifyVector = FALSE)
  if (!is.null(resume)) {
    record <- country_run_read(root, resume)
    scope <- country_run_codes(countries %||% unlist(record$countries), config)
    if (!identical(ids, as.character(unlist(record$tasks))) ||
        !setequal(scope, unlist(record$countries)) || !identical(config, record$config)) {
      stop("Resume requires the saved task selection, countries and configuration. Start a new invocation for changed settings.", call. = FALSE)
    }
    requested <- country_run_income_selection(selection, config, saved = record$selection)
    expected <- country_run_income_selection(record$selection %||% list(), config)
    if (!is.null(selection) && !identical(requested, expected)) {
      stop("Resume requires the saved income steps and statistical units. Omit --steps/--units to restore them, or start a new invocation.", call. = FALSE)
    }
    selection <- requested
  } else {
    scope <- country_run_codes(countries, config)
    selection <- country_run_income_selection(selection, config)
    record <- list(version = 1L, id = paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), "-countries-", Sys.getpid(), "-", basename(tempfile())),
                   tasks = as.list(ids), countries = as.list(scope), config = config,
                   started_at = format(Sys.time(), "%FT%T%z"), status = "running", chunks = list(), baselines = list())
  }
  country_run_income_tasks(tasks, selection)
  if (!is.null(selection)) record$selection <- lapply(selection, as.list)
  context$id <- record$id
  context$countries <- as.character(unlist(record$countries))
  context$tasks <- ids
  context$selection <- selection
  context$directory <- file.path(root, "output", "run_logs", context$id)
  context$record <- record
  if (!dry_run) {
    country_output_lock_recover(root)
    # Recover a process interrupted during a file rename, before any new work.
    for (key in names(record$chunks)) {
      journal <- file.path(context$directory, "work", key, "publication.rds")
      country_output_recover(journal, identical(record$chunks[[key]]$status, "succeeded"))
    }
    record$status <- "running"
    context$record <- record
    country_run_save(context)
  }
  context
}

country_run_finish <- function(context, status) {
  if (is.null(context) || !is.environment(context)) return(invisible(NULL))
  context$record$status <- status
  context$record$ended_at <- format(Sys.time(), "%FT%T%z")
  country_run_save(context)
}

country_run_chunks <- function(task, config, countries, selection = NULL) {
  scope <- country_run_scope(task, config, countries)
  if (!length(scope)) return(list())
  if (identical(task$country_scope, "shared")) return(list(list(country = scope, step = "all", unit = "all", key = paste0(task$id, "--shared"))))
  if (startsWith(task$id, "06a")) {
    steps <- as.character(unlist(config$run$steps))
    units <- as.character(unlist(config$run$units))
    if (isTRUE(config$run$debug)) {
      steps <- c("raw", "natinc", "por", "psp", "bfm_norep_pre", "rescaled", "pon", "pod")
      units <- c("esn", "pch")
    }
    if (!is.null(selection)) {
      steps <- selection$steps
      units <- selection$units
    }
    grid <- expand.grid(country = scope, step = steps, unit = units, stringsAsFactors = FALSE)
  } else grid <- data.frame(country = scope, step = "all", unit = "all")
  lapply(seq_len(nrow(grid)), function(i) {
    row <- as.list(grid[i, ])
    row$key <- paste(task$id, row$country, row$step, row$unit, sep = "--")
    if (startsWith(task$id, "06b")) row$shared <- i == 1L
    row
  })
}

country_run_cases <- function(root, config, country, stage = "raw") {
  path <- file.path(root, "intermediary_data", "microdata", stage)
  files <- list.files(path, pattern = paste0("^", country, "_[0-9]{4}_.*\\.dta$"), recursive = TRUE)
  year <- suppressWarnings(as.integer(sub(paste0("^", country, "_([0-9]{4}).*"), "\\1", basename(files))))
  sort(unique(year[!is.na(year) & year >= config$years$first & year <= config$years$last]))
}

country_run_preview <- function(task, root, session, countries, force = FALSE, run_context = NULL, planned_tasks = NULL) {
  config <- dina_session_config(session, root, expand_env = FALSE)
  scope <- country_run_scope(task, config, countries)
  selection <- run_context$selection %||% NULL
  chunks <- country_run_chunks(task, config, countries, selection)
  completed <- if (is.null(run_context)) character() else names(Filter(function(x) identical(x$status, "succeeded") || identical(x$status, "cached"), run_context$record$chunks))
  reuse <- vapply(Filter(function(x) x$key %in% completed, chunks), `[[`, "", "key")
  chunks <- Filter(function(x) !x$key %in% completed, chunks)
  compute <- vapply(chunks, function(x) paste(paste(x$country, collapse = ","), if (x$step != "all") paste(x$step, x$unit, sep = "/") else ""), "")
  issues <- messages <- character()
  if (!is.null(selection)) {
    if (grepl("^(06[ab]|07a)(-|$)", task$id)) messages <- c(messages,
      paste0("Selected income steps: ", paste(selection$steps, collapse = ", "), "; units: ",
        paste(selection$units, collapse = ", "), ". Existing other concepts and units are retained."))
    if (grepl("^07[cd](-|$)", task$id)) messages <- c(messages,
      "Use complete combined results, including retained income concepts and units; --steps/--units do not narrow this assembly.")
  }
  if (startsWith(task$id, "07d")) {
    tryCatch({
      publication <- dina_finished_publication_readiness(root, config,
        internal_pending = any(startsWith(planned_tasks %||% character(), "07c")))
      messages <- c(messages, publication$message)
    }, error = function(e) issues <<- c(issues, conditionMessage(e)))
  }
  shared <- if (identical(task$country_scope, "shared")) "Rebuild complete shared products from existing combined inputs." else "Merge selected contributions; retain other countries' published rows, sheets and files."
  if (!length(scope)) return(list(task = task$id, status = "not_applicable", compute = character(), reuse = reuse, shared = character(), issues = character(), messages = "No applicable selected countries."))
  all_tasks <- dina_task_map(root)
  earlier_outputs <- unlist(lapply(all_tasks[intersect(planned_tasks, names(all_tasks))], function(x) x$outputs %||% character()))
  is_planned <- function(relative) any(vapply(earlier_outputs, function(output) {
    fixed <- sub("[?*\\[].*$", "", output)
    startsWith(relative, sub("/$", "", fixed)) || startsWith(fixed, paste0(relative, "/"))
  }, logical(1)))
  missing <- dina_task_missing_inputs(task, root, session, countries = scope)
  for (path in missing) {
    relative <- dina_relative(path, root)
    if (is_planned(relative)) messages <- c(messages, paste("Produced earlier in this range:", relative))
    else issues <- c(issues, paste(task$id, "needs", relative, "(run its preparation task first)."))
  }
  if (!identical(task$country_scope, "shared")) for (country in scope) {
    raw <- country_run_cases(root, config, country)
    corrected <- country_run_cases(root, config, country, "bfm_norep_pre")
    if (startsWith(task$id, "04b")) corrected <- union(corrected, country_run_cases(root, config, country, "bfm_norep_pos"))
    if (startsWith(task$id, "03a") || startsWith(task$id, "03b")) {
      messages <- c(messages, sprintf("%s: %d survey years; compute eligible survey/Admin overlaps using current trust rules.", country, length(raw)))
      if (!length(raw)) issues <- c(issues, paste(country, "has no configured raw survey cases; run 01e/01f first."))
    } else if (grepl("^(04[bc]|05[bcd]|06a)", task$id) && !length(corrected)) {
      if (any(grepl("^03[abe]", planned_tasks))) messages <- c(messages, paste(country, "corrected microdata will be produced earlier in the range."))
      else issues <- c(issues, paste(country, "has no corrected surveys. Required: dina run --from 03a --to 04c --countries", country))
    }
  }
  if (startsWith(task$id, "05a")) messages <- c(messages, "Compare prepared donor-dependent tables before and after this shared update; report changed nonselected recipients before 05b/05d.")
  case_scope <- if (identical(task$country_scope, "shared")) country_run_codes(countries, config) else scope
  cases <- country_run_case_prerequisites(task, root, config, case_scope, planned_tasks)
  issues <- c(issues, cases$issues)
  messages <- c(messages, cases$messages)
  list(task = task$id, status = if (length(issues)) "blocked" else if (!length(chunks)) "resume" else "planned", compute = compute,
       reuse = reuse, shared = shared, issues = unique(issues), messages = unique(messages))
}

country_run_stage <- function(root, directory, task) {
  stage <- file.path(directory, "stage")
  if (dir.exists(stage)) unlink(stage, recursive = TRUE)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  paths <- c("output", "input_data/admin_data", "intermediary_data/microdata", "intermediary_data/weight_adjusters")
  # Empty directories only; canonical input data are never copied.
  for (base in c("output/bfm_summary", "output/figures", "output/ineqstats", "output/synthetic_microfiles",
                 "output/snacompare_summary", "output/efftax", "output/data_reports", "output/latest_wid_series",
                 "intermediary_data/microdata", "intermediary_data/tax_rates")) {
    paths <- c(paths, base, substring(list.dirs(file.path(root, base), recursive = TRUE), nchar(root) + 2L))
  }
  universe <- dina_session_config(dina_load_session(root = root), root, expand_env = FALSE)$countries
  paths <- c(paths, file.path("input_data/admin_data", universe, "eff-tax-rate"),
             file.path("intermediary_data/microdata", c("bfm_norep_pre", "bfm_norep_pos")),
             file.path("output/bfm_summary", c("bfm_norep_pre", "bfm_norep_pos")))
  for (path in unique(paths)) dir.create(file.path(stage, path), recursive = TRUE, showWarnings = FALSE)
  stage
}

country_run_external <- function(root) {
  registry <- yaml::read_yaml(file.path(root, "config", "sources.yml"))$external_artifacts %||% list()
  unique(vapply(registry, function(x) x$path %||% "", ""))
}

country_run_baselines <- function(context, stage) {
  # Resolve a historical generation once using successful 07a logs, not mtime.
  key <- "07a_previous_generation"
  root <- context$root
  files <- list.files(file.path(root, "output", "run_logs"), "run-manifest.json$", recursive = TRUE, full.names = TRUE)
  runs <- lapply(files, function(path) tryCatch(jsonlite::read_json(path), error = function(e) NULL))
  runs <- Filter(function(x) !is.null(x) && identical(x$task, "07a-export-synthetic-microfiles") && identical(x$status, "succeeded") && is.null(x$country_scope), runs)
  dates <- unique(vapply(runs, function(x) substr(x$ended_at %||% x$run_id, 1L, 10L), ""))
  records <- list.files(file.path(root, "output/run_logs"), "country-run.json$", recursive = TRUE, full.names = TRUE)
  published <- character()
  for (file in records) {
    prior <- jsonlite::read_json(file)
    for (chunk in prior$chunks) if (identical(chunk$status, "succeeded")) {
      outputs <- as.character(unlist(chunk$outputs))
      published <- c(published, outputs[grepl("/(smicrofile_|efftax_).*\\.xlsx$", outputs)])
    }
  }
  result <- context$record$baselines[[key]] %||% list()
  incoming <- c(list.files(file.path(stage, "output", "synthetic_microfiles"), "\\.xlsx$", full.names = TRUE),
                list.files(file.path(stage, "output", "efftax"), "\\.xlsx$", full.names = TRUE))
  for (path in incoming) {
    name <- basename(path)
    if (!is.null(result[[name]])) next
    stem <- sub("_[0-9]{1,2}[A-Za-z]{3}[0-9]{4}\\.xlsx$", "", name)
    folder <- dirname(substring(path, nchar(stage) + 2L))
    candidates <- list.files(file.path(root, folder), paste0("^", stem, "_[0-9]{1,2}[A-Za-z]{3}[0-9]{4}\\.xlsx$"), full.names = TRUE)
    parsed <- as.Date(sub(".*_([0-9]{1,2}[A-Za-z]{3}[0-9]{4})\\.xlsx$", "\\1", candidates), format = "%d%b%Y")
    permitted <- which(as.character(parsed) %in% dates | substring(candidates, nchar(root) + 2L) %in% published)
    if (length(permitted)) result[[name]] <- dina_relative(candidates[permitted[which.max(parsed[permitted])]], root)
    else if (length(candidates)) stop("No successful 07a generation log establishes the previous workbook for ", name, ". Existing files were left intact.")
  }
  context$record$baselines[[key]] <- result
  country_run_save(context)
  result
}

country_run_worker <- function(task, root, session, chunk, stage, progress, extra = character()) {
  env <- c(DINA_COUNTRY_WORKER = task$id, DINA_RUN_COUNTRIES = paste(chunk$country, collapse = " "),
           DINA_OUTPUT_ROOT = stage, DINA_RSCRIPT = file.path(R.home("bin"), "Rscript"))
  if (chunk$step != "all") env <- c(env, DINA_RUN_STEPS = chunk$step, DINA_RUN_UNITS = chunk$unit)
  if (!is.null(chunk$shared)) env <- c(env, DINA_RUN_SHARED = if (chunk$shared) "1" else "0")
  env[names(extra)] <- extra
  dina_run_task(task, root, session, dry_run = FALSE, force = TRUE, progress = progress,
                env_extra = env, skip_current = TRUE, record_session = FALSE)
}

country_run_execute <- function(task, root, session, countries, force = FALSE, progress = NULL, run_context = NULL) {
  config <- dina_session_config(session, root, expand_env = FALSE)
  context <- run_context %||% country_run_begin(root, list(task), config, countries)
  finished <- FALSE
  if (is.null(run_context)) {
    dina_progress(progress, "Country run %s; recovery command: dina run --resume %s.", context$id, context$id)
    on.exit(country_run_finish(context, if (finished) "succeeded" else "failed"), add = TRUE)
  }
  chunks <- country_run_chunks(task, config, countries, context$selection %||% NULL)
  if (!length(chunks)) {
    finished <- TRUE
    return(list(task = task$id, status = "not_applicable", reason = "No applicable selected countries."))
  }
  result <- list(task = task$id, status = "skipped", log_dir = dina_relative(context$directory, root))
  pending <- Filter(function(chunk) {
    previous <- context$record$chunks[[chunk$key]]
    is.null(previous) || !previous$status %in% c("succeeded", "cached")
  }, chunks)
  if (length(pending)) {
    boundary <- country_run_case_prerequisites(task, root, config, country_run_codes(countries, config), character())
    for (message in boundary$messages) dina_progress(progress, "%s", message)
    if (length(boundary$issues)) stop(paste(boundary$issues, collapse = "\n"), call. = FALSE)
  }
  if (startsWith(task$id, "05a")) country_run_donor_before(context, root, session, progress)
  if (grepl("^05[bd]", task$id)) country_run_donor_check(context, task, root, session, progress)
  for (chunk in chunks) {
    previous <- context$record$chunks[[chunk$key]]
    if (!is.null(previous) && previous$status %in% c("succeeded", "cached")) {
      dina_progress(progress, "Resume: keeping completed %s.", chunk$key)
      next
    }
    directory <- file.path(context$directory, "work", chunk$key)
    stage <- country_run_stage(root, directory, task)
    context$record$chunks[[chunk$key]] <- list(status = "running", countries = as.list(chunk$country), step = chunk$step, unit = chunk$unit)
    country_run_save(context)
    dina_progress(progress, "Starting %s: %s%s.", task$id, paste(chunk$country, collapse = ","), if (chunk$step != "all") paste0(" / ", chunk$step, " / ", chunk$unit) else "")
    result <- tryCatch({
      extra <- country_run_income_env(task, context$selection %||% NULL)
      worker <- country_run_worker(task, root, session, chunk, stage, progress, extra = extra)
      if (!identical(worker$status, "succeeded")) {
        context$record$chunks[[chunk$key]]$log_dir <- worker$log_dir
        stop("Script failed; logs: ", worker$log_dir, "\n", paste(unlist(worker$failure %||% "Inspect the task log."), collapse = " "), call. = FALSE)
      }
      coverage <- country_output_coverage(stage, required = !identical(task$country_scope, "shared"))
      country_output_prepare_empty_workbooks(stage)
      staged_outputs <- list.files(stage, recursive = TRUE)
      context$record$chunks[[chunk$key]]$outputs <- as.list(staged_outputs[grepl("^(output|intermediary_data|input_data)/", staged_outputs)])
      if (any(coverage$status == "success") && !length(context$record$chunks[[chunk$key]]$outputs)) {
        stop("Worker reported success but staged no scientific outputs", call. = FALSE)
      }
      commit <- function() {
        context$record$chunks[[chunk$key]]$status <- "succeeded"
        context$record$chunks[[chunk$key]]$log_dir <- worker$log_dir
        context$record$chunks[[chunk$key]]$ended_at <- format(Sys.time(), "%FT%T%z")
        country_run_save(context)
      }
      if (startsWith(task$id, "07c")) {
        context$record$chunks[[chunk$key]]$status <- "cached"
        context$record$chunks[[chunk$key]]$log_dir <- worker$log_dir
        country_run_save(context)
      } else {
        baselines <- if (startsWith(task$id, "07a")) country_run_baselines(context, stage) else list()
        published <- country_output_publish(stage, root, task, chunk$country, config$countries,
          file.path(directory, "publication.rds"), external = country_run_external(root), baselines = baselines, commit = commit)
        context$record$chunks[[chunk$key]]$outputs <- as.list(published)
        country_run_save(context)
        unlink(stage, recursive = TRUE)
      }
      worker
    }, error = function(e) {
      context$record$chunks[[chunk$key]]$status <- "failed"
      context$record$chunks[[chunk$key]]$error <- conditionMessage(e)
      country_run_save(context)
      stop(task$id, " / ", paste(chunk$country, collapse = ","), ": ", conditionMessage(e), "\nResume: dina run --resume ", context$id, call. = FALSE)
    })
    if (!identical(result$status, "succeeded")) {
      context$record$chunks[[chunk$key]]$status <- "failed"
      context$record$chunks[[chunk$key]]$log_dir <- result$log_dir
      country_run_save(context)
      return(result)
    }
  }
  if (startsWith(task$id, "07c")) country_run_gpinter_assemble(context, task, chunks, config)
  result$status <- "succeeded"
  result$run_id <- context$id
  finished <- TRUE
  result
}

country_run_gpinter_assemble <- function(context, task, chunks, config) {
  cached <- Filter(function(x) identical(context$record$chunks[[x$key]]$status, "cached"), chunks)
  if (!length(cached)) return(invisible(NULL))
  assembly <- file.path(context$directory, "work", paste0(task$id, "--assembly"))
  stage <- country_run_stage(context$root, assembly, task)
  stages <- vapply(cached, function(chunk) file.path(context$directory, "work", chunk$key, "stage"), "")
  country_output_gpinter_assemble(stages, stage,
    file.path(context$root, "output/ineqstats/_gpinter_all_topcorrected.csv"),
    unique(unlist(lapply(cached, `[[`, "country"))))
  country_output_publish(stage, context$root, task, unique(unlist(lapply(cached, `[[`, "country"))), config$countries,
    file.path(assembly, "publication.rds"), external = country_run_external(context$root), commit = function() {
      for (chunk in cached) context$record$chunks[[chunk$key]]$status <- "succeeded"
      country_run_save(context)
    })
}

country_run_donor_snapshot <- function(context, task, root, session, when, progress) {
  directory <- file.path(context$directory, "donors", task$id, when)
  stage <- country_run_stage(root, directory, task)
  chunk <- list(country = as.character(unlist(context$record$config$countries)), step = "all", unit = "all")
  dina_progress(progress, "Preparing small %s donor tables (%s); no microdata updates.", task$id, when)
  result <- country_run_worker(task, root, session, chunk, stage, progress, c(DINA_DONOR_SNAPSHOT = "1"))
  if (!identical(result$status, "succeeded")) stop("Donor preparation failed: ", result$log_dir)
  files <- list.files(stage, "^_donors.*\\.csv$", full.names = TRUE)
  if (!length(files)) stop("Donor preparation did not produce its comparison tables")
  files
}

country_run_donor_before <- function(context, root, session, progress) {
  if (setequal(context$countries, unlist(context$record$config$countries))) return(invisible(NULL))
  tasks <- Filter(function(task) grepl("^05[bd]", task$id), dina_task_map(root))
  for (task in tasks) {
    if (!is.null(context$record$donors[[task$id]])) next
    value <- tryCatch({
      paths <- country_run_donor_snapshot(context, task, root, session, "before", progress)
      list(status = "available", before = as.list(paths))
    }, error = function(e) list(status = "unavailable", reason = conditionMessage(e)))
    context$record$donors[[task$id]] <- value
    country_run_save(context)
    if (value$status == "unavailable") dina_progress(progress, "Previous %s donor comparison unavailable: %s.", task$id, value$reason)
  }
}

country_run_donor_compare <- function(before, after, selected) {
  if (!setequal(basename(before), basename(after))) stop("The prepared donor table set changed; inspect missing or new tables before continuing.")
  changed <- character()
  for (file in after) {
    old <- before[basename(before) == basename(file)]
    if (length(old) != 1L) stop("Missing previous donor table: ", basename(file))
    a <- read.csv(old, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "."))
    b <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "."))
    if (!all(c("country", "year", "variable", "value") %in% names(a)) || !identical(names(a), names(b))) stop("Donor comparison schema changed")
    key <- function(x) paste(x$country, x$year, x$variable, sep = "/")
    joined <- merge(a, b, by = c("country", "year", "variable"), all = TRUE, suffixes = c(".before", ".after"))
    same <- (is.na(joined$value.before) & is.na(joined$value.after)) | (!is.na(joined$value.before) & !is.na(joined$value.after) & joined$value.before == joined$value.after)
    added <- setdiff(key(b), key(a)); removed <- setdiff(key(a), key(b))
    changed <- union(changed, joined$country[!same | key(joined) %in% c(added, removed)])
  }
  setdiff(changed, selected)
}

country_run_donor_check <- function(context, task, root, session, progress) {
  before <- context$record$donors[[task$id]]
  if (is.null(before)) {
    if (setequal(context$countries, unlist(context$record$config$countries))) {
      dina_progress(progress, "%s uses the complete prepared donor inputs; all configured countries are selected, so there are no countries outside this run to check.", task$id)
    } else {
      dina_progress(progress, "%s uses the complete prepared donor inputs; this invocation has not rebuilt 05a.", task$id)
    }
    return(invisible(NULL))
  }
  if (!identical(before$status, "available")) {
    stop("Cannot establish the effect of this 05a update on other countries: previous ", task$id,
      " donor preparation failed: ", before$reason, ". Fix that preparation and inspect the before/after tables before continuing this restricted run. No additional countries were launched.", call. = FALSE)
  }
  after <- country_run_donor_snapshot(context, task, root, session, "after", progress)
  changed <- country_run_donor_compare(unlist(before$before), after, context$countries)
  context$record$donors[[task$id]]$changed_recipients <- as.list(changed)
  country_run_save(context)
  if (length(changed)) stop("Prepared donor values changed for additional countries: ", paste(changed, collapse = ","),
    ". They were not processed. Required additional scope starts at ", substr(task$id, 1L, 3L),
    ": dina run --from ", substr(task$id, 1L, 3L), " --to 07d --countries ", paste(changed, collapse = ","),
    ". To cover this invocation as well, start a new run from that stage with --countries ",
    paste(union(context$countries, changed), collapse = ","), ". No additional countries were launched.", call. = FALSE)
}

country_run_r_entry <- function(task_id, args = commandArgs(trailingOnly = TRUE)) {
  index <- which(args == "--countries" | startsWith(args, "--countries="))
  selected <- NULL
  if (length(index)) {
    if (length(index) != 1L) stop("Specify --countries once")
    i <- index[[1L]]
    if (args[i] == "--countries") {
      if (i == length(args)) stop("Missing --countries value")
      selected <- args[i + 1L]; args <- args[-c(i, i + 1L)]
    } else { selected <- sub("^--countries=", "", args[i]); args <- args[-i] }
  }
  if (identical(Sys.getenv("DINA_COUNTRY_WORKER"), task_id)) return(args)
  root <- normalizePath(getwd())
  country_run_load_cli(root)
  session <- country_run_session(root)
  config <- dina_session_config(session, root, expand_env = FALSE)
  task <- dina_task_map(root)[[task_id]]
  if (is.null(task)) stop("Unknown country task: ", task_id)
  scope <- country_run_scope(task, config, selected)
  if (!length(scope)) { cat("No applicable selected countries.\n"); quit(status = 0L) }
  if (any(args %in% c("--output", "--output-dir"))) {
    # Existing explicit comparison destinations are independent of publication.
    Sys.setenv(DINA_RUN_COUNTRIES = paste(scope, collapse = " "), DINA_COUNTRY_WORKER = task_id)
    return(args)
  }
  if (length(args)) stop("Unrecognized standalone arguments: ", paste(args, collapse = " "))
  result <- country_run_execute(task, root, session, selected, progress = function(message) { cat(message, "\n"); flush.console() })
  quit(status = if (result$status %in% c("succeeded", "skipped", "not_applicable")) 0L else 1L)
}

# Read only explicit published case inventories. Unlisted raw years are not
# automatically considered expected corrected observations.
country_run_index_cases <- function(root) {
  paths <- file.path(root, c("output/bfm_summary/all_thetas.xlsx",
    "output/bfm_summary/bfm_norep_pre/merging_points.xlsx", "output/bfm_summary/bfm_norep_pos/merging_points.xlsx",
    "intermediary_data/weight_adjusters/index.xlsx"))
  info <- file.info(paths)
  # An in-memory read cache for these four small indexes only. It never decides
  # whether a calculation is reusable and writes no tracking files.
  cache_key <- paste(paths, info$size, sprintf("%.9f", as.numeric(info$mtime)), collapse = "|")
  if (identical(country_run_inventory_cache$key, cache_key)) return(country_run_inventory_cache$value)
  empty <- data.frame(country = character(), year = integer(), kind = character(),
                      failed = logical(), source = character(), stringsAsFactors = FALSE)
  rows <- list()
  read_sheet <- function(relative, sheet) {
    path <- file.path(root, relative)
    if (!file.exists(path)) return(NULL)
    sheets <- openxlsx::getSheetNames(path)
    if (!sheet %in% sheets) return(NULL)
    openxlsx::read.xlsx(path, sheet, check.names = FALSE)
  }
  add <- function(data, kind, source) {
    if (is.null(data) || !nrow(data)) return(invisible(NULL))
    if (all(c("country", "year") %in% names(data))) {
      country <- as.character(data$country)
      year <- suppressWarnings(as.integer(data$year))
    } else if ("country_year" %in% names(data)) {
      key <- as.character(data$country_year)
      valid <- grepl("^[A-Z]{3}_?[0-9]{4}$", key)
      country <- ifelse(valid, substr(key, 1L, 3L), NA_character_)
      year <- suppressWarnings(as.integer(ifelse(valid, sub("^[A-Z]{3}_?", "", key), NA_character_)))
    } else return(invisible(NULL))
    failed <- if ("fail" %in% names(data)) !is.na(data$fail) & as.character(data$fail) == "1" else rep(FALSE, nrow(data))
    keep <- !is.na(country) & grepl("^[A-Z]{3}$", country) & !is.na(year)
    if (any(keep)) rows[[length(rows) + 1L]] <<- unique(data.frame(
      country = country[keep], year = year[keep], kind = kind, failed = failed[keep],
      source = source, stringsAsFactors = FALSE))
  }
  panel <- "output/bfm_summary/all_thetas.xlsx"
  add(read_sheet(panel, "panel"), "observed", paste0(panel, "#panel"))
  for (type in c("pre", "pos")) {
    path <- sprintf("output/bfm_summary/bfm_norep_%s/merging_points.xlsx", type)
    for (sheet in c("country_years", "country_years_2stage")) {
      add(read_sheet(path, sheet), "observed", paste0(path, "#", sheet))
    }
  }
  index <- "intermediary_data/weight_adjusters/index.xlsx"
  for (sheet in c("country_years_03d", "country_years_03e")) {
    add(read_sheet(index, sheet), "extrapolated", paste0(index, "#", sheet))
  }
  result <- if (!length(rows)) empty else unique(do.call(rbind, rows))
  result <- result[order(result$country, result$year, result$kind, result$source), , drop = FALSE]
  country_run_inventory_cache$key <- cache_key
  country_run_inventory_cache$value <- result
  result
}

country_run_observed_donor_prerequisites <- function(root, config, scope, planned_tasks = character()) {
  panel <- file.path(root, "output/bfm_summary/all_thetas.xlsx")
  if (!file.exists(panel)) return(list(issues = character(), messages = character()))
  data <- openxlsx::read.xlsx(panel, sheet = "panel")
  if (!all(c("country", "year") %in% names(data))) {
    return(list(issues = "03d needs a valid observed panel; run 03c first.", messages = character()))
  }
  data$year <- suppressWarnings(as.integer(data$year))
  data <- unique(data[data$country %in% scope & !is.na(data$year) &
    data$year >= config$years$first & data$year <= config$years$last, c("country", "year")])
  # These are the existing 03d donor exclusions, not publication target rules.
  data <- data[!(data$country == "CHL" & data$year %in% c(2000L, 2003L)) &
                 !(data$country == "ECU" & data$year == 2011L), , drop = FALSE]
  metadata <- if (file.exists(file.path(root, "config/pipeline.yml")))
    yaml::read_yaml(file.path(root, "config/pipeline.yml"))$tasks else list()
  producers <- Filter(function(x) grepl("^03[ab]-", x$id), metadata)
  issues <- messages <- character()
  for (country in unique(data$country)) {
    years <- data$year[data$country == country]
    paths <- file.path(root, "output/bfm_summary/observed_donors", paste0(country, years, ".dta"))
    missing <- years[!file.exists(paths)]
    if (!length(missing)) {
      messages <- c(messages, sprintf("%s: %d stable observed-donor tables; 03d does not read overwritten corrected microdata.", country, length(years)))
      next
    }
    producer <- Filter(function(x) country %in% unlist(x$country_applicability), producers)
    id <- if (length(producer)) producer[[1L]]$id else "03a/03b"
    cases <- paste0(country, "_", sort(missing), collapse = ", ")
    if (id %in% planned_tasks) {
      messages <- c(messages, sprintf("Stable observed donors for %s are produced by %s earlier in this range; checked again before 03d.", cases, id))
    } else {
      issues <- c(issues, sprintf("03d is missing independent observed donors for %s. Required: dina run %s --countries %s. Corrected microdata cannot substitute for these tables.", cases, id, country))
    }
  }
  list(issues = issues, messages = messages)
}

country_run_case_prerequisites <- function(task, root, config, scope, planned_tasks = character()) {
  issues <- messages <- character()
  scope <- intersect(as.character(unlist(scope)), as.character(unlist(config$countries)))
  planned_tasks <- as.character(unlist(planned_tasks))
  if (startsWith(task$id, "03d")) {
    return(country_run_observed_donor_prerequisites(root, config, scope, planned_tasks))
  }
  # Preview current overlap candidates without changing trust/eligibility rules.
  if (grepl("^03[ab]", task$id)) {
    prefix <- if (startsWith(task$id, "03a")) "gpinter" else "wage"
    for (country in scope) {
      paths <- list.files(file.path(root, "input_data/admin_data", country),
                          paste0("^", prefix, "_", country, "_[0-9]{4}\\.xlsx$"))
      years <- suppressWarnings(as.integer(sub(".*_([0-9]{4})\\.xlsx$", "\\1", paths)))
      overlap <- intersect(years, country_run_cases(root, config, country))
      messages <- c(messages, paste0(country, " candidate ", prefix, "/survey overlaps: ",
        if (length(overlap)) paste(sort(overlap), collapse = ", ") else "none", "; trust and scientific eligibility are checked by the worker."))
    }
    return(list(issues = issues, messages = messages))
  }
  consumes_corrected <- grepl("^(05[abcd]|06a|07a)", task$id)
  if (startsWith(task$id, "06a") && !isTRUE(config$run$debug)) {
    steps <- as.character(unlist(config$run$steps))
    if (length(steps) && all(steps %in% c("raw", "urb"))) consumes_corrected <- FALSE
  }
  if (!consumes_corrected || !length(scope)) return(list(issues = issues, messages = messages))
  cases <- country_run_index_cases(root)
  keys <- paste(cases$country, cases$year, sep = "_")
  accepted <- keys[endsWith(cases$source, "all_thetas.xlsx#panel")]
  targets <- keys[endsWith(cases$source, "index.xlsx#country_years_03d")]
  cases$failed[keys %in% setdiff(accepted, targets)] <- FALSE
  if (startsWith(task$id, "05a")) {
    other_failures <- cases[cases$failed & !cases$country %in% scope &
      cases$country %in% as.character(unlist(config$countries)) &
      cases$year >= config$years$first & cases$year <= config$years$last, , drop = FALSE]
    if (nrow(other_failures)) messages <- c(messages,
      paste("Known 03e failures outside the requested countries (not recomputed):",
        paste(unique(paste(other_failures$country, other_failures$year, sep = "_")), collapse = ", ")))
  }
  cases <- cases[cases$country %in% scope & cases$year >= config$years$first & cases$year <= config$years$last, , drop = FALSE]
  if (!nrow(cases)) return(list(issues = issues, messages = messages))
  metadata <- if (file.exists(file.path(root, "config/pipeline.yml"))) yaml::read_yaml(file.path(root, "config/pipeline.yml"))$tasks else list()
  planned_for <- function(prefix, country) {
    selected <- planned_tasks[startsWith(planned_tasks, prefix)]
    if (!length(selected)) return(FALSE)
    any(vapply(selected, function(id) {
      definition <- Filter(function(x) identical(x$id, id), metadata)
      !length(definition) || country %in% as.character(unlist(definition[[1L]]$country_applicability %||% config$countries))
    }, logical(1)))
  }
  for (key in unique(paste(cases$country, cases$year, sep = "_"))) {
    row <- cases[paste(cases$country, cases$year, sep = "_") == key, , drop = FALSE]
    country <- row$country[[1L]]; year <- row$year[[1L]]
    failed <- any(row$failed)
    observed <- any(row$kind == "observed")
    relative <- sprintf("intermediary_data/microdata/bfm_norep_pre/%s_%d_bfm_norep_pre.dta", country, year)
    missing <- !file.exists(file.path(root, relative))
    if (!missing && !failed) next
    if (startsWith(task$id, "07a") && planned_for("06a", country)) {
      messages <- c(messages, paste(key, "statistics are produced by 06a earlier in the selected range; its case prerequisites are checked there."))
      next
    }
    extrapolated <- !observed || failed
    producer_planned <- if (extrapolated) planned_for("03e", country) else
      planned_for("03a", country) || planned_for("03b", country)
    postax <- sprintf("intermediary_data/microdata/bfm_norep_pos/%s_%d_bfm_norep_pos.dta", country, year)
    # 04b can produce a missing pretax counterpart from existing posttax data;
    # an old counterpart does not repair a recorded failed 03e calculation.
    covered <- planned_for("04b", country) &&
      (producer_planned || (!failed && file.exists(file.path(root, postax))))
    if (covered) {
      messages <- c(messages, paste(key, "corrected contribution is produced earlier in the selected range."))
      next
    }
    start <- if (extrapolated) "03e" else if (file.exists(file.path(root, postax))) "04b" else
      if (any(grepl("country_years_2stage$", row$source)) || any(vapply(metadata, function(definition) {
        startsWith(definition$id, "03b") && country %in% as.character(unlist(definition$country_applicability))
      }, logical(1)))) "03b" else "03a"
    reason <- if (failed) "has a failed 03e entry; an older corrected file cannot stand in for its result" else paste("is listed but its corrected file is missing:", relative)
    issues <- c(issues, sprintf("%s %s. Required: dina run --from %s --to 04c --countries %s", key, reason, start, country))
  }
  list(issues = unique(issues), messages = unique(messages))
}
