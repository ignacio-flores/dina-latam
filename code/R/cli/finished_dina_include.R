# Finished distributions are prepared in experiments and accepted separately.
# Only the registered originals and their prepared companions are published.

dina_finished_include_engine <- function(root) {
  engine <- new.env(parent = environment(dina_finished_include_engine))
  for (file in c("finished_dina_explorer.R", "finished_dina_preparer.R", "finished_dina_grouped.R")) {
    source(file.path(root, "code", "R", "source-diagnostics", file), local = engine)
  }
  engine
}

dina_finished_include_path <- function(root, path, allowed) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      ".." %in% strsplit(path, "/", fixed = TRUE)[[1L]] || grepl("[*?\\[]", path)) {
    stop("Finished DINA paths must be exact paths without '..' or wildcards.", call. = FALSE)
  }
  canonical <- function(p) {
    suffix <- character()
    while (!file.exists(p)) {
      suffix <- c(basename(p), suffix)
      p <- dirname(p)
    }
    paste(c(normalizePath(p, mustWork = TRUE), suffix), collapse = "/")
  }
  if (!grepl("^/", path)) path <- file.path(root, path)
  path <- canonical(path)
  allowed <- canonical(file.path(root, allowed))
  if (!startsWith(path, paste0(allowed, "/"))) {
    stop("Finished DINA path must be inside ", allowed, ".", call. = FALSE)
  }
  path
}

dina_finished_include_sources <- function(root) {
  sources <- dina_source_registry(root, family = "dina")
  if (!length(sources)) stop("No finished DINA sources are registered.", call. = FALSE)
  ids <- vapply(sources, function(x) x$id, character(1))
  if (anyDuplicated(ids)) stop("Finished DINA source IDs must be unique.", call. = FALSE)
  sources <- lapply(sources, function(source) {
    source$input <- dina_finished_include_path(root, unlist(source$inbox),
      "input_data/_new/dina")
    source$original <- dina_finished_include_path(root, unlist(source$canonical),
      "input_data/dina")
    grouped <- identical(source$finished_series$format, "wid_grouped_xlsx")
    if (!grepl(if (grouped) "\\.xlsx$" else "\\.dta$", source$original, ignore.case = TRUE)) {
      stop("Finished DINA canonical files must have a ", if (grouped) ".xlsx" else ".dta", " extension.", call. = FALSE)
    }
    source$prepared <- dina_finished_include_path(root,
      source$prepared %||% sub("\\.dta$", "_prepared.dta", source$original,
        ignore.case = TRUE), "input_data/dina")
    if (!grepl("\\.dta$", source$prepared, ignore.case = TRUE)) stop("Prepared DINA artifacts must have a .dta extension.", call. = FALSE)
    if (!file.exists(source$input) || dir.exists(source$input)) {
      stop("Supplied finished DINA file is missing: ", source$input, call. = FALSE)
    }
    source
  })
  destinations <- unlist(lapply(sources, function(source) c(source$original, source$prepared)),
    use.names = FALSE)
  if (anyDuplicated(destinations)) {
    stop("Finished DINA sources must have distinct original and prepared destinations.", call. = FALSE)
  }
  sources
}

dina_finished_include_identity <- function(root, sources, engine = NULL) {
  dina_need("digest")
  # The CLI resolves the repository root, while direct callers may use a
  # symlink spelling (notably /var versus /private/var on macOS).
  root <- normalizePath(root, mustWork = TRUE)
  gpinter_version <- if (!is.null(engine$gpinter_version)) engine$gpinter_version else {
    dina_need("gpinter")
    as.character(utils::packageVersion("gpinter"))
  }
  list(
    sources = lapply(sources, function(source) list(id = source$id,
      input = source$input, sha256 = dina_hash_file(source$input),
      specification = digest::digest(source, algo = "sha256"))),
    code = dina_review_hashes(file.path(root, "code", "R", "source-diagnostics",
      c("finished_dina_explorer.R", "finished_dina_preparer.R", "finished_dina_grouped.R"))),
    gpinter_version = as.character(gpinter_version)
  )
}

dina_finished_include_verify_review <- function(root, record, sources) {
  if (is.null(record) || !identical(record$family, "dina") ||
      record$status %in% c("incomplete", "blocked")) {
    stop("Explore the supplied source successfully first: dina sources explore dina", call. = FALSE)
  }
  # Source and registry evidence must still match the review. Presentation code
  # is deliberately excluded: changes to colours cannot alter scientific data.
  required <- c(file.path(root, "input_data", "_new", "dina"),
    file.path(root, "config", "sources.yml"))
  # Scientific reader/validator changes invalidate reviews that recorded them.
  scientific <- file.path(root, "code", "R", "source-diagnostics", c("finished_dina_explorer.R", "finished_dina_grouped.R"))
  required <- c(required, intersect(scientific, names(record$inputs)))
  stored <- record$inputs
  if (!all(required %in% names(stored)) ||
      !identical(dina_review_hashes(required), stored[sort(required)])) {
    stop("The reviewed DINA source or registry changed. Run dina sources explore dina again.", call. = FALSE)
  }
  invisible(TRUE)
}

dina_finished_include_save <- function(candidate) {
  path <- file.path(candidate$run, "candidate.json")
  pending <- tempfile("candidate-", tmpdir = candidate$run)
  on.exit(unlink(pending), add = TRUE)
  dina_write_json(candidate, pending)
  if (!file.rename(pending, path)) stop("Could not save the DINA candidate.", call. = FALSE)
  invisible(candidate)
}

dina_finished_include_candidate <- function(root, record, run = record$include_candidate) {
  if (is.list(run)) run <- run$run
  if (is.null(run)) return(NULL)
  run <- dina_finished_include_path(root, run, "output/experiments")
  candidate <- dina_read_json(file.path(run, "candidate.json"), default = NULL)
  if (is.null(candidate) || !identical(candidate$family, "dina") ||
      !identical(normalizePath(candidate$run, mustWork = FALSE), run)) {
    stop("Not a saved finished DINA inclusion candidate: ", run, call. = FALSE)
  }
  candidate
}

dina_finished_include_verify <- function(candidate, root, record, sources, engine = NULL) {
  if (!candidate$status %in% c("ready", "accepted")) {
    stop("This DINA candidate has unresolved failures. Inspect its problems table.", call. = FALSE)
  }
  if (!identical(candidate$reviewed_run, record$run)) {
    stop("This DINA candidate belongs to a superseded exploration. Prepare a new dry run.", call. = FALSE)
  }
  identity <- dina_finished_include_identity(root, sources, engine)
  if (!identical(candidate$identity, identity)) {
    stop("The DINA source, preparation recipe or GPinter version changed. Prepare a new dry run.", call. = FALSE)
  }
  expected <- unlist(lapply(sources, function(s) c(s$original, s$prepared)), use.names = FALSE)
  destinations <- vapply(candidate$artifacts, function(a) a$destination, character(1))
  if (anyDuplicated(destinations) || !setequal(expected, destinations)) {
    stop("Candidate artifacts do not match the complete registered source coverage.", call. = FALSE)
  }
  for (artifact in candidate$artifacts) {
    staged <- dina_finished_include_path(root, artifact$staged, "output/experiments")
    dina_finished_include_path(root, artifact$destination, "input_data/dina")
    if (!startsWith(staged, paste0(candidate$run, "/")) ||
        !identical(dina_hash_file(staged), artifact$sha256)) {
      stop("A prepared DINA artifact is missing or changed: ", staged, call. = FALSE)
    }
  }
  if (!identical(dina_review_hashes(names(candidate$evidence)), candidate$evidence)) {
    stop("The saved DINA change report changed. Prepare a new dry run.", call. = FALSE)
  }
  invisible(TRUE)
}

dina_finished_include_verify_candidate <- function(candidate, record, root) {
  dina_finished_include_verify(candidate, root, record,
    dina_finished_include_sources(root))
}

dina_finished_include_prepare <- function(root, record, flags = list(),
                                         engine = dina_finished_include_engine(root)) {
  sources <- dina_finished_include_sources(root)
  dina_finished_include_verify_review(root, record, sources)
  identity <- dina_finished_include_identity(root, sources, engine)
  base <- dina_finished_output_root(root,
    flags[["output-dir"]] %||% "output/experiments/finished_dina_include")
  run <- file.path(base, "runs", paste0("include-", format(Sys.time(), "%Y%m%d-%H%M%S"),
    "-", basename(tempfile())))
  dir.create(file.path(run, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(run, "artifacts"), recursive = TRUE, showWarnings = FALSE)
  candidate <- list(family = "dina", status = "preparing", run = run,
    reviewed_run = record$run, prepared_at = dina_now(), identity = identity,
    scope = "registered source coverage and declared preparation-year selection", sources = list(),
    artifacts = list(), tables = list(), failures = list())
  tables <- setNames(rep(list(data.frame()), 6L),
    c("summary", "changes", "impact", "checks", "problems", "coverage"))
  add <- function(name, rows, id) {
    if (!is.data.frame(rows) || !nrow(rows)) return(invisible(NULL))
    rows$source_id <- id
    tables[[name]] <<- dina_review_bind(list(tables[[name]], rows))
  }
  for (i in seq_along(sources)) {
    source <- sources[[i]]
    grouped_source <- identical(source$finished_series$format, "wid_grouped_xlsx")
    dina_cli_cat("Preparing ", source$id, if (grouped_source) " (full-population observed years only)." else " (all years, concepts and units).")
    result <- tryCatch({
      dina_need("haven")
      grouped <- identical(source$finished_series$format, "wid_grouped_xlsx")
      original <- if (grouped) engine$finished_dina_grouped_read(source$input, source) else as.data.frame(haven::read_dta(source$input))
      prepare_args <- list(data = original, specification = dina_finished_specification(source))
      if ("progress" %in% names(formals(engine$finished_dina_prepare))) {
        prepare_args$progress <- function(index, total, case) {
          dina_cli_cat(sprintf("  [%s/%s] %s", index, total, case))
        }
      }
      prepared <- if (grouped) engine$finished_dina_grouped_prepare(original, source, progress = prepare_args$progress) else do.call(engine$finished_dina_prepare, prepare_args)
      validation <- prepared$validation %||% engine$finished_dina_validate(
        prepared$data, dina_finished_specification(source))
      for (name in c("summary", "changes", "impact")) add(name, prepared[[name]], source$id)
      add("coverage", if (grouped) prepared$coverage else validation$coverage, source$id)
      add("checks", validation$checks, source$id)
      add("problems", validation$issues, source$id)
      if (any(validation$checks$status %in% c("fail", "review"))) {
        stop("Prepared distribution failed its consistency checks.", call. = FALSE)
      }
      # A pure engine result is validated again after serialization to ensure
      # the exact file being accepted has complete coverage and valid values.
      staged_raw <- file.path(run, "artifacts", paste0(i, "-original.", if (grouped) "xlsx" else "dta"))
      staged_prepared <- file.path(run, "artifacts", paste0(i, "-prepared.dta"))
      if (!file.copy(source$input, staged_raw, overwrite = FALSE)) {
        stop("Could not stage the byte-identical original.", call. = FALSE)
      }
      if (!identical(dina_hash_file(staged_raw), identity$sources[[i]]$sha256)) {
        stop("The supplied original changed while it was being staged.", call. = FALSE)
      }
      haven::write_dta(prepared$data, staged_prepared, version = 14)
      serialized <- as.data.frame(haven::read_dta(staged_prepared))
      checked <- if (grouped) engine$finished_dina_grouped_validate_prepared(serialized, source, original) else engine$finished_dina_validate(serialized, dina_finished_specification(source))
      if (grouped && !isTRUE(all.equal(serialized, prepared$data, check.attributes = FALSE, tolerance = 1e-12))) stop("Serialized grouped distribution differs from the prepared observations.")
      if (any(checked$checks$status %in% c("fail", "review"))) {
        stop("Serialized prepared distribution failed validation.", call. = FALSE)
      }
      for (role in c("original", "prepared")) {
        staged <- if (role == "original") staged_raw else staged_prepared
        candidate$artifacts[[length(candidate$artifacts) + 1L]] <- list(
          source_id = source$id, role = role, staged = staged,
          destination = source[[role]], sha256 = dina_hash_file(staged))
      }
      meta <- source$finished_series
      candidate$sources[[length(candidate$sources) + 1L]] <- list(
        id = source$id, version = meta$version, citation = meta$citation, doi = meta$doi,
        publication = if (grouped) "requires publication policy review" else paste(meta$publication_step, meta$publication_unit, sep = "/"),
        representation = "distribution",
        requires_harmonization = FALSE,
        coverage = list(country = source$country, first_year = meta$first_year,
          last_year = meta$last_year, steps = meta$steps, units = meta$units),
        processing = if (grouped) "Full-population years only; proportional share normalization and GPinter; mean-one scale; no year interpolation/extrapolation" else "DINA project preparation and local GPinter interpolation",
        recipe = prepared$recipe)
      NULL
    }, error = function(e) e)
    if (inherits(result, "error")) {
      candidate$failures[[length(candidate$failures) + 1L]] <- list(
        source_id = source$id, message = conditionMessage(result))
      add("checks", data.frame(check_id = "preparation", status = "fail",
        affected_rows = NA_integer_, affected_cases = NA_integer_,
        detail = conditionMessage(result)), source$id)
      add("problems", data.frame(check_id = "preparation", status = "fail",
        country = paste(source$country, collapse = ","), detail = conditionMessage(result)), source$id)
    }
  }
  if (!identical(identity, dina_finished_include_identity(root, sources, engine))) {
    candidate$failures[[length(candidate$failures) + 1L]] <- list(
      source_id = "", message = "Source or recipe changed during preparation.")
  }
  for (name in names(tables)) {
    path <- file.path(run, "tables", paste0(name, ".csv"))
    utils::write.csv(tables[[name]], path, row.names = FALSE, na = "")
    candidate$tables[[name]] <- path
  }
  candidate$evidence <- dina_review_hashes(unlist(candidate$tables, use.names = FALSE))
  candidate$status <- if (length(candidate$failures)) "blocked" else "ready"
  dina_finished_include_save(candidate)
  record$include_candidate <- run
  dina_review_save(record, root)
  candidate
}

dina_finished_include_copy <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  pending <- tempfile(".finished-dina-", tmpdir = dirname(to))
  on.exit(unlink(pending), add = TRUE)
  if (!file.copy(from, pending, overwrite = TRUE) ||
      !identical(dina_hash_file(from), dina_hash_file(pending)) ||
      !file.rename(pending, to)) stop("Could not replace ", to, ".", call. = FALSE)
  invisible(to)
}

dina_finished_include_rollback <- function(entries) {
  errors <- character()
  for (entry in rev(entries)) {
    result <- tryCatch({
      if (isTRUE(entry$existed)) {
        if (!identical(dina_hash_file(entry$backup), entry$before_sha256)) {
          stop("Backup is missing or changed.")
        }
        dina_finished_include_copy(entry$backup, entry$destination)
      } else if (file.exists(entry$destination)) {
        if (unlink(entry$destination) != 0L) stop("Could not remove new artifact.")
      }
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(result)) errors <- c(errors, paste(entry$destination, result))
  }
  if (length(errors)) stop("DINA rollback needs attention: ", paste(errors, collapse = "; "), call. = FALSE)
  invisible(TRUE)
}

dina_finished_include_pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (length(pid) != 1L || is.na(pid) || pid < 1L) return(TRUE)
  tryCatch(isTRUE(suppressWarnings(tools::pskill(pid, signal = 0L))),
    error = function(e) TRUE)
}

dina_finished_include_write_journal <- function(value, path) {
  pending <- tempfile(".journal-", tmpdir = dirname(path))
  on.exit(unlink(pending), add = TRUE)
  dina_write_json(value, pending)
  if (!file.rename(pending, path)) stop("Could not save the DINA inclusion journal.", call. = FALSE)
}

dina_finished_include_latest_confirmation <- function(root) {
  base <- file.path(root, "output", "experiments", "finished_dina_include", "confirms")
  paths <- list.files(base, pattern = "^confirmation\\.json$", recursive = TRUE, full.names = TRUE)
  paths <- paths[order(file.info(paths)$mtime, decreasing = TRUE)]
  for (path in paths) {
    journal <- dina_read_json(path, default = NULL)
    if (identical(journal$family, "dina") && identical(journal$status, "confirmed")) {
      return(journal$run)
    }
  }
  NULL
}

dina_finished_include_publish <- function(candidate, root, record, sources,
                                          engine = NULL, copy_file = dina_finished_include_copy,
                                          save_record = dina_review_save) {
  dina_finished_include_verify(candidate, root, record, sources, engine)
  base <- dina_finished_output_root(root, "output/experiments/finished_dina_include")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  lock <- file.path(base, ".publishing")
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Another finished DINA publication is active. Inspect ", lock, " before retrying.", call. = FALSE)
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  confirm <- file.path(base, "confirms", paste0("confirm-", format(Sys.time(), "%Y%m%d-%H%M%S"),
    "-", basename(tempfile())))
  dina_write_json(list(pid = Sys.getpid(), confirmation = confirm, started_at = dina_now()),
    file.path(lock, "owner.json"))
  dir.create(file.path(confirm, "backups"), recursive = TRUE, showWarnings = FALSE)
  pointer <- dina_review_pointer(root, "dina")
  entries <- list()
  paths <- c(vapply(candidate$artifacts, function(x) x$destination, character(1)), pointer)
  if (anyDuplicated(paths)) stop("Duplicate DINA publication destinations.", call. = FALSE)
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    existed <- file.exists(path)
    if (dir.exists(path)) stop("Publication destination is a directory: ", path, call. = FALSE)
    backup <- file.path(confirm, "backups", paste0(i, "-", basename(path)))
    if (existed && !file.copy(path, backup)) stop("Could not back up ", path, call. = FALSE)
    entries[[i]] <- list(destination = path, existed = existed, backup = backup,
      before_sha256 = if (existed) dina_hash_file(path) else NULL,
      after_sha256 = if (i <= length(candidate$artifacts)) candidate$artifacts[[i]]$sha256 else NULL)
  }
  manifest <- list(family = "dina", status = "publishing", run = confirm,
    candidate = candidate$run, started_at = dina_now(), pid = Sys.getpid(), entries = entries,
    previous_confirmation = dina_finished_include_latest_confirmation(root),
    artifacts = lapply(candidate$artifacts, function(a) a[c("destination", "sha256", "role", "source_id")]))
  manifest_path <- file.path(confirm, "confirmation.json")
  dina_finished_include_write_journal(manifest, manifest_path)
  completed <- FALSE
  on.exit({
    if (!completed) {
      dina_finished_include_rollback(entries)
      manifest$status <- "rolled_back"
      dina_finished_include_write_journal(manifest, manifest_path)
    }
  }, add = TRUE, after = FALSE)
  # Recheck immediately before the first scientific-file mutation.
  dina_finished_include_verify(candidate, root, record, sources, engine)
  for (artifact in candidate$artifacts) {
    copy_file(artifact$staged, artifact$destination)
  }
  record$status <- "included"
  record$review_only <- FALSE
  record$included_at <- dina_now()
  record$confirmation <- confirm
  record$include_candidate <- candidate$run
  record$accepted_candidate <- candidate$run
  record$included_artifacts <- lapply(candidate$artifacts, function(a) a[c("destination", "sha256", "role", "source_id")])
  record$acceptance_watch <- dina_review_watch(c(
    vapply(sources, function(x) x$input, character(1)),
    vapply(candidate$artifacts, function(x) x$destination, character(1))))
  record$watch <- record$acceptance_watch
  staged_record <- file.path(confirm, "accepted-review.json")
  dina_write_json(record, staged_record)
  manifest$entries[[length(entries)]]$after_sha256 <- dina_hash_file(staged_record)
  # Record both possible pointer generations before its atomic replacement.
  dina_finished_include_write_journal(manifest, manifest_path)
  save_record(record, root)
  manifest$status <- "confirmed"
  manifest$completed_at <- dina_now()
  manifest$artifacts <- record$included_artifacts
  # The confirmation is the final completion record. No scientific writes follow.
  dina_finished_include_write_journal(manifest, manifest_path)
  completed <- TRUE
  invisible(record)
}

dina_finished_include_restore <- function(root, path, record = dina_review_read(root, "dina")) {
  path <- dina_finished_include_path(root, path, "output/experiments")
  manifest_path <- file.path(path, "confirmation.json")
  manifest <- dina_read_json(manifest_path, default = NULL)
  if (is.null(manifest) || !identical(manifest$family, "dina") ||
      !identical(manifest$run, path)) stop("Not a finished DINA inclusion backup: ", path, call. = FALSE)
  if (identical(manifest$status, "restored")) {
    dina_cli_cat("This finished DINA inclusion was already restored.")
    return(invisible(record))
  }
  recovering <- identical(manifest$status, "publishing")
  if (!manifest$status %in% c("confirmed", "publishing")) {
    stop("This inclusion did not complete; its backup cannot be restored automatically.", call. = FALSE)
  }
  if (!recovering && !identical(dina_finished_include_latest_confirmation(root), path)) {
    stop("A newer inclusion is active. Restore that inclusion first.", call. = FALSE)
  }
  if (recovering && dina_finished_include_pid_alive(manifest$pid)) {
    stop("This DINA publication process is still running; recovery cannot replace its files.", call. = FALSE)
  }
  if (recovering && !identical(dina_finished_include_latest_confirmation(root),
      manifest$previous_confirmation)) {
    stop("A newer inclusion completed after this interrupted publication. Restore it first.", call. = FALSE)
  }
  lock <- file.path(root, "output", "experiments", "finished_dina_include", ".publishing")
  previous_owner <- list(pid = manifest$pid, confirmation = path, started_at = manifest$started_at)
  restored <- FALSE
  if (dir.exists(lock)) {
    owner <- dina_read_json(file.path(lock, "owner.json"), default = NULL)
    if (is.null(owner) || !identical(owner$confirmation, path) ||
        dina_finished_include_pid_alive(owner$pid)) {
      stop("The DINA publication lock belongs to a live or different operation.", call. = FALSE)
    }
    previous_owner <- owner
    # Only an explicit recovery of this dead owner's journal may clear its lock.
    unlink(lock, recursive = TRUE)
  }
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Another finished DINA publication is active. Inspect ", lock, " before retrying.", call. = FALSE)
  }
  on.exit({
    if (recovering && !restored) {
      dina_write_json(previous_owner, file.path(lock, "owner.json"))
    } else unlink(lock, recursive = TRUE)
  }, add = TRUE)
  dina_write_json(list(pid = Sys.getpid(), confirmation = path, restoring = TRUE),
    file.path(lock, "owner.json"))
  for (artifact in if (!recovering) manifest$artifacts else list()) {
    dina_finished_include_path(root, artifact$destination, "input_data/dina")
    if (!identical(dina_hash_file(artifact$destination), artifact$sha256)) {
      stop("Accepted DINA file changed after inclusion; restore would overwrite it: ",
        artifact$destination, call. = FALSE)
    }
  }
  pointer <- dina_review_pointer(root, "dina")
  for (entry in manifest$entries) {
    if (!identical(entry$destination, pointer)) {
      dina_finished_include_path(root, entry$destination, "input_data/dina")
    }
    if (isTRUE(entry$existed) && (!startsWith(entry$backup, paste0(path, "/backups/")) ||
        !identical(dina_hash_file(entry$backup), entry$before_sha256))) {
      stop("A restore backup is missing or changed: ", entry$backup, call. = FALSE)
    }
    if (recovering) {
      current <- dina_hash_file(entry$destination)
      expected <- unlist(entry[c("before_sha256", "after_sha256")], use.names = FALSE)
      absent_new <- !isTRUE(entry$existed) && !file.exists(entry$destination)
      if (!absent_new && (is.na(current) || !current %in% expected)) {
        stop("DINA recovery found changes outside the interrupted publication: ",
          entry$destination, call. = FALSE)
      }
    }
  }
  # Keep the currently accepted generation available if restoration fails.
  saved <- lapply(seq_along(manifest$entries), function(i) {
    entry <- manifest$entries[[i]]
    file <- file.path(path, paste0(".restore-current-", i))
    existed <- file.exists(entry$destination)
    if (existed && !file.copy(entry$destination, file, overwrite = TRUE)) {
      stop("Could not stage the current inclusion for safe restoration.", call. = FALSE)
    }
    list(destination = entry$destination, existed = existed, backup = file,
      before_sha256 = if (existed) dina_hash_file(file) else NULL)
  })
  restored <- FALSE
  on.exit({
    if (!restored) dina_finished_include_rollback(saved)
    unlink(vapply(saved, function(x) x$backup, character(1)))
  }, add = TRUE, after = FALSE)
  dina_finished_include_rollback(manifest$entries)
  manifest$status <- "restored"
  manifest$restored_at <- dina_now()
  if (recovering) manifest$recovered_interrupted_publication <- TRUE
  dina_finished_include_write_journal(manifest, manifest_path)
  restored <- TRUE
  dina_cli_cat("Restored the previous finished DINA input generation.")
  invisible(dina_review_read(root, "dina"))
}

dina_finished_include <- function(root, flags = list(), input = "stdin",
                                   is_terminal = isatty(stdin()),
                                   record = dina_review_read(root, "dina"),
                                   engine = NULL) {
  if (isTRUE(flags[["dry-run"]]) && (isTRUE(flags$confirm) || !is.null(flags$restore))) {
    stop("A dry run cannot be combined with acceptance or restore.", call. = FALSE)
  }
  if (!is.null(flags$restore) && (isTRUE(flags$confirm) || !is.null(flags[["include-run"]]))) {
    stop("Choose acceptance or restore, not both.", call. = FALSE)
  }
  if (isTRUE(flags$apply) || !is.null(flags[["exploration-run"]])) {
    stop("Use --dry-run to prepare, --include-run to select a candidate, or --restore for its backup.", call. = FALSE)
  }
  if (!is.null(flags$restore)) return(dina_finished_include_restore(root, flags$restore, record))
  if (is.null(engine)) engine <- dina_finished_include_engine(root)
  sources <- dina_finished_include_sources(root)
  dina_finished_include_verify_review(root, record, sources)
  explicit_run <- flags[["include-run"]]
  candidate <- if (!is.null(explicit_run)) {
    dina_finished_include_candidate(root, record, explicit_run)
  } else if (isTRUE(flags[["dry-run"]]) || is.null(record$include_candidate)) {
    NULL
  } else dina_finished_include_candidate(root, record)
  if (is.null(candidate)) {
    candidate <- dina_finished_include_prepare(root, record, flags, engine)
    record <- dina_review_read(root, "dina")
  }
  dina_finished_include_print(record, candidate, root, filters = flags)
  if (!candidate$status %in% c("ready", "accepted")) {
    stop("Finished DINA preparation needs attention. Inspect dina sources table dina prepared-problems --run ",
      candidate$run, ".", call. = FALSE)
  }
  dina_finished_include_verify(candidate, root, record, sources, engine)
  if (isTRUE(flags[["dry-run"]])) {
    dina_cli_cat("Candidate prepared for review. Accepted inputs and publications are unchanged.")
    return(invisible(record))
  }
  same <- identical(record$status, "included") &&
    identical(record$accepted_candidate, candidate$run)
  if (same) {
    for (artifact in candidate$artifacts) {
      if (!identical(dina_hash_file(artifact$destination), artifact$sha256)) {
        stop("An accepted DINA artifact is missing or changed. Inspect it before accepting again: ",
          artifact$destination, call. = FALSE)
      }
    }
    dina_cli_cat("This finished DINA candidate is already included; no files changed.")
    return(invisible(record))
  }
  confirmed <- isTRUE(flags$confirm)
  if (!confirmed && isTRUE(is_terminal)) {
    confirmed <- dina_menu_confirm("Include finished DINA series",
      "Accept the complete prepared candidate?", input = input,
      is_terminal = is_terminal, preserve_screen = TRUE)
  }
  if (!confirmed) {
    dina_cli_cat("No accepted files changed. Accept this candidate with: dina sources include dina --confirm")
    return(invisible(record))
  }
  accepted <- dina_finished_include_publish(candidate, root, record, sources, engine)
  dina_cli_cat("Finished DINA inputs included. Continuation and publication replacement remain deferred.")
  dina_cli_cat("Restore: dina sources include dina --restore ", accepted$confirmation)
  invisible(accepted)
}
