# WID source include/confirm/restore workflow.
#
# Sourcing this file also exposes run_wid_explorer() during the transition from
# the former combined explorer/include module.

wid_include_module_file <- local({
  files <- vapply(sys.frames(), function(frame) {
    value <- frame$ofile
    if (is.null(value) || !nzchar(value)) "" else as.character(value)
  }, character(1))
  files <- files[nzchar(files)]
  if (length(files)) normalizePath(files[[length(files)]], mustWork = FALSE) else ""
})
wid_include_module_dir <- if (nzchar(wid_include_module_file)) dirname(wid_include_module_file) else file.path(getwd(), "code", "R", "source-diagnostics")
source(file.path(wid_include_module_dir, "wid_explorer.R"), local = environment())

wid_include_prepare_one <- function(root, contract, paths, artifact) {
  source_id <- artifact$source_id
  years <- wid_include_years(root, contract, artifact)
  required_years <- wid_include_required_years(root, contract, artifact)
  stage_paths <- wid_include_artifact_paths(root, artifact, staged_repo = paths$staged_repo)
  prod_paths <- wid_include_artifact_paths(root, artifact)
  result <- tryCatch({
    raw <- wid_include_fetch_raw(root, contract, artifact, stage_paths$raw)
    candidate <- wid_include_derive_artifact(raw, artifact, years)
    wid_include_write_dta(candidate, stage_paths$canonical)
    validation <- wid_include_validate_candidate(source_id, artifact, candidate, required_years)
    comparison <- wid_include_compare_artifact(source_id, artifact, stage_paths$canonical, prod_paths$canonical, candidate)
    list(ok = !any(validation$severity == "blocked", na.rm = TRUE), raw = raw, candidate = candidate, validation = validation, comparison = comparison, error = "")
  }, error = function(e) {
    validation <- data.frame(source_id = source_id, check = "derive_or_fetch", status = if (grepl("WID download failed|WID returned", conditionMessage(e))) "blocked_fetch_failed" else "blocked_derivation_failed", severity = "blocked", detail = conditionMessage(e), next_command = "dina sources include wid --dry-run", stringsAsFactors = FALSE)
    list(ok = FALSE, raw = data.frame(stringsAsFactors = FALSE), candidate = data.frame(stringsAsFactors = FALSE), validation = validation, comparison = list(artifact = data.frame(stringsAsFactors = FALSE), numeric = data.frame(stringsAsFactors = FALSE)), error = conditionMessage(e))
  })
  candidate_read <- wid_include_read_dta(stage_paths$canonical)
  raw_read <- wid_include_read_dta(stage_paths$raw)
  inventory <- wid_include_bind(
    wid_include_dataset_status(source_id, "derived", "candidate", stage_paths$canonical, wid_include_relative_path(stage_paths$canonical, root), artifact$canonical, candidate_read$data, candidate_read$ok, candidate_read$error),
    wid_include_dataset_status(source_id, "raw", "candidate", stage_paths$raw, wid_include_relative_path(stage_paths$raw, root), artifact$raw, raw_read$data, raw_read$ok, raw_read$error)
  )
  promotions <- if (isTRUE(result$ok)) {
    wid_include_bind(
      data.frame(source_id = source_id, artifact_type = "derived", from_rel = stage_paths$canonical, to_rel = stage_paths$canonical_rel, promotion_scope = "promote", stringsAsFactors = FALSE),
      data.frame(source_id = source_id, artifact_type = "raw", from_rel = stage_paths$raw, to_rel = stage_paths$raw_rel, promotion_scope = "promote", stringsAsFactors = FALSE)
    )
  } else {
    data.frame(source_id = character(), artifact_type = character(), from_rel = character(), to_rel = character(), promotion_scope = character(), stringsAsFactors = FALSE)
  }
  list(source_id = source_id, ok = result$ok, validation = result$validation, inventory = inventory, comparison = result$comparison$artifact, numeric = result$comparison$numeric, promotion_plan = promotions)
}

wid_include_read_exploration <- function(root, contract, exploration_run = NULL) {
  run <- normalizePath(wid_include_path(exploration_run %||% contract$explore_output_root, root), mustWork = FALSE)
  tables <- file.path(run, "tables")
  logs <- file.path(run, "logs")
  list(
    root = run,
    wid_request_plan = wid_include_read_csv(file.path(tables, "wid_request_plan.csv")),
    source_inventory = wid_include_read_csv(file.path(tables, "source_inventory.csv")),
    validation_report = wid_include_read_csv(file.path(tables, "validation_report.csv")),
    wid_artifact_status = wid_include_read_csv(file.path(tables, "wid_artifact_status.csv")),
    wid_artifact_comparison = wid_include_read_csv(file.path(tables, "wid_artifact_comparison.csv")),
    wid_numeric_comparison = wid_include_read_csv(file.path(tables, "wid_numeric_comparison.csv")),
    review_actions = wid_include_read_csv(file.path(tables, "review_actions.csv")),
    unsupported_sources = wid_include_read_csv(file.path(tables, "unsupported_sources.csv")),
    explore_manifest = wid_include_read_csv(file.path(logs, "explore_manifest.csv"))
  )
}

wid_include_promotion_fingerprints <- function(promotion_plan) {
  if (!nrow(promotion_plan)) {
    return(data.frame(source_id = character(), artifact_type = character(), from_rel = character(), to_rel = character(), exists = logical(), kind = character(), hash_algorithm = character(), hash = character(), stringsAsFactors = FALSE))
  }
  algo <- wid_include_hash_algorithm()
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    exists <- file.exists(from) && !dir.exists(from)
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from_rel = from, to_rel = row$to_rel, exists = exists, kind = if (exists) "file" else "missing", hash_algorithm = algo, hash = if (exists) wid_include_hash_path(from) else NA_character_, stringsAsFactors = FALSE)
  })
  wid_include_bind(rows)
}

wid_include_source_fingerprints <- function(promotion_plan) {
  staged_raw <- promotion_plan[promotion_plan$artifact_type == "raw", , drop = FALSE]
  if (!nrow(staged_raw)) {
    return(data.frame(source_id = character(), source_set = character(), rel = character(), exists = logical(), kind = character(), hash_algorithm = character(), hash = character(), stringsAsFactors = FALSE))
  }
  algo <- wid_include_hash_algorithm()
  rows <- lapply(seq_len(nrow(staged_raw)), function(i) {
    row <- staged_raw[i, , drop = FALSE]
    path <- row$from_rel[[1L]]
    exists <- file.exists(path) && !dir.exists(path)
    data.frame(source_id = row$source_id, source_set = "staged_raw", rel = path, exists = exists, kind = if (exists) "file" else "missing", hash_algorithm = algo, hash = if (exists) wid_include_hash_path(path) else NA_character_, stringsAsFactors = FALSE)
  })
  wid_include_bind(rows)
}

wid_include_summary <- function(prepared, promotion_plan) {
  if (!length(prepared)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(prepared, function(item) {
    inv <- item$inventory[item$inventory$artifact_type == "derived", , drop = FALSE]
    blocked <- if (nrow(item$validation)) sum(item$validation$severity == "blocked", na.rm = TRUE) else 0L
    promos <- promotion_plan[promotion_plan$source_id == item$source_id, , drop = FALSE]
    data.frame(
      source_id = item$source_id,
      status = if (blocked > 0L || !nrow(promos)) "blocked" else "all_good",
      incoming_rows = if (nrow(inv)) inv$rows[[1L]] else 0L,
      incoming_countries = if (nrow(inv)) inv$countries[[1L]] else NA_integer_,
      incoming_first_year = if (nrow(inv)) inv$first_year[[1L]] else NA_integer_,
      incoming_last_year = if (nrow(inv)) inv$last_year[[1L]] else NA_integer_,
      staged_sources = nrow(promos),
      promotions = nrow(promos),
      overlap_rows = NA_integer_,
      coverage_differences = NA_integer_,
      warnings = 0L,
      blocked = blocked,
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
}

wid_include_overall_status <- function(summary) {
  if (!nrow(summary)) return("blocked")
  if (any(summary$status == "blocked", na.rm = TRUE)) return("blocked")
  "all_good"
}

run_wid_include <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  exploration_run = NULL,
  output_dir = NULL,
  write_outputs = TRUE,
  run_id = NULL
) {
  contract <- wid_include_read_contract(root, contract_path)
  run_id <- run_id %||% wid_include_run_id()
  paths <- wid_include_output_paths_for_run(root, contract, output_dir, run_id)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$staged_repo, recursive = TRUE, showWarnings = FALSE)
  exploration <- wid_include_read_exploration(root, contract, exploration_run)
  if (!nrow(exploration$source_inventory)) {
    fresh <- run_wid_explorer(root = root, contract_path = contract_path, write_outputs = FALSE, dry_run = TRUE)
    exploration <- c(list(root = fresh$paths$root), fresh$outputs)
  }
  prepared <- lapply(wid_include_artifacts(contract), function(artifact) wid_include_prepare_one(root, contract, paths, artifact))
  validation <- wid_include_bind(lapply(prepared, `[[`, "validation"))
  inventory <- wid_include_bind(exploration$source_inventory, lapply(prepared, `[[`, "inventory"))
  comparison <- wid_include_bind(lapply(prepared, `[[`, "comparison"))
  numeric <- wid_include_bind(lapply(prepared, `[[`, "numeric"))
  promotion_plan <- wid_include_bind(lapply(prepared, `[[`, "promotion_plan"))
  source_fingerprints <- wid_include_source_fingerprints(promotion_plan)
  promotion_fingerprints <- wid_include_promotion_fingerprints(promotion_plan)
  summary <- wid_include_summary(prepared, promotion_plan)
  status <- wid_include_overall_status(summary)
  manifest <- wid_include_manifest(run_id, status, contract, dry_run = TRUE, exploration_root = exploration$root)
  outputs <- list(
    wid_request_plan = exploration$wid_request_plan %||% wid_include_request_plan(root, contract),
    source_inventory = inventory,
    include_detail = validation,
    validation_report = validation,
    include_summary = summary,
    staged_source_mappings = data.frame(stringsAsFactors = FALSE),
    promotion_plan = promotion_plan,
    source_fingerprints = source_fingerprints,
    promotion_fingerprints = promotion_fingerprints,
    wid_artifact_comparison = comparison,
    wid_numeric_comparison = numeric,
    coverage_differences = data.frame(stringsAsFactors = FALSE),
    overlap_differences = data.frame(stringsAsFactors = FALSE),
    overlap_summary = data.frame(stringsAsFactors = FALSE),
    overlap_year_summary = data.frame(stringsAsFactors = FALSE),
    include_manifest = manifest
  )
  if (isTRUE(write_outputs)) {
    wid_include_write_csvs(outputs, paths, "include_manifest")
  }
  list(paths = paths, outputs = outputs, manifest = manifest, contract = contract, run_id = run_id)
}

wid_include_manifest_value <- function(manifest, key) {
  if (!nrow(manifest) || !("key" %in% names(manifest)) || !("value" %in% names(manifest))) return("")
  hit <- manifest$key == key
  if (!any(hit)) "" else as.character(manifest$value[which(hit)[[1L]]])
}

wid_include_resolve_run <- function(root, contract, include_run) {
  if (is.null(include_run) || !nzchar(include_run)) stop("--include-run is required for WID confirm.", call. = FALSE)
  candidates <- normalizePath(c(include_run, file.path(root, include_run), file.path(wid_include_output_root(root, contract), "runs", include_run)), mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, "logs", "include_manifest.csv"))]
  if (!length(hit)) stop("WID include run not found: ", include_run, call. = FALSE)
  hit[[1L]]
}

wid_include_resolve_confirm <- function(root, contract, confirm_run) {
  if (is.null(confirm_run) || !nzchar(confirm_run)) stop("Confirm run is required for WID restore.", call. = FALSE)
  candidates <- normalizePath(c(confirm_run, file.path(root, confirm_run), file.path(wid_include_output_root(root, contract), "confirms", confirm_run)), mustWork = FALSE)
  hit <- candidates[file.exists(file.path(candidates, "logs", "confirm_manifest.csv"))]
  if (!length(hit)) stop("WID confirm run not found: ", confirm_run, call. = FALSE)
  hit[[1L]]
}

wid_include_verify_promotion_fingerprints <- function(include_run, promotion_plan) {
  expected <- wid_include_read_csv(file.path(include_run, "tables", "promotion_fingerprints.csv"))
  if (!nrow(promotion_plan)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    hit <- expected[expected$source_id == row$source_id & expected$artifact_type == row$artifact_type & expected$to_rel == row$to_rel, , drop = FALSE]
    expected_hash <- if (nrow(hit)) hit$hash[[1L]] else NA_character_
    exists <- file.exists(from) && !dir.exists(from)
    hash <- if (exists) wid_include_hash_path(from) else NA_character_
    status <- if (!nrow(hit)) "missing_dry_run_fingerprint" else if (!exists) "missing_staged_artifact" else if (!identical(as.character(expected_hash), as.character(hash))) "hash_changed" else "ok"
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from = from, to = row$to_rel, dry_run_hash = expected_hash, current_hash = hash, status = status, stringsAsFactors = FALSE)
  })
  report <- wid_include_bind(rows)
  if (nrow(report) && any(report$status != "ok", na.rm = TRUE)) {
    bad <- report[report$status != "ok", , drop = FALSE]
    stop("Confirm refused: staged WID artifacts changed since dry-run. Rerun the include dry-run. First mismatch: ", bad$to[[1L]], " (", bad$status[[1L]], ").", call. = FALSE)
  }
  report
}

wid_include_copy_path <- function(from, to) {
  if (!file.exists(from)) return("missing_source")
  if (dir.exists(from)) return("unsupported_directory_source")
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(file.copy(from, to, overwrite = TRUE, copy.date = TRUE))) "staged" else "copy_failed"
}

wid_include_confirm_manifest <- function(confirm_id, include_run, status) {
  data.frame(key = c("confirm_id", "source_type", "workflow", "status", "include_run", "confirmed_at"), value = c(confirm_id, "wid", "wid_sources", status, normalizePath(include_run, mustWork = FALSE), as.character(Sys.time())), stringsAsFactors = FALSE)
}

wid_include_confirm_sources <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  include_run = NULL,
  output_dir = NULL
) {
  contract <- wid_include_read_contract(root, contract_path)
  include_run <- wid_include_resolve_run(root, contract, include_run)
  include_manifest <- wid_include_read_csv(file.path(include_run, "logs", "include_manifest.csv"))
  status <- wid_include_manifest_value(include_manifest, "status")
  if (!identical(status, "all_good")) stop("WID include run is not clean; status is ", status, ".", call. = FALSE)
  promotion_plan <- wid_include_read_csv(file.path(include_run, "tables", "promotion_plan.csv"))
  if (!nrow(promotion_plan)) stop("WID include run has no staged artifact to promote.", call. = FALSE)
  staged_check <- wid_include_verify_promotion_fingerprints(include_run, promotion_plan)
  missing <- promotion_plan[!file.exists(promotion_plan$from_rel), , drop = FALSE]
  if (nrow(missing)) stop("Confirm refused: staged WID artifacts are missing.", call. = FALSE)
  confirm_id <- wid_include_confirm_id()
  paths <- wid_include_output_paths_for_confirm(root, contract, output_dir, confirm_id)
  report <- lapply(seq_len(nrow(promotion_plan)), function(i) {
    row <- promotion_plan[i, , drop = FALSE]
    from <- row$from_rel[[1L]]
    to <- file.path(root, row$to_rel[[1L]])
    backup <- file.path(paths$snapshots, "original", row$to_rel[[1L]])
    backup_status <- if (file.exists(to)) wid_include_copy_path(to, backup) else "destination_absent"
    promote_status <- if (backup_status %in% c("staged", "destination_absent")) {
      wid_include_copy_path(from, to)
    } else {
      "skipped_backup_failed"
    }
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, from = from, to = row$to_rel, backup = if (identical(backup_status, "destination_absent")) "" else wid_include_relative_path(backup, root), backup_status = if (identical(backup_status, "staged")) "backed_up" else backup_status, promote_status = promote_status, stringsAsFactors = FALSE)
  })
  promote_report <- wid_include_bind(report)
  dir.create(paths$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$logs, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(promote_report, file.path(paths$tables, "promote_report.csv"), row.names = FALSE, na = "")
  utils::write.csv(staged_check, file.path(paths$tables, "staged_artifact_fingerprint_check.csv"), row.names = FALSE, na = "")
  utils::write.csv(data.frame(stringsAsFactors = FALSE), file.path(paths$tables, "source_fingerprint_check.csv"), row.names = FALSE, na = "")
  bad_copy <- promote_report[
    !(promote_report$backup_status %in% c("backed_up", "destination_absent")) |
      promote_report$promote_status != "staged",
    ,
    drop = FALSE
  ]
  if (nrow(bad_copy)) {
    stop("WID confirm copy failed for ", bad_copy$to[[1L]], ".", call. = FALSE)
  }
  manifest <- wid_include_confirm_manifest(confirm_id, include_run, "confirmed")
  utils::write.csv(manifest, file.path(paths$logs, "confirm_manifest.csv"), row.names = FALSE, na = "")
  list(paths = paths, outputs = list(promote_report = promote_report, source_fingerprint_check = data.frame(stringsAsFactors = FALSE), staged_artifact_fingerprint_check = staged_check), manifest = manifest, contract = contract)
}

wid_include_restore_sources <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  confirm_run = NULL
) {
  contract <- wid_include_read_contract(root, contract_path)
  confirm_run <- wid_include_resolve_confirm(root, contract, confirm_run)
  promote_report <- wid_include_read_csv(file.path(confirm_run, "tables", "promote_report.csv"))
  rows <- lapply(seq_len(nrow(promote_report)), function(i) {
    row <- promote_report[i, , drop = FALSE]
    dest <- file.path(root, row$to[[1L]])
    if (identical(row$backup_status[[1L]], "backed_up")) {
      restore_status <- wid_include_copy_path(file.path(root, row$backup[[1L]]), dest)
    } else if (identical(row$backup_status[[1L]], "destination_absent")) {
      if (file.exists(dest)) unlink(dest, recursive = TRUE)
      restore_status <- "removed_promoted_destination"
    } else {
      restore_status <- "no_backup_available"
    }
    data.frame(source_id = row$source_id, artifact_type = row$artifact_type, destination = row$to, restore_status = restore_status, stringsAsFactors = FALSE)
  })
  restore_report <- wid_include_bind(rows)
  utils::write.csv(restore_report, file.path(confirm_run, "tables", "restore_report.csv"), row.names = FALSE, na = "")
  list(paths = list(root = confirm_run, restore_report = file.path(confirm_run, "tables", "restore_report.csv")), outputs = list(restore_report = restore_report), contract = contract)
}
