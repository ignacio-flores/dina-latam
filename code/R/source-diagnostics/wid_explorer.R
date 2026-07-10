# WID source explorer.
#
# This file is review-only: it inspects local WID artifacts and writes tables
# describing the configured WID requests, mappings, and artifact status.

wid_explorer_module_file <- local({
  files <- vapply(sys.frames(), function(frame) {
    value <- frame$ofile
    if (is.null(value) || !nzchar(value)) "" else as.character(value)
  }, character(1))
  files <- files[nzchar(files)]
  if (length(files)) normalizePath(files[[length(files)]], mustWork = FALSE) else ""
})
wid_explorer_module_dir <- if (nzchar(wid_explorer_module_file)) dirname(wid_explorer_module_file) else file.path(getwd(), "code", "R", "source-diagnostics")
source(file.path(wid_explorer_module_dir, "wid_common.R"), local = environment())

wid_include_current_status <- function(root, contract, artifact) {
  paths <- wid_include_artifact_paths(root, artifact)
  read <- wid_include_read_dta(paths$canonical)
  status <- wid_include_dataset_status(artifact$source_id, "derived", "current", paths$canonical, paths$canonical_rel, paths$canonical_rel, read$data, read$ok, read$error)
  inputs <- wid_include_workflow_inputs(root, contract)
  inputs <- inputs[file.exists(inputs)]
  latest_input <- if (length(inputs)) max(file.info(inputs)$mtime, na.rm = TRUE) else as.POSIXct(NA)
  if (isTRUE(status$exists[[1L]]) && !is.na(latest_input) && latest_input > file.info(paths$canonical)$mtime[[1L]]) {
    status$status <- "stale"
  }
  status
}

wid_include_review_actions <- function(status, validation = data.frame(stringsAsFactors = FALSE)) {
  if (!nrow(status)) return(data.frame(stringsAsFactors = FALSE))
  candidate <- status[status$source_set %in% c("_new", "candidate"), , drop = FALSE]
  blocked_candidates <- candidate$source_id[candidate$status %in% c("missing_candidate", "read_failed")]
  if (nrow(validation) && all(c("source_id", "severity") %in% names(validation))) {
    blocked_candidates <- unique(c(blocked_candidates, validation$source_id[validation$severity == "blocked"]))
  }
  incoming_sources <- unique(candidate$source_id)
  rows <- lapply(unique(status$source_id), function(source_id) {
    part <- status[status$source_id == source_id, , drop = FALSE]
    current <- part[part$source_set == "current", , drop = FALSE]
    needs_fetch <- current$status %in% c("missing_current_artifact", "stale")
    incoming <- source_id %in% incoming_sources
    blocked <- source_id %in% blocked_candidates
    data.frame(
      source_id = source_id,
      action = if (isTRUE(blocked)) "fetch_blocked" else if (isTRUE(incoming)) "review_include" else if (any(needs_fetch)) "fetch_wid" else "no_action",
      severity = if (isTRUE(blocked)) "blocked" else "info",
      next_command = if (isTRUE(blocked) || any(needs_fetch) && !isTRUE(incoming)) "dina sources explore wid --fetch" else if (isTRUE(incoming)) "dina sources include wid --dry-run" else "",
      detail = if (isTRUE(blocked)) {
        "Incoming WID candidate is missing or unreadable; rerun the WID fetch."
      } else if (isTRUE(incoming)) {
        "Incoming WID candidate in input_data/_new/wid is ready for include review."
      } else if (any(needs_fetch)) {
        "WID artifact is missing or older than WID workflow inputs; fetch through the WID explorer."
      } else {
        "WID artifact is present."
      },
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
}

wid_include_fetch_needed <- function(inventory) {
  nrow(inventory) && any(inventory$source_set == "current" & inventory$status %in% c("missing_current_artifact", "stale"), na.rm = TRUE)
}

wid_include_unsupported_sources <- function(root, contract) {
  wid_sources <- wid_include_registry_wid_sources(root)
  supported <- wid_include_supported_ids(contract)
  unsupported <- Filter(function(source) {
    source_id <- source$id %||% ""
    covered_by <- source$covered_by %||% ""
    !(source_id %in% supported || covered_by %in% supported)
  }, wid_sources)
  if (!length(unsupported)) return(data.frame(stringsAsFactors = FALSE))
  wid_include_bind(lapply(unsupported, function(source) {
    data.frame(source_id = source$id %||% "", family = source$family %||% "", method = source$method %||% "", status = "unsupported_by_wid_workflow", detail = "Listed by the registry but not included in config/wid_include.yml source_ids.", stringsAsFactors = FALSE)
  }))
}

wid_include_year_range_label <- function(years) {
  if (!length(years)) return("")
  paste0(min(years, na.rm = TRUE), "-", max(years, na.rm = TRUE))
}

wid_include_request_plan <- function(root, contract) {
  artifacts <- wid_include_artifacts(contract)
  rows <- lapply(artifacts, function(artifact) {
    request <- artifact$request %||% list()
    paths <- wid_include_artifact_paths(root, artifact)
    areas <- wid_include_areas(request$area_set %||% request$areas, artifact)
    years <- tryCatch(wid_include_years(root, contract, artifact), error = function(e) integer())
    required_years <- tryCatch(wid_include_required_years(root, contract, artifact), error = function(e) integer())
    data.frame(
      source_id = artifact$source_id,
      artifact_type = artifact$type %||% "raw_subset",
      area_set = paste(wid_include_source_values(request$area_set %||% request$areas), collapse = ","),
      area_count = length(areas),
      areas = paste(areas, collapse = ","),
      indicators = paste(wid_include_source_values(request$indicators), collapse = ","),
      ages = paste(wid_include_source_values(request$ages), collapse = ","),
      percentiles = paste(wid_include_source_values(request$perc), collapse = ","),
      population_flag = paste(wid_include_source_values(request$pop), collapse = ","),
      request_years = wid_include_year_range_label(years),
      required_years = wid_include_year_range_label(required_years),
      raw_output = paths$raw_rel,
      derived_output = paths$canonical_rel,
      mapping_summary = wid_include_mapping_summary(wid_include_output_map(artifact)),
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
}

run_wid_explorer <- function(
  root = wid_include_repo_root(),
  contract_path = file.path(root, "config", "wid_include.yml"),
  output_dir = NULL,
  write_outputs = TRUE,
  dry_run = FALSE,
  fetch = FALSE
) {
  contract <- wid_include_read_contract(root, contract_path)
  paths <- wid_include_explore_paths(root, contract, output_dir)
  artifacts <- wid_include_artifacts(contract)
  request_plan <- wid_include_request_plan(root, contract)
  current_inventory <- wid_include_bind(lapply(artifacts, function(artifact) wid_include_current_status(root, contract, artifact)))
  needs_fetch <- wid_include_fetch_needed(current_inventory)
  incoming <- lapply(artifacts, function(artifact) wid_include_inspect_incoming_one(root, contract, artifact, require_candidate = FALSE))
  fetch_validation <- data.frame(stringsAsFactors = FALSE)
  publish_report <- data.frame(stringsAsFactors = FALSE)
  fetch_attempted <- isTRUE(fetch) && isTRUE(needs_fetch) && !isTRUE(dry_run)
  if (isTRUE(fetch_attempted)) {
    dir.create(paths$fetch_tmp, recursive = TRUE, showWarnings = FALSE)
    prepared <- lapply(artifacts, function(artifact) wid_include_prepare_candidate_one(root, contract, paths, artifact))
    fetch_validation <- wid_include_bind(lapply(prepared, `[[`, "validation"))
    if (!any(fetch_validation$severity == "blocked", na.rm = TRUE)) {
      publish_report <- wid_include_publish_fetch_to_incoming(root, prepared)
      fetch_validation <- wid_include_publish_validation(publish_report)
    }
    if (!any(fetch_validation$severity == "blocked", na.rm = TRUE)) {
      incoming <- lapply(artifacts, function(artifact) wid_include_inspect_incoming_one(root, contract, artifact, require_candidate = FALSE))
      fetch_validation <- data.frame(stringsAsFactors = FALSE)
    }
  }
  candidate_inventory <- wid_include_bind(lapply(incoming, `[[`, "inventory"))
  inventory <- wid_include_bind(current_inventory, candidate_inventory)
  validation <- wid_include_bind(lapply(incoming, `[[`, "validation"), fetch_validation)
  comparison <- wid_include_bind(lapply(incoming, `[[`, "comparison"))
  numeric <- wid_include_bind(lapply(incoming, `[[`, "numeric"))
  promotion_plan <- wid_include_bind(lapply(incoming, `[[`, "promotion_plan"))
  source_fingerprints <- wid_include_source_fingerprints(promotion_plan)
  promotion_fingerprints <- wid_include_promotion_fingerprints(promotion_plan)
  artifact_status <- data.frame(
    source_id = current_inventory$source_id,
    artifact = current_inventory$destination,
    exists = current_inventory$exists,
    status = current_inventory$status,
    rows = current_inventory$rows,
    first_year = current_inventory$first_year,
    last_year = current_inventory$last_year,
    next_command = ifelse(current_inventory$status %in% c("missing_current_artifact", "stale"), "dina sources explore wid --fetch", ""),
    stringsAsFactors = FALSE
  )
  actions <- wid_include_review_actions(inventory, validation)
  unsupported <- wid_include_unsupported_sources(root, contract)
  overall <- if (any(validation$severity == "blocked", na.rm = TRUE) || any(inventory$status == "read_failed", na.rm = TRUE)) {
    "blocked"
  } else if (isTRUE(fetch_attempted)) {
    "fetched"
  } else {
    "review"
  }
  tables <- list(
    wid_request_plan = request_plan,
    source_inventory = inventory,
    wid_artifact_status = artifact_status,
    validation_report = validation,
    wid_artifact_comparison = comparison,
    wid_numeric_comparison = numeric,
    review_actions = actions,
    unsupported_sources = unsupported,
    promotion_plan = promotion_plan,
    source_fingerprints = source_fingerprints,
    promotion_fingerprints = promotion_fingerprints,
    fetch_publish_report = publish_report,
    explore_manifest = wid_include_manifest("explore", overall, contract, dry_run = dry_run)
  )
  if (isTRUE(write_outputs)) {
    wid_include_write_csvs(tables, paths, "explore_manifest")
  }
  list(paths = paths, outputs = tables, manifest = tables$explore_manifest, contract = contract, status = overall)
}
