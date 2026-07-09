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

wid_include_review_actions <- function(status) {
  if (!nrow(status)) return(data.frame(stringsAsFactors = FALSE))
  rows <- lapply(unique(status$source_id), function(source_id) {
    part <- status[status$source_id == source_id, , drop = FALSE]
    needs <- part$status %in% c("missing_current_artifact", "stale", "read_failed")
    data.frame(
      source_id = source_id,
      action = if (any(needs)) "review_include" else "no_action",
      severity = "info",
      next_command = if (any(needs)) "dina sources include wid --dry-run" else "",
      detail = if (any(needs)) "WID artifact is missing, unreadable, or older than WID workflow inputs." else "WID artifact is present.",
      stringsAsFactors = FALSE
    )
  })
  wid_include_bind(rows)
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
  dry_run = FALSE
) {
  contract <- wid_include_read_contract(root, contract_path)
  paths <- wid_include_explore_paths(root, contract, output_dir)
  artifacts <- wid_include_artifacts(contract)
  request_plan <- wid_include_request_plan(root, contract)
  inventory <- wid_include_bind(lapply(artifacts, function(artifact) wid_include_current_status(root, contract, artifact)))
  artifact_status <- data.frame(
    source_id = inventory$source_id,
    artifact = inventory$destination,
    exists = inventory$exists,
    status = inventory$status,
    rows = inventory$rows,
    first_year = inventory$first_year,
    last_year = inventory$last_year,
    next_command = ifelse(inventory$status %in% c("missing_current_artifact", "stale", "read_failed"), "dina sources include wid --dry-run", ""),
    stringsAsFactors = FALSE
  )
  actions <- wid_include_review_actions(inventory)
  unsupported <- wid_include_unsupported_sources(root, contract)
  overall <- if (any(inventory$status == "read_failed", na.rm = TRUE)) "blocked" else "review"
  tables <- list(
    wid_request_plan = request_plan,
    source_inventory = inventory,
    wid_artifact_status = artifact_status,
    validation_report = data.frame(stringsAsFactors = FALSE),
    wid_artifact_comparison = data.frame(stringsAsFactors = FALSE),
    wid_numeric_comparison = data.frame(stringsAsFactors = FALSE),
    review_actions = actions,
    unsupported_sources = unsupported,
    explore_manifest = wid_include_manifest("explore", overall, contract, dry_run = dry_run)
  )
  if (isTRUE(write_outputs)) {
    wid_include_write_csvs(tables, paths, "explore_manifest")
  }
  list(paths = paths, outputs = tables, manifest = tables$explore_manifest, contract = contract, status = overall)
}
