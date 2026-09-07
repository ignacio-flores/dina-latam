dina_results_graph_names <- function() c(t10 = "Top 10%", t1 = "Top 1%", t01 = "Top 0.1%", t001 = "Top 0.01%", m40 = "Middle 40%", b50 = "Bottom 50%")

dina_results_settings <- function(config) list(countries = unlist(config$countries), years = config$years,
  export_validation = config$export_validation)

dina_results_metadata_path <- function(root, date, pending = FALSE) {
  file.path(root, "output", "figures", "updates", paste0("comparison-", date, if (pending) ".pending.json" else ".json"))
}

dina_results_publish_metadata <- function(metadata, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temp <- tempfile("comparison-", tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  dina_write_json(metadata, temp)
  if (!file.rename(temp, path)) stop("Could not publish comparison metadata.", call. = FALSE)
}

dina_results_begin <- function(root, date, config) {
  stopifnot(length(date) == 1L, grepl("^[A-Za-z0-9]+$", date))
  path <- dina_results_metadata_path(root, date)
  pending <- dina_results_metadata_path(root, date, TRUE)
  unlink(c(path, pending))
  baseline <- config$export_validation$previous_update_file
  problems <- dina_baseline_check(baseline, root)
  if (length(problems)) stop(paste(problems, collapse = "\n"), call. = FALSE)
  full <- if (grepl("^/", baseline)) baseline else file.path(root, baseline)
  metadata <- list(version = 1L, status = "preparing", date = date, started_at = dina_now(),
    baseline = baseline, baseline_hash = dina_hash_file(full), settings = dina_results_settings(config))
  dina_results_publish_metadata(metadata, pending)
  invisible(metadata)
}

dina_results_complete <- function(root, date, config) {
  pending <- dina_results_metadata_path(root, date, TRUE)
  metadata <- dina_read_json(pending, default = NULL)
  if (is.null(metadata)) stop("Comparison generation metadata was not prepared.", call. = FALSE)
  baseline <- metadata$baseline
  full <- if (grepl("^/", baseline)) baseline else file.path(root, baseline)
  same <- function(x, y) identical(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), jsonlite::toJSON(y, auto_unbox = TRUE, null = "null"))
  if (!file.exists(full) || !identical(dina_hash_file(full), metadata$baseline_hash) || !same(metadata$settings, dina_results_settings(config))) stop("Comparison baseline or settings changed during export.", call. = FALSE)
  graphs <- file.path("output", "figures", "updates", paste0("update-", date, "-sptinc992j-", names(dina_results_graph_names()), ".pdf"))
  series <- file.path("output", "latest_wid_series", paste0("dina_latam", c("_wide", "", ""), "_", date, c("", "", "_amory"), ".dta"))
  paths <- c(graphs, series)
  if (any(!file.exists(file.path(root, paths)))) stop("Comparison export is incomplete: expected graphs or final series are missing.", call. = FALSE)
  metadata$status <- "complete"
  metadata$generated_at <- dina_now()
  metadata$graphs <- graphs
  metadata$series <- series
  metadata$artifacts <- setNames(lapply(paths, function(path) dina_hash_file(file.path(root, path))), paths)
  dina_results_publish_metadata(metadata, dina_results_metadata_path(root, date))
  unlink(pending)
  invisible(metadata)
}

dina_results_inventory <- function(root) {
  folder <- file.path(root, "output", "figures", "updates")
  paths <- list.files(folder, pattern = "^update-.*-sptinc992j-(t10|t1|t01|t001|m40|b50)\\.pdf$", full.names = TRUE)
  paths <- paths[order(file.info(paths)$mtime, decreasing = TRUE)]
  cfg <- tryCatch(dina_session_config(dina_load_session(root = root), root, expand_env = FALSE), error = function(e) NULL)
  metadata_files <- list.files(folder, pattern = "^comparison-[A-Za-z0-9]+\\.json$", full.names = TRUE)
  metadata <- lapply(metadata_files, function(path) tryCatch({ value <- dina_read_json(path, default = NULL); if (is.list(value)) value else NULL }, error = function(e) NULL))
  states <- lapply(metadata, function(meta) {
    if (is.null(meta) || !identical(meta$status, "complete")) return("Generation incomplete")
    if (!is.character(meta$baseline) || length(meta$baseline) != 1L || !length(meta$artifacts) || !length(meta$graphs) || !length(meta$series) || !all(unlist(c(meta$graphs, meta$series)) %in% names(meta$artifacts))) return("Generation metadata incomplete")
    baseline <- if (grepl("^/", meta$baseline)) meta$baseline else file.path(root, meta$baseline)
    if (!file.exists(baseline) || !identical(dina_hash_file(baseline), meta$baseline_hash)) return("Baseline changed or missing; regenerate export")
    equal <- function(x, y) identical(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), jsonlite::toJSON(y, auto_unbox = TRUE, null = "null"))
    if (is.null(cfg) || !equal(meta$settings, dina_results_settings(cfg))) return("Settings changed; regenerate export")
    if (is.null(meta$artifacts) || any(!vapply(names(meta$artifacts), function(path) {
      full <- file.path(root, path)
      file.exists(full) && identical(dina_hash_file(full), meta$artifacts[[path]])
    }, logical(1)))) return("Generated artifacts changed or missing")
    "Matches recorded baseline and settings"
  })
  graphs <- dina_review_bind(lapply(paths, function(path) {
    rel <- dina_relative(path, root)
    group <- sub("^.*-([^-]+)\\.pdf$", "\\1", basename(path))
    hit <- which(vapply(metadata, function(meta) rel %in% unlist(meta$graphs), logical(1)))
    meta <- if (length(hit)) metadata[[hit[1]]] else NULL
    data.frame(graph = group, title = dina_results_graph_names()[[group]], path = rel,
      baseline = meta$baseline %||% "Generation baseline unknown",
      generated = meta$generated_at %||% "Unknown",
      status = if (length(hit)) states[[hit[1]]] else "Generation baseline unknown", stringsAsFactors = FALSE)
  }))
  if (!nrow(graphs)) graphs <- data.frame(graph = character(), title = character(), path = character(), baseline = character(), generated = character(), status = character())
  series <- list.files(file.path(root, "output", "latest_wid_series"), pattern = "\\.dta$", full.names = TRUE)
  list(graphs = graphs, series = vapply(series, dina_relative, character(1), root = root))
}

dina_results_show <- function(root) {
  inventory <- dina_results_inventory(root)
  dina_cli_header("Results — Final WID series")
  cfg <- tryCatch(dina_session_config(dina_load_session(root = root), root, expand_env = FALSE), error = function(e) list())
  dina_cli_cat(paste("Baseline for the next export:", cfg$export_validation$previous_update_file %||% "Not selected"))
  if (!nrow(inventory$graphs)) dina_cli_cat("No comparison graphs available.") else dina_review_show_rows(inventory$graphs, c("graph", "title", "baseline", "generated", "status", "path"), limit = nrow(inventory$graphs))
  dina_cli_cat("Final-series files:")
  if (length(inventory$series)) for (path in inventory$series) dina_cli_cat(paste(" ", path)) else dina_cli_cat("None available.")
  dina_cli_cat("Open: dina results open GRAPH\nRegenerate through the existing export: dina run 07d")
  invisible(inventory)
}

dina_results_open <- function(root, graph, viewer = NULL) {
  rows <- dina_results_inventory(root)$graphs
  selected <- rows[rows$path == graph | rows$graph == graph, , drop = FALSE]
  if (!nrow(selected)) stop("No matching comparison graph. Run dina results show.", call. = FALSE)
  if (nrow(selected) > 1L) stop("Several versions exist. Select the full graph path shown by dina results show.", call. = FALSE)
  path <- file.path(root, selected$path[[1]])
  if (!is.null(viewer)) return(invisible(viewer(path)))
  if (.Platform$OS.type == "windows") shell.exec(path) else {
    command <- if (Sys.info()[["sysname"]] == "Darwin") "open" else "xdg-open"
    dina_need("processx")
    result <- processx::run(command, path, error_on_status = FALSE)
    if (result$status != 0L) stop("Could not open the graph: ", result$stderr, call. = FALSE)
  }
  invisible(path)
}

dina_results_menu <- function(root, input = "stdin", is_terminal = isatty(stdin())) {
  repeat {
    inventory <- dina_results_show(root)
    if (!is_terminal || !nrow(inventory$graphs)) return(invisible(inventory))
    rows <- inventory$graphs
    selected <- dina_menu_select("Open a comparison graph", lapply(seq_len(nrow(rows)), function(i) dina_menu_action(rows$path[i],
      paste(rows$title[i], "—", basename(rows$path[i])), command = paste("dina results open", shQuote(rows$path[i])))), input = input, is_terminal = is_terminal)
    if (is.null(selected) || selected == "quit") return(invisible(inventory))
    dina_results_open(root, selected)
  }
}

dina_cmd_results <- function(root, args) {
  sub <- dina_arg(args, 1L, "show")
  if (!length(args) && isatty(stdin())) return(dina_results_menu(root))
  if (sub == "show") return(dina_results_show(root))
  if (sub == "open" && length(args) == 2L) return(dina_results_open(root, args[[2]]))
  stop("Usage: dina results [show]\n       dina results open GRAPH", call. = FALSE)
}
