# Publication candidates are separate from accepted source preparation and 07c.
# Only the small accepted-source contributions are tracked here.
dina_finished_publication_pointer <- function(root) {
  file.path(root, "output", "experiments", "source_reviews", "dina_publication.json")
}

dina_finished_publication_state <- function(root) {
  dina_read_json(dina_finished_publication_pointer(root),
    default = list(family = "dina_publication", active = list()))
}

dina_finished_publication_save <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dina_finished_include_write_journal(value, path)
  invisible(value)
}

dina_finished_publication_config <- function(root, config = NULL) {
  if (is.null(config)) config <- dina_session_config(dina_load_session(root = root), root)
  list(first_year = as.integer(config$years$first),
    last_internal_year = as.integer(config$years$last),
    last_export_year = as.integer(config$export_validation$last_year),
    unit = as.character(config$export_validation$unit),
    steps = sort(as.character(unlist(config$export_validation$steps))))
}

dina_finished_publication_receipts <- function(root) {
  # A new exploration may replace dina.json. Successful inclusion journals are
  # the durable acceptance evidence; neither the inbox nor its mtime is needed.
  base <- file.path(root, "output", "experiments", "finished_dina_include", "confirms")
  paths <- list.files(base, pattern = "^confirmation\\.json$", full.names = TRUE, recursive = TRUE)
  paths <- paths[order(file.info(paths)$mtime, decreasing = TRUE)]
  receipts <- list()
  for (path in paths) {
    record <- dina_read_json(path, default = NULL)
    if (identical(record$family, "dina") && identical(record$status, "confirmed")) {
      receipts[[length(receipts) + 1L]] <- record
    }
  }
  receipts
}

dina_finished_publication_sources <- function(root, ids = NULL) {
  root <- normalizePath(root, mustWork = TRUE)
  sources <- dina_source_registry(root, family = "dina")
  if (!is.null(ids)) {
    ids <- unique(trimws(unlist(strsplit(paste(ids, collapse = ","), ",", fixed = TRUE))))
    unknown <- setdiff(ids, vapply(sources, function(s) s$id, character(1)))
    if (length(unknown)) stop("Unknown finished DINA source: ", paste(unknown, collapse = ", "), call. = FALSE)
    sources <- Filter(function(s) s$id %in% ids, sources)
  }
  receipts <- dina_finished_publication_receipts(root)
  result <- list()
  for (source in sources) {
    meta <- source$finished_series
    if (is.null(meta$publication_step) || is.null(meta$publication_unit)) next
    status <- meta$publication_policy$status %||% "active"
    if (length(status) != 1L || !status %in% c("active", "hold")) {
      stop("Invalid finished-series publication status for ", source$id,
        "; use active or hold.", call. = FALSE)
    }
    if (identical(status, "hold")) {
      if (!is.null(ids)) stop("Finished-series publication is on hold: ", source$id,
        ". ", meta$publication_policy$hold_reason %||% "Scientific review pending.", call. = FALSE)
      next
    }
    source$prepared <- dina_finished_include_path(root, source$prepared,
      "input_data/dina")
    receipt <- artifact <- NULL
    for (item in receipts) {
      matches <- Filter(function(a) identical(a$source_id, source$id) &&
        identical(a$role, "prepared") && identical(normalizePath(a$destination, mustWork = FALSE), source$prepared), item$artifacts)
      if (length(matches)) { receipt <- item; artifact <- matches[[1L]]; break }
    }
    if (is.null(artifact)) {
      if (!is.null(ids)) stop("Source has no successful prepared-input acceptance: ", source$id, call. = FALSE)
      next
    }
    if (!identical(dina_hash_file(source$prepared), artifact$sha256)) {
      stop("Accepted prepared source is missing or changed: ", source$prepared, call. = FALSE)
    }
    if (identical(meta$format, "wid_grouped_xlsx") &&
        (!identical(meta$publication_policy$continuation, "four_group_relative_changes") ||
         !isTRUE(meta$publication_policy$carry_forward) || !isTRUE(meta$publication_policy$include_history) ||
         !identical(meta$publication_policy$observation_years, "accepted_prepared") ||
         !identical(meta$publication_policy$gini_kind, "gpinter_fitted"))) {
      stop("Included grouped source has an unsupported integration policy: ", source$id, call. = FALSE)
    }
    if (length(source$country) != 1L || !grepl("^[A-Z]{3}$", source$country)) {
      stop("A publication source must register one ISO3 country: ", source$id, call. = FALSE)
    }
    if (identical(meta$publication_policy$observation_years, "accepted_prepared")) {
      accepted <- as.data.frame(haven::read_dta(source$prepared))
      required <- c("country", "year", "step", "unit", "requires_harmonization", "interpolate_missing_years", "extrapolate_years")
      if (!all(required %in% names(accepted))) stop("Accepted prepared source lacks year-selection evidence: ", source$id)
      accepted <- accepted[accepted$country == source$country & accepted$step == meta$publication_step & accepted$unit == meta$publication_unit, , drop = FALSE]
      if (!nrow(accepted) || anyNA(accepted[required]) || any(accepted$requires_harmonization != 0) ||
          any(accepted$interpolate_missing_years != 0 | accepted$extrapolate_years != 0) ||
          any(accepted$year < meta$harmonization$first_full_year)) stop("Accepted grouped source violates observed-year preparation policy: ", source$id)
      source$publication_years <- sort(unique(accepted$year))
    }
    source$acceptance <- list(confirmation = receipt$run, sha256 = artifact$sha256)
    result[[length(result) + 1L]] <- source
  }
  if (!length(result)) stop("No accepted finished DINA publication sources are available.", call. = FALSE)
  keys <- vapply(result, function(s) paste(s$country, s$finished_series$publication_step,
    s$finished_series$publication_unit, sep = "/"), character(1))
  if (anyDuplicated(keys)) stop("Overlapping publication sources for ",
    paste(unique(keys[duplicated(keys)]), collapse = ", "), ". Resolve the registry overlap first.", call. = FALSE)
  result
}

dina_finished_publication_engine <- function(root) {
  e <- new.env(parent = environment(dina_finished_publication_engine))
  source(file.path(root, "code", "R", "source-diagnostics", "finished_dina_publication.R"), local = e)
  e
}

dina_finished_publication_inputs <- function(root, sources, config = NULL) {
  root <- normalizePath(root, mustWork = TRUE)
  dina_need("haven"); dina_need("digest")
  if (is.null(config)) config <- dina_session_config(dina_load_session(root = root), root)
  universe <- as.character(unlist(config$countries, use.names = FALSE))
  outside <- setdiff(vapply(sources, function(source) source$country, character(1)), universe)
  if (length(outside)) stop("Finished publication source countries are outside the configured country universe: ",
    paste(outside, collapse = ", "), ". Update the effective configuration before preparing or activating them.", call. = FALSE)
  # Validate membership without fingerprinting the whole universe: adding a
  # different configured country cannot invalidate an approved contribution.
  settings <- dina_finished_publication_config(root, config)
  path <- file.path(root, "output", "ineqstats", "_gpinter_all_topcorrected.csv")
  if (!file.exists(path)) stop("Internal distributions are missing. Run 07c first: ", path, call. = FALSE)
  internal <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("country", "year", "step", "unit", "p", "avg", "thr", "bckt_sh", "topavg", "topsh")
  if (!all(required %in% names(internal))) stop("Internal GPinter file lacks required columns.", call. = FALSE)
  wid_path <- file.path(root, "input_data", "wid", "export_scaling_anninc_xrates.dta")
  if (!file.exists(wid_path)) stop("Current WID export scaling inputs are missing: ", wid_path, call. = FALSE)
  wid <- as.data.frame(haven::read_dta(wid_path))
  areas <- dina_read_yaml(file.path(root, "config", "wid_include.yml"))$area_metadata
  rows <- list(); prepared <- list(); wid_rows <- list(); source_identities <- list()
  for (source in sources) {
    meta <- source$finished_series
    data <- as.data.frame(haven::read_dta(source$prepared))
    data <- data[data$country == source$country & data$step == meta$publication_step &
      data$unit == meta$publication_unit, , drop = FALSE]
    if (!nrow(data)) stop("Accepted source has no registered publication group: ", source$id, call. = FALSE)
    data <- data[order(data$year, data$p), , drop = FALSE]; rownames(data) <- NULL
    if (!identical(meta$publication_unit, settings$unit) || !meta$publication_step %in% settings$steps) {
      stop("Source publication group is outside the current export unit/concepts: ", source$id, call. = FALSE)
    }
    part <- internal[internal$country == source$country & internal$step == meta$publication_step &
      internal$unit == meta$publication_unit & internal$year <= settings$last_internal_year, required, drop = FALSE]
    part <- part[order(part$year, part$p), , drop = FALSE]; rownames(part) <- NULL
    part$average <- NA_real_
    for (year in unique(part$year)) {
      zero <- part$year == year & abs(part$p) < 1e-10
      if (sum(zero) == 1L) part$average[part$year == year] <- part$topavg[zero]
    }
    area <- Filter(function(a) identical(a$iso3, source$country), areas)
    if (length(area) != 1L) stop("Missing unique WID country mapping for ", source$country, call. = FALSE)
    scale <- wid[wid$iso == area[[1L]]$wid_area & wid$year >= min(data$year) &
      wid$year <= settings$last_export_year, c("year", "anninc992i"), drop = FALSE]
    scale$country <- source$country
    scale <- scale[order(scale$year), , drop = FALSE]; rownames(scale) <- NULL
    prepared[[source$id]] <- data; rows[[source$id]] <- part; wid_rows[[source$id]] <- scale
    # Only the selected source and its consumed rows/settings matter. Adding a
    # different registered source or recomputing another country is harmless.
    source_identities[[source$id]] <- list(prepared = source$prepared,
      sha256 = source$acceptance$sha256,
      specification = source[c("id", "country", "finished_series")],
      internal = digest::digest(part, algo = "sha256"),
      scaling = digest::digest(scale, algo = "sha256"))
  }
  code_paths <- c(file.path(root, "code", "R", "source-diagnostics", "finished_dina_publication.R"),
    file.path(root, "code", "R", "cli", "finished_dina_publication.R"))
  list(prepared = prepared, internal = rows, wid = wid_rows, settings = settings,
    identity = list(sources = source_identities, settings = settings,
      code = dina_review_hashes(code_paths)))
}

dina_finished_publication_candidate <- function(root, run = NULL) {
  if (is.null(run)) run <- dina_finished_publication_state(root)$latest_candidate
  if (is.null(run)) stop("Prepare a publication candidate: dina sources include dina --publication --dry-run", call. = FALSE)
  run <- dina_finished_include_path(root, run, "output/experiments/finished_dina_publication")
  candidate <- dina_read_json(file.path(run, "candidate.json"), default = NULL)
  if (!identical(candidate$family, "dina_publication") || !identical(candidate$run, run)) {
    stop("Not a saved finished DINA publication candidate: ", run, call. = FALSE)
  }
  candidate
}

dina_finished_publication_verify <- function(root, candidate, config = NULL, ids = NULL) {
  if (!identical(candidate$status, "ready")) stop("Publication candidate has unresolved failures.", call. = FALSE)
  if (is.null(ids)) ids <- names(candidate$identity$sources)
  sources <- dina_finished_publication_sources(root, ids)
  current <- dina_finished_publication_inputs(root, sources, config)$identity
  expected <- candidate$identity
  expected$sources <- expected$sources[ids]
  # JSON round trips may simplify numeric scalars; canonical JSON compares the
  # values actually recorded instead of R's incidental integer storage type.
  same <- function(a, b) identical(jsonlite::toJSON(a, auto_unbox = TRUE, null = "null", digits = NA),
    jsonlite::toJSON(b, auto_unbox = TRUE, null = "null", digits = NA))
  if (!same(current, expected)) stop("Accepted source, relevant internal data, WID scaling, publication settings or continuation code changed. Prepare a new publication dry run.", call. = FALSE)
  if (!identical(dina_review_hashes(names(candidate$evidence)), candidate$evidence)) {
    stop("Publication candidate artifacts or review tables changed. Prepare a new dry run.", call. = FALSE)
  }
  invisible(TRUE)
}

# This reader is only for the comparison figure. It does not change the
# prepared scientific input, continuation identity, or accepted contributions.
dina_finished_publication_original <- function(root, source) {
  empty <- function(reason) {
    rows <- data.frame()
    attr(rows, "unavailable_reason") <- reason
    rows
  }
  if (is.null(source$canonical) || !nzchar(source$canonical)) {
    return(empty("Original supplied file is not registered."))
  }
  path <- dina_finished_include_path(root, source$canonical, "input_data/dina")
  if (!file.exists(path)) return(empty(paste("Original supplied file is unavailable:", path)))
  receipts <- dina_finished_publication_receipts(root)
  if (!is.null(source$acceptance$confirmation)) receipts <- Filter(function(item)
    identical(item$run, source$acceptance$confirmation), receipts)
  artifact <- NULL
  for (receipt in receipts) {
    matches <- Filter(function(item) identical(item$source_id, source$id) &&
      identical(item$role, "original") && identical(normalizePath(item$destination, mustWork = FALSE), path),
      receipt$artifacts)
    if (length(matches)) { artifact <- matches[[1L]]; break }
  }
  if (is.null(artifact)) return(empty("Original supplied file has no matching acceptance receipt."))
  if (!identical(dina_hash_file(path), artifact$sha256)) {
    return(empty("Original supplied file differs from its accepted checksum; comparison unavailable."))
  }
  meta <- source$finished_series
  if (identical(meta$format, "wid_grouped_xlsx")) {
    engine <- dina_finished_include_engine(root)
    rows <- engine$finished_dina_grouped_read(path, source)
    rows <- rows[rows$measure == "share" & rows$year %in% source$publication_years, , drop = FALSE]
    # Plot supplied bracket shares directly, especially bottom 50: an
    # inconsistent supplied total must not turn it into 1 - top50.
    return(rows[c("year", "percentile", "value")])
  }
  rows <- as.data.frame(haven::read_dta(path))
  if (!all(c("country", "step", "unit", "year", "topsh") %in% names(rows))) {
    return(empty("Original supplied file lacks fields required for labelled-group comparisons."))
  }
  meta <- source$finished_series
  keep <- !is.na(rows$country) & !is.na(rows$step) & !is.na(rows$unit) &
    rows$country == source$country & rows$step == meta$publication_step &
    rows$unit == meta$publication_unit
  rows <- rows[keep, , drop = FALSE]
  if (!nrow(rows)) return(empty("Original supplied file has no registered publication group."))
  rows
}

dina_finished_publication_plot_stat <- function(data, cutoff, original = FALSE) {
  if (is.null(data) || !nrow(data)) return(data.frame(year = numeric(), value = numeric()))
  years <- sort(unique(data$year))
  data.frame(year = years, value = vapply(years, function(year) {
    rows <- data[data$year == year, , drop = FALSE]
    if (original) {
      if (all(c("percentile", "value") %in% names(rows))) {
        label <- if (cutoff == .5) "p0p50" else paste0("p", format(100 * cutoff, trim = TRUE, scientific = FALSE), "p100")
        hit <- which(rows$percentile == label)
        return(if (length(hit) == 1L && is.finite(rows$value[hit])) rows$value[hit] else NA_real_)
      }
      # Raw percentile labels need not imply exact population widths. Display
      # its supplied cumulative shares without inventing width*mean shares.
      boundaries <- if ("p" %in% names(rows)) rows$p else rep(NA_real_, nrow(rows))
      if ("percentile" %in% names(rows)) {
        labels <- as.character(rows$percentile)
        valid <- !is.na(labels) & grepl("^p[0-9]+([.][0-9]+)?p[0-9]+([.][0-9]+)?$", labels)
        boundaries[valid] <- as.numeric(sub("^p([0-9.]+)p.*$", "\\1", labels[valid])) / 100
      }
      hit <- which(is.finite(boundaries) & abs(boundaries - cutoff) <= 6e-8)
      if (length(hit) != 1L || !is.finite(rows$topsh[hit])) return(NA_real_)
      return(if (cutoff == .5) 1 - rows$topsh[hit] else rows$topsh[hit])
    }
    rows <- rows[order(rows$p), , drop = FALSE]
    mass <- diff(c(rows$p, 1)); income <- mass * rows$avg
    if (cutoff == .5) sum(income[rows$p < cutoff - 1e-10]) / sum(income) else
      sum(income[rows$p >= cutoff - 1e-10]) / sum(income)
  }, numeric(1)))
}

dina_finished_publication_plot_data <- function(source, internal, proposed, raw, original = NULL) {
  cutoffs <- c(.5, .9, .99, .999)
  labels <- c("Bottom 50%", "Top 10%", "Top 1%", "Top 0.1%")
  result <- lapply(seq_along(cutoffs), function(i) {
    curves <- c(list(dina_finished_publication_plot_stat(original, cutoffs[i], original = TRUE)),
      lapply(list(source, internal, proposed, raw), dina_finished_publication_plot_stat, cutoff = cutoffs[i]))
    comparison <- merge(curves[[1L]], curves[[2L]], by = "year", all = TRUE,
      suffixes = c("_original", "_prepared"))
    comparison$difference_pp <- 100 * (comparison$value_prepared - comparison$value_original)
    comparison$group <- labels[i]
    list(group = labels[i], cutoff = cutoffs[i], series = curves, difference = comparison)
  })
  names(result) <- labels
  result
}

# The original source and the prepared source are deliberately separate curves.
# Monetary scaling is reported separately and applied once by the export.
dina_finished_publication_plot <- function(path, source, internal, proposed, raw, anchor, original = NULL) {
  panels <- dina_finished_publication_plot_data(source, internal, proposed, raw, original)
  original_available <- !is.null(original) && nrow(original) > 0L
  unavailable <- attr(original, "unavailable_reason") %||% "Original supplied file was not provided."
  labels <- c("Original supplied file (labelled groups)", "Prepared source (exact groups)",
    "Internal", "Four-group continuation + join repair", "Continuation before join repair")
  if (!original_available) labels[1L] <- "Original supplied file (unavailable)"
  colors <- c("black", "#009E73", "#737373", "#0072B2", "#D55E00")
  types <- c(2, 1, 1, 1, 3)
  widths <- c(1.6, 1.7, 1.2, 1.7, 1.2)
  grDevices::pdf(path, width = 9, height = 7, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::layout(rbind(c(1, 1), c(2, 3), c(4, 5)), heights = c(.75, 3, 3))
  graphics::par(mar = c(0, 0, 0, 0), oma = c(3.4, 0, 2, 0))
  graphics::plot.new()
  graphics::legend("center", labels, col = colors, lty = types, lwd = widths,
    bty = "n", cex = .72, ncol = 2)
  graphics::par(mar = c(3.7, 4, 2.5, 1))
  for (panel in panels) {
    series <- panel$series
    xx <- range(unlist(lapply(series, function(x) x$year)), finite = TRUE)
    yy <- range(unlist(lapply(series, function(x) x$value)) * 100, finite = TRUE)
    graphics::plot(NA, xlim = xx, ylim = yy, xlab = "Year", ylab = "Income share (%)", main = panel$group)
    graphics::abline(v = anchor, col = "grey55", lty = 3)
    for (j in c(3, 5, 4, 2, 1)) {
      x <- series[[j]]
      if (nrow(x)) {
        years <- seq.int(min(x$year), max(x$year))
        graphics::lines(years, x$value[match(years, x$year)] * 100,
          col = colors[j], lty = types[j], lwd = widths[j])
      }
    }
  }
  title <- paste(source$country[1], source$step[1], source$unit[1])
  graphics::mtext(paste(title, "- source preparation and four-group continuation"),
    outer = TRUE, side = 3, line = .5)
  graphics::mtext(paste("Groups: p0-50 | p50-90 | p90-99 | p99-100. Vertical line:", anchor, "join."),
    outer = TRUE, side = 1, line = 1, cex = .75, col = "grey40")
  grouped_original <- original_available && "value" %in% names(original)
  graphics::mtext(if (grouped_original) "Grouped source shares are normalized proportionally; missing years stay blank. Details are on page 2."
    else if (original_available) "Labelled source boundaries may differ from exact population boundaries; preparation differences are on page 2." else unavailable,
    outer = TRUE, side = 1, line = 2.1, cex = .72, col = "grey40")

  graphics::layout(matrix(1:4, nrow = 2, byrow = TRUE))
  graphics::par(mar = c(3.7, 4.5, 2.5, 1), oma = c(3.8, 0, 2.2, 0))
  for (panel in panels) {
    rows <- panel$difference
    xx <- range(source$year, finite = TRUE)
    finite <- is.finite(rows$difference_pp)
    yy <- if (any(finite)) range(c(0, rows$difference_pp[finite])) else c(-1, 1)
    if (diff(yy) == 0) yy <- yy + c(-1, 1) * .001
    graphics::plot(NA, xlim = xx, ylim = yy, xlab = "Year",
      ylab = "Prepared minus original (pp)", main = panel$group)
    graphics::abline(h = 0, col = "grey65", lty = 3)
    if (any(finite)) {
      years <- seq.int(min(rows$year), max(rows$year))
      graphics::lines(years, rows$difference_pp[match(years, rows$year)], type = "o", pch = 16,
        cex = .45, lwd = 1.4, col = "#009E73")
    } else graphics::text(mean(xx), mean(yy),
        "Original supplied comparison unavailable", cex = .75, col = "grey40")
  }
  graphics::mtext(paste(title, "- preparation differences"), outer = TRUE, side = 3, line = .6)
  graphics::mtext(if (grouped_original) "Grouped shares: differences reflect proportional normalization; GPinter preserves normalized group shares."
    else "Prepared source at exact population groups minus cumulative shares at the original labelled boundaries.",
    outer = TRUE, side = 1, line = 1, cex = .75, col = "grey40")
  graphics::mtext(if (grouped_original) "Missing source years remain gaps; no temporal interpolation is performed."
    else "Differences include recovered population weights and regridding, not just income corrections. Missing boundaries are not interpolated.",
    outer = TRUE, side = 1, line = 2.1, cex = .72, col = "grey40")
  invisible(panels)
}

dina_finished_publication_figure_paths <- function(root, candidate) {
  lapply(candidate$sources[names(candidate$figures)], function(source) {
    name <- paste("finished-dina", source$country, source$step, source$unit, "candidate.pdf", sep = "-")
    file.path(root, "output", "figures", "updates", gsub("[^A-Za-z0-9_.-]", "_", name))
  })
}

dina_finished_publication_copy_figures <- function(root, candidate) {
  # Keep the reviewed originals immutable. These convenience copies show the
  # latest candidate beside the regular update graphs, without activating it.
  paths <- dina_finished_publication_figure_paths(root, candidate)
  for (id in names(paths)) {
    dir.create(dirname(paths[[id]]), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(candidate$figures[[id]], paths[[id]], overwrite = TRUE)) {
      warning("Could not copy the comparison to ", paths[[id]], call. = FALSE)
    }
  }
  invisible(paths)
}

dina_finished_publication_prepare <- function(root, flags = list(), engine = NULL, config = NULL) {
  root <- normalizePath(root, mustWork = TRUE)
  if (is.null(engine)) engine <- dina_finished_publication_engine(root)
  adjustment <- flags$adjustment %||% "group_join"
  if (!adjustment %in% c("none", "group_join")) stop("Use --adjustment none or --adjustment group_join. Earlier isotonic candidates used a different continuation method and require a new review.", call. = FALSE)
  sources <- dina_finished_publication_sources(root, flags$source)
  inputs <- dina_finished_publication_inputs(root, sources, config)
  base <- file.path(root, "output", "experiments", "finished_dina_publication")
  run <- file.path(base, "runs", paste0("publication-", format(Sys.time(), "%Y%m%d-%H%M%S"),
    "-", basename(tempfile())))
  for (folder in c("tables", "artifacts", "figures")) dir.create(file.path(run, folder), recursive = TRUE, showWarnings = FALSE)
  candidate <- list(family = "dina_publication", status = "preparing", run = run,
    prepared_at = dina_now(), adjustment = adjustment, identity = inputs$identity,
    sources = list(), artifacts = list(), tables = list(), figures = list(), failures = list())
  tables <- setNames(rep(list(data.frame()), 7L), c("summary", "changes", "impact", "checks", "problems", "scaling", "groups"))
  add <- function(table, rows, id, variant = NULL) {
    if (!is.data.frame(rows) || !nrow(rows)) return(invisible(NULL))
    rows$source_id <- id
    if (!is.null(variant)) rows$variant <- variant
    tables[[table]] <<- dina_review_bind(list(tables[[table]], rows))
  }
  for (i in seq_along(sources)) {
    source <- sources[[i]]; id <- source$id
    dina_cli_cat("Preparing publication candidate: ", id, " (", adjustment, ").")
    for (field in c("population", "currency", "price_basis", "citation")) {
      value <- paste(source$finished_series[[field]] %||% "", collapse = "; ")
      if (!nzchar(trimws(value)) || grepl("unconfirmed|unknown|not (yet )?(defined|confirmed|specified)", value, ignore.case = TRUE)) {
        detail <- if (field == "population") {
          "Population definition remains unconfirmed. Normalizing income means does not establish compatibility with the internal population or export statistical unit."
        } else if (field == "citation") "Complete the publication citation; the current reference is explicitly unconfirmed."
        else paste0(if (field == "currency") "Currency" else "Price basis",
          " definition remains unconfirmed. Normalizing the distribution and applying WID means does not establish the supplied monetary definition.")
        warning <- data.frame(country = source$country, year = NA_integer_,
          step = source$finished_series$publication_step, unit = source$finished_series$publication_unit,
          check_id = "source_definition", status = "warning", field = field,
          supplied_definition = if (nzchar(value)) value else "not declared", detail = detail)
        add("checks", warning, id, "shared")
        add("problems", warning, id, "shared")
      }
    }
    result <- tryCatch({
      prepared <- inputs$prepared[[id]]; internal <- inputs$internal[[id]]
      specification <- dina_finished_specification(source)
      if (!is.null(source$publication_years)) specification$years <- source$publication_years
      specification$publication_policy <- source$finished_series$publication_policy
      specification$gini_kind <- source$finished_series$publication_policy$gini_kind %||% "supplied"
      specification$source_id <- id
      for (key in c("citation", "doi", "version")) specification[[key]] <- source$finished_series[[key]]
      args <- list(prepared = prepared, internal = internal, specification = specification,
        first_year = inputs$settings$first_year, last_year = inputs$settings$last_internal_year,
        export_last_year = inputs$settings$last_export_year)
      raw <- do.call(engine$finished_dina_publication_build, c(args, list(adjustment = "none")))
      adjusted <- do.call(engine$finished_dina_publication_build, c(args, list(adjustment = "group_join")))
      selected <- if (adjustment == "group_join") adjusted else raw
      for (variant in c("raw", "group_join")) {
        value <- if (variant == "raw") raw else adjusted
        for (table in c("changes", "impact", "checks", "problems", "groups")) add(table, value[[table]], id, variant)
        path <- file.path(run, "artifacts", paste0(i, "-", variant, ".csv"))
        utils::write.csv(value$data, path, row.names = FALSE, na = "")
        candidate$artifacts[[paste0(id, "_", variant)]] <- path
      }
      candidate$sources[[id]] <- list(id = id, country = source$country,
        step = specification$publication$step, unit = specification$publication$unit,
        anchor_year = max(prepared$year), first_year = min(prepared$year),
        observation_years = sort(unique(prepared$year)),
        publication_policy = source$finished_series$publication_policy,
        last_year = inputs$settings$last_export_year, citation = specification$citation,
        doi = specification$doi, version = specification$version, recipe = selected$recipe,
        artifact = candidate$artifacts[[paste0(id, "_", if (adjustment == "none") "raw" else "group_join")]])
      for (year in sort(unique(selected$data$year))) {
        changes <- adjusted$changes[adjusted$changes$year == year, , drop = FALSE]
        raw_checks <- raw$checks[which(raw$checks$year == year), , drop = FALSE]
        selected_checks <- selected$checks[which(selected$checks$year == year), , drop = FALSE]
        changed_means <- changes[changes$rule == "review_group_join_means", , drop = FALSE]
        group_rows <- selected$groups[which(selected$groups$year == year), , drop = FALSE]
        raw_rows <- raw$data[raw$data$year == year, , drop = FALSE]
        raw_rows <- raw_rows[order(raw_rows$p), , drop = FALSE]
        means <- raw_rows$avg; thresholds <- raw_rows$thr
        lower <- c(-Inf, head(means, -1L))
        raw_reversals <- sum(diff(means) < -1e-12 * pmax(1, abs(head(means, -1L)), abs(tail(means, -1L))))
        raw_thresholds <- sum((is.finite(lower) & thresholds < lower - 1e-12 * pmax(1, abs(lower))) |
          thresholds > means + 1e-12 * pmax(1, abs(means)))
        add("summary", data.frame(country = source$country, year = year,
          step = specification$publication$step, unit = specification$publication$unit,
          origin = selected$data$origin[match(year, selected$data$year)],
          raw_mean_reversals = raw_reversals, raw_threshold_violations = raw_thresholds,
          adjusted_means = nrow(changed_means),
          max_mean_change_percent = if (nrow(changed_means)) max(abs(changed_means$relative_percent), na.rm = TRUE) else 0,
          threshold_changes = sum(changes$rule %in% c("review_threshold_repair", "review_threshold_margin")),
          groups_checked = nrow(group_rows),
          group_income_failures = if (nrow(group_rows)) sum(group_rows$status == "fail") else 0L,
          max_group_income_gap = if (nrow(group_rows)) max(abs(group_rows$income_difference)) else 0,
          status = if (any(selected_checks$status == "fail")) "failed" else "review"), id)
      }
      scale <- inputs$wid[[id]]
      years <- sort(unique(selected$data$year))
      for (year in years) {
        supplied_year <- min(year, max(prepared$year))
        source_mean <- unique(prepared$average[prepared$year == supplied_year])
        wid_mean <- scale$anninc992i[scale$year == year]
        if (length(source_mean) != 1L || length(wid_mean) > 1L) stop("Ambiguous source or WID mean for ", id, "/", year)
        available <- length(wid_mean) == 1L && is.finite(wid_mean) && wid_mean > 0
        add("scaling", data.frame(country = source$country, year = year,
          step = specification$publication$step, unit = specification$publication$unit,
          supplied_reference_year = supplied_year, supplied_reference_mean = source_mean,
          wid_mean = if (available) wid_mean else NA_real_,
          ratio_to_supplied_reference = if (available) wid_mean / source_mean else NA_real_,
          status = if (available) "available" else "missing",
          detail = if (available) "Export applies WID mean once; currency/price-basis definitions remain unconfirmed." else
            "No current WID mean: shares remain available; monetary amounts are not invented."), id)
        if (!available) add("problems", data.frame(country = source$country, year = year,
          step = specification$publication$step, unit = specification$publication$unit,
          check_id = "wid_scaling", status = "warning", detail = "Current WID mean is missing; export monetary amounts remain missing."), id, "shared")
        if (!available && isTRUE(source$finished_series$publication_policy$require_wid_scaling)) {
          stop("Missing required WID income benchmark for ", source$country, "/", year,
            "; refresh the extended WID export-scaling input before publication.", call. = FALSE)
        }
      }
      if (isTRUE(adjusted$valid) && nrow(raw$data)) {
        figure <- file.path(run, "figures", paste0(i, "-comparison.pdf"))
        dina_finished_publication_plot(figure, prepared, internal, adjusted$data, raw$data, max(prepared$year),
          original = dina_finished_publication_original(root, source))
        candidate$figures[[id]] <- figure
      }
      if (!isTRUE(selected$valid)) stop("Selected publication alternative failed validation. Inspect continuation-problems.")
      NULL
    }, error = function(e) e)
    if (inherits(result, "error")) {
      candidate$failures[[id]] <- conditionMessage(result)
      add("problems", data.frame(country = source$country, check_id = "preparation", status = "fail",
        detail = conditionMessage(result)), id, "selected")
    }
  }
  if (!identical(inputs$identity, dina_finished_publication_inputs(root, sources, config)$identity)) {
    candidate$failures[["inputs"]] <- "Relevant inputs changed during publication preparation."
  }
  for (table in names(tables)) {
    path <- file.path(run, "tables", paste0(table, ".csv"))
    utils::write.csv(tables[[table]], path, row.names = FALSE, na = "")
    candidate$tables[[table]] <- path
  }
  candidate$status <- if (length(candidate$failures)) "blocked" else "ready"
  candidate$evidence <- dina_review_hashes(c(unlist(candidate$tables), unlist(candidate$artifacts), unlist(candidate$figures)))
  dina_finished_publication_save(candidate, file.path(run, "candidate.json"))
  dina_finished_publication_copy_figures(root, candidate)
  state <- dina_finished_publication_state(root)
  state$latest_candidate <- run
  dina_finished_publication_save(state, dina_finished_publication_pointer(root))
  candidate
}

dina_finished_publication_show <- function(root, table = "continuation", flags = list(), candidate = NULL) {
  if (is.null(candidate)) candidate <- dina_finished_publication_candidate(root, flags$run %||% flags[["include-run"]])
  table <- sub("^continuation-?", "", table)
  if (!nzchar(table)) table <- "summary"
  if (!table %in% names(candidate$tables)) {
    if (table == "groups") stop("This historical candidate predates the four-group continuation. Prepare a new publication dry run.", call. = FALSE)
    stop("Unknown publication table: ", table, call. = FALSE)
  }
  dina_cli_section("Finished DINA publication candidate")
  dina_cli_cat("Status: ", candidate$status, " · selected adjustment: ", candidate$adjustment)
  rows <- dina_read_csv_table_or_empty(candidate$tables[[table]])
  rows <- dina_finished_prepared_filter(rows, flags)
  if (!is.null(flags$source) && "source_id" %in% names(rows)) rows <- rows[rows$source_id %in% strsplit(flags$source, ",", fixed = TRUE)[[1L]], , drop = FALSE]
  if (!is.null(flags$variant) && "variant" %in% names(rows)) rows <- rows[rows$variant == flags$variant, , drop = FALSE]
  if (table == "groups" && is.null(flags$variant) && "variant" %in% names(rows)) {
    selected_variant <- if (candidate$adjustment == "none") "raw" else candidate$adjustment
    rows <- rows[rows$variant == selected_variant, , drop = FALSE]
  }
  if (table == "summary") {
    rows <- rows[rows$origin != "supplied", , drop = FALSE]
    dina_finished_note("Supplied observations are preserved through each source's anchor. The table shows continuation and carry-forward years.")
  }
  limit <- suppressWarnings(as.integer(flags$limit %||% 30L))
  if (is.na(limit) || limit < 1L) limit <- 30L
  if (nrow(rows)) {
    display <- rows
    if (table == "groups" && !isTRUE(flags$exact)) {
      display <- data.frame(Country = rows$country, Year = rows$year, Group = rows$group,
        Factor = rows$growth_factor, `Target %` = 100 * rows$target_share,
        `After %` = 100 * rows$achieved_share, `Gap pp` = 100 * rows$income_difference,
        Check = rows$status, Variant = rows$variant, check.names = FALSE)
      dina_finished_note("Factor = internal group-income growth relative to the anchor. Target/After are income shares; Gap is the adjustment's change in percentage points.")
      dina_finished_note("Use --exact for source and internal group shares, population masses and normalization factors; --variant raw shows the unadjusted alternative.")
    }
    format_cell <- function(name, values) {
      if (!is.numeric(values) || identical(name, "year")) return(as.character(values))
      if (isTRUE(flags$exact)) return(format(values, digits = 15, trim = TRUE))
      vapply(values, function(value) if (is.na(value)) "-" else
        format(signif(value, 4), trim = TRUE, scientific = abs(value) > 0 && abs(value) < 1e-4), character(1))
    }
    dina_print_data_frame_compact(display, limit = limit, format_value = format_cell,
      dim_headers = FALSE, dim_columns = c("detail", "reason", "basis"))
  } else dina_cli_cat("No rows match this view.")
  if (nrow(rows) > limit) dina_finished_note("Showing ", limit, " of ", nrow(rows), " rows; use --limit or country/year filters.")
  problems <- dina_read_csv_table_or_empty(candidate$tables$problems)
  definitions <- if ("check_id" %in% names(problems)) problems[which(problems$check_id == "source_definition"), , drop = FALSE] else data.frame()
  definitions <- dina_finished_prepared_filter(definitions, flags[setdiff(names(flags), "year")])
  if (nrow(definitions)) {
    dina_cli_cat("Unresolved source definitions: ", nrow(definitions), " warning(s).")
    for (i in seq_len(nrow(definitions))) dina_finished_note(definitions$country[i], " / ",
      definitions$field[i], ": ", definitions$detail[i])
  }
  if ("groups" %in% names(candidate$tables)) {
    dina_finished_note("Continuation uses four disjoint groups: p0–50, p50–90, p90–99 and p99–100. Each group's internal normalized-income growth factor applies to all its supplied brackets.")
    dina_finished_note("Join corrections preserve each group's target income. Raw proportional changes and local join repairs are reported separately; neither changes supplied years.")
    dina_finished_note("Group totals and factors: dina sources table dina continuation-groups")
  } else {
    dina_finished_note("Historical candidate using the earlier bracket-level continuation method. Its saved report is retained; prepare a new dry run for the current four-group method.")
  }
  copies <- dina_finished_publication_figure_paths(root, candidate)
  for (id in names(candidate$figures)) {
    path <- candidate$figures[[id]]
    if (file.exists(copies[[id]]) && identical(dina_hash_file(copies[[id]]), candidate$evidence[[path]])) path <- copies[[id]]
    dina_finished_note("Comparison ", id, ": ", path)
  }
  dina_finished_note("Report: ", candidate$run)
  invisible(rows)
}

dina_finished_publication_active <- function(root, config = NULL) {
  state <- dina_finished_publication_state(root)
  active <- state$active %||% list()
  if (!length(active)) return(list(data = data.frame(), attribution = data.frame(), active = list()))
  journal <- dina_read_json(file.path(state$confirmation, "confirmation.json"), default = NULL)
  if (!identical(journal$status, "confirmed")) stop("Finished-series activation did not complete. Restore its recorded confirmation before exporting.", call. = FALSE)
  data <- list()
  runs <- unique(vapply(active, function(a) a$candidate, character(1)))
  for (run in runs) {
    ids <- names(active)[vapply(active, function(a) identical(a$candidate, run), logical(1))]
    candidate <- dina_finished_publication_candidate(root, run)
    dina_finished_publication_verify(root, candidate, config, ids)
    # An optional DOI can be blank for every row. Keep it as text rather than
    # letting read.csv infer a logical NA column that Stata cannot concatenate.
    for (id in ids) data[[id]] <- utils::read.csv(candidate$sources[[id]]$artifact,
      stringsAsFactors = FALSE, colClasses = c(doi = "character"))
  }
  data <- dina_review_bind(data)
  key <- c("country", "year", "step", "unit", "p")
  if (anyDuplicated(data[key])) stop("Active finished publication sources overlap.", call. = FALSE)
  fields <- intersect(c("country", "year", "step", "unit", "source_id", "source_version", "origin",
    "anchor_year", "carried_from", "adjustment", "citation", "doi", "supplied_gini", "fitted_gini", "grouped_gini", "gini_method"), names(data))
  list(data = data, attribution = unique(data[fields]), active = active)
}

# Inclusion selects export sources. This state is only a derived integration cache;
# it never grants permission to use an otherwise accepted source.
dina_finished_publication_readiness <- function(root, config = NULL, internal_pending = FALSE) {
  state <- dina_finished_publication_state(root)
  sources <- tryCatch(dina_finished_publication_sources(root), error = function(e) {
    if (identical(conditionMessage(e), "No accepted finished DINA publication sources are available.")) return(NULL)
    stop(e)
  })
  if (!length(sources)) return(list(status = "inactive", ids = character(),
    message = "Finished DINA integration: no included sources enabled for publication; retain internal distributions."))
  ids <- vapply(sources, function(source) source$id, character(1))
  same <- function(a, b) identical(jsonlite::toJSON(a, auto_unbox = TRUE, null = "null", digits = NA),
    jsonlite::toJSON(b, auto_unbox = TRUE, null = "null", digits = NA))
  if (is.null(config)) config <- dina_session_config(dina_load_session(root = root), root)
  settings <- dina_finished_publication_config(root, config)
  for (source in sources) {
    if (!source$country %in% unlist(config$countries) ||
        !identical(source$finished_series$publication_unit, settings$unit) ||
        !source$finished_series$publication_step %in% settings$steps) {
      stop("Included source is outside the effective export country/concept/unit configuration: ", source$id, call. = FALSE)
    }
  }
  changed <- TRUE
  if (!internal_pending) {
    # Missing required inputs are errors. A stale/missing derived cache simply
    # schedules regeneration, including on the first export after inclusion.
    current <- dina_finished_publication_inputs(root, sources, config)$identity
    changed <- !isTRUE(tryCatch({
      if (!setequal(names(state$active), ids)) stop("Source selection changed")
      journal <- dina_read_json(file.path(state$confirmation, "confirmation.json"), default = NULL)
      saved <- dina_read_json(file.path(state$confirmation, "approved-state.json"), default = NULL)
      if (!identical(journal$status, "confirmed") || !same(state$active, saved$active)) stop("Incomplete cache")
      recipe <- dina_finished_publication_engine(root)$finished_dina_publication_recipe()
      for (id in ids) {
        candidate <- dina_finished_publication_candidate(root, state$active[[id]]$candidate)
        if (!identical(candidate$status, "ready")) stop("Cache validation failed")
        if (!identical(candidate$adjustment, "group_join") || !same(candidate$sources[[id]]$recipe, recipe)) stop("Recipe changed")
        previous <- candidate$identity; previous$sources <- previous$sources[id]
        consumed <- current; consumed$sources <- consumed$sources[id]
        if (!same(consumed, previous)) stop("Inputs changed")
        if (!identical(dina_review_hashes(names(candidate$evidence)), candidate$evidence)) stop("Cache artifacts changed")
      }
      TRUE
    }, error = function(e) FALSE))
  }
  list(status = if (changed) "refresh_pending" else "current", ids = ids, state = state,
    message = if (changed) paste0("Included finished-series sources: ", paste(ids, collapse = ", "),
      ". 07d will prepare and validate their integration automatically",
      if (internal_pending) " after the planned 07c" else " from current internal/WID inputs", ".") else
      paste("Included finished-series integration is current:", paste(ids, collapse = ", ")))
}

dina_finished_publication_ensure <- function(root, config = NULL) {
  readiness <- dina_finished_publication_readiness(root, config)
  dina_cli_cat(readiness$message)
  if (identical(readiness$status, "inactive")) return(list(data = data.frame(), attribution = data.frame(), active = list()))
  if (!identical(readiness$status, "refresh_pending")) return(dina_finished_publication_active(root, config))
  # Reuse a validated preview only when it covers every included source and the
  # configured production method. Inclusion itself is the source selection.
  candidate <- tryCatch({
    value <- dina_finished_publication_candidate(root)
    if (!identical(value$adjustment, "group_join") ||
        !setequal(names(value$sources), readiness$ids)) stop("Different review scope")
    dina_finished_publication_verify(root, value, config)
    value
  }, error = function(e) NULL)
  if (is.null(candidate)) candidate <- dina_finished_publication_prepare(root,
    list(source = paste(readiness$ids, collapse = ","), adjustment = "group_join"), config = config)
  if (!identical(candidate$status, "ready")) {
    stop("Automatic supplied-series integration refresh failed; cached integration and previous exports are unchanged. Inspect ",
      candidate$run, "/tables/problems.csv", call. = FALSE)
  }
  current <- dina_finished_publication_state(root)
  if (!identical(current$active, readiness$state$active) ||
      !identical(current$confirmation, readiness$state$confirmation)) {
    stop("Supplied-series integration cache changed during refresh; retry the export.", call. = FALSE)
  }
  current_ids <- vapply(dina_finished_publication_sources(root), function(source) source$id, character(1))
  if (!setequal(current_ids, readiness$ids)) stop("Included sources changed during refresh; retry the export.", call. = FALSE)
  dina_finished_publication_activate(root, candidate, config = config, replace = TRUE)
  dina_cli_cat("Prepared and validated included-source integration; report: ", candidate$run)
  dina_finished_publication_active(root, config)
}

dina_finished_publication_activate <- function(root, candidate, save = dina_finished_publication_save, config = NULL, replace = FALSE) {
  root <- normalizePath(root, mustWork = TRUE)
  dina_finished_publication_verify(root, candidate, config)
  state <- dina_finished_publication_state(root)
  previous_active <- if (replace) list() else state$active %||% list()
  for (id in names(candidate$sources)) previous_active[[id]] <- list(candidate = candidate$run, source_id = id)
  # Validate retained contributions too; publishing a new source cannot silently
  # carry an invalid previously active contribution into a complete overlay.
  groups <- character()
  for (id in names(previous_active)) {
    item <- previous_active[[id]]
    other <- dina_finished_publication_candidate(root, item$candidate)
    if (!identical(other$run, candidate$run)) dina_finished_publication_verify(root, other, config, ids = id)
    src <- other$sources[[id]]
    groups <- c(groups, paste(src$country, src$step, src$unit, sep = "/"))
  }
  if (anyDuplicated(groups)) stop("Activation would overlap an existing country/concept/unit contribution.", call. = FALSE)
  base <- file.path(root, "output", "experiments", "finished_dina_publication")
  lock <- file.path(base, ".publishing")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Another publication activation is active; inspect ", lock, call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  confirm <- file.path(base, "confirms", paste0("confirm-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-", basename(tempfile())))
  dir.create(file.path(confirm, "backups"), recursive = TRUE)
  pointer <- dina_finished_publication_pointer(root)
  backup <- file.path(confirm, "backups", "dina_publication.json")
  existed <- file.exists(pointer)
  if (existed && !file.copy(pointer, backup)) stop("Could not back up integration cache.", call. = FALSE)
  entry <- list(destination = pointer, existed = existed, backup = backup,
    before_sha256 = if (existed) dina_hash_file(backup) else NULL)
  state$active <- previous_active; state$confirmation <- confirm
  state$latest_candidate <- candidate$run; state$activated_at <- dina_now()
  pending <- file.path(confirm, "approved-state.json")
  dina_write_json(state, pending)
  entry$after_sha256 <- dina_hash_file(pending)
  manifest <- list(family = "dina_publication", status = "publishing", run = confirm,
    candidate = candidate$run, entries = list(entry), pid = Sys.getpid(),
    previous_confirmation = dina_finished_publication_state(root)$confirmation,
    started_at = dina_now())
  dina_write_json(list(pid = Sys.getpid(), confirmation = confirm), file.path(lock, "owner.json"))
  journal <- file.path(confirm, "confirmation.json")
  dina_finished_publication_save(manifest, journal)
  done <- FALSE
  on.exit({
    if (!done) {
      dina_finished_include_rollback(list(entry))
      manifest$status <- "rolled_back"
      dina_finished_publication_save(manifest, journal)
    }
  }, add = TRUE, after = FALSE)
  save(state, pointer)
  manifest$status <- "confirmed"; manifest$completed_at <- dina_now()
  dina_finished_publication_save(manifest, journal)
  done <- TRUE
  invisible(state)
}

dina_finished_publication_restore <- function(root, path) {
  root <- normalizePath(root, mustWork = TRUE)
  path <- dina_finished_include_path(root, path, "output/experiments/finished_dina_publication")
  journal <- file.path(path, "confirmation.json")
  manifest <- dina_read_json(journal, default = NULL)
  if (!identical(manifest$family, "dina_publication") || !identical(manifest$run, path)) stop("Not a publication activation backup.", call. = FALSE)
  if (identical(manifest$status, "restored")) return(invisible(dina_finished_publication_state(root)))
  if (!manifest$status %in% c("confirmed", "publishing")) stop("This activation is not restorable.", call. = FALSE)
  state <- dina_finished_publication_state(root)
  if (identical(manifest$status, "confirmed") && !identical(state$confirmation, path)) stop("Restore the latest publication activation first.", call. = FALSE)
  recovering <- identical(manifest$status, "publishing")
  if (recovering && dina_finished_include_pid_alive(manifest$pid)) stop("The publication process is still running.", call. = FALSE)
  pointer <- dina_finished_publication_pointer(root)
  if (length(manifest$entries) != 1L || !identical(manifest$entries[[1L]]$destination, pointer)) stop("Invalid publication backup destination.", call. = FALSE)
  entry <- manifest$entries[[1L]]
  if (!startsWith(entry$backup, paste0(path, "/backups/"))) stop("Invalid publication backup path.", call. = FALSE)
  expected <- if (recovering) unlist(entry[c("before_sha256", "after_sha256")]) else entry$after_sha256
  if (!dina_hash_file(pointer) %in% expected) {
    # A later dry run legitimately updates latest_candidate; it never changes
    # active approvals. Preserve that new review while restoring active entries.
    approved <- dina_read_json(file.path(path, "approved-state.json"), default = NULL)
    if (recovering || !identical(state$active, approved$active) || !identical(state$confirmation, approved$confirmation)) {
      stop("Integration cache changed outside this update; restore would overwrite it.", call. = FALSE)
    }
  }
  previous <- if (isTRUE(entry$existed)) {
    if (!identical(dina_hash_file(entry$backup), entry$before_sha256)) stop("Publication backup changed.", call. = FALSE)
    dina_read_json(entry$backup)
  } else list(family = "dina_publication", active = list())
  if (!recovering) previous$latest_candidate <- state$latest_candidate
  lock <- file.path(root, "output", "experiments", "finished_dina_publication", ".publishing")
  if (dir.exists(lock)) {
    owner <- dina_read_json(file.path(lock, "owner.json"), default = NULL)
    if (!recovering || !identical(owner$confirmation, path) || dina_finished_include_pid_alive(owner$pid)) stop("Another activation owns the publication lock.", call. = FALSE)
    unlink(lock, recursive = TRUE)
  }
  if (!dir.create(lock, showWarnings = FALSE)) stop("Another publication operation is active.", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  dina_finished_publication_save(previous, pointer)
  manifest$status <- "restored"; manifest$restored_at <- dina_now()
  dina_finished_publication_save(manifest, journal)
  dina_cli_cat("Restored the previous integration cache. 07d will validate it against current included sources and rebuild if needed.")
  invisible(previous)
}

dina_finished_publication_include <- function(root, flags = list(), input = "stdin", is_terminal = isatty(stdin())) {
  if (isTRUE(flags[["dry-run"]]) && (isTRUE(flags$confirm) || !is.null(flags$restore))) stop("A publication dry run cannot accept or restore.", call. = FALSE)
  if (!is.null(flags$restore)) {
    if (isTRUE(flags$confirm) || !is.null(flags[["include-run"]])) stop("Choose publication activation or restore.", call. = FALSE)
    return(dina_finished_publication_restore(root, flags$restore))
  }
  state <- dina_finished_publication_state(root)
  candidate <- if (is.null(flags[["include-run"]])) {
    dina_finished_publication_prepare(root, flags)
  } else dina_finished_publication_candidate(root, flags[["include-run"]])
  if (!is.null(flags$adjustment) && !identical(flags$adjustment, candidate$adjustment)) stop("Requested adjustment differs from the saved candidate. Prepare a new --dry-run.", call. = FALSE)
  if (!is.null(flags$source)) {
    selected <- sort(trimws(strsplit(flags$source, ",", fixed = TRUE)[[1L]]))
    if (!identical(selected, sort(names(candidate$sources)))) stop("Requested sources differ from the saved candidate. Prepare a new --dry-run.", call. = FALSE)
  }
  dina_finished_publication_show(root, "continuation", flags, candidate)
  if (!identical(candidate$status, "ready")) stop("Publication candidate needs attention; inspect continuation-problems.", call. = FALSE)
  dina_finished_publication_verify(root, candidate)
  if (isTRUE(flags[["dry-run"]])) {
    dina_cli_cat("Integration preview prepared. The integration cache and final exports are unchanged; 07d integrates included sources automatically.")
    return(invisible(candidate))
  }
  same <- all(vapply(names(candidate$sources), function(id) identical(state$active[[id]]$candidate, candidate$run), logical(1)))
  if (same) {
    dina_finished_publication_active(root)
    dina_cli_cat("This publication candidate is already active; no files changed.")
    return(invisible(state))
  }
  state <- dina_finished_publication_activate(root, candidate)
  dina_cli_cat("Validated integration cached. Run 07d to update final exports and donor calculations; no separate approval is required.")
  dina_finished_note("Restore cache: dina sources include dina --publication --restore ", state$confirmation)
  invisible(state)
}
