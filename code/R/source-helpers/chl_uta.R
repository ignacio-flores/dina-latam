`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

chl_uta_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required R package is missing: ", package, call. = FALSE)
  }
}

chl_uta_parse_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x <- gsub("[^0-9,.-]", "", x)
  has_comma <- grepl(",", x, fixed = TRUE)
  x[has_comma] <- gsub("\\.", "", x[has_comma])
  x[has_comma] <- gsub(",", ".", x[has_comma], fixed = TRUE)
  thousands_dot <- !has_comma & grepl("^[0-9]{1,3}(\\.[0-9]{3})+$", x)
  x[thousands_dot] <- gsub("\\.", "", x[thousands_dot])
  suppressWarnings(as.numeric(x))
}

chl_uta_normalize <- function(data, path = "", years = NULL, strict_years = FALSE) {
  names(data) <- tolower(names(data))
  missing <- setdiff(c("year", "uta"), names(data))
  if (length(missing)) {
    stop("Chile UTA input must contain year and uta columns", if (nzchar(path)) paste0(": ", path) else "", call. = FALSE)
  }
  out <- data.frame(
    year = suppressWarnings(as.integer(data$year)),
    uta = chl_uta_parse_number(data$uta),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$year), , drop = FALSE]
  if (!nrow(out)) {
    stop("Chile UTA input has no usable year rows", if (nzchar(path)) paste0(": ", path) else "", call. = FALSE)
  }
  if (any(is.na(out$uta)) || any(out$uta <= 0, na.rm = TRUE)) {
    stop("Chile UTA values must be positive numeric values", if (nzchar(path)) paste0(": ", path) else "", call. = FALSE)
  }
  duplicated_years <- unique(out$year[duplicated(out$year)])
  if (length(duplicated_years)) {
    stop("Chile UTA input has duplicate years: ", paste(duplicated_years, collapse = ","), call. = FALSE)
  }
  out <- out[order(out$year), , drop = FALSE]
  if (!is.null(years)) {
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    if (isTRUE(strict_years)) {
      missing_years <- setdiff(years, out$year)
      if (length(missing_years)) {
        stop("Chile UTA input is missing required years: ", paste(missing_years, collapse = ","), call. = FALSE)
      }
    }
    out <- out[out$year %in% years, , drop = FALSE]
  }
  out
}

chl_uta_read_csv <- function(path, years = NULL, strict_years = FALSE) {
  chl_uta_need("readr")
  if (!file.exists(path)) {
    stop("Chile UTA input is missing: ", path, call. = FALSE)
  }
  data <- readr::read_csv(path, show_col_types = FALSE)
  chl_uta_normalize(data, path = path, years = years, strict_years = strict_years)
}

chl_uta_sii_url <- function(year, url_template = Sys.getenv("DINA_FETCH_CHL_UTA_URL_TEMPLATE", unset = "")) {
  if (nzchar(url_template)) {
    return(gsub("\\{year\\}", as.character(year), url_template))
  }
  if (year < 2013L) {
    sprintf("https://www.sii.cl/pagina/valores/utm/utm%s.htm", year)
  } else {
    sprintf("https://www.sii.cl/valores_y_fechas/utm/utm%s.htm", year)
  }
}

chl_uta_extract_year <- function(content, year) {
  chl_uta_need("rvest")
  chl_uta_need("stringr")
  tables <- rvest::html_table(content, fill = TRUE, dec = ",", convert = FALSE)
  for (table in tables) {
    if (!nrow(table)) next
    row_text <- apply(table, 1, function(row) paste(as.character(row), collapse = " "))
    rows <- stringr::str_detect(row_text, stringr::regex("^\\s*Diciembre\\b", ignore_case = TRUE))
    if (!any(rows)) next
    row <- table[which(rows)[[1L]], , drop = FALSE]
    values <- chl_uta_parse_number(unlist(row, use.names = FALSE))
    values <- values[!is.na(values)]
    values <- values[values > 0 & values != year]
    if (length(values) >= 2L) {
      return(data.frame(year = as.integer(year), uta = values[[2L]], stringsAsFactors = FALSE))
    }
  }
  text <- rvest::html_text2(content)
  lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  lines <- lines[stringr::str_detect(lines, stringr::regex("^\\s*Diciembre\\b", ignore_case = TRUE))]
  for (line in lines) {
    values <- chl_uta_parse_number(unlist(regmatches(line, gregexpr("[0-9][0-9.,]*", line, perl = TRUE)), use.names = FALSE))
    values <- values[!is.na(values)]
    values <- values[values > 0 & values != year]
    if (length(values) >= 2L) {
      return(data.frame(year = as.integer(year), uta = values[[2L]], stringsAsFactors = FALSE))
    }
  }
  stop("Could not find December UTA in SII table for year ", year, call. = FALSE)
}

chl_uta_scrape <- function(years, url_template = Sys.getenv("DINA_FETCH_CHL_UTA_URL_TEMPLATE", unset = "")) {
  chl_uta_need("rvest")
  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years)]
  rows <- lapply(years, function(year) {
    url <- chl_uta_sii_url(year, url_template = url_template)
    content <- rvest::read_html(url)
    chl_uta_extract_year(content, year)
  })
  chl_uta_normalize(do.call(rbind, rows), years = years, strict_years = TRUE)
}

chl_uta_load <- function(input_root = ".", years, allow_fetch = FALSE) {
  path <- file.path(input_root, "input_data", "admin_data", "CHL", "uta_december.csv")
  if (file.exists(path)) {
    return(chl_uta_read_csv(path, years = years, strict_years = TRUE))
  }
  if (isTRUE(allow_fetch)) {
    return(chl_uta_scrape(years))
  }
  stop(
    "Chile UTA canonical input is missing: ", path,
    ". Run `dina sources fetch chl-uta`, then `dina sources explore admin` and `dina sources include admin --dry-run`.",
    call. = FALSE
  )
}
