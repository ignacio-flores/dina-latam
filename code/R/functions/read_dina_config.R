# Read the effective DINA YAML config used for the current run.
# During `dina run`, DINA_CONFIG_OVERRIDE_YML points R helpers to the working
# update override so experiments do not silently edit benchmark config.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

read_dina_yaml <- function(path, default = list()) {
  if (!file.exists(path)) {
    return(default)
  }
  yaml::read_yaml(path)
}

merge_dina_config <- function(base, override) {
  if (!is.list(base) || !is.list(override)) {
    return(override)
  }
  out <- base
  for (name in names(override)) {
    if (!is.null(out[[name]]) && is.list(out[[name]]) && is.list(override[[name]])) {
      out[[name]] <- merge_dina_config(out[[name]], override[[name]])
    } else {
      out[[name]] <- override[[name]]
    }
  }
  out
}

read_dina_config <- function(
  path = Sys.getenv("DINA_CONFIG_YML", unset = "config/dina.yml"),
  override_path = Sys.getenv("DINA_CONFIG_OVERRIDE_YML", unset = "")
) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The yaml package is required to read DINA config.", call. = FALSE)
  }
  config <- read_dina_yaml(path)
  if (nzchar(override_path) && file.exists(override_path)) {
    config <- merge_dina_config(config, read_dina_yaml(override_path))
  }

  # Compatibility fields for older R pipeline code that expects Stata-style names.
  config$first_y <- config$years$first
  config$last_y <- config$years$last
  config$all_countries <- paste(config$countries %||% character(), collapse = " ")
  config
}

read_dina_countries <- function(config) {
  if (!is.null(config$countries) && length(config$countries)) {
    return(config$countries)
  }
  raw_list <- strsplit(config$all_countries %||% "", "\\s+")[[1]]
  raw_list <- raw_list[raw_list != ""]
  countries <- gsub('^"|"$', "", raw_list)
  countries[nzchar(countries)]
}
