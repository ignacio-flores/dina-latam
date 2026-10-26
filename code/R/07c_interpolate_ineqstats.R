# Direct and runner entry share the same temporary-output publisher.
source("code/R/country_run.R")
country_run_r_entry("07c-interpolate-ineqstats", commandArgs(trailingOnly = TRUE))
output_root <- Sys.getenv("DINA_OUTPUT_ROOT")
selected_countries <- strsplit(trimws(Sys.getenv("DINA_RUN_COUNTRIES")), "[[:space:],]+")[[1L]]
selected_countries <- unique(selected_countries[nzchar(selected_countries)])
if (!nzchar(output_root) || !length(selected_countries)) {
  stop("Missing country-run output folder or selected countries.", call. = FALSE)
}
output_path <- function(...) file.path(output_root, ...)
# devtools::install_github("world-inequality-database/gpinter")
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(gpinter)
library(tibble)
library(stringr)
source("code/R/gpinter_precision.R")
source("code/R/gpinter_thresholds.R")
source("code/R/gpinter_tail_repair.R")
source("code/R/gpinter_atoms.R")

options(scipen = 999)

# =======================
# Config
# =======================
path_csv <- "output/synthetic_microfiles/_smicrofile_long_detailed.csv"

# rounding precision to collapse tiny float gaps
thr_digits <- 2
avg_digits <- 2

# tolerance used for “strictness” checks (scale-aware sanitizer uses its own eps too)
tol <- 0.5 * 10^(-max(thr_digits, avg_digits))

p_grid <- c(
  seq(0,       0.99,    by = 0.01),
  seq(0.991,   0.999,   by = 0.001),
  seq(0.9991,  0.9999,  by = 0.0001),
  seq(0.99991, 0.99999, by = 0.00001)
)

# =======================
# Read + base prep
# =======================
df <- readr::read_csv(path_csv, show_col_types = FALSE)
df_base <- df

# Step 1: start with base selection and scaling rules
df_scaled <- df %>%
 select(step, unit, country, year, p, average, thr, avg, topavg, topsh) %>%
 group_by(step, unit, country, year) %>%
 mutate(
   pop = lead(p) - p, 
   pop = if_else(row_number() == n(), 0.0001, pop),
   p2 = "NA",
   p2 = if_else(p >= 0.99 & p < 0.999, "p99-p99.9", p2),
   p2 = if_else(p >= 0.999 & p < 0.9999, "p99.9-p99.99", p2)) %>%
 group_by(step, unit, country, year, p2) %>%
 mutate(avg2 = weighted.mean(avg, pop)) %>%
 ungroup() %>%  
 mutate(avg = if_else(p2 == "p99-p99.9" | p2 == "p99.9p99.99", avg2, avg)) %>%
 select(-c("p2", "avg2", "pop")) %>%  
 filter(p <= 0.99 | p %in% c(0.999, 0.9999)) %>%
 mutate(
   # scale top averages for top 0.1% and 0.01%
   topavg = case_when(
     country == "CHL" & p == 0.999  ~ ((topsh * 1.98) * average)/0.001,
     country == "CHL" & p == 0.9999 ~ ((topsh * 6.83) * average)/0.0001,
     country == "BRA" & p == 0.999  ~ ((topsh * 2.38) * average)/0.001,
     country == "BRA" & p == 0.9999 ~ ((topsh * 7.62) * average)/0.0001,
     !(country %in% c("CHL", "BRA", "PER", "ECU", "DOM", "CRI", "ARG")) & p == 0.999  ~ ((topsh * 2.18) * average)/0.001,
     !(country %in% c("CHL", "BRA", "PER", "ECU", "DOM", "CRI", "ARG")) & p == 0.9999 ~ ((topsh * 7.23) * average)/0.0001,
     (country == "PER" & !(year %in% c(2001, 2003)) & p == 0.999)  ~ ((topsh * 2.18) * average)/0.001,
     (country == "PER" & !(year %in% c(2001, 2003)) & p == 0.9999) ~ ((topsh * 7.23) * average)/0.0001,
     (country == "ECU" & !(year %in% c(2007, 2010, 2012)) & p == 0.999)  ~ ((topsh * 2.18) * average)/0.001,
     (country == "ECU" & !(year %in% c(2007, 2010, 2012)) & p == 0.9999) ~ ((topsh * 7.23) * average)/0.0001,
     (country == "DOM" & !(year %in% c(2016, 2018)) & p == 0.999)  ~ ((topsh * 2.18) * average)/0.001,
     (country == "DOM" & !(year %in% c(2016, 2018)) & p == 0.9999) ~ ((topsh * 7.23) * average)/0.0001,
     (country == "CRI" & !(year %in% c(2016, 2021)) & p == 0.999)  ~ ((topsh * 2.18) * average)/0.001,
     (country == "CRI" & !(year %in% c(2016, 2021)) & p == 0.9999) ~ ((topsh * 7.23) * average)/0.0001,
     (country == "ARG" & !(year %in% c(2003, 2007)) & p == 0.999)  ~ ((topsh * 2.18) * average)/0.001,
     (country == "ARG" & !(year %in% c(2003, 2007)) & p == 0.9999) ~ ((topsh * 7.23) * average)/0.0001,
     TRUE                                           ~ topavg
   ),
   # ensure avg equals topavg for the very top
   avg = case_when(
     dplyr::near(p, 0.9999) ~ topavg,
     TRUE                   ~ avg
   ),
   # clamp tiny negatives (e.g., "-0.00")
   thr = pmax(as.numeric(thr), 0),
   avg = pmax(as.numeric(avg), 0)
 )


# Step 2: compute the residual bracket mean per group using scaled topavg
bracket_means <- df_scaled %>%
 filter(dplyr::near(p, 0.999) | dplyr::near(p, 0.9999)) %>%
 mutate(
   p_key = case_when(
     dplyr::near(p, 0.999)  ~ "mu_0.999",
     dplyr::near(p, 0.9999) ~ "mu_0.9999"
   )
 ) %>%
 select(country, year, unit, step, p_key, topavg) %>%
 pivot_wider(names_from = p_key, values_from = topavg) %>%
 mutate(
   mu_999_bracket = (10 * mu_0.999 - mu_0.9999) / 9
 )

# Step 3: join the bracket means back to the main data
df_joined <- left_join(df_scaled, bracket_means,
                      by = c("country", "year", "unit", "step"))

# Step 4: update avg at p == 0.999 to the residual bracket mean
df_base <- df_joined %>%
 mutate(
   avg = case_when(
     dplyr::near(p, 0.999) & !is.na(mu_999_bracket) ~ mu_999_bracket,
     TRUE                                           ~ avg
   ),
   avg = pmax(avg, 0)
 ) %>%
 select(-mu_0.999, -mu_0.9999, -mu_999_bracket)

# Step 4: normalize averages and adjust bottom  
norm <- df_base %>% 
  select(step, unit, country, year, p, thr, avg, average, topavg) %>%
  mutate(avg = avg / average, topavg = topavg / average) %>%
  group_by(step, unit, country, year) %>%
  mutate(
    pop = lead(p) - p, 
    pop = if_else(row_number() == 102, 0.0001, pop),
    top = if_else(p >= 0.999, avg, NA),
    bot = if_else(p <  0.999 & p >= 0.9, avg, NA),
    t10a = if_else(p == 0.9, topavg, NA),
    t10a = sum(t10a, na.rm = T),
    topa = weighted.mean(top, pop, na.rm = T),
    bota = weighted.mean(bot, pop, na.rm = T),
    tgt = if_else(!is.na(bot), (t10a - topa * 0.01)/ 0.99, NA),
    sca = if_else(!is.na(bot), tgt / bota, NA),
    avg = if_else(!is.na(bot), avg * sca, avg),
    checkpop = sum(pop, na.rm=T),
    checkavg = weighted.mean(avg, pop),
    avg = avg * average
    )

# Step 5: go back to normality 
df_base <- select(norm, step, unit, country, year, p, thr, avg, average)  
df_base

# =======================
# Collapse logic per group
# =======================
collapse_and_clean <- function(d) {
  d <- d %>%
    arrange(p) %>%
    distinct(p, .keep_all = TRUE) %>%             # drop exact duplicate p's (if any)
    mutate(width = lead(p, default = 1) - p)
  
  # Build runs to MERGE forward until a row is "clean":
  # start a new run only if thr strictly increases AND avg > thr
  d <- d %>%
    mutate(
      thr_prev = lag(thr),
      new_run  = case_when(
        row_number() == 1L ~ TRUE,
        (thr > thr_prev + tol) & (avg > thr + tol) ~ TRUE,
        TRUE ~ FALSE
      ),
      run_id = cumsum(new_run)
    )
  
  # Merge each run: p = first (left boundary), thr = first thr of run,
  # avg = width-weighted across merged span; 'average' = mean
  merged <- d %>%
    group_by(run_id) %>%
    summarise(
      p         = first(p),
      thr       = first(thr),
      avg       = stats::weighted.mean(avg, w = width, na.rm = TRUE),
      average   = mean(average, na.rm = TRUE),
      width_tot = sum(width, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    arrange(p)
  
  # Collapse any remaining equal thresholds (even if avg differs slightly)
  merged2 <- merged %>%
    group_by(thr) %>%
    summarise(
      p       = min(p),
      avg     = stats::weighted.mean(avg, w = width_tot, na.rm = TRUE),
      average = mean(average, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(p)
  
  # Safety clamps post-averaging
  merged2 %>%
    mutate(
      thr = pmax(thr, 0),
      avg = pmax(avg, 0)
    )
}

# Scale-aware epsilon for sanitizer
sanitize_eps <- function(x) {
  pmax(1e-6, 1e-9 * pmax(abs(x), 1))
}

# helper that guarantees a margin >= tol
strict_margin <- function(x) {
  pmax(sanitize_eps(x), tol)
}

# Enforce avg strictly inside (use margin >= tol on both sides)
enforce_avg_strictly_inside <- function(d) {
  d %>%
    arrange(p) %>%
    mutate(
      thr_lo = thr,
      thr_hi = lead(thr),
      g      = thr_hi - thr_lo,
      # base margins from scale + tol
      m_lo0  = strict_margin(thr_lo),
      m_hi0  = strict_margin(coalesce(thr_hi, thr_lo)),
      # gap-aware margin
      eps    = ifelse(is.finite(thr_hi),
                      pmax(0, pmin(m_lo0, m_hi0, g/3)),
                      m_lo0),
      avg = ifelse(
        is.finite(thr_hi),
        pmin(pmax(avg, thr_lo + eps), thr_hi - eps),
        pmax(avg, thr_lo + eps)   # open top
      )
    ) %>%
    select(-thr_lo, -thr_hi, -g, -m_lo0, -m_hi0, -eps)
}

# =======================
# Fit + tabulation
# =======================
fit_and_tab <- function(d) {
  prepare <- function(x) gpinter_threshold_prepare(x, tol, strict_margin,
    collapse_and_clean, enforce_avg_strictly_inside)
  prepared <- tryCatch(prepare(d), error = identity)
  tail_repair <- NULL
  atoms <- NULL
  threshold_changes <- NULL
  method <- "not_fitted"
  tryCatch({
  if (inherits(prepared, "error")) {
    # Explicitly approved fallback: preserve the bottom-90 bins and broad tail
    # shares; only relax incompatible detailed p90--p99 anchors/top-.01 target.
    tail_repair <- gpinter_tail_repair(d)
    d <- tail_repair$data
    # Legacy equal-threshold consolidation can redistribute income within the
    # bottom 90%. Fit the retained repaired means directly instead.
    fitting <- d
    leading_zeros <- sum(d$avg == 0)
    if (leading_zeros > 1L) fitting <- d[c(1L, seq.int(leading_zeros + 1L, nrow(d))), ]
    equality_tol <- 128 * .Machine$double.eps * pmax(1, abs(head(fitting$avg, -1L)), abs(tail(fitting$avg, -1L)))
    if (any(diff(fitting$avg) <= equality_tol)) {
      prepared <- simpleError("Retain exact equal-income population masses.")
    } else {
      prepared <- list(data = fitting, original = d,
        adjustments = gpinter_precision_empty_adjustments(), method = "shares_fit",
        reason = "Reconstruct thresholds from the retained population and income anchors after the approved tail repair.")
    }
  }
  union_grid <- if (is.null(tail_repair)) p_grid else
    sort(unique(round(c(p_grid, tail_repair$data$p), 12)))
  if (inherits(prepared, "error")) {
    # GPinter's continuous component cannot represent positive equal-income
    # masses itself. Preserve them exactly around the fitted continuous part.
    tab <- gpinter_atoms_tabulate(d, union_grid)
    atoms <- attr(tab, "atoms")
    original_tab <- attr(tab, "original_tabulation")
    changed <- original_tab$threshold != d$thr
    threshold_changes <- data.frame(p = d$p[changed],
      threshold_before = d$thr[changed], threshold_after = original_tab$threshold[changed],
      bracket_mean = d$avg[changed], difference = original_tab$threshold[changed] - d$thr[changed],
      rule = rep("gpinter_threshold_reconstruction", sum(changed)),
      reason = rep("Reconstruct continuous thresholds while retaining exact equal-income population masses.", sum(changed)))
    method <- "shares_fit_with_atoms"
    prepared <- list(adjustments = gpinter_precision_empty_adjustments(), method = method)
  } else {
  d <- prepared$data
  method <- prepared$method
  if (nrow(d) < 2L) stop("Not enough rows in group after collapsing.")
  # Substantive threshold conflicts use the adjusted means and populations as
  # anchors. Never clamp or reorder those means to make the fit succeed.
  dist <- if (identical(prepared$method, "shares_fit")) {
    gpinter_threshold_fit_shares(d)
  } else {
    gpinter::tabulation_fit(
      p          = d$p,
      threshold  = d$thr,
      bracketavg = d$avg,
      average    = d$average[1]
    )
  }
  if (length(attr(dist, "dina_fit_notes"))) prepared$reason <-
    paste(c(prepared$reason, attr(dist, "dina_fit_notes")), collapse = " ")
  threshold_changes <- gpinter_threshold_changes(dist, prepared)
  
  tab <- gpinter_threshold_tabulate(dist, union_grid, prepared$data)
  gpinter_threshold_validate_fit(dist, tab, prepared)
  }
  if (!is.null(tail_repair)) {
    gpinter_tail_validate_tab(tab, tail_repair)
    method <- paste0(method, "_tail_repaired")
  }
  # Aggregate any extra validation boundaries back to the exact export grid.
  index <- match(round(p_grid, 12), round(tab$fractile, 12))
  if (anyNA(index)) stop("GPinter omitted an export-grid boundary.")
  tail_income <- tab$top_average[index] * (1 - p_grid)
  if (length(index) != length(tab$fractile)) {
    bracket_income <- -diff(c(tail_income, 0))
    tab <- list(fractile = p_grid, threshold = tab$threshold[index],
      bracket_average = bracket_income / diff(c(p_grid, 1)),
      bracket_share = bracket_income / tail_income[1L],
      top_average = tab$top_average[index], top_share = tab$top_share[index])
  } else tab$fractile <- p_grid
  
  if (length(tab$fractile) != length(p_grid) ||
      anyDuplicated(tab$fractile) || any(abs(tab$fractile - p_grid) > 1e-10) ||
      !all(vapply(tab[c("fractile", "threshold", "bracket_average", "bracket_share", "top_average", "top_share")],
                  function(value) all(is.finite(value)), logical(1)))) {
    stop("GPinter produced incomplete or non-finite fractile statistics.")
  }
  gpinter_threshold_check_tab(tab, tail_income[1L], "Export grid")
  result <- tibble(
    p       = tab$fractile,
    thr     = tab$threshold,
    avg     = tab$bracket_average,
    bckt_sh = tab$bracket_share,
    topavg  = tab$top_average,
    topsh   = tab$top_share
  )
  attr(result, "precision_adjustments") <- prepared$adjustments
  attr(result, "threshold_reconstruction") <- threshold_changes
  attr(result, "tail_adjustments") <- if (is.null(tail_repair)) NULL else tail_repair$adjustments
  attr(result, "atoms") <- atoms
  attr(result, "fit_method") <- method
  result
  }, error = function(error) {
    error$precision_adjustments <- prepared$adjustments
    error$threshold_reconstruction <- threshold_changes
    if (!is.null(tail_repair)) error$tail_adjustments <- tail_repair$adjustments
    error$atoms <- atoms
    error$fit_method <- method
    stop(error)
  })
}

# =======================
# map_dfr over all groups (robust) + single-pass error log
# =======================
safe_fit_and_tab <- purrr::safely(fit_and_tab, otherwise = NULL)

# Keep full preprocessing and expected group coverage; fit selected countries only.
full_groups <- df_base %>% distinct(country, year, step, unit) %>% arrange(country, year, step, unit)
readr::write_csv(full_groups, output_path("_groups.csv"))
selected_groups <- full_groups %>% filter(country %in% selected_countries)
grouped <- df_base %>%
  filter(country %in% selected_countries) %>%
  group_by(country, year, step, unit) %>%
  group_split(.keep = TRUE)

keys_list <- map(grouped, ~ dplyr::distinct(.x, country, year, step, unit))
res_list  <- map(grouped, safe_fit_and_tab)

# Keep the small repair report with the staged outputs, including failed groups.
precision_report <- map2_dfr(keys_list, res_list, function(keys, res) {
  changes <- if (is.null(res$error)) attr(res$result, "precision_adjustments") else
    res$error$precision_adjustments
  if (is.null(changes) || !nrow(changes)) return(tibble())
  bind_cols(keys[rep(1L, nrow(changes)), ], changes)
})
if (!ncol(precision_report)) precision_report <- bind_cols(
  tibble(country = character(), year = integer(), step = character(), unit = character()),
  gpinter_precision_empty_adjustments())
readr::write_csv(precision_report, output_path("output/data_reports/gpinter_precision_adjustments.csv"))

# Keep reconstructed thresholds separate from small storage-precision repairs.
threshold_report <- map2_dfr(keys_list, res_list, function(keys, res) {
  changes <- if (is.null(res$error)) attr(res$result, "threshold_reconstruction") else
    res$error$threshold_reconstruction
  if (is.null(changes) || !nrow(changes)) return(tibble())
  bind_cols(keys[rep(1L, nrow(changes)), ], changes)
})
if (!ncol(threshold_report)) threshold_report <- tibble(
  country = character(), year = integer(), step = character(), unit = character(),
  p = double(), threshold_before = double(), threshold_after = double(),
  bracket_mean = double(), difference = double(), rule = character(), reason = character())
readr::write_csv(threshold_report, output_path("output/data_reports/gpinter_threshold_reconstruction.csv"))

tail_report <- map2_dfr(keys_list, res_list, function(keys, res) {
  changes <- if (is.null(res$error)) attr(res$result, "tail_adjustments") else res$error$tail_adjustments
  if (is.null(changes) || !nrow(changes)) return(tibble())
  bind_cols(keys[rep(1L, nrow(changes)), ], changes)
})
if (!ncol(tail_report)) tail_report <- bind_cols(
  tibble(country = character(), year = integer(), step = character(), unit = character()),
  gpinter_tail_empty_adjustments())
readr::write_csv(tail_report, output_path("output/data_reports/gpinter_tail_adjustments.csv"))

atom_report <- map2_dfr(keys_list, res_list, function(keys, res) {
  changes <- if (is.null(res$error)) attr(res$result, "atoms") else res$error$atoms
  if (is.null(changes) || !nrow(changes)) return(tibble())
  bind_cols(keys[rep(1L, nrow(changes)), ], changes)
})
if (!ncol(atom_report)) atom_report <- tibble(
  country = character(), year = integer(), step = character(), unit = character(),
  p_start = double(), p_end = double(), income = double(), population = double(),
  first_row = integer(), last_row = integer(), mean_roundoff = double())
readr::write_csv(atom_report, output_path("output/data_reports/gpinter_atoms.csv"))

result_tabs <- if (length(keys_list)) {
  map2_dfr(keys_list, res_list, function(keys, res) {
    if (is.null(res$result)) return(tibble())
    tab <- res$result
    dplyr::bind_cols(keys[rep(1, nrow(tab)), ], tab)
  })
} else tibble(country = character(), year = integer(), step = character(), unit = character(),
              p = double(), thr = double(), avg = double(), bckt_sh = double(), topavg = double(), topsh = double())
if (nrow(result_tabs)) result_tabs <- arrange(result_tabs, country, year, step, unit, p)

errors_log <- map2_chr(keys_list, res_list, function(keys, res) {
  if (!is.null(res$error)) {
    paste0(
      keys$country, "-", keys$year, "-", keys$step, "-", keys$unit,
      " | ", conditionMessage(res$error)
    )
  } else NA_character_
}) %>%
  discard(is.na)

coverage <- selected_groups %>% mutate(status = "success", reason = "", fit_method = "not_fitted")
if (length(res_list)) {
  coverage$status <- vapply(res_list, function(x) if (is.null(x$error)) "success" else "failed", "")
  coverage$reason <- vapply(res_list, function(x) if (is.null(x$error)) "" else conditionMessage(x$error), "")
  coverage$fit_method <- vapply(res_list, function(x) {
    method <- if (is.null(x$error)) attr(x$result, "fit_method") else x$error$fit_method
    if (is.null(method)) "not_fitted" else method
  }, "")
}
absent_countries <- setdiff(selected_countries, selected_groups$country)
if (length(absent_countries)) coverage <- bind_rows(coverage,
  tibble(country = absent_countries, year = 0L, step = "gpinter", unit = "all",
         status = "excluded", reason = "No eligible groups after the existing preprocessing rules",
         fit_method = "not_fitted"))
readr::write_csv(coverage, output_path("output/data_reports/gpinter_fitting_methods.csv"))
# Preserve the shared coverage schema used by saved country chunks and resume.
readr::write_csv(select(coverage, -fit_method), output_path("_coverage.csv"))
readr::write_lines(errors_log, output_path("output/data_reports/gpinter_errors.log"))
if (length(errors_log)) {
  stop(sprintf("GPinter failed for %d group(s): %s. The published CSV has not changed.",
               length(errors_log), errors_log[[1L]]), call. = FALSE)
}
if (nrow(result_tabs)) {
  keys <- c("country", "year", "step", "unit")
  if (anyDuplicated(as.data.frame(result_tabs[c(keys, "p")]))) stop("GPinter produced duplicate fractile keys.")
  produced <- result_tabs %>% count(country, year, step, unit, name = "rows")
  if (nrow(anti_join(selected_groups, produced, by = keys)) ||
      nrow(anti_join(produced, selected_groups, by = keys)) || any(produced$rows != length(p_grid))) {
    stop("GPinter selected group coverage is incomplete.", call. = FALSE)
  }
  numerical <- c("p", "thr", "avg", "bckt_sh", "topavg", "topsh")
  if (!all(vapply(result_tabs[numerical], function(x) all(is.finite(x)), logical(1)))) {
    stop("GPinter produced non-finite statistics.", call. = FALSE)
  }
} else if (nrow(selected_groups)) stop("GPinter produced no selected groups.", call. = FALSE)
readr::write_csv(result_tabs, output_path("output/ineqstats/_gpinter_all_topcorrected.csv"))
