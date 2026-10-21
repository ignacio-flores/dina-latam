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
     (country == "cRI" & !(year %in% c(2016, 2021)) & p == 0.9999) ~ ((topsh * 7.23) * average)/0.0001,
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
# Build df_clean (collapse + sanitize)
# =======================
df_clean <- df_base %>%
  group_by(country, year, step, unit) %>%
  group_modify(~ collapse_and_clean(.x)) %>%
  group_modify(~ enforce_avg_strictly_inside(.x)) %>%
  ungroup()

#check total average again
df_clean  <- df_clean %>% group_by(step, unit, country, year) %>%
  mutate(pop = lead(p) - p, 
  pop = if_else(row_number() == n(), 1-p, pop),
  checkpop = sum(pop, na.rm=T),
  checkavg = weighted.mean(avg, pop)) %>% 
  mutate(average = checkavg) %>%
  select(-c("pop", "checkpop", "checkavg")) %>%
  ungroup()

# =======================
# Fit + tabulation
# =======================
fit_and_tab <- function(d) {
  if (nrow(d) < 2L) stop("Not enough rows in group after collapsing.")
  
  # extra guard: enforce strictly greater avg than thr (tiny nudge)
  #d <- d %>% mutate(avg = ifelse(avg <= thr + tol, thr + 2 * tol, avg))
  d <- d %>%
    arrange(p) %>%
    mutate(
      next_thr = lead(thr),
      g        = next_thr - thr,
      m_lo0    = strict_margin(thr),
      m_hi0    = strict_margin(coalesce(next_thr, thr)),
      eps      = ifelse(is.finite(next_thr),
                        pmax(0, pmin(m_lo0, m_hi0, g/3)),
                        m_lo0),
      avg = ifelse(
        is.finite(next_thr),
        pmin(pmax(avg, thr + eps), next_thr - eps),
        pmax(avg, thr + eps)
      )
    ) %>%
    select(-next_thr, -g, -m_lo0, -m_hi0, -eps)
  
  all_thr_na <- all(is.na(d$thr))
  dist <- if (all_thr_na) {
    gpinter::tabulation_fit(
      p          = d$p,
      bracketavg = d$avg,
      average    = d$average[1]
    )
  } else {
    gpinter::tabulation_fit(
      p          = d$p,
      threshold  = d$thr,
      bracketavg = d$avg,
      average    = d$average[1]
    )
  }
  
  tab <- gpinter::generate_tabulation(
    dist,
    fractiles    = p_grid,
    threshold    = TRUE,
    bracketavg   = TRUE,
    bracketshare = TRUE,
    topavg       = TRUE,
    topshare     = TRUE
  )
  
  if (length(tab$fractile) != length(p_grid) ||
      anyDuplicated(tab$fractile) || any(abs(tab$fractile - p_grid) > 1e-10) ||
      !all(vapply(tab[c("fractile", "threshold", "bracket_average", "bracket_share", "top_average", "top_share")],
                  function(value) all(is.finite(value)), logical(1)))) {
    stop("GPinter produced incomplete or non-finite fractile statistics.")
  }
  tibble(
    p       = tab$fractile,
    thr     = tab$threshold,
    avg     = tab$bracket_average,
    bckt_sh = tab$bracket_share,
    topavg  = tab$top_average,
    topsh   = tab$top_share
  )
}

# =======================
# map_dfr over all groups (robust) + single-pass error log
# =======================
safe_fit_and_tab <- purrr::safely(fit_and_tab, otherwise = NULL)

# Keep full preprocessing and expected group coverage; fit selected countries only.
full_groups <- df_clean %>% distinct(country, year, step, unit) %>% arrange(country, year, step, unit)
readr::write_csv(full_groups, output_path("_groups.csv"))
selected_groups <- full_groups %>% filter(country %in% selected_countries)
grouped <- df_clean %>%
  filter(country %in% selected_countries) %>%
  group_by(country, year, step, unit) %>%
  group_split(.keep = TRUE)

keys_list <- map(grouped, ~ dplyr::distinct(.x, country, year, step, unit))
res_list  <- map(grouped, safe_fit_and_tab)

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

coverage <- selected_groups %>% mutate(status = "success", reason = "")
if (length(res_list)) {
  coverage$status <- vapply(res_list, function(x) if (is.null(x$error)) "success" else "failed", "")
  coverage$reason <- vapply(res_list, function(x) if (is.null(x$error)) "" else conditionMessage(x$error), "")
}
absent_countries <- setdiff(selected_countries, selected_groups$country)
if (length(absent_countries)) coverage <- bind_rows(coverage,
  tibble(country = absent_countries, year = 0L, step = "gpinter", unit = "all",
         status = "excluded", reason = "No eligible groups after the existing preprocessing rules"))
readr::write_csv(coverage, output_path("_coverage.csv"))
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
