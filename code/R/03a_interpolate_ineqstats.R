# devtools::install_github("world-inequality-database/gpinter")

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(gpinter)
library(tibble)
library(stringr)

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

df_base <- df %>%
  select(step, unit, country, year, p, average, thr, avg, topavg, topsh) %>%
  filter(p <= 0.99) %>%
  mutate(
    # use topavg on the last open bracket
    avg = ifelse(p != 0.99, avg, topavg),
    
    # clamp tiny negatives to 0 (e.g., "-0.00")
    thr = pmax(as.numeric(thr), 0),
    avg = pmax(as.numeric(avg), 0),
    
    # kill float noise
    thr = round(thr, thr_digits),
    avg = round(avg, avg_digits)
  )

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

# Enforce avg strictly inside (thr, next_thr) (or > thr for last open bracket)
enforce_avg_strictly_inside <- function(d) {
  d %>%
    arrange(p) %>%
    mutate(
      thr_lo = thr,
      thr_hi = lead(thr),
      eps_lo = sanitize_eps(thr_lo),
      eps_hi = sanitize_eps(coalesce(thr_hi, thr_lo)),
      avg = ifelse(
        is.finite(thr_hi),
        pmin(pmax(avg, thr_lo + eps_lo), thr_hi - eps_hi),
        pmax(avg, thr_lo + eps_lo)   # open-top: only lower clamp
      )
    ) %>%
    select(-thr_lo, -thr_hi, -eps_lo, -eps_hi)
}

# =======================
# Build df_clean (collapse + sanitize)
# =======================
df_clean <- df_base %>%
  group_by(country, year, step, unit) %>%
  group_modify(~ collapse_and_clean(.x)) %>%
  group_modify(~ enforce_avg_strictly_inside(.x)) %>%
  ungroup()

# =======================
# Fit + tabulation
# =======================
fit_and_tab <- function(d) {
  if (nrow(d) < 2L) stop("Not enough rows in group after collapsing.")
  
  # extra guard: enforce strictly greater avg than thr (tiny nudge)
  d <- d %>% mutate(avg = ifelse(avg <= thr + tol, thr + 2 * tol, avg))
  
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

grouped <- df_clean %>%
  group_by(country, year, step, unit) %>%
  group_split(.keep = TRUE)

keys_list <- map(grouped, ~ dplyr::distinct(.x, country, year, step, unit))
res_list  <- map(grouped, safe_fit_and_tab)

result_tabs <- map2_dfr(keys_list, res_list, function(keys, res) {
  if (is.null(res$result)) return(tibble())
  tab <- res$result
  dplyr::bind_cols(keys[rep(1, nrow(tab)), ], tab)
}) %>%
  arrange(country, year, step, unit, p)

errors_log <- map2_chr(keys_list, res_list, function(keys, res) {
  if (!is.null(res$error)) {
    paste0(
      keys$country, "-", keys$year, "-", keys$step, "-", keys$unit,
      " | ", conditionMessage(res$error)
    )
  } else NA_character_
}) %>%
  discard(is.na)

# Example writes:
readr::write_csv(result_tabs, "output/ineqstats/_gpinter_all.csv")
if (length(errors_log)) readr::write_lines(errors_log, "output/data_reports/gpinter_errors.log")

