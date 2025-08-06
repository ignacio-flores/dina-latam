# Setup
suppressMessages(suppressPackageStartupMessages({
  required_packages <- c(
    "purrr", "furrr", "readr", "tibble", "dplyr", "Hmisc",
    "glue", "magrittr", "janitor", "haven", "stringr", "stringi"
  )
  
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  for (pkg in required_packages) {
    if (!suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE))) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}))
source("code/R/functions/get_svy_totpop.R")

#read config file 
lines <- readLines("_config.do")
config <- list()
for (line in lines) {
  if (grepl("^\\s*global\\s+", line)) {
    parts <- strcapture("^\\s*global\\s+([a-zA-Z0-9_]+)\\s+\"?(.+?)\"?$", line, proto = list(name = "", value = ""))
    config[[parts$name]] <- parts$value
  }
}
config$first_year <- as.integer(config$first_y)
config$last_year <- as.integer(config$last_y)
raw_list <- strsplit(config$all_countries, "\\s+")[[1]]
raw_list <- raw_list[raw_list != ""]
countries <- gsub('^"|"$', "", raw_list)

print("Using an R call to compute survey populations...")


# I. Get total populations from surveys ----------------------------------------------------------

#get population data from surveys 
popdata <- map_dfr(countries, 
   ~get_svy_totpop(
     c = .x,
     years = config$first_year:config$last_year
   )
)

#inter/extra-polate population linearly 
popdata %<>% 
  group_by(country) %>% 
  mutate(
    totpop_i      = approx(x = year, y = totpop, xout = year, method = "linear")$y, 
    pct_adults_i  = approx(x = year, y = pct_adults, xout = year, method = "linear")$y, 
    totpop_ie     = approxExtrap(x = year, y = totpop_i, xout = year, method = "linear")$y, 
    pct_adults_ie = approxExtrap(x = year, y = pct_adults_i, xout = year, method = "linear")$y
  )

# Define the folder path
folder_path <- "intermediary_data/population"
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
  message("Folder created: ", folder_path)
} 

#write .dta file 
write_dta(popdata, "intermediary_data/population/SurveyPop.dta")