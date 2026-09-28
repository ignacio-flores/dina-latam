do "code/Stata/auxiliar/dina_runtime_config.do"
di as txt "Using an R to interpolate ineqstats..."
rcall: source("code/R/07c_interpolate_ineqstats.R")
