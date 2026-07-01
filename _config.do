* Runtime DINA config loader.
* DINA keeps config in YAML and passes a temporary Stata globals file at run time.
local dina_config_runtime : environment DINA_CONFIG_DO
if "`dina_config_runtime'" != "" {
	run "`dina_config_runtime'"
	exit
}

di as error "DINA config globals are no longer stored in _config.do."
di as error "Run pipeline tasks through: dina run TASK --execute"
di as error "For manual Stata runs, export a temporary config with: dina config stata --output PATH"
di as error "Then set DINA_CONFIG_DO to that PATH before running Stata."
exit 198
