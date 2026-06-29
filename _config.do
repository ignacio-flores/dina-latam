* Configuration file
local dina_config_override : environment DINA_CONFIG_DO
if "`dina_config_override'" != "" {
	run "`dina_config_override'"
	exit
}

global all_countries " "COL" "ARG" "PER" "URY" "CRI" "ECU" "CHL" "BRA" "SLV" "MEX" "DOM" "
global first_y 2000
global last_y 2023

global lang "eng"
global debug "no"
global bfm_replace "no" 

global all_units " "ind" "esn" "pch" " 
global all_steps " "natinc" "pon" "
