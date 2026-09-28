* Load the single configuration authority for a DINA task.
* `dina run` supplies DINA_CONFIG_DO; manual runs must supply an explicitly
* generated and validated file through the same environment variable.

if "${dina_runtime_config_loaded}" != "yes" {
    local dina_config_do : environment DINA_CONFIG_DO
    if "`dina_config_do'" == "" {
        di as error "DINA runtime configuration is required."
        di as error "Run through `dina run`, or generate one with `dina config stata --output PATH`."
        exit 198
    }
    capture confirm file "`dina_config_do'"
    if _rc {
        di as error "DINA_CONFIG_DO does not point to a readable file: `dina_config_do'"
        exit 601
    }
    quietly do "`dina_config_do'"
    foreach required in dina_config_scope dina_config_fingerprint dina_baseline_fingerprint dina_config_countries all_countries first_y last_y lang debug bfm_replace all_units all_steps export_unit export_steps export_last_y previous_update {
        if `"${`required'}"' == "" {
            di as error "Generated DINA runtime configuration is missing `required'."
            exit 198
        }
    }
    global dina_runtime_config_loaded "yes"
}
