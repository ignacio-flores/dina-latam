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
    foreach required in dina_config_scope dina_config_fingerprint dina_baseline_fingerprint dina_config_countries admin_trust_regions all_countries first_y last_y lang debug bfm_replace all_units all_steps export_unit export_steps export_last_y previous_update {
        if `"${`required'}"' == "" {
            di as error "Generated DINA runtime configuration is missing `required'."
            exit 198
        }
    }
    global dina_runtime_config_loaded "yes"
}
if "${bfm_replace}" != "no" {
    di as error "Only BFM noreplace is supported; bfm_replace must be no."
    exit 198
}

* Selection belongs to this worker process; the configured donor universe stays
* in all_countries. Every invocation refreshes these values from its own env.
local dina_worker : environment DINA_COUNTRY_WORKER
local dina_selected : environment DINA_RUN_COUNTRIES
local dina_output : environment DINA_OUTPUT_ROOT
* The generated universe quotes each ISO3. Strip those delimiters into a
* separate list; ordinary local assignment can unbalance the end quotes.
local dina_universe : subinstr global all_countries `"""' "", all
local dina_universe = strtrim("`dina_universe'")
if "`dina_worker'" != "" {
    if trim("`dina_selected'") == "" | `"`dina_output'"' == "" {
        di as error "Country worker requires a country selection and output folder."
        exit 198
    }
    foreach dina_country in `dina_selected' {
        local dina_one "`dina_country'"
        local dina_known : list dina_one in dina_universe
        if !`dina_known' {
            di as error "Unknown execution country: `dina_country'"
            exit 198
        }
    }
    global dina_run_countries "`dina_selected'"
    global dina_output_root `"`dina_output'"'
}
else {
    global dina_run_countries "`dina_universe'"
    global dina_output_root "."
}
