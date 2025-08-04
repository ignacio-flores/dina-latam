# ---------------------------------------------------------------------------- #
# Import and do some basic cleaning of the UN SNA data
# ---------------------------------------------------------------------------- #

#Libraries
library(readr)
library(magrittr)
library(glue)
library(readxl)
library(janitor)
library(plyr)
library(dplyr)
library(haven)

# Import all tables and normalize column names ------------------------------- #

table_names <- c(
  "Table 1.1: Gross domestic product by expenditures at current prices",
  "Table 1.2: Gross domestic product by expenditures at constant prices",
  "Table 1.3: Relations among product, income, savings, and net lending aggregates",
  "Table 2.1: Value added by industries at current prices (ISIC Rev. 3)",
  "Table 2.2: Value added by industries at constant prices (ISIC Rev. 3)",
  "Table 2.3: Output, gross value added, and fixed assets by industries at current prices (ISIC Rev. 3)",
  "Table 2.4: Value added by industries at current prices (ISIC Rev. 4)",
  "Table 2.5: Value added by industries at constant prices (ISIC Rev. 4)",
  "Table 2.6: Output, gross value added and fixed assets by industries at current prices (ISIC Rev. 4)",
  "Table 3.1: Government final consumption expenditure by function at current prices",
  "Table 3.2: Individual consumption expenditure of households, NPISHs, and general government at current prices",
  "Table 4.1: Total Economy (S.1)",
  "Table 4.2: Rest of the world (S.2)",
  "Table 4.3: Non-financial Corporations (S.11)",
  "Table 4.4: Financial Corporations (S.12)",
  "Table 4.5: General Government (S.13)",
  "Table 4.6: Households (S.14)",
  "Table 4.7: Non-profit institutions serving households (S.15)",
  "Table 4.8: Combined Sectors: Non-Financial and Financial Corporations (S.11 + S.12)",
  "Table 4.9: Combined Sectors: Households and NPISH (S.14 + S.15)",
  "Table 5.1: Cross classification of Gross value added by industries and institutional sectors (ISIC Rev. 3)",
  "Table 5.2: Cross classification of Gross value added by industries and institutional sectors (ISIC Rev. 4)"
)

table_codes <- c(
    "101", "102", "103",
    "201", "202", "203", "204", "205", "206",
    "301", "302",
    "401", "402", "403", "404", "405", "406", "407", "408", "409",
    "501", "502"
)

un_tables <- lapply(table_codes, function(code) {
    table <- read_csv(file.path("primary_data/sna-un", glue("{code}.csv.gz")))
    table <- clean_names(table)

    # Datasets in constant currency use "Fiscal Year" instead of year: we
    # change that to harmonize names between datasets
    if ("fiscal_year" %in% colnames(table)) {
        table %<>% rename(year = fiscal_year)
    }

    return(table)
})

# Identify country 2-letter ISO codes ---------------------------------------- #
iso_dict <- read_excel(file.path("primary_data/sna-un", "iso-codes-dict.xlsx"), sheet = "UN_SNA")

un_tables %<>% llply(function(table) {
    table %<>%
        rename(country = country_or_area) %>%
        left_join(iso_dict)

    table %<>% filter(!is.na(iso)) 
    
    # Check that all countries were correctly identified
    if (any(nchar(table$iso) != 2)) {
        stop("not all countries identified")
    }

    table %<>% select(-country)

    return(table)
})

# Identify and correct currencies -------------------------------------------- #
un_tables %<>% lapply(function(table) {
    # Harmonize currency names
    table %<>% mutate(currency = trimws(tolower(currency)), currency_iso = "")

    # One item correspond to a population, yet has a currency associated to it.
    # We remove it so that it won't be converted
    table %<>% mutate(
        currency = if_else(item == "Employment (average, in 1000 persons)", "", currency)
    )

    # First correct for currency changes
    table %<>%
        mutate(currency_iso = if_else((iso == "AD") & (currency == "euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AE") & (currency == "uae dirham"), "AED", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "AF") & (currency == "afghani"), "AFN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AF") & (currency == "afghanis"), "AFN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AF") & (currency == "new afghanis"), "AFN", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "AG") & (currency == "ec dollar"), "XCD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AI") & (currency == "ec dollar"), "XCD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AL") & (currency == "lek"), "ALL", currency_iso)) %>%

        mutate(value = if_else((iso == "AM") & (currency == "russian ruble"), value/200, value)) %>%
        mutate(currency_iso = if_else((iso == "AM") & (currency == "russian ruble"), "AMD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AM") & (currency == "dram"), "AMD", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "AN") & (currency == "netherlands antillean guilder"), "ANG", currency_iso)) %>%

        mutate(value = if_else((iso == "AO") & (currency == "readjusted kwanza"), value/1000, value)) %>%
        mutate(currency_iso = if_else((iso == "AO") & (currency == "readjusted kwanza"), "AOA", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AO") & (currency == "(second) kwanza"), "AOA", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "AR") & (currency == "argentine peso"), "ARS", currency_iso)) %>%

        mutate(value = if_else((iso == "AT") & (currency == "austrian schilling"), value/13.7603, value)) %>%
        mutate(currency_iso = if_else((iso == "AT") & (currency == "austrian schilling"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AT") & (currency == "1999 ats euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AT") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "AU") & (currency == "australian dollar"), "AUD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AW") & (currency == "aruban florin"), "AWG", currency_iso)) %>%

        mutate(value = if_else((iso == "AZ") & (currency == "azerbaijan manat"), value/5000, value)) %>%
        mutate(currency_iso = if_else((iso == "AZ") & (currency == "azerbaijan manat"), "AZN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "AZ") & (currency == "azerbaijan new manat"), "AZN", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "BA") & (currency == "convertible marks"), "BAM", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BB") & (currency == "barbados dollar"), "BBD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BD") & (currency == "taka"), "BDT", currency_iso)) %>%

        mutate(value = if_else((iso == "BE") & (currency == "belgian franc"), value/40.3399, value)) %>%
        mutate(currency_iso = if_else((iso == "BE") & (currency == "belgian franc"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BE") & (currency == "1999 bef euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BE") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "BF") & (currency == "cfa franc"), "XOF", currency_iso)) %>%

        mutate(value = if_else((iso == "BG") & (currency == "lev") & (series %in% c(10, 100)), value/1000, value)) %>%
        mutate(currency_iso = if_else((iso == "BG") & (currency == "lev (re-denom. 1:1000)"), "BGN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BG") & (currency == "lev"), "BGN", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "BH") & (currency == "bahrain dinar"), "BHD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BI") & (currency == "burundi franc"), "BIF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BJ") & (currency == "cfa franc"), "XOF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BM") & (currency == "bermuda dollar"), "BMD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BN") & (currency == "brunei dollar"), "BND", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BO") & (currency == "boliviano"), "BOB", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BQ") & (currency == "us dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BR") & (currency == "real"), "BRL", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BS") & (currency == "bahamian dollar"), "BSD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BT") & (currency == "ngultrum"), "BTN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BT") & (currency == "ngultum"), "BTN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BW") & (currency == "pula"), "BWP", currency_iso)) %>%

        mutate(value = if_else((iso == "BY") & (currency == "russian rouble"), value/(10*1000*10000), value)) %>%
        mutate(value = if_else((iso == "BY") & (currency == "belarussian rouble"), value/(1000*10000), value)) %>%
        mutate(value = if_else((iso == "BY") & (currency == "belarussian rouble (re-denom. 1:1000)"), value/10000, value)) %>%
        mutate(currency_iso = if_else((iso == "BY") & (currency == "russian rouble"), "BYN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BY") & (currency == "belarussian rouble"), "BYN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BY") & (currency == "belarussian rouble (re-denom. 1:1000)"), "BYN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "BY") & (currency == "belarussian rouble (re-denom. 1:10000)"), "BYN", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "BZ") & (currency == "belize dollar"), "BZD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CA") & (currency == "canadian dollar"), "CAD", currency_iso)) %>%

        mutate(value = if_else((iso == "CD") & (currency == "new zaire"), value/100000, value)) %>%
        mutate(currency_iso = if_else((iso == "CD") & (currency == "new zaire"), "CDF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CD") & (currency == "congolese franc"), "CDF", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "CF") & (currency == "cfa franc"), "XAF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CG") & (currency == "cfa franc"), "XAF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CH") & (currency == "swiss franc"), "CHF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CI") & (currency == "cfa franc"), "XOF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CK") & (currency == "new zealand dollars"), "NZD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CK") & (currency == "new zealander dollars"), "NZD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CL") & (currency == "chilean peso"), "CLP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CM") & (currency == "cfa franc"), "XAF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CN") & (currency == "yuan renminbi"), "CNY", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CO") & (currency == "colombian peso"), "COP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CR") & (currency == "costa rican colon"), "CRC", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CU") & (currency == "cuban peso"), "CUP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CV") & (currency == "escudo"), "CVE", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CW") & (currency == "netherlands antillean guilder"), "ANG", currency_iso)) %>%

        mutate(value = if_else((iso == "CY") & (currency == "cyprus pound"), value/0.585274, value)) %>%
        mutate(currency_iso = if_else((iso == "CY") & (currency == "cyprus pound"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "CY") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "CZ") & (currency == "czech koruna"), "CZK", currency_iso)) %>%

        mutate(value = if_else((iso == "DB") & (currency == "deutsche mark"), value/1.95583, value)) %>%
        mutate(currency_iso = if_else((iso == "DB") & (currency == "deutsche mark"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "DB") & (currency == "1999 dem euro / euro"), "EUR", currency_iso)) %>%

        mutate(value = if_else((iso == "DE") & (currency == "deutsche mark"), value/1.95583, value)) %>%
        mutate(currency_iso = if_else((iso == "DE") & (currency == "deutsche mark"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "DE") & (currency == "1999 dem euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "DE") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "DJ") & (currency == "djibouti franc"), "DJF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "DK") & (currency == "danish krone"), "DKK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "DM") & (currency == "ec dollar"), "XCD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "DO") & (currency == "dominican peso"), "DOP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "DZ") & (currency == "algerian dinar"), "DZD", currency_iso)) %>%

        mutate(value = if_else((iso == "EC") & (currency == "sucre"), value/25000, value)) %>%
        mutate(currency_iso = if_else((iso == "EC") & (currency == "sucre"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "EC") & (currency == "us dollar"), "USD", currency_iso)) %>%

        mutate(value = if_else((iso == "EE") & (currency == "estonian kroon"), value/15.6466, value)) %>%
        mutate(currency_iso = if_else((iso == "EE") & (currency == "estonian kroon"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "EE") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "EG") & (currency == "egyptian pound"), "EGP", currency_iso)) %>%

        mutate(value = if_else((iso == "ES") & (currency == "peseta"), value/166.386, value)) %>%
        mutate(currency_iso = if_else((iso == "ES") & (currency == "peseta"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ES") & (currency == "1999 esp euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ES") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "ET") & (currency == "ethiopian birr"), "ETB", currency_iso)) %>%

        mutate(value = if_else((iso == "FI") & (currency == "finish markka"), value/5.94573, value)) %>%
        mutate(currency_iso = if_else((iso == "FI") & (currency == "finish markka"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "FI") & (currency == "1999 fim euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "FI") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "FJ") & (currency == "fiji dollar"), "FJD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "FM") & (currency == "us dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "FO") & (currency == "danish krone"), "DKK", currency_iso)) %>%

        mutate(value = if_else((iso == "FR") & (currency == "french franc"), value/6.55957, value)) %>%
        mutate(currency_iso = if_else((iso == "FR") & (currency == "french franc"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "FR") & (currency == "1999 frf euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "FR") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "GA") & (currency == "cfa franc"), "XAF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GB") & (currency == "pound sterling"), "GBP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GD") & (currency == "ec dollar"), "XCD", currency_iso)) %>%

        mutate(value = if_else((iso == "GE") & (currency == "georgia coupon currency"), value/1e6, value)) %>%
        mutate(value = if_else((iso == "GE") & (currency == "russian ruble"), value/1e6, value)) %>%
        mutate(currency_iso = if_else((iso == "GE") & (currency == "georgia coupon currency"), "GEL", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GE") & (currency == "russian ruble"), "GEL", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GE") & (currency == "lari"), "GEL", currency_iso)) %>%

        mutate(value = if_else((iso == "GF") & (currency == "french franc"), value/6.55957, value)) %>%
        mutate(currency_iso = if_else((iso == "GF") & (currency == "french franc"), "EUR", currency_iso)) %>%

        mutate(value = if_else((iso == "GH") & (currency == "cedi"), value/1.2e4, value)) %>%
        mutate(value = if_else((iso == "GH") & (currency == "cedi (second)"), value/1e4, value)) %>%
        mutate(currency_iso = if_else((iso == "GH") & (currency == "cedi"), "GHS", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GH") & (currency == "cedi (second)"), "GHS", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GH") & (currency == "cedi (third)"), "GHS", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "GL") & (currency == "danish krone (ddk.)"), "DKK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GL") & (currency == "danish krone"), "DKK", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "GM") & (currency == "dalasi"), "GMD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GN") & (currency == "guinean franc"), "GNF", currency_iso)) %>%

        mutate(value = if_else((iso == "GP") & (currency == "french franc"), value/6.55957, value)) %>%
        mutate(currency_iso = if_else((iso == "GP") & (currency == "french franc"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "GQ") & (currency == "cfa franc"), "XAF", currency_iso)) %>%

        mutate(value = if_else((iso == "GR") & (currency == "drachma"), value/340.75, value)) %>%
        mutate(currency_iso = if_else((iso == "GR") & (currency == "drachma"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GR") & (currency == "2001 grd euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GR") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "GT") & (currency == "quetzal"), "GTQ", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GU") & (currency == "us dollar"), "USD", currency_iso)) %>%

        mutate(value = if_else((iso == "GW") & (currency == "guinea-bissau peso"), value/65, value)) %>%
        mutate(currency_iso = if_else((iso == "GW") & (currency == "guinea-bissau peso"), "XOF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "GW") & (currency == "cfa franc"), "XOF", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "GY") & (currency == "guyana dollar"), "GYD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "HK") & (currency == "hong kong dollar"), "HKD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "HN") & (currency == "lempira"), "HNL", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "HR") & (currency == "kuna"), "HRK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "HT") & (currency == "gourde"), "HTG", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "HU") & (currency == "forint"), "HUF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ID") & (currency == "indonesian rupiah"), "IDR", currency_iso)) %>%

        mutate(value = if_else((iso == "IE") & (currency == "irish pound"), value/0.787564, value)) %>%
        mutate(currency_iso = if_else((iso == "IE") & (currency == "irish pound"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "IE") & (currency == "1999 iep euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "IE") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "IL") & (currency == "new sheqel"), "ILS", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "IN") & (currency == "indian rupee"), "INR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "IQ") & (currency == "iraqi dinar"), "IQD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "IR") & (currency == "iranian rial"), "IRR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "IS") & (currency == "icelandic kr??na"), "ISK", currency_iso)) %>%

        mutate(value = if_else((iso == "IT") & (currency == "italian lira"), value/1936.27, value)) %>%
        mutate(currency_iso = if_else((iso == "IT") & (currency == "italian lira"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "IT") & (currency == "1999 itl euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "IT") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "JM") & (currency == "jamaican dollar"), "JMD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "JO") & (currency == "jordan dinar"), "JOD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "JP") & (currency == "yen"), "JPY", currency_iso)) %>%

        mutate(value = if_else((iso == "KE") & (currency == "kenya pound"), value*20, value)) %>%
        mutate(currency_iso = if_else((iso == "KE") & (currency == "kenya pound"), "KES", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KE") & (currency == "kenya shillings"), "KES", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "KG") & (currency == "kyrgyz som"), "KGS", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KH") & (currency == "riel"), "KHR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KI") & (currency == "australian dollars"), "AUD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KM") & (currency == "comorian franc"), "KMF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KN") & (currency == "ec dollar"), "XCD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KR") & (currency == "korean won"), "KRW", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KS") & (currency == "euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KW") & (currency == "kuwaiti dinar"), "KWD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KY") & (currency == "cayman islands dollar"), "KYD", currency_iso)) %>%

        mutate(value = if_else((iso == "KZ") & (currency == "russian ruble"), value/500, value)) %>%
        mutate(currency_iso = if_else((iso == "KZ") & (currency == "russian ruble"), "KZT", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "KZ") & (currency == "tenge"), "KZT", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "LA") & (currency == "kip"), "LAK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LB") & (currency == "lebanese pound"), "LBP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LC") & (currency == "ec dollar"), "XCD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LI") & (currency == "swiss franc"), "CHF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LK") & (currency == "sri lanka rupee"), "LKR", currency_iso)) %>%

        mutate(value = if_else((iso == "LR") & (currency == "liberian dollar") & (year <= 1973), value/1.57, value)) %>%
        mutate(value = if_else((iso == "LR") & (currency == "liberian dollar") & (year >= 1974), value/1.20, value)) %>%
        mutate(currency_iso = if_else((iso == "LR") & (currency == "liberian dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LR") & (currency == "us dollar"), "USD", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "LS") & (currency == "loti"), "LSL", currency_iso)) %>%

        mutate(value = if_else((iso == "LT") & (currency == "litas"), value/3.4528, value)) %>%
        mutate(currency_iso = if_else((iso == "LT") & (currency == "litas"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LT") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(value = if_else((iso == "LU") & (currency == "luxembourg franc"), value/40.3399, value)) %>%
        mutate(currency_iso = if_else((iso == "LU") & (currency == "luxembourg franc"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LU") & (currency == "1999 luf euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LU") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(value = if_else((iso == "LV") & (currency == "lats"), value/0.702804, value)) %>%
        mutate(currency_iso = if_else((iso == "LV") & (currency == "lats"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "LV") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "LY") & (currency == "libyan dinar"), "LYD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MA") & (currency == "moroccan dirham"), "MAD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MC") & (currency == "euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MD") & (currency == "moldovan leu"), "MDL", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ME") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(value = if_else((iso == "MG") & (currency == "malagasy franc"), value/5, value)) %>%
        mutate(currency_iso = if_else((iso == "MG") & (currency == "malagasy franc"), "MGA", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MG") & (currency == "ariary"), "MGA", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "MH") & (currency == "us dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MK") & (currency == "tfyr macedonian denar"), "MKD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ML") & (currency == "cfa franc"), "XOF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MM") & (currency == "kyat"), "MMK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MN") & (currency == "togrog"), "MNT", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MO") & (currency == "pataca"), "MOP", currency_iso)) %>%

        mutate(value = if_else((iso == "MQ") & (currency == "french franc"), value/6.55957, value)) %>%
        mutate(currency_iso = if_else((iso == "MQ") & (currency == "french franc"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "MR") & (currency == "ouguiyas"), "MRO", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MR") & (currency == "ouguiya"), "MRO", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MS") & (currency == "ec dollar"), "XCD", currency_iso)) %>%

        mutate(value = if_else((iso == "MT") & (currency == "maltese liri"), value/0.4293, value)) %>%
        mutate(currency_iso = if_else((iso == "MT") & (currency == "maltese liri"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MT") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "MU") & (currency == "mauritian rupee"), "MUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MV") & (currency == "rufiyaa"), "MVR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MW") & (currency == "malawi kwacha"), "MWK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MX") & (currency == "mexican new peso"), "MXN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MY") & (currency == "malayan dollar"), "MYR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MY") & (currency == "ringgit"), "MYR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "MZ") & (currency == "metical"), "MZN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NA") & (currency == "namibia dollar"), "NAD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NC") & (currency == "cfp franc"), "XPF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NE") & (currency == "cfa franc"), "XOF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NG") & (currency == "naira"), "NGN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NI") & (currency == "c??rdoba"), "NIO", currency_iso)) %>%

        mutate(value = if_else((iso == "NL") & (currency == "netherlands guilder"), value/2.20371, value)) %>%
        mutate(currency_iso = if_else((iso == "NL") & (currency == "netherlands guilder"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NL") & (currency == "1999 nlg euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NL") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "NO") & (currency == "norwegian krone"), "NOK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NP") & (currency == "nepalese rupee"), "NPR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NR") & (currency == "australian dollar"), "AUD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NU") & (currency == "new zealand dollar"), "NZD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "NZ") & (currency == "new zealand dollar"), "NZD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "OM") & (currency == "rial omani"), "OMR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PA") & (currency == "balboa"), "PAB", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PE") & (currency == "new sol"), "PEN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PF") & (currency == "cfp franc"), "XPF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PG") & (currency == "kina"), "PGK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PH") & (currency == "philippine peso"), "PHP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PK") & (currency == "pakistan rupee"), "PKR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PL") & (currency == "zloty"), "PLN", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PR") & (currency == "us dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PS") & (currency == "us dollar"), "USD", currency_iso)) %>%

        mutate(value = if_else((iso == "PT") & (currency == "portuguese escudo"), value/200.482, value)) %>%
        mutate(currency_iso = if_else((iso == "PT") & (currency == "portuguese escudo"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PT") & (currency == "1999 pte euro / euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PT") & (currency == "euro"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "PW") & (currency == "us dollar"), "USD", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "PY") & (currency == "guarani"), "PYG", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "QA") & (currency == "qatar riyal"), "QAR", currency_iso)) %>%

        mutate(value = if_else((iso == "RE") & (currency == "french franc"), value/6.55957, value)) %>%
        mutate(currency_iso = if_else((iso == "RE") & (currency == "french franc"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "RM") & (currency == "yugoslav dinar"), "YUM", currency_iso)) %>%

        mutate(value = if_else((iso == "RO") & (currency == "romanian leu"), value/1e4, value)) %>%
        mutate(currency_iso = if_else((iso == "RO") & (currency == "romanian leu"), "RON", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "RO") & (currency == "new romanian leu"), "RON", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "RS") & (currency == "dinar"), "RSD", currency_iso)) %>%

        mutate(value = if_else((iso == "RU") & (currency == "russian ruble"), value/1e3, value)) %>%
        mutate(currency_iso = if_else((iso == "RU") & (currency == "russian ruble"), "RUB", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "RU") & (currency == "russian ruble (re-denom. 1:1000)"), "RUB", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "RW") & (currency == "rwanda franc"), "RWF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SA") & (currency == "saudi arabian riyal"), "SAR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SB") & (currency == "solomon islands dollar"), "SBD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SB") & (currency == "solomon island dollar"), "SBD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SC") & (currency == "seychelles rupee"), "SCR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SD") & (currency == "sudanese pound"), "SDG", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SE") & (currency == "swedish krona"), "SEK", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SG") & (currency == "singapore dollar"), "SGD", currency_iso)) %>%

        mutate(value = if_else((iso == "SI") & (currency == "tolar"), value/239.64, value)) %>%
        mutate(value = if_else((iso == "SI") & (currency == "slovenian tolar"), value/239.64, value)) %>%
        mutate(currency_iso = if_else((iso == "SI") & (currency == "tolar"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SI") & (currency == "slovenian tolar"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SI") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(value = if_else((iso == "SK") & (currency == "slovak koruna"), value/30.126, value)) %>%
        mutate(currency_iso = if_else((iso == "SK") & (currency == "slovak koruna"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SK") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "SL") & (currency == "leone"), "SLL", currency_iso)) %>%

        mutate(value = if_else((iso == "SM") & (currency == "lira"), value/1936.27, value)) %>%
        mutate(currency_iso = if_else((iso == "SM") & (currency == "lira"), "EUR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SM") & (currency == "euro"), "EUR", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "SN") & (currency == "cfa franc"), "XOF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SO") & (currency == "somali shilling"), "SOS", currency_iso)) %>%

        mutate(value = if_else((iso == "SR") & (currency == "suriname guilder"), value/1e3, value)) %>%
        mutate(currency_iso = if_else((iso == "SR") & (currency == "suriname guilder"), "SRD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SR") & (currency == "suriname dollar (srd)"), "SRD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SR") & (currency == "suriname dollar"), "SRD", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "SS") & (currency == "south sudanese pound"), "SSP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ST") & (currency == "dobra"), "STD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SU") & (currency == "roubles"), "SUR", currency_iso)) %>%

        mutate(value = if_else((iso == "SV") & (currency == "el salvadoran colon"), value/8.75, value)) %>%
        mutate(currency_iso = if_else((iso == "SV") & (currency == "el salvadoran colon"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SV") & (currency == "us dollars"), "USD", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "SX") & (currency == "netherlands antillean guilder (ang)"), "ANG", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SY") & (currency == "syrian pound"), "SYP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "SZ") & (currency == "lilangeni"), "SZL", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TC") & (currency == "us dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TD") & (currency == "cfa franc"), "XAF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TG") & (currency == "cfa franc"), "XOF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TH") & (currency == "baht"), "THB", currency_iso)) %>%

        mutate(value = if_else((iso == "TJ") & (currency == "russian ruble"), value/1000/100, value)) %>%
        mutate(value = if_else((iso == "TJ") & (currency == "tajik ruble"), value/1000, value)) %>%
        mutate(currency_iso = if_else((iso == "TJ") & (currency == "russian ruble"), "TJS", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TJ") & (currency == "tajik ruble"), "TJS", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TJ") & (currency == "samoni"), "TJS", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "TL") & (currency == "us dollars"), "USD", currency_iso)) %>%
        # All the information in indonesian rupiah for Timor-Leste is redundant
        filter(!((iso == "TL") & (currency == "indonesian rupiah"))) %>%

        mutate(value = if_else((iso == "TM") & (currency == "russian ruble"), value/500, value)) %>%
        mutate(currency_iso = if_else((iso == "TM") & (currency == "russian ruble"), "TMT", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TM") & (currency == "turkmen manat"), "TMT", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "TN") & (currency == "tunisian dinar"), "TND", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TO") & (currency == "pa 'anga"), "TOP", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TO") & (currency == "pa'anga"), "TOP", currency_iso)) %>%

        mutate(value = if_else((iso == "TR") & (currency == "turkish lira"), value/1e6, value)) %>%
        mutate(currency_iso = if_else((iso == "TR") & (currency == "turkish lira"), "TRY", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TR") & (currency == "new turkish lira"), "TRY", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "TT") & (currency == "trinidad and tobago dollar"), "TTD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TV") & (currency == "australian dollar"), "AUD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TZ") & (currency == "tanzania shilling"), "TZS", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "TZ") & (currency == "tanzanian shilling"), "TZS", currency_iso)) %>%

        mutate(value = if_else((iso == "UA") & (currency == "karbovantsy"), value/1e5, value)) %>%
        mutate(currency_iso = if_else((iso == "UA") & (currency == "karbovantsy"), "UAH", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "UA") & (currency == "hryvnia"), "UAH", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "UG") & (currency == "uganda shilling"), "UGX", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "US") & (currency == "us dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "UY") & (currency == "uruguayan peso"), "UYU", currency_iso)) %>%

        mutate(value = if_else((iso == "UZ") & (currency == "russian ruble"), value/1e3, value)) %>%
        mutate(currency_iso = if_else((iso == "UZ") & (currency == "russian ruble"), "UZS", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "UZ") & (currency == "uzbek sum"), "UZS", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "VC") & (currency == "ec dollar"), "XCD", currency_iso)) %>%

        mutate(value = if_else((iso == "VE") & (currency == "bolivar"), value/1000, value)) %>%
        mutate(currency_iso = if_else((iso == "VE") & (currency == "bolivar"), "VEF", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "VE") & (currency == "bolivar fuerte"), "VEF", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "VG") & (currency == "us dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "VN") & (currency == "dong"), "VND", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "VU") & (currency == "vatu"), "VUV", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "WS") & (currency == "tala"), "WST", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "XE") & (currency == "ethiopian birr"), "ETB", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "XI") & (currency == "indonesian rupiah"), "IDR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "XS") & (currency == "sudanese pound"), "SDG", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "XS") & (currency == "sudanese pounds"), "SDG", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "XS") & (currency == "sdg"), "SDG", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "YA") & (currency == "yemeni rial"), "YER", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "YD") & (currency == "dinars"), "YDD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "YE") & (currency == "yemeni rial"), "YER", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "YU") & (currency == "yugoslavian dinar"), "YUM", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ZA") & (currency == "rand"), "ZAR", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ZM") & (currency == "zambia kwacha"), "ZMW", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ZM") & (currency == "zambian kwacha"), "ZMW", currency_iso)) %>%

        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1970), value/0.533495400962600, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1971), value/0.531933967745777, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1972), value/0.491330095605119, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1973), value/0.437185422321498, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1974), value/0.435376529903476, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1975), value/0.425816296116076, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1976), value/0.467082148889144, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1977), value/0.469310380421387, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1978), value/0.503449626597452, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1979), value/0.507682021724636, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1980), value/0.481416275342180, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1981), value/0.515499504531715, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1982), value/0.566954226213844, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1983), value/0.756665114811631, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1984), value/0.939224500568787, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1985), value/1.205249208350040, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1986), value/1.244759862078940, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1987), value/1.241019171823400, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1988), value/1.348652714921590, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1989), value/1.582617029302130, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1990), value/1.831183106867370, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1991), value/2.704780586521060, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1992), value/3.808016623885500, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1993), value/4.842205864803760, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1994), value/6.088329865225110, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1995), value/6.495270915805400, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1996), value/7.470688690341000, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1997), value/9.045840085571470, value)) %>%
        mutate(value = if_else((iso == "ZW") & (currency == "zimbabwe dollar") & (year == 1998), value/17.68577131962360, value)) %>%
        filter(!((iso == "ZW") & (currency == "zimbabwe dollar") & (year >= 1999))) %>%
        mutate(currency_iso = if_else((iso == "ZW") & (currency == "zimbabwe dollar"), "USD", currency_iso)) %>%
        mutate(currency_iso = if_else((iso == "ZW") & (currency == "usd"), "USD", currency_iso)) %>%

        mutate(currency_iso = if_else((iso == "ZZ") & (currency == "tanzania shilling"), "TZS", currency_iso))

    # Check that all currencies were properly identified
    if (anyNA(table$currency_iso) || any(nchar(table$currency_iso) != 3 & table$currency != "")) {
      # stop("not all currencies identified")
    }

    # Remove old currency variable (not necessary anymore)
    table %<>% select(-currency)

    return(table)
})

# Give proper column names to the tables ------------------------------------- #
names(un_tables) <- table_codes

for (i in seq_along(un_tables)) {
    name <- table_names[i]
    write_dta(un_tables[[i]], paste0("primary_data/sna-un/_clean/", name, ".dta"))
}

remove(iso_dict)
