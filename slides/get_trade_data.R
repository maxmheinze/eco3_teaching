library(comtradr)
library(dplyr)

# API key is read from ~/.Renviron
comtradr::set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))

# Parameters
year    <- 2023
hs_code <- "7403"   # refined copper

# Download data
raw <- comtradr::ct_get_data(
  type = "goods",
  frequency = "A",
  commodity_classification = "HS",
  commodity_code = hs_code,
  flow_direction = "Export",
  reporter = "all_countries",
  partner  = "all_countries",
  start_date = as.character(year),
  end_date   = as.character(year),
  tidy_cols = TRUE
)

saveRDS(raw, "trade.RDS")