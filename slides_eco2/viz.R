
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)

stri <- read_csv("data/covid_stringency.csv")
mort <- read_csv("data/excess_mortality.csv")

stri <- stri |>
  dplyr::filter(country_code %in% c("AUT")) |>
  pivot_longer(`01Jan2020`:`20Feb2023`) |>
  rename(ccode = country_code, cname = country_name, date = name, stringency = value) |>
  mutate(date = dmy(date)) |>
  dplyr::select(-region_code, -region_name, -jurisdiction)

mort <- mort |>
  rename(ccode = Code, cname = Entity, date = Day, mortality = p_avg_all_ages) |>
  dplyr::filter(ccode %in% c("AUT"))

dat <- stri |>
  left_join(mort)

p <- plot_ly()
p <- add_lines(
  p, data = dat, x = ~date, y = ~stringency,
  name = "Stringency Index",
  hovertemplate = "Date: %{x|%Y-%m-%d}<br>Stringency: %{y:.1f}<extra></extra>",
  yaxis = "y1",
  connectgaps = TRUE
)
p <- add_lines(
  p, data = dat, x = ~date, y = ~mortality,
  name = "Excess Mortality",
  hovertemplate = "Date: %{x|%Y-%m-%d}<br>Excess mortality: %{y:.1f}<extra></extra>",
  yaxis = "y2",
  connectgaps = TRUE
)
p <- layout(
  p,
  title = "Austria: Stringency vs. Excess Mortality",
  xaxis  = list(title = "Date", rangeslider = list(visible = FALSE)),
  yaxis  = list(title = "Stringency Index", rangemode = "tozero"),
  yaxis2 = list(title = "Excess Mortality", overlaying = "y", side = "right"),
  legend = list(orientation = "h", x = 0, y = -0.1),
  hovermode = "x unified"
)
p