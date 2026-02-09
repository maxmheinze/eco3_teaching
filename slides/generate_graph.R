# Load necessary packages
library(ggplot2)
library(datasets)
library(zoo)  # For yearmon conversion

# Example: Use the AirPassengers dataset for demonstration
data("AirPassengers")

# Convert time to Date format using the zoo package
df <- data.frame(
  Date = as.Date(as.yearmon(time(AirPassengers))),  # Convert to Date format using zoo
  Y = as.numeric(AirPassengers)
)

# Filter data for the year 1952
df <- df[df$Date >= as.Date("1952-01-01") & df$Date <= as.Date("1952-12-01"),]

# Define the specific dates for t=0 (June 1952), t-3 (March 1952), and t+3 (September 1952)
t0_date <- as.Date("1952-06-01")  # t=0 (June 1952)
t_minus_3_date <- as.Date("1952-03-01")  # t-3 (March 1952)
t_plus_3_date <- as.Date("1952-09-01")  # t+3 (September 1952)

# Plot the time series with the original and lagged values
ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Y), color = "black", size = 1.2) +  # Monthly passenger data
  geom_vline(xintercept = t0_date, color = "#74b83d", size = 1.2) +  # t=0 (June 1952) in green
  geom_vline(xintercept = t_minus_3_date, color = "#4072c2", size = 1.2) +  # t-3 (March 1952) in blue
  geom_vline(xintercept = t_plus_3_date, color = "#4072c2", size = 1.2) +  # t+3 (Sept 1952) in blue
  labs(title = "Passenger Numbers and Lagged Values (1952)",
       x = "Time", y = "Number of Passengers") +
  annotate("text", x = t0_date, y = max(df$Y) + 10, label = "t=0 (June)", color = "#74b83d", hjust = -0.1) +
  annotate("text", x = t_minus_3_date, y = max(df$Y) + 10, label = "t-3 (March)", color = "#4072c2", hjust = -0.1) +
  annotate("text", x = t_plus_3_date, y = max(df$Y) + 10, label = "t+3 (Sept)", color = "#4072c2", hjust = -0.1) +
  scale_x_date(labels = scales::date_format("%m-%Y"), breaks = "1 month") +  # Format x-axis for monthly dates
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    legend.position = "bottom"
  )















# Econ time series example: ggplot2::economics (monthly US macro data)
library(ggplot2)

df <- ggplot2::economics

# Keep only observations from year 2000 onward
df <- df[df$date >= as.Date("2010-01-01"), ]

# Choose one series (PCE). Swap to unemploy/psavert/uempmed if you prefer.
df$Y <- df$pce

# 1) Levels plot (untouched time series)
p_level <- ggplot(df, aes(x = date, y = Y)) +
  geom_line(color = "#74b83d", linewidth = 1) +
  labs(
    title = "Personal Consumption Expenditures",
    x = NULL, y = "PCE"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

print(p_level)

# 2) First difference: ΔY_t = Y_t - Y_{t-1}
df_diff <- data.frame(
  date = df$date[-1],
  dY   = diff(df$Y)
)

p_diff <- ggplot(df_diff, aes(x = date, y = dY)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(color = "#4072c2", linewidth = 1) +
  labs(
    title = "First Difference: ΔPCE",
    x = NULL, y = expression(Delta*PCE)
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

print(p_diff)

library(patchwork)
p_level / p_diff






# YEARLY (annual) US Real GDP from FRED + 3-panel plot like your figure
# Uses quantmod (easy, no key)
library(quantmod)
library(mFilter)

# 1) Get real GDP (quarterly) and convert to YEARLY (annual average)
getSymbols("GDPC1", src = "FRED")          # real GDP, chained 2017$, quarterly
gdp_y <- apply.yearly(GDPC1, mean)         # annual average
yr    <- as.numeric(format(index(gdp_y), "%Y"))
y     <- as.numeric(gdp_y)

# 2) Transformations
lgdp <- log(y)

# Yearly growth (log difference)  -- in decimals (matches your y-axis style)
growth <- c(NA, diff(lgdp))

# Detrended (linear trend in logs)
detrended <- resid(lm(lgdp ~ yr))

# HP filter cycle (annual lambda = 100)
hp_cycle <- as.numeric(hpfilter(ts(lgdp, start = min(yr), frequency = 1), freq = 100)$cycle)

# 3) Plot (base R) in one image: growth, detrended, HP filter
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

# 3) Plot (base R) in one image: growth, detrended, HP filter
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

plot(yr, growth, type = "l", col = "#74b83d", xlab = "", ylab = "Yearly growth")
abline(h = 0)

plot(yr, detrended, type = "l", col = "#74b83d", xlab = "", ylab = "Detrended")
abline(h = 0)

plot(yr, hp_cycle, type = "l", col = "#74b83d", xlab = "", ylab = "HP filter")
abline(h = 0)


# GDP (levels) alone — blue line
plot(yr, y, type = "l", col = "#4072c2", xlab = "Year", ylab = "Real GDP",
     main = "US Real GDP over time")








## ACF plots: GDP (levels) vs HP-detrended GDP (cycle)
## (Assumes you already ran the code that creates: yr, y, lgdp, hp_cycle)

# If you want to start in 1995 like your screenshot:
keep <- yr >= 1995
lgdp_use    <- lgdp[keep]
hp_cycle_use <- hp_cycle[keep]

par(mfrow = c(2,1), mar = c(4,4,2,1))

acf(lgdp_use,
    main = "",
    ylab = "ACF, GDP")

acf(hp_cycle_use,
    main = "",
    ylab = "ACF, HP-detrended GDP")



ts_gdp <- ts(lgdp_use, start = min(yr[keep]), frequency = 1)
ts_hp  <- ts(hp_cycle_use, start = min(yr[keep]), frequency = 1)

par(mfrow = c(2,1), mar = c(4,4,2,1))
acf(ts_gdp, main = "ACF, GDP", ylab = "ACF, GDP")
acf(ts_hp,  main = "ACF, HP-detrended GDP", ylab = "ACF, adjusted GDP")





















