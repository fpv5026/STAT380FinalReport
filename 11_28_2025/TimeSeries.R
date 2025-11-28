## =========================================================
## 0. Load packages
## =========================================================
library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(ggplot2)
library(dplyr)

## =========================================================
## 1. Read & prepare SPY data
## =========================================================

spy <- read_csv(
  "C:/Users/josep/OneDrive/Documents/GitHub/STAT380FinalReport/SPY_data.csv",
  show_col_types = FALSE
)

spy <- spy %>%
  rename(
    Date      = `...1`,        # edit if your date col is named differently
    Adj.Close = SPY.Adjusted,
    Close     = SPY.Close
  ) %>%
  mutate(
    Date = as.Date(Date),
    ret  = log(Adj.Close / lag(Adj.Close))
  ) %>%
  arrange(Date) %>%
  mutate(
    vol_5d = rollapply(ret, width = 5, FUN = sd, fill = NA, align = "right")
  )

head(spy)

## =========================================================
## 2. Read macro event data: CPI, Fed, Unemployment
##    (using lubridate::mdy() for US-format dates like 01/15/2023)
## =========================================================

### ---- CPI ----
cpi <- read_csv("cpi_cleaned.csv", show_col_types = FALSE) %>%
  transmute(
    Date      = lubridate::mdy(Date),   # change to ymd() if your file is "2023-01-15"
    Actual    = Actual,
    Previous  = Previous,
    Indicator = "CPI",
    Diff      = Actual - Previous
  )

### ---- Fed ----
fed <- read_csv("fed_rate_cleaned.csv", show_col_types = FALSE) %>%
  transmute(
    Date      = lubridate::mdy(Date),
    Actual    = Actual,
    Previous  = Previous,
    Indicator = "Fed",
    Diff      = Actual - Previous
  )

### ---- Unemployment ----
unemp <- read_csv("unemployment_cleaned.csv", show_col_types = FALSE) %>%
  transmute(
    Date      = lubridate::mdy(Date),
    Actual    = Actual,
    Previous  = Previous,
    Indicator = "Unemployment",
    Diff      = Actual - Previous
  )

## Quick sanity check
head(cpi)
head(fed)
head(unemp)

## =========================================================
## 3. Function to build event-level volatility reactions
## =========================================================

build_events <- function(macro_df, window = 5) {
  macro_df %>%
    filter(!is.na(Actual), !is.na(Previous)) %>%
    rowwise() %>%
    mutate(
      pre_vol = mean(
        spy$vol_5d[spy$Date >= (Date - window) & spy$Date < Date],
        na.rm = TRUE
      ),
      post_vol = mean(
        spy$vol_5d[spy$Date >= Date & spy$Date <= (Date + window)],
        na.rm = TRUE
      ),
      vol_change = post_vol - pre_vol
    ) %>%
    ungroup()
}

events_cpi   <- build_events(cpi)
events_fed   <- build_events(fed)
events_unemp <- build_events(unemp)

events_all <- bind_rows(events_cpi, events_fed, events_unemp) %>%
  filter(!is.na(vol_change), !is.na(Diff))

head(events_all)
nrow(events_all)

## =========================================================
## 4. Set factor levels for Indicator
## =========================================================

events_all$Indicator <- factor(
  events_all$Indicator,
  levels = c("CPI", "Unemployment", "Fed")
)

## =========================================================
## 5. Regression models
## =========================================================

# Model 1: Aggregate effect of surprise size
model1 <- lm(vol_change ~ Diff, data = events_all)
summary(model1)

# Model 2: Interaction with indicator type
model2 <- lm(vol_change ~ Diff * Indicator, data = events_all)
summary(model2)

## =========================================================
## 6. Plots: macro surprises vs volatility
## =========================================================

# Scatter + regression line by macro indicator
ggplot(events_all, aes(Diff, vol_change, color = Indicator)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Macro Surprises vs SPY Volatility Change",
    x = "Diff (Actual − Previous)",
    y = "Volatility Change (post − pre)"
  ) +
  theme_minimal()

# Boxplot of volatility reaction by indicator type
ggplot(events_all, aes(Indicator, vol_change, fill = Indicator)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Volatility Reaction Across Macro Releases",
    y = "Change in 5-Day Volatility"
  ) +
  theme_minimal()

## =========================================================
## 7. ARIMA model for 5-day rolling volatility
## =========================================================

# Use 5-day rolling volatility
vol_series <- spy$vol_5d

# Drop initial NAs
vol_series <- vol_series[!is.na(vol_series)]

# Convert to time series
vol_ts <- ts(vol_series)

# Fit ARIMA automatically
arima_fit <- auto.arima(vol_ts)
summary(arima_fit)

# Forecast next 30 days of volatility
vol_forecast <- forecast(arima_fit, h = 30)

# Basic forecast plot
autoplot(vol_forecast) +
  labs(
    title = "ARIMA Forecast of SPY 5-Day Volatility",
    y = "Forecasted Volatility"
  )

## =========================================================
## 8. Zoomed view: last 60 days + 30-day forecast
## =========================================================

# Take last 60 days of realized volatility
last_60 <- tail(vol_ts, 60)

# Historical data frame
df_hist <- data.frame(
  Day = 1:60,
  Volatility = as.numeric(last_60),
  Type = "Historical"
)

# Forecast data frame
df_forecast <- data.frame(
  Day = 61:(61 + 29),
  Volatility = as.numeric(vol_forecast$mean),
  Lo80 = as.numeric(vol_forecast$lower[, 1]),
  Hi80 = as.numeric(vol_forecast$upper[, 1]),
  Lo95 = as.numeric(vol_forecast$lower[, 2]),
  Hi95 = as.numeric(vol_forecast$upper[, 2]),
  Type = "Forecast"
)

# Combined
df_all <- bind_rows(df_hist, df_forecast)

# Plot
ggplot(df_all, aes(x = Day, y = Volatility, color = Type)) +
  geom_line(size = 1.1) +
  geom_ribbon(
    data = df_forecast,
    aes(ymin = Lo95, ymax = Hi95),
    fill = "blue", alpha = 0.15, color = NA
  ) +
  geom_ribbon(
    data = df_forecast,
    aes(ymin = Lo80, ymax = Hi80),
    fill = "blue", alpha = 0.25, color = NA
  ) +
  labs(
    title = "Last 60 Days of SPY Volatility + 30-Day ARIMA Forecast",
    x = "Time (Days)",
    y = "Volatility"
  ) +
  scale_color_manual(values = c("Historical" = "black", "Forecast" = "blue")) +
  theme_minimal()
