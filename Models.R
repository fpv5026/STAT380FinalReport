library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(forecast)

spy <- read_csv("/Users/dhruvnasit/Desktop/380 cleaned dataset/SPY_data.csv")

spy <- spy %>%
  rename(
    Date = `...1`,
    Adj.Close = SPY.Adjusted,
    Close = SPY.Close
  ) %>%
  mutate(
    Date = as.Date(Date),
    ret = log(Adj.Close / lag(Adj.Close))
  ) %>%
  arrange(Date) %>%
  mutate(
    vol_5d = rollapply(ret, width = 5, FUN = sd, fill = NA, align = "right")
  )

head(spy)

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

events_all$Indicator <- factor(events_all$Indicator,
                               levels = c("CPI", "Unemployment", "Fed"))

model1 <- lm(vol_change ~ Diff, data = events_all)
summary(model1)

model2 <- lm(vol_change ~ Diff * Indicator, data = events_all)

summary(model2)``

ggplot(events_all, aes(Diff, vol_change, color = Indicator)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Macro Surprises vs SPY Volatility Change",
    x = "Diff (Actual − Previous)",
    y = "Volatility Change (post − pre)"
  ) +
  theme_minimal()

ggplot(events_all, aes(Indicator, vol_change, fill = Indicator)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Volatility Reaction Across Macro Releases",
    y = "Change in 5-Day Volatility"
  ) +
  theme_minimal()


# Use your computed 5-day rolling volatility
vol_series <- spy$vol_5d

# Remove NA's from the start
vol_series <- vol_series[!is.na(vol_series)]

# Convert to time series
vol_ts <- ts(vol_series)

# Fit ARIMA automatically
arima_fit <- auto.arima(vol_ts)

# View model
summary(arima_fit)

# Forecast next 30 days of volatility
vol_forecast <- forecast(arima_fit, h = 30)
autoplot(vol_forecast) +
  labs(title = "ARIMA Forecast of SPY 5-Day Volatility",
       y = "Forecasted Volatility")


# Extract last 60 days of volatility
last_60 <- tail(vol_ts, 60)

# Create forecast (already done)
# arima_fit <- auto.arima(vol_ts)
# vol_forecast <- forecast(arima_fit, h = 30)

# Combine historical and forecast into one data frame
df_hist <- data.frame(
  Day = 1:60,
  Volatility = as.numeric(last_60),
  Type = "Historical"
)

df_forecast <- data.frame(
  Day = 61:(61+29),
  Volatility = as.numeric(vol_forecast$mean),
  Lo80 = as.numeric(vol_forecast$lower[,1]),
  Hi80 = as.numeric(vol_forecast$upper[,1]),
  Lo95 = as.numeric(vol_forecast$lower[,2]),
  Hi95 = as.numeric(vol_forecast$upper[,2]),
  Type = "Forecast"
)

df_all <- bind_rows(df_hist, df_forecast)

# Plot
ggplot(df_all, aes(x = Day, y = Volatility, color = Type)) +
  geom_line(size = 1.1) +
  geom_ribbon(data = df_forecast,
              aes(ymin = Lo95, ymax = Hi95),
              fill = "blue", alpha = 0.15, color = NA) +
  geom_ribbon(data = df_forecast,
              aes(ymin = Lo80, ymax = Hi80),
              fill = "blue", alpha = 0.25, color = NA) +
  labs(
    title = "Zoomed View: Last 60 Days of SPY Volatility + 30-Day ARIMA Forecast",
    x = "Time (Days)",
    y = "Volatility"
  ) +
  scale_color_manual(values = c("Historical" = "black", "Forecast" = "blue")) +
  theme_minimal()