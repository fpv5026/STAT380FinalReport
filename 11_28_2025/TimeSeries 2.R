# ===============================
# Time-Series Models: ARIMAX & GARCH
# ===============================

library(dplyr)
library(readr)
library(tidyr)
library(forecast)
library(rugarch)

# -------------------------
# 1. Load Data 
# -------------------------
cpi <- read_csv("cpi_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

unemp <- read_csv("unemployment_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

ffr <- read_csv("fed_rate_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

spy <- read.csv("SPY_data.csv") %>%
  rename(Date = X) %>% 
  mutate(Date = as.Date(Date))

# -------------------------
# 2. Restrict to 2008–2015
# -------------------------
cpi   <- cpi   %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
unemp <- unemp %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
ffr   <- ffr   %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
spy   <- spy   %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")

# -------------------------
# 3. Compute surprises & SPY volatility
# -------------------------
cpi   <- cpi   %>% mutate(cpi_surprise   = Actual - Previous)
unemp <- unemp %>% mutate(unemp_surprise = Actual - Previous)
ffr   <- ffr   %>% mutate(ffr_surprise   = Actual - Previous)

spy <- spy %>%
  arrange(Date) %>%
  mutate(
    ret    = log(SPY.Adjusted / lag(SPY.Adjusted)),
    spy_vol = abs(ret)
  ) %>%
  filter(!is.na(spy_vol))   # drop first NA

# -------------------------
# 4. Merge daily SPY with macro surprises (same-day)
# -------------------------
data <- spy %>%
  left_join(cpi   %>% select(Date, cpi_surprise),   by = "Date") %>%
  left_join(unemp %>% select(Date, unemp_surprise), by = "Date") %>%
  left_join(ffr   %>% select(Date, ffr_surprise),   by = "Date") %>%
  replace_na(list(
    cpi_surprise   = 0,
    unemp_surprise = 0,
    ffr_surprise   = 0
  ))

xreg_numeric <- data %>%
  select(cpi_surprise, unemp_surprise, ffr_surprise)

# -------------------------
# 5. Train / Test Split
# -------------------------
set.seed(380)  # for reproducibility of any internal routines

n <- nrow(data)
split <- floor(0.7 * n)

train_vol <- data$spy_vol[1:split]
test_vol  <- data$spy_vol[(split + 1):n]

train_x <- as.matrix(xreg_numeric[1:split, ])
test_x  <- as.matrix(xreg_numeric[(split + 1):n, ])

cat("TRAIN:", length(train_vol), "obs   TEST:", length(test_vol), "obs\n")

# -------------------------
# 6. ARIMAX model (volatility ~ own lags + macro surprises)
# -------------------------
spy_vol_ts_train <- ts(train_vol, frequency = 252)

arimax_model <- auto.arima(
  spy_vol_ts_train,
  xreg = train_x,
  seasonal = FALSE,      # volatility is not strongly seasonal here
  stepwise = TRUE,
  approximation = FALSE
)

cat("\n================= ARIMAX Model =================\n")
print(arimax_model)

# Forecast on test period
arimax_fc <- forecast(arimax_model, xreg = test_x, h = length(test_vol))$mean

arimax_rmse <- sqrt(mean((arimax_fc - test_vol)^2))
arimax_mae  <- mean(abs(arimax_fc - test_vol))

cat("\nARIMAX Model Performance\n")
cat("RMSE:", round(arimax_rmse, 6), "\n")
cat("MAE :", round(arimax_mae,  6), "\n")

# -------------------------
# 7. GARCH(1,1) model on volatility (no xreg)
# -------------------------
garch_spec <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "norm"
)

garch_fit <- ugarchfit(spec = garch_spec, data = train_vol)

cat("\n================= GARCH(1,1) Fit =================\n")
show(garch_fit)

# Forecast for test period
garch_fc <- ugarchforecast(
  garch_fit,
  n.ahead = length(test_vol)
)

# conditional mean of volatility
garch_fc_values <- as.numeric(fitted(garch_fc))

garch_rmse <- sqrt(mean((garch_fc_values - test_vol)^2))
garch_mae  <- mean(abs(garch_fc_values - test_vol))

cat("\nGARCH Model Performance\n")
cat("RMSE:", round(garch_rmse, 6), "\n")
cat("MAE :", round(garch_mae,  6), "\n")

# -------------------------
# 8. Model Comparison + Coeffs
# -------------------------
Model_Comparison <- data.frame(
  Model = c("ARIMAX", "GARCH(1,1)"),
  RMSE  = c(arimax_rmse, garch_rmse),
  MAE   = c(arimax_mae,  garch_mae)
)

cat("\n----------------- Model Comparison -----------------\n")
print(Model_Comparison)
cat("\n(Lower RMSE/MAE indicates better forecasting performance.)\n")

cat("\n----------------- ARIMAX Coefficients -----------------\n")
arimax_coefs <- coef(arimax_model)
print(arimax_coefs)
cat("\n(Positive coefficients on surprises suggest higher volatility when surprises are positive; negative coefficients suggest the opposite.)\n")

cat("\n----------------- GARCH(1,1) Coefficients -----------------\n")
garch_coefs <- coef(garch_fit)
print(garch_coefs)
cat("\n(Alpha + beta close to 1 indicates strong volatility clustering and persistence.)\n")
