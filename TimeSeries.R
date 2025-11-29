# required packages
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

# 2008-2015
cpi   <- cpi   %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
unemp <- unemp %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
ffr   <- ffr   %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
spy   <- spy   %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")

# -------------------------
# 2. Compute surprises 
# -------------------------
cpi <- cpi %>% mutate(cpi_surprise = Actual - Previous)
unemp <- unemp %>% mutate(unemp_surprise = Actual - Previous)
ffr <- ffr %>% mutate(ffr_surprise = Actual - Previous)

# compute spy_vol daily returns
spy <- spy %>%
  arrange(Date) %>%
  mutate(ret = log(SPY.Adjusted / lag(SPY.Adjusted)),
         spy_vol = abs(ret))

# -------------------------
# 2. Merged Dataset
# -------------------------

# Remove this to compare SAME DAY IMPACT
cpi <- cpi %>% mutate(cpi_surprise_lead = lag(cpi_surprise))
unemp <- unemp %>% mutate(unemp_surprise_lead = lag(unemp_surprise))
ffr <- ffr %>% mutate(ffr_surprise_lead = lag(ffr_surprise))

# Merge SPY and surprises
data <- spy %>%
  left_join(cpi   %>% select(Date, cpi_surprise_lead),   by = "Date") %>%
  left_join(unemp %>% select(Date, unemp_surprise_lead), by = "Date") %>%
  left_join(ffr   %>% select(Date, ffr_surprise_lead),   by = "Date") %>%
  replace_na(list(
    cpi_surprise_lead   = 0,
    unemp_surprise_lead = 0,
    ffr_surprise_lead   = 0
  ))
# Remove rows with NA volatility
data <- data %>% filter(!is.na(spy_vol))
#xreg_numeric <- data %>% select(cpi_surprise, unemp_surprise, ffr_surprise)
xreg_numeric <- data %>%
  select(cpi_surprise_lead, unemp_surprise_lead, ffr_surprise_lead)

# -------------------------
# 3. Time Series
# -------------------------

# ---------- Train / Test Split ----------
split <- floor(0.7 * nrow(data))

train_vol <- data$spy_vol[1:split]
test_vol  <- data$spy_vol[(split+1):nrow(data)]

train_x <- as.matrix(xreg_numeric[1:split, ])
test_x  <- as.matrix(xreg_numeric[(split+1):nrow(data), ])

cat("TRAIN:", length(train_vol), "obs   TEST:", length(test_vol), "obs\n")

# ---------- ARIMAX ----------
spy_vol_ts_train <- ts(train_vol, frequency = 252)

arimax_model <- auto.arima(spy_vol_ts_train, xreg = train_x)
print(arimax_model)

# Forecast on test period
arimax_fc <- forecast(arimax_model, xreg = test_x, h = length(test_vol))$mean

arimax_rmse <- sqrt(mean((arimax_fc - test_vol)^2))
arimax_mae  <- mean(abs(arimax_fc - test_vol))

cat("\nARIMAX Model Performance")
cat("\nRMSE:", round(arimax_rmse, 6))
cat("\nMAE :", round(arimax_mae,  6))

# ---------- GARCH ----------
spec <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1,1),
    external.regressors = train_x
  ),
  mean.model = list(
    armaOrder = c(0,0),
    include.mean = TRUE,
    external.regressors = train_x
  ),
  distribution.model = "norm"
)

garch_fit <- ugarchfit(spec = spec, data = train_vol)
show(garch_fit)

# Forecast volatility for test period
garch_fc <- ugarchforecast(
  garch_fit,
  n.ahead = length(test_vol),
  external.regressors = test_x
)

# predicted volatility
garch_fc_values <- as.numeric(fitted(garch_fc))

garch_rmse <- sqrt(mean((garch_fc_values - test_vol)^2))
garch_mae  <- mean(abs(garch_fc_values - test_vol))

cat("\nGARCH Model Performance")
cat("\nRMSE:", round(garch_rmse, 6))
cat("\nMAE :", round(garch_mae,  6))



Model_Comparison <- data.frame(
  Model = c("ARIMAX", "GARCH"),
  RMSE  = c(arimax_rmse, garch_rmse),
  MAE   = c(arimax_mae,  garch_mae)
)

cat("\n----------------- Model Comparison -----------------\n")
print(Model_Comparison)
cat("\n(Lower RMSE/MAE indicates better forecasting performance.)\n")

cat("\n----------------- ARIMAX Coefficients -----------------\n")
arimax_coefs <- coef(arimax_model)
sort(arimax_coefs, decreasing = TRUE)
print(arimax_coefs)
print("a more positive coefficient leads to increased daily volatility, more negative coefficeint leads to decreased volatility")

cat("\n----------------- GARCH Coefficients -----------------\n")
garch_coefs <- coef(garch_fit)
print(garch_coefs)
coef(garch_fit)[grep("surprise", names(coef(garch_fit)))]
print("#mxreg1: CPI\n
#mxreg2: unemployment\n
#mxreg3:Fed\n
A positive value is an increase in volatility and a negative value is a decrease")


