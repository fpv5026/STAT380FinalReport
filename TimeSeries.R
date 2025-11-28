# required packages
library(dplyr)
library(readr)
library(tidyr)
library(forecast)
library(rugarch)

# -------------------------
# 1. Load Data 
# -------------------------
cpi <- read_csv("/Users/felic/Downloads/cpi_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

unemp <- read_csv("/Users/felic/Downloads/unemployment_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

ffr <- read_csv("/Users/felic/Downloads/fed_rate_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

spy <- read.csv("C:/Users/felic/Downloads/SPY_data.csv") %>%
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

# Merge SPY and surprises
data <- spy %>%
  left_join(cpi %>% select(Date, cpi_surprise), by = "Date") %>%
  left_join(unemp %>% select(Date, unemp_surprise), by = "Date") %>%
  left_join(ffr %>% select(Date, ffr_surprise), by = "Date") %>%
  replace_na(list(cpi_surprise = 0, unemp_surprise = 0, ffr_surprise = 0))

# -------------------------
# 3. Time Series
# -------------------------

# Time series of SPY volatility
spy_vol_ts <- ts(data$spy_vol, frequency = 252)

# ARIMAX with numeric surprises
xreg_numeric <- data %>% select(cpi_surprise, unemp_surprise, ffr_surprise)
arimax_model <- auto.arima(spy_vol_ts, xreg = as.matrix(xreg_numeric))
summary(arimax_model)

# GARCH with numeric surprises
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                        external.regressors = as.matrix(xreg_numeric)), 
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE, 
                    external.regressors = as.matrix(xreg_numeric)),
  distribution.model = "norm"
)
garch_fit <- ugarchfit(spec = spec, data = data$spy_vol)
garch_fit