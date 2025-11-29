# required packages
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(zoo)
library(sandwich)
library(lmtest)
library(broom)

# -------------------------
# 1. Load Data 
# -------------------------
cpi <- read_csv("cpi_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

unemp <- read_csv("unemployment_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

ffr <- read_csv("fed_rate_cleaned.csv") %>%
  mutate(Date = as.Date(Date, format = "%b %d, %Y"))

spy <- read.csv("C:/Users/josep/OneDrive/Documents/GitHub/STAT380FinalReport/SPY_data.csv") %>%
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
# Helper function:
# For each calendar month, find that month's release 
# Then set dummy=1 on and after the release date but only within that month.
# Surprise is set to the release surprise for the active days, otherwise 0.
# -------------------------
expand_month_limited <- function(daily_dates, releases_df, surprise_col, prefix) {
  # releases_df must have Date and surprise_col
  rel <- releases_df %>%
    select(Date, !!sym(surprise_col)) %>%
    mutate(ym = floor_date(Date, "month"))
  
  # if there are multiple releases in the same month (unlikely for CPI/unemp),
  # choose the first release of the month (min Date). Adjust as needed.
  rel_month <- rel %>%
    group_by(ym) %>%
    summarize(
      release_date = min(Date, na.rm = TRUE),
      # pick the surprise corresponding to that release_date
      release_surprise = (!!sym(surprise_col))[which.min(Date)],
      .groups = "drop"
    )
  
  # daily panel with ym
  df <- tibble(Date = daily_dates) %>%
    mutate(ym = floor_date(Date, "month")) %>%
    left_join(rel_month, by = "ym") %>%
    # dummy = 1 if (a) we have a release in this month AND (b) current day is on/after release_date
    mutate(
      dummy = ifelse(!is.na(release_date) & Date >= release_date & Date <= (ym + months(1) - days(1)), 1, 0),
      surprise_active = ifelse(dummy == 1, release_surprise, 0)
    ) %>%
    select(Date, !!paste0(prefix, "_surprise") := surprise_active, !!paste0(prefix, "_day") := dummy)
  
  return(df)
}

# -------------------------
# 3. Apply to CPI / Unemp / FFR
# -------------------------
daily_dates <- spy$Date

cpi_expanded <- expand_month_limited(
  daily_dates = daily_dates,
  releases_df = cpi,
  surprise_col = "cpi_surprise",
  prefix = "cpi"
)

unemp_expanded <- expand_month_limited(
  daily_dates = daily_dates,
  releases_df = unemp,
  surprise_col = "unemp_surprise",
  prefix = "unemp"
)

ffr_expanded <- expand_month_limited(
  daily_dates = daily_dates,
  releases_df = ffr,
  surprise_col = "ffr_surprise",
  prefix = "ffr"
)

# -------------------------
# 4. Merge into final dataset
# -------------------------
data <- spy %>%
  left_join(cpi_expanded, by = "Date") %>%
  left_join(unemp_expanded, by = "Date") %>%
  left_join(ffr_expanded, by = "Date") %>%
  mutate(
    cpi_surprise = replace_na(cpi_surprise, 0),
    unemp_surprise = replace_na(unemp_surprise, 0),
    ffr_surprise = replace_na(ffr_surprise, 0),
    cpi_day = replace_na(cpi_day, 0),
    unemp_day = replace_na(unemp_day, 0),
    ffr_day = replace_na(ffr_day, 0),
    all_three = ifelse(cpi_day + unemp_day + ffr_day == 3, 1, 0)
  )

# Verify
table(data$cpi_day)   
table(data$unemp_day)
table(data$ffr_day)
head(data %>% filter(cpi_day==1), 10) 

# -------------------------
# 5. Run regression (same model as before)
# -------------------------
model <- lm(
  spy_vol ~ 
    cpi_surprise + unemp_surprise + ffr_surprise +
    cpi_day + unemp_day + ffr_day +
    all_three,
  data = data
)

summary(model)

# robust SES if desired
robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
robust_se

