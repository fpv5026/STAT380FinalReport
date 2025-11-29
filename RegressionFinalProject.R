library(tidyverse)
library(lubridate)
library(dplyr)

#load data
cpi <- read.csv("cpi_cleaned.csv")
fed <- read.csv("fed_rate_cleaned.csv")
unemp <- read.csv('unemployment_cleaned.csv')
spy <- read.csv("SPY_data.csv")

#data formatting
cpi$Date   <- as.Date(as.character(cpi$Date),   format = "%b %d, %Y")
unemp$Date <- as.Date(as.character(unemp$Date), format = "%b %d, %Y")
fed$Date   <- as.Date(as.character(fed$Date),   format = "%b %d, %Y")
spy$X <- as.Date(as.character(spy$X))

#2008-2015
cpi   <- cpi   %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
unemp <- unemp %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
fed   <- fed   %>% filter(Date >= "2008-01-01", Date <= "2015-12-31")
spy   <- spy   %>% filter(X >= "2008-01-01", X <= "2015-12-31")

#monthly changes
cpi$month   <- floor_date(cpi$Date, "month")
unemp$month <- floor_date(unemp$Date, "month")
fed$month   <- floor_date(fed$Date, "month")
spy$month   <- floor_date(spy$X, "month")

#economic surprises 
cpi$cpi_surprise       <- cpi$Actual - cpi$Previous
unemp$unemp_surprise   <- unemp$Actual - unemp$Previous
fed$fed_surprise       <- fed$Actual - fed$Previous

#daily returns for SPY
spy <- spy %>%
  mutate(return = log(SPY.Adjusted / lag(SPY.Adjusted)))

#monthly volatilty(standard deviation of daily returns)
spy_monthly <- spy %>%
  group_by(month) %>%
  summarise(volatility = sd(return, na.rm = TRUE))

#merged data 
merged_data <- cpi[, c("month", "cpi_surprise")] %>%
  merge(unemp[, c("month", "unemp_surprise")], by = "month") %>%
  merge(fed[, c("month", "fed_surprise")], by = "month") %>%
  merge(spy_monthly, by = "month")

#high volitility vs. low
threshold <- median(merged_data$volatility, na.rm = TRUE)

merged_data$high_vol <- ifelse(merged_data$volatility > threshold, 1, 0)

table(merged_data$high_vol)

#logistical regression
model <- glm(high_vol ~ cpi_surprise + unemp_surprise + fed_surprise,
             data = merged_data,
             family = binomial)

print(summary(model))

#Confusion Matrix
merged_data$pred_prob <- predict(model, type = "response")
merged_data$pred_class <- ifelse(merged_data$pred_prob > 0.5, 1, 0)

library(caret)

confusionMatrix(
  factor(merged_data$pred_class),
  factor(merged_data$high_vol),
  positive = "1"
)

#ROC and AUC
library(pROC)

roc_obj <- roc(merged_data$high_vol, merged_data$pred_prob)
plot(roc_obj)
auc(roc_obj)

# Best threshold
coords(roc_obj, "best", ret = "threshold")

# Area under Curve
print(auc(roc_obj))


# Interpretation
coef_table <- summary(model)$coefficients
print(coef_table)

# Convert log-odds to odds ratios for easier interpretation
odds_ratios <- exp(coef(model))
print(odds_ratios)

# Create a summary interpretation table
interpretation <- data.frame(
  Variable = names(odds_ratios),
  Odds_Ratio = round(odds_ratios, 3),
  Meaning = c(
    "Baseline odds of high volatility when all surprises = 0",
    "Effect of 1 unit increase in CPI surprise",
    "Effect of 1 unit increase in Unemployment surprise",
    "Effect of 1 unit increase in Fed funds rate surprise"
  )
)

print(interpretation)
print("When unemployment comes as a suprise bigger than expected, that same month is more likely to be high-volatility.")
