library(tidyverse)
library(lubridate)
library(gridExtra)

# -----------------------------
# 1. Load datasets
# -----------------------------
cpi  <- read_csv("cpi_cleaned.csv")
fed  <- read_csv("fed_rate_cleaned.csv")
unemp <- read_csv('unemployment_cleaned.csv')

# -----------------------------
# 2. Convert dates + compute Diff
# -----------------------------
process_macro <- function(df, name) {
  df %>%
    mutate(
      Date = as.Date(Date, format = "%b %d, %Y"),
      Diff = Actual - Previous,
      Indicator = name
    )
}

cpi  <- process_macro(cpi, "CPI")
fed  <- process_macro(fed, "Fed")
unemp <- process_macro(unemp, "Unemployment")

macro_all <- bind_rows(cpi, fed, unemp)

# -----------------------------
# 3. Summary statistics
# -----------------------------
summary_stats <- macro_all %>%
  group_by(Indicator) %>%
  summarise(
    n = n(),
    mean_actual = mean(Actual, na.rm = TRUE),
    sd_actual   = sd(Actual, na.rm = TRUE),
    mean_diff   = mean(Diff, na.rm = TRUE),
    sd_diff     = sd(Diff, na.rm = TRUE),
    min_diff    = min(Diff, na.rm = TRUE),
    max_diff    = max(Diff, na.rm = TRUE)
  )

print(summary_stats)

# -----------------------------
# 4. Time-series plots of Diff
# -----------------------------
p_cpi <- ggplot(cpi, aes(Date, Diff)) +
  geom_line(color = "orange") +
  labs(title = "CPI Month-to-Month Change (Diff)",
       y = "Actual - Previous", x = "")

p_fed <- ggplot(fed, aes(Date, Diff)) +
  geom_line(color = "steelblue") +
  labs(title = "Fed Funds Rate Change (Diff)",
       y = "Actual - Previous", x = "")

p_unemp <- ggplot(unemp, aes(Date, Diff)) +
  geom_line(color = "darkgreen") +
  labs(title = "Unemployment Rate Change (Diff)",
       y = "Actual - Previous", x = "")

grid.arrange(p_cpi, p_fed, p_unemp, ncol = 1)

# -----------------------------
# 5. Histogram for Diff by indicator
# -----------------------------
ggplot(macro_all, aes(Diff, fill = Indicator)) +
  geom_histogram(alpha = 0.6, bins = 40) +
  facet_wrap(~Indicator, scales = "free") +
  labs(title = "Distribution of Month-to-Month Changes (Diff)",
       x = "Diff", y = "Count") +
  theme_minimal()

# -----------------------------
# 6. Boxplots to compare volatility of Diff
# -----------------------------
ggplot(macro_all, aes(Indicator, Diff, fill = Indicator)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Comparison of Diff Across Indicators",
       y = "Month-to-Month Change (Diff)", x = "") +
  theme_minimal()

# -----------------------------
# 7. Overlay all 3 indicators’ Diff in one plot
# -----------------------------
ggplot(macro_all, aes(Date, Diff, color = Indicator)) +
  geom_line(alpha = 0.8) +
  labs(title = "Overlay of Diff for CPI, Fed, Unemployment",
       y = "Actual - Previous", x = "") +
  theme_minimal()

# -----------------------------
# 8. Scatter: Actual vs Previous
# -----------------------------
ggplot(macro_all, aes(Previous, Actual, color = Indicator)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Actual vs Previous by Indicator",
       x = "Previous", y = "Actual") +
  theme_minimal()

# -----------------------------
# 9. Missingness check
# -----------------------------
missing_summary <- macro_all %>%
  summarise(
    missing_actual = sum(is.na(Actual)),
    missing_previous = sum(is.na(Previous)),
    missing_diff = sum(is.na(Diff))
  )

print(missing_summary)
