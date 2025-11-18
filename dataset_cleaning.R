library(dplyr)
# Unemployment dataset cleaning
# 1. Convert Actual column from "4.3%" → 4.3 (numeric) 
unemployment_date_actual$Actual <- as.numeric(gsub("%", "", unemployment_date_actual$Actual))

# 2. Create Previous column (lead: next row's Actual) 
unemployment_date_actual$Previous <- c(unemployment_date_actual$Actual[-1], NA)

# 3. Clean Date column (remove "(Sep)" and "07:30") 
unemployment_date_actual$Date <- unemployment_date_actual$Date |>
  gsub("\\([^\\)]*\\)", "", x = _) |>        # remove (Sep), (Aug), etc.
  gsub("\\s*\\d{2}:\\d{2}", "", x = _) |>    # remove times like 07:30, 08:30
  trimws()                                   # remove leftover spaces

# 4. Set first Actual value to NA
unemployment_date_actual$Actual[1] <- NA

# 5. EXPORT CLEANED DATAFRAME TO CSV
write.csv(unemployment_date_actual,
          file = "unemployment_cleaned.csv",
          row.names = FALSE)

# Fed Rate Dataset Cleaning
# 1. Convert Actual column from "4.3%" → 4.3 (numeric)
fed_rate_date_actual$Actual <- as.numeric(gsub("%", "", fed_rate_date_actual$Actual))

# 2. Create Previous column (lead: next row's Actual)
fed_rate_date_actual$Previous <- c(fed_rate_date_actual$Actual[-1], NA)

# 3. Set first Actual value to NA
fed_rate_date_actual$Actual[1] <- NA

# 4. EXPORT CLEANED DATAFRAME TO CSV
write.csv(fed_rate_date_actual,
          file = "fed_rate_cleaned.csv",
          row.names = FALSE)

# Core CPI dataset cleaning
# 1. Convert Actual column from "4.3%" → 4.3 (numeric)
core_cpi_mom_date_actual$Actual <- as.numeric(gsub("%", "", core_cpi_mom_date_actual$Actual))

# 2. Create Previous column (lead: next row's Actual)
core_cpi_mom_date_actual$Previous <- c(core_cpi_mom_date_actual$Actual[-1], NA)

# 3. Set first Actual value to NA
core_cpi_mom_date_actual$Actual[1] <- NA

# 4. EXPORT CLEANED DATAFRAME TO CSV
write.csv(core_cpi_mom_date_actual,
          file = "cpi_cleaned.csv",
          row.names = FALSE)