# ==============================================================================
# Road Crash Analysis - Victoria
# Enhanced version combining VicRoad.ipynb and Question_2.ipynb
# 
# Features:
# - Total crashes analysis and forecasting
# - Fatal+Serious crashes analysis and forecasting
# - Annual summary with rates
# - Trend correlation analysis
# - Detailed forecast tables
# ==============================================================================

# Check R version
R.version.string

# ==============================================================================
# 1. Setup and Data Loading
# ==============================================================================

# Install packages if not already installed
required_packages <- c("dplyr", "lubridate", "readr", "forecast", "ggplot2", "tidyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages)
}

# Load required libraries
library(dplyr)
library(lubridate)
library(readr)
library(forecast)
library(ggplot2)
library(tidyr)

# Create charts directory if it doesn't exist
if (!dir.exists("charts")) {
  dir.create("charts")
  cat("Created 'charts' directory for saving charts\n")
}

# Verify critical libraries are loaded
if (!require("readr", quietly = TRUE)) {
  stop("readr package is required but could not be loaded. Please install it using: install.packages('readr')")
}

# Load accident data
# Update the path to your actual data file location
data_path <- "dataupdate/accident.csv"

# Try to read with readr::read_csv, fallback to read.csv if needed
if (file.exists(data_path)) {
  # Use read_csv from readr package (faster, handles dates better)
  accident <- read_csv(data_path, show_col_types = FALSE)
  cat("Data loaded successfully using readr::read_csv\n")
} else {
  stop(paste("File not found:", data_path, "\nPlease update the 'data_path' variable with the correct path to your CSV file."))
}

# ==============================================================================
# 2. Data Exploration and Validation
# ==============================================================================

cat("Total rows in file:", nrow(accident), "\n")

# Auto-parse ACCIDENT_DATE (handles both YYYY-MM-DD and M/D/YYYY formats)
first5 <- head(accident$ACCIDENT_DATE, 5)
is_y_m_d <- all(grepl("^\\d{4}-\\d{2}-\\d{2}$", first5))

# Parse dates and create additional columns
accident <- accident %>%
  mutate(
    ACCIDENT_DATE = if (is_y_m_d) ymd(ACCIDENT_DATE) else mdy(ACCIDENT_DATE),
    year = year(ACCIDENT_DATE),
    month = month(ACCIDENT_DATE),
    Has_Fatal = ifelse(NO_PERSONS_KILLED > 0, 1, 0),
    Has_Serious = ifelse(NO_PERSONS_INJ_2 > 0, 1, 0),
    Has_Fatal_Serious = ifelse(Has_Fatal == 1 | Has_Serious == 1, 1, 0)
  ) %>%
  filter(!is.na(ACCIDENT_DATE))

cat("✓ Data loaded:", nrow(accident), "rows\n")
cat("  Date range:", range(accident$ACCIDENT_DATE, na.rm = TRUE), "\n")

# ==============================================================================
# 3. Data Aggregation - Monthly Totals
# ==============================================================================

# Total crashes per month
monthly_total <- accident %>%
  group_by(year = year(ACCIDENT_DATE), month = month(ACCIDENT_DATE)) %>%
  summarise(total_crashes = n(), .groups = "drop") %>%
  arrange(year, month) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d"))

# Fatal+Serious crashes per month
monthly_fatal_serious <- accident %>%
  filter(Has_Fatal_Serious == 1) %>%
  group_by(year = year(ACCIDENT_DATE), month = month(ACCIDENT_DATE)) %>%
  summarise(total_fatal_serious_crashes = n(),
            total_fatal = sum(NO_PERSONS_KILLED, na.rm = TRUE),
            total_serious = sum(NO_PERSONS_INJ_2, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(year, month) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d"))

cat("✓ Aggregated months — Total:", nrow(monthly_total), 
    "| Fatal+Serious:", nrow(monthly_fatal_serious), "\n")

# View summary statistics
cat("\nTotal Crashes Summary:\n")
head(monthly_total)
summary(monthly_total)

# ==============================================================================
# 3.5. Road Crash Severity Analysis Over Time
# Question: To what extent have road crash severities in Victoria changed 
# over time, and what patterns can be identified in their long-term development?
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("ROAD CRASH SEVERITY ANALYSIS OVER TIME\n")
cat(rep("=", 80), "\n\n", sep = "")

# ==========================================
# 3.5.1. Annual Severity Summary Table
# ==========================================
# Check number of months per year to exclude incomplete years
months_per_year <- accident %>%
  group_by(year = year(ACCIDENT_DATE), month = month(ACCIDENT_DATE)) %>%
  summarise(.groups = "drop") %>%
  group_by(year) %>%
  summarise(months_count = n(), .groups = "drop")

# Identify years with complete data (12 months) - exclude partial years
complete_years <- months_per_year %>%
  filter(months_count >= 12) %>%
  pull(year)

cat("Years with complete data (12 months):", paste(complete_years, collapse = ", "), "\n")
if (length(setdiff(unique(accident$year), complete_years)) > 0) {
  incomplete_years <- setdiff(unique(accident$year), complete_years)
  cat("Years excluded (incomplete):", paste(incomplete_years, collapse = ", "), "\n")
}

annual_severity <- accident %>%
  filter(year(ACCIDENT_DATE) %in% complete_years) %>%
  group_by(year = year(ACCIDENT_DATE)) %>%
  summarise(
    total_crashes = n(),
    fatal_crashes = sum(Has_Fatal == 1, na.rm = TRUE),
    serious_crashes = sum(Has_Serious == 1, na.rm = TRUE),
    fatal_serious_crashes = sum(Has_Fatal_Serious == 1, na.rm = TRUE),
    total_fatalities = sum(NO_PERSONS_KILLED, na.rm = TRUE),
    total_serious_injuries = sum(NO_PERSONS_INJ_2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    fatal_rate = round((fatal_crashes / total_crashes * 100), 2),
    serious_rate = round((serious_crashes / total_crashes * 100), 2),
    fatal_serious_rate = round((fatal_serious_crashes / total_crashes * 100), 2),
    fatalities_per_100_crashes = round((total_fatalities / total_crashes * 100), 2),
    serious_per_100_crashes = round((total_serious_injuries / total_crashes * 100), 2)
  ) %>%
  arrange(year)

cat("\nANNUAL SEVERITY SUMMARY TABLE (Complete Years Only)\n")
cat(rep("-", 80), "\n", sep = "")
print(annual_severity)

# ==========================================
# 3.5.2. Monthly Severity Rates Table
# ==========================================
monthly_severity_rates <- monthly_total %>%
  left_join(
    monthly_fatal_serious %>% 
      select(year, month, total_fatal_serious_crashes),
    by = c("year", "month")
  ) %>%
  mutate(
    fatal_serious_rate = round((total_fatal_serious_crashes / total_crashes * 100), 2),
    fatal_serious_rate = ifelse(is.na(fatal_serious_rate), 0, fatal_serious_rate)
  ) %>%
  select(year, month, date, total_crashes, total_fatal_serious_crashes, fatal_serious_rate)

# Show summary by year instead of individual months
cat("\nMONTHLY SEVERITY RATES - SUMMARY BY YEAR:\n")
cat(rep("-", 80), "\n", sep = "")

# Calculate annual averages from monthly data (only complete years)
monthly_summary_by_year <- monthly_severity_rates %>%
  filter(year %in% complete_years) %>%
  group_by(year) %>%
  summarise(
    months_count = n(),
    avg_monthly_total = round(mean(total_crashes), 1),
    avg_monthly_fatal_serious = round(mean(total_fatal_serious_crashes), 1),
    avg_fatal_serious_rate = round(mean(fatal_serious_rate), 2),
    min_rate = round(min(fatal_serious_rate), 2),
    max_rate = round(max(fatal_serious_rate), 2),
    .groups = "drop"
  )

cat("\nMONTHLY SEVERITY RATES - SUMMARY BY YEAR (Complete Years Only):\n")
cat(rep("-", 80), "\n", sep = "")
print(monthly_summary_by_year)

# Show sample of first and last complete year's monthly data for detail
cat("\nDETAILED MONTHLY DATA - Sample from first and last complete year:\n")
cat(rep("-", 80), "\n", sep = "")

first_complete_year <- min(complete_years)
last_complete_year <- max(complete_years)

cat("\nFirst complete year (", first_complete_year, ") - All months:\n", sep = "")
print(monthly_severity_rates %>% filter(year == first_complete_year))

cat("\nLast complete year (", last_complete_year, ") - All months:\n", sep = "")
print(monthly_severity_rates %>% filter(year == last_complete_year))

# Show partial year data separately if exists
if (length(setdiff(unique(monthly_severity_rates$year), complete_years)) > 0) {
  partial_years <- setdiff(unique(monthly_severity_rates$year), complete_years)
  cat("\nNOTE: Partial year data (excluded from analysis):\n")
  for (py in partial_years) {
    partial_data <- monthly_severity_rates %>% filter(year == py)
    cat("\nYear", py, "-", nrow(partial_data), "months of data:\n")
    print(partial_data)
  }
}

# ==========================================
# 3.5.3. Trend Analysis - Summary Statistics
# ==========================================
cat("\n", rep("=", 80), "\n", sep = "")
cat("TREND ANALYSIS SUMMARY\n")
cat(rep("=", 80), "\n\n", sep = "")
cat("NOTE: Analysis includes only complete years (12 months of data)\n")
cat("Incomplete years are excluded to ensure fair comparison.\n\n")

# Calculate changes over time
first_year <- min(annual_severity$year)
last_year <- max(annual_severity$year)
first_period <- annual_severity %>% filter(year == first_year)
last_period <- annual_severity %>% filter(year == last_year)

cat("COMPARISON: First Year (", first_year, ") vs Last Year (", last_year, ")\n", sep = "")
cat(rep("-", 80), "\n", sep = "")
cat("Metric", "                    First Year", "   Last Year", "   Change", "   % Change\n", sep = "")
cat(rep("-", 80), "\n", sep = "")

changes <- data.frame(
  metric = c("Total Crashes", "Fatal Crashes", "Serious Crashes", 
             "Fatal+Serious Crashes", "Fatal Rate (%)", "Serious Rate (%)", 
             "Fatal+Serious Rate (%)"),
  first = c(
    first_period$total_crashes,
    first_period$fatal_crashes,
    first_period$serious_crashes,
    first_period$fatal_serious_crashes,
    first_period$fatal_rate,
    first_period$serious_rate,
    first_period$fatal_serious_rate
  ),
  last = c(
    last_period$total_crashes,
    last_period$fatal_crashes,
    last_period$serious_crashes,
    last_period$fatal_serious_crashes,
    last_period$fatal_rate,
    last_period$serious_rate,
    last_period$fatal_serious_rate
  )
) %>%
  mutate(
    change = last - first,
    pct_change = round((change / first) * 100, 2)
  )

for (i in 1:nrow(changes)) {
  cat(sprintf("%-25s %12.0f %12.0f %10.0f %10.2f%%\n", 
              changes$metric[i], changes$first[i], changes$last[i], 
              changes$change[i], changes$pct_change[i]))
}

# Overall trend statistics
cat("\nOVERALL TREND STATISTICS (", first_year, "-", last_year, ")\n", sep = "")
cat(rep("-", 80), "\n", sep = "")
cat("Average Annual Fatal+Serious Rate:", 
    round(mean(annual_severity$fatal_serious_rate, na.rm = TRUE), 2), "%\n")
cat("Min Fatal+Serious Rate:", round(min(annual_severity$fatal_serious_rate, na.rm = TRUE), 2), 
    "% (Year:", annual_severity$year[which.min(annual_severity$fatal_serious_rate)], ")\n")
cat("Max Fatal+Serious Rate:", round(max(annual_severity$fatal_serious_rate, na.rm = TRUE), 2), 
    "% (Year:", annual_severity$year[which.max(annual_severity$fatal_serious_rate)], ")\n")

# Trend direction
if (last_period$fatal_serious_rate < first_period$fatal_serious_rate) {
  cat("Trend Direction: DECREASING (Improving)\n")
} else if (last_period$fatal_serious_rate > first_period$fatal_serious_rate) {
  cat("Trend Direction: INCREASING (Worsening)\n")
} else {
  cat("Trend Direction: STABLE\n")
}

# ==========================================
# 3.5.4. Visualizations - Charts
# ==========================================
cat("\n", rep("=", 80), "\n", sep = "")
cat("VISUALIZATIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Chart 1: Annual Total Crashes vs Fatal+Serious Crashes
p1 <- ggplot(annual_severity, aes(x = year)) +
  geom_line(aes(y = total_crashes, color = "Total Crashes"), linewidth = 1.2) +
  geom_line(aes(y = fatal_serious_crashes, color = "Fatal+Serious"), linewidth = 1.2) +
  geom_point(aes(y = total_crashes, color = "Total Crashes"), size = 2) +
  geom_point(aes(y = fatal_serious_crashes, color = "Fatal+Serious"), size = 2) +
  labs(
    title = "Road Crash Trends Over Time: Total vs Fatal+Serious Crashes",
    subtitle = "Victoria, Annual Data",
    x = "Year",
    y = "Number of Crashes",
    color = "Crash Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Total Crashes" = "steelblue", "Fatal+Serious" = "darkred"))

print(p1)
ggsave("charts/01_annual_total_vs_fatal_serious.png", p1, width = 10, height = 6, dpi = 300)

# Chart 2: Fatal+Serious Rate Over Time
p2 <- ggplot(annual_severity, aes(x = year, y = fatal_serious_rate)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed", alpha = 0.3) +
  labs(
    title = "Fatal+Serious Crash Rate Over Time",
    subtitle = "Percentage of Total Crashes that are Fatal or Serious",
    x = "Year",
    y = "Fatal+Serious Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  ylim(0, max(annual_severity$fatal_serious_rate, na.rm = TRUE) * 1.1)

print(p2)
ggsave("charts/02_fatal_serious_rate_over_time.png", p2, width = 10, height = 6, dpi = 300)

# Chart 3: Monthly Severity Rate Trend
# Filter to show only complete years for cleaner visualization
monthly_for_chart <- monthly_severity_rates %>%
  filter(year %in% complete_years)

p3 <- ggplot(monthly_for_chart, aes(x = date, y = fatal_serious_rate)) +
  geom_line(color = "darkorange", linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed", alpha = 0.2) +
  labs(
    title = "Monthly Fatal+Serious Crash Rate Trend",
    subtitle = "Smoothed trend line shows long-term pattern (complete years only)",
    x = "Date",
    y = "Fatal+Serious Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p3)
ggsave("charts/03_monthly_severity_rate_trend.png", p3, width = 12, height = 6, dpi = 300)

# Chart 4: Stacked Area Chart - Breakdown by Severity
annual_severity_long <- annual_severity %>%
  select(year, total_crashes, fatal_crashes, serious_crashes) %>%
  mutate(
    other_crashes = total_crashes - fatal_crashes - serious_crashes
  ) %>%
  select(year, fatal_crashes, serious_crashes, other_crashes) %>%
  pivot_longer(cols = c(fatal_crashes, serious_crashes, other_crashes),
               names_to = "severity", values_to = "count") %>%
  mutate(
    severity = case_when(
      severity == "fatal_crashes" ~ "Fatal",
      severity == "serious_crashes" ~ "Serious",
      TRUE ~ "Other"
    )
  )

p4 <- ggplot(annual_severity_long, aes(x = year, y = count, fill = severity)) +
  geom_area(alpha = 0.7) +
  labs(
    title = "Crash Severity Breakdown Over Time",
    subtitle = "Stacked area showing composition of crashes by severity",
    x = "Year",
    y = "Number of Crashes",
    fill = "Severity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("Fatal" = "darkred", "Serious" = "orange", "Other" = "lightblue"))

print(p4)
ggsave("charts/04_crash_severity_breakdown.png", p4, width = 10, height = 6, dpi = 300)

# ==========================================
# 3.5.5. Key Patterns Identified
# ==========================================
cat("\n", rep("=", 80), "\n", sep = "")
cat("KEY PATTERNS IDENTIFIED\n")
cat(rep("=", 80), "\n\n", sep = "")

# Calculate linear trend for fatal_serious_rate
if (nrow(annual_severity) > 1) {
  trend_model <- lm(fatal_serious_rate ~ year, data = annual_severity)
  trend_slope <- coef(trend_model)[2]
  trend_p <- summary(trend_model)$coefficients[2, 4]
  
  cat("1. LONG-TERM TREND:\n")
  if (trend_slope < 0 && trend_p < 0.05) {
    cat("   - Fatal+Serious rate shows a SIGNIFICANT DECREASING trend (", 
        sprintf("%.2f", abs(trend_slope)), "% per year)\n", sep = "")
    cat("   - This indicates IMPROVEMENT in road safety over time\n")
  } else if (trend_slope > 0 && trend_p < 0.05) {
    cat("   - Fatal+Serious rate shows a SIGNIFICANT INCREASING trend (", 
        sprintf("%.2f", trend_slope), "% per year)\n", sep = "")
    cat("   - This indicates WORSENING road safety over time\n")
  } else {
    cat("   - Fatal+Serious rate shows NO SIGNIFICANT TREND (relatively stable)\n")
  }
  cat("   - Statistical significance: p =", sprintf("%.4f", trend_p), "\n\n")
}

# Identify periods of change
cat("2. PERIODS OF CHANGE:\n")
if (nrow(annual_severity) >= 3) {
  # Calculate year-over-year changes
  annual_severity <- annual_severity %>%
    mutate(
      yoy_change = fatal_serious_rate - lag(fatal_serious_rate),
      yoy_pct_change = round((yoy_change / lag(fatal_serious_rate)) * 100, 2)
    )
  
  largest_decrease <- annual_severity %>%
    filter(!is.na(yoy_change)) %>%
    slice_min(yoy_change, n = 1)
  
  largest_increase <- annual_severity %>%
    filter(!is.na(yoy_change)) %>%
    slice_max(yoy_change, n = 1)
  
  cat("   - Largest decrease:", sprintf("%.2f", largest_decrease$yoy_change), 
      "% (", largest_decrease$year - 1, " to ", largest_decrease$year, ")\n", sep = "")
  cat("   - Largest increase:", sprintf("%.2f", largest_increase$yoy_change), 
      "% (", largest_increase$year - 1, " to ", largest_increase$year, ")\n\n", sep = "")
}

# Average rates by period
if (nrow(annual_severity) >= 6) {
  mid_point <- ceiling(nrow(annual_severity) / 2)
  first_half <- annual_severity[1:mid_point, ]
  second_half <- annual_severity[(mid_point + 1):nrow(annual_severity), ]
  
  cat("3. PERIOD COMPARISON:\n")
  cat("   - First half period (", first_half$year[1], "-", 
      first_half$year[nrow(first_half)], "): ", 
      sprintf("%.2f", mean(first_half$fatal_serious_rate, na.rm = TRUE)), "%\n", sep = "")
  cat("   - Second half period (", second_half$year[1], "-", 
      second_half$year[nrow(second_half)], "): ", 
      sprintf("%.2f", mean(second_half$fatal_serious_rate, na.rm = TRUE)), "%\n", sep = "")
  
  period_change <- mean(second_half$fatal_serious_rate, na.rm = TRUE) - 
    mean(first_half$fatal_serious_rate, na.rm = TRUE)
  cat("   - Change between periods:", sprintf("%.2f", period_change), "%\n\n")
}

cat("4. SUMMARY:\n")
cat("   Road crash severities in Victoria have ", 
    ifelse(last_period$fatal_serious_rate < first_period$fatal_serious_rate,
           "DECREASED", "INCREASED"),
    " from ", first_period$fatal_serious_rate, "% (", first_year, 
    ") to ", last_period$fatal_serious_rate, "% (", last_year, ")\n", sep = "")
cat("   This represents a change of ", 
    sprintf("%.2f", last_period$fatal_serious_rate - first_period$fatal_serious_rate),
    " percentage points over the study period.\n\n")

# ==============================================================================
# 4. Time Series Analysis
# ==============================================================================

# Create time series objects (start fixed to 2012-01)
total_crashes_ts <- ts(monthly_total$total_crashes, start = c(2012, 1), frequency = 12)
fatal_serious_ts_monthly <- ts(monthly_fatal_serious$total_fatal_serious_crashes, 
                               start = c(2012, 1), frequency = 12)

# Fit ARIMA models using auto.arima
fit_total <- auto.arima(total_crashes_ts)
fit_fatal_serious <- auto.arima(fatal_serious_ts_monthly)

cat("✓ ARIMA models fitted on FULL history\n")
cat("\nTotal Crashes Model:\n")
summary(fit_total)
cat("\nFatal+Serious Crashes Model:\n")
summary(fit_fatal_serious)

# Generate forecasts for next 12 months
forecast_total <- forecast(fit_total, h = 12)
forecast_fatal_serious <- forecast(fit_fatal_serious, h = 12)

cat("\n✓ Forecasts generated for 12 months ahead\n")

# ==============================================================================
# 5. Visualization
# ==============================================================================

# Plot forecast for Total Crashes
p_forecast_total <- autoplot(forecast_total, PI = TRUE) +
  labs(
    title = "Forecast of Monthly Total Crashes in Victoria",
    subtitle = "ARIMA Model Forecast with 80% and 95% Confidence Intervals",
    x = "Year", 
    y = "Number of Crashes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid.minor = element_blank()
  )
print(p_forecast_total)
ggsave("charts/05_forecast_total_crashes.png", p_forecast_total, width = 12, height = 6, dpi = 300)

# Plot forecast for Fatal+Serious Crashes
p_forecast_fatal_serious <- autoplot(forecast_fatal_serious, PI = TRUE) +
  labs(
    title = "Forecast of Monthly Fatal+Serious Crashes in Victoria",
    subtitle = "ARIMA Model Forecast with 80% and 95% Confidence Intervals",
    x = "Year", 
    y = "Number of Crashes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid.minor = element_blank()
  )
print(p_forecast_fatal_serious)
ggsave("charts/06_forecast_fatal_serious_crashes.png", p_forecast_fatal_serious, width = 12, height = 6, dpi = 300)

# ==============================================================================
# 6. Model Evaluation (Total Crashes & Fatal+Serious)
# ==============================================================================

# ==========================================
# 6.1. Total Crashes Model Evaluation
# ==========================================
cat("\n", rep("=", 60), "\n", sep = "")
cat("MODEL EVALUATION - TOTAL CRASHES\n")
cat(rep("=", 60), "\n", sep = "")

# Split data into training and test sets for Total Crashes
train_total <- head(total_crashes_ts, length(total_crashes_ts)-12)
test_total <- tail(total_crashes_ts, 12)

# Fit model on training data
fit_train_total <- auto.arima(train_total)
pred_total <- forecast(fit_train_total, h = 12)

# Calculate accuracy metrics
cat("\nModel Accuracy Metrics (Total Crashes):\n")
accuracy(pred_total, test_total)

# Display training and test periods
cat("\n📘 Training period:\n")
cat(start(train_total)[1], "-", end(train_total)[1], "\n")
cat("Start:", paste(start(train_total), collapse = "-"),
    "| End:", paste(end(train_total), collapse = "-"), "\n\n")

cat("🔍 Test period:\n")
cat("Start:", paste(start(test_total), collapse = "-"),
    "| End:", paste(end(test_total), collapse = "-"), "\n")

# ==========================================
# 6.2. Fatal+Serious Model Evaluation
# ==========================================
cat("\n", rep("=", 60), "\n", sep = "")
cat("MODEL EVALUATION - FATAL+SERIOUS CRASHES\n")
cat(rep("=", 60), "\n", sep = "")

# Split data into training and test sets for Fatal+Serious
train_fatal_serious <- head(fatal_serious_ts_monthly, length(fatal_serious_ts_monthly)-12)
test_fatal_serious <- tail(fatal_serious_ts_monthly, 12)

# Fit model on training data
fit_train_fatal_serious <- auto.arima(train_fatal_serious)
pred_fatal_serious <- forecast(fit_train_fatal_serious, h = 12)

# Calculate accuracy metrics
cat("\nModel Accuracy Metrics (Fatal+Serious Crashes):\n")
accuracy(pred_fatal_serious, test_fatal_serious)

# Display training and test periods
cat("\n📘 Training period:\n")
cat(start(train_fatal_serious)[1], "-", end(train_fatal_serious)[1], "\n")
cat("Start:", paste(start(train_fatal_serious), collapse = "-"),
    "| End:", paste(end(train_fatal_serious), collapse = "-"), "\n\n")

cat("🔍 Test period:\n")
cat("Start:", paste(start(test_fatal_serious), collapse = "-"),
    "| End:", paste(end(test_fatal_serious), collapse = "-"), "\n")

# ==============================================================================
# 7. Annual Summary & Rates
# ==============================================================================

# Use complete_years from section 3.5.1 to ensure consistency
# If complete_years not available, calculate it
if (!exists("complete_years")) {
  months_per_year_check <- accident %>%
    group_by(year = year(ACCIDENT_DATE), month = month(ACCIDENT_DATE)) %>%
    summarise(.groups = "drop") %>%
    group_by(year) %>%
    summarise(months_count = n(), .groups = "drop")
  
  complete_years <- months_per_year_check %>%
    filter(months_count >= 12) %>%
    pull(year)
}

annual_total <- accident %>%
  filter(year(ACCIDENT_DATE) %in% complete_years) %>%
  group_by(year = year(ACCIDENT_DATE)) %>%
  summarise(total_crashes = n(), .groups = "drop")

annual_fatal_serious <- accident %>%
  filter(year(ACCIDENT_DATE) %in% complete_years) %>%
  filter(Has_Fatal_Serious == 1) %>%
  group_by(year = year(ACCIDENT_DATE)) %>%
  summarise(fatal_serious_crashes = n(), .groups = "drop")

annual_summary <- annual_total %>%
  left_join(annual_fatal_serious, by = "year") %>%
  mutate(
    fatal_serious_rate = round((fatal_serious_crashes / total_crashes * 100), 2),
    across(everything(), ~coalesce(., 0))
  )

cat("\n==========================================\n")
cat("Annual Summary (Total; Fatal+Serious)\n")
cat("Complete Years Only\n")
cat("==========================================\n")
print(annual_summary)

# ==============================================================================
# 8. Trend Correlation Analysis
# ==============================================================================

# Correlation of trends
fatal_serious_trend <- decompose(fatal_serious_ts_monthly)$trend
total_trend <- decompose(total_crashes_ts)$trend

trend_comparison <- data.frame(
  fs = as.numeric(fatal_serious_trend),
  tot = as.numeric(total_trend)
) %>% filter(!is.na(fs) & !is.na(tot))

cor_fs_total <- cor(trend_comparison$fs, trend_comparison$tot)

cat("\n==========================================\n")
cat("SUMMARY OF ANALYSIS (Total; Fatal+Serious)\n")
cat("==========================================\n\n")
cat("- Trend correlation (Fatal+Serious vs Total):", sprintf("%.3f", cor_fs_total), "\n")
cat("- Avg monthly Total:", round(mean(monthly_total$total_crashes), 1), "\n")
cat("- Avg monthly Fatal+Serious:", round(mean(monthly_fatal_serious$total_fatal_serious_crashes), 1), "\n")
cat("- Avg annual Fatal+Serious rate:",
    round(mean(annual_summary$fatal_serious_rate, na.rm = TRUE), 2), "%\n\n")

# ==============================================================================
# 9. Detailed Forecast Tables
# ==============================================================================

# Forecast window (Mar 2025 - Feb 2026)
forecast_start <- as.Date("2025-03-01")
forecast_dates <- seq(forecast_start, by = "month", length.out = 12)

# Total Crashes Forecast Table
total_forecast_df <- data.frame(
  Month = format(forecast_dates, "%b %Y"),
  Forecast = round(as.numeric(forecast_total$mean), 0),
  Lo_80 = round(as.numeric(forecast_total$lower[,1]), 0),
  Hi_80 = round(as.numeric(forecast_total$upper[,1]), 0),
  Lo_95 = round(as.numeric(forecast_total$lower[,2]), 0),
  Hi_95 = round(as.numeric(forecast_total$upper[,2]), 0)
)

cat("FORECAST (", format(min(forecast_dates), "%b %Y"), " - ",
    format(max(forecast_dates), "%b %Y"), ")\n", sep = "")
cat("Total Crashes:\n")
print(total_forecast_df, row.names = FALSE)

# Complete Forecast Table (Total + Fatal+Serious)
total_detailed <- data.frame(
  Month = format(forecast_dates, "%b %Y"),
  Type = "Total Crashes",
  Forecast = round(as.numeric(forecast_total$mean), 0),
  Lo_80 = round(as.numeric(forecast_total$lower[,1]), 0),
  Hi_80 = round(as.numeric(forecast_total$upper[,1]), 0),
  Lo_95 = round(as.numeric(forecast_total$lower[,2]), 0),
  Hi_95 = round(as.numeric(forecast_total$upper[,2]), 0)
)

fatal_serious_detailed <- data.frame(
  Month = format(forecast_dates, "%b %Y"),
  Type = "Fatal+Serious",
  Forecast = round(as.numeric(forecast_fatal_serious$mean), 0),
  Lo_80 = round(as.numeric(forecast_fatal_serious$lower[,1]), 0),
  Hi_80 = round(as.numeric(forecast_fatal_serious$upper[,1]), 0),
  Lo_95 = round(as.numeric(forecast_fatal_serious$lower[,2]), 0),
  Hi_95 = round(as.numeric(forecast_fatal_serious$upper[,2]), 0)
)

complete_forecast <- rbind(total_detailed, fatal_serious_detailed)

cat("\n==========================================\n")
cat("COMPLETE FORECAST TABLE - TOTAL; FATAL+SERIOUS\n")
cat("==========================================\n\n")
print(complete_forecast, row.names = FALSE)

cat("\nSUMMARY FOR TEAM:\n")
cat(rep("=", 80), "\n")
cat("- Avg Monthly Total:", round(mean(total_detailed$Forecast), 0), "\n")
cat("- Avg Monthly Fatal+Serious:", round(mean(fatal_serious_detailed$Forecast), 0), "\n")

cat("\n12-month sums (Total Crashes):\n")
cat("- Point:", sum(total_detailed$Forecast), "\n")
cat("- 80%:", sum(total_detailed$Lo_80), "-", sum(total_detailed$Hi_80), "\n")
cat("- 95%:", sum(total_detailed$Lo_95), "-", sum(total_detailed$Hi_95), "\n")

# ==============================================================================
# 10. Year-over-Year Comparison: 2024 vs 2025 (Total Crashes Only)
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("YEAR-OVER-YEAR COMPARISON: 2024 vs 2025 (Total Crashes)\n")
cat("2025 = 2 months actual data (Jan-Feb) + 10 months forecast (Mar-Dec)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Get actual data for 2024 (full year)
total_2024 <- accident %>%
  filter(year(ACCIDENT_DATE) == 2024) %>%
  summarise(total_crashes = n(), .groups = "drop") %>%
  pull(total_crashes)

# Get actual data for 2025 (Jan-Feb only)
total_2025_actual <- accident %>%
  filter(year(ACCIDENT_DATE) == 2025) %>%
  summarise(total_crashes = n(), .groups = "drop") %>%
  pull(total_crashes)

# Get forecast for Mar-Dec 2025 (10 months)
# Forecast dates start from Mar 2025, so first 10 months are Mar-Dec 2025
total_2025_forecast <- round(sum(as.numeric(forecast_total$mean[1:10])), 0)

# Combine actual (Jan-Feb) + forecast (Mar-Dec) for 2025
total_2025_combined <- total_2025_actual + total_2025_forecast

# Calculate percentage change
change_2025 <- total_2025_combined - total_2024
pct_change_2025 <- round((change_2025 / total_2024) * 100, 2)

# Create comparison table
comparison_table <- data.frame(
  Year = c("2024", "2025"),
  Description = c("Actual (Full Year)", "Combined (Actual Jan-Feb + Forecast Mar-Dec)"),
  Total_Crashes = c(total_2024, total_2025_combined),
  Actual_Jan_Feb = c(NA, total_2025_actual),
  Forecast_Mar_Dec = c(NA, total_2025_forecast)
)

cat("\nCOMPARISON TABLE:\n")
cat(rep("-", 80), "\n", sep = "")
print(comparison_table, row.names = FALSE)

cat("\n", rep("-", 80), "\n", sep = "")
cat("PERCENTAGE CHANGE: 2024 → 2025\n")
cat(rep("-", 80), "\n", sep = "")
cat(sprintf("Change: %.0f crashes\n", change_2025))
cat(sprintf("Percentage Change: %.2f%%\n", pct_change_2025))
cat(sprintf("Direction: %s\n", ifelse(change_2025 < 0, "DECREASE ↓", "INCREASE ↑")))

# ==============================================================================
# 11. Trend Analysis: 2021 to 2025 (Total Crashes)
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("TREND ANALYSIS: 2021 to 2025 (Total Crashes)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Get totals for each year (2021-2024 actual, 2025 combined)
totals_by_year <- data.frame(
  year = 2021:2025,
  total_crashes = c(
    accident %>% filter(year(ACCIDENT_DATE) == 2021) %>% summarise(n(), .groups = "drop") %>% pull(),
    accident %>% filter(year(ACCIDENT_DATE) == 2022) %>% summarise(n(), .groups = "drop") %>% pull(),
    accident %>% filter(year(ACCIDENT_DATE) == 2023) %>% summarise(n(), .groups = "drop") %>% pull(),
    accident %>% filter(year(ACCIDENT_DATE) == 2024) %>% summarise(n(), .groups = "drop") %>% pull(),
    total_2025_combined
  ),
  data_type = c("Actual", "Actual", "Actual", "Actual", "Combined (Actual Jan-Feb + Forecast Mar-Dec)")
)

# Calculate year-over-year changes
totals_by_year <- totals_by_year %>%
  mutate(
    yoy_change = total_crashes - lag(total_crashes),
    yoy_pct_change = round((yoy_change / lag(total_crashes)) * 100, 2),
    change_from_2021 = total_crashes - totals_by_year$total_crashes[1],
    pct_change_from_2021 = round((change_from_2021 / totals_by_year$total_crashes[1]) * 100, 2)
  )

cat("\nANNUAL TREND TABLE:\n")
cat(rep("-", 80), "\n", sep = "")
print(totals_by_year, row.names = FALSE)

cat("\n", rep("-", 80), "\n", sep = "")
cat("YEAR-OVER-YEAR CHANGES:\n")
cat(rep("-", 80), "\n", sep = "")
for (i in 2:nrow(totals_by_year)) {
  prev_year <- totals_by_year$year[i-1]
  curr_year <- totals_by_year$year[i]
  change_val <- totals_by_year$yoy_change[i]
  pct_val <- totals_by_year$yoy_pct_change[i]
  
  cat(sprintf("%d → %d: %s by %.0f (%.2f%%)\n", 
              prev_year, curr_year,
              ifelse(change_val < 0, "DECREASE ↓", "INCREASE ↑"),
              abs(change_val), abs(pct_val)))
}

cat("\n", rep("-", 80), "\n", sep = "")
cat("OVERALL CHANGE FROM 2021 TO 2025:\n")
cat(rep("-", 80), "\n", sep = "")
total_change_2021_2025 <- totals_by_year$change_from_2021[nrow(totals_by_year)]
total_pct_change_2021_2025 <- totals_by_year$pct_change_from_2021[nrow(totals_by_year)]

cat(sprintf("2021 Total Crashes: %.0f\n", totals_by_year$total_crashes[1]))
cat(sprintf("2025 Total Crashes: %.0f\n", totals_by_year$total_crashes[nrow(totals_by_year)]))
cat(sprintf("Absolute Change: %.0f crashes\n", total_change_2021_2025))
cat(sprintf("Percentage Change: %.2f%%\n", total_pct_change_2021_2025))
cat(sprintf("Direction: %s\n", ifelse(total_change_2021_2025 < 0, "DECREASE ↓", "INCREASE ↑")))

if (total_change_2021_2025 < 0) {
  cat(sprintf("\n✅ From 2021 to 2025, total crashes DECREASED by %.2f%%\n", abs(total_pct_change_2021_2025)))
} else {
  cat(sprintf("\n⚠️  From 2021 to 2025, total crashes INCREASED by %.2f%%\n", abs(total_pct_change_2021_2025)))
}

# ==========================================
# 11.1. Trend Visualization
# ==========================================
cat("\n", rep("-", 80), "\n", sep = "")
cat("TREND VISUALIZATION:\n")
cat(rep("-", 80), "\n", sep = "")

# Create trend chart
p_trend <- ggplot(totals_by_year, aes(x = year, y = total_crashes)) +
  geom_line(color = "steelblue", linewidth = 1.5) +
  geom_point(aes(color = data_type), size = 4) +
  geom_text(aes(label = sprintf("%.0f", total_crashes)), 
            vjust = -0.5, hjust = 0.5, size = 3.5) +
  labs(
    title = "Total Crashes Trend: 2021 to 2025",
    subtitle = "2021-2024: Actual Data | 2025: Combined (Actual + Forecast)",
    x = "Year",
    y = "Total Crashes",
    color = "Data Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 2021:2025) +
  scale_color_manual(values = c("Actual" = "darkgreen", "Combined (Actual Jan-Feb + Forecast Mar-Dec)" = "orange"))

print(p_trend)
ggsave("charts/07_trend_2021_to_2025.png", p_trend, width = 10, height = 6, dpi = 300)

cat("\n", rep("=", 80), "\n", sep = "")
cat("END OF ANALYSIS\n")
cat(rep("=", 80), "\n", sep = "")
cat("\nAll charts have been saved to the 'charts' directory:\n")
cat("  - 01_annual_total_vs_fatal_serious.png\n")
cat("  - 02_fatal_serious_rate_over_time.png\n")
cat("  - 03_monthly_severity_rate_trend.png\n")
cat("  - 04_crash_severity_breakdown.png\n")
cat("  - 05_forecast_total_crashes.png\n")
cat("  - 06_forecast_fatal_serious_crashes.png\n")
cat("  - 07_trend_2021_to_2025.png\n")
cat(rep("=", 80), "\n", sep = "")
