# ==============================================================================
# Linear Regression Analysis - Road Crash Data
# Converted from LinearRegression.ipynb
# 
# Features:
# - Data cleaning and feature engineering
# - Monthly crash aggregation
# - Weather, surface, urban, and lighting condition analysis
# - Linear regression modeling
# - Model diagnostics and visualization
# - Cross-validation
# ==============================================================================

# Check R version
R.version.string

# ==============================================================================
# 1. Setup and Package Loading
# ==============================================================================

# Install packages if not already installed
required_packages <- c("dplyr", "lubridate", "readr", "stringr", "tidyr", "conflicted",
                       "car", "broom", "ggplot2", "sjPlot", "effects", "ggeffects",
                       "ggfortify", "lmtest")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages)
}

# Load required libraries
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

# Handle conflicts (prefer dplyr functions)
if (require("conflicted", quietly = TRUE)) {
  conflicted::conflicts_prefer(dplyr::select, dplyr::filter, dplyr::lag)
}

# Create charts directory if it doesn't exist
if (!dir.exists("charts")) {
  dir.create("charts")
  cat("Created 'charts' directory for saving charts\n")
}

# ==============================================================================
# 2. Data Loading
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("DATA LOADING\n")
cat(rep("=", 80), "\n\n", sep = "")

# Load data files
# Update the paths to your actual data file locations
data_dir <- "sample_data"  # Change this to your data directory path

# Alternative: use dataupdate directory if sample_data doesn't exist
if (!dir.exists(data_dir)) {
  data_dir <- "dataupdate"
  cat("Note: Using 'dataupdate' directory instead of 'sample_data'\n")
}

accident_path <- file.path(data_dir, "accident.csv")
node_path <- file.path(data_dir, "node.csv")
atmospheric_cond_path <- file.path(data_dir, "atmospheric_cond.csv")
road_surface_cond_path <- file.path(data_dir, "road_surface_cond.csv")

# Load accident data
if (file.exists(accident_path)) {
  accident <- read_csv(accident_path, show_col_types = FALSE)
  cat("✓ Accident data loaded successfully\n")
} else {
  stop(paste("File not found:", accident_path, "\nPlease update the 'data_dir' variable with the correct path."))
}

# Load node data
if (file.exists(node_path)) {
  node <- read_csv(node_path, show_col_types = FALSE)
  cat("✓ Node data loaded successfully\n")
} else {
  warning(paste("File not found:", node_path))
  node <- NULL
}

# Load atmospheric condition data
if (file.exists(atmospheric_cond_path)) {
  atmospheric_cond <- read_csv(atmospheric_cond_path, show_col_types = FALSE)
  cat("✓ Atmospheric condition data loaded successfully\n")
} else {
  warning(paste("File not found:", atmospheric_cond_path))
  atmospheric_cond <- NULL
}

# Load road surface condition data
if (file.exists(road_surface_cond_path)) {
  road_surface_cond <- read_csv(road_surface_cond_path, show_col_types = FALSE)
  cat("✓ Road surface condition data loaded successfully\n")
} else {
  warning(paste("File not found:", road_surface_cond_path))
  road_surface_cond <- NULL
}

# Ensure core fields exist
required_fields <- c("ACCIDENT_NO", "ACCIDENT_DATE", "SPEED_ZONE", "LIGHT_CONDITION", "NODE_ID")
missing_fields <- setdiff(required_fields, names(accident))
if (length(missing_fields) > 0) {
  stop(paste("Missing required fields in accident data:", paste(missing_fields, collapse = ", ")))
}

# Auto-parse ACCIDENT_DATE (handles both YYYY-MM-DD and M/D/YYYY formats)
first5 <- head(accident$ACCIDENT_DATE, 5)
is_y_m_d <- all(grepl("^\\d{4}-\\d{2}-\\d{2}$", first5))
accident <- accident %>%
  mutate(
    ACCIDENT_DATE = if (is_y_m_d) ymd(ACCIDENT_DATE) else mdy(ACCIDENT_DATE)
  ) %>%
  filter(!is.na(ACCIDENT_DATE))

cat("\nData summary:\n")
cat(sprintf("  - Total accidents: %d\n", nrow(accident)))
cat(sprintf("  - Date range: %s to %s\n", 
            min(accident$ACCIDENT_DATE, na.rm = TRUE),
            max(accident$ACCIDENT_DATE, na.rm = TRUE)))

# ==============================================================================
# 3. Data Cleaning and Feature Engineering
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("DATA CLEANING AND FEATURE ENGINEERING\n")
cat(rep("=", 80), "\n\n", sep = "")

# --- 3.1. Clean accident data and create basic variables ---
accident_m <- accident %>%
  mutate(
    ym = floor_date(ACCIDENT_DATE, "month"),
    year = year(ACCIDENT_DATE),
    month = month(ACCIDENT_DATE),
    SPEED_ZONE = suppressWarnings(as.numeric(SPEED_ZONE)),
    LC = suppressWarnings(as.integer(LIGHT_CONDITION)),
    is_dark = if_else(LC %in% c(3,4,5,6), 1L, 0L),
    is_duskdawn = if_else(LC == 2, 1L, 0L)
  )

# --- 3.2. Monthly accident summary ---
accident_summary_m <- accident_m %>%
  summarise(
    .by = c(ym, year, month),
    Total_Crashes = n(),
    Avg_Speed_Zone = mean(SPEED_ZONE, na.rm = TRUE)
  )

cat("✓ Monthly accident summary created\n")

# --- 3.3. Weather conditions: rain, fog, wind ---
if (!is.null(atmospheric_cond)) {
  weather_per_acc <- atmospheric_cond %>%
    mutate(
      is_rain = ATMOSPH_COND %in% c(2),
      is_fog = ATMOSPH_COND %in% c(4),
      is_wind = ATMOSPH_COND %in% c(7)
    ) %>%
    group_by(ACCIDENT_NO) %>%
    summarise(
      Rain_Event = as.integer(any(is_rain, na.rm = TRUE)),
      Fog_Event = as.integer(any(is_fog, na.rm = TRUE)),
      Wind_Event = as.integer(any(is_wind, na.rm = TRUE)),
      .groups = "drop"
    )
  
  weather_summary_m <- accident_m %>%
    select(ACCIDENT_NO, ym, year, month) %>%
    left_join(weather_per_acc, by = "ACCIDENT_NO") %>%
    mutate(across(c(Rain_Event, Fog_Event, Wind_Event), ~replace_na(., 0L))) %>%
    summarise(
      .by = c(ym, year, month),
      Rainy_Crash_Count = sum(Rain_Event),
      Rainy_Crash_Rate = mean(Rain_Event),
      Fog_Share = mean(Fog_Event),
      Wind_Share = mean(Wind_Event)
    )
  cat("✓ Weather summary created\n")
} else {
  # Create empty weather summary if data not available
  weather_summary_m <- accident_summary_m %>%
    select(ym, year, month) %>%
    mutate(
      Rainy_Crash_Count = 0L,
      Rainy_Crash_Rate = 0,
      Fog_Share = 0,
      Wind_Share = 0
    )
  cat("⚠ Weather data not available, using defaults\n")
}

# --- 3.4. Road surface conditions ---
if (!is.null(road_surface_cond)) {
  surface_per_acc <- road_surface_cond %>%
    mutate(is_wet = SURFACE_COND %in% c(2)) %>%
    group_by(ACCIDENT_NO) %>%
    summarise(Any_Wet_Surface = as.integer(any(is_wet, na.rm = TRUE)), .groups = "drop")
  
  surface_summary_m <- accident_m %>%
    select(ACCIDENT_NO, ym, year, month) %>%
    left_join(surface_per_acc, by = "ACCIDENT_NO") %>%
    mutate(Any_Wet_Surface = replace_na(Any_Wet_Surface, 0L)) %>%
    summarise(
      .by = c(ym, year, month),
      Wet_Surface_Share = mean(Any_Wet_Surface),
      Wet_Surface_Count = sum(Any_Wet_Surface)
    )
  cat("✓ Surface condition summary created\n")
} else {
  # Create empty surface summary if data not available
  surface_summary_m <- accident_summary_m %>%
    select(ym, year, month) %>%
    mutate(
      Wet_Surface_Share = 0,
      Wet_Surface_Count = 0L
    )
  cat("⚠ Surface condition data not available, using defaults\n")
}

# --- 3.5. Urban areas ---
if (!is.null(node)) {
  node_clean <- node %>% distinct(NODE_ID, .keep_all = TRUE)
  
  urban_per_acc <- accident_m %>%
    select(ACCIDENT_NO, NODE_ID, ym, year, month) %>%
    left_join(node_clean %>% select(NODE_ID, DEG_URBAN_NAME), by = "NODE_ID") %>%
    mutate(is_urban = if_else(str_detect(coalesce(DEG_URBAN_NAME, ""), regex("urban", TRUE)), 1L, 0L))
  
  urban_summary_m <- urban_per_acc %>%
    summarise(.by = c(ym, year, month), Urban_Rate = mean(is_urban, na.rm = TRUE))
  cat("✓ Urban area summary created\n")
} else {
  # Create empty urban summary if data not available
  urban_summary_m <- accident_summary_m %>%
    select(ym, year, month) %>%
    mutate(Urban_Rate = 0)
  cat("⚠ Node data not available, using defaults\n")
}

# --- 3.6. Lighting conditions ---
lighting_summary_m <- accident_m %>%
  summarise(
    .by = c(ym, year, month),
    Dark_Share = mean(is_dark, na.rm = TRUE),
    DuskDawn_Share = mean(is_duskdawn, na.rm = TRUE)
  )
cat("✓ Lighting condition summary created\n")

# --- 3.7. Combine all features ---
data_model_m <- accident_summary_m %>%
  left_join(weather_summary_m, by = c("ym", "year", "month")) %>%
  left_join(surface_summary_m, by = c("ym", "year", "month")) %>%
  left_join(urban_summary_m, by = c("ym", "year", "month")) %>%
  left_join(lighting_summary_m, by = c("ym", "year", "month")) %>%
  mutate(
    across(
      c(Rainy_Crash_Rate, Fog_Share, Wind_Share, Wet_Surface_Share, Urban_Rate, Dark_Share, DuskDawn_Share),
      ~replace_na(., 0)
    ),
    across(c(Rainy_Crash_Count, Wet_Surface_Count), ~replace_na(., 0L))
  ) %>%
  arrange(ym) %>%
  mutate(
    time_index = as.numeric(ym - min(ym)),
    Lagged_Crashes = lag(Total_Crashes, 1),
    Rain_Wet_Interaction = Rainy_Crash_Rate * Wet_Surface_Share,
    month_factor = factor(month)  # Create factor variable for effects package compatibility
  )

cat("\n✓ Combined dataset created\n")
cat(sprintf("  - Total months: %d\n", nrow(data_model_m)))
cat(sprintf("  - Variables: %d\n", ncol(data_model_m)))

# ==============================================================================
# 4. Linear Regression Model
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("LINEAR REGRESSION MODEL\n")
cat(rep("=", 80), "\n\n", sep = "")

# Fit improved regression model
# Use month_factor instead of factor(month) for effects package compatibility
lm_month_v2 <- lm(
  Total_Crashes ~ Lagged_Crashes + Avg_Speed_Zone +
    Rainy_Crash_Rate + Wet_Surface_Share + Rain_Wet_Interaction +
    Urban_Rate + Dark_Share + month_factor + time_index,
  data = data_model_m
)

cat("Model summary:\n")
cat(rep("-", 80), "\n", sep = "")
summary(lm_month_v2)

# Create alias for compatibility with notebook code
model <- lm_month_v2

# Create df with only rows used in the model (for compatibility with notebook)
model_rows <- as.numeric(rownames(model$model))
df <- data_model_m[model_rows, ]

# ==============================================================================
# 5. Model Diagnostics - Multicollinearity
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("MODEL DIAGNOSTICS - MULTICOLLINEARITY\n")
cat(rep("=", 80), "\n\n", sep = "")

if (require("car", quietly = TRUE)) {
  library(car)
  vif_values <- vif(model)
  cat("Variance Inflation Factors (VIF):\n")
  cat(rep("-", 80), "\n", sep = "")
  print(vif_values)
  
  if (any(vif_values > 10)) {
    cat("\n⚠ Warning: Some variables have VIF > 10, indicating potential multicollinearity\n")
  } else {
    cat("\n✓ No severe multicollinearity detected (all VIF < 10)\n")
  }
} else {
  cat("⚠ 'car' package not available for VIF calculation\n")
}

# ==============================================================================
# 6. Model Visualization - Coefficient Plot
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("MODEL VISUALIZATION - COEFFICIENTS\n")
cat(rep("=", 80), "\n\n", sep = "")

if (require("broom", quietly = TRUE) && require("ggplot2", quietly = TRUE)) {
  library(broom)
  library(ggplot2)
  
  coefs <- tidy(model) %>% 
    dplyr::filter(term != "(Intercept)")
  
  p_coef <- ggplot(coefs, aes(x = reorder(term, estimate), y = estimate)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                      ymax = estimate + 1.96*std.error), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      x = "Term",
      y = "Estimate",
      title = "Coefficient Estimates with 95% Confidence Intervals"
    ) +
    theme_minimal()
  
  print(p_coef)
  ggsave("charts/lm_coefficients.png", p_coef, width = 10, height = 8, dpi = 300)
  cat("✓ Coefficient plot saved to charts/lm_coefficients.png\n")
}

# Alternative coefficient plot using sjPlot
if (require("sjPlot", quietly = TRUE)) {
  library(sjPlot)
  p_sj <- plot_model(model, type = "est", show.values = TRUE, 
                     value.offset = .3, title = "Model Coefficients")
  print(p_sj)
  ggsave("charts/lm_coefficients_sjplot.png", p_sj, width = 10, height = 8, dpi = 300)
  cat("✓ Alternative coefficient plot saved to charts/lm_coefficients_sjplot.png\n")
}

# ==============================================================================
# 7. Predicted vs Observed
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("PREDICTED VS OBSERVED\n")
cat(rep("=", 80), "\n\n", sep = "")

# Add predictions and residuals to df (matching notebook approach)
df$pred <- predict(model)
df$response_var <- df$Total_Crashes  # For compatibility with notebook

p_pred_obs <- ggplot(df, aes(x = pred, y = response_var)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    x = "Predicted",
    y = "Observed",
    title = "Predicted vs Observed"
  ) +
  theme_minimal()

print(p_pred_obs)
ggsave("charts/lm_predicted_vs_observed.png", p_pred_obs, width = 8, height = 6, dpi = 300)
cat("✓ Predicted vs Observed plot saved\n")

# ==============================================================================
# 8. Model Diagnostics - Residual Plots
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("MODEL DIAGNOSTICS - RESIDUAL PLOTS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Standard diagnostic plots
png("charts/lm_diagnostics.png", width = 12, height = 10, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))
dev.off()
cat("✓ Diagnostic plots saved to charts/lm_diagnostics.png\n")

# Add residuals to df (matching notebook approach)
df$resid <- residuals(model)

# Residuals over time
p_resid_time <- ggplot(df, aes(x = time_index, y = resid)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Time Index",
    y = "Residual",
    title = "Residuals Over Time"
  ) +
  theme_minimal()

print(p_resid_time)
ggsave("charts/lm_residuals_time.png", p_resid_time, width = 10, height = 6, dpi = 300)
cat("✓ Residuals over time plot saved\n")

# ==============================================================================
# 9. Effect Plots
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("EFFECT PLOTS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Base graphics effect plots
if (require("effects", quietly = TRUE)) {
  library(effects)
  png("charts/lm_effects_base.png", width = 12, height = 10, units = "in", res = 300)
  plot(allEffects(model))
  dev.off()
  cat("✓ Effect plots (base graphics) saved to charts/lm_effects_base.png\n")
}

# ggplot2 effect plots
if (require("ggeffects", quietly = TRUE)) {
  library(ggeffects)
  
  # Speed zone effect
  p_speed <- plot(ggpredict(model, terms = "Avg_Speed_Zone"))
  print(p_speed)
  ggsave("charts/lm_effect_speed_zone.png", p_speed, width = 8, height = 6, dpi = 300)
  
  # Urban rate effect
  p_urban <- plot(ggpredict(model, terms = "Urban_Rate"))
  print(p_urban)
  ggsave("charts/lm_effect_urban_rate.png", p_urban, width = 8, height = 6, dpi = 300)
  
  cat("✓ Effect plots (ggplot2) saved\n")
}

# Interaction plot: Rain × Wet Surface
if (require("ggeffects", quietly = TRUE)) {
  p_interaction <- plot(ggpredict(model, 
                                  terms = c("Rainy_Crash_Rate", "Wet_Surface_Share [low, mean, high]"))) +
    labs(title = "Interaction: Rain × Wet Surface")
  print(p_interaction)
  ggsave("charts/lm_interaction_rain_wet.png", p_interaction, width = 10, height = 6, dpi = 300)
  cat("✓ Interaction plot saved\n")
}

# ==============================================================================
# 10. Monthly Effects
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("MONTHLY EFFECTS\n")
cat(rep("=", 80), "\n\n", sep = "")

if (require("broom", quietly = TRUE)) {
  months <- tidy(model) %>%
    filter(grepl("^month_factor", term)) %>%
    mutate(month = as.integer(sub("month_factor", "", term))) %>%
    filter(!is.na(month))  # Remove any rows where extraction failed
  
  p_months <- ggplot(months, aes(x = month, y = estimate)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                      ymax = estimate + 1.96*std.error), width = 0.2) +
    labs(
      x = "Month (vs baseline)",
      y = "Effect",
      title = "Monthly Effects with 95% Confidence Intervals"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = 1:12)
  
  print(p_months)
  ggsave("charts/lm_monthly_effects.png", p_months, width = 10, height = 6, dpi = 300)
  cat("✓ Monthly effects plot saved\n")
}

# ==============================================================================
# 11. Advanced Diagnostics
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("ADVANCED DIAGNOSTICS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Added variable plots
if (require("car", quietly = TRUE)) {
  png("charts/lm_avplots.png", width = 12, height = 8, units = "in", res = 300)
  avPlots(model, terms = ~ Lagged_Crashes + Avg_Speed_Zone + Urban_Rate)
  dev.off()
  cat("✓ Added variable plots saved to charts/lm_avplots.png\n")
}

# Cook's distance and leverage
if (require("ggfortify", quietly = TRUE)) {
  library(ggfortify)
  p_influence <- autoplot(model, which = 4:5)
  print(p_influence)
  ggsave("charts/lm_influence.png", p_influence, width = 10, height = 6, dpi = 300)
  cat("✓ Influence diagnostics saved\n")
}

# Durbin-Watson test for autocorrelation
if (require("lmtest", quietly = TRUE)) {
  library(lmtest)
  dw_test <- dwtest(model)
  cat("\nDurbin-Watson Test for Autocorrelation:\n")
  cat(rep("-", 80), "\n", sep = "")
  print(dw_test)
  
  if (dw_test$p.value < 0.05) {
    cat("\n⚠ Warning: Significant autocorrelation detected (p < 0.05)\n")
  } else {
    cat("\n✓ No significant autocorrelation detected\n")
  }
}

# ==============================================================================
# 12. Cross-Validation
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("CROSS-VALIDATION\n")
cat(rep("=", 80), "\n\n", sep = "")

set.seed(123)
train_index <- sample(1:nrow(data_model_m), 0.8 * nrow(data_model_m))
train <- data_model_m[train_index, ]
test <- data_model_m[-train_index, ]

cat(sprintf("Training set: %d observations (%.1f%%)\n", nrow(train), 100 * nrow(train)/nrow(data_model_m)))
cat(sprintf("Test set: %d observations (%.1f%%)\n", nrow(test), 100 * nrow(test)/nrow(data_model_m)))

# Simplified model for cross-validation (removing some variables that might not be available)
model_cv <- lm(
  Total_Crashes ~ Lagged_Crashes + Avg_Speed_Zone + Urban_Rate + Dark_Share + month_factor,
  data = train
)

pred <- predict(model_cv, newdata = test)
test_r2 <- cor(test$Total_Crashes, pred)^2

cat("\nCross-validation results:\n")
cat(rep("-", 80), "\n", sep = "")
cat(sprintf("Test R²: %.4f\n", test_r2))
cat(sprintf("Test RMSE: %.2f\n", sqrt(mean((test$Total_Crashes - pred)^2))))

# Plot predictions on test set
p_cv <- ggplot(data.frame(Observed = test$Total_Crashes, Predicted = pred),
               aes(x = Predicted, y = Observed)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    x = "Predicted",
    y = "Observed",
    title = paste("Cross-Validation: Predicted vs Observed (Test Set, R² =", round(test_r2, 3), ")")
  ) +
  theme_minimal()

print(p_cv)
ggsave("charts/lm_cross_validation.png", p_cv, width = 8, height = 6, dpi = 300)
cat("✓ Cross-validation plot saved\n")

# ==============================================================================
# Summary
# ==============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALYSIS COMPLETE\n")
cat(rep("=", 80), "\n\n", sep = "")
cat("All charts have been saved to the 'charts' directory:\n")
cat("  - lm_coefficients.png\n")
cat("  - lm_coefficients_sjplot.png\n")
cat("  - lm_predicted_vs_observed.png\n")
cat("  - lm_diagnostics.png\n")
cat("  - lm_residuals_time.png\n")
cat("  - lm_effects_base.png\n")
cat("  - lm_effect_speed_zone.png\n")
cat("  - lm_effect_urban_rate.png\n")
cat("  - lm_interaction_rain_wet.png\n")
cat("  - lm_monthly_effects.png\n")
cat("  - lm_avplots.png\n")
cat("  - lm_influence.png\n")
cat("  - lm_cross_validation.png\n")
cat(rep("=", 80), "\n", sep = "")

