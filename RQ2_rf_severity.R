# --- 1. Load Required Libraries ---
library(tidyverse)
library(randomForest)

sink("my_output.txt") 
# --- 2. Load All Raw Data ---

accidents <- read_csv("data/accident.csv") %>%
  select(ACCIDENT_NO, SEVERITY, LIGHT_CONDITION, ROAD_GEOMETRY, SPEED_ZONE)

road_surface <- read_csv("data/road_surface_cond.csv") %>%
  select(ACCIDENT_NO, SURFACE_COND_DESC)

vehicles <- read_csv("data/vehicle.csv") %>%
  select(ACCIDENT_NO, SEATING_CAPACITY)

# --- 3. Pre-process & Aggregate Data ---

# Find the min and max seating capacity for each accident
vehicles_agg <- vehicles %>%
  group_by(ACCIDENT_NO) %>%
  summarise(
    # na.rm=TRUE handles NAs *before* aggregation
    MIN_SEATING_CAPACITY = min(SEATING_CAPACITY, na.rm = TRUE),
    MAX_SEATING_CAPACITY = max(SEATING_CAPACITY, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # If an accident had NO seating data, min/max return Inf.
  # Turn these Inf values into NA to impute them later.
  mutate(
    MIN_SEATING_CAPACITY = if_else(is.infinite(MIN_SEATING_CAPACITY), 
                                   NA_real_, 
                                   MIN_SEATING_CAPACITY),
    MAX_SEATING_CAPACITY = if_else(is.infinite(MAX_SEATING_CAPACITY), 
                                   NA_real_, 
                                   MAX_SEATING_CAPACITY)
  )


# --- 4. Feature Engineering and Merging ---

model_data <- accidents %>%
  # Join the road surface data
  left_join(road_surface, by = "ACCIDENT_NO") %>%
  
  # Join our new, aggregated vehicle data
  left_join(vehicles_agg, by = "ACCIDENT_NO") %>%
  
  mutate(
    # 1. Binary features
    is_wet = (SURFACE_COND_DESC == "Wet") %>% replace_na(FALSE),
    is_dark = (LIGHT_CONDITION < 3) %>% replace_na(FALSE),
    is_intersect = (ROAD_GEOMETRY < 5) %>% replace_na(FALSE),
    
    # 2. Clean SPEED_ZONE feature, "999", "777", "888" are magic numbers, assign to NA
    SPEED_ZONE_NUM = if_else(SPEED_ZONE %in% c("999", "777", "888"), 
                             NA_character_, 
                             SPEED_ZONE) %>% as.numeric(),
    
    # 3. Re-frame the problem into 2 classes
    SEVERITY_CLASS = case_when(
      SEVERITY == 3 ~ "Severe",
      SEVERITY < 3  ~ "Not_Severe",
      TRUE ~ NA_character_
    ),
    SEVERITY_CLASS = as.factor(SEVERITY_CLASS)
  )


# --- 5. Impute Missing (NA) Feature Values ---

# Calculate medians for all numeric features
median_speed <- median(model_data$SPEED_ZONE_NUM, na.rm = TRUE)

median_min_seat <- median(model_data$MIN_SEATING_CAPACITY, na.rm = TRUE)
median_max_seat <- median(model_data$MAX_SEATING_CAPACITY, na.rm = TRUE)

print(paste("Median speed (used for NA):", median_speed))
print(paste("Median MIN seating (used for NA):", median_min_seat))
print(paste("Median MAX seating (used for NA):", median_max_seat))

# Fill in all NAs with their respective medians
model_data <- model_data %>%
  mutate(
    SPEED_ZONE_NUM = replace_na(SPEED_ZONE_NUM, median_speed),
    
    MIN_SEATING_CAPACITY = replace_na(MIN_SEATING_CAPACITY, median_min_seat),
    MAX_SEATING_CAPACITY = replace_na(MAX_SEATING_CAPACITY, median_max_seat)
  )


# --- 6. Select Final Model Columns ---

model_data <- model_data %>%
  select(
    # Target variable
    SEVERITY_CLASS, 
    
    # Predictor features
    is_wet, 
    is_dark, 
    is_intersect, 
    SPEED_ZONE_NUM,
    MIN_SEATING_CAPACITY,
    MAX_SEATING_CAPACITY
  ) %>%
  na.omit()


# Check the structure of our final data
print("--- Final Model Data Structure ---")
str(model_data)

print("--- Class Distribution ---")
print(table(model_data$SEVERITY_CLASS))


# --- 7. Split Data into Training and Testing Sets ---

set.seed(123) 
sample_size <- floor(0.80 * nrow(model_data))
train_indices <- sample(seq_len(nrow(model_data)), size = sample_size)

train_set <- model_data[train_indices, ]
test_set <- model_data[-train_indices, ]


# --- 8. Train the Random Forest Model ---

class_counts <- table(train_set$SEVERITY_CLASS)
print("--- Training Set Class Counts ---")
print(class_counts)

min_class_size <- min(class_counts)
print(paste("Smallest class size (for sampling):", min_class_size))

rf_model <- randomForest(
  SEVERITY_CLASS ~ ., 
  data = train_set,
  ntree = 100,
  # 6 features. mtry = sqrt(6) = 2.45 ~ round to 2
  mtry = 2, 
  
  # Imbalance Fix: down-sample larger class to match smaller class
  strata = train_set$SEVERITY_CLASS,
  sampsize = rep(min_class_size, n = 2) 
)


# --- 9. Evaluate the Model ---

print("--- Model Summary ---")
print(rf_model)

print("--- Feature Importance ---")
print(importance(rf_model))

print("--- Confusion Matrix (Actual vs. Predicted) ---")
predictions <- predict(rf_model, test_set)
confusion_matrix <- table(Actual = test_set$SEVERITY_CLASS, Predicted = predictions)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Overall Accuracy on Test Set:", round(accuracy * 100, 2), "%"))
  
sink()