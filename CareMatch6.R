# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(geosphere)  # For better distance calculation
library(lubridate)  # For flexible time parsing

# Define a function to check time slot overlap
check_time_slot_overlap <- function(time_slots1, time_slots2) {
  # Convert time slots into a list of time intervals
  time_intervals1 <- strsplit(time_slots1, ", ")
  time_intervals2 <- strsplit(time_slots2, ", ")

  # Check if there's any overlap between the two sets of time intervals
  overlap <- length(intersect(time_intervals1[[1]], time_intervals2[[1]])) > 0
  return(overlap)
}

# Load CSV files
setwd("C:/MS Data Science - WMU/CS5610-Advanced R/Projects/CareMatch/Project Code/CareMatch")

elderly <- read.csv("elderly.csv")
volunteers <- read.csv("volunteers.csv")
needs <- read.csv("needs.csv")
skills <- read.csv("skills.csv")
elderly_needs <- read.csv("elderly_needs.csv")
volunteer_skills <- read.csv("volunteer_skills.csv")
matches <- read.csv("matches.csv")
notifications <- read.csv("notifications.csv")
historical_matches <- read.csv("historical_matches.csv")
clusters <- read.csv("clusters.csv")

# Check column names of elderly and volunteers to make sure 'preferred_time_slots' and 'availability' are present
cat("Elderly columns: ", colnames(elderly), "\n")
cat("Volunteers columns: ", colnames(volunteers), "\n")

# --- Jaccard Similarity Calculation ---
# Function to calculate Jaccard similarity
jaccard_similarity <- function(set1, set2) {
  if (length(set1) == 0 | length(set2) == 0) return(0)
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  return(intersection / union)
}

# Clean the elderly and volunteer datasets by removing rows with NA lat/lng before joining
elderly_clean <- elderly %>% 
  filter(!is.na(lat) & !is.na(lng))

volunteers_clean <- volunteers %>% 
  filter(!is.na(lat) & !is.na(lng))

# Now perform the join on the cleaned data
matches <- matches %>% 
  left_join(elderly_clean %>% select(elderly_id, lat, lng, radius_preference, preferred_time_slots), by = "elderly_id") %>% 
  left_join(volunteers_clean %>% select(volunteer_id, lat, lng, radius_willingness, availability), by = "volunteer_id") %>% 
  # Rename columns for clarity
  rename(
    elderly_lat = lat.x,
    elderly_lng = lng.x,
    elderly_radius_preference = radius_preference,
    elderly_preferred_time_slots = preferred_time_slots,
    volunteer_lat = lat.y,
    volunteer_lng = lng.y,
    volunteer_radius_willingness = radius_willingness,
    volunteer_availability = availability
  ) %>% 
  # Calculate location distance and time slot overlap
  mutate(
    location_distance = distHaversine(cbind(elderly_lng, elderly_lat), 
                                      cbind(volunteer_lng, volunteer_lat)) / 1000,  # Convert meters to km
    time_slot_overlap = mapply(check_time_slot_overlap, 
                               strsplit(elderly_preferred_time_slots, ", "), 
                               strsplit(volunteer_availability, ", "))
  ) %>% 
  # Filter based on location distance and time slot overlap
  filter(location_distance <= elderly_radius_preference, time_slot_overlap == TRUE)

# --- Machine Learning Integration ---
# Train a Logistic Regression Model
set.seed(123)
train_index <- createDataPartition(historical_matches$accepted, p = 0.8, list = FALSE)
train_data <- historical_matches %>% 
  left_join(elderly_clean %>% select(elderly_id, radius_preference), by = "elderly_id") %>% 
  left_join(volunteers_clean %>% select(volunteer_id, radius_willingness), by = "volunteer_id") %>% 
  rename(
    elderly_radius_preference = radius_preference,
    volunteer_radius_willingness = radius_willingness
  )

# Train logistic regression model
model <- glm(accepted ~ location_distance + time_slot_overlap + elderly_radius_preference + volunteer_radius_willingness, 
             data = train_data, family = binomial)

# Check the model summary
summary(model)

# --- Preparing Test Data ---
# Ensure the accepted column in test_data is a factor with the same levels
test_data$accepted <- factor(test_data$accepted, levels = c(0, 1))

cat("Matches columns: ", colnames(matches), "\n")

# Rename only the necessary columns to avoid duplication
matches <- matches %>% 
  select(-elderly_radius_preference, -volunteer_radius_willingness) %>% 
  left_join(elderly_clean %>% select(elderly_id, radius_preference), by = "elderly_id") %>%
  left_join(volunteers_clean %>% select(volunteer_id, radius_willingness), by = "volunteer_id") %>%
  rename(
    elderly_radius_preference = radius_preference,
    volunteer_radius_willingness = radius_willingness
  )

# Check the updated column names
cat("Updated Matches columns: ", colnames(matches), "\n")

# Ensure test_data contains all the necessary features for prediction
# Join 'elderly_radius_preference' and 'volunteer_radius_willingness' to the test data
test_data <- test_data %>% 
  left_join(elderly_clean %>% select(elderly_id, radius_preference), by = "elderly_id") %>% 
  left_join(volunteers_clean %>% select(volunteer_id, radius_willingness), by = "volunteer_id") 

# Encode time_slot_overlap as a set of binary features for the model (same as training)
# Assuming you have defined the time slots elsewhere in the code
time_slots <- c("10-12", "12-2", "2-4")  # Example time slots
for (slot in time_slots) {
  test_data[paste0("time_slot_", gsub(" ", "_", slot))] <- as.numeric(str_detect(test_data$time_slot_overlap, slot))
}

# --- Make Predictions ---
# Make predictions using the trained model
predictions <- predict(model, newdata = test_data, type = "response")

# Evaluate model performance using confusion matrix
confusionMatrix(as.factor(ifelse(predictions > 0.5, 1, 0)), test_data$accepted)


# --- Predict Acceptance Probability ---
# Add predicted acceptance probability to matches


# Define the time slots (the same time slots as during training)
time_slots <- c("Mon 12-3", "Mon 3-6", "Mon 6-9", "Mon 9-12",
                "Tue 12-3", "Tue 3-6", "Tue 6-9", "Tue 9-12",
                "Wed 12-3", "Wed 3-6", "Wed 6-9", "Wed 9-12",
                "Thu 12-3", "Thu 3-6", "Thu 6-9", "Thu 9-12",
                "Fri 12-3", "Fri 3-6", "Fri 6-9", "Fri 9-12",
                "Sat 12-3", "Sat 3-6", "Sat 6-9", "Sat 9-12",
                "Sun 12-3", "Sun 3-6", "Sun 6-9", "Sun 9-12")

# Create binary columns for the time slot overlaps in the matches dataset
for (slot in time_slots) {
  matches[paste0("time_slot_", gsub(" ", "_", slot))] <- as.numeric(str_detect(matches$time_slot_overlap, slot))
}

# Make predictions using the trained model (now that time_slot_overlap is encoded correctly)
updated_matches <- matches %>% 
  mutate(acceptance_probability = predict(model, newdata = matches, type = "response"))

# View the updated dataset with acceptance probabilities
head(updated_matches)

# Save updated matches
write.csv(updated_matches, "updated_matches.csv", row.names = FALSE)

# --- Notifications ---
# Filter matches with acceptance probability > 70%
notifications <- updated_matches %>% 
  filter(acceptance_probability > 0.7) %>% 
  select(match_id, volunteer_id, elderly_id, match_score, acceptance_probability)

# Save notifications to CSV
write.csv(notifications, "notifications_filtered.csv", row.names = FALSE)
