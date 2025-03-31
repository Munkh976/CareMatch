#
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(geosphere)
library(lubridate)
library(stringr)

# Improved Time Slot Overlap Function
detect_overlap <- function(time_slots1, time_slots2) {
  if (is.na(time_slots1) || is.na(time_slots2)) return(FALSE)
  slots1 <- unlist(strsplit(time_slots1, ", "))
  slots2 <- unlist(strsplit(time_slots2, ", "))
  return(any(slots1 %in% slots2))
}

# Load data
datasets <- list("elderly", "volunteers", "needs", "skills", 
                 "elderly_needs", "volunteer_skills", "matches", 
                 "notifications", "historical_matches", "clusters")

for (dataset in datasets) {
  assign(dataset, read.csv(paste0(dataset, ".csv")))
}

# Clean data
elderly_clean <- elderly %>% filter(!is.na(lat) & !is.na(lng))
volunteers_clean <- volunteers %>% filter(!is.na(lat) & !is.na(lng))

# Match Calculation
matches <- matches %>%
  left_join(elderly_clean %>% select(elderly_id, lat, lng, preferred_time_slots), by = "elderly_id") %>%
  left_join(volunteers_clean %>% select(volunteer_id, lat, lng, radius_willingness, availability), by = "volunteer_id") %>%
  rename(elderly_lat = lat.x, elderly_lng = lng.x, volunteer_lat = lat.y, volunteer_lng = lng.y) %>%
  mutate(
    location_distance = distHaversine(cbind(elderly_lng, elderly_lat), 
                                      cbind(volunteer_lng, volunteer_lat)) / 1000,
    # Creating the time_slot_overlapped with binary (0/1)
    time_slot_overlapped = as.numeric(mapply(detect_overlap, preferred_time_slots, availability))
  ) %>%
  filter(time_slot_overlapped == 1)

# Machine Learning Integration

#Handling Missing Data:
# we can either remove or impute missing values.
# Removing rows with missing values in any column
historical_matches_clean <- na.omit(historical_matches)
# Imputation using the mean or median
historical_matches$location_distance[is.na(historical_matches$location_distance)] <- mean(historical_matches$location_distance, na.rm = TRUE)

#Scaling and Normalizing Features for k-NN.
historical_matches$location_distance_scaled <- scale(historical_matches$location_distance)

#using one-hot encoding or label encoding for categorical variables.
historical_matches <- historical_matches %>%
  mutate(accepted = as.factor(accepted))

set.seed(123)
train_index <- createDataPartition(historical_matches$accepted, p = 0.8, list = FALSE)
train_data <- historical_matches %>%
  left_join(volunteers_clean %>% select(volunteer_id, radius_willingness), by = "volunteer_id") %>%
  filter(!is.na(radius_willingness)) %>%
  mutate(accepted = as.factor(accepted))

model <- glm(accepted ~ location_distance + time_slot_overlapped + radius_willingness, 
             data = train_data, family = binomial)

# Prediction
test_data <- historical_matches %>%
  left_join(volunteers_clean %>% select(volunteer_id, radius_willingness), by = "volunteer_id") %>%
  filter(!is.na(radius_willingness)) %>%
  mutate(accepted = as.factor(accepted))

predictions <- predict(model, newdata = test_data, type = "response")
confusionMatrix(as.factor(ifelse(predictions > 0.5, 1, 0)), test_data$accepted)

# Match Probability Update
updated_matches <- matches %>%
  mutate(
    time_slot_overlapped = as.numeric(time_slot_overlapped),  # Ensuring binary 0/1 format
    acceptance_probability = predict(model, newdata = matches, type = "response")
  )

write.csv(updated_matches, "updated_matches.csv", row.names = FALSE)

# Notifications Filter
notifications <- updated_matches %>%
  filter(acceptance_probability > 0.7) %>%
  select(match_id, volunteer_id, elderly_id, match_score, acceptance_probability)

write.csv(notifications, "notifications_filtered.csv", row.names = FALSE)

library(ggplot2)
ggplot(historical_matches, aes(x = location_distance, fill = accepted)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("red", "lightgreen")) +
  labs(title = "Does Distance Affect Match Acceptance?", x = "Distance (km)", y = "Frequency") +
  theme_minimal() 

#ggplot(data = historical_matches, aes(x = location_distance)) +
 # geom_histogram(bins = 30, fill = 'skyblue', color = 'black') +
 # labs(title = "Distribution of Location Distance", x = "Distance (km)", y = "Frequency")

ggplot(data = historical_matches, aes(x = accepted)) +
  geom_bar(fill = 'lightgreen', color = 'black') +
  labs(title = "Accepted vs Not Accepted", x = "Accepted", y = "Count")

ggplot(data = historical_matches, aes(x = accepted, y = location_distance)) +
  geom_boxplot(fill = 'lightgreen') +
  labs(title = "Location Distance by Acceptance", x = "Accepted", y = "Distance (km)")

# install.packages("corrplot")
library(corrplot)
correlation_matrix <- cor(historical_matches %>% select(location_distance, time_slot_overlapped))
corrplot(correlation_matrix, method = "circle")

summary(historical_matches$location_distance)

mean(historical_matches$location_distance, na.rm = TRUE)
sd(historical_matches$location_distance, na.rm = TRUE)

t.test(location_distance ~ accepted, data = historical_matches)

cor.test(historical_matches$location_distance, historical_matches$time_slot_overlapped)

#install.packages("ggbiplot")
library(ggbiplot)
pca <- prcomp(historical_matches %>% select(location_distance, time_slot_overlapped), center = TRUE, scale. = TRUE)
ggbiplot(pca)

