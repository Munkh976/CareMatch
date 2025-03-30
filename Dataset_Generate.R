## Load necessary libraries
library(dplyr)
library(tidyr)
library(randomNames)
library(zipcodeR)
library(lubridate)

# Load zipcode data
data("zip_code_db")

# --- Data Generation Functions ---

# 1. Generate Elderly Profiles
generate_elderly_profiles <- function(n) {
  data.frame(
    elderly_id = 1:n,
    name = randomNames(n, which.names = "both"),
    age = sample(65:95, n, replace = TRUE),
    zipcode = sample(zip_code_db$zipcode, n, replace = TRUE),
    preferred_time_slots = replicate(n, paste(sample(c("Mon 9-12", "Mon 12-3", "Mon 3-6", "Mon 6-9",
                                 "Tue 9-12", "Tue 12-3", "Tue 3-6", "Tue 6-9",
                                 "Wed 9-12", "Wed 12-3", "Wed 3-6", "Wed 6-9",
                                 "Thu 9-12", "Thu 12-3", "Thu 3-6", "Thu 6-9",
                                 "Fri 9-12", "Fri 12-3", "Fri 3-6", "Fri 6-9",
                                 "Sat 9-12", "Sat 12-3", "Sat 3-6", "Sat 6-9",
                                 "Sun 9-12", "Sun 12-3", "Sun 3-6", "Sun 6-9"), sample(2:3, 1), replace = TRUE), collapse = ", ")),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(zipcode = as.character(zipcode)) %>%
    left_join(zip_code_db %>% select(zipcode, major_city, state, lat, lng), by = "zipcode") %>%  # Use lat and lng for coordinates
    select(elderly_id, name, age, major_city, state, lat, lng, preferred_time_slots)
}

# 2. Generate Volunteer Profiles
generate_volunteer_profiles <- function(n) {
  data.frame(
    volunteer_id = 1:n,
    name = randomNames(n, which.names = "both"),
    zipcode = sample(zip_code_db$zipcode, n, replace = TRUE),
    availability = replicate(n, paste(sample(c("Mon 9-12", "Mon 12-3", "Mon 3-6", "Mon 6-9",
                                 "Tue 9-12", "Tue 12-3", "Tue 3-6", "Tue 6-9",
                                 "Wed 9-12", "Wed 12-3", "Wed 3-6", "Wed 6-9",
                                 "Thu 9-12", "Thu 12-3", "Thu 3-6", "Thu 6-9",
                                 "Fri 9-12", "Fri 12-3", "Fri 3-6", "Fri 6-9",
                                 "Sat 9-12", "Sat 12-3", "Sat 3-6", "Sat 6-9",
                                 "Sun 9-12", "Sun 12-3", "Sun 3-6", "Sun 6-9"), sample(2:3, 1), replace = TRUE), collapse = ", ")),
    radius_willingness = sample(10:30, n, replace = TRUE),
    acceptance_probability = runif(n, 0.5, 1),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(zipcode = as.character(zipcode)) %>%
    left_join(zip_code_db %>% select(zipcode, major_city, state, lat, lng), by = "zipcode") %>%  # Use lat and lng for coordinates
    select(volunteer_id, name, major_city, state, lat, lng, availability, radius_willingness, acceptance_probability)
}

# 3. Generate Needs
generate_needs <- function() {
  data.frame(
    need_id = 1:10,
    need_name = c("Grocery help", "Transportation", "Meal Prep", "Housekeeping", "Companionship", 
                  "Medical Assistance", "Shopping", "Errands", "Pet Care", "Gardening")
  )
}

# 4. Generate Skills
generate_skills <- function() {
  data.frame(
    skill_id = 1:10,
    skill_name = c("Driving", "Cooking", "Cleaning", "Nursing", "Gardening", 
                   "Shopping", "Errands", "Pet Care", "Companionship", "First Aid")
  )
}

# 5. Generate Elderly_Needs
generate_elderly_needs <- function(elderly, needs) {
  data.frame(
    elderly_id = sample(elderly$elderly_id, 2000, replace = TRUE),
    need_id = sample(needs$need_id, 2000, replace = TRUE)
  )
}

# 6. Generate Volunteer_Skills
generate_volunteer_skills <- function(volunteers, skills) {
  data.frame(
    volunteer_id = sample(volunteers$volunteer_id, 2000, replace = TRUE),
    skill_id = sample(skills$skill_id, 2000, replace = TRUE)
  )
}

# 7. Generate Matches
generate_matches <- function(elderly, volunteers, elderly_needs, volunteer_skills) {
  data.frame(
    match_id = 1:1000,
    elderly_id = sample(elderly$elderly_id, 1000, replace = TRUE),
    volunteer_id = sample(volunteers$volunteer_id, 1000, replace = TRUE),
    match_score = runif(1000, 0.5, 1),
    location_distance = runif(1000, 1, 20),
    acceptance_probability = runif(1000, 0.5, 1),
    status = sample(c("Pending", "Accepted", "Rejected"), 1000, replace = TRUE)
  )
}

# 8. Generate Notifications
generate_notifications <- function(matches) {
  data.frame(
    notification_id = 1:1000,
    match_id = sample(matches$match_id, 1000, replace = TRUE),
    volunteer_id = sample(matches$volunteer_id, 1000, replace = TRUE),
    notification_time = Sys.time() - runif(1000, 0, 86400 * 30),  # Random time in the last 30 days
    status = sample(c("Sent", "Read", "Accepted", "Rejected"), 1000, replace = TRUE)
  )
}

# 9. Generate Historical_Matches
generate_historical_matches <- function() {
  data.frame(
    historical_match_id = 1:1000,
    elderly_id = sample(1:1000, 1000, replace = TRUE),
    volunteer_id = sample(1:1000, 1000, replace = TRUE),
    match_score = runif(1000, 0.5, 1),
    time_slot_overlapped = sample(0:1, 1000, replace = TRUE),
    location_distance = runif(1000, 1, 20),
    accepted = sample(0:1, 1000, replace = TRUE)
  )
}

# 10. Generate Clusters
generate_clusters <- function() {
  data.frame(
    cluster_id = 1:1000,
    cluster_type = sample(c("Urban Grocery", "Rural Transportation"), 1000, replace = TRUE),
    elderly_id = sample(1:1000, 1000, replace = TRUE),
    volunteer_id = sample(1:1000, 1000, replace = TRUE)
  )
}

# --- Generate All Datasets ---
elderly <- generate_elderly_profiles(1000)
volunteers <- generate_volunteer_profiles(1000)

needs <- generate_needs()
skills <- generate_skills()
elderly_needs <- generate_elderly_needs(elderly, needs)
volunteer_skills <- generate_volunteer_skills(volunteers, skills)
matches <- generate_matches(elderly, volunteers, elderly_needs, volunteer_skills)
notifications <- generate_notifications(matches)
historical_matches <- generate_historical_matches()
clusters <- generate_clusters()


# --- Export Datasets to CSV ---
write.csv(elderly, "elderly.csv", row.names = FALSE)
write.csv(volunteers, "volunteers.csv", row.names = FALSE)
write.csv(needs, "needs.csv", row.names = FALSE)
write.csv(skills, "skills.csv", row.names = FALSE)
write.csv(elderly_needs, "elderly_needs.csv", row.names = FALSE)
write.csv(volunteer_skills, "volunteer_skills.csv", row.names = FALSE)
write.csv(matches, "matches.csv", row.names = FALSE)
write.csv(notifications, "notifications.csv", row.names = FALSE)
write.csv(historical_matches, "historical_matches.csv", row.names = FALSE)
write.csv(clusters, "clusters.csv", row.names = FALSE)

# Print confirmation
cat("All datasets have been created and exported to CSV files.\n")

