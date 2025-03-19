library(shiny)
library(dplyr)
library(tidyr)
library(randomNames)
library(zipcodeR)
library(lubridate)

# Load zipcode data
data("zip_code_db")

# --- Fixed Data Generation Functions ---
generate_elderly_profiles <- function(n) {
  data.frame(
    elderly_id = 1:n,
    name = randomNames(n, which.names = "both"),  # FIXED: Changed to "both"
    age = sample(65:95, n, replace = TRUE),
    zipcode = sample(zip_code_db$zipcode, n, replace = TRUE),
    needs = replicate(n, paste(sample(c("Grocery shopping",
                                        "Transportation to appointments",
                                        "Companionship (social activities)",
                                        "Home maintenance (minor repairs)",
                                        "Gardening",
                                        "Cooking meals",
                                        "Housekeeping",
                                        "Pet care",
                                        "Tech support (computers, phones)",
                                        "Errands (post office, dry cleaning)",
                                        "Reading aloud (books, newspapers)",
                                        "Playing games (cards, board games)",
                                        "Walking/exercise assistance",
                                        "Respite care (for family caregivers)",
                                        "Personal care (dressing, bathing - non-medical)",
                                        "Medication reminders (non-medical)",
                                        "Financial management assistance (budgeting)",
                                        "Legal assistance referrals",
                                        "Social event planning",
                                        "Arts and crafts activities",
                                        "Music and entertainment",
                                        "Travel companionship",
                                        "Volunteer coordination",
                                        "Senior center activities",
                                        "Educational programs",
                                        "Accessibility modifications (home)",
                                        "Assistive technology training",
                                        "Home safety assessments",
                                        "Fall prevention assistance",
                                        "Mental wellness support (non-clinical)"), 
                                      sample(1:10, 1), replace = TRUE), collapse = ", ")),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(zipcode = as.character(zipcode)) %>%
    left_join(zip_code_db %>% select(zipcode, major_city, state), by = "zipcode") %>%
    select(elderly_id, name, age, zipcode, major_city, state, needs)
}

# 2. Generate Volunteer Profiles (FIXED)
generate_volunteer_profiles <- function(n) {
  data.frame(
    volunteer_id = 1:n,
    name = randomNames(n, which.names = "both"),  # FIXED: Changed to "both"
    age = sample(18:50, n, replace = TRUE),
    zipcode = sample(zip_code_db$zipcode, n, replace = TRUE),
    skills = replicate(n, paste(sample(c("Grocery shopping assistance",
                                         "Transportation (driving, accompanying)",
                                         "Companionship (social outings, visits)",
                                         "Minor home repairs (plumbing, electrical)",
                                         "Gardening and yard work",
                                         "Meal preparation and cooking",
                                         "Housekeeping and cleaning",
                                         "Pet care (walking, feeding)",
                                         "Tech support (computers, phones, tablets)",
                                         "Running errands (post office, pharmacy)",
                                         "Reading aloud (books, newspapers)",
                                         "Playing games (cards, board games)",
                                         "Walking and exercise assistance",
                                         "Respite care (for family caregivers)",
                                         "Personal care assistance (dressing, bathing)",
                                         "Medication reminders (non-medical)",
                                         "Financial management assistance (budgeting)",
                                         "Legal assistance referrals",
                                         "Social event planning",
                                         "Arts and crafts instruction",
                                         "Music and entertainment (playing instruments, singing)",
                                         "Travel companionship",
                                         "Volunteer coordination",
                                         "Senior center activities organization",
                                         "Educational program development",
                                         "Home accessibility modifications",
                                         "Assistive technology training",
                                         "Home safety assessments",
                                         "Fall prevention assistance",
                                         "Mental wellness support (non-clinical)"), 
                                       sample(1:8, 1), replace = TRUE), collapse = ", ")),
    availability = replicate(n, paste(sample(c("Mon 9-12", "Tue 14-17"), 
                                             sample(2:3, 1), replace = TRUE), collapse = ", ")),
    stringsAsFactors = FALSE
  ) %>% 
    mutate(zipcode = as.character(zipcode)) %>%
    left_join(zip_code_db %>% select(zipcode, major_city, state), by = "zipcode") %>%
    select(volunteer_id, name, age, zipcode, major_city, state, skills, availability)
}


# 3. Generate Care Requests (NO CHANGES NEEDED)
generate_care_requests <- function(elderly, n_per_elderly = 3) {
  bind_rows(lapply(1:nrow(elderly), function(i) {
    data.frame(
      elderly_id = elderly$elderly_id[i],
      city = elderly$major_city[i],
      request_date = Sys.Date() - days(sample(0:30, n_per_elderly)),
      status = sample(c("Completed", "Pending"), n_per_elderly, prob = c(0.7, 0.3), replace = TRUE),
      needs = elderly$needs[i],
      stringsAsFactors = FALSE
    )
  })) %>%
    mutate(request_id = paste0("R", row_number()))
}

# 4. Generate Volunteer Notifications (FIXED)
generate_notifications <- function(volunteers, requests) {
  bind_rows(lapply(1:nrow(volunteers), function(i) {
    matched_requests <- requests %>%
      filter(status == "Pending",
             city == volunteers$major_city[i],
             grepl(paste(strsplit(volunteers$skills[i], ", ")[[1]], collapse = "|"), needs))
    
    if(nrow(matched_requests) > 0) {
      data.frame(
        volunteer_id = volunteers$volunteer_id[i],
        request_id = sample(matched_requests$request_id, min(3, nrow(matched_requests))),
        notification_date = Sys.Date() - days(sample(0:7, min(3, nrow(matched_requests)))),
        status = sample(c("Accepted", "Declined", "Pending"), min(3, nrow(matched_requests)), replace = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }))
}

elderly_profiles <- generate_elderly_profiles(1000)
volunteer_profiles <- generate_volunteer_profiles(1000)
care_requests <- generate_care_requests(elderly_profiles)
notifications <- generate_notifications(volunteer_profiles, care_requests)


# Save data
write.csv(elderly_profiles, "elderly_profiles.csv", row.names = FALSE)
write.csv(volunteer_profiles, "volunteer_profiles.csv", row.names = FALSE)
write.csv(care_requests, "care_requests.csv", row.names = FALSE)
write.csv(notifications, "notifications.csv", row.names = FALSE)
