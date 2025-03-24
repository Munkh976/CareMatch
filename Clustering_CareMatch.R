# CareMatch - Clustering Module (Fixed Duplicates Handling)
library(tidyverse)
library(cluster)
library(factoextra)
library(leaflet)

# --- 1. Load Data ---
elderly <- read_csv("elderly.csv") %>% 
  select(elderly_id, lat, lng, preferred_time_slots)

volunteers <- read_csv("volunteers.csv") %>% 
  select(volunteer_id, lat, lng, availability, radius_willingness)

# --- 2. Handle Duplicates Before Pivoting ---
elderly_needs <- read_csv("elderly_needs.csv") %>%
  distinct(elderly_id, need_id, .keep_all = TRUE)  # Remove duplicates

volunteer_skills <- read_csv("volunteer_skills.csv") %>%
  distinct(volunteer_id, skill_id, .keep_all = TRUE)  # Remove duplicates

# --- 3. Feature Engineering ---
combined_data <- bind_rows(
  elderly %>% 
    transmute(
      id = elderly_id,
      type = "elderly",
      lat, lng,
      time_slots = preferred_time_slots
    ),
  
  volunteers %>% 
    transmute(
      id = volunteer_id, 
      type = "volunteer",
      lat, lng,
      time_slots = availability
    )
)

# Time slot encoding
time_slot_levels <- c(
  "Mon 9-12", "Mon 12-3", "Mon 3-6", "Mon 6-9",
  "Tue 9-12", "Tue 12-3", "Tue 3-6", "Tue 6-9",
  "Wed 9-12", "Wed 12-3", "Wed 3-6", "Wed 6-9",
  "Thu 9-12", "Thu 12-3", "Thu 3-6", "Thu 6-9",
  "Fri 9-12", "Fri 12-3", "Fri 3-6", "Fri 6-9",
  "Sat 9-12", "Sat 12-3", "Sat 3-6", "Sat 6-9",
  "Sun 9-12", "Sun 12-3", "Sun 3-6", "Sun 6-9"
)

for (slot in time_slot_levels) {
  combined_data[[str_replace_all(slot, " ", "_")]] <- as.integer(
    str_detect(combined_data$time_slots, fixed(slot))
  )
}

# Create needs/skills matrices with duplicate handling
needs_matrix <- elderly_needs %>%
  group_by(elderly_id, need_id) %>%
  summarise(has_need = 1L, .groups = "drop") %>%
  pivot_wider(
    id_cols = elderly_id,
    names_from = need_id,
    names_prefix = "need_",
    values_from = has_need,
    values_fill = 0L
  )

skills_matrix <- volunteer_skills %>%
  group_by(volunteer_id, skill_id) %>%
  summarise(has_skill = 1L, .groups = "drop") %>%
  pivot_wider(
    id_cols = volunteer_id,
    names_from = skill_id,
    names_prefix = "skill_",
    values_from = has_skill,
    values_fill = 0L
  )

# Merge all features
cluster_data <- combined_data %>%
  left_join(needs_matrix, by = c("id" = "elderly_id")) %>%
  left_join(skills_matrix, by = c("id" = "volunteer_id")) %>%
  mutate(across(starts_with("need_") | starts_with("skill_"), ~ coalesce(., 0L)))

# --- 4. Prepare Cluster Features ---
cluster_features <- cluster_data %>%
  select(
    lat, lng,
    starts_with("Mon_"), starts_with("Tue_"), starts_with("Wed_"),
    starts_with("Thu_"), starts_with("Fri_"), starts_with("Sat_"), starts_with("Sun_"),
    starts_with("need_"), starts_with("skill_")
  ) %>%
  mutate(across(everything(), as.numeric))

# --- 5. Clustering (Fixed) ---
set.seed(123)
k <- 4

# Identify complete cases
complete_cases <- complete.cases(cluster_features)
cluster_data_clean <- cluster_data[complete_cases, ]
features_clean <- cluster_features[complete_cases, ]

# Perform clustering
kmeans_result <- kmeans(scale(features_clean), centers = k, nstart = 25)
cluster_data_clean$cluster <- factor(kmeans_result$cluster)

# --- 6. Save Results ---
elderly <- elderly %>%
  left_join(
    cluster_data_clean %>% 
      filter(type == "elderly") %>% 
      select(elderly_id = id, cluster),
    by = "elderly_id"
  ) %>%
  mutate(cluster = factor(cluster))  # Ensure factor type

volunteers <- volunteers %>%
  left_join(
    cluster_data_clean %>% 
      filter(type == "volunteer") %>% 
      select(volunteer_id = id, cluster),
    by = "volunteer_id"
  ) %>%
  mutate(cluster = factor(cluster))



write_csv(elderly, "elderly_with_clusters.csv")
write_csv(volunteers, "volunteers_with_clusters.csv")

# --- 7. Cluster Analysis (Fixed) ---
cluster_summary <- cluster_data_clean %>%
  group_by(cluster, type) %>%
  summarise(
    n = n(),
    avg_lat = mean(lat, na.rm = TRUE),
    avg_lng = mean(lng, na.rm = TRUE),
    across(
      c(starts_with("Mon_"), starts_with("Tue_"), starts_with("Wed_"),
        starts_with("Thu_"), starts_with("Fri_"), 
        starts_with("Sat_"), starts_with("Sun_"),
        starts_with("need_"), starts_with("skill_")),
      \(x) mean(x, na.rm = TRUE)  # Updated anonymous function syntax
    ),
    .groups = "drop"
  )

# --- 8. Enhanced Visualization ---
library(ggplot2)

# 1. Time Slot Heatmap
time_slot_data <- cluster_summary %>%
  select(cluster, type, matches("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun)_")) %>%
  pivot_longer(cols = -c(cluster, type), 
               names_to = "time_slot", 
               values_to = "prevalence") %>%
  mutate(time_slot = str_replace(time_slot, "_", " "))

ggplot(time_slot_data, aes(x = time_slot, y = factor(cluster), fill = prevalence)) +
  geom_tile(color = "white") +
  facet_wrap(~type, ncol = 1) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Time Slot Availability by Cluster",
       x = "Time Slot",
       y = "Cluster",
       fill = "Prevalence")

# 2. Needs/Skills Radar Chart
if (requireNamespace("ggiraphExtra", quietly = TRUE)) {
  needs_skills_radar <- cluster_summary %>%
    select(cluster, type, starts_with("need_"), starts_with("skill_")) %>%
    pivot_longer(cols = -c(cluster, type),
                 names_to = "feature",
                 values_to = "prevalence") %>%
    mutate(feature_type = ifelse(str_detect(feature, "need_"), "Need", "Skill"))
  
  ggiraphExtra::ggRadar(
    data = needs_skills_radar,
    aes(group = cluster, facet = type, colour = feature_type),
    rescale = FALSE,
    interactive = FALSE
  ) +
    labs(title = "Needs and Skills by Cluster") +
    theme(legend.position = "right")
}

# 3. Geographic Cluster Centers
cluster_centers <- cluster_summary %>%
  select(cluster, type, avg_lat, avg_lng) %>%
  distinct()

leaflet(cluster_centers) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~avg_lng,
    lat = ~avg_lat,
    color = ~factor(cluster),
    popup = ~paste("Cluster:", cluster, "<br>Type:", type),
    radius = 10
  ) %>%
  addLegend(
    position = "bottomright",
    colors = scales::hue_pal()(length(unique(cluster_centers$cluster))),
    labels = unique(cluster_centers$cluster),
    title = "Cluster Centers"
  )
