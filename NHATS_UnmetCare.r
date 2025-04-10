# analyze_nhats.R
install.packages("ggradar")
# Load necessary libraries

library(dplyr)
library(ggplot2)
library(plotly)
library(maps)
library(haven)
library(zipcodeR)
library(ggradar)
library(viridis)

# Set working directory
setwd("C:/MS Data Science - WMU/CS5610-Advanced R/Projects")

# Load NHATS data
nhats_sp <- read_sas("NHATS_R13_Final_Release_SAS/NHATS_Round_13_SP_File.sas7bdat")

# Decode helper
decode_nhats <- function(x, code_type) {
  case_when(
    code_type == "difficulty" ~ case_when(
      x == 2 ~ "Some difficulty",
      x == 3 ~ "A lot of difficulty",
      x == 4 ~ "Cannot do",
      x < 0 ~ NA_character_,
      TRUE ~ "Unknown"
    ),
    code_type == "assistance" ~ case_when(
      x == 1 ~ "Receives help",
      x == 2 ~ "Difficulty but no help",
      x < 0 ~ NA_character_,
      TRUE ~ "Unknown"
    ),
    code_type == "memory" ~ case_when(
      x == 1 ~ "Excellent",
      x == 2 ~ "Very good",
      x == 3 ~ "Good",
      x == 4 ~ "Fair",
      x == 5 ~ "Poor",
      x < 0 ~ NA_character_,
      TRUE ~ "Unknown"
    )
  )
}

# Prepare and clean NHATS data
elderly_data <- nhats_sp %>%
  select(
    spid, r13dgender, r13d2intvrage, el13dhigstschl, rl13dracehisp, ia13totinc,
    re13dcensdiv, r13dmetnonmet, hh13dhshldnum,

    pc13walk6blks, pc13walk3blks, pc13up20stair, pc13bendover, pc13rechovrhd, pc13opnjarwhd,

    sc13dreshlp, sc13eathlp, sc13bathhlp, sc13toilhlp, sc13toilwout, sc13bathwout, sc13dreswout,
    ha13shopoft, ha13mealoft, ha13bankoft, ha13launoft, mc13medsrem,
    dt13getoplcs1, dt13getoplcs2, dt13getoplcs3, dt13getoplcs4, dt13getoplcs5, dt13getoplcs6, dt13getoplcs7,

    ss13probchswl, ss13seewellst, cp13memrygood
  ) %>%
  mutate(
    across(where(is.numeric), ~ ifelse(. < 0, NA, .)),
    gender = case_when(
      r13dgender == 1 ~ "Male",
      r13dgender == 2 ~ "Female",
      TRUE ~ NA_character_
    ), 
    across(starts_with("pc13"), ~ decode_nhats(., "difficulty")),
    across(starts_with("sc13"), ~ decode_nhats(., "assistance")),
    across(starts_with("ha13"), ~ decode_nhats(., "assistance")),  # ← Add this

    memory_rating = decode_nhats(cp13memrygood, "memory"),
    metro_status = case_when(
      r13dmetnonmet == 1 ~ "Metro",
      r13dmetnonmet == 2 ~ "Non-Metro",
      TRUE ~ NA_character_
    )
  )

elderly_data <- elderly_data %>%
  mutate(
    age_group = case_when(
      r13d2intvrage == 1 ~ "65-69",
      r13d2intvrage == 2 ~ "70-74",
      r13d2intvrage == 3 ~ "75-79",
      r13d2intvrage == 4 ~ "80-84",
      r13d2intvrage == 5 ~ "85-89",
      r13d2intvrage == 6 ~ "90+",
      TRUE ~ NA_character_
    )
  )

# Add synthetic location data
set.seed(123)
geo_data <- zip_code_db %>%
  group_by(state) %>%
  sample_n(1) %>%
  select(zipcode, major_city, state, lat, lng)

elderly_data <- elderly_data %>%
  mutate(
    synthetic_zip = sample(geo_data$zipcode, n(), replace = TRUE),
    synthetic_state = sample(geo_data$state, n(), replace = TRUE)
  ) %>%
  left_join(geo_data, by = c("synthetic_zip" = "zipcode"))

# Standardize state naming
geo_data$state <- tolower(geo_data$state)
elderly_data$state <- tolower(elderly_data$synthetic_state)

# Enrich with unmet needs
elderly_data <- elderly_data %>%
  mutate(
    needs_transport_help = rowSums(across(starts_with("dt13getoplcs"), ~ . == 1), na.rm = TRUE) > 0,

    unmet_adl_count = rowSums(across(c(
      sc13eathlp, sc13bathhlp, sc13toilhlp, sc13dreshlp,
      sc13toilwout, sc13bathwout, sc13dreswout
    ), ~ . == "Difficulty but no help"), na.rm = TRUE),

    unmet_iadl_count = rowSums(across(c(
      ha13shopoft, ha13mealoft, ha13bankoft, ha13launoft, mc13medsrem
    ), ~ . == 5), na.rm = TRUE) +
      ifelse(needs_transport_help, 1, 0),

    total_unmet_count = unmet_adl_count + unmet_iadl_count,
    total_unmet_needs = total_unmet_count >= 3,  # ← threshold stricter here

    unmet_category = case_when(
      total_unmet_count == 0 ~ "None",
      total_unmet_count <= 2 ~ "Low",
      total_unmet_count <= 5 ~ "Medium",
      total_unmet_count > 5 ~ "High"
    )
  )

# Summary by state
care_summary <- elderly_data %>%
  mutate(state = tolower(synthetic_state)) %>%
  group_by(state) %>%
  summarise(
    unmet_care_needs_pct = mean(total_unmet_needs, na.rm = TRUE),
    n = n()
  ) %>%
  left_join(geo_data %>% select(state, lat, lng) %>% distinct(), by = "state")

# Remove AK and HI
exclude_states <- c("ak", "hi")
care_summary <- care_summary %>% filter(!state %in% exclude_states)
state_map <- map_data("state") %>% filter(!region %in% exclude_states)

# Plot interactive map
ggplotly(
  ggplot(care_summary, aes(text = paste0(
    "State: ", state, "\n",
    "Unmet Care Needs: ", scales::percent(unmet_care_needs_pct), "\n",
    "Sample Size: ", n
  ))) +
    geom_polygon(
      data = state_map,
      aes(x = long, y = lat, group = group),
      fill = "gray90", color = "white",
      inherit.aes = FALSE
    ) +
    geom_point(
      aes(x = lng, y = lat, size = unmet_care_needs_pct, color = unmet_care_needs_pct),
      alpha = 0.7
    ) +
    scale_color_viridis_c(option = "magma", labels = scales::percent) +
    scale_size_continuous(range = c(3, 12)) +
    theme_void() +
    labs(title = "Geographic Distribution of Elderly Care Needs"),
  tooltip = "text"
) %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(color = "black")
    )
  )

# Optional: Save summary
write.csv(care_summary, "care_summary.csv", row.names = FALSE)

# Optional: Visualize unmet need distribution
ggplot(elderly_data, aes(x = total_unmet_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", na.rm = TRUE) +
  labs(title = "Distribution of Total Unmet Needs", x = "Total Unmet Need Count", y = "Number of People")

library(scales)

elderly_data %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    percent_unmet = mean(total_unmet_needs, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(x = age_group, y = percent_unmet, fill = age_group)) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Percentage of Elderly with Unmet Needs by Age Group",
    x = "Age Group", y = "% with Unmet Needs"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

elderly_data %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    count = n(),
    avg_unmet = round(mean(total_unmet_count, na.rm = TRUE), 2),
    pct_with_unmet = round(mean(total_unmet_needs, na.rm = TRUE) * 100, 1),
    avg_income = round(mean(ia13totinc, na.rm = TRUE), 1),
    pct_female = round(mean(gender == "Female", na.rm = TRUE) * 100, 1)
  )

elderly_data %>%
  count(age_group, unmet_category) %>%
  ggplot(aes(x = age_group, y = n, fill = unmet_category)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Unmet Need Severity by Age Group",
    x = "Age Group", y = "Proportion",
    fill = "Unmet Category"
  ) +
  theme_minimal()

# Proportion with unmet needs by gender:

elderly_data %>%
  group_by(gender) %>%
  summarise(
    pct_unmet = mean(total_unmet_needs, na.rm = TRUE),
    avg_unmet = mean(total_unmet_count, na.rm = TRUE)
  )

ggplot(elderly_data, aes(x = gender, fill = unmet_category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Unmet Need Category by Gender",
    y = "Proportion", x = "Gender", fill = "Unmet Category"
  ) +
  theme_minimal()

# 2. Rural vs. Urban Comparison

elderly_data %>%
  mutate(metro_status = ifelse(r13dmetnonmet == 1, "Metro", "Non-Metro")) %>%
  group_by(metro_status) %>%
  summarise(
    avg_unmet = mean(total_unmet_count, na.rm = TRUE),
    pct_unmet = mean(total_unmet_needs, na.rm = TRUE)
  )

ggplot(elderly_data, aes(x = metro_status, fill = unmet_category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Unmet Needs by Metro vs Non-Metro Status",
    y = "Proportion", x = "", fill = "Unmet Category"
  ) +
  theme_minimal()

# 3. Average Total Unmet Needs by Income Group

elderly_data %>%
  mutate(
    income_group = cut(ia13totinc,
                       breaks = c(0, 20000, 40000, 60000, 80000, Inf),
                       labels = c("<20K", "20–40K", "40–60K", "60–80K", "80K+"))
  ) %>%
  ggplot(aes(x = income_group, y = total_unmet_count, fill = income_group)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 20, color = "black", size = 3) +
  labs(
    title = "Distribution of Unmet Needs by Income Group",
    x = "Income Group",
    y = "Total Unmet Needs"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# 4. Correlation Matrix (for numeric variables)
install.packages("GGally")
library(GGally)

elderly_data %>%
  select(total_unmet_count, ia13totinc, r13dmetnonmet) %>%
  ggpairs()

# 5. Cognitive or Memory Decline vs Unmet Needs

ggplot(elderly_data, aes(x = memory_rating, fill = unmet_category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Memory Rating and Unmet Care Needs",
       x = "Memory Rating", y = "Proportion", fill = "Unmet Category") +
  theme_minimal()


6.  #Correlation matrix household size vs unmet needs
library(dplyr)
library(ggplot2)
library(GGally)  # for ggcorr
library(corrplot)  # optional: pretty correlation plots
library(scales)

# Select relevant numeric fields
cor_data <- elderly_data %>%
  select(
    age = r13d2intvrage, 
    income = ia13totinc,
    household_size = hh13dhshldnum,
    unmet_adl_count, 
    unmet_iadl_count, 
    total_unmet_count
  ) %>%
  filter(!is.na(household_size)) 

# Check structure and summary
summary(cor_data)

# Compute correlation matrix (pairwise complete to handle NAs)
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

# Visualize using corrplot
corrplot(cor_matrix, method = "color", type = "lower",
         addCoef.col = "black", tl.col = "black", number.cex = 0.8,
         title = "Correlation Matrix: Household Size vs Unmet Needs", mar=c(0,0,1,0))

# Word Cloud for Unmet Needs
#install.packages("wordcloud")
library(dplyr)
library(tidyr)
library(wordcloud)
library(RColorBrewer)

# Define variables
unmet_vars <- c(
  "sc13eathlp", "sc13bathhlp", "sc13toilhlp", "sc13dreshlp",
  "sc13toilwout", "sc13bathwout", "sc13dreswout",
  "ha13shopoft", "ha13mealoft", "ha13bankoft", "ha13launoft",
  "mc13medsrem",
  "dt13getoplcs1", "dt13getoplcs2", "dt13getoplcs3",
  "dt13getoplcs4", "dt13getoplcs5", "dt13getoplcs6", "dt13getoplcs7"
)

# Map readable labels
need_labels <- c(
  sc13eathlp = "Eating", sc13bathhlp = "Bathing", sc13toilhlp = "Toileting", sc13dreshlp = "Dressing",
  sc13toilwout = "Toileting (No help)", sc13bathwout = "Bathing (No help)", sc13dreswout = "Dressing (No help)",
  ha13shopoft = "Shopping", ha13mealoft = "Meals", ha13bankoft = "Banking", ha13launoft = "Laundry",
  mc13medsrem = "Medication Reminders",
  dt13getoplcs1 = "Transportation: Groceries",
  dt13getoplcs2 = "Transportation: Doctor",
  dt13getoplcs3 = "Transportation: Pharmacy",
  dt13getoplcs4 = "Transportation: Religious",
  dt13getoplcs5 = "Transportation: Recreation",
  dt13getoplcs6 = "Transportation: Family",
  dt13getoplcs7 = "Transportation: Other"
)

# Make all variables character for pivoting
word_data <- elderly_data %>%
  select(all_of(unmet_vars)) %>%
  mutate(across(everything(), as.character)) %>%
  rename_with(~ need_labels[.]) %>%
  pivot_longer(cols = everything(), names_to = "Need", values_to = "Response") %>%
  filter(
    Response %in% c("Difficulty but no help", "1", "5")
  ) %>%
  count(Need, sort = TRUE)

# Plot word cloud
set.seed(123)
wordcloud(
  words = word_data$Need,
  freq = word_data$n,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.25,
  colors = brewer.pal(8, "Dark2")
)
