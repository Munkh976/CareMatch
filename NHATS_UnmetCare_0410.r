# ------------------------------------------
# 1. Libraries and Setup
# ------------------------------------------
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)
library(haven)
library(zipcodeR)
library(ggradar)
library(viridis)
library(scales)
library(GGally)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(corrplot)

# Set working directory
setwd("C:/MS Data Science - WMU/CS5610-Advanced R/Projects/Nhats")

# ------------------------------------------
# 2. Decode Helper Function
# ------------------------------------------
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

# ------------------------------------------
# 3. Load and Prepare NHATS Data
# ------------------------------------------

nhats_sp <- read_sas("NHATS_R13_Final_Release_SAS/NHATS_Round_13_SP_File.sas7bdat")

elderly_data <- nhats_sp %>%
  select(
    spid, r13dgender, hh13dhshldnum, r13d2intvrage,
    el13dhigstschl, rl13dracehisp, ia13totinc,
    re13dcensdiv, r13dmetnonmet,
    pc13walk6blks, pc13walk3blks, pc13up20stair, pc13bendover, pc13rechovrhd, pc13opnjarwhd,
    sc13dreshlp, sc13eathlp, sc13bathhlp, sc13toilhlp, sc13toilwout, sc13bathwout, sc13dreswout,
    ha13shopoft, ha13mealoft, ha13bankoft, ha13launoft, mc13medsrem, mo13beddif,
    dt13getoplcs1:dt13getoplcs7,
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
    across(starts_with("ha13"), ~ decode_nhats(., "assistance")),
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
  
# ------------------------------------------
# 4. Assign Synthetic Location
# ------------------------------------------
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

geo_data$state <- tolower(geo_data$state)
elderly_data$state <- tolower(elderly_data$synthetic_state)

# ------------------------------------------
# 5. Calculate Unmet Needs
# ------------------------------------------
elderly_data <- elderly_data %>%
  mutate(
    needs_transport_help = rowSums(across(starts_with("dt13getoplcs"), ~ . == 1), na.rm = TRUE) > 0,
    unmet_adl_count = rowSums(across(c(
      sc13eathlp, sc13bathhlp, sc13toilhlp, sc13dreshlp,
      sc13toilwout, sc13bathwout, sc13dreswout
    ), ~ . == "Difficulty but no help"), na.rm = TRUE),
    unmet_iadl_count = rowSums(across(c(
      ha13shopoft, ha13mealoft, ha13bankoft, ha13launoft, mc13medsrem
    ), ~ . == 5), na.rm = TRUE) + ifelse(needs_transport_help, 1, 0),
    total_unmet_count = unmet_adl_count + unmet_iadl_count,
    total_unmet_needs = total_unmet_count >= 3,
    unmet_category = case_when(
      total_unmet_count == 0 ~ "No unmet needs",
      total_unmet_count <= 2 ~ "Low unmet needs",
      total_unmet_count <= 5 ~ "Medium unmet needs",
      total_unmet_count > 5 ~ "High unmet needs",
    )
  )

# ------------------------------------------
# 6. Visualization and Summary
# ------------------------------------------

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


# Histogram of unmet needs
ggplot(elderly_data, aes(x = total_unmet_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  labs(title = "Distribution of Total Unmet Needs", x = "Total Unmet Need Count", y = "Number of People") +
  theme_minimal()

# Unmet Needs by Age Group (Stacked)
elderly_data %>%
  count(age_group, unmet_category) %>%
  ggplot(aes(x = age_group, y = n, fill = unmet_category)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Unmet Need Severity by Age Group", x = "Age Group", y = "Proportion", fill = "Category") +
  theme_minimal()

# Summary Table by Age Group
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


# Gender Bar Plot
ggplot(elderly_data, aes(x = gender, fill = unmet_category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Unmet Needs by Gender", y = "Proportion", x = "Gender") +
  theme_minimal()

# Urban vs. Rural Bar Plot
ggplot(elderly_data, aes(x = metro_status, fill = unmet_category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Unmet Needs by Metro Status", y = "Proportion", x = "") +
  theme_minimal()

# Income Group Boxplot
elderly_data %>%
  filter(!is.na(ia13totinc), !is.na(total_unmet_count)) %>%
  mutate(
    income_group = cut(
      ia13totinc,
      breaks = c(0, 20000, 40000, 60000, 80000, Inf),
      labels = c("<20K", "20–40K", "40–60K", "60–80K", "80K+")
    )
  ) %>%
  ggplot(aes(x = income_group, y = total_unmet_count, fill = income_group)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  labs(title = "Distribution of Unmet Needs by Income Group",
       x = "Income Group", y = "Total Unmet Needs") +
  theme_minimal() +
  theme(legend.position = "none")

# Correlation Plot
cor_data <- elderly_data %>%
  select(
    Age = r13d2intvrage,
    Income = ia13totinc,
    Household = hh13dhshldnum,
    Unmet_ADL = unmet_adl_count,
    Unmet_IADL = unmet_iadl_count,
    Total_Unmet = total_unmet_count
  ) %>%
  filter(!is.na(Household))

cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color", type = "lower", addCoef.col = "black",
         tl.col = "black", number.cex = 0.8,
         title = "Correlation Matrix", mar = c(0, 0, 1, 0))

# Memory vs Unmet
ggplot(elderly_data, aes(x = memory_rating, fill = unmet_category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Unmet Needs by Memory Rating", x = "Memory", y = "Proportion") +
  theme_minimal()

# Word Cloud for Unmet Needs
unmet_vars <- c(
  "sc13eathlp", "sc13bathhlp", "sc13toilhlp", "sc13dreshlp",
  "sc13toilwout", "sc13bathwout", "sc13dreswout",
  "ha13shopoft", "ha13mealoft", "ha13bankoft", "ha13launoft",
  "mc13medsrem", "mo13beddif",
  "dt13getoplcs1", "dt13getoplcs2", "dt13getoplcs3",
  "dt13getoplcs4", "dt13getoplcs5", "dt13getoplcs6", "dt13getoplcs7"
)

need_labels <- c(
  sc13eathlp = "Eating", sc13bathhlp = "Bathing", sc13toilhlp = "Toileting", sc13dreshlp = "Dressing",
  sc13toilwout = "Toileting (No help)", sc13bathwout = "Bathing (No help)", sc13dreswout = "Dressing (No help)",
  ha13shopoft = "Shopping", ha13mealoft = "Meals", ha13bankoft = "Banking", ha13launoft = "Laundry",
  mc13medsrem = "Medication Reminders", mo13beddif = "Get of Bed",
  dt13getoplcs1 = "Goto Groceries", dt13getoplcs2 = "Goto Doctor", dt13getoplcs3 = "Goto Pharmacy",
  dt13getoplcs4 = "Goto Religious", dt13getoplcs5 = "Goto Recreation",
  dt13getoplcs6 = "Goto Family", dt13getoplcs7 = "Goto Other"
)

word_data <- elderly_data %>%
  select(all_of(unmet_vars)) %>%
  mutate(across(everything(), as.character)) %>%
  rename_with(~ need_labels[.]) %>%
  pivot_longer(cols = everything(), names_to = "Need", values_to = "Response") %>%
  filter(Response %in% c("Difficulty but no help", "1", "5")) %>%
  count(Need, sort = TRUE)

wordcloud(words = word_data$Need, freq = word_data$n, min.freq = 1, max.words = 100,
          random.order = FALSE, rot.per = 0.25, colors = brewer.pal(8, "Dark2"))
# install.packages("ggridges") if not installed
library(ggridges)

elderly_data %>%
  filter(!is.na(age_group), !is.na(total_unmet_count)) %>%
  ggplot(aes(x = total_unmet_count, y = age_group, fill = age_group)) +
  geom_density_ridges(alpha = 0.7) +
  labs(title = "Unmet Needs Distribution by Age Group",
       x = "Total Unmet Needs", y = "Age Group") +
  theme_minimal() +
  theme(legend.position = "none")

# Mapping frequency codes to weekly counts
freq_to_weekly <- function(x) {
  case_when(
    x == 1 ~ 7,   # daily
    x == 2 ~ 3,   # several times/week
    x == 3 ~ 1,   # once/week
    x == 4 ~ 0.5, # < once/week
    x == 5 ~ 0,   # never
    TRUE ~ NA_real_
  )
}

#📊 Plot: Average Weekly Care Demands by Age Group

elderly_data <- elderly_data %>%
  left_join(nhats_sp %>%
              select(spid, ha13shopoft, ha13mealoft, ha13bankoft, ha13launoft),
            by = "spid", suffix = c("", "_raw")) %>%
  mutate(
    shopping_freq = freq_to_weekly(ha13shopoft_raw),
    meals_freq    = freq_to_weekly(ha13mealoft_raw),
    banking_freq  = freq_to_weekly(ha13bankoft_raw),
    laundry_freq  = freq_to_weekly(ha13launoft_raw),

    weekly_care_demand = rowSums(across(c(shopping_freq, meals_freq, banking_freq, laundry_freq)), na.rm = TRUE)
  )

elderly_data <- elderly_data %>% select(-ends_with("_raw"))


library(dplyr)
library(ggplot2)

# Bar chart: Total Care Demand per Week by Age Group
elderly_data %>%
  filter(!is.na(age_group), !is.na(weekly_care_demand)) %>%
  group_by(age_group) %>%
  summarise(
    total_demand = sum(weekly_care_demand, na.rm = TRUE),
    avg_demand = mean(weekly_care_demand, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(x = age_group, y = total_demand, fill = age_group)) +
  geom_col() +
  labs(
    title = "Total Weekly Care Demands by Age Group",
    x = "Age Group",
    y = "Total Care Tasks per Week",
    fill = "Age Group"
  ) +
  theme_minimal()

#  Estimate Total Volunteer Demand
# We can estimate total volunteer hours needed by assuming:
# Each care task takes ~1 hour.
# Then weekly_care_demand gives you care hours per person per week.
# Multiply by population to estimate total weekly demand.

total_weekly_demand <- sum(elderly_data$weekly_care_demand, na.rm = TRUE)
cat("📦 Estimated Total Monthly Volunteer Care Tasks Needed:", round(total_weekly_demand)*4, "\n")
