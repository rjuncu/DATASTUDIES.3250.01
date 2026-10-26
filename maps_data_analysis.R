install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidytext")
library(dplyr)
library(ggplot2)
library(tidyr)
getwd()

list.files()

setwd("./US_maps_data/")

# Glyphosate usage from USGS is available at this link: 
# https://water.usgs.gov/nawqa/pnsp/usage/maps/county-level/StateLevel/AgPestUsebyCropGroup92to16.zip
# The table 'glyphosate_2016_merged_nhl.csv' uses data from this link
# The rows were selected for Compound = Glyphosate, summed by crop, and summed 1,200 rows <- 48 states * 25 years
# Then the glyphosate amounts were divided by 2020 census data to achieve "per capita" units similar to NHL data
# The NHL cancer by race data was taken from NHL cancer atlas interactive map tool 
glyphosate_usage <- read.csv("./glyphosate_2016_merged_nhl.csv")
general_pop <- read.csv("./Incidence_US_by_State_All_Races_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv")
population_black <- read.csv("./Incidence_US_by_State_Non-Hispanic_Black_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv")
population_hispanic <- read.csv("./Incidence_US_by_State_Hispanic_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv")
population_native <- read.csv("./Incidence_US_by_State_Non-Hispanic_American_Indian_Alaskan_Native_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv")
population_white <- read.csv("./Incidence_US_by_State_Non-Hispanic_White_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv")
population_asian <- read.csv("./Incidence_US_by_State_Non-Hispanic_Asian_Pacific_Islander_Non-Hodgkin_Lymphoma_(Both_Sexes)_2018_-_2022.csv")

head(general_pop)

glyphosate_usage <- glyphosate_usage %>%
  select(Area, Glyphosate_kg_per_100k)

general_pop <- general_pop %>% 
  select(Area, Age.Adjusted.Rate.per.100.000) %>%
  rename_at('Age.Adjusted.Rate.per.100.000', ~"General_pop")

population_black <- population_black %>%
  select(Area, Age.Adjusted.Rate.per.100.000) %>%
  rename_at('Age.Adjusted.Rate.per.100.000', ~'Black_pop')

population_asian <- population_asian %>%
  select(Area, Age.Adjusted.Rate.per.100.000) %>%
  rename_at('Age.Adjusted.Rate.per.100.000', ~'Asian_pop')

population_hispanic <- population_hispanic %>%
  select(Area, Age.Adjusted.Rate.per.100.000) %>%
  rename_at('Age.Adjusted.Rate.per.100.000', ~'Hispanic_pop')

population_native <- population_native %>%
  select(Area, Age.Adjusted.Rate.per.100.000) %>%
  rename_at('Age.Adjusted.Rate.per.100.000', ~'Native_pop')

population_white <- population_white %>%
  select(Area, Age.Adjusted.Rate.per.100.000) %>%
  rename_at('Age.Adjusted.Rate.per.100.000', ~'White_pop')

master_data <- inner_join(glyphosate_usage, general_pop, by='Area')
master_data <- inner_join(master_data, population_black, by = 'Area')
master_data <- inner_join(master_data, population_asian, by='Area')
master_data <- inner_join(master_data, population_hispanic, by='Area')
master_data <- inner_join(master_data, population_native, by='Area')
master_data <- inner_join(master_data, population_white, by='Area')

head(master_data)
summary(master_data)

# Make sure measurement columns are numeric
master_data <- master_data %>%
  mutate(
    across(
      -Area,
      ~ as.numeric(na_if(as.character(.x), "No Data/Suppressed"))
    )
  )


# GRAPH 1: TOP 10 STATES BY CANCER INCIDENCE
# Select the six population incidence variables
population_long <- master_data %>%
  select(
    Area,
    General_pop,
    Black_pop,
    Asian_pop,
    Hispanic_pop,
    Native_pop,
    White_pop
  ) %>%
  pivot_longer(
    cols = -Area,
    names_to = "Population",
    values_to = "Incidence"
  ) %>%
  filter(!is.na(Incidence))

# Find the top 10 states for each population
top_population_states <- population_long %>%
  group_by(Population) %>%
  slice_max(
    order_by = Incidence,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup()

# Plot all populations together in ONE graph
ggplot(
  top_population_states,
  aes(
    x = Incidence,
    y = reorder(Area, Incidence),
    fill = Population
  )
) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  ) +
  labs(
    title = "Top 10 States by Non-Hodgkin Lymphoma Incidence",
    subtitle = "Age-adjusted incidence rates by population group, 2018–2022",
    x = "Incidence Rate per 100,000",
    y = "State",
    fill = "Population"
  )



# GRAPH 2: TOP 15 STATES BY GLYPHOSATE USE
top_glyphosate_states <- master_data %>%
  filter(!is.na(Glyphosate_kg_per_100k)) %>%
  slice_max(
    order_by = Glyphosate_kg_per_100k,
    n = 15,
    with_ties = FALSE
  ) %>%
  arrange(Glyphosate_kg_per_100k) %>%
  mutate(
    Area = factor(Area, levels = Area)
  )

ggplot(
  top_glyphosate_states,
  aes(
    x = Glyphosate_kg_per_100k,
    y = Area
  )
) +
  geom_col(fill = "darkolivegreen3") +
  theme_minimal() +
  labs(
    title = "Top 15 States by Glyphosate Usage",
    subtitle = "Glyphosate use per 100,000 people",
    x = "Glyphosate (kg per 100,000)",
    y = "State"
  )








