# Load any required packages
library(tidyverse)

# Read in the data
hauntedplaces <- readr::read_csv("https://query.data.world/s/clxkgmmwh3oqlkafnntj2fiqnhq5ci?dws=00000")

# Take a look at this dataframe
glimpse(hauntedplaces)

# How many haunted places are in each state?
hauntedplaces %>%
  dplyr::count(state, sort = TRUE)

haunted_bystate <- hauntedplaces %>%
  dplyr::count(state, sort = TRUE)

view(haunted_bystate)

# What even are some of these places? Let's look at a sample
hauntedplaces %>%
  dplyr::slice_sample(n = 20) %>%
  dplyr::select(location)

# How often is the location a "school"? A "cemetery"?
haunted_location_counts <- hauntedplaces %>%
  mutate(location = str_to_lower(location)) %>%
  mutate(location = case_when(
    str_detect(location, "school") ~ "school",
    str_detect(location, "cemetery|graveyard") ~ "cemetery",
    .default = "other"
  )) %>%
  group_by(state, location) %>%
  summarize(n = n())

view(haunted_location_counts)

# What are the haunted places in New York, NY?
haunted_nyc <- hauntedplaces %>%
  filter(state == "New York" & city %in% c("New York City", "Manhattan"))

view(haunted_nyc)

# How many descriptions mention a "boy" or "girl"
haunted_description_counts <- hauntedplaces %>%
  mutate(description = str_to_lower(description)) %>%
  mutate(description_category = case_when(
    str_detect(description, "boy") ~ "boy",
    str_detect(description, "girl") ~ "girl",
    .default = "other"
  )) %>%
  count(state, description_category)

view(haunted_description_counts)

# What other questions do you want to ask?
