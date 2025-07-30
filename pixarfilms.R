# install the package that has the data we want
install.packages("pixarfilms")

# load the packages we need
library(tidyverse)
library(pixarfilms)

# take a look at the data in here
glimpse(pixar_films)
glimpse(pixar_people)
glimpse(genres)
glimpse(box_office)
glimpse(public_response)
glimpse(academy)

# How many Pixar films have been nominated for or won for best score or best song?
pixar_academy_music <- academy %>%
  filter(award_type %in% c("Original Score","Original Song"))

length(unique(pixar_academy_music$film))

length(unique(pixar_academy_music$film))/length(unique(academy$film))

# Who is responsible for those musical Academy accolades?
pixar_people_music <- pixar_people %>%
  filter(role_type == "Musician")

pixar_academy_people_joined <- pixar_academy_music %>%
  left_join(pixar_people_music, by = "film") %>%
  group_by(name) %>%
  summarise(n = n())

# How much does an average Pixar film cost and make worldwide?
pixar_joined <- pixar_films %>%
  left_join(public_response, by="film") %>%
  left_join(box_office, by="film")

mean(pixar_joined$budget, na.rm = T)
options(scipen = 999)
mean(pixar_joined$box_office_worldwide, na.rm = T)

# How have runtimes for Pixar films changed over time?
pixar_joined %>%
  filter(!is.na(run_time)) %>%
  ggplot(aes(x=release_date, y=run_time)) +
  geom_line() +
  theme_minimal()

# which Pixar film had the highest rating from the public?
public_response_cleaned <- public_response %>%
  select(-cinema_score) %>%
  mutate(film = fct_inorder(film)) %>%
  pivot_longer(cols = c("rotten_tomatoes", "metacritic", "critics_choice"),
               names_to = "ratings",
               values_to = "value") %>%
  drop_na()

### visualize this!

# this gives you just a blank plot...
public_response_cleaned %>%
  ggplot()

# ...because we haven't said what we want to map and with what geometries
public_response_cleaned %>%
  ggplot(mapping = aes(x=film, y=value))

public_response_cleaned %>%
  ggplot(mapping = aes(x=film, y=value)) +
  geom_point()

# but all the dots are kinda scattered still...let's connect them
public_response_cleaned %>%
  ggplot(mapping = aes(x=film, y=value)) +
  geom_point() +
  geom_line(aes(group = ratings))

# no way to tell the lines apart, so let's add a color distinction
public_response_cleaned %>%
  ggplot(aes(x = film, y = value, color = ratings)) + # note that both the point AND line have matching colors
  geom_point() +
  geom_line(aes(group = ratings))

# let's adjust the color palette to something a bit easier on the eyes
public_response_cleaned %>%
  ggplot(aes(x = film, y = value, color = ratings)) +
  geom_point() +
  geom_line(aes(group = ratings)) +
  scale_color_brewer(palette = "Dark2")

# let's tweak the axes labels
public_response_cleaned %>%
  ggplot(aes(x = film, y = value, color = ratings)) +
  geom_point() +
  geom_line(aes(group = ratings)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value")

# the grey background is kinda, we want something a bit cleaner
public_response_cleaned %>%
  ggplot(aes(x = film, y = value, color = ratings)) +
  geom_point() +
  geom_line(aes(group = ratings)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value") +
  theme_minimal()

# can't actually read anything on the x-axis, and want to move the legend to the bottom
public_response_cleaned %>%
  ggplot(aes(x = film, y = value, color = ratings)) +
  geom_point() +
  geom_line(aes(group = ratings)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom")

# what if, instead of a line plot, I wanted to make a bar chart?
public_response_cleaned %>%
  ggplot(aes(x = film, y = value, color = ratings)) +
  geom_col() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom")

# that doesn't look right...
public_response_cleaned %>%
  ggplot(aes(x = film, y = value, fill = ratings)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom")

# but they're still stacked?
public_response_cleaned %>%
  ggplot(aes(x = film, y = value, fill = ratings)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

# what if I just wanted to look at the distribution of ratings across all films?
public_response_cleaned %>%
  ggplot(aes(x=value)) +
  geom_histogram()

# but what if I want to separate this out by the type of rating?
public_response_cleaned %>%
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~ratings) +
  theme_minimal()

### Is there a relationship between box office of the movie and the ratings?
pixar_joined %>%
  filter(!is.na(box_office_us_canada)) %>%
  ggplot(aes(x=metacritic, y=box_office_us_canada)) +
  geom_point() +
  theme_minimal()

# let's improve our chart a bit more...
pixar_joined %>%
  filter(!is.na(box_office_us_canada)) %>%
  ggplot(aes(x=metacritic, y=box_office_us_canada)) +
  geom_point() +
  theme_minimal() +
  geom_smooth()

# more tweaks...
library(scales)
pixar_joined %>%
  filter(!is.na(box_office_us_canada)) %>%
  ggplot(aes(x=metacritic, y=box_office_us_canada)) +
  geom_point() +
  theme_minimal() +
  geom_smooth() +
  scale_y_continuous(labels = scales::comma)

# and some more...
library(ggrepel)
pixar_joined %>%
  filter(!is.na(box_office_us_canada)) %>%
  ggplot(aes(x=metacritic, y=box_office_us_canada)) +
  geom_point() +
  theme_minimal() +
  geom_smooth() +
  scale_y_continuous(labels = scales::comma)

# What other questions do you have of the data?
