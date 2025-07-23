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

public_response_cleaned %>%
  ggplot(aes(x = film, y = value, col = ratings)) +
  geom_point() +
  geom_line(aes(group = ratings)) +
  #scale_color_brewer(palette = "Dark2") +
  labs(x = "Pixar film", y = "Rating value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom")

# What other questions do you have of the data?
