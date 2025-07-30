library(tidyverse)
library(vroom)
library(rvest)

### s6
s6page <- read_html("https://www.netflix.com/tudum/articles/love-is-blind-season-6-cast-instagrams")

age_jobs_s6 <- s6page %>%
  html_elements(".e1l1jp228+ .e1l1jp226") %>%
  html_text()

extraagejob_s6 <- s6page %>%
  html_element("hr+ .e1l1jp227 .e1l1jp228+ .e1l1jp226") %>%
  html_text()

age_jobs_all_s6 <- c(extraagejob_s6, age_jobs_s6)

names_s6 <- s6page %>%
  html_elements(".e1l1jp228 h3") %>%
  html_text2()

s6cast <- data.frame(names_s6, age_jobs_all_s6)

s6cast_final <- s6cast %>%
  mutate(age_jobs_all_s6 = str_remove(age_jobs_all_s6,"Age and Occupation")) %>%
  separate_wider_delim(age_jobs_all_s6, delim = ", ", names = c("age","job")) %>%
  mutate(season = "6") %>%
  rename(names = names_s6)

# alt version
s6cast_df <- tibble(
  names = s6page %>%
    html_elements(".e1l1jp228 h3") %>%
    html_text2(),
  age_jobs_all_s6 = age_jobs_all_s6,
  ) %>%
  mutate(age_jobs_all_s6 = str_remove(age_jobs_all_s6,"Age and Occupation")) %>%
  separate_wider_delim(age_jobs_all_s6, delim = ", ", names = c("age","job")) %>%
  mutate(season = "6")

# grab all the instagram and tiktok links too
social_links = s6page %>%
  html_elements(".e1l1jp226 a") %>%
  html_attr("href")
