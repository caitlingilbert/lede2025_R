library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

# get census key at: http://api.census.gov/data/key_signup.html

# you can also add this key to your Renviron (overwrite = T if you want to replace existing one)
census_api_key("YOUR API KEY GOES HERE", overwrite = FALSE, install = TRUE)
# For the first time, reload your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check that your key is stored by running:
Sys.getenv("CENSUS_API_KEY")

# take a look at all variables for 2023
var2023 <- load_variables(2023, "acs5", cache = TRUE)

# look at one variable and map it by county
queens <- get_acs(
  state = "NY",
  county = "Queens",
  geography = "tract",
  variables = "B19013_001", #median household income
  geometry = TRUE,
  year = 2023
)

head(queens)

queens %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal()

# look at multiple variables, and facet-map them
racevars <- c(White = "P2_005N",
              Black = "P2_006N",
              Asian = "P2_008N",
              Hispanic = "P2_002N")

queens_race <- get_decennial(
  geography = "tract",
  variables = racevars,
  state = "NY",
  county = "Queens",
  geometry = TRUE,
  summary_var = "P2_001N", # multi-group denominator
  year = 2020,
  sumfile = "pl"
)

head(queens_race)

queens_race %>%
  mutate(percent = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = percent)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "% of population\n(2020 Census)")

# deal with shorelines...
queens_race %>%
  erase_water(year = 2020) %>%
  mutate(percent = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = percent)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "% of population\n(2020 Census)")

# look at an interactive map
library(mapgl)
tidycensus::get_acs(
  geography = "place",
  variables = c("B19025_001", "B19001_001"), #aggregate household income over past year, total number of households in given geography
  state = "NY",
  geometry = TRUE,
  output = "wide"
) %>%
  dplyr::mutate(mean_income = round(B19025_001E / B19001_001E)) %>%
  mapgl::maplibre_view(column = "mean_income")

# look at all states in a map
us_median_age <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2023,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  shift_geometry() # rescales AK, HI, PR in US-wide map

plot(us_median_age$geometry)

us_median_age %>%
ggplot(aes(fill = estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "RdPu",
                       direction = 1) +
  labs(title = "Median Age by State, 2019",
       caption = "Data source: 2023 1-year ACS, US Census Bureau",
       fill = "ACS estimate") +
  theme_void()
