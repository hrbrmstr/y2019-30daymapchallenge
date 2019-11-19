library(sf)
library(readxl)
library(tigris)
library(statebins)
library(albersusa)
library(hrbrthemes)
library(tidyverse)

farms <- read_excel(here::here("data/Ag_Census_Map_data_07172015.xlsx"), sheet = "Farms", col_names = TRUE)

ffactor <- c("Less than 250", "250 - 499", "500 - 749", "750 - 999", "1,000 or more")

counties_sf() %>%
  mutate(fips = as.character(fips)) %>%
  left_join(
    select(farms, fips = FIPSTEXT, range = y12_M249_classRange, ct = y12_M249_valueNumeric),
    by = "fips"
  ) %>%
  mutate(range = factor(range, ffactor)) -> farms_df

as_tibble(farms_df) %>%
  mutate(state = as.character(state)) %>%
  count(state, wt=ct) -> farm_counts

ggplot() +
  ggalt::geom_bkde(
    data = farms_df, aes(ct),
    color = "#006d2c", fill = alpha("#006d2c", 3/4)
  ) +
  scale_x_comma() +
  labs(
    x = NULL, y = "Density",
    title = "Farms-per-County Distribution"
  ) +
  theme_ipsum_es(grid="XY")

ggplot() +
  ggalt::geom_bkde(
    data = farm_counts, aes(n),
    color = "#006d2c", fill = alpha("#006d2c", 3/4)
  ) +
  scale_x_comma() +
  labs(
    x = NULL, y = "Density",
    title = "Farms-per-State Distribution"
  ) +
  theme_ipsum_es(grid="XY")

ggplot(farm_counts) +
  geom_statebins(
    aes(state = state, fill = n), family = font_es, size = 5
  ) +
  scale_fill_viridis_c(
    name = "# Farms", direction = -1, label = scales::comma
  ) +
  coord_fixed() +
  guides(fill = guide_colourbar(title.position = "top")) +
  labs(
    x = NULL, y = NULL,
    title = "Farms",
    subtitle = "farms",
    caption = "Farms"
  ) +
  theme_ipsum_es(grid="") +
  theme(axis.text = element_blank()) +
  theme(legend.position = c(0, 0.9)) +
  theme(legend.justification = "left") +
  theme(legend.direction = "horizontal") +
  theme(legend.key.width = unit(2.75, "lines"))

ggplot() +
  geom_sf(data = farms_df, aes(fill = range), color = "#b2b2b2", size = 0.1) +
  scale_fill_brewer(
    name = "# Farms", palette = "Greens", na.value = "white", labels = ffactor, breaks = ffactor
  ) +
  coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
  labs(
    x = NULL, y = NULL,
    title = "Down On The Farm: County-level Census of Agriculture Farms-per-County",
    subtitle = "While the USDA does track some 'urban farms' most of the farmland is, indeed, rural.",
    caption = "Data: USDA <www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Ag_Census_Web_Maps/Data_download/index.php>; {tigris}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ipsum_es(grid="") +
  theme(legend.position = c(0.985, 0.5))

