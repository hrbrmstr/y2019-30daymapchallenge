library(sf)
library(readxl)
library(tigris)
library(geofacet)
library(albersusa)
library(hrbrthemes)
library(tidyverse)

farms_2012 <- read_excel(here::here("data/Ag_Census_Map_data_07172015.xlsx"), sheet = "Farms", col_names = TRUE)
farms_2017 <- read_excel(here::here("data/NASSAgcensusDownload2017.xlsx"), sheet = "Farms", col_names = TRUE)

ffactor <- c("Less than 250", "250 - 499", "500 - 749", "750 - 999", "1,000 or more")

counties_sf() %>%
  mutate(fips = as.character(fips)) %>%
  left_join(
    select(
      farms_2012,
      fips = FIPSTEXT,
      range = y12_M249_classRange,
      ct = y12_M249_valueNumeric
    ),
    by = "fips"
  ) %>%
  mutate(
    year = "2012",
    range = factor(range, ffactor)
  ) -> y2012

counties_sf() %>%
  mutate(fips = as.character(fips)) %>%
  left_join(
    select(
      farms_2017,
      fips = FIPSTEXT,
      range = y17_M001_classRange,
      ct = y17_M001_valueNumeric
    ),
    by = "fips"
  ) %>%
  mutate(
    year = "2017",
    range = factor(range, ffactor)
  ) -> y2017

select(y2017, fips, year, range, ct) %>%
  ggplot() +
  geom_sf(aes(fill = range), color = "#b2b2b2", size = 0.1) +
  scale_fill_brewer(
    name = "# Farms", palette = "Greens", na.value = "white", labels = ffactor, breaks = ffactor
  ) +
  coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
  labs(
    x = NULL, y = NULL,
    title = "Down On The Farm: County-level Census of Agriculture Farms-per-County",
    subtitle = "While the USDA does track some 'urban farms' most of the farmland is, indeed, rural.",
    caption = "Data: USDA <www.nass.usda.gov/Publications/AgCensus>; {tigris}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ipsum_es(grid="") +
  theme(legend.position = c(0.985, 0.5))

select(y2017, fips, y2017=ct) %>%
  left_join(
    as_tibble(y2012) %>%
      select(fips, y2012=ct)
  ) %>%
  mutate(diff = y2017-y2012) -> decline

ggplot(decline) +
  geom_sf(aes(fill = diff), color = "#b2b2b2", size = 0.1) +
  scale_fill_distiller(
    name = "Farms delta", palette = "BrBG", direction = 1, na.value = "white"
  ) +
  coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
  labs(
    x = NULL, y = NULL,
    title = "Change in County Farm Counts 2012 - 2017",
    subtitle = "~63% of counties saw a decline in the number of farms since the 2012 USDA Ag Census",
    caption = "Data: USDA <www.nass.usda.gov/Publications/AgCensus>; {tigris}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ipsum_es(grid="") +
  theme(legend.position = c(0.985, 0.5))

farm_internet <- readRDS(here::here("data/farm-internet-history.rds"))

ggplot(farm_internet) +
  geom_segment(aes(year, pct, xend=year, yend=0), color = "#66bd63") +
  scale_x_date(breaks = range(farm_internet$year), labels = c("1997", "2019")) +
  scale_y_percent(limits = c(0, 1)) +
  facet_geo(~state) +
  labs(
    x = NULL, y = NULL,
    title = "State-level Farms with Internet Access % (1997-2019)",
    subtitle = "I'll admit to being optimistic that the current % was so high across the board",
    caption = "Data: USDA <usda.library.cornell.edu/concern/publications/h128nd689>; {geofacet}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ipsum_es(
    grid="Y", strip_text_family = font_es_bold,
    strip_text_size = 9, axis_text_size = 9
  ) +
  theme(axis.text.x = element_text(hjust = c(0, 1))) +
  theme(panel.spacing = unit(0.25, "lines"))


