library(sf)
library(tigris)
library(hrbrthemes)
library(tidyverse)

st_read(here::here("data/me-counties.json")) %>%
  st_set_crs(4326) -> maine

list_counties("me") %>%
  pull(county) %>%
  map(~roads("me", .x, class="sf")) -> me_roads

map(me_roads, ~filter(.x, grepl("green", tolower(FULLNAME)))) %>%
  do.call(rbind, .) -> green_roads

ggplot() +
  geom_sf(data = maine, color = "#b2b2b2", size = 0.125, fill = "#3B454A") +
  geom_sf(data = green_roads, color = "forestgreen", size = 0.75) +
  coord_sf(datum = NA) +
  labs(
    title = "Green Roads of Maine",
    subtitle = "Linestrings of all roads in Maine with 'green' in the name",
    caption = "Data source: {tigris}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc() +
  theme(axis.text = element_blank()) +
  theme(legend.position = "none")
