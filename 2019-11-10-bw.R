library(sf)
library(tigris)
library(hrbrthemes)
library(tidyverse)

st_read(here::here("data/me-counties.json")) %>%
  st_set_crs(4326) -> maine

list_counties("me") %>%
  pull(county) %>%
  map(~roads("me", .x, class="sf")) -> me_roads

me_roads <- do.call(rbind, me_roads)

# st_write(me_roads, "~/Data/me-roads/me-roads.shp")
# me_roads <- st_read("~/Data/me-roads/me-roads.shp")

as_tibble(me_roads) %>%
  select(-geometry) -> tt

count(tt, RTTYP)

# Route Type Code	Route Type Code Description
# C	County
# I	Interstate
# M	Common Name
# O	Other
# S	State recognized
# U	U.S.

state <- filter(me_roads, RTTYP == "S")
county <- filter(me_roads, RTTYP == "C")
interstate <- filter(me_roads, RTTYP == "I")
main <- filter(me_roads, RTTYP == "M")

ggplot() +
  geom_sf(data = main, color = "#777777", size = 0.075, alpha = 2/3) +
  geom_sf(data = state, color = "#999999", size = 0.1, alpha = 1/2) +
  geom_sf(data = county, color = "#eeeeee", size = 0.125, alpha = 1/5) +
  geom_sf(data = interstate, color = "#999999", size = 2, alpha = 1/10) +
  geom_sf(data = interstate, color = "black", size = 1, alpha = 1/5) +
  geom_sf(data = interstate, color = "#aaaaaa", size = 1/2, alpha = 1/2) +
  labs(
    title = "Maine Without Borders",
    caption = "Data source: {tigris}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "black", color = "black")) +
  theme(panel.background = element_rect(fill = "black", color = "black")) +
  theme(panel.grid = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_blank())



