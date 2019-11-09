library(sf)
library(tigris)
library(hrbrthemes)
library(mapdeck)
library(widgetcard)
library(tidyverse)

mapdeck_api_key <- Sys.getenv(MAPBOX_PUBLIC_TOKEN)

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
    title = "The 'Green' Roads of Maine",
    subtitle = "Linestrings of all roads in Maine with 'green' in the name",
    caption = "Data source: {tigris}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text = element_blank()) +
  theme(legend.position = "none")

mapdeck(
  token = mapdeck_api_key,
  style = mapdeck_style("dark"),
  location = c(-69.4455, 45.2538),
  zoom = 5
) %>%
  add_sf(
    data = green_roads,
    layer_id = "FULLNAME",
    stroke_width = 2,
    stroke_colour = "#228b22",
    tooltip = "FULLNAME",
    update_view = FALSE
  ) %>%
  add_title("The 'Green' Roads of Maine") -> wdgt

wdgt

wdgt %>%
  card_widget(
    output_dir = "~/widgets/mdme",
    name_prefix = "mdme",
    preview_img = "~/Desktop/memd.png",
    html_title = "The 'Green' Roads of Maine",
    card_twitter_handle = "@hrbrmstr",
    card_title = "The 'Green' Roads of Maine",
    card_description = "Example of {mapdeck} for Day 8 of #30DayMapChallenge",
    card_image_url_prefix = "https://rud.is/vis/mdme/",
    card_player_url_prefix = "https://rud.is/vis/mdme/",
    card_player_width = 480,
    card_player_height = 480
  ) -> arch_fil


