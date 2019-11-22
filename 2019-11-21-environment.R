library(sf)
library(tigris)
library(patchwork)
library(hrbrthemes)
library(tidyverse)

if (!file.exists(here::here("data/me-topo.geojson"))) {
  download.file("http://techslides.com/demos/d3/us/data/me.topo.json", here::here("data/me-topo.geojson"))
}

st_read(here::here("data/me-topo.geojson")) %>%
  st_set_crs(4326) %>%
  st_transform("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") -> towns

st_read(here::here("data/me-counties.json")) %>%
  st_set_crs(4326) %>%
  st_transform("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") -> maine

filter(maine, NAME == "York") -> york

rivers <- linear_water(state = "ME", "York", class="sf")
water <- area_water(state = "ME", "York", class = "sf")

dsn <- "~/Desktop/ME_West_slr_data_dist/ME_West_slr_final_dist.gdb/"

st_layers(dsn)

x <- st_read(dsn, "ME_West_low_1ft", options=list("METHOD=SKIP"))
x <- st_intersection(st_buffer(x, 0), york)

y <- st_read(dsn, "ME_West_low_10ft", options=list("METHOD=SKIP"))
y <- st_intersection(st_buffer(y, 0), york)

york_towns <- st_intersection(st_buffer(towns, 0), york)

ggplot() +
  geom_sf(data = maine, fill = "#efefef", size = 0.25) +
  geom_sf(data = york, fill = "#efefef", size = 0.125) +
  # geom_sf(data = water, fill = "#8cb6d3", color = "#8cb6d3") +
  geom_sf(data = rivers, size = 0.075, color = "#8cb6d366") +
  geom_sf(data = x, fill = "#bd002666", color = "#bd002666") +
  geom_sf_text(data = maine, aes(label = NAME), family = font_es_bold, size = 3.25, color="white") +
  geom_sf_text(data = maine, aes(label = NAME), family = font_es_light, size = 3) +
  coord_sf(datum = NA) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_ipsum_es(grid="") -> gg

ggplot() +
  geom_sf(data = york, fill = "#efefef", size = 0.25) +
  geom_sf(data = water, fill = "#8cb6d3", color = "#8cb6d3") +
  geom_sf(data = rivers, size = 0.1, color = "#8cb6d3") +
  geom_sf_text(data = york_towns, aes(label = id), family = font_es_bold, size = 2.25, color="white") +
  geom_sf_text(data = york_towns, aes(label = id), family = font_es_light, size = 2) +
  geom_sf(data = x, fill = "#bd002666", color = "#bd002666") +
  coord_sf(datum = NA) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_ipsum_es(grid="") -> gg1

ggplot() +
  geom_sf(data = york, fill = "#efefef", size = 0.25) +
  geom_sf(data = water, fill = "#8cb6d3", color = "#8cb6d3") +
  geom_sf(data = rivers, size = 0.1, color = "#8cb6d3") +
  geom_sf(data = x, fill = "#bd002666", color = "#bd002666") +
  geom_sf_text(data = york_towns, aes(label = id), family = font_es_bold, size = 4.25, color="white") +
  geom_sf_text(data = york_towns, aes(label = id), family = font_es_light, size = 4) +
  coord_sf(datum = NA, xlim=c(-70.84655, -70.577334), ylim=c(43.061743, 43.216308)) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_ipsum_es(grid="") -> gg2

gg + gg1 + gg2 + plot_layout(ncol = 3)



