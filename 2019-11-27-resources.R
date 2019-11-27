library(stars)
library(sf)
library(hrbrthemes)
library(tidyverse)

#  https://pubs.usgs.gov/sir/2017/5118/sir20175118_geo.php

if (!all(file.exists(c(here::here("data/Zn_tif.zip"),
                       here::here("data/Fe_tif.zip"),
                       here::here("data/U_tif.zip"),
                       here::here("data/Cu_tif.zip"))))) {
  download.file(
    url = c(
      "https://pubs.usgs.gov/sir/2017/5118/elements/Zinc/Zn_tif.zip",
      "https://pubs.usgs.gov/sir/2017/5118/elements/Iron/Fe_tif.zip",
      "https://pubs.usgs.gov/sir/2017/5118/elements/Uranium/U_tif.zip",
      "https://pubs.usgs.gov/sir/2017/5118/elements/Copper/Cu_tif.zip"
    ),
    destfile = c(
      here::here("data/Zn_tif.zip"),
      here::here("data/Fe_tif.zip"),
      here::here("data/U_tif.zip"),
      here::here("data/Cu_tif.zip")
    ),
    method = "libcurl"
  )

  unzip(zipfile = here::here("data/Zn_tif.zip"), exdir = here::here("data/usgs"))
  unzip(zipfile = here::here("data/Fe_tif.zip"), exdir = here::here("data/usgs"))
  unzip(zipfile = here::here("data/U_tif.zip"), exdir = here::here("data/usgs"))
  unzip(zipfile = here::here("data/Cu_tif.zip"), exdir = here::here("data/usgs"))

}

copper_r <- read_stars(here::here("data/usgs/A_Cu.tif")) # read raster
st_as_sf(copper_r[1], as_points = FALSE, merge = TRUE) %>% # make polygons
  lwgeom::st_make_valid() %>% # ensure polygons are valid
  st_transform(st_crs(me)) %>% # our CRS
  st_intersection(me) %>% # only maine
  rename(concentration = A_Cu.tif) %>% # need a common name for the value
  mutate(element = "Copper (Cu)") -> copper_me # name the element

zinc_r <- read_stars(here::here("data/usgs/A_Zn.tif"))
st_as_sf(zinc_r[1], as_points = FALSE, merge = TRUE) %>%
  lwgeom::st_make_valid() %>%
  st_transform(st_crs(me)) %>%
  st_intersection(me) %>%
  rename(concentration = A_Zn.tif) %>%
  mutate(element = "Zinc (Zn)") -> zinc_me

iron_r <- read_stars(here::here("data/usgs/A_Fe.tif"))
st_as_sf(iron_r[1], as_points = FALSE, merge = TRUE) %>%
  lwgeom::st_make_valid() %>%
  st_transform(st_crs(me)) %>%
  st_intersection(me) %>%
  rename(concentration = A_Fe.tif) %>%
  mutate(element = "Iron (Fe)") -> iron_me

uranium_r <- read_stars(here::here("data/usgs/A_U.tif"))
st_as_sf(uranium_r[1], as_points = FALSE, merge = TRUE) %>%
  st_buffer(0) %>%
  st_transform(st_crs(me)) %>%
  st_intersection(me) %>%
  rename(concentration = A_U.tif) %>%
  mutate(element = "Uranium (U)") -> uranium_me

all_four <- rbind(copper_me, zinc_me, iron_me, uranium_me)
all_four <- mutate(all_four, quantile = gtools::quantcut(concentration, 5))

ggplot() +
  geom_sf(data = all_four, aes(fill = quantile), size = 0.125, color = "white") +
  geom_sf(data = me, fill = NA) +
  scale_fill_viridis_d(
    name = "Concentration Quantile\n(relative to scale of individual mineral)",
    option = "magma",  labels = 1:5
  ) +
  facet_wrap(~element, ncol = 4) +
  guides(
    fill = guide_legend(title.position = "top")
  ) +
  labs(
    title = "Geochemistry of Maine",
    subtitle = "A-horizon (surface) samples of four selected elements",
    caption = "Data source: USGS <pubs.usgs.gov/sir/2017/5118/sir20175118_geo.php>\n<git.rud.is/hrbrmstr/y2019-30daymapchallenge> • #30DayMapChallenge"
  ) +
  coord_sf(datum = NA) +
  theme_ipsum_es(grid="", strip_text_family = font_es_bold) +
  theme(legend.position = "bottom") +
  theme(legend.direction = "horizontal")

