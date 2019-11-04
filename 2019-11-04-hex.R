library(magick)
library(asam)
library(rvest)
library(sf)
library(grid)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(hrbrthemes)
library(tidyverse)

# get the world without antarctica
ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  st_transform("+proj=eqearth +wktext") -> world

# get oceans
st_read(here::here("data/ocean/ne_110m_ocean.shp")) %>%
  select(scalerank, geometry) %>%
  st_transform("+proj=eqearth +wktext") -> ocean

# make a grid from the oceans
if (!file.exists(here::here("data/2019-11-01-geocoded.rds"))) {

  st_make_grid(
    ocean,
    n = c(250, 250),
    crs = st_crs(ocean),
    what = "polygons",
    square = FALSE
  ) -> grid

  grid <- st_sf(index = 1:length(lengths(grid)), grid)

  saveRDS(grid, here::here("data/ocean-grid-250.rds"))

}

grid <- readRDS(here::here("data/ocean-grid-250.rds"))

# get PIRATES!
if (!file.exists(here::here("data/asam-2019-11-04.rds"))) {
  asam_df <- read_asam()
  saveRDS(asam_df, here::here("data/asam-2019-11-04.rds"))
}

asam_df <- readRDS(here::here("data/asam-2019-11-04.rds"))

# make a simple features tibble and add in the decade
st_as_sf(asam_df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform("+proj=eqearth +wktext") %>%
  mutate(decade = (as.integer(format(asam_sf$date, "%Y")) %/% 10L) * 10L) -> asam_sf

# put each point in a hex polygon
hexbin <- st_join(asam_sf, grid, join = st_intersects)

group_by(hexbin, decade) %>%
  count(decade, index) %>%
  as_tibble() %>%
  select(decade, index, n) -> by_decade

# compute all attacks
grid %>%
  left_join(
    count(hexbin, index) %>%
      as_tibble() %>%
      select(index, ct=n)
  ) -> attacks

year <- "1978-2019"

ggplot() +
  geom_sf(data = world, color = "#b2b2b2", size = 0.1, fill = "#3B454A") +
  geom_sf(
    data = attacks, size = 0.125,
    aes(fill = ct, color = I(ifelse(is.na(ct), "#ffffff00", "#ffffff77")))
  ) +
  scale_fill_viridis_c(
    option = "magma", na.value = "#ffffff00", begin = 0.2,
    trans = "log10", aesthetics = "fill",
    name = "# Attacks"
  ) +
  guides(
    fill = guide_colourbar(title.position = "top")
  ) +
  labs(
    title = glue::glue("Anti-Shipping Activity (a.k.a. PIRATES!) : {year}"),
    caption = "Data source: {hrbrmstr/asam}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="") +
  theme(axis.text = element_blank()) +
  theme(legend.position = c(0.65, 0.15)) +
  theme(legend.direction = "horizontal") +
  theme(legend.key.width = unit(3, "lines"))