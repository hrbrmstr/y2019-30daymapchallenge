library(sf)
library(magick)
library(terminator)
library(rnaturalearth)
library(hrbrthemes)
library(tidyverse)

ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") -> world

proj <- "+proj=cea +lat_ts=37.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"

st_as_sf(tibble(lon = -45, lat = -90), coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
  st_transform(crs = proj) %>%
  st_coordinates() %>%
  .[,2] -> mid

get_terms <- function(i) {

  terminator(as.integer((as.POSIXct(Sys.Date()) + (60*60*(i)))), -180, 180, 0.1) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
    st_transform(crs = proj) %>%
    st_coordinates() %>%
    as_tibble() %>%
    set_names(c("lng", "lat"))

}

img <- image_graph(width=1000*2, height=500*2, res=144)

pb <- progress_estimated(24)

for (i in 0:23) {

  pb$tick()$print()

  ggplot() +
    geom_sf(data = world, size = 0.125, fill = "#3B454A", color = "#b2b2b2") +
    geom_ribbon(
      data = get_terms(i), aes(lng, ymin=mid, ymax = lat),
      fill = "#f9d71c", alpha = 1/6
    ) +
    coord_sf(crs = proj) +
    labs(
      x = NULL, y = NULL, title = sprintf("%02d:00", i),
      caption = "Data source: {hrbrmstr/terminator}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
    ) +
    theme_ft_rc(grid = "XY") +
    theme(plot.title = element_text(hjust = 0.5)) -> gg

  print(gg)

}

dev.off()

img <- image_animate(img)

image_write(img, "~/Desktop/terminus.gif")
