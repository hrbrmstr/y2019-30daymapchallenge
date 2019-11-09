library(magick)
library(sf)
library(stringi)
library(raster)
library(rasterVis)
library(rnaturalearth)
library(rnaturalearthdata)
library(hrbrthemes)
library(tidyverse)

# lower 48 borders
rnaturalearth::ne_states("united states of america", returnclass = "sf") %>%
  filter(!(postal %in% c("AK", "HI"))) %>%
  st_transform(
    crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
  )-> states

# lower 48 big poly
st_union(states) %>%
  as("Spatial") -> whole

# some of the rasters have errors
safe_raster <- possibly(raster, NULL, quiet = TRUE)

# read them in, discarding problematic ones
list.files(here::here("data/temps"), full.names = TRUE) %>%
  purrr::map(safe_raster) %>%
  discard(is.null) -> temp_rasters

# get overall range; this takes a while so commenting it in case I
# fat-finger RStudio's session restarter (turns out this was smart b/c I did)

# temp_rasters %>%
#   purrr::map(values) %>%
#   purrr::map(range) %>%
#   purrr::flatten_dbl() %>%
#   range() -> min_max

min_max <- c(-40.39574,  32.76227)

frames <- image_graph(700, 600, res = 96)

purrr::walk(temp_rasters, ~{

  temps <- .x
  temps <- projectRaster(temps, crs = crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  temps <- mask(temps, whole)

  temps_spdf <- as.data.frame(as(temps, "SpatialPixelsDataFrame"))
  colnames(temps_spdf) <- c("value", "x", "y")

  temp_date <- as.Date(stri_match_first_regex(names(temps), "([[:digit:]]{8})")[,2], format = "%Y%m%d")

  ggplot() +
    geom_tile(data = temps_spdf, aes(x, y, fill = value)) +
    geom_sf(data = states, fill = NA, color = "white", size = 0.125) +
    scale_fill_viridis_c(
      option = "magma", limits = c(-40, 35), breaks = c(-40, -20, 0, 20, 35)
    ) +
    coord_sf(datum = NA) +
    guides(fill = guide_colourbar(title.position = "top")) +
    labs(
      x = NULL, y = NULL, fill = "Min temp range for 2019 (°C)",
      title = glue::glue("Minimum Temps for {temp_date}")
    ) +
    theme_ft_rc(grid="") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text = element_blank()) +
    theme(legend.key.width = unit(2, "lines")) +
    theme(legend.position = "bottom") -> gg

  print(gg)

})

dev.off()

gif <- image_animate(frames, fps = 5)

# save it out

image_write(gif, here::here("out/temps.gif"))