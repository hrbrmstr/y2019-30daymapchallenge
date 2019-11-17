library(sf)
library(magick)
library(tweenr)
library(rnaturalearth)
library(hrbrthemes)
library(tidyverse)

# https://my.vanderbilt.edu/jeremyatack/data-downloads/
r1 <- st_read(here::here("data/RR1826-1911Modified0509161/RR1826-1911Modified050916.shp"))

ne_states("United States of America", returnclass = "sf") %>%
  filter(!(name %in% c("Alaska", "Hawaii"))) -> states

border <- st_union(states)

in_op_rng <- sort(unique(r1$InOpBy))

img <- image_graph(width=1000*2, height=600*2, res=144)

pb <- progress_estimated(length(in_op_rng))

for (i in in_op_rng) {

  pb$tick()$print()

  ggplot() +
    geom_sf(data = border, color = "#252525", size = 1, fill = NA) +
    geom_sf(data = states, color = "#b2b2b2", size = 0.1, fill = "white", linetype = "dotted") +
    geom_sf(data = filter(r1, InOpBy <= i), aes(color = InOpBy), size = 0.1) +
    coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
    scale_color_continuous(limits = range(r1$InOpBy)) +
    labs(
      x = NULL, y = NULL,
      title = sprintf("U.S. Railroad Expansion • Year: %s", i),
      subtitle = "The 'Ties' That Bound Us Together",
      caption = "Data source: <my.vanderbilt.edu/jeremyatack/data-downloads/>\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
    ) +
    theme_ipsum_es(grid="") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(panel.background = element_rect(fill = "#b15928")) +
    theme(legend.position = "none") -> gg

  print(gg)

}

dev.off()

img <- image_animate(img, )

image_write(img, "rr.gif")
