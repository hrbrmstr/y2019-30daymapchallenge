library(sf)
library(ggimage)
library(ggspatial)
library(tidyverse)

cont <- st_read("data/GISofThrones/GoTRelease/Continents.shp")
islands <- st_read("data/GISofThrones/GoTRelease/Islands.shp")
regions <- st_read("data/GISofThrones/GoTRelease/Regions.shp")
polit <- st_read("data/GISofThrones/GoTRelease/Political.shp")
land <- st_read("data/GISofThrones/GoTRelease/Land.shp")
landsc <- st_read("data/GISofThrones/GoTRelease/Landscape.shp")
riv <- st_read("data/GISofThrones/GoTRelease/Rivers.shp")
lak <- st_read("data/GISofThrones/GoTRelease/Lakes.shp")
wall <- st_read("data/GISofThrones/GoTRelease/Wall.shp")
roads <- st_read("data/GISofThrones/GoTRelease/Roads.shp")
loc <- st_read("data/GISofThrones/GoTRelease/Locations.shp")

sea_col <- "#39474e"
land_col <- "#bbc09c"
forest_col <- "#31382588"
mountain_col <- "#989a81"
stepp_col <- "#bec2a5"
swamp_col <- "#6e715c"
water_col <- "#425461"
road_col <- "black"
loc_col <- "#5d2210"

mutate(landsc, f_col = case_when(
  type == "forest" ~ forest_col,
  type == "mountain" ~ mountain_col,
  type == "stepp" ~ stepp_col,
  type == "swamp" ~ swamp_col
)) -> landsc

mutate(regions, r_col = case_when(
  name == "The Red Waste" ~ "#81432877",
  name == "The Land of Always Winter" ~ "#ffffff99",
  name == "The Disputed Lands" ~ "#c2af94",
  name == "The Flatlands" ~ "#c2af94",
  TRUE ~ "#00000000"
)) -> regions

mutate(islands, i_col = case_when(
  name == "Ibben" ~ "#ffffff99",
  TRUE ~ land_col
)) -> islands

mutate(loc, loc_sz = case_when(
  type == "Castle" ~ 1,
  type == "City" ~ 0.6,
  type == "Other" ~ 0.25,
  type == "Ruin" ~ 0.33,
  type == "Town" ~ 0.4
)) -> loc

ggplot() +
  geom_sf(data = land, fill = land_col, color = "#5f7f7f", size = 0.5) +
  geom_sf(data = islands, aes(fill = I(i_col)), color = "#5f7f7f", size = 0.5) +
  geom_sf(data = cont, fill = land_col, size = 0.125) +
  geom_sf(data = regions, aes(fill = r_col), color = NA, size = 0.125) +
  geom_sf(data = landsc, aes(fill = I(f_col)), size = 0.125) +
  geom_sf(data = lak, size = 0.125, color = water_col, fill = water_col) +
  geom_sf(data = riv, fill = NA, size = 0.125, color = water_col) +
  geom_sf(data = wall, fill = NA, size = 1, color = "white") +
  geom_sf(data = roads, fill = NA, size = 0.33, color = road_col) +
  geom_sf(data = loc, fill = NA, color = loc_col, aes(size = I(loc_sz))) +
  geom_sf_text(
    data = regions, aes(label = name),
    family = "Luminari", size = 2, color = "#b2b2b2"
  ) +
  geom_image(
    data = data.frame(),
    aes(x = 32, y = 41, image = "data/rose3.png"),
    size = 0.1
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 70, y = 45, label = "A Song Of Ice and Fire"),
    family = "Luminari", size = 8, color = "white"
  ) +
  coord_sf(datum=NA) +
  labs(
    x = NULL, y = NULL,
    caption = "Data source: GISofThrones <downloads.gvsig.org/download/documents/books/>\n#30DayMapChallenge • <git.rud.is/hrbrmstr/y2019-30daymapchallenge>"
  ) +
  theme(plot.caption = element_text(color = "white")) +
  theme(plot.background = element_rect(color=sea_col, fill=sea_col)) +
  theme(panel.background = element_rect(color=sea_col, fill=sea_col))

