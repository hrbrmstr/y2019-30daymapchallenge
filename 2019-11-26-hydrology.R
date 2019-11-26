library(sf)
library(tigris)
library(hrbrthemes)
library(tidyverse)

c(
  "#366cff", "#4476ff", "#517fff", "#5f89ff", "#6c93ff", "#799dff", "#87a7ff", "#94b1ff",
  "#a1baff", "#afc4ff", "#bcceff", "#cad8ff", "#d7e2ff", "#e4ebff", "#f2f5ff", "#ffffff"
) -> pal

st_read(here::here("data/me-counties.json"), stringsAsFactors = FALSE) %>%
  st_set_crs(4326) -> me_counties

pull(me_counties, NAME) %>%
  unique() %>%
  map(~area_water(state = "Maine", county = .x, class = "sf") %>%
        mutate(county = .x)) %>%
  do.call(rbind, .) -> me_aw

pull(me_counties, NAME) %>%
  unique() %>%
  map(~linear_water(state = "Maine", county = .x, class = "sf") %>%
        mutate(county = .x)) %>%
  do.call(rbind, .) -> me_rv

me_rv %>%
  mutate(
    sz = ifelse(grepl("(River|Rive|Riv)$", FULLNAME), 0.25, 0.04)
  ) -> me_rv

ggplot() +
  geom_sf(data = st_union(me_counties), fill = "#04005e", color = NA) +
  geom_sf(data = me_aw, size = 0, fill = "white", color = NA, show.legend = FALSE) +
  geom_sf(data = me_rv, aes(size = I(sz)), color = "white", fill = NA, show.legend = FALSE) +
  coord_sf(datum = NA) +
  labs(
    title = "Maine Hydrology"
  ) +
  theme_ipsum_es(grid="") +
  theme(plot.title = element_text(color = "white")) +
  theme(plot.background = element_rect(color = "black", fill = "black")) +
  theme(panel.background = element_rect(color = "black", fill = "black"))


ggplot() +
  geom_sf(data = st_union(me_counties), fill = "#04005e", color = NA) +
  geom_sf(data = me_aw, size = 0, fill = "white", color = NA, show.legend = FALSE) +
  geom_sf(data = me_rv, aes(size = I(sz)), color = "white", fill = NA, show.legend = FALSE) +
  coord_sf(datum = NA) +
  labs(
    title = "Maine Hydrology",
    caption = "Data: {tigris} • <git.rud.is/hrbrmstr/y2019-30daymapchallenge> • #30DayMapChallenge"
  ) +
  theme_ipsum_es(grid="") +
  theme(plot.title = element_text(hjust = 0.5))

# vern <- st_read("~/Desktop/Maine_Significant_Vernal_Pools/Maine_Significant_Vernal_Pools.shp", stringsAsFactors=FALSE)
#
# st_intersection(vern, st_transform(me_counties, st_crs(vern))) %>%
#   filter(NAME == "York") -> yc_vern
#
# ggplot() +
#   geom_sf(data = yc_vern[10,], color = NA, fill = "white") +
#   coord_sf(datum = NA) +
#   theme_ipsum_es(grid="") +
#   theme(plot.background = element_rect(color = "black", fill = "black")) +
#   theme(panel.background = element_rect(color = "black", fill = "black"))
#
#

unique(as.character(me_rv$FULLNAME)) %>%
  stringi::stri_match_last_regex(" ([[:alpha:]]+)$") %>%
  .[,2] %>%
  unique() %>% sort()



