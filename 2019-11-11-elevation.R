library(sf)
library(hrbrthemes)
library(tidyverse)

if (!all(file.exists(here::here("data", c("mdi.rds", "mdi-border.rds", "mdi-inland-water.rds"))))) {

  # get all the towns and focus on the ones we care about
  st_read(here::here("data/Maine_Boundaries_Town_Polygon/Maine_Boundaries_Town_Polygon.shp")) %>%
    filter(TOWN %in% c("Bar Harbor", "Mount Desert", "Tremont", "Southwest Harbor", "Cranberry Isles")) %>%
    select(OBJECTID) %>%
    filter( # this removes island and meta-borders we don't want
      OBJECTID < 3000,
      !OBJECTID %in% c(2412, 2519, 2480, 2520, 2530, 2533, 2523, 2266, 2661, 2694, 2255, 2287, 2631, 2674, 2643, 2698)
    ) %>%
    st_union() -> mdi_border # finally, smush them all together

  plot(mdi_border) # give it quick look

  # the actual elevation contours
  st_read(here::here("data/Shape/Elev_Contour.shp")) %>%
    st_crop(xmin = -68.4719, ymin = 44.2108, xmax = -68.1542, ymax = 44.4666) %>% # reduce scope for faster intersection
    st_transform(26919) %>%
    st_intersection(mdi_border) -> mdi # only what's in the smushed outline above

  plot(mdi["ContourEle"], lwd=0.1) # give it a quick look

  # lakes and ponds
  st_read(here::here("data/lake-data/lakepolys.shp")) %>%
    filter(STATE_ABBR == "ME") %>% # reduce scope for faster intersection
    st_transform(26919) %>%
    st_intersection(mdi_border) %>% # only what's in the smushed outline above
    filter(!is.na(GNIS_NAME)) -> inland_water # finally, get rid of unnamed water bodies

  # save ^^ work out so we don't have to do it again
  saveRDS(mdi, here::here("data/mdi.rds"))
  saveRDS(mdi_border, here::here("data/mdi-border.rds"))
  saveRDS(inland_water, here::here("data/mdi-inland-water.rds"))

}

# read in our hard work
mdi <- readRDS(here::here("data/mdi.rds"))
mdi_border <- readRDS(here::here("data/mdi-border.rds"))
inland_water <- readRDS(here::here("data/mdi-inland-water.rds"))

tibble(
  lat = c(44.3380, 44.3526, 44.3351, 44.3426, 44.3329),
  lng = c(-68.3119, -68.2251, -68.2438, -68.2728, -68.2667),
  size = c(4.5, 3, 3, 3, 3),
  angle = c(-80, 0, 0, 0, 0),
  lab = c("Somes Sound", "Cadillac\nMountain", "Pemetic\nMountain", "Sargent\nMountain", "Penobscot\nMountain")
) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(26919) -> outer_lab

ggplot() +
  geom_sf(data = mdi_border, color = "#999999", fill = NA, size = 2, alpha = 1/20) + # water light outline
  geom_sf(data = mdi_border, color = "black", fill = "#f6e3b9", size = 0.5) + # maine border
  geom_sf(data = mdi, aes(color = ContourEle), size = 0.1) + # the contours
  geom_sf(data = inland_water, color = "black", fill = "#3a4f5b", size = 0.3) + # the lakes and ponds
  geom_sf_text( # label inland water we care about
    data = inland_water %>%
      filter(
        GNIS_NAME %in% c("Jordan Pond", "Eagle Lake", "Echo Lake", "Seal Cove Pond") |
          (GNIS_NAME == "Long Pond" & ACRES > 100)
      ),
    aes(label = GNIS_NAME),
    family = font_es, color = "white",
    size = c(3.25, 3.25, 2.5, 2.5, 2.5),  # these align with the ordered factor names
    angle = c(87.5, -75, -75, -80, -87.5)
  ) +
  geom_sf_text(
    data = outer_lab,
    aes(label = lab, size = I(size)+0.125, angle = I(angle)),
    family = font_es, color = c("black", "white", "white", "white", "white"), lineheight = 0.875
  ) +
  geom_sf_text(
    data = outer_lab,
    aes(label = lab, size = I(size), angle = I(angle)),
    family = font_es, color = c("white", "black", "black", "black", "black"), lineheight = 0.875
  ) +
  scale_color_viridis_c(
    name = "Elevation (ft)", option = "magma", label = scales::comma
  ) +
  coord_sf(datum = NA) +
  labs(
    x = NULL, y = NULL,
    title = "Mount Desert Island / Acadia National Park Elevation Contours",
    caption = "Data source: ArcGIS/<maine.gov>\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ipsum_es(grid="", plot_title_size = 24) +
  theme(plot.background = element_rect(fill = "#3a4f5b", color = "#3a4f5b")) +
  theme(panel.background = element_rect(fill = "#3a4f5b", color = "#3a4f5b")) +
  theme(legend.title = element_text(color = "white")) +
  theme(legend.text = element_text(color = "white")) +
  theme(plot.title = element_text(hjust = 0.5, color = "white")) +
  theme(plot.caption = element_text(color = "white")) +
  theme(legend.position = c(0.9, 0.275))














