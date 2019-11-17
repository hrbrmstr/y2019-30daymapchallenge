library(sf)
library(albersusa)
library(rnaturalearth)
library(hrbrthemes)
library(asam) # hrbrmstr/asam

zones <- asam_subregions()

incidents <- read_asam()
incidents <- st_as_sf(incidents, coords = c("longitude", "latitude"), crs = us_longlat_proj)

zones %>%
  left_join(
    st_intersection(incidents, select(zones, SUBREGION)) %>%
      as_tibble() %>%
      count(SUBREGION)
  ) -> locs

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(
    data = world, color = "#2b2b2b", size = 0.125, fill="#d9d9d9"
  ) +
  geom_sf(
    data = select(locs, SUBREGION, n), aes(fill = n),
    size = 0.125, alpha=1/2, color = "#b3000077"
  )  +
  geom_sf_text(
    data = select(locs, SUBREGION, n),
    aes(
      label = I(ifelse(is.na(n), "", scales::comma(n))),
      color = I(ifelse(n > 900, "black", "white"))
    ),
    family = font_es_bold, size = 3.33
  ) +
  geom_sf_text(
    data = select(locs, SUBREGION, n),
    aes(
      label = I(ifelse(is.na(n), "", scales::comma(n))),
      color = I(ifelse(n > 900, "white", "black"))
    ),
    family = font_es_bold, size = 3.25
  ) +
  scale_fill_viridis_c(
    name = "Number of Anti-Shipping Incidents",
    option = "magma", direction = -1, na.value = "white",
    label = scales::comma
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Total Anti-Shipping Activity since 1978 by Sub-Region Zone",
    subtitle = "Zones depicted with red border; counts are total incidents (mostly piracy)",
    caption = "Data source: {asam}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ipsum_es(grid="") +
  theme(legend.position = "top") +
  theme(legend.key.width = unit(2, "lines")) +
  theme(legend.direction = "horizontal")
