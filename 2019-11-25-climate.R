library(sf)
library(hrbrthemes)
library(tidyverse)

#https://www.ncdc.noaa.gov/ibtracs/

st_read(here::here("data/me-counties.json")) %>%
  st_set_crs(4326) %>%
  st_transform(albersusa::us_laea_proj) -> maine

rnaturalearth::ne_states("united states of america", returnclass = "sf") %>%
  filter(!(postal %in% c("AK", "HI"))) %>%
  st_transform(albersusa::us_laea_proj) -> states

if (!file.exists(here::here("data/IBTrACS.ALL.list.v04r00.lines.zip"))) {

  download.file(
    "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.ALL.list.v04r00.lines.zip",
    here::here("data/IBTrACS.ALL.list.v04r00.lines.zip")
  )

  unzip(
    here::here("data/IBTrACS.ALL.list.v04r00.lines.zip"),
    exdir = here::here("data/ibtracs")
  )

  ibtracs <- st_read(here::here("data/ibtracs/IBTrACS.ALL.list.v04r00.lines.shp"))
  ibtracs <- st_transform(ibtracs, albersusa::us_laea_proj)

  me_impacts <- st_intersects(ibtracs, st_buffer(st_union(maine), 200000), sparse = FALSE)
  me_impacts <- as.logical(me_impacts)

  me_hur <- filter(ibtracs, me_impacts)
  me_hur <- filter(ibtracs, SID %in% unique(me_hur$SID))

  group_by(me_hur, SID, SEASON) %>%
    summarise(do_union = FALSE) %>%
    st_cast("MULTILINESTRING") %>%
    ungroup() -> me_hur

  ibtracs %>%
    as_tibble() %>%
    filter(SID %in% me_hur$SID) %>%
    group_by(SID) %>%
    summarise(
      mt = max(USA_SSHS),
      start = min(anytime::anytime(as.character(ISO_TIME))),
      dec = lubridate::year(start) - (lubridate::year(start) %% 10)
    ) %>%
    count(dec, mt) %>%
    mutate(
      dec = factor(dec, levels = seq(min(dec), max(dec), 10)),
      mt = factor(mt, levels = c(-2, -1, 0, 1, 2, 3, 4, 5),
                  labels = c("SubTrop", "TropDep", "TropStrm", "Cat1", "Cat2", "Cat3", "Cat4", "Cat5"),
                  ordered = TRUE)
    ) %>%
    complete(dec, mt) %>%
    filter(!is.na(mt)) -> cat_by_dec

  saveRDS(me_hur, here::here("data/me-hur.rds"))
  saveRDS(cat_by_dec, here::here("data/cat-by-dec.rds"))

}

me_hur <- readRDS(here::here("data/me-hur.rds"))
cat_by_dec <- readRDS(here::here("data/cat-by-dec.rds"))

ggplot(cat_by_dec, aes(dec, mt)) +
  geom_tile(
    aes(fill = n), color = "#252a32", size = 1, show.legend = FALSE
  ) +
  geom_text(
    aes(label = n, color = I(ifelse(n>3, "black", "white"))),
    family = font_rc, size = 2
  ) +
  scale_x_discrete(position = "top") +
  scale_fill_viridis_c(option = "magma", direction = 1, na.value = "#DEE5E855") +
  labs(x = NULL, y = NULL) +
  theme_ft_rc(grid="", axis_text_size = 8) -> gg

gb <- ggplotGrob(gg)

ggplot() +
  annotation_custom(
    gb,
    xmin = -2031896.9,
    xmax =  7725851.8,
    ymin =  2150000.0, # #732581.2,
    ymax =  5685229.4
    # ymin =  1500000.0, # #732581.2,
    # ymax =  4685229.4
  ) +
  geom_sf(data = states, color = "white", fill = "#3B454A", size = 0.0725) +
  geom_sf(data = maine, size = 0.125) +
  geom_sf(data = me_hur, aes(color = SEASON), size = 0.25, alpha=1/4, linetype = "dotted") +
  scale_color_viridis_c(option = "magma") +
  coord_sf(datum = NA) +
  guides(
    color = guide_colorbar(title.position = "top")
  ) +
  labs(
    title = "Tropical Storms Impacting Maine (1851-2017)",
    subtitle = "126 direct track intersections or close-enough pass-bys for substantial wind/rain impact.",
    caption = "Data source: NOAA NCDC <www.ncdc.noaa.gov/ibtracs/>\n#30DayMapChallenge • <git.rud.is/hrbrmstr/y2019-30daymapchallenge>"
  ) +
  theme_ft_rc(grid="") +
  theme(legend.position = c(0.75, 0.6)) +
  theme(legend.direction = "horizontal") +
  theme(legend.key.width = unit(2, "lines")) +
  theme(legend.title = element_text(hjust = 1))






