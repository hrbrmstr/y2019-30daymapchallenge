library(sf)
library(readxl)
library(hrbrthemes)
library(albersusa)
library(tidyverse)

cmap <- counties_sf() %>% st_transform(us_laea_proj)

# https://www.census.gov/data/tables/2015/demo/metro-micro/commuting-flows-2015.html

read_excel(here::here("data/table1.xlsx"), skip=6) %>%
  janitor::clean_names() %>%
  select(
    start_state_fips = state_fips_code_1,
    start_county_fips = county_fips_code_2,
    start_state = state_name_3,
    start_county = county_name_4,
    end_state_fips = state_fips_code_5,
    end_county_fips = county_fips_code_6,
    end_state = state_name_7,
    end_county = county_name_8,
    workers = workers_in_commuting_flow,
    moe = margin_of_error
  ) %>%
  mutate(end_state_fips = gsub("^0", "", end_state_fips)) -> xdf

filter(xdf, start_state == "Maine", end_state != "Maine") %>%
  filter(start_county_fips != end_county_fips) %>%
  mutate(
    start_fips = glue::glue("{start_state_fips}{start_county_fips}") %>%
      as.character() %>%
      factor(levels = levels(cmap$fips)),
    end_fips = glue::glue("{end_state_fips}{end_county_fips}") %>%
      as.character() %>%
      factor(levels = levels(cmap$fips))
  ) -> me_start

select(cmap, fips, geometry) %>%
  mutate(geometry = st_centroid(geometry)) %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(
    select(cmap, fips) %>%
      as_tibble() %>%
      select(-geometry)
  ) %>%
  select(fips, lng = X, lat = Y) -> centers

count(me_start, start_county, wt=workers, sort=TRUE) %>%
  mutate(lab = glue::glue("{gsub(' County', '', start_county)} Outflow: {scales::comma(n)}")) -> labs

left_join(
  me_start, centers,
  by = c("start_fips"="fips")
) %>%
  rename(start_lng = lng, start_lat = lat) %>%
  glimpse() %>%
  left_join(centers, by = c("end_fips"="fips")) %>%
  rename(end_lng = lng, end_lat = lat) %>%
  left_join(labs) %>%
  mutate(lab = factor(lab, levels = labs$lab)) %>%
  glimpse() -> start

ggplot() +
  geom_sf(data = cmap, color = "#b2b2b277", size = 0.05, fill = "#3B454A") +
  geom_curve(
    data = start,
    aes(
      x = start_lng, y = start_lat, xend = end_lng, yend = end_lat,
      color = workers
    ),
    size = 0.15, arrow = arrow(type = "open", length = unit(5, "pt"))
  ) +
  scale_color_distiller(
    limits = range(start$workers), labels = scales::comma,
    trans = "log10", palette = "Reds", direction = 1, name = "Worker\nOutflow"
  ) +
  coord_sf(datum = NA, ylim = c(-2500000.0, 1500000)) +
  facet_wrap(~lab) +
  labs(
    x = NULL, y = NULL,
    title = "Oh The Places [Mainers] Will Go [For Work]!",
    subtitle = "2011-2015 5-Year ACS commuting outflows from Maine counties to out-of-state counties, sorted from most worker outflow to least.",
    caption = "Data source: <www.census.gov/data/tables/2015/demo/metro-micro/commuting-flows-2015.html>\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="", strip_text_family = font_es_bold, strip_text_size = 13) +
  theme(strip.text = element_text(color = "white"))
