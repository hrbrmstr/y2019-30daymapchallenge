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
  filter(name != "Antarctica") -> world

# figure out the center of Maine
me <- which(state.abb == "ME")

st_sfc(st_point(c(state.center$x[me], state.center$y[me]))) %>%
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_transform(crs = "+proj=eqearth +wktext") -> maine_center # need this in the eqearth CRS (see note below)

# read the web page
pg <- read_html("https://www.census.gov/foreign-trade/statistics/state/data/me.html")

html_nodes(pg, "table") %>%  # parse the page
  .[[2]] %>%
  html_table() %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  select(country, x2018_value) %>%
  mutate(x2018_value = parse_number(x2018_value)) %>%
  filter(!grepl("Total", country)) %>%
  mutate(country = case_when( # clean up the countries
    country == "Macau" ~ "Macao",
    country == "Korea, South" ~ "Korea",
    TRUE ~ country
  )) %>%
  arrange(desc(x2018_value)) %>%
  slice(1:20) %>%
  left_join(world, by = c("country" = "name")) %>% # get the geometries for the target countries so we can get their centers
  mutate(to = suppressWarnings(st_centroid(geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>% # need to transform (see note below)
  select(country, x2018_value, to) %>%
  mutate(maine = maine_center) -> xdf # temporary variable

# we're using geom_curve() so we need regular values
# these values need to be pre-transformed since we're going to use geom_curve() instead of geom_sf()
st_coordinates(xdf$maine) %>%
  as_tibble() %>%
  set_names(c("from_x", "from_y")) %>%
  bind_cols(
    st_coordinates(xdf$to) %>%
      as_tibble() %>%
      set_names(c("to_x", "to_y")),
    select(xdf, country, x2018_value)
  ) -> maine_exports_to

# for displaying a data table next to the map
as_tibble(maine_exports_to) %>%
  select(Country = country, Exports = x2018_value) %>%
  mutate(Exports = glue::glue("{scales::comma(Exports)}M USD")) %>%
  tableGrob(
    rows = NULL,
    theme = ttheme_default(
      core = list(
        fg_params = list(
          fontfamily = font_rc,
          hjust = c(rep(0, 20), rep(1, 20)),
          x = c(rep(0.1, 20), rep(0.9, 20))
        )
      )
    )
  ) -> tabl

# map time#
ggplot() +
  geom_sf(data = world, size = 0.125, fill = "#3B454A", color = "#b2b2b2") +
  geom_curve(
    data = maine_exports_to, aes(x = from_x, y = from_y, xend = to_x, yend = to_y, color = x2018_value, size = x2018_value),
    curvature = 0.2, arrow = arrow(length = unit(10, "pt"), type = "closed"),
  ) +
  guides(
    color = guide_legend(reverse = TRUE)
  ) +
  annotation_custom(tabl, xmin = -16920565, xmax = -14000000,  ymin=761378/2.25, ymax = 761378) + # values are in eqarea meters
  coord_sf(crs = "+proj=eqearth +wktext") +
  scale_color_distiller(
    palette = "RdYlBu", trans = "log10", name = "(Size & color\nlog10 scale)", label = scales::comma,
    breaks = c(30, 50, 100, 300, 1000), limits = c(15, 1500)
  ) +
  scale_size_continuous(
    trans = "log10", range = c(0.75, 3),
    breaks = c(30, 50, 100, 300, 1000), limits = c(15, 1500),
    guide = FALSE
  ) +
  theme_ft_rc(grid="") +
  labs(
    x = NULL, y = NULL,
    title = "Top 20 (by Value) Export Destinations for Maine Goods (2018)",
    caption = "Data source: Census Bureau Foreign Trade <https://www.census.gov/foreign-trade/statistics/state/data/me.html>\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="") +
  theme(legend.key.height = unit(2.8, "lines")) +
  theme(legend.position = c(0.2, 0.3)) +
  theme(axis.text.x = element_blank())
