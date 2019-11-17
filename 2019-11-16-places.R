library(sf)
library(rgeocodio)
library(rvest)
library(stringi)
library(pdftools)
library(hrbrthemes)
library(albersusa)
library(tidyverse)
library(magrittr)

if (!file.exists(here::here("data/russe.rds"))) {
  russe_pg <- read_html("https://www.businessinsider.com/charlotte-russe-bankruptcy-stores-closing-list-2019-2")

  html_nodes(russe_pg, xpath=".//p[contains(., 'of the closing')]/following-sibling::ul/li") %>%
    html_text() -> russe

  russe_g <- rgeocodio::gio_batch_geocode(russe)

  saveRDS(russe_g, here::here("data/russe.rds"))
}

if (!file.exists(here::here("data/sears.rds"))) {

  sears_pg <- read_html("https://www.businessinsider.com/sears-closes-80-more-stores-2018-12")
  html_nodes(sears_pg, xpath=".//span[contains(., 'of the latest')]/../../p") %>%
    html_text() %>%
    keep(stri_detect_regex, "^(Sears|Kmart)") %>%
    stri_replace_first_regex("^(Sears[\\*]*|Kmart)", "") %>%
    stri_trim_both() -> sears

  sears2_pg <- read_html("https://www.businessinsider.com/sears-kmart-stores-closing-list-2018-10")
  html_nodes(sears2_pg, xpath=".//h2[text()='Sears' or text()='Kmart']/following-sibling::ul/li ") %>%
    html_text() %>%
    stri_trim_both() -> sears2

  sears_g <- rgeocodio::gio_batch_geocode(c(sears, sears2))
  saveRDS(sears_g, here::here("data/sears.rds"))

}

dressbarn <- as_tibble(jsonlite::stream_in(gzcon(url("https://rud.is/dl/dressbarn-locations.json.gz"))))

payless <- read_csv("http://rud.is/dl/2019-payless-store-closings.csv")

saveRDS(dressbarn, here::here("data/dressbarn.rds"))
saveRDS(payless, here::here("data/payless.rds"))

bind_rows(
  filter(sears_g, map_lgl(response_results, ~nrow(.x) > 0)) %>%
    mutate(ll = map(response_results, ~select(.x, location.lng, location.lat) %>% slice(1))) %>%
    select(ll) %>%
    unnest(ll) %>%
    set_names(c("lng", "lat")) %>%
    mutate(brand = "Sears/Kmart"),

  filter(russe_g, map_lgl(response_results, ~nrow(.x) > 0)) %>%
    mutate(ll = map(response_results, ~select(.x, location.lng, location.lat) %>% slice(1))) %>%
    select(ll) %>%
    unnest(ll) %>%
    set_names(c("lng", "lat")) %>%
    mutate(brand = "Russe"),

  select(payless, lng = longitude, lat=latitude) %>%
    mutate(brand = "Payless"),

  select(dressbarn, lng = lon, lat) %>%
    mutate(brand = "Dressbarn")
) %>%
  filter(lng > -130, lat > 21) -> continental

usa <- usa_sf("laea") %>% filter(!(name %in% c("Alaska", "Hawaii")))

st_as_sf(continental, coords = c("lng", "lat"), crs = us_longlat_proj) %>%
  st_transform(albersusa::us_laea_proj) -> continental

ggplot() +
  geom_sf(data = usa, fill = "#252525", size = 0.125, color = "#b2b2b277") +
  geom_sf(data = continental, aes(color = brand), size = 0.25, alpha = 1/3, show.legend = "point") +
  ggthemes::scale_color_tableau(name = NULL) +
  coord_sf(datum = NA) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 2, alpha=1)
    )
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Places of the 2019 Retail Apocalpyse",
    subtitle = "Locations of four major brands store closings in 2019 alone (~3,100 stores in total)",
    caption = "Data source: (various + {rgeocodio})\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="") +
  theme(legend.position = c(0.5, 0.95)) +
  theme(legend.direction = "horizontal") +
  theme(axis.text = element_blank())
