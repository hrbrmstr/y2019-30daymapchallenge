library(zipcode)
library(sf)
library(curlconverter) # hrbrmstr/curlconverter
library(hrbrthemes)
library(tidyverse)

st_read(here::here("data/me-counties.json")) %>%
  st_set_crs(4326) -> maine

url <- "curl 'https://hotspots.wifi.comcast.com/ajax/map-search' -H 'Connection: keep-alive' -H 'sec-ch-ua: \"Google Chrome 79\"' -H 'Accept: application/json, text/javascript, */*; q=0.01' -H 'Sec-Fetch-Dest: empty' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.16 Safari/537.36' -H 'DNT: 1' -H 'Content-Type: application/x-www-form-urlencoded; charset=UTF-8' -H 'Origin: https://hotspots.wifi.xfinity.com' -H 'Sec-Fetch-Site: cross-site' -H 'Sec-Fetch-Mode: cors' -H 'Referer: https://hotspots.wifi.xfinity.com/mobile/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9,la;q=0.8' --data 'txtSearch=03901&typeFilter=[]&clat=44.686349&clon=-68.904427' --compressed"

straighten(url) %>%
  make_req() -> req

fetch_hotspots <- function(zip) {

  message(zip)

  httr::POST(
    url = "https://hotspots.wifi.comcast.com/ajax/map-search",
    httr::add_headers(
      Connection = "keep-alive",
      `sec-ch-ua` = "Google Chrome 79",
      Accept = "application/json, text/javascript, */*; q=0.01",
      `Sec-Fetch-Dest` = "empty",
      `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.16 Safari/537.36",
      DNT = "1", Origin = "https://hotspots.wifi.xfinity.com",
      `Sec-Fetch-Site` = "cross-site",
      `Sec-Fetch-Mode` = "cors",
      Referer = "https://hotspots.wifi.xfinity.com/mobile/",
      `Accept-Encoding` = "gzip, deflate, br",
      `Accept-Language` = "en-US,en;q=0.9,la;q=0.8"
    ),
    body = list(
      txtSearch = zip,
      typeFilter = "[]",
      clat = "44.686349",
      clon = "-68.904427"
    ),
    encode = "form"
  ) -> res

  stop_for_status(res)

  out <- content(res, as = "text", encoding = "UTF-8")
  out <- jsonlite::fromJSON(out)

  if (length(out$results) == 0) return(NULL)

  out <- as_tibble(out$results)
  out <- select(out, lng=V7, lat=V6)
  out <- mutate_all(out, as.numeric)

  out

}

if (!file.exists(here::here("data/me-xfin-hotspots.rds"))) {

  as_tibble(zipcode) %>%
    filter(state == "ME") %>%
    pull(zip) %>%
    map_df(fetch_hotspots) -> maine_xfin_hotspots

  saveRDS(maine_xfin_hotspots, here::here("data/me-xfin-hotspots.rds"))

}

maine_xfin_hotspots <- readRDS(here::here("data/me-xfin-hotspots.rds"))

bbox <- st_bbox(maine)

maine_xfin_hotspots %>%
  filter(
    between(lat, bbox$ymin, bbox$ymax),
    between(lng, bbox$xmin, bbox$xmax),
  ) -> maine_xfin_hotspots

ggplot() +
  geom_sf(
    data = maine, color = "#b2b2b2", size = 0.125, fill = "#3B454A"
  ) +
  geom_point(
     data = maine_xfin_hotspots, aes(lng, lat),
     size = 0.125, alpha=1/2, color = "white"
  ) +
  stat_density_2d(
   data = maine_xfin_hotspots, geom = "polygon",
   aes(lng, lat, fill = stat(level)), n = 1000, h = c(0.2, 0.2)
  ) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  coord_sf(datum = NA) +
  labs(
    x = NULL, y = NULL,
    title = "Xfinity Hotspot Coverage in Maine",
    subtitle = glue::glue("{scales::comma(nrow(maine_xfin_hotspots))} total Xfinity hotspots in Maine\nPolygons show areas of highest hotspot density\nDots show individual hotspots"),
    caption = "Data source: <https://hotspots.wifi.xfinity.com/mobile/>\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="") +
  theme(axis.text = element_blank()) +
  theme(legend.position = "none")
