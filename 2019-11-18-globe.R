library(threejs)
library(widgetcard)
library(tidyverse)

attacks <- read_csv(here::here("data/attacks.csv"))
attacks$col <- scales::brewer_pal(palette = "RdYlBu", direction = -1)(11)[cut(log10(attacks$n), 11)]

attacks2 <- read_csv(here::here("data/attacks2.csv")) %>% filter(complete.cases(.))

mutate_at(attacks2, vars(-n), ~round(.x, 0)) %>%
  count(src_latitude, src_longitude, dst_latitude, dst_longitude, wt=n) -> attacks2

attacks2$col <- scales::brewer_pal(palette = "RdYlBu", direction = -1)(11)[cut(log10(attacks2$n), 11)]

globejs(
  lat = attacks$src_latitude,
  long = attacks$src_longitude,
  value = attacks$n/100000,
  color = attacks$col,
  pointsize = 0.5,
  atmosphere = TRUE
) -> attack_sources

attack_sources %>%
  htmlwidgets::saveWidget("~/stage/heis-oct-nov-sources.html", selfcontained = TRUE)

attack_sources %>%
  card_widget(
    output_dir = "~/widgets/globe01",
    name_prefix = "globe01",
    preview_img = "~/Desktop/globe01.png",
    html_title = "All Opportunistic Attack & Probe Sources to Rapid7 Project Heisenberg Since 2019-10",
    card_twitter_handle = "@hrbrmstr",
    card_title = "All Opportunistic Attack & Probe Sources to Rapid7 Project Heisenberg Since 2019-10",
    card_description = "Example of {threejs} for Day 17 of #30DayMapChallenge",
    card_image_url_prefix = "https://rud.is/vis/globe01/",
    card_player_url_prefix = "https://rud.is/vis/globe01/",
    card_player_width = 480,
    card_player_height = 480
  ) -> arch_fil

dst <- distinct(attacks2, dst_latitude, dst_longitude)

top_attacks <- top_n(attacks2, 1000, wt = n)
top_attacks$col <- scales::brewer_pal(palette = "RdYlBu", direction = -1)(11)[cut(log10(top_attacks$n), 11)]

globejs(
  lat = dst$dst_latitude,
  long = dst$dst_longitude,
  value = 0.5,
  arcs = select(top_attacks, 1:4) %>% as.data.frame,
  bodycolor = "#aaaaff",
  color = "#00aaff",
  pointsize = 0.5,
  arcsHeight = 0.3,
  arcsLwd = 2,
  arcsColor = top_attacks$col,
  arcsOpacity = 0.25,
  atmosphere = TRUE
) %>%
  htmlwidgets::saveWidget("~/stage/heis-oct-nov.html", selfcontained = TRUE)

