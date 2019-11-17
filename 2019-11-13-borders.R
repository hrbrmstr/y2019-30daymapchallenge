library(stringi)
library(tigris)
library(hrbrthemes)
library(tidyverse)

jsonlite::fromJSON("https://github.com/CivilServiceUSA/us-house/raw/master/us-house/data/us-house.json") %>%
  as_tibble() %>%
  left_join(
    distinct(fips_codes, STATEFP=state_code, state_name)
  ) %>%
  mutate(district = ifelse(is.na(district), 0, district)) %>%
  mutate(GEOID = sprintf("%s%02s", STATEFP, district)) -> house

congressional_districts(TRUE, "20m", year = 2018, class="sf") %>%
  left_join(
    distinct(fips_codes, STATEFP=state_code, state_name)
  ) %>%
  filter(!(STATEFP %in% c("02", 15, 60:78))) %>%
  left_join(house) %>%
  filter(!is.na(party)) %>%
  mutate(party = stri_trans_totitle(party)) -> cd

ggplot() +
  geom_sf(data = cd, aes(fill = party), color = "white", size = 0.25) +
  coord_sf(crs=albersusa::us_laea_proj, datum = NA) +
  scale_fill_manual(
    values = c(
      "Republican" = "#a50026",
      "Democrat" = "#313695"
    ), name = NULL
  ) +
  labs(
    x = NULL, y = NULL,
    title = "116th Congress District Borders",
    caption = "Data source: {tigris}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="") +
  theme(legend.position = c(0.5, 0.95)) +
  theme(legend.position = "horizontal")

map(cd$state_name, ~{

  f <- filter(cd, state_name == .x)

  ggplot() +
    geom_sf(data = f, aes(fill = party), color = "white", size = 0.125) +
    scale_fill_manual(
      values = c(
        "Republican" = "#a50026",
        "Democrat" = "#313695"
      ), name = NULL
    ) +
    coord_sf(crs=albersusa::us_laea_proj, datum = NA) +
    labs(
      x = NULL, y = NULL,
      title = f$state_name[[1]]
    ) +
    theme_ipsum_rc(grid="") +
    theme(legend.position = "none") -> gg

  ggsave(
    here::here(sprintf("out/13/%s.png", tolower(f$state_name[[1]]))),
    plot = gg, width=250/72, height=250/72
  )

  gg

}) -> gd




