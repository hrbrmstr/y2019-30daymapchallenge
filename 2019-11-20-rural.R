library(sf)
library(readxl)
library(tigris)
library(albersusa)
library(hrbrthemes)
library(tidyverse)

farms <- read_excel(here::here("data/Ag_Census_Map_data_07172015.xlsx"), sheet = "Farms", col_names = TRUE)

ffactor <- c("Less than 250", "250 - 499", "750 - 999", "500 - 749", "1,000 or more")

counties_sf() %>%
  mutate(fips = as.character(fips)) %>%
  left_join(
    select(farms, fips = FIPSTEXT, range = y12_M249_classRange),
    by = "fips"
  ) %>%
  mutate(range = factor(range, ffactor)) -> farms_df

ggplot() +
  geom_sf(data = farms_df, aes(fill = range), color = "#b2b2b2", size = 0.1) +
  scale_fill_brewer(
    name = "# Farms", palette = "Greens", na.value = "white", labels = ffactor, breaks = ffactor
  ) +
  coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
  labs(
    x = NULL, y = NULL,
    title = "Down On The Farm: County-level Census of Agriculture Farms-per-County",
    subtitle = "While the USDA does track some 'urban farms' most of the farmland is, indeed, rural.",
    caption = "Data: USDA <www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Ag_Census_Web_Maps/Data_download/index.php>; {tigris}\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ipsum_es(grid="") +
  theme(legend.position = c(0.985, 0.5))
