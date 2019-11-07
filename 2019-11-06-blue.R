library(readxl)
library(sf)
library(grid)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(hrbrthemes)
library(tidyverse)

# get the world without antarctica
ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") %>%
  st_transform("+proj=eqearth +wktext") -> world

read_excel(here::here("data/Chapter2OnlineData.xlsx"), 2) %>%
  janitor::clean_names() %>%
  select(country, happiness_score) %>%
  mutate(country = case_when(
    country == "Russia" ~ "Russian Federation",
    country == "Taiwan Province of China" ~ "Taiwan",
    country == "Congo (Brazzaville)" ~ "Democratic Republic of the Congo",
    country == "Congo (Kinshasa)" ~ "Republic of Congo",
    country == "Gambia" ~ "The Gambia",
    country == "Ivory Coast" ~ "Côte d'Ivoire",
    country == "North Cyprus" ~ "Northern Cyprus",
    country == "Palestinian Territories" ~ "Palestine",
    country == "South Korea" ~ "Republic of Korea",
    country == "Hong Kong S.A.R. of China" ~ "Hong Kong",
    country == "Laos" ~ "Lao PDR",
    TRUE ~ country
  )) -> happy

left_join(world, happy, by=c("name_long"="country")) %>%
  select(country=name_long, happiness_score) -> happy_spdf

by_pal <- colorRampPalette(c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf", "#fee090"))(10)

ggplot() +
  geom_sf(
    data = happy_spdf, aes(fill = happiness_score),
    size = 0.125, colour = "#2b2b2b77"
  ) +
  scale_fill_gradientn(
    name = "Happiness Score",
    colours = by_pal, limits = c(1, 10), breaks = 1:10,
    labels = c("Super Sad :-(", rep("", 3), "Meh", "...", rep("", 3), "Super Glad :-)")
  ) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(
    title = "World Happiness Report 2019",
    subtitle = "Average Happiness across Countries (Pooled OLS);",
    caption = "Data source: <https://worldhappiness.report/faq/>\nhttps://git.rud.is/hrbrmstr/y2019-30daymapchallenge • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text = element_blank()) +
  theme(legend.title = element_text(hjust = 0.5)) +
  theme(legend.key.width = unit(2, "lines")) +
  theme(legend.position = "bottom")
