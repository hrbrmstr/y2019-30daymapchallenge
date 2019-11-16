library(sf)
library(rvest)
library(hrbrthemes)
library(curlconverter)
library(ggwordcloud)
library(tidyverse)

if (!file.exists(here::here("data/maine-names.rds"))) {

  cURL <- "curl 'https://www.ssa.gov/cgi-bin/namesbystate.cgi' -H 'Connection: keep-alive' -H 'Cache-Control: max-age=0' -H 'DNT: 1' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.36 Safari/537.36' -H 'Sec-Fetch-User: ?1' -H 'Origin: https://www.ssa.gov' -H 'Content-Type: application/x-www-form-urlencoded' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' -H 'Sec-Fetch-Site: same-origin' -H 'Sec-Fetch-Mode: navigate' -H 'Referer: https://www.ssa.gov/cgi-bin/namesbystate.cgi' -H 'Accept-Encoding: gzip, deflate, br' -H 'Accept-Language: en-US,en;q=0.9,la;q=0.8' -H 'Cookie: TS014661b0=01cfd667a591a309251d155252645f7ba8d3e92ca313a0cc50797331d66b9eca4cda5b1bf09262fc2dd204124dd994ed097d2b0147; TS01838516=017e2f91c304e1561be5e212ee69533011530d7386dacfbaf9306ecd64d33947273a6a054c612f05721de9c9378d41fde9c08cf713' --data 'state=ME&year=2017' --compressed"

  straighten() %>%
    make_req() -> req

  get_names <- function(yr = 2018) {

    httr::POST(
      url = "https://www.ssa.gov/cgi-bin/namesbystate.cgi",
      body = list(
        state = "ME",
        year = as.character(yr)
      ),
      encode = "form"
    ) -> res

    out <- httr::content(res, as = "parsed", encoding = "UTF-8")

    html_node(out, xpath = ".//table[@bordercolor = '#aaabbb']") %>%
      html_table(header = TRUE, trim = TRUE) %>%
      as_tibble() %>%
      janitor::clean_names() %>%
      mutate(year = yr)

  }

  maine_names <- map_df(1960:2018, get_names)

  saveRDS(maine_names, here::here("data/maine-names.rds"))

}

maine_names <- readRDS(here::here("data/maine-names.rds"))

if (!file.exists(here::here("data/me-silhouette.png"))) {

  st_read(here::here("data/me-counties.json")) %>%
    st_set_crs(4326) -> maine

  ggplot() +
    geom_sf(data = maine, fill = "black", color = "black") +
    coord_sf(datum = NA) +
    theme_ipsum_es(grid="") +
    theme(axis.text = element_blank()) -> gg

  ggsave(here::here("data/me-silhouette.png"), plot = gg, width = 500/72, height = 500/72, dpi = 96)

}

maine_names %>%
  {
    bind_rows(
      select(., name = male_name, ct = number_of_males) %>%
        count(name, wt = ct, name = "ct") %>%
        mutate(color = scales::brewer_pal(palette = "PuBu")(9)[cut(ct, 9)]),
      select(., name = female_name, ct = number_of_females) %>%
        count(name, wt = ct, name = "ct") %>%
        mutate(color = scales::brewer_pal(palette="OrRd")(9)[cut(ct, 9)])
    ) %>%
      arrange(desc(ct))
  } %>%
  write_delim(here::here("data/wordart.csv"), delim=";", col_names = FALSE) -> wc_df
