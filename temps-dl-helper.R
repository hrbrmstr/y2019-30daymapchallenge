library(stringi)
library(httr)
library(tidyverse)

rem_dir <- "ftp://ftp.cpc.ncep.noaa.gov/GIS/GRADS_GIS/GeoTIFF/TEMP/us_tmin/"

res <- httr::GET(rem_dir)

fils <- stri_match_all_regex(rawToChar(res$content), "(us\\.tmin_nohads_ll_2019[[:digit:]]{4}_float\\.tif)")[[1]][,2]

walk(
  fils,
  ~download.file(
    file.path(rem_dir, .x),
    here::here("data", "temps", .x)
  )
)
