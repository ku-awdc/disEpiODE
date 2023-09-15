
library(magrittr)
library(tidyverse)
library(sf)
library(eurostat)

devtools::load_all()

get_eurostat_geospatial() -> nuts

nuts %>%
  filter(CNTR_CODE == "DK") ->
  nuts_dk

ggplot() +
  geom_sf(data = nuts_dk, fill = NA) +
  geom_sf(data = nuts_dk %>% filter(NUTS_ID == "DK032"), fill = "red") +
  # geom_sf_text(data = nuts_dk, aes(label = NUTS_ID),check_overlap = TRUE) +
  theme_blank_background()

#' Synderjylland is
#'
# DK032
south_jutland <- nuts_dk %>% filter(NUTS_ID == "DK032")
south_jutland
south_jutland %>%
  st_area() %>%
  units::set_units("km**2")
