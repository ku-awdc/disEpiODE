
library(magrittr)
library(tidyverse)
library(sf)
library(eurostat)

conflicted::conflicts_prefer(dplyr::filter)

devtools::load_all()

NUTS <- get_eurostat_geospatial()

NUTS %>%
  st_transform(crs = 25832) %>%
  filter(CNTR_CODE == "DK") ->
  nuts_dk

nuts_dk %>%
  nest(data = -LEVL_CODE) %>%
  deframe() ->
  nuts_dk_levels

# nuts_dk_levels[[1]]
# nuts_dk_levels[[2]]
# nuts_dk_levels[[3]]

ggplot() +
  geom_sf(data = nuts_dk_levels[[3]], fill = NA) +
  geom_sf(
    aes(fill = geo),
    data = nuts_dk_levels[[3]]) +
  theme_blank_background()


bornholm <-
  nuts_dk_levels[[4]] %>%
  filter(geo == "DK014")

nuts_dk_vet <- st_difference(
  nuts_dk_levels[[1]],
  bornholm
)

ggplot() +
  geom_sf(data = nuts_dk_vet, fill = NA) +
  theme_blank_background()



#' Validate...
nuts_dk_vet %>%
  # st_make_grid(cellsize = c(10e3, 10e3)) %>%
  st_area() %>%
  units::set_units("km**2")


dk_vet_grid <- nuts_dk_vet %>%
  st_simplify() %>%
  st_make_grid(cellsize = c(10e3, 10e3)) %>%
  st_intersection(nuts_dk_vet) %>%
  st_sf() %>%
  # st_join(left = FALSE,
  #         y = nuts_dk_levels %>% `[[`(length(.)),
  #         join = st_intersects) %>%
  # mutate(overlap = st_area())
  # st_sf() %>%
  # glimpse() %>%
  filter(as.numeric(st_area(geometry)) > 0) %>%
  # glimpse() %>%
  # VERIFY
  # st_area() %>%
  # units::set_units("km**2")
  identity()

dk_vet_grid %>% nrow()

ggplot() +
  geom_sf(data = nuts_dk_vet, fill = NA) +
  geom_sf(data = dk_vet_grid, fill = NA) +
  theme_blank_background()


#'
#' ggplot() +
#'   geom_sf(data = nuts_dk_vet, fill = NA) +
#'   theme_blank_background()
#'
#' ggplot() +
#'   geom_sf(data = nuts_dk_levels[[1]], fill = NA) +
#'   theme_blank_background()
#'
#' ggplot() +
#'   geom_sf(data = nuts_dk_levels[[2]], fill = NA) +
#'   theme_blank_background()
#'
#' ggplot() +
#'   geom_sf(data = nuts_dk_levels[[3]], fill = NA) +
#'   theme_blank_background()
#'
#' nuts_dk_utm <-
#'   nuts_dk %>%
#'   st_transform(crs = 25832) %>%
#'   st_simplify() %>%
#'   st_make_valid() %>%
#'   identity()
#'
#' # st_touches(nuts_dk)
#' # st_intersects(nuts_dk)
#' # st_intersection(nuts_dk)
#' # st_snap(nuts_dk_utm,nuts_dk_utm, tolerance = 1000) ->
#' #   nuts_dk_snap
#' #
#' # ggplot() +
#' #   geom_sf(data = nuts_dk_snap, fill = "green") +
#' #   theme_blank_background()
#' nuts_dk_utm %>% View()
#'
#' nuts_dk_utm_vet <-
#'   nuts_dk_utm %>%
#'   # st_sf() %>%
#'   filter(geo != "DK014") %>%
#'   identity()
#' nuts_dk_utm_vet %>% View()
#' ggplot() +
#'   geom_sf(data = nuts_dk_utm_vet, fill = "green") +
#'   theme_blank_background()
#'
#' g
#' nuts_dk_utm
#' #' Synderjylland is
#' #'
#' # DK032
#' south_jutland <- nuts_dk %>% filter(NUTS_ID == "DK032")
#' south_jutland
#' south_jutland %>%
#'   st_area() %>%
#'   units::set_units("km**2")
#'
#'
