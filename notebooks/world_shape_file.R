library(magrittr)
library(geodata)
library(sf)


#' REPLACE `path` with a local path that you'd store the
#' downloaded files locally in...
world <- geodata::world(path = tempdir())
world_sf <- world %>% st_as_sf()

ggplot() +
  geom_sf(data = world_sf) +
  coord_sf(expand = FALSE) +
  theme(panel.grid = element_blank(), panel.background = element_blank())

# DEBUG
# world_sf %>%
#   st_make_valid() %>%
#   print(n = Inf)

#' This is necessary because things broke otherwise...
sf_use_s2(FALSE)

world_smooth_sf <- world_sf %>%
  # st_make_valid() %>%
  st_make_valid(geos_method = "valid_structure") %>%

  st_simplify(preserveTopology = FALSE, dTolerance = 0.3) %>%
  # remove anartica
  filter(GID_0 != "ATA") %>%
  identity()

ggplot() +
  # geom_sf(data = world_sf) +
  geom_sf(data = world_smooth_sf) +
  coord_sf(expand = FALSE) +
  theme(panel.grid = element_blank(), panel.background = element_blank())


world_smooth_sf %>%
  left_join(geodata::country_codes(), by = join_by(NAME_0 == NAME)) %>%
  identity() -> world_smooth_cont_sf

# world_smooth_cont_sf

ggplot() +
  # geom_sf(data = world_sf) +
  # geom_sf(data = world_smooth_sf) +
  # geom_sf(data = world_smooth_cont_sf, aes(fill = UNREGION1), show.legend = FALSE) +
  geom_sf(data = world_smooth_cont_sf, aes(fill = UNREGION2), show.legend = FALSE) +
  coord_sf(expand = FALSE) +
  theme(panel.grid = element_blank(), panel.background = element_blank())
