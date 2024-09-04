devtools::load_all() 

landscape_sf <- st_sfc(
  st_polygon(
    rbind(c(0, 0),
          c(0, 1),
          c(1, 1),
          c(1, 0),
          c(0, 0)) %>% list()
  )
)
landscape <- landscape_sf

common_cellsize <- 1 / 3

landscape_padding <- st_bbox(landscape)
hex_size <- common_cellsize
horiz <- sqrt(3) * hex_size
vert <- (3/2) * hex_size
#' [xmin ymin xmax ymax]
landscape_padding <- landscape_padding +
  c(-1, -1, 1, 1) *
  c(vert, horiz / 2, vert / 2, horiz)

landscape_padding <- st_as_sfc(landscape_padding)

hex0 <- st_make_grid(
  landscape,
  cellsize = common_cellsize,
  square = FALSE,
  flat_topped = FALSE
) %>%
  st_sf()

hex_pad <- st_make_grid(
  landscape_padding,
  cellsize = common_cellsize,
  square = FALSE,
  flat_topped = FALSE
) %>%
  st_sf()
# bind_rows(
#   st_filter()
# )

ggplot() +
  geom_sf(data = landscape_padding, fill = NA) +
  geom_sf(data = landscape, fill = NA) +

  # theme_grid_plot()
  NULL

ggplot() +
  geom_sf(data = landscape, fill = NA, linetype = "dashed", aes(color = "landscape")) +
  geom_sf(data = landscape_padding, fill = NA, linetype = "dashed", aes(color = "padding")) +
  geom_sf(data = hex0, fill = NA, linetype = "dotted", aes(color = "zero")) +
  geom_sf(data = hex_pad, fill = NA, aes(color = "pad")) +

  geom_sf(data = st_centroid(landscape), aes(color = "center")) +
  # geom_sf(data = st_centroid(hex_pad), aes(color = "center of bbox")) +
  geom_sf(data = st_filter(hex_pad, st_centroid(landscape)), aes(fill = "cell with landscape center"), alpha = 0.2) +
  geom_sf(data = st_centroid(st_filter(hex_pad, st_centroid(landscape))), aes(color = "center cell centroid")) +

  labs(color = NULL, fill = NULL) +

  theme_blank_background() +
  # theme_grid_plot() +
  NULL
