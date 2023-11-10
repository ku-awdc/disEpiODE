
devtools::load_all()

library(ggplot2)

#' Given an artificial, square landscape:
world_scale <- 5.2 * 10
# world_scale <- 22.5**2
world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
                             ymin = 0, ymax = world_scale)) %>%
  st_as_sfc()

create_grid(world_landscape,
            # n = c(5, 5),
            cellarea = 10**2,
            celltype = "triangle") %>%
  rowid_to_column() ->
  grid

#+ fig.width=9,fig.height=9
ggplot() +
  geom_sf(data = grid, aes(fill = area),
          color = "black",
          linetype = "dashed", linewidth = 1.2) +

  theme_grid_plot() +
  coord_sf(expand = FALSE) +
  theme_blank_background()

dist_grid <- grid %>% st_centroid() %>% st_distance()
hdist <- min(dist_grid[lower.tri(dist_grid, diag = FALSE)])
dist_grid_grid <- st_make_grid(grid, cellsize = hdist) %>%
  st_sf() %>%
  rowid_to_column()
dgg_m_dim <- st_bbox(grid) %>% {
  c(diff(.[c(1, 3)]), diff(.[c(2, 4)]))
}
dgg_m_dim <- ceiling(dgg_m_dim / hdist)
dist_grid_grid$rowid <- matrix(dist_grid_grid$rowid,
                               nrow = dgg_m_dim[1],
                               ncol = dgg_m_dim[2], byrow = TRUE) %>%
  as.numeric()
# dist_grid_grid <- dist_grid_grid %>% arrange(rowid)
dist_grid_grid

# dist_grid_grid
# dist_grid_grid %>% length()
dist_gg_m <-
  st_within(dist_grid_grid,
            x = grid %>% st_centroid(), sparse = TRUE)
dist_gg_m %>% dim()
grid$rowid <- dist_gg_m %>% unlist(recursive = FALSE)
grid <- grid %>% arrange(rowid)
dist_grid <- grid %>% st_centroid() %>% st_distance()

Matrix::Matrix(dist_grid) %>% Matrix::image()


ggplot() +
  geom_sf(data = grid, aes(fill = area),
          color = "black",
          linetype = "dashed", linewidth = 1.2) +
  geom_sf_text(data = grid, aes(label = rowid)) +
  # geom_sf(data = dist_grid,
  #         fill = NA) +

  theme_blank_background()

dist_grid_m <- Matrix::Matrix(dist_grid)

dist_grid_m %>% Matrix::image()

# Wrong ordering: Results in varying block-matrix size around diagonal
# dist_grid_m <- dist_grid_m %>%
#   `[`(order(.[1,]),order(.[1,]))
(exp(-dist_grid_m) %>% zapsmall()) == 0
sum((exp(-dist_grid_m) %>% zapsmall()) == 0)
prod(dim(dist_grid))
Matrix::image(
  (exp(-dist_grid_m) %>% zapsmall()) != 0,
  useRaster = TRUE
)

# dist_grid_m %>% order()
# dist_grid_m %>%
#   asplit(MARGIN = 2) %>%
#   lapply(order)
#
#
# dist_grid_m %>%
#   Matrix::image(useRaster = TRUE)
#
# # WRONG
# # dist_grid_m %>%
# #   `[`(order(.[1,]),) %>%
# #   Matrix::image(useRaster = TRUE)
#
# dist_grid_m %>%
#   `[`(order(.[1,]),order(.[1,])) ->
#   dist_grid_m_rot
#
# kernel_grid_m <- dist_grid_m_rot
# kernel_grid_m[] <- exp(-kernel_grid_m / 25)
#
# kernel_grid_m %>%
#   Matrix::image(useRaster = TRUE)
#
# library(d)
