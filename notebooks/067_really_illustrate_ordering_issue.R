#' ---
#' title: Ordeirng of cells from `st_make_grid` is problematic
#' author: Mossa
#' ---

devtools::load_all()

library(ggplot2)

#' Given an artificial, square landscape:
world_scale <- 5.2 * 10
# world_scale <- 22.5**2
world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
                             ymin = 0, ymax = world_scale)) %>%
  st_as_sfc()

#' ## Ordering of `st_make_grid`
#'
#' Here we show that `st_make_grid` return cells in row-major, and not
#' col-major order:

grid_magic <- create_grid(world_landscape, n = c(6,6),
                          celltype = "square") %>%
  mutate(m = pracma::magic(6) %>%
           # HERE
           as.numeric())

grid_magic_rot <-
  create_grid(world_landscape, n = c(6,6),
              celltype = "square") %>%
  mutate(m = pracma::magic(6) %>%
           t() %>% # convert to row-major
           as.numeric())

#' We'd expect this (with y reversed)
pracma::magic(6) %>%
  Matrix::Matrix()
#'
#'
#'
ggplot(grid_magic) +
  geom_sf(fill = NA) +
  geom_sf_text(aes(label = m)) +

  theme_grid_plot() +
  coord_sf(expand = FALSE) +
  theme_blank_background()

ggplot(grid_magic_rot) +
  geom_sf(fill = NA) +
  geom_sf_text(aes(label = m)) +

  theme_grid_plot() +
  coord_sf(expand = FALSE) +
  theme_blank_background()


#' ## Recovering the indexing of a cells in non-square grids
#'
#' What can we do to recover these indices? Use distances to
#' y-axis, and x-axis:
#'
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

rot_id <- grid %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  rowid_to_column() %>%
  mutate(distX = sqrt((X - X)**2 + (Y - 0)**2),
         distY = sqrt((X - 0)**2 + (Y - Y)**2)) %>%
  arrange(distY, distX) %>%
  # arrange(distX, distY) %>%
  rowid_to_column("rowid_rot")

rot_id

grid_rot <- grid[as.integer(rot_id$rowid),] %>%
  mutate(rowid = seq_len(n()))

ggplot() +
  # geom_sf(data = grid, fill = NA) +
  # geom_sf(data = grid, aes(fill = area)) +
  geom_sf(data = grid,
          aes(fill = area),
          color = "black",
          linetype = "dashed", linewidth = 1.2) +
  geom_sf_text(data = grid,
               size = 4,
               color = "grey",
               check_overlap = TRUE,
               # nudge_x = -1.5, nudge_y = 1.5,
               aes(label = rowid, color = "reg")) +
  geom_sf_text(data = grid_rot,
               size = 4,
               color = "black",
               check_overlap = TRUE,
               nudge_x = 1.5,
               # nudge_x = 1.5, nudge_y = -1.5,
               # position = ggrepel::position_nudge_repel(1, 1.5),
               aes(label = rowid, color = "rot")) +
  theme_grid_plot() +
  coord_sf(expand = FALSE) +
  theme_blank_background()

#' ## Impact on resulting distance matrices
#'


dist_grid <- st_distance(grid %>% st_centroid())

#+ fig.width=9,fig.height=9
dist_grid %>%
  Matrix::Matrix(.) %>%
  Matrix::image(useRaster = TRUE, aspect = 1,
                sub = "distance matrix of direct `st_make_grid")

dist_grid_rot <- st_distance(grid_rot %>% st_centroid())

#+ fig.width=9,fig.height=9
dist_grid_rot %>%
  Matrix::Matrix(.) %>%
  Matrix::image(useRaster = TRUE, aspect = 1,
                sub = "distance matrix of rotated `st_make_grid`")


all.equal(dist_grid, dist_grid_rot)


#'
#'
#'
#'
# Matrix::Matrix(pracma::magic(6))
#
# Matrix::Matrix(pracma::magic(6)) %>%
#   Matrix:::image(useRaster = TRUE, labels = pracma::magic(6))
#
# a_mat <- Matrix::Matrix(grid$area, byrow = TRUE, nrow = 6, ncol = 6)
#
# a_mat
# a_vec <- a_mat %>% as.numeric()
#
# ar_mat <- Matrix::Matrix(outer(a_vec, 1/a_vec))
# ar_mat %>% Matrix::image(useRaster=T)
#
# ar_mat_l <- Matrix::Matrix(outer(1/a_vec, a_vec))
# ar_mat_l %>% Matrix::image(useRaster=T)
#
# sweep()
