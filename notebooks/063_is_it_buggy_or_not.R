
# 3857
#
world_scale <- 39
world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
                             ymin = 0, ymax = world_scale),
                           # crs = NA_crs_) %>%
                           crs = 3857) %>%
  st_as_sfc()

square_grid <- st_make_grid(world_landscape,
                            cellsize = cellsize, square = TRUE)
# if (sum(st_area(square_grid)) != st_area(world_landscape)) {
#   square_grid <- st_intersection(
#     square_grid, world_landscape
#   )
# }


ggplot() +
  geom_sf(data = world_landscape, size = 1.2, alpha = .8, fill = NA) +
  geom_sf(data = square_grid,
          fill = NA, linetype = "dashed", color = "cornflowerblue") +
  theme_grid_plot() +
  coord_sf(expand = FALSE) +
  NULL


#' Put two different sized grids side by side in ggplot.
#'
n_A <- 15
n_B <- 25
A <- pracma::magic(n_A)
B <- pracma::magic(n_B)

grid_A <- st_make_grid(world_landscape, n = n_A, square = TRUE, crs = NA_crs_)
grid_B <- st_make_grid(world_landscape, n = n_B, square = TRUE, crs = NA_crs_)

row_to_col <-
  \(grid_nrow, grid_ncol) {
    id <- seq.default(1, grid_nrow * grid_ncol)
    (((id - 1) %% grid_nrow) * grid_ncol + (id - 1) %/% grid_nrow + 1)
  }

row_to_col(n_A, n_A)

grid_sqA <- grid_A[row_to_col(n_A, n_A)]
grid_sqB <- grid_B[row_to_col(n_B, n_B)]

bind_rows(
  A = grid_A %>% st_sf() %>% mutate(m = as.numeric(A)),
  B = grid_B %>% st_sf() %>% mutate(m = as.numeric(B)),
  .id = "grid"
) ->
  grid_df

bind_rows(
  A = grid_sqA %>% st_sf() %>% mutate(m = as.numeric(A)),
  B = grid_sqB %>% st_sf() %>% mutate(m = as.numeric(B)),
  .id = "grid"
) ->
  sq_grid_df

ggplot() +
  # geom_sf(data = grid_df, fill = NA) +
  geom_sf(data = grid_df, aes(fill = m)) +

  facet_wrap(~grid) +

  theme_grid_plot() +
  coord_sf(expand = FALSE) +
  # guides(fill = guide_colorbar(
  #   title.position = "top",
  #   title.hjust = 0
  # )) +
  NULL

ggplot() +
  # geom_sf(data = grid_df, fill = NA) +
  # geom_sf(data = grid_df, aes(fill = m)) +
  # geom_sf(data = sq_grid_df, aes(fill = m)) +
  geom_sf(data = sq_grid_df, fill = NA) +

  # geom_sf_text(data = sq_grid_df, aes(label = m)) +
  # geom_sf_text(data = sq_grid_df %>% rowid_to_column("id"), aes(label = id)) +
  geom_sf_text(data = grid_df %>% rowid_to_column("id"), aes(label = id)) +

  facet_wrap(~grid) +

  theme_grid_plot() +
  coord_sf(expand = FALSE) +
  # guides(fill = guide_colorbar(
  #   title.position = "top",
  #   title.hjust = 0
  # )) +
  NULL
