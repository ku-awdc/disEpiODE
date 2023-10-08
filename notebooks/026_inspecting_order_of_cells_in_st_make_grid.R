world <- create_landscape(4)
world$landscape -> world_landscape

grid <- st_make_grid(world_landscape, cellsize = c(1, 2)) %>%
  st_sf()
grid <- rowid_to_column(grid, "id")

ggplot() +
  geom_sf(data = grid, fill = NA) +

  geom_sf_text(data = grid, aes(label = id)) +
  labs(caption = "row-major") +

  theme_blank_background()

# NOTE: must fill this out from above.
grid_nrow <- 2
grid_ncol <- 4

# convert 1:(nrow×ncol) from row-major to col-major.

grid2 <- grid %>%
  # id ~> 1:(grid_nrow×grid_ncol)
  # (id - 1) ~> 0:(grid_nrow×grid_ncol-1)
  # (id - 1) %% grid_nrow ~> 0:(grid_nrow×grid_ncol - 2)
  #  -||- × grid_nrow ~> what column is `id`'th element in
  #
  # (id - 1) %/% grid_nrow ~> what row is this `id`'th element in
  slice(((id - 1) %% grid_nrow) * grid_ncol + (id - 1) %/% grid_nrow + 1) %>%
  mutate(id = NULL) %>%
  rowid_to_column("id")


ggplot() +
  geom_sf(data = grid, fill = NA) +
  geom_sf_text(data = grid2, aes(label = id)) +
  labs(caption = "col-major") +
  theme_blank_background()
#' ## Conclusion
#'
#' `st_make_grid` returns elements in row-major,
#' meaning that the order they are returned in cannot be
#' used directly in the context of R's matrices, and
#' R's natural vectorisation, as R uses col-major.
#'
