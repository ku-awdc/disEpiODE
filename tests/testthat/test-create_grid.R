

#TODO:
test_that("0 test", {

})

#TODO: Check if indeed it is triangles, when you ask for triangles,
# squares, etc., by counting the number of corners / vertices.
#
# tau_rstate %>%
#   enframe() %>%
#   unnest_wider(value) %>%
#   select(name, grid) %>%
#   bind_cols(params1) %>%
#   select(name, grid, celltype) %>%
#
#   mutate(
#     flag = map2_lgl(grid, celltype, \(grid, celltype) {
#       # browser()
#       n_coords <- grid %>%
#         st_geometry() %>%
#         st_cast("MULTIPOINT") %>%
#         map_int(length)
#       n_pts <- n_coords / 2
#       switch (celltype,
#               triangle = all(n_pts == 4),
#               square = all(n_pts == 5)
#       )
#     })
#   ) %>%
#   summarise(all(flag))


test_that("conservation of area", {
  world_scale <- 17
  world <- create_landscape(scale = world_scale)
  world_landscape <- world$landscape

  world_area <- st_area(world_landscape)

  # this is an assumption of a fully square landscape
  # stopifnot(isTRUE(all.equal(world_scale**2, world_area)))
  expect_equal(world_scale**2, world_area)

  for (n in 1:100) {
    grid <- create_grid(n, world_landscape, landscape_scale = world_scale)

    grid_area <- st_area(st_union(grid))
    # stopifnot(isTRUE(all.equal(grid_area, world_area)))
    expect_equal(grid_area, world_area)
  }


  # DEBUG
  # ggplot() +
  #   geom_sf(data = world_landscape, fill = NA) +
  #
  #   geom_sf(data = grid, fill = NA, linetype = "dashed") +
  #
  #   theme_blank_background()


})
