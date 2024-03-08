

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
