#' test_that("perfect tessellation", {
#'
#'   world_scale <- 39
#'   world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
#'                                ymin = 0, ymax = world_scale)) %>%
#'     st_as_sfc()
#'
#'   cellsize <- world_scale
#'   # all_cellsize <- seq.default(1, cellsize, length.out = 150)
#'   all_cellsize <- cellsize / seq.default(1, 100, length.out = 150)
#'   # old_all_cellsize <- all_cellsize
#'   # all_cellsize <- world_scale / round(world_scale / all_cellsize)
#'   # all_cellsize <- unique(zapsmall(all_cellsize))
#'
#'   expect_gt(length(all_cellsize), 1)
#'
#'   for (cellsize in all_cellsize) {
#'
#'     square_grid <- st_make_grid(world_landscape,
#'                                 cellsize = cellsize, square = TRUE)
#'     if (sum(st_area(square_grid)) != st_area(world_landscape)) {
#'       square_grid <- st_intersection(
#'         square_grid, world_landscape
#'       )
#'     }
#'
#'     # ggplot() +
#'     #   # geom_sf(data = triangle_grid, fill = NA) +
#'     #   geom_sf(data = square_grid, fill = NA) +
#'     #
#'     #   theme_grid_plot()
#'
#'
#'
#'     expect_equal(sum(st_area(square_grid)),
#'                  st_area(world_landscape))
#'
#'     # square_grid %>% st_geometry() %>% plot()
#'     expect_length(
#'       st_geometry_type(square_grid) %>% unique(),
#'       1)
#'
#'
#'     flag <- square_grid %>% st_area() %>% zapsmall() %>% unique()
#'     # square_grid[max(flag) >= st_area(square_grid)] -> square_grid
#'     # flag <- square_grid %>% st_area() %>% zapsmall() %>% unique()
#'     flag
#'     expect_length(flag, 1)
#'
#'     #' in reality, this needs to be adjusted, like 2× or 1/2
#'
#'     # region: triangle
#'
#'     # triangle_grid <- square_grid %>%
#'     #   st_triangulate_constrained() %>%
#'     #   st_collection_extract()
#'     #
#'     # ggplot() +
#'     #   geom_sf(data = triangle_grid, fill = NA) +
#'     #   # geom_sf(data = square_grid) +
#'     #
#'     #   theme_grid_plot()
#'
#'
#'     # if (sum(st_area(triangle_grid)) < st_area(world_landscape)) {
#'     #   triangle_grid <- st_intersection(
#'     #     triangle_grid, world_landscape
#'     #   )
#'     # }
#'     #
#'     # expect_equal(sum(st_area(triangle_grid)),
#'     #              st_area(world_landscape))
#'     #
#'     # # square_grid %>% st_geometry() %>% plot()
#'     # expect_length(
#'     #   st_geometry_type(triangle_grid) %>% unique(),
#'     #   1)
#'     #
#'     # flag <- triangle_grid %>% st_area() %>% zapsmall() %>% unique()
#'     # flag
#'     # expect_length(flag, 1)
#'     # endregion
#'   }
#'
#'
#' })


test_that("nested splits perfect tessellation", {

  world_scale <- 39
  world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
                               ymin = 0, ymax = world_scale)) %>%
    st_as_sfc()

  cellsize <- world_scale
  # current_grid <- world_landscape
  all_n_cells <- 1:round(sqrt(2500))
  for (n_cells in all_n_cells) {
    square_grid <- create_grid(world_landscape,
                               n = n_cells,
                               celltype = "square")
    expect_equal(sum(st_area(square_grid)),
                 st_area(world_landscape))


    flag <- square_grid %>% st_area() %>% zapsmall() %>% unique()
    flag
    expect_length(flag, 1)

    n_square <- nrow(square_grid)

    triangle_grid <- create_grid(world_landscape,
                                 n = n_cells,
                                 # n = c(n_cells / 2, n_cells / 2  / 2)C,
                                 celltype = "triangle")
    expect_equal(sum(st_area(triangle_grid)),
                 st_area(world_landscape))

    flag <- triangle_grid %>% st_area() %>% zapsmall() %>% unique()
    flag
    expect_length(flag, 1)

    n_triangle <- nrow(triangle_grid)
    if (n_cells == 1) {

    } else {
      expect_equal(n_triangle, n_square)

    }

  }
  ggplot() +
    geom_sf(data = square_grid, fill = NA , linewidth = 1.9, linetype = "dashed") +
    geom_sf(data = triangle_grid, fill = NA, linewidth = 1.5, linetype = "dotted") +
    theme_grid_plot()

  ggplot() +
    geom_sf(data = create_grid(celltype = "square", n = 2, landscape = world_landscape),
            fill = NA , linewidth = 1.9, linetype = "dashed") +
    geom_sf(
      data = create_grid(celltype = "triangle", n = c(2, 1),
                         landscape = world_landscape),
      fill = NA, linewidth = 1.5, linetype = "dotted") +
    theme_grid_plot() +
    coord_sf(expand = FALSE)

  ggplot() +
    geom_sf(data = create_grid(celltype = "square", n = c(2, 3), landscape = world_landscape),
            fill = NA , linewidth = 1.9, linetype = "dashed", color = "cornflowerblue") +
    geom_sf(
      data = create_grid(celltype = "triangle", n = c(1, 3),
                         landscape = world_landscape),
      fill = NA, linewidth = 1.5, linetype = "dotted", color = "orange") +
    theme_grid_plot() +
    coord_sf(expand = FALSE)

})

