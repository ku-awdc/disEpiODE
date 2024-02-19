test_that("multiplication works", {

  landscape_sf <- st_sfc(
    st_polygon(
      rbind(c(0, 0),
            c(0, 1),
            c(1, 1),
            c(1, 0),
            c(0, 0)) %>% list()
    )
  )

  p_landscape <- ggplot() +
    geom_sf(data = landscape_sf, fill = NA) +
    theme_grid_plot() +
    theme_blank_background() +
    NULL
  p_landscape
  # create_grid <- function(landscape,
  #                         cellarea = NULL,
  #                         n = NULL,
  #                         middle = FALSE,
  #                         celltype = c("square", "hexagon", "hexagon_rot", "triangle"),
  #                         offset = c("corner", "middle", "bottom", "left"))
  create_grid(landscape_sf, cellarea = 1, celltype = "square")
  create_grid(landscape_sf, cellarea = 1, celltype = "hexagon")
  create_grid(landscape_sf, cellarea = 1, celltype = "triangle")
  p_landscape + geom_sf(fill = NA, data = create_grid(landscape_sf, cellarea = 1, celltype = "square"))
  p_landscape + geom_sf(fill = NA, data = create_grid(landscape_sf, cellarea = 1, celltype = "hexagon"))
  p_landscape + geom_sf(fill = NA, data = create_grid(landscape_sf, cellarea = 1, celltype = "triangle"))
  # p_landscape + geom_sf(fill = NA, data = create_grid(landscape_sf, cellarea = 11, celltype = "triangle"))
  cellareas <- seq_cellarea(
    n = 100,
    min_cellarea = 0.0001, max_cellarea = 1)
  for (cellarea in cellareas) {
    expect_no_error(create_grid(landscape_sf, cellarea = cellarea, celltype = "square"))
    expect_no_error(create_grid(landscape_sf, cellarea = cellarea, celltype = "hexagon"))
    expect_no_error(create_grid(landscape_sf, cellarea = cellarea, celltype = "triangle"))
  }
})
