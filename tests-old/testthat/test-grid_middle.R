test_that("middle grid resolution", {


  world_scale <- 5.8 * 10
  # world_scale <- 22.5**2
  world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
                               ymin = 0, ymax = world_scale)) %>%
    st_as_sfc()

  cellarea <- 10**2
  {
    grid <- create_grid(landscape = world_landscape,
                        cellarea = cellarea) %>%
      mutate(area_flag = area %>% zapsmall() %>% factor())

    grid_middle <- create_grid(landscape = world_landscape,
                               cellarea = cellarea,
                               middle = TRUE) %>%
      mutate(area_flag = area %>% zapsmall() %>% factor())
    grids <- bind_rows(
      middle = grid_middle,
      regular = grid,
      .id = "mode"
    )

    ggplot() +
      geom_sf(data = grids, aes(fill = area_flag, group = mode)) +
      coord_sf(expand = FALSE) +
      facet_wrap(~mode) +
      theme_blank_background()
  }


  # ggplot() +
  #
  #   # geom_sf(fill = NA,
  #   #         data = grid) +
  #   geom_sf(data = grid, aes(fill = area_flag)) +
  #   coord_sf(expand = FALSE) +
  #   theme_blank_background()
  # ggplot() +
  #
  #   # geom_sf(fill = NA,
  #   #         data = grid) +
  #   geom_sf(data = grid_middel, aes(fill = area_flag)) +
  #   coord_sf(expand = FALSE) +
  #   theme_blank_background()

})

test_that("triangle middle grid resolution", {


  world_scale <- 3.4 * 10
  # world_scale <- 22.5**2
  world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
                               ymin = 0, ymax = world_scale)) %>%
    st_as_sfc()

  cellarea <- 10**2
  {
    grid <- create_grid(landscape = world_landscape,
                        celltype = "triangle",
                        cellarea = cellarea) %>%
      mutate(area_flag = area %>% zapsmall() %>% factor())

    grid_middle <- create_grid(landscape = world_landscape,
                               cellarea = cellarea,
                               celltype = "triangle",
                               middle = TRUE) %>%
      mutate(area_flag = area %>% zapsmall() %>% factor())
    grids <- bind_rows(
      middle = grid_middle,
      regular = grid,
      .id = "mode"
    )

    ggplot() +
      geom_sf(data = grids, aes(fill = area_flag, group = mode)) +
      facet_wrap(~mode) +

      theme(legend.position = "bottom") +
      coord_sf(expand = FALSE) +
      theme_blank_background()
  }


  # ggplot() +
  #
  #   # geom_sf(fill = NA,
  #   #         data = grid) +
  #   geom_sf(data = grid, aes(fill = area_flag)) +
  #   coord_sf(expand = FALSE) +
  #   theme_blank_background()
  # ggplot() +
  #
  #   # geom_sf(fill = NA,
  #   #         data = grid) +
  #   geom_sf(data = grid_middel, aes(fill = area_flag)) +
  #   coord_sf(expand = FALSE) +
  #   theme_blank_background()

})


test_that("square middle grid resolution", {


  # world_scale <- 39
  world_scale <- 39
  world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
                               ymin = 0, ymax = world_scale)) %>%
    st_as_sfc()

  for (cellarea in seq_cellarea(n = 100,
                                min_cellarea = 5,
                                max_cellarea = world_scale**2) %>%
       rev()) {

    grid_middle <- create_grid(landscape = world_landscape,
                               cellarea = cellarea,
                               middle = TRUE)
    ggplot(grid_middle) +

      geom_sf(aes(fill =  area %>% zapsmall() %>% factor()),
              data = grid_middle) +
      coord_sf(expand = FALSE) +
      theme_blank_background()

    # inner cells, corner, and edges
    expect_lte(
      grid_middle$area %>% zapsmall() %>% n_distinct(),
      3
    )
  }
})
