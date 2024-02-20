test_that("testing generated grids", {

  landscape_sf <- st_sfc(
    st_polygon(
      rbind(c(0, 0),
            c(0, 1),
            c(1, 1),
            c(1, 0),
            c(0, 0)) %>% list()
    )
  )

  # p_landscape <- ggplot() +
  #   geom_sf(data = landscape_sf, fill = NA) +
  #   theme_grid_plot() +
  #   theme_blank_background() +
  #   NULL
  # p_landscape
  # create_grid <- function(landscape,
  #                         cellarea = NULL,
  #                         n = NULL,
  #                         middle = FALSE,
  #                         celltype = c("square", "hexagon", "hexagon_rot", "triangle"),
  #                         offset = c("corner", "middle", "bottom", "left"))
  common_cellarea <- 1 / 185
  g_sq <- create_grid(landscape_sf,  cellarea = common_cellarea, celltype = "square")
  g_hex <- create_grid(landscape_sf, cellarea = common_cellarea, celltype = "hexagon")
  g_tri <- create_grid(landscape_sf, cellarea = common_cellarea, celltype = "triangle")


  # p_landscape + geom_sf(fill = NA, data = g_tri)
  # p_landscape + geom_sf(fill = NA, data = g_sq)
  # p_landscape + geom_sf(fill = NA, data = g_hex)
  g_sq$geometry %>% vapply(\(x) unclass(x) %>% `[[`(1) %>% nrow(), integer(1)) %>%
    all(. == 4 + 1) %>%
    expect_true()
  g_tri$geometry %>% vapply(\(x) unclass(x) %>% `[[`(1) %>% nrow(), integer(1)) %>%
    all(. == 3 + 1) %>%
    expect_true()
  # g_hex$geometry %>% vapply(\(x) unclass(x) %>% `[[`(1) %>% nrow(), integer(1)) %>%
  #   all(. == 6 + 1) # doesn't work because of the border



  # p_landscape + geom_sf(fill = NA, data = create_grid(landscape_sf, cellarea = 11, celltype = "triangle"))
  cellareas <- seq_cellarea(
    n = 100,
    min_cellarea = 1 / 2000, max_cellarea = 1)
  for (cellarea in cellareas) {
    expect_no_error(g_tri <- create_grid(landscape_sf, cellarea = cellarea, celltype = "triangle"))
    expect_no_error(g_sq <- create_grid(landscape_sf, cellarea = cellarea, celltype = "square"))
    expect_no_error(g_hex <- create_grid(landscape_sf, cellarea = cellarea, celltype = "hexagon"))

    expect_true(
      g_sq$geometry %>% vapply(\(x) unclass(x) %>% `[[`(1) %>% nrow(), integer(1)) %>%
        all(. == 4 + 1)
    )
    expect_true(
      g_tri$geometry %>% vapply(\(x) unclass(x) %>% `[[`(1) %>% nrow(), integer(1)) %>%
        all(. == 3 + 1)
    )
    # g_hex$geometry %>% vapply(\(x) unclass(x) %>% `[[`(1) %>% nrow(), integer(1)) %>%
    #   all(. == 6 + 1) # doesn't work because of the border

  }
})
