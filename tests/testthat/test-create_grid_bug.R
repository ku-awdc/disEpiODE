test_that("multiplication works", {

  world_scale <- 29


  world_landscape <- st_bbox(c(xmin = 0, xmax = world_scale,
                               ymin = 0, ymax = world_scale)) %>%
    st_as_sfc()
  cellarea <- seq_cellarea(n = 150, min_cellarea = .50, max_cellarea = 10)


  combos <- tidyr::expand_grid(
    cellarea = cellarea,
    celltype = c("triangle","square"),
    middle = c(TRUE, FALSE)
  ) %>% transpose()

  for (combo in combos) {
    # browser()
    cellarea <- combo$cellarea
    celltype <- combo$celltype
    middle <- combo$middle
    grid <- create_grid(world_landscape,
                        cellarea = cellarea,
                        celltype = celltype,
                        middle = middle
    )
  }

})
