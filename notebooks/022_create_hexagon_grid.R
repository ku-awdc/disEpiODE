
library(magrittr)
library(glue)
# library(ggplot2)
library(tidyverse)
library(sf)
devtools::load_all()

world_scale <- 29
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape
# world_area <- st_area(world_landscape)

library(future)
library(furrr)
plan(multisession(workers = 4))

grids_df <- expand_grid(
  n = seq_len(50),
  offset = c("corner", "middle", "bottom", "left"),
  square = c(TRUE, FALSE)) %>%
  mutate(grid =
           furrr::future_pmap(., function(n, offset, square)
             disEpiODE:::create_grid(n = n,
                                     landscape = world_landscape,
                                     landscape_scale = world_scale,
                                     offset = offset, square = square))
  )
grids_df$grid[[10]] %>% nrow()
grids_df$grid[[10]] %>% st_collection_extract(type = "POLYGON")

grids_df <- grids_df %>%
  mutate(n_grid = grid %>% map_dbl(nrow))
#'
#'
#' Make sure that the grid count is somewhat the same between
#' the two `square == TRUE` vs. `square == FALSE`.
grids_df %>%
  glimpse() %>%
  ggplot() +
  aes(n, n_grid) +
  aes(group = str_c(offset, square)) +
  geom_step(aes(color = offset)) +
  # facet_wrap(offset~square, scales = "free") +
  facet_wrap(~square, scales = "free") +
  theme_blank_background()
#'
#'
#'
#' Inspect randomly grids just to see if they are correct looking..
grids_df %>%
  sample_n(size = 5) %>%
  pmap(function(n, offset, square, grid, n_grid) {
    ggplot() +
      geom_sf(data = world_landscape, fill = NA, linetype = "dotted") +
      geom_sf(data = grid, fill = NA) +
      labs(caption = glue("offset = {offset}, n = {n}, n² = {n**2}, n_grid = {n_grid}")) +
      theme_blank_background()
  })












#' st_make_grid(x = world_landscape,
#'              cellsize = world_scale / 1,
#'              offset = c(world_scale, world_scale) / 2,
#'              # offset = c(world_scale, world_scale) / 2,
#'              # dy = sqrt(3) * dx/2
#'              # cellsize = c(world_scale, (sqrt(3)*world_scale)/2),
#'              # cellsize = world_scale * 2 / sqrt(3),
#'              what = "polygons",
#'              square = FALSE) %>%
#'   st_intersection(world_landscape, dimensions = "polygon") %>%
#'   print() %>%
#'   # st_simplify(preserveTopology = FALSE) %>%
#'   st_make_valid()
#'
#' ggplot() +
#'   geom_sf(fill = NA,
#'           data = world_landscape,
#'           linetype = "dotted") +
#'   geom_sf(fill = NA, data =
#'             st_make_grid(x = world_landscape,
#'                          # cellsize = world_scale / ,
#'                          offset = c(world_scale, world_scale) / 2,
#'                          # offset = c(world_scale, world_scale) / 2,
#'                          # dy = sqrt(3) * dx/2
#'                          # cellsize = c(world_scale, (sqrt(3)*world_scale)/2),
#'                          # cellsize = world_scale * 2 / sqrt(3),
#'                          square = FALSE) %>%
#'             st_intersection(world_landscape, dimensions = "polygon")) +
#'   disEpiODE::theme_blank_background()
#'
#' # n <- 10
#' # grid <-
#' #   create_grid(n, landscape = world_landscape,
#' #               landscape_scale = world_scale,
#' #               square = FALSE,
#' #               offset = "corner")
#'
#'
#'
#' grids_stats <- expand_grid(
#'   n = seq_len(5),
#'   # offset = c("corner", "middle", "bottom", "left")
#' ) %>%
#'   rowwise() %>%
#'   mutate(
#'     cellsize = c((world_scale * 2) / (sqrt(3) * n)),
#'     grid = st_make_grid(x = world_landscape,
#'                         offset = c(0,0),
#'                         # offset = c(0,0) - c(cellsize, cellsize) / 2,
#'                         # offset = c(0,0) - c(cellsize/2, 0),
#'                         offset = c(0,0) - c(0, cellsize/2),
#'
#'
#'
#'                         # dy = sqrt(3) * dx/2
#'                         # cellsize = c(world_scale, (sqrt(3)*world_scale)/2),
#'                         cellsize = cellsize,
#'                         square = FALSE) %>%
#'       st_intersection(world_landscape, dimensions = "polygon") %>%
#'       # st_intersection(world_landscape) %>%
#'       list()
#'   ) %>%
#'   # glimpse() %>%
#'   mutate(
#'     `n²` = n**2,
#'     n_grid = grid %>% st_geometry() %>% length(),
#'     area = grid %>% st_area() %>% sum()) %>%
#'   mutate(p_grid = {
#'     ggplot() +
#'       geom_sf(fill = NA,
#'               data = world_landscape,
#'               linetype = "dotted") +
#'       geom_sf(fill = NA, data = grid) +
#'       disEpiODE::theme_blank_background()
#'   } %>%
#'     # print() %>%
#'     list()) %>%
#'   glimpse() %>%
#'   ungroup()
#' grids_stats %>% {
#'   ggplot(.) +
#'     aes(n, n_grid) +
#'     geom_line() +
#'     geom_line(aes(y = `n²`, color = "n²")) +
#'     # geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
#'     theme_blank_background()
#' }
#'
#'
#' #'
#' #'
#' #' # mutate(grid_n = length(grid[[1]]$geometry),
#' #'          cellsize = grid[[1]]$geometry %>% st_area() %>% zapsmall() %>% unique())
#' #'
#' #'
#' #' expand_grid(
#' #'   n = seq_len(50),
#' #'   offset = c("corner", "middle", "bottom", "left")
#' #' ) %>%
#' #'   # sample_n(size = n()) %>%
#' #'   sample_n(3) %>%
#' #'
#' #'   pmap(\(n, offset) {
#' #'
#' #'     ggplot() +
#' #'       geom_sf(data = world_landscape, fill = NA,
#' #'               linestyle = "dotted") +
#' #'       geom_sf(data = create_grid(n,
#' #'                                  landscape = world_landscape,
#' #'                                  landscape_scale = world_scale,
#' #'                                  square = FALSE,
#' #'                                  offset),
#' #'               fill = NA)+
#' #'       disEpiODE::theme_blank_background()
#' #'   }) %>%
#' #'   identity()
#' #'
#' #' #' Just test all the combinations
#' #' for (n in seq_len(50)) {
#' #'   for (offset in c("corner", "middle", "bottom", "left"))
#' #'     create_grid(n, landscape = world_landscape,
#' #'                 landscape_scale = world_scale,
#' #'                 square = FALSE,
#' #'                 offset = offset)
#' #' }
