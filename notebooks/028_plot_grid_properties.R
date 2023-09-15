

library(magrittr)
library(conflicted)


library(tidyverse)
library(sf)

devtools::load_all()


world <- create_landscape(world_scale <- 4)
world$landscape -> world_landscape
cellarea <- 1
create_grid(
  landscape = world_landscape,
  cellarea = cellarea
)
expand_grid(
  # cellarea = c(0.5, 10),
  cellarea = seq.default(
    length.out = 1000,
    0, to = st_area(world_landscape))[-1],
  celltype =
    c("square", "hexagon", "hexagon_rot", "triangle")
) %>%
  rowwise() %>%
  mutate(grid = create_grid(world_landscape,
                            cellarea = cellarea,
                            celltype = celltype) %>%
           list(),
         n_grid = grid %>% nrow()) %>%
  ungroup() ->
  grid_df



grid_df %>% {
  ggplot(.) +
    aes(cellarea, n_grid) +
    geom_step(aes(color = celltype)) +
    scale_x_reverse() +
    theme_blank_background()
}
#
#'  The found grids are not increasing in `n` (total cells) linearly


curve(plogis(x, scale = 50, lower.tail = FALSE), to = 150)
curve(plogis(sqrt(x), scale = 5, lower.tail = FALSE), to = 100)

expand_grid(
  cellarea = c(
    # seq.default(0, 1, length.out = 25)[-1], #100
    # seq.default(0, 1, length.out = 70)[-1],
    plogis(lower.tail = FALSE, scale = 5, q = seq.default(50)),
    seq_len(10)
  ),
  # cellarea = world_scale / seq.default(2, 100),
  # cellarea = c(0.5, 10),
  # cellarea = seq.default(
  #   length.out = 1000,
  #   0, to = st_area(world_landscape))[-1],
  celltype =
    c("square", "hexagon", "hexagon_rot", "triangle")
) %>%
  rowwise() %>%
  mutate(grid = create_grid(world_landscape,
                            cellarea = cellarea,
                            celltype = celltype) %>%
           list(),
         n_grid = grid %>% nrow()) %>%
  ungroup() ->
  grid_df



grid_df %>%
  ggplot(.) +
  aes(cellarea, n_grid) +
  geom_step(aes(color = celltype)) +
  scale_x_reverse() +
  scale_y_log10() +
  theme_blank_background()
#'
#'
#'
#'
