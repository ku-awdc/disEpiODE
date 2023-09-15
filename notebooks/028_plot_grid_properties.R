

library(magrittr)
library(conflicted)


library(tidyverse)
library(sf)

devtools::load_all()


world <- create_landscape(world_scale <- 4)
world$landscape -> world_landscape
# cellarea <- 1
# create_grid(
#   landscape = world_landscape,
#   cellarea = cellarea
# )


# create_grid(
#   landscape = world_landscape,
#   cellarea = 20
# ) %>%
#   print() %>%
#   nrow()

min_cellarea <- 0.005
create_grid(
  landscape = world_landscape,
  cellarea = min_cellarea
) %>%
  print() %>%
  nrow()

precision <- 0.1
# cellarea <- world_scale**2 * (1 - precision) ** x
# cellarea_min <- world_scale * (1 - precision) ** x
# cellarea_min / world_scale <- (1 - precision) ** x
# log(cellarea_min / world_scale) <- x * log(1 - precision)
# x <- log(cellarea_min / world_scale) / log(1 - precision)
max_x <- log(min_cellarea / (world_scale**2)) / log(1 - precision)
max_x <- floor(max_x)

all_cellarea <- (world_scale**2) * (1 - precision) ** (0:max_x)
all_cellarea
# n <- 100
# precision <- 0.1
# all_cellarea <- world_scale * (1 - precision)**(0:n)
# all_cellarea %>% zapsmall()

expand_grid(
  cellarea = all_cellarea,
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
    theme_blank_background() +
    # scale_x_log10() +
    # scale_x_log_rev() +
    # scale_x_log10_rev() +
    scale_y_log10() +
    scale_x_log10_rev() +
    theme(axis.line.x = element_line(
      arrow = arrow(
        length = unit(0.5, "char"),
        ends = "first", type = "closed")
    )) +
    NULL
} %>%
  # plotly::ggplotly() %>%
  identity()
#'
#'
#'
