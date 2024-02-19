

landscape_sf <- st_sfc(
  st_polygon(
    rbind(c(0, 0),
          c(0, 1),
          c(1, 1),
          c(1, 0),
          c(0, 0)) %>% list()
  )
)
#'
#'
#' Pig farms or zones defined...

buffer_radius <- 0.15
buffer_offset_percent <- 0.2
source_target <-
  get_buffer_source_target(landscape_width = 1,
                           landscape_height = 1,
                           buffer_radius = buffer_radius,
                           buffer_offset_percent = buffer_offset_percent)
middle_buffer <- get_middle_buffer(source_target = source_target,
                                   buffer_radius = buffer_radius)

all_buffers <-
  rbind(source_target, middle_buffer) %>%
  mutate(label = factor(label, c("source", "middle", "target")))

#' Multiple grids represented

bind_rows(
  # triangle = create_grid(landscape_sf, n = 24, celltype = "triangle"),
  # square = create_grid(landscape_sf, n = 24, celltype = "square"),
  triangle = create_grid(landscape_sf, cellarea = 1 / 185, celltype = "triangle"),
  square = create_grid(landscape_sf,   cellarea = 1 / 185, celltype = "square"),
  hexagon = create_grid(landscape_sf,  cellarea = 1 / 185, celltype = "hexagon"),
  .id = "celltype"
) %>%
  mutate(celltype = fct_inorder(celltype)) %>%
  identity() %>% {
    ggplot(.) +

      geom_sf(fill = NA) +

      geom_sf(data = all_buffers,
              aes(geometry = buffer_polygon, color = label),
              linewidth = 1,
              fill = NA) +

      # geom_sf_text(aes(label = "🐖",
      #                  geometry = buffer_point),
      #              size = 10,
      #              data = all_buffers) +
      facet_wrap(~celltype) +
      guides(color = guide_legend(override.aes = list(fill = NA))) +
      labs(color = NULL) +
      theme(legend.position = "bottom") %+%
      theme_grid_plot() %+%
      theme_blank_background() %+%
      NULL
  }
#'
#' Plot the three types of discretisations considered in this paper.
#' 

ggplot() +
  geom_sf(data = grid, fill = NA) +
  geom_sf_text(aes(label = "🐷", geometry = buffer_point),
               size = 10,
               data = all_buffers) +
  geom_sf(data = all_buffers,
          aes(geometry = buffer_polygon, color = label),
          linewidth = 1,
          fill = NA) +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.justification = "center") +
  NULL
