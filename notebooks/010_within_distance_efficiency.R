#'
#+ setup, message=FALSE
library(magrittr)
library(tidyverse)
library(sf)

devtools::load_all(".")

#' ## Motivation

curve(exp(-x), to = 17)
abline(v = print(qexp(0.99)))
abline(v = print(qexp(0.999)))
abline(v = print(qexp(0.9999)))
abline(v = print(qexp(0.99999)))

#'
world_scale <- 29
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape
# world_area <- st_area(world_landscape)
# n <- 10
exp(-6.9)
exp(-11.51293)
output_summary <- expand_grid(
  n = seq_len(50)
) %>%
  rowwise() %>%
  mutate(
    nonzero_prop  = {
      grid <- create_grid(n, world_landscape,
                          landscape_scale = world_scale)

      grid_geometry_centroid <-
        grid$geometry %>% st_centroid()
      st_is_within_distance(grid_geometry_centroid,
                            dist = 6.9) %>%
        lengths() %>%
        sum() %>% {
          N <- (n**2)
          # ((N * (N-1))/2 + N) - .
          # ((N**2 + N)/2) - .
          . / ((N**2 + N)/2)
        }
    }
  )
# dist_grid <- st_distance(grid$geometry %>% st_centroid())
# isSymmetric(dist_grid)
# diag(dist_grid) %>% table()
# beta_mat <- beta_baseline * exp(-dist_grid)
output_summary %>%
  ggplot() +
  aes(n, nonzero_prop) +
  # aes(n**2, nonzero_prop) +
  geom_step() +

  theme_blank_background()
