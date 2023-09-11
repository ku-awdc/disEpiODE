#'---
#'title: "Discrete ODE models"
#'subtitle: "Benchmark"
#'output: html_output
#'---
#'
#'
#+setup
library(magrittr)
library(tidyverse)
library(sf)

devtools::load_all()
#'
#'

if (!exists("params")) {
  params <- list(
    world_scale = 29,
    beta_baseline = 0.05,
    buffer_radius = 3.5,
    n = 11
  )

}

world_scale   <- params$world_scale
n             <- params$n
beta_baseline <- params$beta_baseline
buffer_radius <- params$buffer_radius
#'
#' ## Full-code in one function
#'
full_function <- function(...) {
  list2env(list(...), environment())
  source_target <-
    disEpiODE:::get_buffer_source_target(landscape_width = world_scale,
                                         landscape_height = world_scale,
                                         buffer_radius = buffer_radius,
                                         buffer_offset_percent = 0.2)
  middle_buffer_point <- source_target$buffer_point %>%
    st_coordinates() %>%
    st_linestring() %>%
    st_centroid()
  middle_buffer <- st_sf(buffer_point = st_sfc(middle_buffer_point)) %>%
    mutate(label = "middle",
           buffer_polygon = st_buffer(buffer_point, buffer_radius),
           #TODO: replace with PI**2*buffer_radius
           buffer_area = st_area(buffer_polygon)
    )

  world <- create_landscape(scale = world_scale)
  world_landscape <- world$landscape
  # glimpse(world)
  # glimpse(source_target)
  # glimpse(middle_buffer)

  all_buffers <-
    rbind(source_target, middle_buffer) %>%
    mutate(label = factor(label, c("source", "middle", "target")))

  world_area <- st_area(world_landscape)

  grid <- create_grid(n, world_landscape,
                      landscape_scale = world_scale)
  grid <- grid %>% rowid_to_column("id")
  population_total <- world_area
  grid$carry <- st_area(grid$geometry)

  y_init <- c(S = grid$carry,
              I = numeric(length(grid$carry)))
  #'

  # disEpiODE:::create_buffer_overlap(grid, buffer)
  all_buffers %>%
    rowwise() %>%
    dplyr::group_map(
      \(buffer, ...)
      disEpiODE:::create_buffer_overlap(grid, buffer)
    ) ->
    all_buffers_overlap

  all_buffers_overlap_map <-
    all_buffers_overlap %>%
    map(disEpiODE:::create_buffer_overlap_map) %>%
    flatten()

  source_overlap <- all_buffers_overlap_map$source
  target_overlap <- all_buffers_overlap_map$target
  middle_overlap <- all_buffers_overlap_map$middle

  # source_overlap
  # target_overlap
  # middle_overlap

  #' Add infected animal

  half_infected_mass <-
    grid$carry[source_overlap$id_overlap] *
    source_overlap$weight *
    (1/2)
  #' remove mass from susceptible
  y_init[source_overlap$id_overlap] <-
    y_init[source_overlap$id_overlap] - half_infected_mass
  y_init[
    nrow(grid) +
      source_overlap$id_overlap
  ] <- +half_infected_mass

  dist_grid <- st_distance(grid$geometry %>% st_centroid())
  # isSymmetric(dist_grid)
  beta_mat <- beta_baseline * exp(-dist_grid)
  disEpiODE:::create_si_model(grid, beta_mat, y_init, target_overlap, middle_overlap, verbose = FALSE) ->
    model_output

  grid
}
#'
#' ## Benchmark
#'
full_function(n = 50)

bench_full_function <- bench::press(
  # n = c(1,2,5, 10, 15, 20, 50),
  n = seq_len(50),
  {
    bench::mark(
      full_function(n = n),
      check = FALSE,
      min_iterations = 5
    )
  }
)
#'
#'
readr::write_rds(bench_full_function,
                 file = "output/011_bench_full_function")
#'
bench_full_function %>%
  glimpse() %>%
  unnest_longer(time) %>%
  # filter(n < 50) %>%
  ggplot() +
  aes(n, time) +
  # geom_step() +
  geom_point(shape = 20) +
  theme_blank_background()
#'
#'
summary(
  print(
    glm(time ~ n, data = bench_full_function %>%
          unnest_longer(time),
        family = poisson())
  )
)

# WITHOUT intercept: However this model is wildly
# inaccurate.
#
#

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# n 0.0425112  0.0009289   45.76   <2e-16 ***

#
bench_full_function %>%
  glimpse() %>%
  unnest_longer(time) %>%
  mutate(time = as.numeric(time)) %>%
  # filter(n < 50) %>%
  ggplot() +
  aes(n, time) +
  # geom_step() +
  # without intercept
  # stat_function(fun = \(x) exp(0.0425112*x)) +
  # with intercept
  stat_function(fun = \(n) exp(-1.894083 + 0.089333*n)) +

  # stat_smooth(method = "glm",
  #             # formula = y ~ x - 1,
  #             # orientation = "y",
  #             method.args = list(family = poisson)) +
  geom_point(shape = 20) +
  theme_blank_background()
