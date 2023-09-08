#'
#'
#'
library(magrittr)
library(tidyverse)
library(sf)

library(glue)


devtools::load_all()
#'
#'
#'


#' Deterministic SI model with pair-wise infectious interactions
#'
#' @param grid Represents a grid of a landscape
#' @param beta_mat Infection rate matrix
#'
#' @return
#' @export
#'
#' @examples
create_si_model <- function(grid, beta_mat, y_init, target_overlap) {

  stopifnot(
    all(c("id", "geometry", "carry") %in% names(grid)),
    all(dim(beta_mat) == c(length(grid$geometry), length(grid$geometry))),
    length(y_init) == 2 * length(grid$geometry),
    all(c("id_overlap", "weight") %in% names(target_overlap))
  )

  parameter_list <- list(
    beta_mat = beta_mat,
    N = nrow(grid),
    carry = grid$carry
  )

  model_func <- function(times, y, parameters) {
    # browser()
    N <- parameters$N
    beta_mat <- parameters$beta_mat

    S <- y[1:N]
    I <- y[(N + 1):(2 * N)]

    # infections <- beta_mat * S * I
    infections <- (I %*% beta_mat) * S
    dS <- -infections
    dI <- +infections

    # reporting
    carry <- parameters$carry
    prevalence_population <- sum(I)  / sum(carry)
    # prevalence_target <- sum(
    #   I[target_overlap$id] * target_overlap$weight /
    #     carry[target_overlap$id] * target_overlap$weight
    # )
    prevalence_target <- sum(
      I[target_overlap$id_overlap] / carry[target_overlap$id_overlap]
    )

    list(c(dS, dI),
         prevalence_target = prevalence_target,
         prevalence_population = prevalence_population)
  }

  #' @description Terminate when there are no more susceptible,
  #' `S == 0`, or if there are no more infected,
  #' `I == 0`.
  terminate_extinction <- function(times, y, parameters) {
    N <- parameters$N
    S <- y[1:N]
    I <- y[(N + 1):(2 * N)]

    c(
      zapsmall(sum(S)),
      zapsmall(sum(I))
    )
  }

  #' @description Root-function for population prevalence equal to 50%.
  find_population_prevalence <- function(times, y, parameters) {
    N <- parameters$N
    I <- y[(N + 1):(2 * N)]

    carry <- parameters$carry
    # could also be divided by sum(y)
    prevalence_population <- sum(I) / sum(carry)

    c(prevalence_population - 0.5,
      terminate_extinction(times, y, parameters))
  }

  find_target_prevalence <- function(times, y, parameters) {
    N <- parameters$N
    I <- y[(N + 1):(2 * N)]

    carry <- parameters$carry

    # prevalence_target <- sum(
    #   I[target_overlap$id] * target_overlap$weight /
    #     carry[target_overlap$id] * target_overlap$weight
    # )
    prevalence_target <- sum(
      I[target_overlap$id_overlap] / carry[target_overlap$id_overlap]
    )

    c(prevalence_target - 0.5,
      terminate_extinction(times, y, parameters))
  }

  # common parameters
  ode_parameters <- list(
    verbose = TRUE,
    y = y_init,
    parms = parameter_list,
    func = model_func,
    ynames = FALSE
  )
  tau_model_output <-
    rlang::exec(deSolve::ode, !!!ode_parameters,
                rootfunc = find_target_prevalence,
                times = c(0, Inf))
  #TODO: check if tau exists
  tau <- tau_model_output[2,1]

  #

  #' this follows the population from 0 until
  #' tau (time when prevalence(target) = 50%)
  model_output <-
    rlang::exec(deSolve::ode, !!!ode_parameters,
                times = seq.default(0, tau, length.out = 250),
                rootfunc = terminate_extinction)

  list(
    tau_model_output = tau_model_output,
    model_output = model_output
  )
}

world_scale <- 29
source_target <-
  get_buffer_source_target(landscape_width = world_scale,
                           landscape_height = world_scale,
                           buffer_radius = 3.5,
                           buffer_offset_percent = 0.2)
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape

glimpse(world)
glimpse(source_target)

ggplot() +
  geom_sf(data = world_landscape, fill = NA) +
  geom_sf(data = source_target,
          aes(fill = label, geometry = buffer_polygon)) +
  theme_blank_background()

world_area <- st_area(world_landscape)

# n <- 100
# n <- 50
n <- 30
grid <- create_grid(n, world_landscape, landscape_scale = world_scale)
grid <- grid %>% rowid_to_column("id")
population_total <- world_area
grid$carry <- st_area(grid$geometry)

y_init <- c(S = grid$carry,
            I = numeric(length(grid$carry)))
#'
#' Define source- and target-buffer indices and weights
source_target_overlap <-
  # this assumes that `grid` has `id` in it
  st_intersection(source_target %>%
                    `st_geometry<-`("buffer_polygon"),
                  grid) %>%
  rename(id_overlap = id,
         buffer_overlap = buffer_polygon) %>%
  mutate(overlap_area = st_area(buffer_overlap),
         # normalise with corresponding grid cells' area
         weight = overlap_area / st_area(grid$geometry[id_overlap]),
         overlap_area = NULL) %>%
  identity()
#'
source_target_overlap



ggplot() +
  geom_sf(data = source_target_overlap, fill = NA) +
  theme_blank_background()
#'
#'
#'

# reduce the carried over information
source_target_overlap <- source_target_overlap %>%
  as_tibble() %>%
  select(-carry, -buffer_area, -buffer_point, -buffer_overlap) %>%
  identity()
# Ignore this warning

source_target_overlap_list <-
  source_target_overlap %>%
  nest(data = -label) %>%
  deframe()

source_target_overlap

source_overlap <- source_target_overlap_list$source
target_overlap <- source_target_overlap_list$target

source_overlap
target_overlap


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

stopifnot(
  "compartments must be non-negative" =
    all(y_init >= 0),
  "equal mass to preset carrying capacity" =
    isTRUE(all.equal(sum(y_init), sum(grid$carry)))
)

beta_baseline <- 0.05
dist_grid <- st_distance(grid$geometry %>% st_centroid())
isSymmetric(dist_grid)
diag(dist_grid) %>% table()
beta_mat <- beta_baseline * exp(-dist_grid)
# VALIDATION
# Matrix::image(Matrix::Matrix(beta_mat))

# grid$centroid <- st_centroid(grid)
# grid$id <- seq_len(nrow(grid))

# ggplot() +
#   geom_sf(data = grid, fill = NA) +
#
#   geom_sf_text(data = grid, aes(geometry = centroid, label = id)) +
#
#   geom_sf(data = grid[closest_to_middle,],
#           fill = "blue") +
#   theme_blank_background()


grid

dist_grid
# beta_mat <- beta_mat + beta_baseline * (1 / dist_score)
# beta_mat <- beta_mat + beta_baseline * exp(-dist_score)

#' What about the order of the distance matrix?
# Matrix::Matrix(dist_grid) %>% Matrix::image()
#
# dist_grid2 <- dist_grid[order(dist_grid[1,]),order(dist_grid[1,])]
# Matrix::Matrix(dist_grid2) %>% Matrix::image()
#
# dist_grid3 <- dist_grid[order(dist_grid[closest_to_middle,]),
#                         order(dist_grid[closest_to_middle,])]
# Matrix::Matrix(dist_grid3) %>% Matrix::image()

create_si_model(grid, beta_mat, y_init, target_overlap) ->
  model_output

model_output$tau_model_output
model_output$tau_model_output[2,1] # tau

model_output$model_output

#' Extract population and target prevalence throughout the trajectory:
model_output$model_output[,c(1, (2*nrow(grid) + 2):ncol(model_output$model_output)),
                          drop = FALSE] ->
  prevalence_throughout
prevalence_throughout %>% head()

#' Population prevalence throughout
prevalence_throughout %>%
  as_tibble() %>%
  pivot_longer(starts_with("prevalence_"),
               names_to = c(NA, "prevlance_name"),
               names_sep = "_") %>%
  ggplot() +
  aes(time, value, group = prevlance_name) +
  geom_line(aes(color = prevlance_name)) +
  lims(y = c(0, 0.5)) +
  theme_blank_background() +
  theme(legend.position = "bottom") +
  labs(caption = "{n}")

I_grid <-
  model_output$tau_model_output[2, (nrow(grid) + 2):(2 * nrow(grid) + 1)] %>%
  # names()
  identity()
prevalence_grid <- (I_grid / grid$carry)
ggplot() +
  geom_sf(data = grid %>% mutate(prev = prevalence_grid),
          aes(fill = prev)) +

  geom_sf(data = source_target,
          fill = NA,
          aes(color = label, geometry = buffer_polygon)) +

  scale_fill_viridis_c() +
  theme(legend.position = "bottom") +
  theme_blank_background()

# # model_output[2, (1:nrow(grid))] -> S
# #
# # sum(I) / sum(S+I)
#
# model_output %>% str()
#
# model_output[2, (nrow(grid) + 2):(2 * nrow(grid) + 1)] -> I
# # I %>% names() %>% head()
# # I %>% names() %>% tail()
# (I / grid$carry) -> prevalence_grid
#
# ggplot() +
#   geom_sf(data = grid %>% mutate(prev = prevalence_grid),
#           aes(fill = prev)) +
#
#   # geom_sf(data = grid[closest_to_middle,],
#   #         fill = "blue") +
#
#   scale_fill_viridis_c() +
#   theme_blank_background()
#
# tau_output <-
#   model_output[, c(1, (2 * nrow(grid) + 2):ncol(model_output))]
# tau_output

