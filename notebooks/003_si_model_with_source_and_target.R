#'
#'
#'
library(magrittr)
library(tidyverse)
library(sf)


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
create_si_model <- function(grid, beta_mat, y_init) {

  stopifnot(
    all(c("carry") %in% names(grid))
  )

  # world <- st_union(grid)

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

    list(c(dS, dI),
         prevalence_population = prevalence_population)
  }

  find_target_prevalence <- function(times, y, parameters) {
    N <- parameters$N
    I <- y[(N + 1):(2 * N)]

    carry <- parameters$carry
    # could also be divided by sum(y)
    prevalence_population <- sum(I) / sum(carry)

    c(prevalence_population - 0.5, zapsmall(sum(I)))
  }
  # browser()
  ode_parameters <- list(
    verbose = TRUE,
    y = y_init,
    parms = parameter_list,
    # times = c(0, Inf),
    # times = seq.default(0, 10, length.out = 250),
    rootfunc = find_target_prevalence,
    func = model_func,
    ynames = FALSE
  )
  tau_model_output <-
    rlang::exec(deSolve::ode, !!!ode_parameters, times = c(0, Inf))
  #TODO: check if tau exists
  tau <- tau_model_output[2,1]

  model_output <-
    rlang::exec(deSolve::ode, !!!ode_parameters,
                times = seq.default(0, tau, length.out = 250))

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

population_total <- world_area

grid$carry <- st_area(grid$geometry)

y_init <- c(S = grid$carry,
            I = numeric(length(grid$carry)))
# find closest to center
closest_to_middle <-
  st_nearest_feature(world_landscape %>% st_centroid(),
                     grid$geometry %>% st_centroid())
ggplot() +
  geom_sf(data = grid, fill = NA) +

  geom_sf(data = grid[closest_to_middle,],
          fill = "blue") +
  theme_blank_background()

#' Add infected animal
y_init[nrow(grid) + closest_to_middle] <- 0.5 * grid$carry[closest_to_middle]
y_init[closest_to_middle] <- y_init[nrow(grid) + closest_to_middle]

y_init

# ignoring
y_init %>%
  zapsmall() %>%
  table()

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


y_init[(1:nrow(grid))] -> S
y_init[(nrow(grid) + 1):(2 * nrow(grid))] -> I

sum(I) / sum(S + I)
sum(I) / sum(grid$carry)

y_init[(1:nrow(grid))] %>% zapsmall() %>% table()
y_init[(nrow(grid) + 1):(2 * nrow(grid))] %>% table()

create_si_model(grid, beta_mat, y_init) ->
  model_output
model_output$tau_model_output[2,1]
model_output$model_output %>% str()
#' Population prevalence throughout
model_output$model_output[,c(1, (2*nrow(grid) + 2):ncol(model_output$model_output)),
                          drop = FALSE] ->
  prevalence_throughout

#' Population prevalence throughout
prevalence_throughout %>%
  as_tibble() %>%
  ggplot() +
  aes(time, prevalence_population) +
  geom_line()

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

