#'
#'
#'
library(magrittr)
library(tidyverse)
library(sf)


devtools::load_all()

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

  model_func <- function(y, times, parameters) {
    N <- parameters$N
    beta_mat <- parameters$beta_mat

    S <- y[1:N]
    I <- y[(N + 1):(2 * N)]

    infections <- beta_mat * S * I
    dS <- -infections
    dI <- +infections

    # reporting
    carry <- parameters$carry
    prevalence_population <- sum(I)  / sum(carry)

    list(c(dS, dI),
         prevalence_population = prevalence_population)
  }

  find_target_prevalence <- function(y, times, parameters) {
    N <- parameters$N
    I <- y[(N + 1):(2 * N)]

    carry <- parameters$carry
    # could also be divided by sum(y)
    prevalence_population <- sum(I) / sum(carry)

    c(prevalence_population - 0.5, sum(I))
  }

  deSolve::ode(
    verbose = TRUE,
    y = y_init,
    parms = parameter_list,
    times = c(0, Inf),
    rootfunc = find_target_prevalence,
    func = model_func,
    ynames = FALSE,
  )

}

world_scale <- 17
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape

world_area <- st_area(world_landscape)

# n <- 100
# n <- 50
n <- 30
grid <- create_grid(n, world_landscape, landscape_scale = world_scale)

population_total <- 1.2 * world_area
grid$carry <- population_total / nrow(grid)

y_init <- c(S = grid$carry,
            I = numeric(length(grid$carry)))
# find closest to center
closest_to_middle <-
  st_nearest_feature(world_landscape %>% st_centroid(),
                     grid)
ggplot() +
  geom_sf(data = grid, fill = NA) +

  geom_sf(data = grid[closest_to_middle,],
          fill = "blue") +
  theme_blank_background()

#' Add infected animal
y_init[nrow(grid) + closest_to_middle] <- 1
# ignoring
y_init %>%
  table()

dist_grid <- st_distance(grid)

dist_grid %>%
  str()

dist_score <- dist_grid
diag(dist_score) <- Inf



nrow(grid)
grid
beta_baseline <- 0.05
beta_mat <- beta_baseline * Matrix::diag(nrow = n, ncol = n)
beta_mat %>% dim()

grid$centroid <- st_centroid(grid)
grid$id <- seq_len(nrow(grid))

ggplot() +
  geom_sf(data = grid, fill = NA) +

  geom_sf_text(data = grid, aes(geometry = centroid, label = id)) +

  geom_sf(data = grid[closest_to_middle,],
          fill = "blue") +
  theme_blank_background()


grid

# Matrix::image(Matrix::Matrix(beta_mat))

# beta_mat <- beta_mat + beta_baseline * (1 / dist_score)
beta_mat <- beta_mat + beta_baseline * exp(-dist_score)

#' What about the order of the distance matrix?
# Matrix::Matrix(dist_grid) %>% Matrix::image()
#
# dist_grid2 <- dist_grid[order(dist_grid[1,]),order(dist_grid[1,])]
# Matrix::Matrix(dist_grid2) %>% Matrix::image()
#
# dist_grid3 <- dist_grid[order(dist_grid[closest_to_middle,]),
#                         order(dist_grid[closest_to_middle,])]
# Matrix::Matrix(dist_grid3) %>% Matrix::image()

create_si_model(grid, beta_mat, y_init) ->
  model_output
beta_mat


tau_output <-
  model_output[, c(1, (2*nrow(grid) + 2):ncol(model_output))]
tau_output
