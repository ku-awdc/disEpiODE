#'
#'
#'
library(magrittr)
library(tidyverse)
library(sf)

#' Title
#'
#' @param grid Reprents a grid of a landscape
#' @param beta_mat Infection rate matrix
#'
#' @return
#' @export
#'
#' @examples
create_si_model <- function(grid, beta_mat) {
  world <- st_union(grid)

}

world_scale <- 17
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape

world_area <- st_area(world_landscape)

n <- 100
grid <- create_grid(n, world_landscape, landscape_scale = world_scale)

grid
beta_baseline <- 0.05
beta_mat <- beta_baseline * Matrix::diag(nrow = n, ncol = n)
Matrix::image(Matrix::Matrix(beta_mat))

create_si_model(grid, beta_mat)
