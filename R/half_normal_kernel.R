

#' Return the `sd` for Half-normal distribution
#'
#' To represent the half-normal distribution using
#' `dnorm`, the `sd` has to be adjusted, to yield
#' a certain mean.
#'
#'
#'
#' @param mean Target mean
#'
#' @return The standard deviation for `dnorm`/`rnorm`, etc. to
#' match the desired mean of a half-normal distribution.
#'
#'
#' @examples
half_normal_sd <- function(mean) {
  sqrt(pi/2) * mean
}


#' Density for the half-normal distribution
#'
#' @param x Numeric vector
#' @param mean mean of the half-normal distribution
#'
#' @return
#'
#'
#' @examples
half_normal_kernel <- function(x, mean = 1, location = 0) {
  (x >= 0) *
    2 *
    dnorm(x, sd = half_normal_sd(mean = mean), mean = location)
}

#' Title
#'
#' @param x
#' @param mean
#' @param location
#' @param magn Magnitude
#'
#' @return
#'
#' @examples
half_normal_param_kernel <- function(x, mean, location, magn) {
  magn * half_normal_kernel(x, mean = mean, location = location)
}


#' 
#' 
#' @export
inv_sigma <- function(distance, sigma = 1) {
  1 / (sigma * distance + 1)
}

#' @export
exp_sigma <- function(distance, sigma = 1) {
  exp(-sigma*distance)
}

#' @export
half_normal_sigma <- function(distance, sigma = 1) {
  exp(-distance**2 / (2 * sigma**2))
}