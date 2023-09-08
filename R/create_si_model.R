

#' Deterministic SI model with pair-wise infectious interactions
#'
#' @param grid Represents a grid of a landscape
#' @param beta_mat Infection rate matrix
#' @param y_init c(S, I) compartments for t = 0
#' @param target_overlap
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
  tau <- tau_model_output[2, 1]

  #' this follows the population from 0 until
  #' tau (time when prevalence(target) = 50%)
  model_output <-
    rlang::exec(deSolve::ode, !!!ode_parameters,
                # Matt: 2*tau instead for a window
                times = seq.default(0, tau, length.out = 250),
                rootfunc = terminate_extinction)

  list(
    tau_model_output = tau_model_output,
    model_output = model_output
  )
}


#' @description S->I model that calculates prevalences as well.
#'
#' @param y Must have length 2×number of cells
#'
#' @note `parameters` must contain `N`, `beta_mat` and `carry`
model_func <- function(times, y, parameters) {
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
  prevalence_population <- sum(I)  / sum(carry) # alternative: divide by sum(S + I) instead
  # weighted arithmetic mean of prevalence
  prevalence_target <- sum((I[target_overlap$id_overlap] /
                              carry[target_overlap$id_overlap]) * target_overlap$weight) /
    sum(target_overlap$weight)

  list(c(dS, dI),
       prevalence_target = prevalence_target,
       prevalence_population = prevalence_population)
}

#' @description Terminate when there are no more susceptible,
#' `S == 0`, or if there are no more infected,
#' `I == 0`.
#'
#' @note `parameters` must contain `N`
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
#'
#' @note `parameters` must contain `N`.
find_population_prevalence <- function(times, y, parameters) {
  N <- parameters$N
  I <- y[(N + 1):(2 * N)]

  carry <- parameters$carry
  # could also be divided by sum(y)
  prevalence_population <- sum(I) / sum(carry)

  c(prevalence_population - 0.5,
    terminate_extinction(times, y, parameters))
}

#' @description Root-function for terminating the simulation
#' when the target-buffer has a prevalence of 50%.
find_target_prevalence <- function(times, y, parameters) {
  N <- parameters$N
  I <- y[(N + 1):(2 * N)]

  carry <- parameters$carry

  prevalence_target <- mean(
    I[target_overlap$id_overlap] * target_overlap$weight /
      (carry[target_overlap$id_overlap] * target_overlap$weight)
  )
  # mean(I / carry)
  # sum(y_i×w_i) / sum(w_i) #weighted arithmetic mean
  # prevalence_target <- sum(
  #   I[target_overlap$id_overlap] / carry[target_overlap$id_overlap]
  # )
  # APPROACH: MATT
  prevalence_target <- sum((I[target_overlap$id_overlap] /
                              carry[target_overlap$id_overlap]) * target_overlap$weight) /
    sum(target_overlap$weight)
  #
  # APPROACH:
  # prevalence_target <- sum(I[target_overlap$id_overlap] * target_overlap$weight) / sum(target_overlap$weight)

  c(prevalence_target - 0.5,
    terminate_extinction(times, y, parameters))
}

