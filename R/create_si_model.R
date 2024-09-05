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
create_si_model <- function(grid, beta_matrix, init, overlap, verbose = FALSE) {

  source_overlap <- overlap %>% filter(label=="source")
  target_overlap <- overlap %>% filter(label=="target")
  middle_overlap <- overlap %>% filter(label=="middle")

  yinit <- init


  result <- list()

  for (beta_mat_name in beta_mat_list) {
    beta_mat <- all_beta_mat[[beta_mat_name]]
    if (remove_within_patch_transmission) {
      diag(beta_mat) <- 0
    }

    tau_model_output <-
      rlang::exec(deSolve::ode,
                  !!!ode_parameters,
                  hmax = na_as_null(hmax_list[[beta_mat_name]]),
                  parms = parameter_list %>% append(list(
                    beta_mat = beta_mat
                  )),
                  # rootfunc = disEpiODE:::find_target_prevalence,
                  rootfunc = disEpiODE:::find_middle_prevalence,
                  times = c(0, Inf)
      )
    output <- list()
    output$rstate <- deSolve::diagnostics(tau_model_output)$rstate
    # TODO: check if tau exists
    output$tau <- tau_model_output[2, 1]
    result[[glue("output_{beta_mat_name}_tau")]] <- output
    result[[glue("output_{beta_mat_name}_tau_state")]] <- tau_model_output

    # save a model you can run with a given end time T
    build_model_function <- function(ode_parameters, hmax, parameter_list, beta_mat) {
      function(times) {
        rlang::exec(deSolve::ode,
                    !!!ode_parameters,
                    hmax = na_as_null(hmax),
                    parms = parameter_list %>% append(list(
                      beta_mat = beta_mat
                    )),
                    # rootfunc = disEpiODE:::find_target_prevalence,
                    # rootfunc = disEpiODE:::find_middle_prevalence,
                    times = times
        )
      }
    }
    current_ode_model <- build_model_function(ode_parameters, hmax_list[[beta_mat_name]], parameter_list, beta_mat)
    # TODO: Maybe store list(run_model = current_ode_model)
    # if so, then the storage of this should mimic the other ways outlined here
    result[[glue("output_ode_model_{beta_mat_name}")]] <- list(run_model = current_ode_model)
    # result[[glue("output_ode_model")]][[beta_mat_name]] <- current_ode_model

    # TODO: consider adding the time column to this..
    # if you do so, the sanity check below needs adjustment
    prevalence_at_tau <-
      tau_model_output[
        # choose time = min?(tau, Inf)
        2,
        # first col is time, jump over Ss and Is,
        # prevalences: source, middle, target, population
        (1 + 1 + 2 * length(st_geometry(grid))):ncol(tau_model_output),
        drop = FALSE
      ]

    # sanity checks: ensure the colnames of prevalence_at_tau
    # all contains "prevalence_"
    stopifnot(
      "all columns must contain the `prevalence_` prefix" =
        all(str_detect(names(prevalence_at_tau), pattern = "prevalence_"))
    )

    result[[glue("output_{beta_mat_name}_prevalence")]] <-
      list(prevalence_at_tau)
  }



  ## OLDER CODE BELOW HERE

  stopifnot(
    all(c("id", "geometry", "carry", "area") %in% names(grid)),
    all(dim(beta_mat) == c(length(grid$geometry), length(grid$geometry))),
    length(y_init) == 2 * length(grid$geometry),
    all(c("id_overlap", "weight") %in% names(target_overlap)),
    all(c("id_overlap", "weight") %in% names(middle_overlap))
  )
  n_grid <- nrow(grid)
  parameter_list <- list(
    beta_mat = beta_mat,
    N = n_grid,
    carry = grid$carry,
    area = grid$area,
    target_overlap = target_overlap,
    middle_overlap = middle_overlap
  )

  # common parameters
  ode_parameters <- list(
    verbose = verbose,
    y = y_init,
    parms = parameter_list,
    func = model_func,
    ynames = FALSE
  )
  tau_model_output <-
    rlang::exec(deSolve::ode,
                !!!ode_parameters,
                rootfunc = find_target_prevalence,
                hmax = hmax,
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
    model_output = model_output,
    n_grid = n_grid
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
  source_overlap <- parameters$source_overlap
  target_overlap <- parameters$target_overlap
  middle_overlap <- parameters$middle_overlap
  area <- parameters$area

  S <- y[1:N]
  I <- y[(N + 1):(2 * N)]

  # infections <- beta_mat * S * I
  infections <- (I %*% beta_mat) * S
  # infections <- ((I / area) %*% beta_mat) * S * area
  # infections <- ((I * area) %*% beta_mat) * S / area
  # doesn't converge to anything, but tau -> 0 for n->infinity
  # infections <- (I %*% beta_mat) * S / area
  dS <- -infections
  dI <- +infections

  # reporting
  carry <- parameters$carry
  prevalence_population <- sum(I)  / sum(carry) # alternative: divide by sum(S + I) instead
  # weighted arithmetic mean of prevalence
  prevalence_source <- sum((I[source_overlap$id_overlap] /
                              carry[source_overlap$id_overlap]) * source_overlap$weight)
  prevalence_middle <- sum((I[middle_overlap$id_overlap] /
                              carry[middle_overlap$id_overlap]) * middle_overlap$weight)
  prevalence_target <- sum((I[target_overlap$id_overlap] /
                              carry[target_overlap$id_overlap]) * target_overlap$weight)

  list(c(dS, dI),
       prevalence_source = prevalence_source,
       prevalence_middle = prevalence_middle,
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
  target_overlap <- parameters$target_overlap
  I <- y[(N + 1):(2 * N)]

  carry <- parameters$carry

  # prevalence_target <- mean(
  #   I[target_overlap$id_overlap] * target_overlap$weight /
  #     (carry[target_overlap$id_overlap] * target_overlap$weight)
  # )
  # mean(I / carry)
  # sum(y_i×w_i) / sum(w_i) #weighted arithmetic mean
  # prevalence_target <- sum(
  #   I[target_overlap$id_overlap] / carry[target_overlap$id_overlap]
  # )
  # APPROACH: MATT
  prevalence_target <- sum((I[target_overlap$id_overlap] /
                              carry[target_overlap$id_overlap]) * target_overlap$weight)
    # sum(target_overlap$weight)
  #
  # APPROACH:
  # prevalence_target <- sum(I[target_overlap$id_overlap] * target_overlap$weight) / sum(target_overlap$weight)

  c(prevalence_target - 0.5,
    terminate_extinction(times, y, parameters))
}

#' @description Root-function for terminating the simulation
#' when the middle-buffer has a prevalence of 50%.
#' Or terminates when there are no longer any any susceptibles,
#' or any infected, see [terminate_extinction].
find_middle_prevalence <- function(times, y, parameters) {
  N <- parameters$N

  # NOTE: changing the stopping criteria from TARGET to MIDDLE
  middle_overlap <- parameters$middle_overlap
  I <- y[(N + 1):(2 * N)]

  carry <- parameters$carry
  prevalence_target <- sum((I[middle_overlap$id_overlap] /
                              carry[middle_overlap$id_overlap]) * middle_overlap$weight)

  c(prevalence_target - 0.5,
    terminate_extinction(times, y, parameters))
}
