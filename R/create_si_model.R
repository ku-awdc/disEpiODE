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
create_si_model <- function(grid, beta_matrix, init, overlap, root=c("B","C","none")) {

  root <- match.arg(root)

  A_overlap <- overlap %>% filter(label=="Farm A")
  B_overlap <- overlap %>% filter(label=="Farm B")
  C_overlap <- overlap %>% filter(label=="Farm C")

  y_init <- init %>% select(S, I) %>% as.matrix() %>% as.numeric()
  beta_mat <- beta_matrix

  ode_parameters <- list(
    verbose = FALSE,
    y = y_init,
    func = model_func,
    ynames = FALSE
  )

  parameter_list <- list(
    N = nrow(grid),
    carry = grid$area,
    area = grid$area,
    source_overlap = A_overlap %>% select(id_overlap=ID, weight),
    middle_overlap = B_overlap %>% select(id_overlap=ID, weight),
    ## Note: C is called "target" even though it isn't by default
    target_overlap = C_overlap %>% select(id_overlap=ID, weight),
    beta_mat = beta_mat
  )

  fcta <- function(x) factor(x, levels=c("source","middle","target","population"), labels=c("Farm A","Farm B","Farm C", "Population"))

  if(root == "C"){
    ff <- function(prevalence=0.5, output=c("tibble","deSolve")) {
      stopifnot(is.numeric(prevalence), length(prevalence)==1, is.finite(prevalence), prevalence>0, prevalence<1)
      output <- match.arg(output)
      parameter_list$prevalence_threshold <- prevalence
      rv <- rlang::exec(deSolve::ode,
                        !!!ode_parameters,
                        hmax = NULL,
                        parms = parameter_list,
                        rootfunc = find_target_prevalence,
                        times = c(0,Inf))
      if(output=="tibble"){
        rv %>%
          as.data.frame() %>%
          as_tibble() %>%
          select(Time=time, starts_with("prevalence")) %>%
          rename_with(\(x) gsub("prevalence_","",x)) %>%
          filter(Time > 0) %>%
          pivot_longer("source":"population", names_to="Area", values_to="Prevalence") %>%
          mutate(Area = fcta(Area)) ->
          rv
      }
      rv
    }
  }else if(root == "B"){
    ff <- function(prevalence=0.5, output=c("tibble","deSolve")) {
      stopifnot(is.numeric(prevalence), length(prevalence)==1, is.finite(prevalence), prevalence>0, prevalence<1)
      output <- match.arg(output)
      parameter_list$prevalence_threshold <- prevalence
      rv <- rlang::exec(deSolve::ode,
                        !!!ode_parameters,
                        hmax = NULL,
                        parms = parameter_list,
                        rootfunc = find_middle_prevalence,
                        times = c(0,Inf))
      if(output=="tibble"){
        rv %>%
          as.data.frame() %>%
          as_tibble() %>%
          select(Time=time, starts_with("prevalence")) %>%
          rename_with(\(x) gsub("prevalence_","",x)) %>%
          filter(Time > 0) %>%
          pivot_longer("source":"population", names_to="Area", values_to="Prevalence") %>%
          mutate(Area = fcta(Area)) ->
          rv
      }
      rv
    }
  }else if(root == "none"){
    ff <- function(times, output=c("tibble","deSolve")) {
      stopifnot(is.numeric(times))
      output <- match.arg(output)
      rv <- rlang::exec(deSolve::ode,
                        !!!ode_parameters,
                        hmax = NULL,
                        parms = parameter_list,
                        times = times)
      if(output=="tibble"){
        rv %>%
          as.data.frame() %>%
          as_tibble() %>%
          select(Time=time, starts_with("prevalence")) %>%
          rename_with(\(x) gsub("prevalence_","",x)) %>%
          pivot_longer("source":"population", names_to="Area", values_to="Prevalence") %>%
          mutate(Area = fcta(Area)) ->
          rv
      }
      rv
    }
  }else{
    stop("Unmatched root")
  }

  return(ff)

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

  prevalence_threshold <- parameters$prevalence_threshold
  if(is.null(prevalence_threshold)) prevalence_threshold <- 0.5

  carry <- parameters$carry
  # could also be divided by sum(y)
  prevalence_population <- sum(I) / sum(carry)

  c(prevalence_population - prevalence_threshold,
    terminate_extinction(times, y, parameters))
}

#' @description Root-function for terminating the simulation
#' when the target-buffer has a prevalence of 50%.
find_target_prevalence <- function(times, y, parameters) {
  N <- parameters$N
  target_overlap <- parameters$target_overlap
  I <- y[(N + 1):(2 * N)]

  carry <- parameters$carry

  prevalence_threshold <- parameters$prevalence_threshold
  if(is.null(prevalence_threshold)) prevalence_threshold <- 0.5

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

  c(prevalence_target - prevalence_threshold,
    terminate_extinction(times, y, parameters))
}

#' @description Root-function for terminating the simulation
#' when the middle-buffer has a prevalence of 50%.
#' Or terminates when there are no longer any any susceptibles,
#' or any infected, see [terminate_extinction].
find_middle_prevalence <- function(times, y, parameters) {
  N <- parameters$N

  prevalence_threshold <- parameters$prevalence_threshold
  if(is.null(prevalence_threshold)) prevalence_threshold <- 0.5

  # NOTE: changing the stopping criteria from TARGET to MIDDLE
  middle_overlap <- parameters$middle_overlap
  I <- y[(N + 1):(2 * N)]

  carry <- parameters$carry
  prevalence_target <- sum((I[middle_overlap$id_overlap] /
                              carry[middle_overlap$id_overlap]) * middle_overlap$weight)

  c(prevalence_target - prevalence_threshold,
    terminate_extinction(times, y, parameters))
}
