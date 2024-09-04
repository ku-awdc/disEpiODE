#' Title
#' @rdname create_transmission_matrix
#'
#' @param grid qn object created by \code{create_grid}
#' @param kernel a function (such as that created by \code{create_kernel}) taking a single vectorised parameter (distance) and returning a non-negative numeric vector of equal length. An input distance of zero must give an output of one.
#' @param type the kernel type: one of exponential, half-normal or inverse
#' @param sigma the scale parameter for the kernel
#'

#' @export
#' @rdname create_transmission_matrix
create_transmission_matrix <- function(grid, kernel){

  ## Check kernel:
  stopifnot(is.function(kernel), length(formalArgs(kernel))==1L)
  test <- kernel(0:2)
  stopifnot(is.numeric(test), length(test)==3L, all.equal(test[1L], 1.0), test>=0)

  ## Calculate distances based on centroids:
  distances <- grid$geometry %>% st_centroid() %>% st_distance()
  rv <- kernel(as.numeric(distances))
  dim(rv) <- dim(distances)
  return(rv)

}

#' @export
#' @rdname create_transmission_matrix
create_kernel <- function(type=c("exponential", "half-normal", "inverse"), sigma=1){

  stopifnot(length(sigma)==1L, is.numeric(sigma), !is.na(sigma))
  type <- match.arg(type)

  ## Note: sigma is captured in function environment:
  inv_sigma <- function(distance) {
    1 / (sigma * distance + 1)
  }
  exp_sigma <- function(distance) {
    exp(-sigma * distance)
  }
  half_normal_sigma <- function(distance) {
    exp(-distance**2 / (2 * sigma**2))
  }

  if(type=="exponential"){
    return(exp_sigma)
  }else if(type=="half-normal"){
    return(half_normal_sigma())
  }else if(type=="inverse"){
    return(inv_sigma)
  }else{
    stop("Unhandled type")
  }

}
