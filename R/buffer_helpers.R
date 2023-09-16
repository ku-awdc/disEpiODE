
#' Title
#'
#' @param grid
#' @param buffer
#' @export
#'
#' @return
#' @rdname buffer_helpers
#' @examples
create_buffer_overlap <- function(grid, buffer) {

  stopifnot(
    nrow(buffer) == 1,
    all(c("buffer_polygon") %in% names(buffer)),
    all(c("id", "geometry") %in% names(grid))
  )

  #' Define source- and target-buffer indices and weights
  # this assumes that `grid` has `id` in it
  buffer_intersection <- st_intersection(y = grid,
                  buffer %>%
                    `st_geometry<-`("buffer_polygon"))
  # browser()
  buffer_intersection %>%
    # st_make_valid() %>%
    # st_as_sf() %>%
    # filter out intersection POINT
    filter(st_area(.$buffer_polygon) > 0) %>%
    st_sf() %>%
    rename(id_overlap = id,
           buffer_overlap = buffer_polygon) %>%
    mutate(overlap_area = st_area(buffer_overlap),
           # normalise with corresponding grid cells' area
           weight = overlap_area / st_area(grid$geometry[id_overlap]),
           overlap_area = NULL) %>%
    identity() ->
    buffer_overlap
  structure(
    list(buffer_overlap),
    class = "buffer_overlap"
  )
}

#' Title
#'
#' @param buffer_overlap
#'
#' @return
#' @export
#'
#' @rdname buffer_helpers
#' @examples
create_buffer_overlap_map <- function(buffer_overlap) {
  stopifnot(
    inherits(buffer_overlap, "buffer_overlap"),
    length(buffer_overlap) == 1
    # buffer_overlap[[1]] %>% length() == 1
  )

  buffer_overlap[[1]] %>%
    as_tibble() %>%
    select(-carry, -buffer_area, -buffer_point, -buffer_overlap) %>%
    nest(data = -label) %>%
    deframe()
}
