#' Title
#'
#' @param landscape an object created by \code{create_landscape}
#' @param landscape_width
#' @param landscape_height
#' @param buffer_radius
#' @param buffer_offset_percent
#'
#' @return
#' @examples
#' landscape <- create_landscape()
#' create_farm_placement(landscape,
#'                          buffer_radius = 5,
#'                          buffer_offset_percent = 0.1)
#'
#' @export
#' @rdname scenario_helpers
create_farm_placement <- function(landscape, buffer_radius = 0.15, buffer_offset_percent=0.2)
  {

  scale <- attr(landscape, "scale")
  st <- get_buffer_source_target(scale[1],scale[2], buffer_radius, buffer_offset_percent)

  bind_rows(
    st,
    get_middle_buffer(st, buffer_radius)
  ) %>%
    st_set_geometry("buffer_polygon") %>%
    mutate(label = factor(label, levels=c("source","middle","target")))

}

#' @export
#' @rdname scenario_helpers
get_buffer_source_target <-
  function(landscape_width, landscape_height,
           buffer_radius = 5, buffer_offset_percent = 0.1) {

    # buffer_offset_percent <- 0.1
    stopifnot("must be within (0,1)" =
                0 <= buffer_offset_percent &
                buffer_offset_percent <= 1,
              length(landscape_width) == 1,
              length(landscape_height) == 1,
              length(buffer_radius) == 1,
              length(buffer_offset_percent) == 1)

    source_coordinate <-
      st_point({c(landscape_width, landscape_height) * buffer_offset_percent} %>% {. + pmax(0, buffer_radius - .)})
    target_coordinate <- st_point(c(landscape_width, landscape_height)) - source_coordinate
    # # Calculate the offset
    # offset <- c(landscape_width, landscape_height) * buffer_offset_percent
    # adjusted_offset <- offset + pmax(0, buffer_radius - offset)
    #
    # # Define the source and target coordinates
    # source_coordinate <- st_point(adjusted_offset)
    # target_coordinate <- st_point(c(landscape_width, landscape_height) - adjusted_offset)

    source_target <- list(
      source = source_coordinate,
      target = target_coordinate
    ) %>%
      enframe(name = "label", value = "buffer_point") %>%
      st_as_sf() %>%
      mutate(
        # BUFFER IS A CIRCLE
        buffer_polygon = st_buffer(buffer_point, buffer_radius),
        # BUFFER IS A SQUARE
        # sf::st_polygon(list(
        #   stop("todo")
        # )),
        buffer_area = buffer_polygon %>% st_area()
      ) %>%
      identity()

    source_target
  }


#' @param source_target
#' @export
#' @rdname scenario_helpers
get_middle_buffer <- function(source_target, buffer_radius) {
  middle_buffer_point <- source_target$buffer_point %>%
    st_coordinates() %>%
    st_linestring() %>%
    st_centroid()
  middle_buffer <- st_sf(buffer_point = st_sfc(middle_buffer_point)) %>%
    mutate(label = "middle",
           buffer_polygon = st_buffer(buffer_point, buffer_radius),
           #TODO: replace with PI**2*buffer_radius
           buffer_area = st_area(buffer_polygon)
    )
  middle_buffer
}
