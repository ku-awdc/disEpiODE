
#' Title
#'
#' @param landscape_width
#' @param landscape_height
#' @param buffer_radius
#' @param buffer_offset_percent
#'
#' @return
#' @export
#'
#' @examples
#' world_scale <- 17
#' get_buffer_source_target(landscape_width = world_scale,
#'                          landscape_height = world_scale,
#'                          buffer_radius = 5,
#'                          buffer_offset_percent = 0.1)
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
