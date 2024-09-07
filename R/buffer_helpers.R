
#' Title
#'
#' @param grid
#' @param buffer
#' @export
#'
#' @return
#' @rdname buffer_helpers
#' @examples
create_farm_overlap <- function(grid, farms) {

  farms %>%
    rowwise() %>%
    group_split() %>%
    lapply(\(buffer){
      grid %>%
        filter(st_intersects(geometry, buffer$buffer_polygon, sparse=FALSE)[,1L]) %>%
        mutate(geometry = st_intersection(geometry, buffer$buffer_polygon)) %>%
        mutate(area_overlap = st_area(geometry), weight = area_overlap/buffer$buffer_area) %>%
        bind_cols(
          buffer %>% select(label)
        ) %>%
        select(label, ID, area, area_overlap, weight, geometry)
    }) %>%
    bind_rows() ->
    overlap

  stopifnot(all.equal(sum(overlap$area_overlap), sum(farms$buffer_area)))

  return(overlap)


  ## OLDER CODE:
  all_buffers_overlap_map <-
    st_intersection(y = grid, all_buffers %>%
        # FIXME: change to `st_set_geometry`
        `st_geometry<-`("buffer_polygon")) %>%
    transmute(label,
      id_overlap = id,
      # after intersection, the `buffer_polygon` is a
      # `cell_polygon` that overlaps with `buffer_polygon`
      weight = st_area(buffer_polygon) / buffer_area
    ) %>%
    st_drop_geometry() %>%
    # normalize weights
    # mutate(.by = label, weight = weight / sum(weight)) %>%

    nest(data = -label) %>%
    deframe() %>%
    identity()
  # grid
  # all_buffers
  # SECOND APPROACH
  # all_buffers_overlap_map <- all_buffers %>%
  #   st_drop_geometry() %>%
  #   mutate(
  #     weight_map = st_interpolate_aw(
  #       grid %>% select(),
  #       x = buffer_polygon %>% st_sf() %>% transmute(weight = st_area(geometry)),
  #       extensive = TRUE) %>%
  #       rownames_to_column("id_overlap") %>%
  #       mutate(id_overlap = as.integer(id_overlap)) %>%
  #       st_drop_geometry() %>%
  #       list(),
  #     .by = label) %>%
  #   ungroup() %>%
  #   select(label, weight_map) %>%
  #   unnest(weight_map) %>%
  #   print(n = Inf) %>%
  #   nest(data = -label) %>%
  #   deframe() %>%
  #   # View()
  #   identity()
  #
  #
  # stop("")
  # FIRST APPROACH:
  # all_buffers_overlap <-
  #   all_buffers %>%
  #   rowwise() %>%
  #   dplyr::group_map(
  #     \(buffer, ...) {
  #       create_buffer_overlap(grid, buffer)
  #     }
  #   )
  # all_buffers_overlap_map <-
  #   all_buffers_overlap %>%
  #   map(. %>% create_buffer_overlap_map()) %>%
  #   flatten()

  #' source -> lower left
  #' middle -> center of landscape
  #' target -> upper right
  #'
  #' terminate when middle achieves 50% prevalence
  source_overlap <- all_buffers_overlap_map$source
  middle_overlap <- all_buffers_overlap_map$middle
  target_overlap <- all_buffers_overlap_map$target

  #' remove mass from susceptible
  # browser()
  # y_init[source_overlap$id_overlap] # S
  # y_init[nrow(grid) + source_overlap$id_overlap] # I
  source_zone_area <-
    all_buffers %>%
    dplyr::filter(label == "source") %>%
    pull(buffer_area)
  # browser()
  carry_density <- sum(grid$carry) / world_scale**2
  infection_mass <- source_zone_area * carry_density
  # infection_mass <- 0.5 * infection_mass
  stopifnot(
    "`seed_infection_proportion` must be between 0 and 1" =
      1 >= seed_infection_proportion && seed_infection_proportion > 0
  )
  infection_mass <- seed_infection_proportion * infection_mass

  # skip susceptibles i.e nrow(grid) then add only to id_overlap tiles.
  y_init[nrow(grid) + source_overlap$id_overlap] <-
    infection_mass * source_overlap$weight
  # seed_infection_proportion * source_overlap$weight
  # browser()
  # message(glue("{sum(source_overlap$weight)}"))
  # stopifnot(
  #   isTRUE(all.equal(
  #     sum(source_overlap$weight), 1
  #   ))
  # )
  # now the susceptibles of those same tiles needs a reduction of
  # individuals
  y_init[source_overlap$id_overlap] <-
    y_init[source_overlap$id_overlap] -
    y_init[nrow(grid) + source_overlap$id_overlap]

  stopifnot(
    "seeding mass of susceptible & infected must total carrying capacity" =
      isTRUE(all.equal(sum(y_init), sum(grid$carry)))
  )



  ### EVEN OLDER CODE:
  buffer <- farms
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


#' @rdname buffer_helpers
#' @export
create_initial_state <- function(grid, overlap, start_prev=0.5){

  overlap %>%
    as_tibble() %>%
    filter(label=="Farm A") ->
    overlap_source

  left_join(
    grid %>% as_tibble(),
    overlap_source %>% select(ID, area_overlap),
    by="ID"
  ) %>%
    replace_na(list(area_overlap=0.0)) %>%
    mutate(I=area_overlap*start_prev, S=area-I) ->
    init_grid

  stopifnot(all.equal(sum(init_grid$I), sum(overlap_source$area_overlap)*start_prev))
  stopifnot(nrow(init_grid)==nrow(grid), all.equal(sum(init_grid$S)+sum(init_grid$I), 1.0))

  init_grid %>%
    select(ID, S, I) ->
    rv

  return(rv)
}


#' @rdname buffer_helpers
#' @export
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
