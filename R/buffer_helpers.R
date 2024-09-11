
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
