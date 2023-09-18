devtools::load_all()

landscape <- create_landscape(scale = 29)

dist_kernel <- function(x) {
  half_normal_kernel(x) / half_normal_kernel(0)
}

curve(dist_kernel(x), to = 10)

grid_resolution <- 50
grid_gen <- st_sample(landscape$landscape, size = grid_resolution) %>%
  st_sf()

grid <- create_grid(landscape = landscape$landscape, cellarea = 1,
                    celltype = "triangle")
grid_centroids <- st_centroid(grid$geometry) %>% st_sf()

bind_rows(.id = "id",
          gen = grid_gen, cent = grid_centroids)
dist_threshold <- 5
gen_location_id <- grid_gen %>% st_within(grid$geometry, sparse = TRUE)
gen_location_id
gen_ids <- grid_gen %>% st_is_within_distance(grid$geometry,
                                              dist = dist_threshold) %>%
  map2(gen_location_id, ~ .x[-which(.x == .y)])
gen_ids
# gen_ids[1]
gen_ids %>% lengths()
gen_weights <- gen_ids %>%
  imap(~st_distance(grid$geometry[.y],
                    grid$geometry[.x])) %>%
  map(dist_kernel) %>%
  # flatten_dbl() %>% sum()
  identity()

fill_mat <- cbind(source = rep.int(gen_location_id %>% unlist(),
                                   gen_ids %>% lengths()),
                  target = gen_ids %>% unlist())
gen_weights[[1]]







