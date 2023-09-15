

library(magrittr)
library(tidyverse)
library(sf)

devtools::load_all(".")

herds <-
  read_csv("../asf-challenge-2020/Data_all_players/initial_data/herds.csv",
           col_types =
             cols(
               population_id = col_integer(),
               X = col_double(),
               Y = col_double(),
               size = col_double(),
               production = col_character(),
               is_outdoor = col_logical(),
               is_commercial = col_logical(),
               multisite = col_integer()
             ))

herds_sf <-
  herds %>%
  st_as_sf(coords = c("X", "Y"))

ggplot() +
  geom_sf(data = herds_sf, fill = NA, shape = ".") +
  theme_blank_background()

SearchTrees::createTree(
  data = herds_sf %>% st_coordinates()
) -> herds_tree

herds_tree
herds_knn <- SearchTrees::knnLookup(herds_tree,
                                    newdat = herds_sf %>% st_coordinates(),
                                    k = min(100,herds_sf %>% nrow())
)
herds_dist <- st_distance(
  herds_sf, by_element = FALSE
)

# herds_dist[knn_herds[1,]]
# herds_dist[knn_herds[1,1], knn_herds[1,-1]]
# herds_dist[knn_herds[,1],knn_herds[,-1], drop = FALSE]
herds_knn %>%
  # class() %>%
  # apply(MARGIN = 2, \(x) sort(herds_dist[x[1], x[-1]])) ->
  # apply(MARGIN = 2, \(x) {print(x);stop()}) ->
  # apply(MARGIN = 1, \(x) {browser();sort(herds_dist[x[1], x[-1]])}) ->
  # apply(MARGIN = 1, \(x) {is.unsorted(herds_dist[x[1], x[-1]])}) ->
  apply(MARGIN = 1, \(x) {
    herds_dist[x[1], x[-1]]
  }, simplify = TRUE) ->
  herds_buffer_distances
# all(!herds_buffer_distances)
herds_buffer_distances %>% str()
herds_buffer_distances %>% divide_by(1000) %>% str()
herds_buffer_distances %>% divide_by(1000) %>%
  asplit(MARGIN = 1) %>%
  # as_tibble(.name_repair = "unique")
  enframe() %>%
  unnest_longer(value) %>%
  mutate(k = name + 1,
         # browser(),
         buffer_dist = value %>% as.numeric(),
         name = NULL, value = NULL) ->
  herds_buffer_model_df

model_herds_buffer <- glm(
  buffer_dist ~ k,
  family = poisson(),
  data = herds_buffer_model_df
)
# library(ggfortify)
# model_herds_buffer %>%
#   autoplot()
# VALIDATION
# expand_grid(k = seq.default(0, 100, by = 1)) %>%
#   bind_cols(., predict.glm(
#     model_herds_buffer,
#     newdata = .,
#     type = "response"
#   ) %>% as_tibble()) %>% {
#     ggplot(.) +
#       aes(k, value) +
#       geom_step() +
#       theme_blank_background()
#   }
model_herds_buffer %>%
  print() %>%
  summary()

ggplot(tibble(x = c(0, 100))) +
  aes(x) +
  stat_function(fun = \(k) {
    exp(2.360e+00 + 1.275e-02*k)
  }) +
  theme_blank_background()

herds_buffer_distances %>%
  set_rownames(seq_len(nrow(.)) + 1) %>%
  enframe()

herds_buffer_distances %>%
  set_rownames(seq_len(nrow(.)) + 1) %>%
  rowMeans(.) %>% {
    . / 1000
  } %>%
  enframe() %>% {
    ggplot(.) +
      aes(as.numeric(name), value) +
      geom_step() +
      theme_blank_background()
  }

# fpc::dbscan(
#   data = herds_sf %>% st_coordinates(),
#   5 * 1e3,
#   MinPts = 10) ->
#   dbscan_output
# dbscan_output$cluster %>%
#   table()
# dbscan_output$MinPts
