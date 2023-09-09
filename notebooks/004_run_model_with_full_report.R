#'---
#'title: "Spatially explicit ODE Model"
#'subtitle: Diagnostics for the square-grid discretisation
#'author: Mossa Merhi Reimert
#'output: html_document
#'---
#'
#+ setup, message=FALSE
library(magrittr)
library(tidyverse)
library(sf)

library(glue)
library(disEpiODE)
#'
knitr::opts_knit$set(verbose = TRUE)

#'
#' Default parameters if not being rendered
#'
if (!exists("params")) {
  params <- list(
    world_scale = 29,
    beta_baseline = 0.05,
    n = 7
  )
}
# print(params)
#'
#' Fixed parameters for document:
world_scale <-
  params$world_scale
n <-
  params$n
beta_baseline <-
  params$beta_baseline
#'
# browser()
#'
source_target <-
  disEpiODE:::get_buffer_source_target(landscape_width = world_scale,
                                       landscape_height = world_scale,
                                       buffer_radius = 3.5,
                                       buffer_offset_percent = 0.2)
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape
glimpse(world)
glimpse(source_target)

ggplot() +
  geom_sf(data = world_landscape, fill = NA) +
  geom_sf(data = source_target,
          aes(fill = label, geometry = buffer_polygon)) +
  theme_blank_background()

world_area <- st_area(world_landscape)

# n <- 15
# n <- 50
# n <- 75
# n <- 100
grid <- create_grid(n, world_landscape,
                    landscape_scale = world_scale)
grid <- grid %>% rowid_to_column("id")
population_total <- world_area
grid$carry <- st_area(grid$geometry)

y_init <- c(S = grid$carry,
            I = numeric(length(grid$carry)))
#'
#' Define source- and target-buffer indices and weights
source_target_overlap <-
  # this assumes that `grid` has `id` in it
  st_intersection(source_target %>%
                    `st_geometry<-`("buffer_polygon"),
                  grid) %>%
  rename(id_overlap = id,
         buffer_overlap = buffer_polygon) %>%
  mutate(overlap_area = st_area(buffer_overlap),
         # normalise with corresponding grid cells' area
         weight = overlap_area / st_area(grid$geometry[id_overlap]),
         overlap_area = NULL) %>%
  identity()
#'
source_target_overlap



ggplot() +
  geom_sf(data = source_target_overlap, fill = NA) +
  geom_sf(data = world_landscape, fill = NA,
          linetype = "dotted") +
  theme_blank_background()
#'
#'

# reduce the carried over information
source_target_overlap <- source_target_overlap %>%
  as_tibble() %>%
  select(-carry, -buffer_area, -buffer_point, -buffer_overlap) %>%
  identity()
# Ignore this warning

source_target_overlap_list <-
  source_target_overlap %>%
  nest(data = -label) %>%
  deframe()

source_target_overlap

source_overlap <- source_target_overlap_list$source
target_overlap <- source_target_overlap_list$target

source_overlap
target_overlap


#' Add infected animal

half_infected_mass <-
  grid$carry[source_overlap$id_overlap] *
  source_overlap$weight *
  (1/2)
#' remove mass from susceptible
y_init[source_overlap$id_overlap] <-
  y_init[source_overlap$id_overlap] - half_infected_mass
y_init[
  nrow(grid) +
    source_overlap$id_overlap
] <- +half_infected_mass

stopifnot(
  "compartments must be non-negative" =
    all(y_init >= 0),
  "equal mass to preset carrying capacity" =
    isTRUE(all.equal(sum(y_init), sum(grid$carry)))
)

dist_grid <- st_distance(grid$geometry %>% st_centroid())
isSymmetric(dist_grid)
diag(dist_grid) %>% table()
beta_mat <- beta_baseline * exp(-dist_grid)
#'
#'
disEpiODE:::create_si_model(grid, beta_mat, y_init, target_overlap) ->
  model_output
#'
#'
stopifnot(
  all(!is.na(model_output$tau_model_output)),
  all(!is.na(model_output$model_output))
)

# model_output$tau_model_output %>% str()
model_output$tau_model_output[2,1] # tau
tau <-
  model_output$tau_model_output[2,1] # tau
tau
# model_output$model_output %>% str()

# Extract population and target prevalence throughout the trajectory:
model_output$model_output[,c(1, (2*nrow(grid) + 2):ncol(model_output$model_output)),
                          drop = FALSE] ->
  prevalence_throughout
#'
#'
#'
#'
#' Population prevalence throughout
prevalence_throughout %>%
  as_tibble() %>%
  pivot_longer(starts_with("prevalence_"),
               names_to = c(NA, "prevlance_name"),
               names_sep = "_") %>%
  ggplot() +
  aes(time, value, group = prevlance_name) +
  geom_line(aes(color = prevlance_name)) +
  lims(y = c(0, 0.5)) +
  theme_blank_background() +
  theme(legend.position = "bottom") +
  labs(caption = glue("{n}"))
#'
#'
#'
I_grid <-
  model_output$tau_model_output[2, (nrow(grid) + 2):(2 * nrow(grid) + 1)] %>%
  # names()
  identity()
prevalence_grid <- (I_grid / grid$carry)

# DEBUG
# target_overlap$id_overlap
# I_grid
# I_grid[target_overlap$id_overlap]
# I_grid[target_overlap$id_overlap] %>% sum()
# grid$carry[target_overlap$id_overlap] %>% sum()

#+ figure.width=12
p_prevalence_at_Tau <-
  ggplot() +
  geom_sf(data = grid %>%
            mutate(prev = prevalence_grid),
          # filter(id %in% target_overlap$id_overlap),
          aes(fill = prev)) +

  geom_sf(data = source_target,
          fill = NA,
          aes(color = label, geometry = buffer_polygon)) +

  scale_fill_viridis_c() +
  theme(legend.position = "bottom") +
  theme_blank_background()
#'
p_prevalence_at_Tau
ggsave(
  plot = p_prevalence_at_Tau,
  filename =
    glue("~/GitHub/disEpiODE/output/004_grid_at_tau_plots/",
    "beta_baseline_{beta_baseline}_n_{n}.png"),
  units = "cm",
  width = 13,
  height = 11.5,
  scale = 1.4,
  dpi = 300
)
#'
#' ## Saving
#'
message(glue("{getwd()}"))
#'
prevalence_at_tau <-
  model_output$tau_model_output[
    2, (1 + 1 + 2 * length(st_geometry(grid))):ncol(model_output$tau_model_output)
  ,drop = FALSE]
#'
#'
tibble(tau) %>%
  bind_cols(as_tibble(params),
            as_tibble(prevalence_at_tau)) ->
  report_row

report_row %>%
  readr::write_excel_csv(
    append = TRUE,
    col_names = !file.exists("~/GitHub/disEpiODE/output/summary.csv"),
    "~/GitHub/disEpiODE/output/summary.csv")
#'
#'
#'
readr::write_rds(
  model_output,
  str_c(
    "~/GitHub/disEpiODE/output/model_output_",
    paste0(names(params), "_", params, collapse = "_"),
    ".rds"
  )
)


