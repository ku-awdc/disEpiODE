#'---
#'title: "Spatially explicit ODE Model"
#'subtitle: Diagnostics for the square-grid discretisation
#'author: Mossa Merhi Reimert
#'output: html_document
#'---
#'
#' OBJECTIVE: Add a buffer-zone between source and
#' target, and log the prevalence of this zone.
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
    buffer_radius = 3.5,
    buffer_offset_percent = 0.2,
    offset = "corner",
    n = 100
  )
}
# print(params)
#'
#' Fixed parameters for document:
world_scale   <- params$world_scale
n             <- params$n
beta_baseline <- params$beta_baseline
buffer_radius <- params$buffer_radius
buffer_offset_percent <- params$buffer_offset_percent
offset <- params$offset
#'
# browser()
#'
source_target <-
  disEpiODE:::get_buffer_source_target(landscape_width = world_scale,
                                       landscape_height = world_scale,
                                       buffer_radius = buffer_radius,
                                       buffer_offset_percent = buffer_offset_percent)
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

world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape
glimpse(world)
glimpse(source_target)
glimpse(middle_buffer)

ggplot() +
  geom_sf(data = world_landscape, linetype = "dotted", fill = NA) +
  geom_sf(data = middle_buffer_point, aes(color = "middle")) +
  geom_sf(data = source_target, aes(color = label, geometry = buffer_point)) +
  theme_blank_background() +
  NULL

all_buffers <-
  rbind(source_target, middle_buffer) %>%
  mutate(label = factor(label, c("source", "middle", "target")))

ggplot() +
  geom_sf(data = world_landscape, linetype = "dotted", fill = NA) +
  geom_sf(data = all_buffers, aes(color = label, fill = label, geometry = buffer_polygon)) +
  guides(color = "none") +
  # theme(legend.position = "top") +
  theme_blank_background() +
  NULL
#'
#'

#'
#' Assume all three buffers don't overlap

stopifnot(
  "buffers overlap, they shouldn't" =
    all(lengths(st_intersects(all_buffers, sparse = TRUE)) == 1)
)

world_area <- st_area(world_landscape)

grid <- create_grid(n, world_landscape,
                    landscape_scale = world_scale,
                    offset = offset)
grid <- grid %>% rowid_to_column("id")
population_total <- world_area
grid$carry <- st_area(grid$geometry)

y_init <- c(S = grid$carry,
            I = numeric(length(grid$carry)))
#'

# disEpiODE:::create_buffer_overlap(grid, buffer)
all_buffers %>%
  rowwise() %>%
  dplyr::group_map(
    \(buffer, ...)
    disEpiODE:::create_buffer_overlap(grid, buffer)
  ) ->
  all_buffers_overlap

# all_buffers_overlap %>% class()
# all_buffers_overlap %>% lapply(class)

# all_buffers_overlap[[1]] %>% attributes()
# all_buffers_overlap %>% flatten() %>%
#   bind_rows()

ggplot() +
  geom_sf(data = all_buffers_overlap %>%
            flatten() %>%
            bind_rows(),
          aes(color = label),
          fill = NA) +
  geom_sf(data = world_landscape, fill = NA,
          linetype = "dotted") +
  theme_blank_background()
#'
#'

all_buffers_overlap_map <-
  all_buffers_overlap %>%
  map(. %>% disEpiODE:::create_buffer_overlap_map()) %>%
  flatten()

source_overlap <- all_buffers_overlap_map$source
target_overlap <- all_buffers_overlap_map$target
middle_overlap <- all_buffers_overlap_map$middle

source_overlap
target_overlap
middle_overlap

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

dist_grid <- st_distance(st_centroid(grid$geometry))
isSymmetric(dist_grid)
beta_mat <- beta_baseline * exp(-dist_grid)
# diag(beta_mat) <- diag(beta_mat) / grid_area[1]
#'
#'
disEpiODE:::create_si_model(grid, beta_mat, y_init, target_overlap, middle_overlap) ->
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
prevalence_throughout %>% head()
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
# this should be number of columns
stopifnot("only n² cells that represent I" =
            length(I_grid) == n**2)
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

  geom_sf(data = all_buffers,
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
    glue("~/GitHub/disEpiODE/output/012_grid_at_tau_plots/",
         "beta_baseline_{beta_baseline}_n_{n}.png") %>%
    normalizePath(),
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
    col_names = !file.exists("~/GitHub/disEpiODE/output/012_summary.csv" %>%
                               normalizePath()),
    "~/GitHub/disEpiODE/output/012_summary.csv" %>% normalizePath())
#'
#'
#'
readr::write_rds(
  model_output,
  str_c(
    "~/GitHub/disEpiODE/output/012_model_output_",
    paste0(names(params), "_", params, collapse = "_"),
    ".rds"
  ) %>%
    fs::path_expand()
)
