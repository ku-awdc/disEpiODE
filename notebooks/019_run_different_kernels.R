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
  tag <- "000"
  params <- list(
    world_scale = 29,
    beta_baseline = 0.05,
    buffer_radius = 3.5,
    buffer_offset_percent = 0.2,
    offset = "corner",
    n = 40
  )
}
# print(params)
#'
#' Fixed parameters for document:

# make all `params` names available
list2env(params, envir = environment());

params_spec <- {
  params_min <- params
  params_min$tag <- NULL
  paste0(names(params_min), "_", params_min, collapse = "_")
}
rm(params_min)
params
params_spec
#'
#' Place the buffers in the landscape.
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

grid <- create_grid(n = n,
                    landscape = world_landscape,
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
beta_mat_exp <- beta_baseline * exp(-dist_grid)


# beta_mat_inverse <- beta_baseline / dist_grid
# diag(beta_mat_inverse) <- beta_baseline
#
# stopifnot(all(is.finite(beta_mat_inverse)))

half_normal_kernel(0) # should be 1
# dist_grid_half_normal <- dist_grid
# diag(dist_grid_half_normal) <- 0
beta_mat_half_normal <- beta_baseline * half_normal_kernel(dist_grid) / half_normal_kernel(0)
# diag(beta_mat_half_normal) <- beta_baseline
diag(beta_mat_half_normal) %>% unique()

stopifnot(all(is.finite(beta_mat_half_normal)))

# # dist_grid_sqrt <- dist_grid
# # range(dist_grid_sqrt)
# # diag(dist_grid_sqrt) <- 1
# beta_mat_sqrt <- beta_baseline / sqrt(dist_grid)
# # diag(beta_mat_sqrt) <- 1 # WRONG
# diag(beta_mat_sqrt) <- beta_baseline
# range(beta_mat_sqrt)
#
# # hist(beta_mat_sqrt)
# stopifnot(all(is.finite(beta_mat_sqrt)))



# normal_half_normal <- function(x) {
#   # see 020_plotting_distance_kernel
#   2 *
#     dnorm(x,
#           sd = disEpiODE:::half_normal_sd(mean = 0.6366252),
#           mean = 0)
# }

# normal_half_normal(0) # is almost one.

# beta_mat_norm_half_normal <- beta_baseline * normal_half_normal(dist_grid)
#
# stopifnot(all(is.finite(beta_mat_norm_half_normal)))

# diag(beta_mat) <- diag(beta_mat) / grid_area[1]
#'
#'

beta_mat_list <- list(
  beta_mat_exp = beta_mat_exp,
  beta_mat_half_normal = beta_mat_half_normal
)

imap(
  beta_mat_list, \(beta_mat, beta_mat_name) {

    #'
    disEpiODE:::create_si_model(grid, beta_mat, y_init,
                                target_overlap, middle_overlap) ->
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
                   names_to = c(NA, "prevalence_name"),
                   names_sep = "_") %>%
      ggplot() +
      aes(time, value, group = prevalence_name) +
      geom_line(aes(color = prevalence_name)) +
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
    # stopifnot("only n² cells that represent I" =
    #             length(I_grid) == n**2)
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
        glue("~/GitHub/disEpiODE/output/{tag}_grid_at_tau_plots_beta_{beta_mat_name}/",
             "prevalence_plot_{params_spec}.png") %>%
        fs::path_expand(),
      units = "cm",
      width = 13,
      height = 11.5,
      scale = 1.4,
      dpi = 300
    )
    #'
    #' ## Saving
    #'
    prevalence_at_tau <-
      model_output$tau_model_output[
        2, (1 + 1 + 2 * length(st_geometry(grid))):ncol(model_output$tau_model_output)
        ,drop = FALSE]
    #'
    #'
    tibble(tau) %>%
      bind_cols(as_tibble(params),
                beta_mat_name = beta_mat_name,
                as_tibble(prevalence_at_tau)) ->
      report_row

    # add a row while running in batch mode, use `{tag}_output_summary.csv` if
    # available
    report_row_path <- glue("~/GitHub/disEpiODE/output/{tag}_summary.csv") %>%
      fs::path_expand()
    report_row %>%
      readr::write_excel_csv(
        append = TRUE,
        col_names = !file.exists(report_row_path),
        report_row_path)
    #'
    #'
    # Save the `model_output` for this configuration
    readr::write_rds(
      model_output,
      glue(str_c(
        "~/GitHub/disEpiODE/output/{tag}_model_output_beta_{beta_mat_name}_",
        params_spec,
        ".rds"
      )) %>%
        fs::path_expand()
    )
    report_row
  }
) -> report_rows

report_row <- bind_rows(report_rows)
