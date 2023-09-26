
# NOTE: Make sure to install disEpiODE before running this script

# Clean the `output` directory, if it is there.
# disEpiODE:::clear_output_dir()
# options(error = recover)
devtools::load_all()


tag <- "043" # REMEMBER TO SET THIS
world_scale <- 29
params1 <- tidyr::expand_grid(
  world_scale = world_scale,
  beta_baseline = c(0.05),
  buffer_offset_percent = 0.2,
  buffer_radius = 3.5,
  cellarea = seq_cellarea(n = 50, min_cellarea = 0.45, max_cellarea = min(10, world_scale)),
  # celltype = c("square", "hexagon", "hexagon_rot", "triangle"),
  celltype = c("square", "hexagon", "triangle"),
  # offset = "corner",
  # offset = c("corner", "middle", "bottom", "left"), #TODO
  hmax = c(NA, 0.3, 0.3 / 2, 0.3 / 2 / 2),
) %>%
  # dplyr::sample_n(size = dplyr::n()) %>%
  identity()

pmap(params1, \(world_scale, beta_baseline, buffer_offset_percent, buffer_radius, cellarea, celltype, hmax) {

  source_target <-
    get_buffer_source_target(landscape_width = world_scale,
                             landscape_height = world_scale,
                             buffer_radius = buffer_radius,
                             buffer_offset_percent = buffer_offset_percent)
  middle_buffer <- get_middle_buffer(source_target = source_target,
                                     buffer_radius = buffer_radius)

  world <- create_landscape(scale = world_scale)
  world_landscape <- world$landscape

  all_buffers <-
    rbind(source_target, middle_buffer) %>%
    mutate(label = factor(label, c("source", "middle", "target")))
  world_area <- st_area(world_landscape)

  grid <- create_grid(landscape = world_landscape,
                      cellarea = cellarea,
                      celltype = celltype)

  grid <- grid %>% rowid_to_column("id")
  population_total <- world_area
  grid$carry <- st_area(grid$geometry)

  y_init <- c(S = grid$carry,
              I = numeric(length(grid$carry)))

  all_buffers_overlap <-
    all_buffers %>%
    rowwise() %>%
    dplyr::group_map(
      \(buffer, ...) {
        create_buffer_overlap(grid, buffer)
      }
    )
  all_buffers_overlap_map <-
    all_buffers_overlap %>%
    map(. %>% create_buffer_overlap_map()) %>%
    flatten()

  source_overlap <- all_buffers_overlap_map$source
  target_overlap <- all_buffers_overlap_map$target
  middle_overlap <- all_buffers_overlap_map$middle

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

  dist_grid <- st_distance(st_centroid(grid$geometry))
  isSymmetric(dist_grid)
  beta_mat_exp <- beta_baseline * exp(-dist_grid)

  half_normal_kernel(0) # should be 1
  # dist_grid_half_normal <- dist_grid
  # diag(dist_grid_half_normal) <- 0
  beta_mat_half_normal <- beta_baseline * half_normal_kernel(dist_grid) / half_normal_kernel(0)
  # diag(beta_mat_half_normal) <- beta_baseline
  diag(beta_mat_half_normal) %>% unique() %>% {
    stopifnot(isTRUE(all.equal(., beta_baseline)))
  }

  stopifnot(all(is.finite(beta_mat_half_normal)))
  # create_si_model(grid, beta_mat, y_init,
  #                 target_overlap, middle_overlap) ->
  #   model_output
  #
  n_grid <- nrow(grid)
  #FIXME: currently, the beta_mat_exp is not being used
  beta_mat <- beta_mat_half_normal
  parameter_list <- list(
    beta_mat = beta_mat,
    N = n_grid,
    carry = grid$carry,
    area = grid$area,
    target_overlap = target_overlap,
    middle_overlap = middle_overlap
  )

  # common parameters
  ode_parameters <- list(
    verbose = FALSE,
    y = y_init,
    parms = parameter_list,
    func = model_func,
    ynames = FALSE,
    hmax = if (is.na(hmax)) { NULL } else { hmax }
  )
  tau_model_output <-
    rlang::exec(deSolve::ode,
                !!!ode_parameters,
                rootfunc = find_target_prevalence,
                times = c(0, Inf))
  rstate <- deSolve::diagnostics(tau_model_output)$rstate
  #TODO: check if tau exists
  tau <- tau_model_output[2, 1]

  list(
    tau = tau,
    rstate = rstate
  )
}) -> tau_rstate

tau_df <- params1 %>%
  bind_cols(
    tau_rstate %>%
      enframe() %>%
      unnest_wider(value) %>%
      unnest_wider(rstate, names_sep = "_") %>%
      identity()
  )

tau_df %>%
  glimpse()

hmax_legend <- paste(Delta, " ", t[max]) %>% expression()
tau_df %>%
  mutate(hmax_label = replace_na(as.character(hmax), "auto")) %>%
  ggplot() +
  aes(cellarea, tau, group = str_c(celltype, hmax)) +
    # geom_step(aes(linetype = hmax)) +
    geom_step(aes(color = celltype)) +
  scale_x_log10_rev() +
  theme_reverse_arrow_x() +
  theme(legend.position = "bottom") +
  facet_wrap(~hmax_label, labeller = label_both) +
  labs(linetype = hmax_legend) +
  theme_blank_background()

p_rstate_base <- tau_df %>%
  ggplot() +
  aes(cellarea, group = str_c(celltype, hmax)) +
  geom_step(aes(color = factor(hmax))) +
  # scale_x_log10_rev() +
  # expand_limits(y = 0) +
  labs(color = expression(paste(Delta, " ", t[max]))) +
  # theme_reverse_arrow_x() +
  theme_blank_background()
p_rstate_base +
  aes(y = rstate_1)
p_rstate_base +
  aes(y = rstate_2)
p_rstate_base +
  aes(y = rstate_3)
p_rstate_base +
  aes(y = rstate_4)
p_rstate_base +
  aes(y = rstate_5)

