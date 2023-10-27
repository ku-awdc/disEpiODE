
# NOTE: Make sure to install disEpiODE before running this script

# Clean the `output` directory, if it is there.
# disEpiODE:::clear_output_dir()
# options(error = recover)
# devtools::load_all()
library(disEpiODE)

library(future)
# plan(multisession(workers = 10))
# plan(multisession(workers = 4))
library(furrr)


tag <- "043" # REMEMBER TO SET THIS
world_scale <- 29
params1 <- tidyr::expand_grid(
  world_scale = world_scale,
  # beta_baseline = c(0.05),
  beta_baseline = c(0.5),
  buffer_offset_percent = 0.2,
  buffer_radius = 3.5,
  cellarea = c(
    # 0.25, # 29** 2 / 0.20 = 4205
    # seq.default(0.25, 2.5, length.out = 4) %>% head(-1),
    # seq.default(0.20, 2.5, length.out = 3) %>% rev(),
    # seq_cellarea(n = 50, min_cellarea = 0.45, max_cellarea = world_scale)
    # seq_cellarea(n = 75, min_cellarea = 0.45, max_cellarea = world_scale)
    # seq_cellarea(n = 50 + 25 + 25, min_cellarea = 2.5, max_cellarea = world_scale)
    seq_cellarea(n = 150, min_cellarea = 0.5, max_cellarea = world_scale)
  ) %>%
    # cellarea = c(
    #   # 0.25, # 29** 2 / 0.20 = 4205
    #   seq.default(0.25, 2.5, length.out = 4) %>% head(-1),
    #   # seq.default(0.20, 2.5, length.out = 3) %>% rev(),
    #   # seq_cellarea(n = 50, min_cellarea = 0.45, max_cellarea = world_scale)
    #   # seq_cellarea(n = 75, min_cellarea = 0.45, max_cellarea = world_scale)
    #   seq_cellarea(n = 50 + 25 + 25, min_cellarea = 2.5, max_cellarea = world_scale)
    # ) %>%
    zapsmall() %>%
    unique(),
  # celltype = c("square", "hexagon", "hexagon_rot", "triangle"),
  celltype = c("square", "hexagon", "triangle"),
  # offset = "corner",
  # offset = c("corner", "middle", "bottom", "left"), #TODO
  # hmax = c(NA, 0.3, 0.3 / 2, 0.3 / 2 / 2),
  # hmax = c(NA, 0.25)
  # hmax = c(0.25)
) %>%
  # dplyr::sample_n(size = dplyr::n()) %>%
  identity()

beta_mat_list <- c("inverse", "scaled_inverse", "half_normal", "exp")
# beta_mat_list <- c("half_normal")

#' This needs to be set for each new "configuration" to get accurate estimates
#' of tau.
#TODO: make into a list that errors if accessing an undefined element
hmax_list <- list(

  # inverse = 0.003,
  # scaled_inverse = 0.005,
  # half_normal = 0.01,
  # exp = 0.01

  # inverse = 0.003,
  # scaled_inverse = 0.005,
  # half_normal = 0.01,
  # exp = 0.01

  # # use `NULL` for auto
  # # inverse = 0.0370,
  # inverse = 0.0192,
  #
  # scaled_inverse = 0.05,
  #
  # # exp = 0.115,
  # exp = 0.100,
  # # half_normal = 0.144
  # half_normal = 0.100
)

# pmap(params1,.progress = TRUE,
future_pmap(params1, .progress = TRUE,
            \(world_scale, beta_baseline, buffer_offset_percent, buffer_radius,
              cellarea, celltype, hmax) {

              #TODO
              remove_within_patch_transmission <- TRUE

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
              # browser()
              # THREE APPROACH

              all_buffers_overlap_map <-
                st_intersection(y = grid, all_buffers %>%
                                  `st_geometry<-`("buffer_polygon")) %>%
                transmute(label, id_overlap = id,
                          weight = st_area(buffer_polygon) / buffer_area) %>%
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
              #
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


              # distance of the grid cells...
              all_beta_mat <- list()
              dist_grid <- st_distance(st_centroid(grid$geometry))

              n_grid <- nrow(grid)
              parameter_list <- list(
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
                func = disEpiODE:::model_func,
                ynames = FALSE
              )

              # region: exp

              # kernel(d) = exp(-d)

              beta_mat_exp <- beta_baseline * exp(-dist_grid)

              stopifnot(all(is.finite(beta_mat_exp)))
              diag(beta_mat_exp) %>% unique() %>% {
                stopifnot(isTRUE(all.equal(., beta_baseline)))
              }
              all_beta_mat$exp <- beta_mat_exp

              # endregion

              # region: half-normal

              # kernel(d) = 2×pdf(mean = 0, sd = mean_formula(1))

              # dist_grid_half_normal <- dist_grid
              # diag(dist_grid_half_normal) <- 0
              beta_mat_half_normal <- beta_baseline *
                half_normal_kernel(dist_grid) / half_normal_kernel(0)
              # beta_mat_half_normal <- beta_baseline *
              #   half_normal_param_kernel(dist_grid, 1.312475, -1.560466, 3.233037)
              # diag(beta_mat_half_normal) <- beta_baseline

              # this test fails, but
              # > half_normal_param_kernel(0, 1.312475, -1.560466, 3.233037)
              # [1] 0.9999617
              # and it should exactly 1
              #
              # diag(beta_mat_half_normal) %>% unique() %>% {
              #   stopifnot(isTRUE(all.equal(., beta_baseline)))
              # }
              stopifnot(all(is.finite(beta_mat_half_normal)))
              all_beta_mat$half_normal <- beta_mat_half_normal


              # endregion


              # region: inverse

              # VALIDATION
              # isSymmetric(dist_grid)

              # kernel(d) = 1 / (1 + d)
              beta_mat_inverse <- beta_baseline * (1/(1 + dist_grid))
              stopifnot(all(is.finite(beta_mat_inverse)))
              diag(beta_mat_inverse) %>% unique() %>% {
                stopifnot(isTRUE(all.equal(., beta_baseline)))
              }
              all_beta_mat$inverse <- beta_mat_inverse

              # region: scaled inverse
              choices <- c(0.4049956, 0.2700693);
              scaled_d <- choices[1]

              # kernel(d) = 1 / (1 + d / scaled_d)
              beta_mat_scaled_inverse <- beta_baseline * (1/(1 + dist_grid / scaled_d))
              stopifnot(all(is.finite(beta_mat_scaled_inverse)))
              diag(beta_mat_scaled_inverse) %>% unique() %>% {
                stopifnot(isTRUE(all.equal(., beta_baseline)))
              }
              all_beta_mat$scaled_inverse <- beta_mat_scaled_inverse

              # endregion



              #FIXME: all the `beta_mat`s are being calculated even if they
              # are not needed 🤷

              # ensure that `beta_mat_list`, which is input, is matched with
              # `all_beta_mat`
              stopifnot(all(names(beta_mat_list %in% names(all_beta_mat))))

              result <- list()

              for (beta_mat_name in beta_mat_list) {

                beta_mat = all_beta_mat[[beta_mat_name]]
                if (remove_within_patch_transmission) {
                  diag(beta_mat) <- 0

                }

                tau_model_output <-
                  rlang::exec(deSolve::ode,
                              !!!ode_parameters,
                              hmax = na_as_null(hmax_list[[beta_mat_name]]),
                              parms = parameter_list %>% append(list(
                                beta_mat = beta_mat
                              )),
                              rootfunc = disEpiODE:::find_target_prevalence,
                              times = c(0, Inf))
                output <- list()
                output$rstate <- deSolve::diagnostics(tau_model_output)$rstate
                #TODO: check if tau exists
                output$tau <- tau_model_output[2, 1]
                result[[glue("output_{beta_mat_name}_tau")]] <- output


                prevalence_at_tau <-
                  tau_model_output[
                    # choose time = min(tau, Inf)
                    2,
                    #first col is time, jump over Ss and Is,
                    #prevalences: target, middle, population
                    (1 + 1 + 2 * length(st_geometry(grid))):ncol(tau_model_output)
                    ,drop = FALSE]

                result[[glue("output_{beta_mat_name}_prevalence")]] <-
                  list(prevalence_at_tau)
              }

              result
            }) ->
  #TODO: rename this
  tau_rstate

tau_rstate %>%
  glimpse(max.level = 2)

# pdf("plots_min_hstep.pdf",
#     height = 6,
#     width = 16 / 9 * 6)


kernel_levels <- c("inverse", "scaled_inverse", "exp", "half_normal")
#' Plot tau
#'
tau_rstate %>%
  enframe() %>%
  unnest_wider(value) %>%
  select(name, ends_with("tau")) %>%
  unnest_wider(ends_with("tau"), names_sep = "_") %>%
  select(name, ends_with("tau")) %>%

  glimpse() %>%

  bind_cols(params1) %>%

  pivot_longer(
    ends_with("tau"),
    names_to = "beta_mat",
    values_to = c("tau"),
    names_pattern = "output_(\\w+)_tau_tau"
  ) %>%
  mutate(beta_mat = factor(beta_mat, kernel_levels)) %>%

  identity() %>%
  # print(width = Inf)

  group_by(beta_mat) %>%

  group_map(\(data, group_id) {
    ggplot(data) +
      aes(cellarea, tau, group = str_c(celltype)) +
      geom_step(aes(color = interaction(celltype))) +
      labs(color = "Shape") +

      # ggplot2::sec_axis()

      scale_x_log10_rev() +
      theme_reverse_arrow_x() +
      theme_blank_background() +
      theme(text = element_text(size = 20)) +
      theme(strip.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.background = element_blank(),
            legend.text = element_text(size = 12)) +
      guides(color = guide_legend(override.aes = list(linewidth = 2))) +


      labs(caption = "Kernel form: {kernel_levels[group_id %>% pull()]}" %>% glue()) +

      NULL
  })



output_prevalence_at_tau <-
  tau_rstate %>%
  enframe("id", "output") %>%
  bind_cols(params1) %>%
  unnest_wider(output) %>%
  unnest(ends_with("prevalence"), names_sep = "_") %>%
  mutate(across(ends_with("prevalence"),
                . %>%
                  do.call(rbind, .) %>%
                  as_tibble() %>%
                  rename_with(~str_remove(., "prevalence_"))
  )) %>%
  unnest(ends_with("prevalence"), names_sep = "_") %>%

  # remove the tau-outputs!
  select(-ends_with("tau")) %>%

  print(width = Inf)


output_prevalence_at_tau %>%

  pivot_longer(
    contains("prevalence"),
    names_to = c("kernel", "prevalence_level"),
    names_pattern = "output_(\\w+)_prevalence_(\\w+)",
    values_to = "prevalence"
  ) %>%

  identity() %>%
  mutate(kernel = factor(kernel, kernel_levels)) %>%
  dplyr::filter(prevalence_level != "target") %>%

  group_by(kernel)  %>%
  group_map(\(data, kernel) {
    kernel_name <- kernel_levels[kernel %>% pull()]
    ggplot(data) +
      aes(cellarea, prevalence, group = str_c(kernel, celltype, prevalence_level)) +

      geom_step(aes(color = celltype)) +

      facet_wrap(~prevalence_level, scales = "free_y") +

      # lims(x = c(4, NA)) +
      # expand_limits(y = 1) +

      labs(caption = glue("Kernel {kernel_name}")) +
      theme(strip.text = element_text(size = 20),
            text = element_text(size = 20)) +

      scale_x_log10_rev() +
      # scale_x_log10_rev(limits = c(4, NA)) +
      theme_reverse_arrow_x() +
      # coord_cartesian(ylim = c(NA, 0.52)) +
      theme_blank_background()
  })

tau_hfirst_df <- tau_rstate %>%
  enframe() %>%
  unnest_wider(value) %>%
  select(name, ends_with("tau")) %>%
  unnest_wider(ends_with("tau"), names_sep = "_") %>%
  select(name, ends_with("rstate")) %>%
  mutate(across(ends_with("rstate"),
                . %>% map_dbl(. %>% `[`(1)))) %>%

  bind_cols(params1) %>%

  pivot_longer(
    ends_with("rstate"),
    names_to = "beta_mat",
    values_to = c("hlast"),
    names_pattern = "output_(\\w+)_tau_rstate"
  ) %>%

  identity()

tau_hfirst_df %>%
  mutate(beta_mat = factor(beta_mat, kernel_levels)) %>%

  ggplot() +
  aes(cellarea, hlast, group = str_c(celltype, beta_mat)) +

  geom_step(aes(color = celltype)) +

  facet_wrap(~beta_mat, scales = "free") +

  theme_blank_background()


#' One could replace the values of `hmax_list` with these;
#' They make sense if the previous runs was based on `hmax = NULL`, i.e. automatic
#' stepsize estimation.
#'
#'
tau_hfirst_df %>%
  summarise(observed = min(hlast), .by = beta_mat) %>%
  mutate(config = beta_mat %>%
           map_chr(. %>% `[[`(hmax_list, .) %>% null_as_na() %>% as.character()),
         config = replace_na(config, "auto")) %>%
  identity()


# dev.off()

#' # NEEDS TO BE ADJUSTED
#' #' p_rstate_base <- tau_df %>%
#' #'   ggplot() +
#' #'   aes(cellarea, group = str_c(celltype, hmax)) +
#' #'   geom_step(aes(color = factor(hmax))) +
#' #'   # scale_x_log10_rev() +
#' #'   # expand_limits(y = 0) +
#' #'   labs(color = expression(paste(Delta, " ", t[max]))) +
#' #'   # theme_reverse_arrow_x() +
#' #'   theme_blank_background()
#' #' #'
#' #' #'
#' #' #' Auxillary plots: Are there more information in`rstate`?
#' #' p_rstate_base +
#' #'   aes(y = rstate_1)
#' #' p_rstate_base +
#' #'   aes(y = rstate_2)
#' #' p_rstate_base +
#' #'   aes(y = rstate_3)
#' #' p_rstate_base +
#' #'   aes(y = rstate_4)
#' #' p_rstate_base +
#' #'   aes(y = rstate_5)
#'
#'
#' # VALIDATION PLOT: Across the given `hmax`, how does `tau` look like.
#'
#' # hmax_legend <- paste(Delta, " ", t[max]) %>% expression()
#' #
#' # model_output_df %>%
#' #   # filter(
#' #   #   cellarea
#' #   #   celltype
#' #   #   hmax
#' #   # )
#' #   # mutate(hmax_label = replace_na(as.character(hmax), "auto")) %>%
#' #   group_by(beta_mat) %>%
#' #   group_map(\(data, group_id) {
#' #     ggplot(data) +
#' #       aes(cellarea, tau, group = str_c(celltype)) +
#' #       # geom_step(aes(linetype = hmax)) +
#' #       geom_step(aes(color = celltype)) +
#' #       scale_x_log10_rev() +
#' #       theme_reverse_arrow_x() +
#' #       theme(legend.position = "bottom") +
#' #       # facet_wrap(~hmax_label, labeller = label_both) +
#' #       # labs(linetype = hmax_legend,
#' #       #      caption = glue("beta_mat: {group_id}")) +
#' #       theme_blank_background()
#' #   }
#' #   )
