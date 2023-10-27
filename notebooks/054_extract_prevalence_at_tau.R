
# NOTE: Make sure to install disEpiODE before running this script

# Clean the `output` directory, if it is there.
# disEpiODE:::clear_output_dir()
# options(error = recover)
# devtools::load_all()
library(disEpiODE)

library(future)
# plan(multisession(workers = 4))
# plan(multisession(workers = 4))
library(furrr)


tag <- "043" # REMEMBER TO SET THIS
world_scale <- 29
params1 <- tidyr::expand_grid(
  world_scale = world_scale,
  beta_baseline = c(0.05),
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

beta_mat_list <- c("inverse", "half_normal", "exp")
# beta_mat_list <- c("half_normal")

#' This needs to be set for each new "configuration" to get accurate estimates
#' of tau.
#TODO: make into a list that errors if accessing an undefined element
hmax_list <- list(
  # use `NULL` for auto
  # inverse = 0.0370,
  inverse = 0.0192,
  # exp = 0.115,
  exp = 0.100,
  # half_normal = 0.144
  half_normal = 0.100
)

# pmap(params1,.progress = TRUE,
future_pmap(params1, .progress = TRUE,
            \(world_scale, beta_baseline, buffer_offset_percent, buffer_radius,
              cellarea, celltype, hmax) {

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
              #
              # THREE APPROACH
              all_buffers_overlap_map <- st_intersection(y = grid, all_buffers %>% `st_geometry<-`("buffer_polygon")) %>%
                transmute(label, id_overlap = id, weight = st_area(buffer_polygon) / area) %>%
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

              #FIXME: all the `beta_mat`s are being calculated even if they
              # are not needed 🤷

              # endregion
              result <- list()

              for (beta_mat_name in beta_mat_list) {
                tau_model_output <-
                  rlang::exec(deSolve::ode,
                              !!!ode_parameters,
                              hmax = na_as_null(hmax_list[[beta_mat_name]]),
                              parms = parameter_list %>% append(list(
                                beta_mat = all_beta_mat[[beta_mat_name]]
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
  mutate(kernel = factor(kernel, c("inverse", "exp", "half_normal"))) %>%
  dplyr::filter(prevalence_level != "target") %>%

  group_by(kernel)  %>%
  group_map(\(data, kernel) {
    ggplot(data) +
      aes(cellarea, prevalence, group = str_c(kernel, celltype, prevalence_level)) +

      geom_step(aes(color = celltype)) +

      facet_wrap(~prevalence_level, scales = "free_y") +
      expand_limits(y = 1) +

      labs(caption = glue("Kernel {kernel}")) +

      scale_x_log10_rev() +
      theme_reverse_arrow_x() +
      theme_blank_background()

  })
#'
#' odel_output_df <-
#'   params1 %>%
#'   bind_cols(
#'     tau_rstate %>% enframe("id", "output")
#'   ) %>%
#'   unnest_longer(output) %>%
#'   mutate(beta_mat = output_id %>%
#'            str_remove("output_")) %>%
#'   unnest_wider(output) %>%
#'   #' just pick out the first `rstate`
#'   mutate(hlast = rstate %>% map_dbl(`[`(1))) %>%
#'   unnest_wider(rstate, names_sep = "_") %>%
#'   # mutate(`hWhat?` = rstate %>% map_dbl(`[`(2))) %>%
#'   # glimpse() %>%
#'   # print(width = Inf) %>%
#'   identity()
#'
#'
#' model_output_df %>%
#'   glimpse() %>%
#'
#'   group_by(beta_mat) %>%
#'
#'   group_map(\(data, group_id) {
#'     ggplot(data) +
#'       aes(cellarea, tau, group = str_c(celltype)) +
#'       geom_step(aes(color = interaction(celltype))) +
#'       labs(color = "Shape") +
#'
#'       # ggplot2::sec_axis()
#'
#'       scale_x_log10_rev() +
#'       theme_reverse_arrow_x() +
#'       theme_blank_background() +
#'       theme(text = element_text(size = 20)) +
#'       theme(strip.text = element_text(size = 15),
#'             legend.title = element_text(size = 15),
#'             legend.background = element_blank(),
#'             legend.text = element_text(size = 12)) +
#'       guides(color = guide_legend(override.aes = list(linewidth = 2))) +
#'
#'
#'       labs(caption = "Kernel form: {group_id}" %>% glue()) +
#'
#'       NULL
#'   })
#'
#'
#'
#'
#' #' One could replace the values of `hmax_list` with these;
#' #' They make sense if the previous runs was based on `hmax = NULL`, i.e. automatic
#' #' stepsize estimation.
#' #'
#' model_output_df %>%
#'   summarise(observed = min(hlast), .by = beta_mat) %>%
#'   mutate(config = beta_mat %>%
#'            map_chr(. %>% `[[`(hmax_list, .) %>% null_as_na() %>% as.character()),
#'          config = replace_na(config, "auto")) %>%
#'   identity()
#'
#' model_output_df %>%
#'   glimpse()
#'
#'
#' # model_output_df %>%
#' #   # mutate(hmax_label = replace_na(as.character(hmax), "auto")) %>%
#' #   glimpse() %>%
#' #   group_by(beta_mat) %>%
#' #   group_map(\(data, group_id) {
#' #     ggplot(data) +
#' #       aes(cellarea, group = str_c(celltype)) +
#' #       aes(y=rstate_1) +
#' #       geom_step(aes(color = output_id)) +
#' #       # scale_x_log10_rev() +
#' #       # expand_limits(y = 0) +
#' #       labs(linetype = hmax_legend,
#' #            caption = glue("beta_mat: {group_id}")) +
#' #       # theme_reverse_arrow_x() +
#' #       theme_blank_background()
#' #   })
#'
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
