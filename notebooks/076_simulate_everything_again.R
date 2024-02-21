
# NOTE: Make sure to install `disEpiODE` before running this script

# Clean the `output` directory, if it is there.
# disEpiODE:::clear_output_dir()
# options(error = recover)
# devtools::load_all()
library(disEpiODE)

library(future)
# plan(multisession(workers = 10))
# future::plan(future::multisession(workers = 4))
library(furrr)

tag <- "076" # REMEMBER TO SET THIS
post_tag <- "final_draft" # REMEMBER TO SET THIS
fs::dir_create(glue("output/{post_tag}"))


world_scale <- 1
remove_within_patch_transmission <- FALSE
generate_animation_pdf <- FALSE
#TODO:
compute_trajectories_to_tau <- FALSE
short_range_kernels <- FALSE

params1 <- tidyr::expand_grid(
  world_scale = world_scale,
  beta_baseline = c(0.005),
  buffer_offset_percent = 0.2,
  buffer_radius = 0.15,
  #TODO: make sure to calculate `cellarea` in the below plot, and
  # provide the same plots but as a function of `n`, but then you cannot
  # compare between `square` and `triangle`, as same choice of `n` leads to different
  # resolution in terms of area, and point (centroid) density.
  cellarea = c(NA, seq_cellarea(n = 150, min_cellarea = 1 / 2000, max_cellarea = 1)),
  # n_cells = NA,
  n_cells = c(NA, seq.default(from = 1, to = floor(sqrt(2000)), by = 5)),
  celltype = c("triangle", "square", "hexagon"),
  middle = c(FALSE)
  # celltype = c("square"),
  # offset = "corner",
  # offset = c("corner", "middle", "bottom", "left"), #TODO
  # hmax = c(NA, 0.3, 0.3 / 2, 0.3 / 2 / 2),
  # hmax = c(NA, 0.25)
  # hmax = c(0.25)
) %>%
  #TODO / FIXME: Only keep rows with either `n_cells` or `cellarea`, not both!
  dplyr::filter(
    # either perfect tessellation or fractured border
    xor(is.na(n_cells), is.na(cellarea)),
    # what if `n_cells` and `cellarea` were list-cols of size `2` pr. element
    # xor(map_lgl(n_cells, \(x) all(is.na(x))), map_lgl(cellarea, \(x) all(is.na(x)))),

    # middle doesn't make sense for perfect tessellation,
    !middle | (middle & !is.na(n_cells)),

    # PRESENTLY: `middle` isn't implemented for `hexagon` or `hexagon_rot`,
    !middle | (middle & !str_detect(celltype, "hexagon|hexagon_rot"))
  ) %>%
  mutate(mode = case_when(!is.na(n_cells) ~ "perfect",
                          !is.na(cellarea) & !middle ~ "axis aligned",
                          !is.na(cellarea) & middle ~ "middle aligned")) %>%
  # random order for error discovery
  # dplyr::sample_n(size = dplyr::n()) %>%
  identity()
#'
#'
#' Each configuration can be identified with:
#' `c(n_cells, cellarea, celltype, middle)`, see below
params1 %>%
  count(
    n_cells, cellarea, celltype, middle
    # mode, celltype, middle # doesn't work
  ) %>%
  print() %>% {
    summarise(., all(n == 1)) %>% print()
  }
#'
#'

#' Defined 1-parameter kernels

inv_sigma <- function(distance, sigma = 1) {
  1 / (sigma * distance + 1)
}
exp_sigma <- function(distance, sigma = 1) {
  exp(-sigma * distance)
}
half_normal_sigma <- function(distance, sigma = 1) {
  exp(-distance ** 2 / (2 * sigma ** 2))
}
#' These are calibrations found in 071
if (short_range_kernels) {
  #' Short-range kernel calibrations
  sigma_inv <- 100
  sigma_exp <- 12.6
  sigma_half_normal <- 0.085
} else {
  #' Long range kernel calibrations
  sigma_inv <- 30
  sigma_exp <- 7.25
  sigma_half_normal <- 0.135
}

stopifnot(
  inv_sigma(0) == 1,
  exp_sigma(0) == 1,
  half_normal_sigma(0) == 1,
  inv_sigma(0, sigma = sigma_inv) == 1,
  exp_sigma(0, sigma = sigma_exp) == 1,
  half_normal_sigma(0, sigma = sigma_half_normal) == 1
)


# beta_mat_list <- c("inverse", "scaled_inverse", "half_normal", "exp")
beta_mat_list <- c("inverse", "exp", "half_normal")
# beta_mat_list <- c("half_normal")

#' This needs to be set for each new "configuration" to get accurate estimates
#' of tau.
#TODO: make into a list that errors if accessing an undefined element
hmax_list <- list(
  # see output for eventual variables to set here
)
kernel_levels <- c("inverse", "exp", "half_normal")
celltype_levels <- c("triangle", "square", "hexagon")
# pmap(params1,.progress = TRUE,
future_pmap(params1, .progress = TRUE,
            \(world_scale, beta_baseline, buffer_offset_percent, buffer_radius,
              cellarea, n_cells, celltype, middle, hmax, mode) {
              #FIXME: `mode` is unused, and needs to be removed "later"

              #TODO: codify this somehow ??
              # do we expect the grid to be a perfect tessellation? i.e.
              # equal sized cells?
              perfect_tessellation <- all(is.na(cellarea)) &&
                (!all(is.na(n_cells))) &&
                (!celltype %in% c("hexagon", "hexagon_rot"))

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
                mutate(label = factor(label, c("source", "middle", "target"),
                                      labels = c("source", "target", "secondary")))
              world_area <- st_area(world_landscape)
              # browser()
              grid <- create_grid(landscape = world_landscape,
                                  cellarea = na_as_null(cellarea),
                                  n = na_as_null(n_cells),
                                  middle = middle,
                                  celltype = celltype)
              #TODO: calculate n if cellarea is provided, and vice versa

              # NOT USEFUL ALWAYS
              if (perfect_tessellation) {
                stopifnot(
                  "grid is not perfect tessellation" =
                    grid$area %>% zapsmall() %>% unique() %>% length() %>% {
                      . == 1
                    }
                )
              }

              grid <- grid %>% rowid_to_column("id")
              # population_total <- world_area
              grid$carry <- st_area(grid$geometry)

              y_init <- c(S = grid$carry,
                          I = numeric(length(grid$carry)))
              # THREE APPROACH

              all_buffers_overlap_map <-
                st_intersection(y = grid, all_buffers %>%
                                  `st_geometry<-`("buffer_polygon")) %>%
                transmute(label, id_overlap = id,
                          #after intersection, the `buffer_polygon` is a
                          # `cell_polygon` that overlaps with `buffer_polygon`
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

              #' remove mass from susceptible
              # browser()
              # y_init[source_overlap$id_overlap] # S
              # y_init[nrow(grid) + source_overlap$id_overlap] # I
              source_zone_area <-
                all_buffers %>%
                dplyr::filter(label == "source") %>%
                pull(buffer_area)

              carry_density <- sum(grid$carry) / world_scale**2
              infection_mass <- source_zone_area * carry_density
              infection_mass <- 0.5 * infection_mass

              y_init[nrow(grid) + source_overlap$id_overlap] <-
                infection_mass * source_overlap$weight
              y_init[source_overlap$id_overlap] <-
                y_init[source_overlap$id_overlap] -
                y_init[nrow(grid) + source_overlap$id_overlap]

              stopifnot("seeding mass of susceptible & infected must total carrying capacity" =
                          isTRUE(all.equal(sum(y_init), sum(grid$carry)))
              )

              # VALIDATION
              # names(y_init)[nrow(grid) + source_overlap$id_overlap]

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

              # region: inverse
              # kernel(d) = 1 / (1 + sigma×d)

              # VALIDATION
              # isSymmetric(dist_grid)

              # kernel(d) = 1 / (1 + d)
              beta_mat_inverse <- beta_baseline * inv_sigma(dist_grid, sigma = sigma_inv)
              stopifnot(all(is.finite(beta_mat_inverse)))
              diag(beta_mat_inverse) %>% unique() %>% {
                stopifnot(isTRUE(all.equal(., beta_baseline)))
              }
              all_beta_mat$inverse <- beta_mat_inverse

              # endregion

              # region: exp

              # kernel(d) = exp(-sigma×d)

              beta_mat_exp <- beta_baseline * exp_sigma(dist_grid, sigma = sigma_exp)

              stopifnot(all(is.finite(beta_mat_exp)))
              diag(beta_mat_exp) %>% unique() %>% {
                stopifnot(isTRUE(all.equal(., beta_baseline)))
              }
              all_beta_mat$exp <- beta_mat_exp

              # endregion

              # region: half-normal

              # kernel(d) = exp(-distance ** 2 / (2 * sigma ** 2))

              beta_mat_half_normal <- beta_baseline *
                half_normal_sigma(dist_grid, sigma = sigma_half_normal)

              diag(beta_mat_half_normal) %>% unique() %>% {
                stopifnot(isTRUE(all.equal(., beta_baseline)))
              }
              stopifnot(all(is.finite(beta_mat_half_normal)))
              all_beta_mat$half_normal <- beta_mat_half_normal

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
                              # rootfunc = disEpiODE:::find_target_prevalence,
                              rootfunc = disEpiODE:::find_middle_prevalence,
                              times = c(0, Inf))
                output <- list()
                output$rstate <- deSolve::diagnostics(tau_model_output)$rstate
                #TODO: check if tau exists
                output$tau <- tau_model_output[2, 1]
                result[[glue("output_{beta_mat_name}_tau")]] <- output
                result[[glue("output_{beta_mat_name}_tau_state")]] <- tau_model_output

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
              structure(
                list(
                  grid = grid,
                  output = result
                )
              )
            }) ->
  #TODO: rename this
  tau_rstate

tau_rstate %>%
  glimpse(max.level = 2)
tau_rstate %>% length()
tau_rstate %>% lengths()
tau_rstate %>% names()
tau_rstate[] %>% names()
tau_rstate[[1]] %>% names()

state_at_tau <-
  tau_rstate %>%
  enframe() %>%
  unnest_wider(value, names_sep = "_") %>%
  select(name, grid = value_grid, ends_with("value_output")) %>%
  unnest_wider(value_output, names_sep = "_") %>%
  select(name, grid, ends_with("tau_state")) %>%
  # glimpse()

  bind_cols(params1) %>%

  # because of perfect tessellation, we can extract the actual cellarea
  mutate(cellarea = if_else(is.na(cellarea),
                            # map_dbl(grid, . %>% st_area() %>% unique())), # numeric
                            map_dbl(grid, . %>% st_area() %>% max()),
                            cellarea),
         n_cells_set = n_cells,
         n_cells = map_int(grid, nrow)
  ) %>%

  pivot_longer(
    ends_with("tau_state"),
    names_to = "beta_mat",
    values_to = c("tau"),
    names_pattern = "value_output_output_(\\w+)_tau_state"
  ) %>%
  mutate(celltype = factor(celltype, celltype_levels)) %>%
  mutate(beta_mat = factor(beta_mat, kernel_levels)) %>%

  print(width = Inf) %>%
  identity()


# state_at_tau$tau[[1]] %>% colnames() %>% {
#   .[,(1 + ):length(.)]
# }
# state_at_tau$tau %>% tail(1) %>%
#   `[[`(1) %>%
#   `[`(1, 1 + 32 + (1:32))
seed_infection_df <- state_at_tau %>%
  # hack to get number of cells `n_cells`
  # mutate(n_cells = map_int(tau, . %>% colnames() %>%
  #                            str_remove_all(pattern = "\\D+") %>%
  #                            as.numeric() %>% max(na.rm = TRUE)),
  #        #alternative to `n_cells`
  mutate(
    seed_infection_mass =
      map2_dbl(tau, n_cells,
               \(tau, n_cells) sum(tau[1, 1 + n_cells + (1:n_cells)]))
  ) %>%
  identity()


#' it is expected that this has one value...
seed_infection_df %>%
  distinct(zapsmall(seed_infection_mass)) %>% {
    stopifnot("seed infection mass must be the same across grids" = nrow(.) == 1)
  }

seed_infection_df <- seed_infection_df %>%
  mutate(
    I_at_tau = map2(tau, n_cells, \(tau, n_cells) {
      tau[2, 1 + n_cells + (1:n_cells)]
    }),
    S_at_tau = map2(tau, n_cells, \(tau, n_cells) {
      tau[2, 1 + (1:n_cells)]
    }),
    # VALIDATION
    # flag = I_at_tau %>% map_lgl(\(I) all(str_detect(names(I), "I"))) %>% all() %>% stopifnot(),
    # flag = NULL,
    grid =
      map2(grid, I_at_tau, \(grid, I) bind_cols(grid, I = I)) %>%
      map2(S_at_tau, \(grid, S) bind_cols(grid, S = S))
  )

if (generate_animation_pdf) {
  seed_infection_df %>%
    rowid_to_column("rowid") %>%
    arrange(beta_mat, celltype, rev(cellarea)) %>%
    group_by(beta_mat, celltype) %>%
    # rowwise() %>%
    group_map(\(data, config_id) {
      # browser()
      celltype <- config_id$celltype
      beta_mat <- config_id$beta_mat
      pdf(glue("output/{post_tag}/{celltype}_{beta_mat}.pdf"))
      data %>%
        rowwise() %>%
        group_map(\(rowid_data, rowid_id){
          p <- ggplot() +
            # ggplot() +
            geom_sf(data = rowid_data$grid[[1]],
                    aes(fill = I / (S + I)), linetype = "dotted") +

            # scale_fill_viridis_c(option = "inferno") +
            scale_fill_viridis_c(option = "magma", limits = c(0, 1)) +
            # theme(legend.position = "none") +

            theme_grid_plot() +
            theme_blank_background() +
            theme(text = element_text(size = 20)) +
            theme(strip.text = element_text(size = 15),
                  legend.title = element_text(size = 15),
                  legend.background = element_blank(),
                  legend.text = element_text(size = 12)) +
            guides(color = guide_legend(override.aes = list(linewidth = 2))) +
            coord_sf(expand = FALSE) +
            NULL
          print(p)
        })
      dev.off()
    })
  #
  # BASH:
  #
  # for pdf_file in *.pdf; do magick -density 150 "$pdf_file" -coalesce "${pdf_file%.pdf}.gif" & done; wait; echo "All conversions completed."

}

pdf("plots_with_{post_tag}.pdf" %>%
      glue(),
    height = 2*6,
    width = 16 / 9 * (2*6))


# Matrix::image(Matrix::Matrix(.))
# TODO: add the zones to the above plots!
#'   geom_sf(data = all_buffers,
#'           fill = NA,
#'           # aes(geometry = st_buffer(buffer_polygon, dist = 0.1)),
#'           aes(geometry = buffer_polygon),
#'
#'           linewidth = 2,
#'           alpha = 0.5,
#'           color = "white"
#'   ) +
#'
#'   geom_sf(data = all_buffers,
#'           fill = NA,
#'           aes(color = label, geometry = buffer_polygon),
#'           linewidth = 1
#'   ) +
#'
#' Plot tau
#'
tau_rstate %>%
  enframe() %>%
  unnest_wider(value) %>%
  unnest_wider(output) %>%
  select(name, grid, ends_with("tau")) %>%
  unnest_wider(ends_with("tau"), names_sep = "_") %>%
  select(name, grid, ends_with("tau")) %>%

  # glimpse() %>%

  bind_cols(params1) %>%

  pivot_longer(
    ends_with("tau"),
    names_to = "beta_mat",
    values_to = c("tau"),
    names_pattern = "output_(\\w+)_tau_tau"
  ) %>%
  mutate(beta_mat = factor(beta_mat, kernel_levels),
         celltype = factor(celltype, celltype_levels),
         #TODO: reenable this?
         # cellarea = if_else(
         #   is.na(cellarea),
         #   # map_dbl(grid, . %>% st_area() %>% unique())), # numeric
         #   map_dbl(grid, . %>% st_area() %>% max()),
         #   cellarea),
         n_cells_set = n_cells,
         n_cells = map_int(grid, nrow)
  ) %>%

  identity() -> tau_plot_data

tau_plot_data %>%
  group_by(beta_mat) %>%

  group_map(\(data, group_id) {
    beta_mat <- group_id$beta_mat %>% as.character()
    data %>%
      ggplot() +
      aes(group = celltype) +
      aes(cellarea, tau) +
      geom_step(aes(color = celltype)) +

      # ggplot2::sec_axis()

      # scale_x_log10_rev(limits = c(30, NA)) +
      # lims(x = c(NA, 10)) +
      labs(color = "Shape") +
      coord_cartesian(xlim = c(30, NA)) +
      scale_x_log10_rev() +
      theme_reverse_arrow_x() +
      theme_blank_background() +
      theme(text = element_text(size = 20)) +
      theme(strip.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.background = element_blank(),
            legend.text = element_text(size = 12)) +
      guides(color = guide_legend(override.aes = list(linewidth = 2))) +

      labs(caption = glue("Kernel form: {beta_mat}")) +

      NULL
  })


tau_plot_data %>%
  group_by(celltype) %>%

  group_map(\(data, group_id) {
    celltype <- group_id$celltype %>% as.character()
    data %>%
      ggplot() +
      aes(group = beta_mat) +
      aes(cellarea, tau) +
      geom_step(aes(color = beta_mat)) +
      labs(color = "Kernel") +
      coord_cartesian(xlim = c(NA, 30)) +
      theme_blank_background() +
      theme(text = element_text(size = 20)) +
      theme(strip.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.background = element_blank(),
            legend.text = element_text(size = 12)) +
      guides(color = guide_legend(override.aes = list(linewidth = 2))) +

      labs(caption = glue("Shape: {celltype}")) +

      NULL
  })

output_prevalence_at_tau <-
  tau_rstate %>%
  enframe("id", "output") %>%
  unnest_wider(output) %>%
  unnest_wider(output) %>%
  bind_cols(params1) %>%

  mutate(
    #TODO: re-enable this?
    # cellarea = if_else(
    #   is.na(cellarea),
    #   # map_dbl(grid, . %>% st_area() %>% unique())), # numeric
    #   map_dbl(grid, . %>% st_area() %>% zapsmall() %>% unique()),
    #   cellarea),
    n_cells_set = n_cells,
    n_cells = map_int(grid, nrow)
  ) %>%
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

      labs(color = "Shape") +
      geom_step(aes(color = celltype)) +

      facet_wrap(~prevalence_level, scales = "free_y") +

      # lims(x = c(4, NA)) +
      # expand_limits(y = 1) +
      #
      coord_cartesian(xlim = c(30, NA)) +

      labs(caption = glue("Kernel form: {kernel_name}")) +
      theme(strip.text = element_text(size = 20),
            text = element_text(size = 20)) +

      scale_x_log10_rev() +
      # scale_x_log10_rev(limits = c(10, NA)) +
      theme_reverse_arrow_x() +
      theme_blank_background()
  })

tau_hfirst_df <- tau_rstate %>%
  enframe() %>%
  unnest_wider(value) %>%
  unnest_wider(output) %>%
  select(name, grid, ends_with("tau")) %>%
  unnest_wider(ends_with("tau"), names_sep = "_") %>%
  select(name, grid, ends_with("rstate")) %>%
  mutate(across(ends_with("rstate"),
                . %>% map_dbl(. %>% `[`(1)))) %>%

  bind_cols(params1) %>%
  mutate(
    # TODO: Re-enable this
    # cellarea = if_else(
    #   is.na(cellarea),
    #   # map_dbl(grid, . %>% st_area() %>% unique())), # numeric
    #   map_dbl(grid, . %>% st_area() %>% zapsmall() %>% unique()),
    #   cellarea),
    n_cells_set = n_cells,
    n_cells = map_int(grid, nrow)) %>%

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
  identity() %>%
  print() %>%
  # use this to set hmax
  glue_data("{beta_mat} = {prettyNum(zapsmall(observed))},")


dev.off()


beepr::beep()
