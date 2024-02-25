
# NOTE: Make sure to install `disEpiODE` before running this script

# Clean the `output` directory, if it is there.
# disEpiODE:::clear_output_dir()
# options(error = recover)
# devtools::load_all()
library(disEpiODE)

library(future)
future::plan(future::multisession(workers = 10))
# future::plan(future::multisession(workers = 4))
library(furrr)

tag <- "081" # REMEMBER TO SET THIS
post_tag <- "final_draft_beta_smaller" # REMEMBER TO SET THIS
fs::dir_create(glue("output/{post_tag}"))


world_scale <- 1
remove_within_patch_transmission <- FALSE
generate_animation_pdf <- FALSE
plot_in_pdf_file <- FALSE
# plot_in_pdf_file <- TRUE
#TODO:
compute_trajectories_to_tau <- FALSE
short_range_kernels <- FALSE

#' Create landscape
world <- create_landscape(scale = world_scale)
world_landscape <- world$landscape

#' Setup the matrix of scenario configurations to run.

params1 <- tidyr::expand_grid(
  world_scale = world_scale,
  beta_baseline = 0.05,
  sigma_inv = 100,
  sigma_exp = 12.6161672832572,
  sigma_half_normal = 0.0845580197025075,
  buffer_offset_percent = 0.2,
  buffer_radius = 0.15,
  #TODO: make sure to calculate `cellarea` in the below plot, and
  # provide the same plots but as a function of `n`, but then you cannot
  # compare between `square` and `triangle`, as same choice of `n` leads to different
  # resolution in terms of area, and point (centroid) density.
  cellarea = c(NA, seq_cellarea(n = 150, min_cellarea = 1 / 2000, max_cellarea = 1)),
  n_cells = NA,
  # cellarea = NA,
  # n_cells = floor(sqrt(2000)),
  # n_cells = c(NA, seq.default(from = 1, to = floor(sqrt(2000)), by = 5)),
  celltype = c("triangle", "square", "hexagon"),
  middle = c(FALSE)
  # celltype = c("square"),
  # offset = "corner",
  # offset = c("corner", "middle", "bottom", "left"), #TODO
  # hmax = c(NA, 0.3, 0.3 / 2, 0.3 / 2 / 2),
  # hmax = c(NA, 0.25)
  # hmax = c(0.25)
) %>%
  # Only keep rows with either `n_cells` or `cellarea`, not both!
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
#' if (short_range_kernels) {
#'   #' Short-range kernel calibrations
#'   sigma_inv <- 100
#'   sigma_exp <- 12.6
#'   sigma_half_normal <- 0.085
#' } else {
#'   #' Long range kernel calibrations
#'   sigma_inv <- 30
#'   sigma_exp <- 7.25
#'   sigma_half_normal <- 0.135
#' }

# stopifnot(
#   inv_sigma(0) == 1,
#   exp_sigma(0) == 1,
#   half_normal_sigma(0) == 1,
#   inv_sigma(0, sigma = sigma_inv) == 1,
#   exp_sigma(0, sigma = sigma_exp) == 1,
#   half_normal_sigma(0, sigma = sigma_half_normal) == 1
# )


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
              cellarea, n_cells, celltype, middle, hmax, mode,
              sigma_inv, sigma_exp, sigma_half_normal) {
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

              all_buffers <-
                rbind(source_target, middle_buffer) %>%
                mutate(label = fct(label, c("source", "middle", "target")))
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

              #' source -> lower left
              #' middle -> center of landscape
              #' target -> upper right
              #'
              #' terminate when middle achieves 50% prevalence
              source_overlap <- all_buffers_overlap_map$source
              middle_overlap <- all_buffers_overlap_map$middle
              target_overlap <- all_buffers_overlap_map$target

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

              # skip susceptibles i.e nrow(grid) then add only to id_overlap tiles.
              y_init[nrow(grid) + source_overlap$id_overlap] <-
                infection_mass * source_overlap$weight
              # now the susceptibles of those same tiles needs a reduction of
              # individuals
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
                source_overlap = source_overlap,
                middle_overlap = middle_overlap,
                target_overlap = target_overlap
              )
              # sanity check: none of the passed parameters can be NULL
              stopifnot(
                !is.null(n_grid),
                !is.null(grid$carry),
                !is.null(grid$area),
                !is.null(source_overlap),
                !is.null(middle_overlap),
                !is.null(target_overlap)
              )

              # common parameters
              ode_parameters <- list(
                verbose = FALSE,
                y = y_init,
                func = disEpiODE:::model_func,
                ynames = FALSE
              )

              # region: inverse
              # kernel(d) = 1 / (1 + sigma × d)

              # VALIDATION
              # isSymmetric(dist_grid)

              beta_mat_inverse <- beta_baseline * inv_sigma(dist_grid, sigma = sigma_inv)
              stopifnot(all(is.finite(beta_mat_inverse)))
              diag(beta_mat_inverse) %>% unique() %>% {
                stopifnot(isTRUE(all.equal(., beta_baseline)))
              }
              all_beta_mat$inverse <- beta_mat_inverse

              # endregion

              # region: exp

              # kernel(d) = exp(-sigma × d)

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

                # save a model you can run with a given end time T
                build_model_function <- function(ode_parameters, hmax, parameter_list, beta_mat) {
                  function(times) {
                    rlang::exec(deSolve::ode,
                                !!!ode_parameters,
                                hmax = na_as_null(hmax),
                                parms = parameter_list %>% append(list(
                                  beta_mat = beta_mat
                                )),
                                # rootfunc = disEpiODE:::find_target_prevalence,
                                # rootfunc = disEpiODE:::find_middle_prevalence,
                                times = times)
                  }
                }
                current_ode_model <- build_model_function(ode_parameters, hmax_list[[beta_mat_name]], parameter_list, beta_mat)
                #TODO: Maybe store list(run_model = current_ode_model)
                # if so, then the storage of this should mimic the other ways outlined here
                result[[glue("output_ode_model_{beta_mat_name}")]] <- list(run_model = current_ode_model)
                # result[[glue("output_ode_model")]][[beta_mat_name]] <- current_ode_model

                #TODO: consider adding the time column to this..
                # if you do so, the sanity check below needs adjustment
                prevalence_at_tau <-
                  tau_model_output[
                    # choose time = min?(tau, Inf)
                    2,
                    #first col is time, jump over Ss and Is,
                    #prevalences: source, middle, target, population
                    (1 + 1 + 2 * length(st_geometry(grid))):ncol(tau_model_output)
                    , drop = FALSE]

                # sanity checks: ensure the colnames of prevalence_at_tau
                # all contains "prevalence_"
                stopifnot(
                  "all columns must contain the `prevalence_` prefix" =
                    all(str_detect(names(prevalence_at_tau), pattern = "prevalence_"))
                )

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
#'
#'

#'
#' Cache the results
output_rds_file <- glue("output/{post_tag}/output.rds")
if (fs::file_exists(output_rds_file)) {
  message("Please, manually save the newly generated results")
  message("Or load the saved file if appropriate")
} else {
  message("Saving results to disk")
  readr::write_rds(x = tau_rstate, file = output_rds_file)
}
# Load saved file
# tau_rstate <- read_rds(output_rds_file)

tau_rstate %>%
  glimpse(max.level = 2)
tau_rstate %>% length()
tau_rstate %>% lengths()
tau_rstate %>% names()
tau_rstate[] %>% names()
tau_rstate[[1]] %>% names()

# IDEA?
# tau_rstate %>%
#   enframe() %>%
#   unnest_wider(value, names_sep = "_") %>%
#   select(name, grid = value_grid, ends_with("value_output")) %>%
#   unnest_wider(value_output) %>%
#   # select(name, grid, ends_with("tau_state")) %>%
#
#   bind_cols(params1) %>%
#   glimpse()

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
  mutate(
    cellarea_set = cellarea,
    cellarea = if_else(is.na(cellarea),
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
  mutate(celltype = fct(celltype, celltype_levels)) %>%
  mutate(beta_mat = fct(beta_mat, kernel_levels)) %>%

  print(width = Inf) %>%
  identity()


# state_at_tau$tau[[1]] %>% colnames() %>% {
#   .[,(1 + ):length(.)]
# }
# state_at_tau$tau %>% tail(1) %>%
#   `[[`(1) %>%
#   `[`(1, 1 + 32 + (1:32))
seed_infection_df <- state_at_tau %>%
  mutate(
    seed_infection_mass =
      map2_dbl(tau, n_cells,
               # first is time col, then Susceptible-cells, and finally Infected ones
               \(tau, n_cells) sum(tau[1, 1 + n_cells + (1:n_cells)]))
  ) %>%
  identity()

state_at_tau$tau[[4]][1,1 + state_at_tau$n_cells[4] + 1]
#' it is expected that this has one value...
seed_infection_df %>%
  distinct(zapsmall(seed_infection_mass)) %>% {
    stopifnot("seed infection mass must be the same across grids" = nrow(.) == 1,
              "seed infection must be greater than 0" = !isTRUE(all.equal(as.numeric(.), 0))
              )
  }

# R weirdness
# all.equal(1, 2)
# all.equal(1, 2) %>% isTRUE()
# all.equal(1, 2) %>% isFALSE()

seed_infection_df <- seed_infection_df %>%
  mutate(
    I_at_tau = map2(tau, n_cells, \(tau, n_cells) {
      # time skipped, then susceptibles are skipped, finally infected are captured
      tau[2, 1 + n_cells + (1:n_cells)]
    }),
    S_at_tau = map2(tau, n_cells, \(tau, n_cells) {
      # time skipped, then susceptibles
      tau[2, 1 + (1:n_cells)]
    }),
    # VALIDATION
    # flag = I_at_tau %>% map_lgl(\(I) all(str_detect(names(I), "I"))) %>% all() %>% stopifnot(),
    # flag = NULL,
    grid =
      # amend grid with infected
      map2(grid, I_at_tau, \(grid, I) bind_cols(grid, I = I)) %>%
      # passing grid amended with infected, to amend susceptibles
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

if (plot_in_pdf_file) {
  pdf("plots_with_{post_tag}.pdf" %>%
        glue(),
      height = 2*6,
      width = 16 / 9 * (2*6))
}

#' Plot the zones that are present
#'
#'
params1 %>%
  distinct(buffer_radius, buffer_offset_percent) %>%
  group_by(buffer_radius, buffer_offset_percent) %>%
  group_map(function(data, key) {
    # buffer_radius <- 0.15
    # buffer_offset_percent <- 0.2
    buffer_radius <- key$buffer_radius
    buffer_offset_percent <- key$buffer_offset_percent
    source_target <-
      get_buffer_source_target(landscape_width = 1,
                               landscape_height = 1,
                               buffer_radius = buffer_radius,
                               buffer_offset_percent = buffer_offset_percent)
    middle_buffer <- get_middle_buffer(source_target = source_target,
                                       buffer_radius = buffer_radius)

    all_buffers <-
      rbind(source_target, middle_buffer) %>%
      mutate(label = factor(label,
                            c("source", "middle", "target"),
                            labels = c("Farm A", "Farm B", "Farm C")))

    #' Multiple grids represented, and the zones used in the simulation
    #'
    common_area <- 1 / 42

    bind_rows(
      triangle = create_grid(world_landscape, cellarea = common_area, celltype = "triangle"),
      square = create_grid(world_landscape,   cellarea = common_area, celltype = "square"),
      hexagon = create_grid(world_landscape,  cellarea = common_area, celltype = "hexagon"),
      .id = "celltype"
    ) %>%
      mutate(celltype = fct_inorder(celltype)) %>%
      identity() %>% {
        ggplot(.) +

          geom_sf(fill = NA) +

          geom_sf(data = all_buffers,
                  aes(geometry = buffer_polygon, color = label),
                  linewidth = 1,
                  fill = NA) +

          # geom_sf_text(aes(label = "🐖",
          #                  geometry = buffer_point),
          #              size = 10,
          #              data = all_buffers) +
          facet_wrap(~celltype) +
          guides(color = guide_legend(override.aes = list(fill = NA))) +
          labs(color = NULL) +
          theme(legend.position = "bottom") %+%
          theme_grid_plot() %+%
          theme_blank_background() %+%
          NULL
      }
    #'
    #' Plot the three types of discretisations considered in this paper.
    #'

  }) %>%
  identity()

#' Plot the kernel chosen
#'

params1 %>%
  distinct(sigma_inv, sigma_exp, sigma_half_normal) %>%
  group_by(sigma_inv, sigma_exp, sigma_half_normal) %>%
  group_map(function(data, key) {
    sigma_inv <- key$sigma_inv
    sigma_exp <- key$sigma_exp
    sigma_half_normal <- key$sigma_half_normal

    tibble(distance = seq.default(0, 1, length.out = 200),
           kernel_inverse = inv_sigma(distance, sigma = sigma_inv),
           kernel_exp = exp_sigma(distance, sigma = sigma_exp),
           kernel_half_normal = half_normal_sigma(distance, sigma = sigma_half_normal),
    ) %>%
      pivot_longer(starts_with("kernel"),
                   names_to = c("kernel"),
                   names_pattern = "kernel_(.*)",
                   values_to = "weight") %>%
      mutate(kernel = fct(kernel, kernel_levels)) %>%
      ggplot() +
      aes(x = distance, y = weight, group = kernel) +
      geom_line(aes(color = kernel)) +
      theme_blank_background() +
      theme(legend.position = "bottom") +
      labs(color = NULL, y = "Distance weight") +
      # lims(y = c(0,1)) +
      NULL
  })


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
tau_plot_data <- tau_rstate %>%
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
  mutate(beta_mat = fct(beta_mat, kernel_levels),
         celltype = fct(celltype, celltype_levels),
         n_cells_set = n_cells,
         n_cells = map_int(grid, nrow)
  ) %>%

  identity()

tau_plot_data %>%
  # dplyr::filter(n_cells >= 35) %>%

  mutate(beta_mat = beta_mat %>%
           fct_recode(Inverse = "inverse", Exponential = "exp",
                      `Half-normal` = "half_normal")) %>%
  group_by(beta_mat) %>%

  group_map(\(data, group_id) {
    beta_mat <- group_id$beta_mat %>% as.character()
    data %>%
      ggplot() +
      aes(group = celltype) +
      aes(cellarea, tau) +
      geom_step(aes(color = celltype)) +

      labs(color = "Shape") +
      scale_x_log10_rev() +
      theme_reverse_arrow_x() +

      theme_blank_background() +
      theme(legend.position = "bottom") +
      theme(text = element_text(size = 20)) +
      theme(strip.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.background = element_blank(),
            legend.text = element_text(size = 12)) +
      guides(color = guide_legend(override.aes = list(linewidth = 2))) +

      labs(caption = glue("Kernel form: {beta_mat}"),
           y = expression(tau),
           x = "Set cellarea",
           color = NULL)  +

      NULL
  })


custom_beta_mat_names <- c(
  inverse = expression(1/(sigma*d + 1)),
  exp = expression(exp(-sigma*d)),
  half_normal = expression(exp(-d ** 2 / (2 * sigma ** 2)))
)
custom_beta_mat_names <- custom_beta_mat_names %>%
  as.character() %>%
  set_names(c("inverse", "exp", "half_normal"))

tau_plot_data %>%

  ggplot() +
  aes(group = celltype) +
  aes(cellarea, tau) +
  geom_step(aes(color = celltype)) +

  labs(color = "Shape") +
  scale_x_log10_rev() +
  theme_reverse_arrow_x() +

  theme_blank_background() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 20)) +
  theme(strip.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.background = element_blank(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +

  labs(y = expression(tau),
       x = "Set cellarea",
       color = NULL)  +

  facet_wrap(~beta_mat,
             labeller = labeller(beta_mat = as_labeller(
               custom_beta_mat_names, label_parsed
             ))
  ) +
  NULL


tau_plot_data %>%
  mutate(celltype = fct_relabel(celltype, str_to_title)) %>%
  #dplyr::filter(n_cells >= 35) %>%
  group_by(celltype) %>%

  group_map(\(data, group_id) {
    celltype <- group_id$celltype %>% as.character()
    data %>%
      ggplot() +
      aes(group = beta_mat) +
      aes(cellarea, tau) +
      geom_step(aes(color = beta_mat)) +
      labs(color = "Kernel") +

      scale_x_log10_rev() +
      theme_reverse_arrow_x() +

      theme_blank_background() +
      theme(text = element_text(size = 20)) +
      theme(strip.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.background = element_blank(),
            legend.text = element_text(size = 12)) +
      guides(color = guide_legend(override.aes = list(linewidth = 2))) +

      theme(legend.position = "bottom") +
      labs(caption = glue("Shape: {celltype}"),
           y = expression(tau), x = "Set cellarea",
           color = NULL) +

      NULL
  })

tau_plot_data %>%
  ggplot() +
  aes(group = beta_mat) +
  aes(cellarea, tau) +
  geom_step(aes(color = beta_mat)) +
  labs(color = "Kernel") +

  scale_x_log10_rev() +
  theme_reverse_arrow_x() +

  theme_blank_background() +
  theme(text = element_text(size = 20)) +
  theme(strip.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.background = element_blank(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +

  theme(legend.position = "bottom") +
  labs(
    y = expression(tau), x = "Set cellarea",
    color = NULL) +

  facet_grid(
    celltype ~ .,
    labeller = labeller(
      celltype = c(triangle = "Triangle",
                   square = "Square",
                   hexagon = "Hexagon")
    # celltype = c(triangle = "\u29C5", square = "\u25A1", hexagon = "\u2B21")
  )) +

  NULL


output_prevalence_at_tau <-
  tau_rstate %>%
  enframe("id", "output") %>%
  unnest_wider(output) %>%
  unnest_wider(output) %>%
  bind_cols(params1) %>%

  mutate(
    cellarea_set = cellarea,
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


output_prevalence_at_tau_plot_df <- output_prevalence_at_tau %>%
  pivot_longer(
    contains("prevalence"),
    names_to = c("kernel", "farm"),
    names_pattern = "output_(\\w+)_prevalence_(\\w+)",
    values_to = "prevalence"
  ) %>%

  identity() %>%
  mutate(kernel = fct(kernel, kernel_levels) %>%
           fct_recode(Inverse = "inverse", Exponential = "exp",
                      `Half-normal` = "half_normal")) %>%
  # dplyr::filter(prevalence_level != "target") %>%
  mutate(farm = fct(farm, c("source", "middle", "target", "population")) %>%
           fct_recode(`Farm A` = "source", `Farm B` = "middle", `Farm C` = "target", `Population` = "population")) %>%
  identity()
output_prevalence_at_tau_plot_df %>%
  group_by(kernel)  %>%
  group_map(\(data, kernel) {
    kernel_name <- kernel$kernel
    ggplot(data) +
      aes(cellarea, prevalence, group = str_c(kernel, celltype, farm)) +

      labs(color = "Shape") +
      geom_step(aes(color = celltype)) +

      facet_wrap(~farm, scales = "free_y") +
      labs(caption = glue("Kernel form: {kernel_name}"),
           color = NULL,
           y = "Prevalence",) +
      theme(strip.text = element_text(size = 20),
            text = element_text(size = 20)) +
      theme(legend.position = "bottom") +

      scale_x_log10_rev() +
      theme_reverse_arrow_x() +
      theme_blank_background()
  })


output_prevalence_at_tau_plot_df %>%

  # mutate(farm = farm %>%
  #          fct(levels = c("source", "middle", "target", "population"))) %>%

  dplyr::filter(farm != "Farm B") %>%

  ggplot() +
  aes(cellarea, prevalence, group = str_c(kernel, celltype, farm)) +

  labs(color = "Shape") +
  geom_step(aes(color = celltype)) +

  theme(strip.text = element_text(size = 20),
        text = element_text(size = 20)) +

  scale_x_log10_rev() +
  theme_reverse_arrow_x() +
  theme_blank_background() +

  labs(color = NULL, x = "Set cellarea", y = "Prevalence") +
  theme(legend.position = "bottom") +
  expand_limits(y = 0) +

  # facet_wrap(prevalence_level~kernel) +
  facet_grid(farm~kernel, scales = "free_y") +
  NULL


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
  mutate(beta_mat = fct(beta_mat, kernel_levels) %>%
           fct_recode(Inverse = "inverse", Exponential = "exp",
                      `Half-normal` = "half_normal")) %>%

  ggplot() +
  aes(cellarea, hlast, group = str_c(celltype, beta_mat)) +

  geom_step(aes(color = celltype)) +

  facet_wrap(~beta_mat, scales = "free") +
  theme(legend.position = "bottom") +
  labs(color = NULL) +
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

#' Produce simulation trajectory plot
#'

#' First calculate the maximum tau amongst them all
tau_rstate %>%
  enframe("configuration") %>%
  unnest_wider(value) %>% unnest_wider(output) %>%
  mutate(across(ends_with("_tau"), . %>% map_dbl(. %>% `[[`("tau")))) %>%
  select(configuration, ends_with("_tau")) %>%
  pivot_longer(
    ends_with("_tau"),
    names_to = c("kernel", ".value"),
    names_pattern = "output_(\\w+)_(\\w+)"
  ) %>%
  summarise(tau_max = max(tau),
            tau_10p = tau_max + tau_max / 10) ->
  tau_max
tau_max$tau_max

tau_rstate %>%
  enframe() %>%
  unnest_wider(value) %>%
  unnest_wider(output) %>%
  mutate(across(ends_with("_tau"), . %>% map_dbl(. %>% `[[`("tau")))) %>%
  bind_cols(params1 %>% select(celltype)) %>%
  summarise(
    .by = celltype,
    across(ends_with("_tau"),
           list(min = min,
                mean = mean, median = median, sd = sd,
                max = max))) %>%
  glimpse() %>%
  pivot_longer(
    starts_with("output"),
    names_to = c("kernel", "metric"),
    names_pattern = "output_(\\w+)_tau_(\\w+)") %>%
  pivot_wider(names_from = metric) %>%
  identity()
#'
ttimes <- seq.default(0, tau_max$tau_10p, length.out = 200)
#'
traj_models_data <-
  tau_rstate %>%
  enframe(name = "configuration") %>%
  unnest_wider(value) %>%
  unnest_wider(output) %>%
  mutate(across(ends_with("_tau"), . %>% map_dbl(. %>% `[[`("tau")))) %>%
  bind_cols(params1) %>%
  mutate(n_cells_set = n_cells,
         n_cells = map_dbl(grid, nrow)) %>%
  select(configuration, n_cells, ends_with("_tau"), starts_with("output_ode_model_")) %>%

  # glimpse() %>%
  identity()

# DEBUG
# n_cells_1 <- traj_models_data$n_cells[[1]]
# traj_models_data$output_ode_model_inverse[[1]][[1]](
#   seq.default(0, 400, length.out = 5))[, 1 +
#                                          2 * n_cells_1 + 1:4, drop = FALSE]

traj_models_data <- traj_models_data %>%

  #TODO: consider simply extracting the prevalences at the end of these..
  mutate(
    #TODO: replace 1:4 with 1:ncol(output_matrix)
    # and maybe call this operation, extract prevalence-matrix, [time prevalence_sourcce prevalence_...]
    inverse_traj = map2(.progress = TRUE,
                        output_ode_model_inverse,
                        n_cells, ~ .x[[1]](ttimes)[, c(1, 1 + 2 * .y + 1:4), drop = FALSE]),
    exp_traj = map2(.progress = TRUE,
                    output_ode_model_exp,
                    n_cells, ~ .x[[1]](ttimes)[, c(1, 1 + 2 * .y + 1:4), drop = FALSE]),
    half_normal_traj = map2(.progress = TRUE,
                            output_ode_model_half_normal,
                            n_cells, ~ .x[[1]](ttimes)[, c(1, 1 + 2 * .y + 1:4), drop = FALSE]),
  ) %>%
  glimpse()

# traj_models_data %>%
#   glimpse()

# DEBUG
# traj_models_data$exp_traj[[2]]

output_traj_rds_file <- glue("output/{post_tag}/output_traj.rds")
readr::write_rds(x = traj_models_data %>%
                   select(-starts_with("output_ode_model")) %>%
                   bind_cols(params1 %>% select(-n_cells)),
                 file = output_traj_rds_file)

# params1 %>%
#   glimpse()

source_seed_prevalence <-
  traj_models_data %>%
  select(-starts_with("output_ode_model")) %>%
  bind_cols(params1 %>% select(-n_cells)) %>%
  mutate(across(ends_with("_traj"), ~ map(.x, as_tibble))) %>%
  # unnest(ends_with("_traj"), names_sep = "_") %>%
  # select(ends_with("_source")) %>%

  mutate(
    across(ends_with("_traj"),
           # browser()
           . %>% map_dbl(. %>% `[`(1,2, drop = TRUE))
    )
  )

#' For diagnostic purposes, what is the prevalence at source tile.
#'
if (source_seed_prevalence %>% count(celltype) %>%  {any(.$n > 1)}) {
  source_seed_prevalence %>%
    mutate(celltype = fct(celltype, c("triangle", "square", "hexagon"))) %>%
    ggplot() +
    aes(group = str_c(celltype)) +
    stat_density(geom = "line", aes(inverse_traj, color = celltype)) +
    # geom_density(aes(exp_traj)) +
    geom_freqpoly(aes(inverse_traj,
                      color = celltype,
                      y = after_stat(density))) +
    labs(x = "Prevalence at Farm A") +
    facet_grid(rows = vars(celltype)) +
    labs(color = NULL, caption = "Across all grids, at time = 0") +
    theme(legend.position = "bottom") +
    theme_blank_background()
}

source_seed_prevalence %>%
  ggplot() +

  aes(x = cellarea, group = str_c(celltype)) +
  geom_step(aes(y = inverse_traj, color = celltype)) +

  labs(x = "Set cellarea", y = "Prevalence at Farm A",
       caption = "Prevalence at Farm A, at time = 0",
       color = NULL) +

  scale_x_log10_rev() +
  theme_reverse_arrow_x() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(fill = NA, linewidth = 1.5))) +
  theme_blank_background()


traj_models_data_ff <- traj_models_data %>%
  select(-starts_with("output_ode_model")) %>%
  bind_cols(params1 %>% select(-n_cells)) %>%
  pivot_longer(
    matches("\\w*_?(inverse|exp|half_normal)+_(\\w+)"),
    names_pattern = "\\w*_?(inverse|exp|half_normal)+_(\\w+)",
    names_to = c("kernel",".value"),
  ) %>%
  mutate(traj = traj %>% map(as_tibble)) %>%
  unnest(c(traj, tau)) %>%
  # glimpse() %>%
  pivot_longer(
    contains("prevalence_"),
    names_pattern = "prevalence_(\\w+)",
    names_to = c("Farm"),
    values_to = "prevalence"
  ) %>%

  # ASSUME: COMMON TIME SCALE
  select(-ends_with("traj_time"), ttime = time) %>%
  mutate(Farm = fct(Farm, levels = c("source", "middle", "target", "population")),
         Farm = fct_recode(
           Farm,
           `Farm A` = "source",
           `Farm B` = "middle",
           `Farm C` = "target",
           `Population` = "population"
         ))


traj_models_data_ff %>%
  group_by(kernel, celltype) %>%
  group_map(\(data, key) {
    # browser()

    kernel <- key$kernel
    celltype <- key$celltype

    ggplot(data) +

      # aes(ttime, prevalence, group = str_c(celltype, kernel, Farm)) +
      aes(ttime, prevalence, group = str_c(configuration, celltype, kernel, Farm)) +
      # aes(ttime, prevalence, group = str_c(configuration, kernel, Farm)) +

      # geom_step(aes(alpha = n_cells)) +
      geom_step(aes(alpha = 1 / cellarea)) +

      geom_rug(aes(x = tau, y = NULL), linewidth = 0.01) +

      facet_grid(vars(Farm)) +
      guides(alpha = guide_none()) +
      labs(alpha = NULL, y = "Prevalence", x = "time") +
      labs(caption = glue("Kernel: {kernel}, and cell-shape: {celltype}")) +

      theme_blank_background() +
      NULL
  })

#' Plot all instances:
ttimes <- seq.default(0, tau_max$tau_max * 2, length.out = 300)

tau_rstate %>%
  enframe(name = "configuration") %>%
  unnest_wider(value) %>%
  unnest_wider(output) %>%
  mutate(across(ends_with("_tau"), . %>% map_dbl(. %>% `[[`("tau")))) %>%
  bind_cols(params1) %>%
  mutate(n_cells_set = n_cells,
         n_cells = map_dbl(grid, nrow)) %>%
  slice_max(n_cells, n = 1, with_ties = FALSE, by = celltype) %>%
  # onlt select c(kernel, celltype) for a high quality grid, as that's the only thing
  # needed

  glimpse() %>%
  select(configuration, n_cells, celltype, ends_with("_tau"), starts_with("output_ode_model_")) %>%

  #TODO: consider simply extracting the prevalences at the end of these..
  mutate(
    inverse_traj = map2(.progress = TRUE,
                        output_ode_model_inverse,
                        n_cells, ~ .x[[1]](ttimes)[, c(1, 1 + 2 * .y + 1:4), drop = FALSE]),
    exp_traj = map2(.progress = TRUE,
                    output_ode_model_exp,
                    n_cells, ~ .x[[1]](ttimes)[, c(1, 1 + 2 * .y + 1:4), drop = FALSE]),
    half_normal_traj = map2(.progress = TRUE,
                            output_ode_model_half_normal,
                            n_cells, ~ .x[[1]](ttimes)[, c(1, 1 + 2 * .y + 1:4), drop = FALSE]),
  ) %>%

  select(-starts_with("output_ode_model")) %>%

  # add configuration params again?
  # bind_cols(params1 %>% select(-n_cells)) %>%
  # glimpse()
  pivot_longer(
    matches("\\w*_?(inverse|exp|half_normal)+_(\\w+)"),
    names_pattern = "\\w*_?(inverse|exp|half_normal)+_(\\w+)",
    names_to = c("kernel",".value"),
  ) %>%
  mutate(traj = traj %>% map(as_tibble)) %>%
  unnest(c(traj, tau)) %>%
  # glimpse() %>%
  pivot_longer(
    contains("prevalence_"),
    names_pattern = "prevalence_(\\w+)",
    names_to = c("Farm"),
    values_to = "prevalence"
  ) %>%

  # ASSUME: COMMON TIME SCALE
  select(-ends_with("traj_time"), ttime = time) %>%
  mutate(Farm = fct(Farm, levels = c("source", "middle", "target", "population")),
         Farm = fct_recode(
           Farm,
           `Farm A` = "source",
           `Farm B` = "middle",
           `Farm C` = "target",
           `Population` = "population"
         )) %>%

  group_by(kernel, celltype) %>%
  group_map(\(data, key) {
    kernel <- key$kernel
    celltype <- key$celltype

    ggplot(data) +
      aes(ttime, prevalence, group = str_c(configuration, celltype, kernel, Farm)) +
      facet_grid(vars(Farm), scales = "fixed") +

      # geom_line() +
      geom_line(data = . %>%
                  filter(ttime <= unique(tau))) +
      geom_line(data = . %>%
                  filter(ttime > unique(tau)), linetype = "dashed") +
      geom_hline(
        data = . %>% filter(Farm == "Farm B"),
        aes(yintercept = 0.50),
        linetype = "dotted"
      ) +
      geom_vline(
        # data = . %>% filter(Farm == "Farm B"),
        aes(xintercept = tau),
        linetype = "dotted"
      ) +

      geom_text(
        data = . %>% filter(Farm == "Farm B", ttime >= tau) %>% slice(1),
        aes(x = tau, y = prevalence / 2, label = "tau"),
        nudge_x = 25,
        size = 5.5,
        parse = TRUE) +
      geom_point(
        data = . %>% filter(Farm == "Farm B", ttime >= tau) %>% slice(1),
        aes(x = tau, y = prevalence),
        size = 2.5,
        shape = 16
      ) +
      labs(alpha = NULL, y = "Prevalence", x = "time") +
      labs(caption = glue("Kernel: {kernel}, and cell-shape: {celltype}")) +

      theme_blank_background() +
      NULL
  })
#'
#'
#'

if (plot_in_pdf_file) {
  dev.off()
}


beepr::beep()
