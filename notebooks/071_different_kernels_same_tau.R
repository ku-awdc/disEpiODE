

devtools::load_all(reset = FALSE)

total_cells <- 250
beta_baseline <- 0.05
beta_mat_list <- c("inverse", "half_normal", "exp")
remove_within_patch_transmission <- FALSE

#' Define unit landscape
#'
landscape_sf <- st_sfc(
  st_polygon(
    rbind(c(0, 0),
          c(0, 1),
          c(1, 1),
          c(1, 0),
          c(0, 0)) %>% list()
  )
)

p_landscape <- ggplot() +
  geom_sf(data = landscape_sf, fill = NA) +
  theme_grid_plot() +
  theme_blank_background() +
  NULL

p_landscape

# square_big_grid <- create_grid(landscape_sf, n = 250,  celltype = "square")
#NOTE: not used later on
square_big_grid <- create_grid(landscape_sf, n = floor(sqrt(total_cells)),
    celltype = "square")

square_big_grid

p_landscape +
  geom_sf(
    data = square_big_grid,
    fill = NA
  )
#'
#' The three kernels considered
#'
inv_sigma <- function(distance, sigma = 1) {
  1 / (sigma * distance + 1)
}

exp_sigma <- function(distance, sigma = 1) {
  exp(-sigma*distance)
}
half_normal_sigma <- function(distance, sigma = 1) {
  exp(-distance**2 / (2 * sigma**2))
}
half_normal_sigma(0)
all.equal(
  half_normal_sigma(0),
  1
)

# INTERESTING, although not useful
# integrate(inv_sigma, lower = 0, upper = Inf) # doesn't converge..
# integrate(exp_sigma, lower = 0, upper = Inf) # 1
# integrate(half_normal_sigma, lower = 0, upper = Inf) # 1.253314 with absolute error < 0.00012, although it is probably just sqrt(pi / 2) * abs(sigma) ...
#

stopifnot(
  inv_sigma(0) == 1,
  exp_sigma(0) == 1,
  half_normal_sigma(0) == 1
)

tibble(distance = seq.default(0, 10, by = 0.2),
       kernel_inv = inv_sigma(distance, sigma = 1),
       kernel_exp = exp_sigma(distance, sigma = 1),
       kernel_half_normal = half_normal_sigma(distance, sigma = 1),
) %>%
  pivot_longer(starts_with("kernel"),
               names_to = c("kernel"),
               names_pattern = "kernel_(.*)",
               values_to = "weight") %>%
  ggplot() +
  aes(x = distance, y = weight, group = kernel) +
  geom_line(aes(color = kernel)) +
  theme_blank_background() +
  theme(legend.position = "bottom") +
  NULL
#'
#' We'll choose "middle" as the operative zone
#'
buffer_radius <- 0.15
buffer_offset_percent <- 0.2
source_target <-
  get_buffer_source_target(landscape_width = 1,
                           landscape_height = 1,
                           buffer_radius = buffer_radius,
                           buffer_offset_percent = buffer_offset_percent)
middle_buffer <- get_middle_buffer(source_target = source_target,
                                   buffer_radius = buffer_radius)

all_buffers <-
  rbind(source_target, middle_buffer) %>%
  mutate(label = factor(label, c("source", "middle", "target")))
# world_area <- st_area(world_landscape)

grid <- create_grid(landscape_sf,
                    n = floor(sqrt(total_cells)),
                    celltype = "square",
                    #middle is not defined when perfect tessellation is used
                    middle = stop("unimplemented?"))
grid
p_landscape +
  geom_sf(data = grid, fill = NA) +
  geom_sf_text(aes(label = "🐷", geometry = buffer_point),
               size = 10,
               data = all_buffers) +
  geom_sf(data = all_buffers,
          aes(geometry = buffer_polygon, color = label),
          linewidth = 1,
          fill = NA) +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.justification = "center") +
  NULL
#TODO: calculate n if cellarea is provided, and vice versa

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


source_overlap <- all_buffers_overlap_map$source
target_overlap <- all_buffers_overlap_map$target
middle_overlap <- all_buffers_overlap_map$middle

source_zone_area <-
  all_buffers %>%
  dplyr::filter(label == "source") %>%
  pull(buffer_area)

# carry_density <- sum(grid$carry) / world_scale**2
carry_density <- sum(grid$carry) / st_area(landscape_sf)**2
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

sigma_exp <- 1
sigma_inv <- 1
sigma_half_normal <- 1


# region: exp

# kernel(d) = exp(-d)

beta_mat_exp <- beta_baseline * exp_sigma(dist_grid, sigma = sigma_exp)

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
  half_normal_sigma(dist_grid, sigma = sigma_half_normal)
# beta_mat_half_normal <- beta_baseline *
#   half_normal_param_kernel(dist_grid, 1.312475, -1.560466, 3.233037)
# diag(beta_mat_half_normal) <- beta_baseline

# this test fails, but
# > half_normal_param_kernel(0, 1.312475, -1.560466, 3.233037)
# [1] 0.9999617
# and it should exactly 1
#
diag(beta_mat_half_normal) %>% unique() %>% {
  stopifnot(isTRUE(all.equal(., beta_baseline)))
}
stopifnot(all(is.finite(beta_mat_half_normal)))

all_beta_mat$half_normal <- beta_mat_half_normal


# endregion


# region: inverse

# VALIDATION
# isSymmetric(dist_grid)

# kernel(d) = 1 / (1 + d)
# beta_mat_inverse <- beta_baseline * (1/(1 + dist_grid))
beta_mat_inverse <- beta_baseline * inv_sigma(dist_grid, sigma = sigma_inv)
stopifnot(all(is.finite(beta_mat_inverse)))
diag(beta_mat_inverse) %>% unique() %>% {
  stopifnot(isTRUE(all.equal(., beta_baseline)))
}
all_beta_mat$inverse <- beta_mat_inverse

# region: scaled inverse
# choices <- c(0.4049956, 0.2700693);
# scaled_d <- choices[1]

# kernel(d) = 1 / (1 + d / scaled_d)
# beta_mat_scaled_inverse <- beta_baseline * (1/(1 + dist_grid / scaled_d))
# stopifnot(all(is.finite(beta_mat_scaled_inverse)))
# diag(beta_mat_scaled_inverse) %>% unique() %>% {
  # stopifnot(isTRUE(all.equal(., beta_baseline)))
# }
# all_beta_mat$scaled_inverse <- beta_mat_scaled_inverse

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
                # hmax = na_as_null(hmax_list[[beta_mat_name]]),
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

  result[[glue("output_{beta_mat_name}_prevalence")]] <- list(prevalence_at_tau)
}
# structure(
#   list(
#     grid = grid,
#     output = result
#   )
# )

result$output_inverse_tau$tau
result$output_exp_tau$tau
result$output_half_normal_tau$tau
