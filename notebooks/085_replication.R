library("disEpiODE")
library("tidyverse")
library("sf")
library("pbapply")

# Replicate paper figures 5 and 6
landscape <- create_landscape(scale=1.0)
farms <- create_farm_placement(landscape)

kernels <- list(
  inverse = create_kernel("inverse", sigma=100),
  exponential = create_kernel("exponential", sigma=12.616),
  halfnormal = create_kernel("half-normal", sigma=0.08456)
)

expand_grid(PatchSize = 10^seq(0,-3,by=-0.01), GridType=c("hexagon","square","triangle")) %>%
  rowwise() %>%
  group_split() %>%
  pblapply(function(x){

    grid <- create_grid(landscape, patch_area = x$PatchSize, grid_type=x$GridType, rotate=0)
    overlap <- create_farm_overlap(grid, farms)
    init <- create_initial_state(grid, overlap, start_prev=0.5)

    lapply(names(kernels), function(k){
      beta_matrix <- create_transmission_matrix(grid, kernels[[k]], beta_baseline=0.05)
      taufun <- create_si_model(grid, beta_matrix, init, overlap, root="B")
      tau <- taufun(prevalence=0.5)
      bind_cols(x, tau) %>% mutate(Kernel = k)
    }) %>%
      bind_rows()

  }) %>%
  bind_rows() ->
  results

# Fig 5:
results |>
  filter(Area=="Population") |>
  ggplot(aes(x=PatchSize, y=Time, col=Kernel)) +
  geom_line() +
  scale_x_log10_rev() +
  facet_wrap(~GridType, ncol=1)

# Fig 6:
results |>
  filter(Area!="Farm B") |>
  ggplot(aes(x=PatchSize, y=Prevalence, col=GridType)) +
  geom_line() +
  scale_x_log10_rev() +
  facet_grid(Area ~ Kernel, scales="free_y")


