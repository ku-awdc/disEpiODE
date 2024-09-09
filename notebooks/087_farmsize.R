library("disEpiODE")
library("tidyverse")
library("sf")
library("pbapply")

# Check impact of farm size

kernels <- list(
  inverse = create_kernel("inverse", sigma=100),
  exponential = create_kernel("exponential", sigma=12.616),
  halfnormal = create_kernel("half-normal", sigma=0.08456)
)
kernel <- kernels[["halfnormal"]]

landscape <- create_landscape(scale=1.0)

all_farms <- list(
  vsmall_close = create_farm_placement(landscape, buffer_radius = 0.05, buffer_offset_percent = 0.3),
  vsmall_reg = create_farm_placement(landscape, buffer_radius = 0.05, buffer_offset_percent = 0.2),
  vsmall_far = create_farm_placement(landscape, buffer_radius = 0.05, buffer_offset_percent = 0.1),
  small_close = create_farm_placement(landscape, buffer_radius = 0.1, buffer_offset_percent = 0.3),
  small_reg = create_farm_placement(landscape, buffer_radius = 0.1, buffer_offset_percent = 0.2),
  regular_close = create_farm_placement(landscape, buffer_radius = 0.15, buffer_offset_percent = 0.25),
  regular_reg = create_farm_placement(landscape, buffer_radius = 0.15, buffer_offset_percent = 0.2),
  large_reg = create_farm_placement(landscape, buffer_radius = 0.2, buffer_offset_percent = 0.2)
)
names(all_farms) |>
  map_df(\(x) all_farms[[x]] |> mutate(Type = x)) |>
  ggplot() + geom_sf(aes(fill=label)) + facet_wrap(~Type)

expand_grid(PatchSize = 10^seq(0,-3,by=-0.01), GridType=c("hexagon","square","triangle"), Rotate=0) %>%
  rowwise() %>%
  group_split() %>%
  pblapply(function(x){

    ss <- try(grid <- create_grid(landscape, patch_area = x$PatchSize, grid_type=x$GridType, rotate=x$Rotate))
    if(inherits(ss, "try-error")) return(NULL)

    lapply(names(all_farms), function(f){
      overlap <- create_farm_overlap(grid, all_farms[[f]])
      init <- create_initial_state(grid, overlap, start_prev=0.5)
      beta_matrix <- create_transmission_matrix(grid, kernel, beta_baseline=0.05)
      taufun <- create_si_model(grid, beta_matrix, init, overlap, root="B")
      tau <- taufun(prevalence=0.5)
      bind_cols(x, tau) %>% mutate(FarmType = f)
    }) %>%
      bind_rows()

  }) %>%
  bind_rows() ->
  results

results |>
  filter(Area=="Population") |>
  ggplot(aes(x=PatchSize, y=Time, col=FarmType)) +
  geom_line() +
  scale_x_log10_rev() +
  facet_wrap(~GridType, ncol=1)
