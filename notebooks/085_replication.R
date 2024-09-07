library("disEpiODE")
library("tidyverse")
library("sf")

landscape <- create_landscape(scale=1.0)
farms <- create_farm_placement(landscape)
kernel <- create_kernel("half-normal", sigma=0.08456)

grid <- create_grid(landscape, cellarea = 0.001, celltype="hexagon", rotate=0)
overlap <- create_farm_overlap(grid, farms)
init <- create_initial_state(grid, overlap, start_prev=0.5)
beta_matrix <- create_transmission_matrix(grid, kernel, beta_baseline=0.05)

taufun <- create_si_model(grid, beta_matrix, init, overlap, root="B")
taufun(0.5)

fun <- create_si_model(grid, beta_matrix, init, overlap, root="none")
results <- fun(time=seq(0,5000,by=10))

ggplot(results, aes(x=Time, y=Prevalence)) +
  geom_line() +
  facet_wrap(~Area, ncol=1)



tibble(PatchSize = 10^seq(0,-3,by=-0.01)) %>%
  rowwise() %>%
  group_split() %>%
  lapply(function(x){

    grid <- create_grid(landscape, cellarea = x$PatchSize, celltype="hexagon", rotate=0)
    overlap <- create_farm_overlap(grid, farms)
    init <- create_initial_state(grid, overlap, start_prev=0.5)
    beta_matrix <- create_transmission_matrix(grid, kernel, beta_baseline=0.05)
    taufun <- create_si_model(grid, beta_matrix, init, overlap, root="middle")
    tau <- taufun(prevalence=0.5)

    bind_cols(x, tau)
  }) %>%
  bind_rows() ->
  results

ggplot(results, aes(x=PatchSize, y=Prevalence, col=Area)) +
  geom_line() +
  scale_x_log10_rev()

ggplot(results, aes(x=PatchSize, y=Time, col=Area)) +
  geom_line() +
  scale_x_log10_rev()


## Matt TODO

sigma_inv = 100
sigma_exp = 12.6161672832572
sigma_half_normal = 0.08456

