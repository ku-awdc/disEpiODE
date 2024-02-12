
library(sfdep)

ggplot() + geom_sf(data = sfdep::guerry)

n <- 2500
geo <- sf::st_sample(sfdep::guerry$geometry, n)

ggplot() +
  geom_sf(data = geo, shape = ".")  +
  geom_sf(data = sfdep::guerry, fill = NA) +

  theme_blank_background() +
  disEpiODE::theme_grid_plot()

# # define upper band for search distances
# threshold <- 20000
threshold <- 20000

nbs <- st_dist_band(geo, threshold)

nbs[3]
st_nb_dists(nbs)

# define your kernel function
kernel_fn <- function(x) exp(-x)
# # this is the kernel function applied to dij
# kernel_fn <- function(x) exp(-1 * x)
#

# generate values
x <- rnorm(n)

# res <- purrr::imap_dbl(nbs, \(.x, idx) sum(kernel_fn(.x) * x[idx]))
head(res)
#> [1]  1.450150e-139 -4.773053e-180  1.451084e-252  1.066659e-201 -4.326234e-215
#> [6]  -9.271764e-33

# # define number of time steps
# t = 10

#
# # create data frame with time, geometry, and beta_vals
# my_df <- tibble(
#   t = rep.int(1:t, n / t), # trick
#   geometry = geo,
#   beta_vals = rbeta(n, 0.5, 0.5)
# )
#
res <- my_df |>
  # group for each time stamp
  group_by(t) |>
  mutate(
    # find neighbors per time stamp based on the
    # distance threshold. stored as sparse matrix
    # list
    nb = st_dist_band(geometry, 0, threshold),
    # for each location's neighbors calculate dist
    nb_dists = st_nb_dists(geometry, nb),
    # apply the kernel function to the distances
    wts = lapply(nb_dists, kernel_fn),
    # the spatial lag is defined as sum(xij * wij)
    # might be wrong in your case
    # you might want xi * sum(wij)
    value = st_lag(beta_vals, nb, wts)
  )
