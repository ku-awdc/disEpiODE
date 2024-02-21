
devtools::load_all()

inv_sigma <- function(distance, sigma = 1) {
  1 / (sigma * distance + 1)
}
exp_sigma <- function(distance, sigma = 1) {
  exp(-sigma * distance)
}
half_normal_sigma <- function(distance, sigma = 1) {
  exp(-distance ** 2 / (2 * sigma ** 2))
}


# tibble(distance = seq.default(0, 2, by = 0.01),
#        kernel_inv = inv_sigma(distance, sigma = sigma_inv),
#        kernel_exp = exp_sigma(distance, sigma = sigma_exp),
#        kernel_half_normal = half_normal_sigma(distance, sigma = sigma_half_normal),
# ) %>%
#   pivot_longer(starts_with("kernel"),
#                names_to = c("kernel"),
#                names_pattern = "kernel_(.*)",
#                values_to = "weight") %>%
common_sigma <- 1

ggplot(tibble(x = c(0, 25))) +
  aes(x) +
  # aes(x = distance, y = weight, group = kernel) +
  # geom_line(aes(color = kernel)) +
  stat_function(fun = \(d) inv_sigma(d, sigma = common_sigma),
                aes(color = "inverse")) +
  stat_function(fun = \(d) exp_sigma(d, sigma = common_sigma),
                aes(color = "exp")) +
  stat_function(fun = \(d) half_normal_sigma(d, sigma = common_sigma),
                aes(color = "half-normal")) +
  labs(x = "distance", y = NULL, color = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  theme_blank_background() +
  labs(caption = expression("Common " ~ sigma == 1)) +
  theme(legend.position = "bottom") +
  NULL

#' Short-range kernel calibrations
sigma_inv <- 100
sigma_exp <- 12.6
sigma_half_normal <- 0.085

ggplot(tibble(x = c(0, 1))) +
  aes(x) +
  # aes(x = distance, y = weight, group = kernel) +
  # geom_line(aes(color = kernel)) +
  stat_function(fun = \(d) inv_sigma(d, sigma = sigma_inv),
                aes(color = "inverse")) +
  stat_function(fun = \(d) exp_sigma(d, sigma = sigma_exp),
                aes(color = "exp")) +
  stat_function(fun = \(d) half_normal_sigma(d, sigma = sigma_half_normal),
                aes(color = "half-normal")) +
  labs(x = "distance", y = NULL, color = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  labs(caption = "Parameters: Inverse {sigma_inv}, exp {sigma_exp}, half-normal  {sigma_half_normal}" %>% glue()) +
  theme_blank_background() +
  theme(legend.position = "bottom") +
  NULL

#' Long range
#'
sigma_inv <- 30
sigma_exp <- 7.25
sigma_half_normal <- 0.135

ggplot(tibble(x = c(0, 1))) +
  aes(x) +
  # aes(x = distance, y = weight, group = kernel) +
  # geom_line(aes(color = kernel)) +
  stat_function(fun = \(d) inv_sigma(d, sigma = sigma_inv),
                aes(color = "inverse")) +
  stat_function(fun = \(d) exp_sigma(d, sigma = sigma_exp),
                aes(color = "exp")) +
  stat_function(fun = \(d) half_normal_sigma(d, sigma = sigma_half_normal),
                aes(color = "half-normal")) +
  labs(x = "distance", y = NULL, color = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  labs(caption = "Parameters: Inverse {sigma_inv}, exp {sigma_exp}, half-normal  {sigma_half_normal}" %>% glue()) +
  theme_blank_background() +
  theme(legend.position = "bottom") +
  NULL

