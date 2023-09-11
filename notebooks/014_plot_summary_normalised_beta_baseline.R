#+ setup, message=FALSE
library(magrittr)
library(tidyverse)
library(sf)

library(glue)

devtools::load_all()
#'
#'
#'
output_summary <- if (file.exists("output/012_output_summary.csv")) {
  readr::read_csv("output/012_output_summary.csv", lazy = FALSE)
} else {
  readr::read_csv("output/012_summary.csv", lazy = FALSE)
}
#'
output_summary <-
  output_summary %>%
  arrange(n, beta_baseline)
#'
#'
output_summary %>%
  group_by(factor(beta_baseline)) %>%
  # mutate(`normalised(tau)` = tau / last(tau)) %>%
  # summarise(last(tau)) %>%
  slice_tail(n = 10) %>%
  ungroup() %>%
  pull(tau) %>%
  identity()

output_summary %>%
  group_by(factor(beta_baseline)) %>%
  mutate(`normalised(tau)` = tau / last(tau)) %>%
  # complete(n = full_seq(n, 1)) %>%
  ungroup() %>%

  ggplot() +
  # aes(n**2, tau, group = factor(beta_baseline)) +
  # aes(n, tau, group = factor(beta_baseline)) +
  aes(n, `normalised(tau)`, group = factor(beta_baseline)) +
  geom_step(aes(color = factor(beta_baseline))) +

  # lims(x = ())

  theme_blank_background() +
  labs(caption = glue_data(output_summary,
                           "n_max = {max(n)}")) +
  theme(legend.position = "top") +
  NULL
#'
#'
output_summary %>%
  dplyr::filter(n >= 5) %>%
  ggplot() +
  # aes(n**2, tau, group = factor(beta_baseline)) +
  aes(n, tau, group = factor(beta_baseline)) +
  geom_step() +

  geom_hline(
    aes(yintercept = tau,
        color = "last tau"),
    data = . %>% group_by(factor(beta_baseline)) %>%
      slice_max(n, n = 1),
    linetype = "dotted"
  ) +

  theme_blank_background() +
  labs(caption = glue_data(output_summary,
                           "n_max = {max(n)}")) +
  theme(legend.position = "top") +
  NULL
#'
#'

ggplot(output_summary) +
  aes(n, prevalence_population,
      group = factor(beta_baseline)) +
  geom_step() +
  # geom_step(aes(color = factor(beta_baseline))) +
  geom_hline(aes(yintercept = 0.5,
                 color = "target prevalence"),
             linetype = "dotted") +

  lims(y = c(0.5, 1)) +

  geom_hline(
    aes(yintercept = last(prevalence_population),
        color = "last population prevalence"),
    linetype = "dashed"
  ) +
  labs(
    y = "Population prevalence",
    caption = "t = tau, n² := total cells, target prevalence = 50%") +
  theme_blank_background() +

  theme(legend.position = "top") +
  NULL
#'
#'
#'
output_summary %>%
  filter(beta_baseline == 0.05) %>%
  pivot_longer(c(prevalence_middle, prevalence_population)) %>%
  ggplot() +
  aes(n, value, group = name) +
  geom_step(aes(color = name)) +
  # facet_wrap(~name, scales = "free_y") +
  theme_blank_background() +
  theme(legend.position = "top") +
  NULL
#'

