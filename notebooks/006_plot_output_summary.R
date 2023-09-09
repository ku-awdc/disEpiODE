#+ setup, message=FALSE
library(magrittr)
library(tidyverse)
library(sf)

library(glue)

devtools::load_all()
#'
#'
output_summary <-
  readr::read_csv("output/summary.csv", lazy = FALSE)
#'
#'
# VALIDATION: this must be empty
# output_summary %>%
#   count(n, sort = T, name = "duplicate_count") %>%
#   filter(duplicate_count > 1) %>% {
#     stopifnot("output grids cannot contain duplicates" =
#                 nrow(.) == 0)
#   }
#'
ggplot(output_summary) +
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
  # facet_wrap(~factor(beta_baseline), ncol = 1,
  #            labeller = . %>% label_both(sep= " = "),
  #
  #            scales = "free_y"
  #            ) +
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
