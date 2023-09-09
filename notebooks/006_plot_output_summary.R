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
output_summary %>%
  count(n, sort = T, name = "duplicate_count") %>%
  filter(duplicate_count > 1) %>% {
    stopifnot("output grids cannot contain duplicates" =
                nrow(.) == 0)
  }
#'
ggplot(output_summary) +
  aes(n**2, tau) +
  # aes(n, tau) +
  geom_step() +
  scale_x_log10() +
  geom_hline(
    aes(yintercept = last(tau),
    color = "last tau"
        ),
    linetype = "dotted"
  ) +
  theme_blank_background() +
  labs(caption = glue_data(output_summary,
                           "Last tau = tau_{last(n)} = {zapsmall(last(tau))}")) +
  theme(legend.position = "top") +
  NULL
#'
#'

ggplot(output_summary) +
  aes(n, prevalence_population) +
  geom_step() +

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
