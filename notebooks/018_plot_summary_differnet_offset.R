#+ setup, message=FALSE
library(magrittr)
library(tidyverse)
library(sf)

library(glue)

devtools::load_all()
#'
#'
#'
output_summary_path <- "output/017_output_summary.csv"
output_summary <- if (file.exists(output_summary_path)) {
  readr::read_csv(output_summary_path, lazy = FALSE)
} else {
  message("Complete results not available;\nLoading processed results")
  readr::read_csv("output/017_summary.csv", lazy = FALSE)
}
#' What's the "key"
#'
output_summary %>%
  count(n, beta_baseline, offset, name = "count_configurations") %>%
  summarise(
    stopifnot(
      "identify what consitutes as single experiments" =
        all(count_configurations == 1))
  )
#'
#'
#'
output_summary <-
  output_summary %>%
  arrange(n, beta_baseline, offset)
#'

output_summary %>%
  group_by(beta_baseline) %>%
  group_map(\(x, group_id) {
    # group_id
    ggplot(x) +
      aes(n, tau, group = offset) +
      geom_step(aes(color = offset)) +
      labs(caption = glue("beta = {group_id}")) +
      theme_blank_background() +
      NULL
  })

fct_facet_var <- . %>%
  factor(levels = c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),
         labels = c("tau / norm(tau)", "prev(target)", "prev(pop)", "prev(center)")
  )

output_summary_square <- output_summary %>%
  glimpse() %>%

  # add normalised tau
  arrange(offset, beta_baseline, n) %>%
  # View()
  group_by(offset, beta_baseline) %>%
  mutate(normalised_tau = tau / last(tau)) %>%
  # mutate(normalised_tau = tau) %>%
  ungroup() %>%


  # prepare for square-plot
  pivot_longer(
    c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),

  ) %>%
  mutate(name = fct_facet_var(name))

output_summary_square %>%
  filter(n >= 3) %>%
  group_by(beta_baseline) %>%
  group_map(
    \(x, group_id) {

      ggplot(x) +
        aes(n, value, group = offset) +
        geom_step(aes(color = offset)) +
        facet_wrap(~name) +
        # scale_y_continuous(trans = "log") +
        # scale_y_continuous(trans = "sqrt") +

        labs(caption = glue("beta = {group_id$beta_baseline}")) +
        theme_blank_background()
    }
  )


#'
#' output_summary %>%
#'   group_by(factor(beta_baseline)) %>%
#'   mutate(`normalised(tau)` = tau / last(tau)) %>%
#'   # complete(n = full_seq(n, 1)) %>%
#'   ungroup() %>%
#'
#'   ggplot() +
#'   # aes(n**2, tau, group = factor(beta_baseline)) +
#'   # aes(n, tau, group = factor(beta_baseline)) +
#'   aes(n, `normalised(tau)`, group = factor(beta_baseline)) +
#'   geom_step(aes(color = factor(beta_baseline))) +
#'
#'   # lims(x = ())
#'
#'   theme_blank_background() +
#'   labs(caption = glue_data(output_summary,
#'                            "n_max = {max(n)}")) +
#'   theme(legend.position = "top") +
#'   NULL
#' #'
#' #'
#' output_summary %>%
#'   dplyr::filter(n >= 5) %>%
#'   ggplot() +
#'   # aes(n**2, tau, group = factor(beta_baseline)) +
#'   aes(n, tau, group = factor(beta_baseline)) +
#'   geom_step() +
#'
#'   geom_hline(
#'     aes(yintercept = tau,
#'         color = "last tau"),
#'     data = . %>% group_by(factor(beta_baseline)) %>%
#'       slice_max(n, n = 1),
#'     linetype = "dotted"
#'   ) +
#'
#'   theme_blank_background() +
#'   labs(caption = glue_data(output_summary,
#'                            "n_max = {max(n)}")) +
#'   theme(legend.position = "top") +
#'   NULL
#' #'
#' #'
#'
#' ggplot(output_summary) +
#'   aes(n, prevalence_population,
#'       group = factor(beta_baseline)) +
#'   geom_step() +
#'   # geom_step(aes(color = factor(beta_baseline))) +
#'   geom_hline(aes(yintercept = 0.5,
#'                  color = "target prevalence"),
#'              linetype = "dotted") +
#'
#'   lims(y = c(0.5, 1)) +
#'
#'   geom_hline(
#'     aes(yintercept = last(prevalence_population),
#'         color = "last population prevalence"),
#'     linetype = "dashed"
#'   ) +
#'   labs(
#'     y = "Population prevalence",
#'     caption = "t = tau, n² := total cells, target prevalence = 50%") +
#'   theme_blank_background() +
#'
#'   theme(legend.position = "top") +
#'   NULL
#' #'
#' #'
#' #'
#' output_summary %>%
#'   filter(beta_baseline == 0.05) %>%
#'   pivot_longer(c(prevalence_middle, prevalence_population)) %>%
#'   ggplot() +
#'   aes(n, value, group = name) +
#'   geom_step(aes(color = name)) +
#'   # facet_wrap(~name, scales = "free_y") +
#'   theme_blank_background() +
#'   theme(legend.position = "top") +
#'   NULL
#' #'

