output_summary %>%
  bind_rows() %>%
  arrange(celltype, beta_mat_name, beta_baseline, cellarea) ->
  output_summary

output_summary %>%
  count(celltype, beta_mat_name)


fct_facet_var <- . %>%
  factor(levels = c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),
         labels = c("tau / last(tau)", "prev(target)", "prev(pop)", "prev(center)")
  )

output_summary_square <- output_summary %>%
  glimpse() %>%

  # add normalised tau
  arrange(celltype, beta_mat_name, beta_baseline, cellarea) ->
  group_by(celltype, beta_mat_name, beta_baseline) %>%
  mutate(normalised_tau = tau / last(tau)) %>%
  # mutate(normalised_tau = tau) %>%
  ungroup() %>%


  # prepare for square-plot
  pivot_longer(
    c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),

  ) %>%
  mutate(name = fct_facet_var(name))

output_summary_square %>%
  # filter(n >= 3) %>%
  group_by(beta_mat_name) %>%
  group_map(
    \(x, group_id) {

      ggplot(x) +
        aes(cellarea, value, group = str_c(celltype, beta_baseline)) +
        geom_step(aes(color = celltype)) +
        facet_wrap(~name, scales = "free_y") +
        # scale_y_continuous(trans = "log") +
        # scale_y_continuous(trans = "sqrt") +

        labs(caption = glue("beta = {group_id$beta_baseline}")) +
        theme_blank_background()
    }
  )
