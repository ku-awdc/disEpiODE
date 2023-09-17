conflicts_prefer(dplyr::filter)

fct_facet_var <- . %>%
  factor(levels = c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),
         labels = c("tau / last(tau)", "prev(target)", "prev(pop)", "prev(center)")
  )

readr::read_csv("output/033_output_summary.csv") ->
  output_summary


output_summary %>% distinct(beta_mat_name)
output_summary %>% distinct(celltype)
output_summary %>% distinct(cellarea)
output_summary %>% distinct(beta_baseline)


output_summary %>%
  filter(celltype == "square",
         beta_mat_name == "beta_mat_exp",
         beta_baseline == 0.05) %>%
  # glimpse() %>%
  # add normalised tau
  arrange(celltype, beta_mat_name, beta_baseline, cellarea) %>%
  group_by(celltype, beta_mat_name, beta_baseline) %>%
  mutate(normalised_tau = tau / first(tau)) %>%
  ungroup() %>%

  # prepare for square-plot
  pivot_longer(
    c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),

  ) %>%
  mutate(name = fct_facet_var(name)) %>%
  identity() %>% {
    beta_baseline <- unique(.$beta_baseline)
    beta_mat_name <- unique(.$beta_mat_name)
    ggplot(.) +
      aes(cellarea, value, group = str_c(celltype)) +
      geom_step() +
      facet_wrap(~name, scales = "free_y") +
      # scale_y_continuous(trans = "log") +
      # scale_y_continuous(trans = "sqrt") +
      # scale_x_reverse() +
      scale_x_log10_rev() +
      theme_reverse_arrow_x() +
      labs(caption = glue("beta = {beta_baseline}, kernel = {beta_mat_name}")) +
      theme_blank_background()
  }




output_summary %>%
  filter(beta_mat_name == "beta_mat_exp",
         beta_baseline == 0.05,
         !celltype %in% c("hexagon_rot")) %>%
  # glimpse() %>%
  # add normalised tau
  arrange(celltype, beta_mat_name, beta_baseline, cellarea) %>%
  group_by(celltype, beta_mat_name, beta_baseline) %>%
  mutate(normalised_tau = tau / first(tau)) %>%
  ungroup() %>%

  # prepare for square-plot
  pivot_longer(
    c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),

  ) %>%
  mutate(name = fct_facet_var(name)) %>%
  identity() %>% {
    beta_baseline <- unique(.$beta_baseline)
    beta_mat_name <- unique(.$beta_mat_name)
    ggplot(.) +
      aes(cellarea, value, group = str_c(celltype)) +
      geom_step(aes(color = celltype)) +
      facet_wrap(~name, scales = "free_y") +
      # scale_y_continuous(trans = "log") +
      # scale_y_continuous(trans = "sqrt") +
      # scale_x_reverse() +
      scale_x_log10_rev() +
      theme_reverse_arrow_x() +
      labs(caption = glue("beta = {beta_baseline}, kernel = {beta_mat_name}")) +
      theme_blank_background()
  }




output_summary %>%
  filter(
         beta_mat_name == "beta_mat_half_normal",
         !celltype %in% c("hexagon_rot")) %>%
  # glimpse() %>%
  # add normalised tau
  arrange(celltype, beta_mat_name, beta_baseline, cellarea) %>%
  group_by(celltype, beta_mat_name, beta_baseline) %>%
  mutate(normalised_tau = tau / first(tau)) %>%
  ungroup() %>%

  # prepare for square-plot
  pivot_longer(
    c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),

  ) %>%
  mutate(name = fct_facet_var(name)) %>%
  identity() %>% {
    beta_baseline <- unique(.$beta_baseline)
    beta_mat_name <- unique(.$beta_mat_name)
    ggplot(.) +
      aes(cellarea, value, group = str_c(celltype, beta_baseline)) +
      geom_step(aes(color = celltype, linetype = factor(beta_baseline))) +
      labs(linetype = "beta baseline") +
      facet_wrap(~name, scales = "free_y") +
      # scale_y_continuous(trans = "log") +
      # scale_y_continuous(trans = "sqrt") +
      # scale_x_reverse() +
      scale_x_log10_rev() +
      theme_reverse_arrow_x() +
      labs(caption = glue("beta = {beta_baseline}, kernel = {beta_mat_name}")) +
      theme_blank_background()
  }



traj1 <- read_rds(
  "output/033_model_output_beta_beta_mat_exp_world_scale_29_beta_baseline_0.05_buffer_offset_percent_0.2_buffer_radius_3.5_cellarea_0.253110943474544_celltype_square.rds"
)
traj1$tau_model_output %>% colnames()
prevalence_throughout <- . %>% {
  .$model_output[,c(1, (2*nrow(grid) + 2):ncol(.$model_output)),
                 drop = FALSE]
}
model_output$model_output[,c(1, (2*nrow(grid) + 2):ncol(model_output$model_output)),
                          drop = FALSE]
traj1$model_output %>%

  as_tibble()

output_summary %>%
  # bind_rows() %>%
  arrange(celltype, beta_mat_name, beta_baseline, cellarea) ->
  output_summary

output_summary %>%
  count(celltype, beta_mat_name, beta_baseline)

output_summary_square <- output_summary %>%
  glimpse() %>%

  # add normalised tau
  arrange(celltype, beta_mat_name, beta_baseline, cellarea) %>%
  group_by(celltype, beta_mat_name, beta_baseline) %>%
  mutate(normalised_tau = tau / last(tau)) %>%
  ungroup() %>%

  # prepare for square-plot
  pivot_longer(
    c("normalised_tau", "prevalence_target", "prevalence_population", "prevalence_middle"),

  ) %>%
  mutate(name = fct_facet_var(name))

output_summary_square %>%
  group_by(beta_mat_name, beta_baseline) %>%
  group_map(
    \(x, group_id) {

      ggplot(x) +
        aes(cellarea, value, group = str_c(celltype)) +
        geom_step(aes(color = celltype)) +
        facet_wrap(~name, scales = "free_y") +
        # scale_y_continuous(trans = "log") +
        # scale_y_continuous(trans = "sqrt") +

        labs(caption = glue("beta = {group_id$beta_baseline}, kernel = {group_id$beta_mat_name}")) +
        theme_blank_background()
    }
  )

