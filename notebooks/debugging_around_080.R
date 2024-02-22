A$output_ode_model[[4]]$exp(seq.default(0, 100, length.out = 250))[, "prevalence_population"]

A$output_exp_tau[[4]]
# 1626.974

ttimes <- seq.default(0, 1626.974, length.out = 250)


plot(
  ttimes,
  # A$output_ode_model[[4]]$exp(ttimes)[, "prevalence_population"]
  # A$output_ode_model[[4]]$exp(ttimes)[, "prevalence_middle"]
  A$output_ode_model[[4]]$exp(ttimes)[, "prevalence_target"]
)
