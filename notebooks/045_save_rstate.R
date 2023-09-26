#
#
# # fs::dir_ls("output/", type = "file", glob = "*.rda")
# # 043_model_output_beta_beta_mat_half_normal_world_scale_29_beta_baseline_0.05_buffer_offset_percent_0.2_buffer_radius_3.5_cellarea_10_celltype_triangle.rds
# #
# # normalizePath("~")
# # C:\Users\minin\GitHub\disEpiODE\output
# # readr::read_rds("output/" %>% normalizePath())
model_output <-
  readr::read_rds("output/043_model_output_beta_beta_mat_exp_world_scale_29_beta_baseline_0.05_buffer_offset_percent_0.2_buffer_radius_3.5_cellarea_0.45_celltype_hexagon.rds")
# model_output$tau_model_output
# methods(class = "deSolve")
# # deSolve::matplot.deSolve(model_output$tau_model_output)
# diag_model_output <-
#   deSolve::diagnostics(model_output$tau_model_output)
# diag_model_output$istate
# diag_model_output$rstate[1]
# diag_model_output$iroot
#
# deSolve::diagnostics(model_output$tau_model_output)
# model_output$tau_model_output %>%
#   attributes()
deSolve::diagnostics(model_output$tau_model_output)$rstate
deSolve::diagnostics(model_output$model_output)$rstate


