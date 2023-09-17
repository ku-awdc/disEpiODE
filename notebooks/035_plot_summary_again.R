# output_summary %>% length()

# readr::read_csv("output/033_summary.csv")
readr::read_csv("output/033_output_summary.csv") %>%
  filter(celltype == "triangle", beta_baseline == 0.05,
         beta_mat_name == "beta_mat_half_normal") %>%
  View()



