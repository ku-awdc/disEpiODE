

#+ setup
library(magrittr)
library(tidyverse)
# library(sf)
#
# library(glue)
# library(disEpiODE)

# Clean the `output` directory, if it is there.
if (fs::dir_exists("output/")) {
  fs::dir_delete("output/")
  fs::dir_create("output/")
} else {
  fs::dir_create("output/")
}


#'
#'

params1 <- tidyr::expand_grid(
  world_scale = 29,
  # beta_baseline = 0.05,
  beta_baseline = c(0.05, 0.005),
  buffer_radius = 3.5,
  # LOG: limit on laptop is 112
  n = seq_len(50),
  # n = seq_len(4),
  # n = seq_len(200)
  # n = c(10, 22, 3)
  # n = c(10,22)
) %>%
  dplyr::sample_n(size = n())

# library(future)
# # plan(multicore(workers = 4))
# plan(future::sequential())
# library(furrr)

output_summary <-
  # furrr::future_map(
  purrr::map(
    purrr::transpose(params1),
    \(params) {
      n <- params$n
      beta_baseline <- params$beta_baseline
      rmarkdown::render(
        input = "notebooks/007_add_middle_observation_buffer.R",
        output_file =
          glue::glue("007_n_{n}_beta_baseline_{beta_baseline}"),
        output_dir = "output/",
        params = params,
        intermediates_dir = tempdir(),
        clean = TRUE,
        quiet = FALSE
      )
      report_row
    })
#'
#'
output_summary %>%
  bind_rows() %>%
  print(n = Inf, width = Inf) %>%
  write_excel_csv("output/007_output_summary.csv")

