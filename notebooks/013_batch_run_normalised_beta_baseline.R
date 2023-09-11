

#+ setup
library(magrittr)
library(tidyverse)
# library(sf)
#
# library(glue)
# library(disEpiODE)

# NOTE: Make sure to install disEpiODE before running this script

# Clean the `output` directory, if it is there.
# if (fs::dir_exists("output/")) {
#   fs::dir_delete("output/")
#   fs::dir_create("output/")
# } else {
#   fs::dir_create("output/")
# }


#'
#'

params1 <- tidyr::expand_grid(
  world_scale = 29,
  # beta_baseline = 0.05,
  # beta_baseline = c(0.05, 0.005),
  beta_baseline = c(0.05),
  buffer_offset_percent = 0.2,
  buffer_radius = 3.5,
  offset = c("corner", "middle", "bottom", "left"),
  # LOG: limit on laptop is 112
  # n = seq_len(100),
  n = seq_len(25),
  # n = 50,
  # n = seq_len(4),
  # n = seq_len(200)
  # n = c(10, 22, 3)
  # n = c(10,22)
) %>%
  # dplyr::sample_n(size = n()) %>%
  identity()

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
        input = "notebooks/012_run_with_common_area_normalisation.R",
        output_file =
          glue::glue("012_n_{n}_beta_baseline_{beta_baseline}.html"),
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
  write_excel_csv("output/012_output_summary.csv")

