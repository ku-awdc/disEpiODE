

#+ setup
library(magrittr)
library(tidyverse)
# library(sf)
#
# library(glue)
# library(disEpiODE)

# CLEAR OUTPUT DIR
# clear_output_dir()


params1 <- tidyr::expand_grid(
  world_scale = 29,
  # beta_baseline = 0.05,
  beta_baseline = c(0.05, 0.005),
  # LOG: limit on laptop is 112
  n = seq_len(50),
  # n = seq_len(4),
  # n = seq_len(200)
  # n = c(10, 22, 3)
  # n = c(10,22)
)

#TODO: shuffle params1

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
      rmarkdown::render(
        input = "notebooks/004_run_model_with_full_report.R",
        output_file =
          glue::glue("004_n_{n}_beta_baseline_{beta_baseline}"),
        output_dir = "output/",
        params = params,
        intermediates_dir = tempdir(),
        quiet = FALSE
      )
      report_row
    })
#'
#'
output_summary %>%
  bind_rows() %>%
  print(n = Inf, width = Inf) %>%
  write_excel_csv("output/output_summary.csv")

