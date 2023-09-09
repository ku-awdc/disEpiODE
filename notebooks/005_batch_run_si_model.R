

#+ setup
library(magrittr)
library(tidyverse)
# library(sf)
#
# library(glue)
# library(disEpiODE)

#'
#'

params1 <- tidyr::expand_grid(
  world_scale = 29,
  beta_baseline = 0.05,
  # LOG: limit on laptop is 112
  n = seq_len(50),
  # n = seq_len(4),
  # n = seq_len(200)
  # n = c(10, 22, 3)
  # n = c(10,22)
)

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
      env <- new.env()
      rmarkdown::render(
        input = "notebooks/004_run_model_with_full_report.R",
        output_file =
          glue::glue("004_n_{n}"),
        # knit_root_dir = "",
        output_dir = "output/",
        params = params,
        envir = env,
        intermediates_dir = tempdir(),
        # clean = FALSE,
        quiet = FALSE
      )
      env$report_row
    })
#'
#'
output_summary %>%
  bind_rows() %>%
  unnest_wider(params) %>%
  write_excel_csv("output/output_summary.csv")

