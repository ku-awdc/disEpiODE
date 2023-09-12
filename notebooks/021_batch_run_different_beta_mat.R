

#+ setup
library(magrittr)
library(tidyverse)
# library(sf)
#
library(glue)
# library(disEpiODE)

# NOTE: Make sure to install disEpiODE before running this script

# Clean the `output` directory, if it is there.
# disEpiODE:::clear_output_dir()
options(error = recover)

#'
#'
tag <- "021" # REMEMBER TO SET THIS
params1 <- tidyr::expand_grid(
  world_scale = 29,
  # beta_baseline = 0.05,
  # beta_baseline = c(0.05),
  beta_baseline = c(0.1, 0.05, 0.005, 0.01),
  buffer_offset_percent = 0.2,
  buffer_radius = 3.5,
  offset = "corner",
  # offset = c("corner", "middle", "bottom", "left"),
  # LOG: limit on laptop is 112
  # n = seq_len(100),
  n = seq_len(50),
  # n = 50,
  # n = seq_len(4),
  # n = seq_len(200)
  # n = c(10, 22, 3)
  # n = c(10,22)
) %>%
  dplyr::sample_n(size = n()) %>%
  identity()

library(future)
plan(multisession(workers = 4))
# plan(multicore(workers = 4))
# plan(future::sequential())
library(furrr)

output_summary <-
  furrr::future_map(
    .options = furrr_options(),

    # purrr::map(
    purrr::transpose(params1),
    \(params) {
      params_spec <- {
        params_min <- params
        params_min$tag <- NULL
        paste0(names(params_min), "_", params_min, collapse = "_")
      }
      # FIXME: remove this within the script..
      # params$root <- fs::path_expand(".")
      tag <- tag
      rmarkdown::render(
        input = glue("notebooks/019_run_different_kernels.R"),
        output_file =
          glue::glue("{tag}_{params_spec}_.html"),
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
  arrange(n) %>%
  # print(n = Inf, width = Inf) %>%
  write_excel_csv(glue("output/{tag}_output_summary.csv"))

beepr::beep()

