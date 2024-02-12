# SOURCE: https://www.sciencedirect.com/science/article/pii/S0304380007000853?casa_token=hwxWP9RaZ5wAAAAA:BFrY5Y90FjK_eZCUALYA_HfoY6LyuZeAnI4tFCYVl5xBpQ0XWyKzZiF3ghh_eKI1PRpuJmSR#bib28
#


#' Returns Probability of Event in density-dependent context
#'
#' @param N Population count (integer)
#' @param cc Carrying Capacity (integer)
#' @param p_low,p_high Lower and upper threshold of event
#'
#' @note The 5 might be because population density varies from 3-5 / km².
pDDevent <- function(N, cc, p_low, p_high) {
  stopifnot(p_low >= 0, p_high >= 0, p_low <= 1, p_high <= 1,
            p_low <= p_high
            )
  p_threshold <- if_else(cc == 0, 5, pmin(N/cc, 5))
  p_low - (p_low - p_high) * tanh(p_threshold)
}

# pDDevent(0:10, 0:10, 0.85, 0.95)
#
expand_grid(
  N = 0:25,
  cc = 0:10,
  p_low = 0.85,
  p_high = 0.95
) %>%
  mutate(pDDevent = pDDevent(N, cc, p_low, p_high)) %>%
  identity() -> prob_df

prob_df %>%
  ggplot() +
  geom_line(aes(N, pDDevent, group = cc, color = cc),
            show.legend = TRUE) +
  # facet_wrap(~cc) +
  # scale_color_discrete() +
  NULL
#'
#'
#' This formula makes sense. I believe it is safe to replace
#' the constant 5 with `max_cc`, as the density assummed in
#' the paper is 3-4 wild boar / km².
#'
