
library(magrittr)
library(ggplot2)
library(tibble)


# plot.new()
# par(m = 0)
# curve(exp(-x), to = 20)
# curve(1/sqrt(x), to = 20, add = TRUE)

ggplot(tibble(x=c(0,10))) +
  aes(x) +
  stat_function(fun = \(x) exp(-x), aes(color = "exp")) +
  stat_function(fun = \(x) 1/sqrt(x), aes(color = "sqrt")) +
  stat_function(fun = \(x)
                disEpiODE:::half_normal_kernel(x) /
                  disEpiODE:::half_normal_kernel(0), aes(color = "half-normal")) +

  disEpiODE::theme_blank_background()
#'
#'
#'
pracma::integral(\(x)exp(-x), xmin = 0, xmax = Inf)
pracma::integral(\(x)1 / sqrt(x), xmin = 0, xmax = Inf)
pracma::integral(\(x)disEpiODE:::half_normal_kernel(x), xmin = 0, xmax = Inf)
pracma::integral(\(x) (1/1.570796) * disEpiODE:::half_normal_kernel(x) /
                   disEpiODE:::half_normal_kernel(0), xmin = 0, xmax = Inf)
#' That's what the normalisation constant is.. somewhat
# 2*pnorm(0, sd = disEpiODE:::half_normal_sd(1), mean = 1, lower.tail = FALSE)
#'

pracma::integral(\(x)
                 2 *
                   dnorm(x, sd = disEpiODE:::half_normal_sd(mean = 1),
                         mean = 1),
                 xmin = 0, xmax = Inf)


# 2 * dnorm(0, sd = disEpiODE:::half_normal_sd(mean = 1),
#       mean = 0)






optimize(
  interval = c(0, 100),
  function(mean) {
    # browser()
    current_kernel <- function(x, mean) {
      2 *
        dnorm(x,
              sd = disEpiODE:::half_normal_sd(mean = mean),
              mean = 0)
    }
    # browser()
    abs(current_kernel(0, mean) - 1) +
      abs(integrate(current_kernel, lower = 0, upper = Inf, mean)$value - 1)
  })
# 0.6366252
#
pracma::integral(\(x)
                 2 *
                   dnorm(x, sd = disEpiODE:::half_normal_sd(mean = 0.6366252),
                         mean = 0),
                 xmin = 0, xmax = Inf)

2 * dnorm(0,
          sd = disEpiODE:::half_normal_sd(mean = 0.6366252),
          mean = 0)
# pracma::integral(\(x)
#                  2 *
#                    dnorm(x, sd = disEpiODE:::half_normal_sd(mean = 0.463047),
#                          mean = 0.463047),
#                  xmin = 0, xmax = Inf)
#



ggplot(tibble(x=c(0,10))) +
  aes(x) +
  stat_function(fun = \(x) exp(-x), aes(color = "exp")) +
  # stat_function(fun = \(x) 1/sqrt(x), aes(color = "sqrt")) +
  stat_function(fun = \(x)
                disEpiODE:::half_normal_kernel(x) /
                  disEpiODE:::half_normal_kernel(0),
                aes(color = "half-normal")) +

  # stat_function(fun = \(x)
  #               2 *
  #                 dnorm(x, sd = disEpiODE:::half_normal_sd(mean = 0.463047),
  #                       mean = 0.463047), aes(color = "???")) +
  stat_function(fun = \(x)
                2 *
                  dnorm(x, sd = disEpiODE:::half_normal_sd(mean = 0.6366252),
                        mean = 0), aes(color = "???")) +
  disEpiODE::theme_blank_background()







