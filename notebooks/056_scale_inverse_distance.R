

#' We wish to scale the inverse distance weight `1 / (1 + d)`, by scaling the
#' distances, such that they are comparable to `exp(-d)`.
#'
#' This is to preserve the property that both kernels has to be K(0) = 1

plot.new()
curve(exp(-x), to = 10)
scale_d <- .6
curve(1 / (1 + x / scale_d), to = 10, add = TRUE)


optimize(
  function(scale_d) {
    # if (scale_d <= 0) {
    #   Inf
    # }
    x <- seq.default(0, to = 10, length.out = 100)
    df <- exp(-x) - (1 / (1 + x / scale_d))
    mean(abs(df))
    # sqrt(sum(df**2))
  },
  interval = c(0, 10000),
  maximum = FALSE
)


# 0.4049956, using  sqrt(sum(df**2))
# 0.2700693, using mean(abs(df))

# plot.new()
curve(exp(-x), to = 10)
scale_d <-  0.4049956
curve(1 / (1 + x / scale_d), to = 10, add = TRUE)


# plot.new()
curve(exp(-x), to = 10)
scale_d <- 0.2700693
curve(1 / (1 + x / scale_d), to = 10, add = TRUE)
