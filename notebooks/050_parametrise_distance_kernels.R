

f <- \(x, mean, location, magn) {
  magn * half_normal_kernel(x, mean = mean, location = location)
}
optim(par = c(mean = 1, location = 0, magn = 1),
      fn = \(par) {
        # if (any(is.infinite(par))) {
        #   return(Inf)
        # }

        # mean should be zero...
        abs(integrate(
          \(x, mean, location, magn) x * f(x, mean = mean, location = location, magn = magn),
          lower = 0, upper = Inf,
          mean = par[1],
          # location = 0,
          location = par[2],
          magn = par[3]
        )$value - 1) +
          # K(0) = 1
          abs(
            f(0, mean = par[1], location = par[2], magn = par[3]) - 1
          ) +
          # overall integral must be 1
          abs(
            integrate(
              f,
              lower = 0, upper = Inf,
              mean = par[1],
              location = par[2],
              magn = par[3]
            )$value - 1
          )
      }
      # , control = list(fnscale = -1)
)



f <- \(x, rate) {
  dexp(x, rate = rate)
}
optim(par = c(rate = 1),
      fn = \(par) {
        # if (any(is.infinite(par))) {
        #   return(Inf)
        # }

        # mean should be zero...
        abs(integrate(
          \(x, rate) x * f(x, rate = rate),
          lower = 0, upper = Inf,
          rate = par[1]
        )$value - 1) +
          # K(0) = 1
          abs(
            f(0, rate = par[1]) - 1
          ) +
          # overall integral must be 1
          abs(
            integrate(
              f,
              lower = 0, upper = Inf,
              rate = par[1]
            )$value - 1
          )
      }
      # , control = list(fnscale = -1)
)


# CONCLUSION: Doesn't work.
# f <- \(x, location, scale) {
#   1 / (x + location) / scale
# }
# optim(par = c(location = 1, scale = 1),
#       fn = \(par) {
#         # if (any(is.infinite(par))) {
#         #   return(Inf)
#         # }
#
#         # mean should be zero...
#         abs(integrate(
#           \(x, location, scale) x * f(x, location = location, scale = scale),
#           lower = 0.0001, upper = 100,
#           location = par[1],
#           scale = par[2]
#         )$value - 1) +
#           # K(0) = 1
#           abs(
#             f(0, location = par[1], scale = par[2]) - 1
#           ) +
#           # overall integral must be 1
#           abs(
#             integrate(
#               f,
#               lower = 0.0001, upper = 100,
#               location = par[1],
#               scale = par[2]
#             )$value - 1
#           )
#       }
#       # , control = list(fnscale = -1)
# )
