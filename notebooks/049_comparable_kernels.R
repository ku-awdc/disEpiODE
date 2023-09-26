

curve(exp(-x), to = 10)
curve(1/ x / 10, to = 10, add = TRUE)
curve(half_normal_kernel(x, mean = 1)/half_normal_kernel(0), add = TRUE)

integrate(\(x) exp(-x), lower = 0, upper = Inf)
integrate(\(x) half_normal_kernel(x), lower = 0, upper = Inf)
integrate(\(x) half_normal_kernel(x) / half_normal_kernel(0),
          lower = 0, upper = Inf)

f <- \(x, mean, location, magn) magn * half_normal_kernel(x, mean = mean, location = location)
optim(par = c(1, 0, 1),
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

curve(
  # f(x, mean = 0.7471085, location = -0.196851,magn = 1.1997776),
  # f(x, mean = 0.7471085, location = -0.1035730,magn = 1.1746191),
  f(x, 1.312475, -1.560466, 3.233037),
  to = 10,
  col = "cornflowerblue"
)
# curve(exp(-x), to = 10, add = TRUE)
# curve(1/ x / 10, to = 10, add = TRUE)
curve(half_normal_kernel(x, mean = 1)/half_normal_kernel(0), add = TRUE,
      col = "green")


