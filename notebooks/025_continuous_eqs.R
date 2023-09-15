

library(magrittr)
landscape_scale <- 3

space_stepsize <- 1

pracma::meshgrid(
  seq.default(0, landscape_scale, by = space_stepsize)
) -> grid
grid$X
grid$Y

plot_matrix <- . %>% {Matrix::image(Matrix::Matrix(.))}

grid$X %>% Matrix::Matrix() %>% Matrix::image()
grid$Y %>% plot_matrix()

pracma::expm()
# pracma::exp()
Matrix::expm()

space_stepsize**2
grid$X[-1,]

toeplitz2()
