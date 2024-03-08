
hex_coords_arr <-
  array(
    c(-5:5) %>% rep(each = 6 * 2),
    dim = c(6, 2, 11)
  )
hex_coords_arr

hex_each <- asplit(hex_coords_arr, MARGIN = 3)
hex_each
