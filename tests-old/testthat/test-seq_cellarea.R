test_that("", {
  expect_error(
    seq_cellarea(
      n = 100,
      precision = 0.01,
      min_cellarea = 0.01, max_cellarea = 1
    ),
    info = "cannot provide `precision` and `n` simultanously"
  )
})
