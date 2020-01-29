test_that("coord_to_dist provides proper messages and warnings", {
  expect_error(coord_to_dist(1))
  expect_error(coord_to_dist(data.frame(matrix(1, ncol = 4, nrow = 3))))
  expect_error(coord_to_dist(data.frame(matrix("a", ncol = 3, nrow = 3))))
  expect_error(coord_to_dist(data.frame(matrix(1, ncol = 3, nrow = 2))))
  expect_message(coord_to_dist(data.frame(matrix(1, ncol = 2, nrow = 3))))
})

test_that("coord_to_dist has correct dimensions and output type", {
  expect_true(is.matrix(coord_to_dist(data.frame(matrix(1, ncol = 2, nrow = 3)))))
  expect_true(dim(coord_to_dist(data.frame(matrix(1, ncol = 2, nrow = 3))))[1] == 
                dim(coord_to_dist(data.frame(matrix(1, ncol = 2, nrow = 3))))[2])
})
