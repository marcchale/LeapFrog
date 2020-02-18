test_that("C2D provides proper messages and warnings", {
  expect_error(C2D(1))
  expect_error(C2D(data.frame(matrix(1, ncol = 4, nrow = 3))))
  expect_error(C2D(data.frame(matrix("a", ncol = 3, nrow = 3))))
  expect_error(C2D(data.frame(matrix(1, ncol = 3, nrow = 2))))
  expect_message(C2D(data.frame(matrix(1, ncol = 2, nrow = 3))))
})

test_that("C2D has correct dimensions and output type", {
  expect_true(is.matrix(C2D(data.frame(matrix(1, ncol = 2, nrow = 3)))))
  expect_true(dim(C2D(data.frame(matrix(1, ncol = 2, nrow = 3))))[1] == 
                dim(C2D(data.frame(matrix(1, ncol = 2, nrow = 3))))[2])
})
