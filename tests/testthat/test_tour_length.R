test_that("tour_length provides proper messages and warnings", {
  expect_error(tour_length(distMatrix = data.frame(matrix(1, ncol = 4, nrow = 4)), 
                           tour = c(1,2,3,4)))
  expect_error(tour_length(matrix(1, ncol = 4, nrow = 3), 
                           tour = c(1,2,3,4)))
  expect_error(tour_length(matrix("a", ncol = 4, nrow = 4), 
                           tour = c(1,2,3,4)))
  expect_error(tour_length(matrix(1, ncol = 2, nrow = 2), 
                           tour = c(1,2)))
  expect_error(tour_length(matrix(1, ncol = 4, nrow = 4), 
                           tour = matrix(1, ncol = 4, nrow = 4)))
  expect_error(tour_length(matrix(1, ncol = 4, nrow = 4), 
                           tour = c(1,2,3,"a")))
  expect_error(tour_length(matrix(1, ncol = 4, nrow = 4), 
                           tour = c(1,2,3)))
})

test_that("tour_length has correct dimensions and output type", {
  expect_true(tour_length(matrix(c(0,6,7,10,
                                   6,0,5,7,
                                   7,5,0,6,
                                   10,7,6,0), 
                                 byrow = TRUE, ncol = 4, nrow = 4), 
                          tour = c(1,3,4,2))==26)
})