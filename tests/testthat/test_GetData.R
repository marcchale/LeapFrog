test_that("GetData() provides proper message and warnings", {
  expect_error(GetData("a"))
})

test_that("GetData() has correct output type", {
  for(i in 1:13){
  expect_is(GetData(i)[[1]], "matrix")
  expect_true(is.matrix(GetData(i)[[1]]))
  expect_true(is.data.frame(GetData(i)[[2]]))
  expect_true(is.vector(GetData(i)[[3]]))
  expect_is(GetData(i)[[3]], "integer")
  expect_is(GetData(i)[[4]], "integer")
  expect_true(nrow(GetData(i)[[1]]) == length(GetData(i)[[3]]))
  }
})