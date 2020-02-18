test_that("GetData() provides proper message and warnings", {
  expect_error(GetData("a"))
})

test_that("GetData() has correct output type", {
  for(i in c("eil51", "ts225", "pr1002", "gr120", "rat195", 
             "Bays29", "Berlin52", "Cho130", "KroA100", 
             "pcb442", "pr76", "gr48", "pma343")){
  expect_is(GetData(i)[[1]], "character")
  expect_true(is.matrix(GetData(i)[[2]]))
  expect_true(tibble::is_tibble(GetData(i)[[3]]))
  expect_is(GetData(i)[[4]], "numeric")
  }
})