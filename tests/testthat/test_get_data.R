test_that("get_data() provides proper message and warnings", {
  expect_error(get_data("a"))
})

test_that("get_data() has correct output type", {
  for(i in c("eil51", "ts225", "pr1002", "gr120", "rat195", 
             "Bays29", "Berlin52", "Cho130", "KroA100", 
             "pcb442", "pr76", "gr48", "pma343")){
  expect_is(get_data(i)[[1]], "character")
  expect_true(is.matrix(get_data(i)[[2]]))
  expect_true(tibble::is_tibble(get_data(i)[[3]]))
  expect_is(get_data(i)[[4]], "numeric")
  }
})