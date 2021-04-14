context("Test permutation")

## .permn is correct

test_that(".permn works", {
  res <- .permn(5)
  
  expect_true(is.list(res))
  expect_true(all(sapply(res, function(x){all(sort(x) == 1:5)})))
  expect_true(length(res) == 5*4*3*2*1)
})