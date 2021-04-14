context("Test circle")

## .find_radian is correct

test_that(".find_radian works", {
  circle <- .construct_circle(c(1,1), radius = 2)
  res <- .find_radian(circle, c(3,1))
  expect_true(abs(res) <= 1e-6)
  
  res <- .find_radian(circle, c(-1,1))
  expect_true(abs(res-pi) <= 1e-6)
  
  res <- .find_radian(circle, c(1,3))
  expect_true(abs(res-pi/2) <= 1e-6)
  
  res <- .find_radian(circle, c(1,-1))
  expect_true(abs(res+pi/2) <= 1e-6)
})

test_that(".find_radian has the correct range", {
  circle <- .construct_circle(c(0,0), radius = 1)
  vec <- seq(-pi, pi, length.out = 100)
  bool_vec <- sapply(vec, function(x){
    position <- c(cos(x), sin(x))
    res <- .find_radian(circle, position)
    abs(res - x) <= 1e-6
  })
  
  expect_true(all(bool_vec))
})


#############

## .position_from_circle is correct

test_that(".position_from_circle works", {
  circle <- .construct_circle(c(1,1), radius = 2)
  rad <- .find_radian(circle, c(3,1))
  res <- .position_from_circle(circle, rad)
  expect_true(sum(abs(res - c(3,1))) <= 1e-6)
  
  rad <- .find_radian(circle, c(-1,1))
  res <- .position_from_circle(circle, rad)
  expect_true(sum(abs(res - c(-1,1))) <= 1e-6)
  
  rad <- .find_radian(circle, c(1,3))
  res <- .position_from_circle(circle, rad)
  expect_true(sum(abs(res - c(1,3))) <= 1e-6)
  
  rad <- .find_radian(circle, c(1,-1))
  res <- .position_from_circle(circle, rad)
  expect_true(sum(abs(res - c(1,-1))) <= 1e-6)
})