context("Test inverse")

## .inverse is correct

test_that(".inverse computes the correct inverse", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    n <- 20; p <- 10
    mat <- matrix(rnorm(n*p), n, p)
    mat <- crossprod(mat)
    
    res1 <- .inverse(mat)
    res2 <- solve(mat)
    
    bool1 <- sum(abs(mat %*% res1 - diag(p))) <= 1e-6
    bool2 <- sum(abs(res1 %*% mat - diag(p))) <= 1e-6
    bool3 <- sum(abs(res1 - res2)) <= 1e-6
    
    bool1 & bool2 & bool3
  })
  
  expect_true(all(bool_vec))
})

test_that(".inverse handles rank-deficient instances", {
  trials <- 50
  # this test is based on the understanding that pseudoinverses
  # are valid inverses when the vector lies in the columnspace
  # of the matrix
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    n <- 10; p <- 20
    mat <- matrix(rnorm(n*p), n, p)
    mat <- crossprod(mat)
    res <- .inverse(mat)
    vec <- mat %*% runif(p)
    
    vec1 <- mat %*% vec
    vec2 <- res %*% vec1
    bool1 <- sum(abs(vec - vec2)) <= 1e-6
    
    vec_orth <- .project_vec2mat(runif(p), mat, rows = F, 
                                 orthogonal = T)
    vec_total <- vec + vec_orth
    vec1 <- mat %*% vec_total
    vec2 <- res %*% vec1
    bool2 <- sum(abs(vec - vec2)) <= 1e-6
    
    bool1 & bool2
  })
  
  expect_true(all(bool_vec))
})

test_that(".inverse gives a warning if all-0", {
  mat <- matrix(0, 5, 5)
  expect_warning(res <- .inverse(mat))
  expect_true(all(is.na(res)))
  expect_true(all(dim(res) == 5))
})
