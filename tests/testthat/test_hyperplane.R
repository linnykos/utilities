context("Test hyperplane")

## .representation_2d is correct

test_that(".representation_2d has the correct properties", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    n <- 100
    vec1 <- rnorm(n); vec2 <- rnorm(n)
    res <- .representation_2d(vec1, vec2)
    
    lin_comb <- vec1*runif(1)+vec2*runif(1)
    bool1 <- sum(abs(lin_comb - tcrossprod(res$basis_mat)%*%lin_comb)) <= 1e-6
    
    bool2 <- sum(abs(vec1 - res$basis_mat%*%res$rep1)) <= 1e-6
    bool3 <- sum(abs(vec2 - res$basis_mat%*%res$rep2)) <= 1e-6
    
    bool4 <- sum(abs(crossprod(res$basis_mat) - diag(2))) <= 1e-6
    
    
    bool1 & bool2 & bool3 & bool4
  })
  
  expect_true(all(bool_vec))
})

test_that("(Coding) .representation_2d returns gracefully if both vectors are the same", {
  set.seed(10)
  vec <- rnorm(100)
  
  res <- .representation_2d(vec, vec)
  
  expect_true(sum(abs(res$rep1 - res$rep2)) <= 1e-6)
  expect_true(abs(.l2norm(res$basis_mat[,1]) - 1) <= 1e-6)
  expect_true(sum(abs(res$basis_mat[,2])) <= 1e-6)
  
  vec2 <- res$basis_mat %*% res$rep1
  expect_true(sum(abs(vec - vec2)) <= 1e-6)
})

test_that(".representation_2d gives correct representation for orthogonal vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    n <- 100
    vec1 <- rnorm(n); vec2 <- rnorm(n)
    vec2 <- .project_vec2vec(vec2, vec1, orthogonal = T)
    res <- .representation_2d(vec1, vec2)
    
    abs(res$rep1[2]) <= 1e-6 & abs(res$rep2[1]) <= 1e-6
  })
  
  expect_true(all(bool_vec))
})
