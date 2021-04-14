context("Test orthogonalization")

############################

## .orthogonal_vec2vec is correct

test_that(".project_vec2vec works", {
  set.seed(10)
  vec1 <- rnorm(10); vec2 <- rnorm(10)
  res <- .project_vec2vec(vec1, vec2, orthogonal = T)
  
  expect_true(is.numeric(res))
  expect_true(!is.matrix(res))
  expect_true(length(res) == 10)
})

test_that(".project_vec2vec works with vec2 has norm 0", {
  set.seed(10)
  vec1 <- rnorm(10); vec2 <- rep(0, 10)
  expect_warning(res <- .project_vec2vec(vec1, vec2, orthogonal = T))
  expect_true(sum(abs(res - vec1)) <= 1e-6)
  
  expect_warning(res <- .project_vec2vec(vec1, vec2, orthogonal = F))
  expect_true(sum(abs(res)) <= 1e-6)
})

test_that(".project_vec2vec preserves orthogonal vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    vec1 <- rnorm(10); vec2 <- rnorm(10)
    res <- .project_vec2vec(vec1, vec2, orthogonal = T)
    res2 <- .project_vec2vec(res, vec2, orthogonal = T)
    
    bool1 <- sum(abs(res - res2)) < 1e-6
    
    res <- .project_vec2vec(vec1, vec2, orthogonal = F)
    res2 <- .project_vec2vec(res, vec2, orthogonal = F)
    
    bool2 <- sum(abs(res - res2)) < 1e-6
    
    bool1 & bool2
  })

  expect_true(all(bool_vec))
})

test_that(".project_vec2vec makes outputs that are orthogonal", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    vec1 <- rnorm(10); vec2 <- rnorm(10)
    res1 <- .project_vec2vec(vec1, vec2, orthogonal = T)
    res2 <- .project_vec2vec(vec1, vec2, orthogonal = F)
    
    bool1 <- abs(res1 %*% vec2) < 1e-6 & abs(res1 %*% res2) < 1e-6
    bool2 <- abs(sum(res1 + res2 - vec1)) < 1e-6
    
    bool1 & bool2
  })
  
  expect_true(all(bool_vec))
})

test_that(".project_vec2vec removes parallel vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    vec1 <- rnorm(10); vec2 <- 2*vec1
    res <- .project_vec2vec(vec1, vec2, orthogonal = T)
    
    sum(abs(res)) < 1e-6
  })
  
  expect_true(all(bool_vec))
})

test_that(".project_vec2vec reduces the norm", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(1)
    vec1 <- rnorm(10); vec2 <- rnorm(10)
    res <- .project_vec2vec(vec1, vec2, orthogonal = T)
    bool1 <- .l2norm(vec1) >= .l2norm(res)
    
    res <- .project_vec2vec(vec1, vec2, orthogonal = F)
    bool2 <- .l2norm(vec1) >= .l2norm(res)
    
    bool1 & bool2
  })

  expect_true(all(bool_vec))
})


################################

## .project_vec2mat is correct
test_that(".project_vec2mat reduces the norm", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    mat <- matrix(rnorm(30), nrow = 10, ncol = 3)
    vec <- rnorm(10)
    res <- .project_vec2mat(vec, mat, row = F, orthogonal = T)
    bool1 <- .l2norm(vec) >= .l2norm(res)
    
    res <- .project_vec2mat(vec, mat, row = F, orthogonal = F)
    bool2 <- .l2norm(vec) >= .l2norm(res)
    
    mat <- matrix(rnorm(30), nrow = 3, ncol = 10)
    vec <- rnorm(10)
    res <- .project_vec2mat(vec, mat, row = T, orthogonal = T)
    bool3 <- .l2norm(vec) >= .l2norm(res)
    
    res <- .project_vec2mat(vec, mat, row = T, orthogonal = F)
    bool4 <- .l2norm(vec) >= .l2norm(res)
    
    bool1 & bool2 & bool3 & bool4
  })
  
  expect_true(all(bool_vec))
})

test_that(".project_vec2mat preserves orthogonal vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    mat <- matrix(rnorm(30), nrow = 10, ncol = 3)
    vec <- rnorm(10)
    res <- .project_vec2mat(vec, mat, row = F, orthogonal = F)
    res2 <- .project_vec2mat(res, mat, row = F, orthogonal = F)
    bool1 <- sum(abs(res - res2)) < 1e-6
    
    res <- .project_vec2mat(vec, mat, row = F, orthogonal = T)
    res2 <- .project_vec2mat(res, mat, row = F, orthogonal = T)
    bool2 <- sum(abs(res - res2)) < 1e-6
    
    mat <- matrix(rnorm(30), nrow = 3, ncol = 10)
    vec <- rnorm(10)
    res <- .project_vec2mat(vec, mat, row = T, orthogonal = F)
    res2 <- .project_vec2mat(res, mat, row = T, orthogonal = F)
    bool3 <- sum(abs(res - res2)) < 1e-6
    
    res <- .project_vec2mat(vec, mat, row = T, orthogonal = T)
    res2 <- .project_vec2mat(res, mat, row = T, orthogonal = T)
    bool4 <- sum(abs(res - res2)) < 1e-6
    
    bool1 & bool2 & bool3 & bool4
  })
  
  expect_true(all(bool_vec))
})

test_that(".project_vec2mat removes parallel vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    mat <- matrix(rnorm(30), nrow = 10, ncol = 3)
    vec <- mat %*% runif(3)
    res <- .project_vec2mat(vec, mat, row = F, orthogonal = T)
    bool1 <- sum(abs(res)) < 1e-6
    
    res <- .project_vec2mat(vec, mat, row = F, orthogonal = F)
    bool2 <- sum(abs(res - vec)) < 1e-6
    
    mat <- matrix(rnorm(30), nrow = 3, ncol = 10)
    vec <- as.numeric(runif(3) %*% mat)
    res <- .project_vec2mat(vec, mat, row = T, orthogonal = T)
    bool3 <- sum(abs(res)) < 1e-6
    
    res <- .project_vec2mat(vec, mat, row = T, orthogonal = F)
    bool4 <- sum(abs(res - vec)) < 1e-6
    
    bool1 & bool2 & bool3 & bool4
  })
    
  expect_true(all(bool_vec))
})

test_that(".project_vec2mat outputs orthogonal vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    mat <- matrix(rnorm(30), nrow = 10, ncol = 3)
    vec <- rnorm(10)
    res1 <- .project_vec2mat(vec, mat, row = F, orthogonal = T)
    res2 <- .project_vec2mat(vec, mat, row = F, orthogonal = F)
    
    bool1 <- sum(abs(res1 %*% res2)) <= 1e-6
    bool2 <- sum(abs(res1 + res2 - vec)) <= 1e-6
    bool3 <- sum(abs(res1 %*% mat)) <= 1e-6
    
    mat <- matrix(rnorm(30), nrow = 3, ncol = 10)
    vec <- rnorm(10)
    res1 <- .project_vec2mat(vec, mat, row = T, orthogonal = T)
    res2 <- .project_vec2mat(vec, mat, row = T, orthogonal = F)
    
    bool4 <- sum(abs(res1 %*% res2)) <= 1e-6
    bool5 <- sum(abs(res1 + res2 - vec)) <= 1e-6
    bool6 <- sum(abs(mat %*% res1)) <= 1e-6
    
    bool1 & bool2 & bool3 & bool4 & bool5 & bool6 
  }) 
  
  expect_true(all(bool_vec))
})


################################

## .project_mat2mat is correct
test_that(".project_mat2mat can handle when mat2 is all 0's", {
  set.seed(10)
  mat1 <- matrix(rnorm(100), 50, 2)
  mat2 <- matrix(0, 50, 3)
  expect_warning(res <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = T))
  expect_true(sum(abs(res - mat1)) <= 1e-6)
  
  expect_warning(res <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = F))
  expect_true(sum(abs(res)) <= 1e-6)
  
  mat1 <- matrix(rnorm(100), 2, 50)
  mat2 <- matrix(0, 3, 50)
  expect_warning(res <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = T))
  expect_true(sum(abs(res - mat1)) <= 1e-6)
  
  expect_warning(res <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = F))
  expect_true(sum(abs(res)) <= 1e-6)
})

test_that(".project_mat2mat reduces the norm", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    mat2 <- matrix(rnorm(100), nrow = 50, ncol = 2)
    mat1 <- matrix(rnorm(150), nrow = 50, ncol = 3)
    res <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = T)
    bool1 <- .l2norm(mat1) >= .l2norm(res)
    
    res <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = F)
    bool2 <- .l2norm(mat1) >= .l2norm(res)
    
    mat2 <- matrix(rnorm(100), nrow = 2, ncol = 50)
    mat1 <- matrix(rnorm(150), nrow = 3, ncol = 50)
    res <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = T)
    bool3 <- .l2norm(mat1) >= .l2norm(res)
    
    res <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = F)
    bool4 <- .l2norm(mat1) >= .l2norm(res)
    
    bool1 & bool2 & bool3 & bool4
  })
  
  expect_true(all(bool_vec))
})

test_that(".project_mat2mat preserves orthogonal vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    mat2 <- matrix(rnorm(100), nrow = 50, ncol = 2)
    mat1 <- matrix(rnorm(150), nrow = 50, ncol = 3)
    res <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = F)
    res2 <- .project_mat2mat(res, mat2, rows = F, orthogonal = F)
    bool1 <- sum(abs(res - res2)) < 1e-6
    
    res <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = T)
    res2 <- .project_mat2mat(res, mat2, rows = F, orthogonal = T)
    bool2 <- sum(abs(res - res2)) < 1e-6
    
    mat2 <- matrix(rnorm(100), nrow = 2, ncol = 50)
    mat1 <- matrix(rnorm(150), nrow = 3, ncol = 50)
    res <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = F)
    res2 <- .project_mat2mat(res, mat2, rows = T, orthogonal = F)
    bool3 <- sum(abs(res - res2)) < 1e-6
    
    res <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = T)
    res2 <- .project_mat2mat(res, mat2, rows = T, orthogonal = T)
    bool4 <- sum(abs(res - res2)) < 1e-6
    
    bool1 & bool2 & bool3 & bool4
  })
  
  expect_true(all(bool_vec))
})

test_that(".project_mat2mat removes parallel vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    mat2 <- matrix(rnorm(100), nrow = 50, ncol = 2)
    mat1 <- cbind(2*mat2, mat2[,1]+mat2[,2])
    res <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = T)
    bool1 <- sum(abs(res)) < 1e-6
    
    res <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = F)
    bool2 <- sum(abs(res - mat1)) < 1e-6
    
    mat2 <- matrix(rnorm(100), nrow = 2, ncol = 50)
    mat1 <- rbind(2*mat2, mat2[1,]+mat2[2,])
    res <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = T)
    bool3 <- sum(abs(res)) < 1e-6
    
    res <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = F)
    bool4 <- sum(abs(res - mat1)) < 1e-6
    
    bool1 & bool2 & bool3 & bool4
  })
  
  expect_true(all(bool_vec))
})

test_that(".project_mat2mat outputs orthogonal vectors", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    mat2 <- matrix(rnorm(100), nrow = 50, ncol = 2)
    mat1 <- matrix(rnorm(150), nrow = 50, ncol = 3)
    res1 <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = T)
    res2 <- .project_mat2mat(mat1, mat2, rows = F, orthogonal = F)
    
    bool1 <- sum(abs(t(res1) %*% res2)) <= 1e-6
    bool2 <- sum(abs(res1 + res2 - mat1)) <= 1e-6
    bool3 <- sum(abs(t(res1) %*% mat2)) <= 1e-6
    
    mat2 <- matrix(rnorm(100), nrow = 2, ncol = 50)
    mat1 <- matrix(rnorm(150), nrow = 3, ncol = 50)
    res1 <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = T)
    res2 <- .project_mat2mat(mat1, mat2, rows = T, orthogonal = F)
    
    bool4 <- sum(abs(res1 %*% t(res2))) <= 1e-6
    bool5 <- sum(abs(res1 + res2 - mat1)) <= 1e-6
    bool6 <- sum(abs(res1 %*% t(mat2))) <= 1e-6
    
    bool1 & bool2 & bool3 & bool4 & bool5 & bool6
  }) 
  
  expect_true(all(bool_vec))
})

######################

## .projection_matrix is correct

test_that(".projection_matrix is correct", {
  trials <- 50
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    n <- 50; p <- 20
    mat <- matrix(runif(n*p), n, p)
    proj_mat <- .projection_matrix(mat, rows = F)
    vec <- mat %*% runif(p)
    vec2 <- proj_mat %*% vec
    bool1 <- sum(abs(vec - vec2)) <= 1e-6
    
    n <- 20; p <- 50
    mat <- matrix(runif(n*p), n, p)
    proj_mat <- .projection_matrix(mat, rows = T)
    vec <- runif(n) %*% mat
    vec2 <- vec %*% proj_mat
    bool2 <- sum(abs(vec - vec2)) <= 1e-6
    
    bool1 & bool2
  })
  
  expect_true(all(bool_vec))
})


