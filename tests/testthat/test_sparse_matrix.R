context("Test sparse matrix")

## .nonzero is correct

test_that(".nonzero is correct", {
  trials <- 100
  n <- 100
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    
    vec <- rep(0, n^2)
    vec[sample(1:n^2, round(runif(1)*n^2*0.1), replace = F)] <- 1
    mat <- Matrix::Matrix(vec, nrow = n, ncol = n, sparse = T)
    idx1 <- .nonzero(mat)
    idx2 <- which(as.matrix(mat) != 0, arr.ind = T)
    
    bool1 <- all(dim(idx1) == dim(idx2))
    bool2 <- all(sapply(1:nrow(idx1), function(x){
      mat[idx1[x,1],idx1[x,2]] != 0
    }))
    
    bool1 & bool2
  })
  
  expect_true(all(bool_vec))
})

## .nonzero_col is correct

test_that(".nonzero_col is correct", {
  trials <- 100
  n <- 100
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    
    vec <- rep(0, 2*n^2)
    vec[sample(1:(2*n^2), round(runif(1)*2*n^2*0.1), replace = F)] <- 1
    mat <- Matrix::Matrix(vec, nrow = n, ncol = 2*n, sparse = T)
    col_idx <- sample(1:(2*n), 1)
    idx1 <- .nonzero_col(mat, col_idx)
    idx2 <- which(as.matrix(mat)[,col_idx] != 0)
    
    bool1 <- all(length(idx1) == length(idx2))
    bool2 <- all(as.matrix(mat)[idx1,col_idx] != 0)
    
    bool1 & bool2
  })
  
  expect_true(all(bool_vec))
})

######################

## .pmax_sparse is correct
test_that(".pmax_sparse is correct", {
  trials <- 100
  n <- 100
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    
    idx1 <- sample(1:n^2, round(runif(1)*n^2*0.1), replace = F)
    vec1 <- rep(0, n^2); vec1[idx1] <- stats::runif(length(idx1))
    mat1 <- Matrix::Matrix(vec1, nrow = n, ncol = n, sparse = T)
    
    idx2 <- sample(1:n^2, round(runif(1)*n^2*0.1), replace = F)
    vec2 <- rep(0, n^2); vec2[idx2] <- stats::runif(length(idx2))
    mat2 <- Matrix::Matrix(vec2, nrow = n, ncol = n, sparse = T)
   
    res1 <- .pmax_sparse(mat1, mat2)
    res2 <- pmax(as.matrix(mat1), as.matrix(mat2))
    
    sum(abs(res1 - res2)) <= 1e-6
  })
  
  expect_true(all(bool_vec))
})
