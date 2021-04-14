context("Test svd")

## .svd_truncated is correct

test_that(".svd_truncated correctly returns the matrix if the matrix is already low-rank", {
  trials <- 100
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    u_mat <- matrix(rnorm(50), 25, 2)
    v_mat <- matrix(rnorm(60), 30, 2)
    dat <- u_mat %*% t(v_mat)
    
    res <- .svd_truncated(dat, K = 2, symmetric = F, rescale = F,
                          K_full_rank = F)
    dat2 <- tcrossprod(.mult_mat_vec(res$u, res$d), res$v)
    
    sum(abs(dat2 - dat)) <= 1e-6
  })
  
  expect_true(all(bool_vec))
})

test_that(".svd_truncated can respect symmetric=T", {
  load("../assets/eigen1.RData")
  res <- .svd_truncated(mat, K = 2, symmetric = F, rescale = F,
                        K_full_rank = F)
  recon1 <- tcrossprod(.mult_mat_vec(res$u, res$d), res$v)
  
  res <- .svd_truncated(mat, K = 2, symmetric = T, rescale = F,
                        K_full_rank = F)
  recon2 <- tcrossprod(.mult_mat_vec(res$u, res$d), res$v)
  
  expect_true(sum(abs(recon2 - t(recon2))) <= sum(abs(recon1 - t(recon1))))
})

test_that(".svd_truncated preserves rownames and colnames", {
  set.seed(1)
  n <- 10
  mat <- matrix(100*runif(n^2), n, n)
  rownames(mat) <- paste0("a", 1:n)
  colnames(mat) <- paste0("b", 1:n)
  
  res <- .svd_truncated(mat, K = 2, symmetric = F, rescale = F,
                        K_full_rank = F)
  
  expect_true(all(rownames(res) == rownames(mat)))
  expect_true(all(colnames(res) == colnames(mat)))
})

test_that(".svd_truncated works on a difficult example", {
  load("../assets/svd1.RData")
  res <- .svd_truncated(mat, K = 2, symmetric = F, rescale = F,
                        K_full_rank = F)
  mat2 <- tcrossprod(.mult_mat_vec(res$u, res$d), res$v)
  
  expect_true(all(dim(mat) == dim(mat2)))
})

test_that(".svd_projection works on another difficult example", {
  load("../assets/svd2.RData")
  expect_true(Matrix::rankMatrix(mat) == 2)
  res <- .svd_truncated(mat, K = 2, symmetric = F, rescale = F,
                        K_full_rank = F)
  mat2 <- tcrossprod(.mult_mat_vec(res$u, res$d), res$v)
  
  expect_true(sum(abs(mat - mat2)) <= 1e-6)
})

test_that(".svd_projection works on another difficult example", {
  load("../assets/svd3.RData")
  expect_true(Matrix::rankMatrix(mat) == 2)
  res <- .svd_truncated(mat, K = 2, symmetric = F, rescale = F,
                        K_full_rank = F)
  
  expect_true(is.list(res))
})