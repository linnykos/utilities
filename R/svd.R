.svd_truncated <- function(mat, K, symmetric, rescale,
                           mean_vec = NULL, sd_vec = NULL,
                           K_full_rank){
  if(is.na(K)) K <- min(dim(mat))
  stopifnot(min(dim(mat)) >= K)
  if(K == min(dim(mat))) K_full_rank <- T
  
  if(min(dim(mat)) > 2*(K+2)){
    res <- tryCatch({
      # ask for more singular values than needed to ensure stability
      if(symmetric){
        tmp <- irlba::partial_eigen(mat, n = ifelse(K_full_rank, K, K+2),
                                    center = mean_vec, scale = sd_vec)
        list(u = tmp$vectors, d = tmp$values, v = tmp$vectors)
      } else {
        irlba::irlba(mat, nv = ifelse(K_full_rank, K, K+2),
                     center = mean_vec, scale = sd_vec)
      }
    }, warning = function(e){
      if(!all(is.null(mean_vec)) | !all(is.null(sd_vec))) print("mean_vec or sd_vec not used")
      RSpectra::svds(mat, k = ifelse(K_full_rank, K, K+2))
    }, error = function(e){
      if(!all(is.null(mean_vec)) | !all(is.null(sd_vec))) print("mean_vec or sd_vec not used")
      RSpectra::svds(mat, k = ifelse(K_full_rank, K, K+2))
    })
  } else {
    res <- svd(mat)
  }
  
  res$u <- res$u[,1:K, drop = F]; res$v <- res$v[,1:K, drop = F]; res$d <- res$d[1:K]
  
  # pass row-names and column-names
  if(length(rownames(mat)) != 0) rownames(res$u) <- rownames(mat)
  if(length(colnames(mat)) != 0) rownames(res$v) <- colnames(mat)
  
  # useful only if your application requires only the singular vectors
  # if the number of rows or columns is too large, the singular vectors themselves
  # are often a bit too small numerically
  if(rescale){
    n <- nrow(mat); p <- ncol(mat)
    res$u <- res$u * sqrt(n)
    res$v <- res$v * sqrt(p)
    res$d <- res$d / (sqrt(n)*sqrt(p))
  }
  
  res
}

.check_svd <- function(svd_res, tol = 1e-6){
  idx <- which(svd_res$d > tol)
  if(length(idx) == length(svd_res$d)) return(svd_res)
  
  svd_res$u <- svd_res$u[, idx, drop = F]
  svd_res$v <- svd_res$v[, idx, drop = F]
  svd_res$d <- svd_res$d[idx]
  
  svd_res
}