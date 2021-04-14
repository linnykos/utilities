#' Compute the inverse of a squarre matrix
#' 
#' If the matrix is rank-deficient, this is computing the pseudoinverse
#' (See https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse)
#' 
#' @param mat square matrix
#' @param tol numeric
#'
#' @return matrix
.inverse <- function(mat, tol = 1e-6){
  stopifnot(all(!is.na(mat)), nrow(mat) == ncol(mat))
  
  svd_res <- .svd_truncated(mat, K = nrow(mat), symmetric = T, 
                            rescale = F, K_full_rank = T)
  
  idx <- which(svd_res$d >= tol)
  if(length(idx) == 0){
    warning("When projecting, mat has rank too close to 0")
    return(matrix(NA, nrow = nrow(mat), ncol = ncol(mat)))
  }
  
  svd_res$u <- svd_res$u[,idx,drop = F]
  svd_res$d <- 1/svd_res$d[idx]
  
  tcrossprod(.mult_mat_vec(svd_res$u, svd_res$d), svd_res$u)
}