# for diag(vec) %*% mat
.mult_vec_mat <- function(vec, mat){
  stopifnot(is.matrix(mat), !is.matrix(vec), length(vec) == nrow(mat))
  vec * mat
}

# for mat %*% diag(vec)
# see https://stackoverflow.com/questions/17080099/fastest-way-to-multiply-matrix-columns-with-vector-elements-in-r
.mult_mat_vec <- function(mat, vec){
  stopifnot(is.matrix(mat), !is.matrix(vec), length(vec) == ncol(mat))
  mat * rep(vec, rep(nrow(mat), length(vec)))
}

.diag_matrix <- function(vec){
  k <- length(vec)
  if(k == 1) {
    matrix(vec, 1, 1)
  } else {
    diag(vec)
  }
}

# See Section D.2.1 in https://arxiv.org/pdf/1711.10467.pdf, aligns mat1 to mat2
.align_two_matrices <- function(mat1, mat2){
  stopifnot(all(dim(mat1) == dim(mat2)))
  
  tmp <- svd(crossprod(mat1, mat2))
  crossprod(tmp$u, tmp$v)
}