# from https://github.com/samWieczorek/DAPAR/blob/master/R/utils.R
.nonzero <- function(mat){
  ## function to get a two-column matrix containing the indices of the
  ### non-zero elements in a "dgCMatrix" class matrix
  
  stopifnot(inherits(mat, "dgCMatrix"))
  if (all(mat@p == 0))
    return(matrix(0, nrow=0, ncol=2,
                  dimnames=list(character(0), c("row","col"))))
  res <- cbind(mat@i+1, rep(seq(dim(mat)[2]), diff(mat@p)))
  colnames(res) <- c("row", "col")
  res <- res[mat@x != 0, , drop = FALSE]
  return(res)
}

## see https://www.r-bloggers.com/2020/03/what-is-a-dgcmatrix-object-made-of-sparse-matrix-format-in-r/
# if you want to find the nonzero entries for a row, I suggest
# first transposing via Matrix::t()
.nonzero_col <- function(mat, col_idx){
  stopifnot(inherits(mat, "dgCMatrix"), col_idx %% 1 == 0,
            col_idx > 0, col_idx <= ncol(mat))
  
  val1 <- mat@p[col_idx]
  val2 <- mat@p[col_idx+1]
  
  if(val1 == val2) return(numeric(0))
  mat@i[(val1+1):val2]+1
}

# .pmax_sparse <- function(mat1, mat2){
#   stopifnot(inherits(mat1, "dgCMatrix"), inherits(mat2, "dgCMatrix"))
#   
#   tmp1 <- mat1 + mat2
#   tmp2 <- mat1*mat2
# }

## from https://stackoverflow.com/questions/11832170/element-wise-max-operation-on-sparse-matrices-in-r
## seems to be an error from https://stackoverflow.com/questions/10527072/using-data-table-package-inside-my-own-package
## this means we need to include "data.table" into the "Depends"
.pmax_sparse <- function(..., na.rm = FALSE) {
  
  # check that all matrices have conforming sizes
  num_rows <- unique(sapply(list(...), nrow))
  num_cols <- unique(sapply(list(...), ncol))
  stopifnot(length(num_rows) == 1, length(num_cols) == 1)
  
  cat_summary <- data.table::rbindlist(lapply(list(...), Matrix::summary)) 
  out_summary <- cat_summary[, list(x = max(x)), by = c("i", "j")]
  
  Matrix::sparseMatrix(i = out_summary[,i],
                       j = out_summary[,j],
                       x = out_summary[,x],
                       dims = c(num_rows, num_cols))
}

