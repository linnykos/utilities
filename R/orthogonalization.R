#' Projection of vector onto another vector
#'
#' Returns the component of \code{vec1} that is orthogonal to \code{vec2}
#' if \code{orthogonal} is \code{TRUE}, and the
#' component of \code{vec1} that is parallel to \code{vec2} if 
#' \code{orthogonal} is \code{FALSE}.
#'
#' @param vec1 vector
#' @param vec2 vector
#' @param orthogonal boolean
#'
#' @return vector
.project_vec2vec <- function(vec1, vec2, orthogonal, tol = 1e-6){
  stopifnot(length(vec1) == length(vec2), !is.matrix(vec1), !is.matrix(vec2),
            all(!is.na(vec1)), all(!is.na(vec2)))
  
  d <- length(vec1)
  val <- .l2norm(vec2)
  if(val < tol){
    warning("When projecting, vec2 has length too close to 0")
    if(orthogonal){
      return(vec1)
    } else {
      return(rep(0, length(vec1)))
    }
  }
  vec2 <- vec2/val
  if(orthogonal){
    vec1 - as.numeric(vec2 %*% crossprod(vec2, vec1))
  } else {
    as.numeric(vec2 %*% crossprod(vec2, vec1))
  }
}

#' Projection of vector onto rows/columns of a matrix
#'
#' If \code{rows=FALSE}, returns the component of 
#' \code{vec} that is orthogonal to all the columns of \code{mat}
#'
#' @param vec vector
#' @param mat matrix
#' @param rows boolean
#' @param orthogonal boolean
#'
#' @return vector
.project_vec2mat <- function(vec, mat, rows, orthogonal){
  stopifnot(all(!is.na(vec)), all(!is.na(mat)))
  
  if(rows){
    stopifnot(ncol(mat) == length(vec))
  } else {
    stopifnot(nrow(mat) == length(vec))
  }
  
  proj_mat <- .projection_matrix(mat, rows = rows)
  if(all(is.na(proj_mat))){
    if(orthogonal) return(vec) else return(vector(0, length = length(vec)))
  }
  
  if(orthogonal){
    vec - as.numeric(proj_mat %*% vec)
  } else {
    as.numeric(proj_mat %*% vec)
  }
}

.project_mat2mat <- function(mat1, mat2, rows, orthogonal){
  stopifnot(all(!is.na(mat1)), all(!is.na(mat2)))
  
  if(rows){
    stopifnot(ncol(mat1) == ncol(mat2))
  } else {
    stopifnot(nrow(mat1) == nrow(mat2))
  }
  
  proj_mat <- .projection_matrix(mat2, rows = rows)
  if(all(is.na(proj_mat))){
    if(orthogonal) return(mat1) else return(matrix(0, nrow = nrow(mat1), ncol = ncol(mat1)))
  }
  
  if(rows){
    if(orthogonal){
      mat1 - mat1 %*% proj_mat
    } else {
      mat1 %*% proj_mat
    }
  } else {
    if(orthogonal){
      mat1 - proj_mat %*% mat1
    } else {
      proj_mat %*% mat1
    }
  }
}

.orthogonalize <- function(mat){
  stopifnot(all(!is.na(mat)))
  
  if(ncol(mat) == 1) return(mat)
  
  mat2 <- mat
  for(j in 2:ncol(mat)){
    mat2[,j] <- .project_vec2mat(mat[,j], mat[,1:(j-1),drop = F], 
                                 rows = F, orthogonal = T)
  }
  
  mat2
}

#' Construct projection matrix
#'
#' @param mat matrix
#' @param rows boolean. If \code{TRUE}, project onto the rows of \code{mat}.
#' If \code{FALSE}, project onto the columns of \code{mat}.
#'
#' @return
#' @export
#'
#' @examples
.projection_matrix <- function(mat, rows){
  stopifnot(all(!is.na(mat)))
  
  if(rows){
    stopifnot(ncol(mat) >= nrow(mat))
    crossprod(mat, .inverse(tcrossprod(mat))) %*% mat
  } else {
    stopifnot(ncol(mat) <= nrow(mat))
    mat %*% tcrossprod(.inverse(crossprod(mat)), mat)
  }
}