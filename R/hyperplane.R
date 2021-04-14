## given 2 vectors, find the hyperplane 2D representation
.representation_2d <- function(vec1, vec2, tol = 1e-6){
  stopifnot(length(vec1) == length(vec2), .l2norm(vec1) > tol, .l2norm(vec2) > tol)
  
  unit1 <- vec1/.l2norm(vec1)
  tmp <- .project_vec2vec(vec2, unit1, orthogonal = T)
  if(.l2norm(tmp) < tol){
    return(list(basis_mat = cbind(unit1, 0), rep1 = c(.l2norm(vec1), 0), 
                rep2 = c(.l2norm(vec2), 0)))
  }
  unit2 <- tmp/.l2norm(tmp)
  
  basis_mat <- cbind(unit1, unit2)
  rep1 <- c(.l2norm(vec1), 0)
  rep2 <- as.numeric(crossprod(basis_mat, vec2))
  names(rep2) <- NULL
  
  list(basis_mat = basis_mat, rep1 = rep1, rep2 = rep2)
}