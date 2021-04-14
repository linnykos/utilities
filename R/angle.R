.cor_vectors <- function(vec1, vec2, tol = 1e-3){
  len1 <- .l2norm(vec1); len2 <- .l2norm(vec2)
  if(len1 <= tol | len2 <= tol) return(NA)
  (vec1 %*%vec2)/(len1*len2)
}

.angle_between_vectors <- function(vec1, vec2){
  cor_val <- .cor_vectors(vec1, vec2)
  if(is.na(cor_val)) return(NA)
  ang <- acos(min(max(cor_val,-1),1))*180/pi
  
  ang
}