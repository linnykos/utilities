# requires both vec1 and vec2 to have the same number of unique clusters
.align_two_membership_vectors <- function(vec1, vec2, override = F){
  if(override & length(unique(vec2)) > length(unique(vec1))){
    len <- length(unique(vec2))
    vec1[1:len] <- 1:len
  }
  stopifnot(length(unique(vec2)) == length(unique(vec1)))
  
  K <- length(unique(vec2))
  tab <- table(vec2, vec1)
  
  permn_list <- .permn(K)
  similarity_vec <- sapply(permn_list, function(x){
    tab2 <- tab
    tab2 <- tab2[,x]
    sum(diag(tab2))/sum(tab2)
  })
  
  idx <- which.max(similarity_vec)
  
  vec1_new <- vec1
  for(i in 1:K){
    vec1_new[which(vec1 == permn_list[[idx]][i])] <- i
  }
  
  vec1_new
}

.confusion_matrix <- function(vec1, vec2){
   vec1_new <- .align_two_membership_vectors(vec1, vec2)
   table(vec1_new, vec2)
}

## https://github.com/cran/combinat/blob/master/R/permn.R
.permn <- function(x, fun = NULL, ...) {
  if(is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x) x <- seq(x)
  n <- length(x)
  nofun <- is.null(fun)
  out <- vector("list", gamma(n + 1))
  p <- ip <- seqn <- 1:n
  d <- rep(-1, n)
  d[1] <- 0
  m <- n + 1
  p <- c(m, p, m)
  i <- 1
  use <-  - c(1, n + 2)
  
  while(m != 1) {
    out[[i]] <- if(nofun) x[p[use]] else fun(x[p[use]], ...)
    i <- i + 1
    m <- n
    chk <- (p[ip + d + 1] > seqn)
    m <- max(seqn[!chk])
    if(m < n)
      d[(m + 1):n] <-  - d[(m + 1):n]
    index1 <- ip[m] + 1
    index2 <- p[index1] <- p[index1 + d[m]]
    p[index1 + d[m]] <- m
    tmp <- ip[index2]
    ip[index2] <- ip[m]
    ip[m] <- tmp
  }
  
  out
}