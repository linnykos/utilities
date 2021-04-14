.list_to_array <- function(lis){
  stopifnot(all(sapply(lis, is.matrix)))
  tmp <- sapply(lis, dim); stopifnot(length(unique(tmp[1,])) == 1, length(unique(tmp[2,])) == 1)
  
  len <- length(lis)
  arr <- array(NA, dim = c(nrow(lis[[1]]), ncol(lis[[2]]), len))
  for(i in 1:len){
    arr[,,i] <- lis[[i]]
  }
  
  arr
}

.flatten_array <- function(arr){
  stopifnot(is.array(arr))
  n <- dim(arr)[1]; p <- dim(arr)[2]; len <- dim(arr)[3]
  
  mat <- matrix(NA, nrow = n, ncol = p*len)
  for(l in 1:len){
    mat[,((l-1)*p+1):(l*p)] <- arr[,,l]
  }
  
  mat
}