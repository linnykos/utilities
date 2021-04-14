.l2norm <- function(vec){sqrt(sum(vec^2))}

.opnorm <- function(mat){.svd_truncated(mat, K = 1)$d[1]}