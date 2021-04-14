context("Test angle")

## .angle_between_vectors is correct

test_that(".angle_between_vectors returns correct angles", {
  trials <- 50
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    n <- 100
    vec1 <- rnorm(n); vec2 <- rnorm(n)
    vec2 <- .project_vec2vec(vec2, vec1, orthogonal = T)
    vec1 <- vec1/.l2norm(vec1)
    vec2 <- vec2/.l2norm(vec2)
    
    res <- .angle_between_vectors(vec1, vec2)
    bool1 <- abs(res-90) <= 1e-6
    
    res <- .angle_between_vectors(vec1, -vec2)
    bool2 <- abs(res-90) <= 1e-6
    
    res <- .angle_between_vectors(vec1, vec2+vec1)
    bool3 <- abs(res-45) <= 1e-6
    
    res <- .angle_between_vectors(vec1, -(vec2+vec1))
    bool4 <- abs(res-(45+90)) <= 1e-6
    
    res <- .angle_between_vectors(vec1, vec2-vec1)
    bool5 <- abs(res-(45+90)) <= 1e-6
    
    bool1 & bool2 & bool3 & bool4 & bool5
  })
  
  expect_true(all(bool_vec))
})
