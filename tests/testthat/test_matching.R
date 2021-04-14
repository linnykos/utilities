context("Test matching")

# .matching_idx is correct

test_that(".matching_idx is correct", {
  trials <- 100
  
  bool_vec <- sapply(1:trials, function(x){
    set.seed(x)
    
    vec1 <- c(11:30)[sample(1:20)]
    vec2 <- c(11:30)[sample(1:20)]
    
    res <- .matching_idx(vec1, vec2)
    
    abs(vec1[res] - vec2) == 0
  })
  
  expect_true(all(bool_vec))
})