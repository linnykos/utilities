.construct_circle <- function(center, radius){
  list(center = center, radius = radius)
}

# has range between -pi and pi
.find_radian <- function(circle, point, tol = 1e-6){
  stopifnot(length(point) == 2, length(circle$center) == 2,
            .l2norm(circle$center - point) <= circle$radius+tol)
  
  atan2(point[2]-circle$center[2], point[1]-circle$center[1])
}

.position_from_circle <- function(circle, radian){
  circle$radius *c(cos(radian),  sin(radian)) + circle$center
}
