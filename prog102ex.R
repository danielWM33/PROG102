gt_threshold <- function(x, threshold) {
  is_gt <- x > threshold
  result <- sum(is_gt)
  return(result)
}

temperatures <- c(18.1, 29.9, 22.3, 25.2)
gt_threshold(temperatures, 25)

# dig in with the debugger
gt_threshold <- function(x, threshold) {
  #trigger debugger
  is_gt <- x > threshold
  browser()
  result <- sum(is_gt)
  return(result)
}

gt_threshold(temperatures, 25)

# changing the parameter values not change orig input
gt_threshold <- function(x, threshold) {
  is_gt <- x > threshold
  result <- sum(is_gt)
  x <- 1
  return(result)
}
gt_threshold(temperatures, 25)


