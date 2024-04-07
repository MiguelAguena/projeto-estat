populational_stdev <- function(data) {
  mean <- mean(data)
  square_sum <- sum((data - mean) ^ 2)
  result <- sqrt(square_sum/length(data))
  return(result)
}

sample_stdev <- function(data) {
  mean <- mean(data)
  square_sum <- sum((data - mean) ^ 2)
  result <- sqrt(square_sum / (length(data) - 1))
  return(result)
}

populational_var <- function(data) {
  mean <- mean(data)
  square_sum <- sum((data - mean) ^ 2)
  result <- square_sum/length(data)
  return(result)
}

sample_var <- function(data) {
  mean <- mean(data)
  square_sum <- sum((data - mean) ^ 2)
  result <- square_sum / (length(data) - 1)
  return(result)
}

normal_error <- function(data, alpha, pop_stdev) {
  z <- qnorm(1 - alpha)
  n <- length(data)
  stdev <- pop_stdev
  return (z * stdev/sqrt(n))
}

student_error <- function(data, alpha) {
  n <- length(data)
  t <- qt(1 - alpha, n - 1)
  stdev <- sample_stdev(data)
  return (t * stdev/sqrt(n))
}