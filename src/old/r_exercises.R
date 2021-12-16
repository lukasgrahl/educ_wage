library(tidyverse)


arr <- rnorm(10000, 0)
hist(arr, breaks = "FD")

my_func <- function(arg1){
  return(arg1)
}

my_func(1)

bootstrap_func <- function(arr, n_trials = 100, sample_size = 0.1){
  
  df <- data.frame()
  output <- c()
  sample_size <- length(arr) * sample_size
  sample_size <- ceiling(sample_size)
  print(sample_size)
  
  for (item in 0 : n_trials){
    x <- sample(arr, size = sample_size)
    hist(x, breaks = "FD")
    output[item] <- mean(x)
    df$item <- x
  }
  return(df)
}

x <- bootstrap_func(arr)
