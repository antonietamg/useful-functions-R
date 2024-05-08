autocorrelation <- function(x) {
  N <- length(x)
  mu <- mean(x)
  R <- numeric(N)
  
  for (k in 0:(N-1)) {
    num <- sum((x[1:(N-k)] - mu) * (x[(1+k):N] - mu))
    den <- sum((x - mu)^2)
    R[k+1] <- num / den
  }
  
  return(R)
}

# Example usage
time_series <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
result <- autocorrelation(time_series)
print(result)



# Sample time series
time_series <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Compute and plot the autocorrelation
acf_result <- acf(time_series, plot=TRUE, lag.max=NULL)
