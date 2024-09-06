## implement Casdagli, 1992, JRSS-B algorithm
## Nick Reich
## Sept 2024


forecast_from_one_time <- function(x_fit, x_i, T, m, tau, k){
  # x_fit is the fitting set part of the time series
  # x_i is the delay vector from the testing set
  # T horizon for which the forecast is desired
  # m is the embedding dimension
  # tau is the delay time
  # k is the number of nearest neighbors

  ## compute distances d_ij of x_i from delay vectors in the fitting set
  n_fit <- length(x_fit)
  first_delay_idx <- (m-1)*tau + 1 ## the smallest index of x that can have a full delay vector
  distances <- rep(NA, n_fit-first_delay_idx+1)
  for(j in first_delay_idx:n_fit){
    x_j <- get_delay_vector(x_fit, j, tau, m)
    distances[j] <- sqrt(sum((x_i - x_j)^2))
  }

  ## order the distances, find the k nearest neighbors
  nn_idx <- order(distances)[1:k]

  ## fit model using least squares

  ## forecast using the model

  ## return the forecast

}

get_delay_vector <- function(x, i, tau, m, verbose = FALSE){
  # x is the time series
  # i is the index for which the delay vector is desired
  # tau is the delay time
  # m is the embedding dimension

  if(i>length(x))
    stop("index of delay vector is greater than the length of the time series")
  if(i - (m-1)*tau <= 0)
    stop("index of delay vector goes to zero or below")
  ## return the delay vector x_i
  delay_idx <- seq.int(from = (i - (m-1)*tau), to = i, by = tau)
  if(verbose)
    print(cat("index of delay vector is", delay_idx))
  return(x[delay_idx])
}
