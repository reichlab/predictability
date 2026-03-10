phi_seasonal <- function(y, h, rho, eta, ...) {
  t <- length(y)

  ## implement gaussian kernel
  ## rho is the frequency of the seasonal cycle
  ## eta is the width of the gaussian kernel
  ## compute similarity between the h-step ahead timepoint and all previous ones
  exp(-sin(rho * (t + h - 1:(t - 1)))^2 / (2 * eta^2))
}


phi_distance <- function(y, p, sigma = 1, ...) {
  ## compute the euclidean distance between
  ## the p-lag vector for the last element of y and every other element of y
  ## there should be t-p elements in the resulting vector:
  ##  no value for phi(t, t)
  ##  no value for phi(t, t') for t'< p

  t <- length(y)
  if (p < 1 | p > t) {
    stop("p must be 1 or greater and must be less than t")
  }

  xt <- y[(t - p + 1):t]

  phis <- numeric(length(y) - p)
  for (tprime in p:(length(y) - 1)) {
    xtprime <- y[(tprime - p + 1):tprime]
    phis[tprime - p + 1] <- sqrt(sum((xt - xtprime)^2))
  }

  exp(-phis / (2 * sigma^2))
}

# phi_distance <- function(t, tprime, y, p) {
#   if(p<1)
#     stop("p must be 1 or greater")
#   # if(length(y)<t | length(y)<tprime)
#   #   stop("y must be a vector of length greater than t and tprime")
#
#   ## for each element of y, compute the euclidean distance between that element and a lagged tprime element
#
#   xt <- y[(t-p+1):t]
#   xtprime <- y[(tprime-p+1):tprime]
#
#   sqrt(sum( (xt-xtprime)^2 ))
# }

phi_uniform <- function(y) {
  rep(1, length(y) - 1)
}


#' Find indices of analogues
#'
#' @param phi vector of similarities. Similarities are presumed to be
#' calculated between observations at timepoint t and i where i is the index
#' of the vector phi.
#' @param k the number of analogues to return.
#' @param h the horizon to predict for
#'
#' @returns a data.frame with three columns.
#'   - topk_indices: the indices of the original phi vector returned
#'   - topk_values: the values of the original phi vector
#'   - topk_weights: the scaled weights (sum to 1) of the phi vector
get_analogues <- function(phi, k, h) {
  ## remove last h-1 indices from phi vector
  ## those entries will not be able to be used for h-step ahead predictions
  phi <- phi[1:(length(phi) - h + 1)] ## (length(phi)-h+1) = t-1-h+1 = t-h

  if (length(phi) < k) {
    stop(paste(
      "length of phi vector",
      length(phi),
      "is less than number of analogues",
      k
    ))
  }

  topk_indices <- order(phi, decreasing = TRUE)[1:k]
  topk_values <- phi[topk_indices]
  if (all(topk_values == 0)) {
    warning("all selected similarity values are zero.")
  }
  if (all(topk_values == 1)) {
    warning("all selected similarity values are one.")
  }
  data.frame(
    topk_indices,
    topk_values,
    topk_weights = topk_values / sum(topk_values)
  )
}


#' Run method of analogues to obtain predictions
#'
#' @param y vector of length t, the observed time series
#' @param h integer horizon for which the prediction is desired
#' @param k integer number of analogues to use
#' @param method which method to use to compute similarity, one of "uniform",
#' "seasonal" or "distance"
#' @param ... other parameters to pass to similarity functions
#'
#' @returns named list
#' @export
return_analogue_preds <- function(y, h, k, method, ...) {
  args <- list(...)
  t <- length(y)

  ## compute similarities depending on specified method
  similarities <- switch(
    method,
    uniform = phi_uniform(y),
    seasonal = phi_seasonal(y, h, ...),
    distance = phi_distance(y, ...)
  )

  ## check that the length of the similarities vector is the right length
  if (method == "uniform" & length(similarities) != t - 1) {
    stop("for uniform similarity, length of similarities vector must be t-1")
  }

  if (method == "seasonal" & length(similarities) != t - 1) {
    stop("for seasonal similarity, length of similarities vector must be t-1")
  }

  if (method == "distance") {
    if (length(similarities) != t - args$p) {
      stop("for uniform similarity, length of similarities vector must be t-p")
    }
  }

  analogue_indices <- get_analogues(phi = similarities, k, h)

  ## adjust indices for distance method
  if (method == "distance") {
    ## since y_1 through y_{p-1} are omitted in the returned similarity vector
    ## we adjust the "topk_indices" by adding p-1 so they match the original indices for y
    analogue_indices$topk_indices <- analogue_indices$topk_indices + args$p - 1
  }

  ## adjust indices for seasonal method
  if (method == "seasonal") {
    ## since similarities are computed relative to the timepoint t+h
    ## adjust topk_indices by subtracting h
    analogue_indices$topk_indices <- analogue_indices$topk_indices - h
  }

  list(
    pred = sum(
      y[analogue_indices$topk_indices + h] * analogue_indices$topk_weights
    ),
    analogue_indices = analogue_indices
  )
}
