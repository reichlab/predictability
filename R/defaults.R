#' Hindcast using cross-validated trend filtering
#'
#' Fits a trend filter to the time series and returns fitted values at the
#' regularization parameter selected by cross-validation (lambda.1se).
#'
#' @param y numeric vector, the observed time series
#' @param ord integer, order of the trend filter (default 2)
#' @param k_cv integer, number of cross-validation folds (default 10)
#'
#' @returns numeric vector of fitted values, same length as `y`
#' @export
hindcast_trendfilter <- function(y, ord = 2, k_cv = 10) {
  tf <- genlasso::trendfilter(y = y, ord = ord)
  cv_tf <- genlasso::cv.trendfilter(tf, k = k_cv)
  as.numeric(predict(tf, lambda = cv_tf$lambda.1se)$fit)
}
