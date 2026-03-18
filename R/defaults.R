#' Hindcast using cross-validated trend filtering
#'
#' Fits a trend filter to the time series and returns fitted values at the
#' regularization parameter selected by cross-validation (lambda.1se).
#'
#' The `maxsteps` parameter for the solution path scales with series length
#' (`max(2000, 4 * length(y))`) to ensure the path reaches sufficient
#' complexity for long seasonal time series.
#'
#' An optional `transform` argument accepts a scales transform object
#' (e.g., [scales::transform_sqrt()], [scales::transform_boxcox()]).
#' The transform is applied before fitting and inverted before returning
#' fitted values.
#'
#' @param y numeric vector, the observed time series
#' @param ord integer, order of the trend filter (default 2)
#' @param k_cv integer, number of cross-validation folds (default 10)
#' @param transform a scales transform object with `$transform` and `$inverse`
#'   methods (default [scales::transform_identity()])
#'
#' @returns numeric vector of fitted values, same length as `y`
#' @importFrom stats predict
#' @export
hindcast_trendfilter <- function(y, ord = 2, k_cv = 10,
                                 transform = scales::transform_identity()) {
  y_t <- transform$transform(y)
  maxsteps <- max(2000L, 4L * length(y_t))
  tf <- genlasso::trendfilter(y = y_t, ord = ord, maxsteps = maxsteps)
  cv_tf <- genlasso::cv.trendfilter(tf, k = k_cv)
  fitted_t <- as.numeric(predict(tf, lambda = cv_tf$lambda.1se)$fit)
  transform$inverse(fitted_t)
}
