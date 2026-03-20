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
#' @param lambda_factor numeric multiplier applied to the CV-selected lambda.
#'   Values > 1 produce smoother fits; values < 1 produce rougher fits.
#'   Default 1 (use `lambda.1se` as-is).
#'
#' @returns numeric vector of fitted values, same length as `y`
#' @importFrom stats predict
#' @export
hindcast_trendfilter <- function(y, ord = 2, k_cv = 10,
                                 transform = scales::transform_identity(),
                                 lambda_factor = 1) {
  y_t <- transform$transform(y)
  maxsteps <- max(2000L, 4L * length(y_t))
  tf <- genlasso::trendfilter(y = y_t, ord = ord, maxsteps = maxsteps)
  cv_tf <- genlasso::cv.trendfilter(tf, k = k_cv)
  lambda <- cv_tf$lambda.1se * lambda_factor
  fitted_t <- as.numeric(predict(tf, lambda = lambda)$fit)
  transform$inverse(fitted_t)
}

#' Hindcast using smoothing splines
#'
#' Fits a smoothing spline to the time series with smoothness selected by
#' generalized cross-validation (GCV) or a user-specified `spar` value.
#'
#' An optional `transform` argument accepts a scales transform object
#' (e.g., [scales::transform_sqrt()], [scales::transform_modulus()]).
#' The transform is applied before fitting and inverted before returning
#' fitted values.
#'
#' @param y numeric vector, the observed time series
#' @param spar numeric smoothing parameter, or `NULL` for GCV selection
#'   (default `NULL`). Higher values produce smoother fits; typical range
#'   is 0 to 1.
#' @param transform a scales transform object with `$transform` and `$inverse`
#'   methods (default [scales::transform_identity()])
#'
#' @returns numeric vector of fitted values, same length as `y`
#' @importFrom stats smooth.spline predict
#' @export
hindcast_spline <- function(y, spar = NULL,
                            transform = scales::transform_identity()) {
  y_t <- transform$transform(y)
  idx <- seq_along(y_t)
  fit <- smooth.spline(idx, y_t, spar = spar)
  fitted_t <- predict(fit, idx)$y
  transform$inverse(fitted_t)
}
