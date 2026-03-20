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

#' Seasonal climatology using a GAM with cyclic splines
#'
#' Fits a smooth seasonal curve using a cyclic cubic regression spline
#' (`bs = "cc"` in [mgcv::gam()]), with smoothness selected by REML.
#' The cyclic basis ensures continuity at the season boundary (the fitted
#' curve and its first two derivatives match at the cycle endpoints).
#'
#' Uses day-of-year as the cyclic variable (scaled to \[0, 1)) to handle
#' week 53 and leap years naturally. For non-annual periodicities, uses
#' the modulus of the date's numeric value with the specified period.
#'
#' @param y numeric vector of observed values (training data)
#' @param dates Date vector, same length as `y`
#' @param period numeric, cycle length in days. Default 365 for annual
#'   seasonality. Use 182.5 for semi-annual, etc.
#' @param k integer, basis dimension for the cyclic spline (default 20).
#'   Controls maximum wiggliness; actual smoothness is selected by REML.
#' @param transform a scales transform object with `$transform` and
#'   `$inverse` methods (default [scales::transform_identity()])
#' @param newdates Date vector of dates to predict at, or `NULL` to
#'   return fitted values at the training dates (default `NULL`)
#'
#' @returns numeric vector of fitted/predicted seasonal values. Length
#'   equals `length(y)` if `newdates` is `NULL`, otherwise `length(newdates)`.
#' @importFrom stats predict
#' @export
climatology_gam <- function(y, dates, period = 365, k = 20,
                            transform = scales::transform_identity(),
                            newdates = NULL) {
  y_t <- transform$transform(y)

  ## Compute position in cycle scaled to [0, 1)
  cycle_pos <- .dates_to_cycle_pos(dates, period)

  df <- data.frame(y_t = y_t, cycle_pos = cycle_pos)

  fit <- mgcv::gam(
    y_t ~ s(cycle_pos, bs = "cc", k = k),
    data   = df,
    method = "REML",
    knots  = list(cycle_pos = c(0, 1))
  )

  if (is.null(newdates)) {
    fitted_t <- as.numeric(predict(fit, type = "response"))
  } else {
    new_pos <- .dates_to_cycle_pos(newdates, period)
    newdf <- data.frame(cycle_pos = new_pos)
    fitted_t <- as.numeric(predict(fit, newdata = newdf, type = "response"))
  }

  transform$inverse(fitted_t)
}

#' GAM climatology wrapper for use as `seas_fn` in [run_analogue_simulation()]
#'
#' Drop-in replacement for [return_analogue_preds()] with `method = "seasonal"`.
#' Fits a cyclic GAM on the pre-season training data and predicts at the
#' target date (determined by `h` steps ahead of the training window end).
#'
#' **Usage with `run_analogue_simulation()`:**
#' ```
#' run_analogue_simulation(
#'   ...,
#'   seas_fn     = return_climatology_gam_preds,
#'   seas_params = list(dates = data$date, period = 365, k_basis = 20,
#'                      transform = scales::transform_modulus(p = 1/100)),
#'   k_val_seas  = NULL,
#'   ...
#' )
#' ```
#'
#' The full `dates` vector for the entire time series must be passed via
#' `seas_params`. The function subsets it to match `y` (training window)
#' and uses `dates[length(y) + h]` as the target date.
#'
#' @param y numeric vector, pre-season training data (subset of full series)
#' @param h integer, number of steps from end of training data to target week
#' @param k integer, ignored (present for interface compatibility)
#' @param dates Date vector for the **full** time series (not just training).
#'   Passed via `seas_params`.
#' @param period numeric, cycle length in days (default 365)
#' @param k_basis integer, basis dimension for the cyclic spline (default 20)
#' @param transform a scales transform object (default [scales::transform_identity()])
#' @param ... ignored
#'
#' @returns A list with:
#'   \describe{
#'     \item{pred}{numeric(1), predicted value at the target date}
#'     \item{analogue_indices}{data.frame with one row (NA placeholder,
#'       since GAM does not use analogues)}
#'   }
#' @export
return_climatology_gam_preds <- function(y, h, k = NULL, dates,
                                         period = 365, k_basis = 20,
                                         transform = scales::transform_identity(),
                                         ...) {
  n <- length(y)
  dates_train <- dates[1:n]
  target_date <- dates[n + h]

  pred <- climatology_gam(
    y         = y,
    dates     = dates_train,
    period    = period,
    k         = k_basis,
    transform = transform,
    newdates  = target_date
  )

  list(
    pred = pred,
    analogue_indices = data.frame(
      topk_indices = NA_integer_,
      topk_values  = NA_real_,
      topk_weights = 1
    )
  )
}

#' Convert dates to cycle position in \[0, 1)
#'
#' For annual cycles (period = 365), uses day-of-year scaled by the actual
#' year length (365 or 366) to avoid leap-year drift. For other periods,
#' uses the raw date modulo the period.
#'
#' @param dates Date vector
#' @param period numeric, cycle length in days
#' @returns numeric vector in \[0, 1)
#' @keywords internal
.dates_to_cycle_pos <- function(dates, period) {
  if (period == 365) {
    doy <- as.integer(format(dates, "%j"))
    year_length <- ifelse(
      as.integer(format(dates, "%Y")) %% 4 == 0 &
        (as.integer(format(dates, "%Y")) %% 100 != 0 |
         as.integer(format(dates, "%Y")) %% 400 == 0),
      366, 365
    )
    (doy - 1) / year_length
  } else {
    (as.numeric(dates) %% period) / period
  }
}
