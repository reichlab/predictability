#' Run simulation using method of analogues to determine predictability
#'
#' @param data data.frame with the data. This function assumes that this object
#' represents a set of time-series observations for a single unit/location. The
#' data must have:
#'   - a column named `outcome_col` with the outcome in it
#'   - a column named `date` with a YYYY-MM-DD value that can be interpreted as the date of the observation
#'   - a column named `season_week` with the week of the season that the observation was made in
#'   - a column named `season` with a label for the season that the observation was made in
#' @param outcome_col string with the column name that contains the outcome variable
#' @param h_vals vector of integer values for horizons
#' @param start_idx integer value for the first row in the dataset to compute
#' predictions for
#' @param k_vals_dist vector of integer values for the number of nearest
#' neighbours to use in distance-based analogue simulation
#' @param k_val_seas integer value for the number of nearest neighbours to use
#' in seasonal analogue simulation
#' @param moa_fn function for distance-based MOA predictions
#' @param moa_params list of additional params passed to `moa_fn`
#' @param seas_fn function for seasonal analogue predictions
#' @param seas_params list of additional params passed to `seas_fn`
#' @param marginal_fn function for marginal model predictions (NULL to skip)
#' @param marginal_params list of additional params passed to `marginal_fn`
#' @param k_val_marginal integer number of analogues for marginal model
#' @param hindcast_fn function that takes a numeric vector and returns fitted values
#' @param transform_fn function applied to outcome column before computation
#'
#' @returns A long-format data.frame with columns: `model`, `observed`,
#'   `predicted`, `horizon`, `forecast_date`, `target_end_date`, `season`,
#'   `season_week`, `k`. Compatible with `scoringutils::as_forecast_point()`.
#' @import dplyr foreach doParallel parallel
#' @export
#'
run_analogue_simulation <- function(
  data,
  outcome_col,
  h_vals,
  start_idx,
  k_vals_dist,
  k_val_seas,
  moa_fn      = return_analogue_preds,
  moa_params  = list(method = "distance", p = 4),
  seas_fn     = return_analogue_preds,
  seas_params = list(method = "seasonal", rho = pi / 52, eta = 1 / 10),
  marginal_fn     = NULL,
  marginal_params = list(method = "uniform"),
  k_val_marginal  = NULL,
  hindcast_fn    = hindcast_trendfilter,
  transform_fn   = identity
) {
  ## Verify data is sorted by date (row-position indexing assumes this)
  if (is.unsorted(data$date)) {
    stop("data must be sorted by date in ascending order")
  }

  ## Apply transform to outcome column
  data[[outcome_col]] <- transform_fn(data[[outcome_col]])

  y <- data[[outcome_col]]
  maxh <- max(h_vals)

  ## ---- Part A: MOA distance (origin-dependent, parallel loop) ----

  dist_data <- expand.grid(
    pred_date_idx = start_idx:(nrow(data) - maxh),
    h = h_vals,
    k = k_vals_dist,
    stringsAsFactors = FALSE
  )

  # Set up parallel backend
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  fn_env <- environment()
  clusterExport(cl, c("moa_fn", "moa_params"), envir = fn_env)

  message("running MOA distance simulation...")
  i <- NULL
  dist_preds <- foreach(i = seq_len(nrow(dist_data)), .combine = "c") %dopar%
    {
      idx <- dist_data$pred_date_idx[i]
      if (is.na(idx) || idx <= 1) return(NA)
      y_sub <- y[1:idx]
      result <- do.call(moa_fn, c(
        list(y = y_sub, h = dist_data$h[i], k = dist_data$k[i]),
        moa_params
      ))
      result$pred
    }

  stopCluster(cl)

  dist_data$target_date_idx <- dist_data$pred_date_idx + dist_data$h

  moa_results <- data.frame(
    model = "moa_distance",
    observed = y[dist_data$target_date_idx],
    predicted = dist_preds,
    horizon = dist_data$h,
    forecast_date = data$date[dist_data$pred_date_idx],
    target_end_date = data$date[dist_data$target_date_idx],
    season = data$season[dist_data$pred_date_idx],
    season_week = data$season_week[dist_data$pred_date_idx],
    k = dist_data$k,
    stringsAsFactors = FALSE
  )

  ## ---- Part B: Pre-season baselines (seasonal, marginal) ----
  ## Computed once per season using only data before the season starts.

  ## Identify seasons that overlap with the evaluation window
  eval_start_date <- data$date[start_idx]
  seasons_in_eval <- unique(data$season[data$date >= eval_start_date])

  baseline_rows <- list()

  for (seas_label in seasons_in_eval) {
    seas_mask <- data$season == seas_label
    seas_indices <- which(seas_mask)
    season_start_idx <- min(seas_indices)

    ## Pre-season data: everything before this season starts
    if (season_start_idx <= 1) next
    y_pre <- y[1:(season_start_idx - 1)]
    pre_season_end <- length(y_pre)
    season_start_date <- data$date[season_start_idx]

    ## For each target week in this season, compute baselines
    for (target_idx in seas_indices) {
      h_for_target <- target_idx - pre_season_end

      ## Seasonal baseline
      seas_result <- do.call(seas_fn, c(
        list(y = y_pre, h = h_for_target, k = k_val_seas),
        seas_params
      ))
      baseline_rows[[length(baseline_rows) + 1]] <- data.frame(
        model = "moa_seasonal",
        observed = y[target_idx],
        predicted = seas_result$pred,
        horizon = NA_integer_,
        forecast_date = season_start_date,
        target_end_date = data$date[target_idx],
        season = data$season[target_idx],
        season_week = data$season_week[target_idx],
        k = k_val_seas,
        stringsAsFactors = FALSE
      )

      ## Marginal baseline (if provided)
      if (!is.null(marginal_fn) && !is.null(k_val_marginal)) {
        marg_result <- do.call(marginal_fn, c(
          list(y = y_pre, h = h_for_target, k = k_val_marginal),
          marginal_params
        ))
        baseline_rows[[length(baseline_rows) + 1]] <- data.frame(
          model = "marginal",
          observed = y[target_idx],
          predicted = marg_result$pred,
          horizon = NA_integer_,
          forecast_date = season_start_date,
          target_end_date = data$date[target_idx],
          season = data$season[target_idx],
          season_week = data$season_week[target_idx],
          k = k_val_marginal,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  message("computing baselines...")
  baseline_results <- do.call(rbind, baseline_rows)

  ## ---- Part C: Retrospective hindcast ----

  message("computing hindcasts...")
  hindcast_fitted <- hindcast_fn(y)

  hindcast_results <- data.frame(
    model = "hindcast",
    observed = y,
    predicted = hindcast_fitted,
    horizon = NA_integer_,
    forecast_date = as.Date(NA),
    target_end_date = data$date,
    season = data$season,
    season_week = data$season_week,
    k = NA_integer_,
    stringsAsFactors = FALSE
  )

  ## Combine all results
  rbind(moa_results, baseline_results, hindcast_results)
}


#' Make an R-squared plot
#'
#' @param analogue_sim_data output from run_analogue_sim
#' @param k_val_dist k_val to plot
#' @param squish logical, whether to squish OOB points or not in the plot
#'
#' @export
#' @import ggplot2
#'
#' @note Deprecated. Use scoringutils for scoring and plotting instead.
plot_analogue_sim <- function(
  analogue_sim_data,
  k_val_dist,
  squish = FALSE
) {
  .Deprecated(msg = "plot_analogue_sim is deprecated. Use scoringutils for scoring and plotting.")
}
