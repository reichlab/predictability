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

  maxh <- max(h_vals)

  dist_data <- expand.grid(
    pred_date_idx = start_idx:(nrow(data) - maxh),
    h = h_vals,
    k = k_vals_dist,
    method = c("distance"),
    stringsAsFactors = FALSE
  ) |>
    mutate(
      pred = NA
    )

  seas_data <- expand.grid(
    pred_date_idx = start_idx:(nrow(data) - maxh),
    h = h_vals,
    k = k_val_seas,
    method = c("seasonal"),
    stringsAsFactors = FALSE
  ) |>
    mutate(
      pred = NA
    )

  analogue_sim_data <- bind_rows(
    dist_data,
    seas_data
  )

  ## Add marginal model rows if marginal_fn is provided
  if (!is.null(marginal_fn) && !is.null(k_val_marginal)) {
    marginal_data <- expand.grid(
      pred_date_idx = start_idx:(nrow(data) - maxh),
      h = h_vals,
      k = k_val_marginal,
      method = c("marginal"),
      stringsAsFactors = FALSE
    ) |>
      mutate(
        pred = NA
      )
    analogue_sim_data <- bind_rows(analogue_sim_data, marginal_data)
  }

  # Set up parallel backend
  num_cores <- detectCores() - 1 # Use all but one core
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  # Export custom functions to workers
  fn_env <- environment()
  clusterExport(cl, c("moa_fn", "moa_params", "seas_fn", "seas_params",
                       "marginal_fn", "marginal_params"),
                envir = fn_env)

  # run simulations in parallel, one iteration is one analogues calculation
  # Parallel loop using foreach
  message("running analogue simulation...")
  i <- NULL ## needed to define the global variable to avoid check warnings
  preds <- foreach(i = 1:nrow(analogue_sim_data), .combine = 'c') %dopar%
    {
      idx <- analogue_sim_data$pred_date_idx[i]
      if (is.na(idx) || idx <= 1) {
        return(NA)
      } # safety check

      y_sub <- data[[outcome_col]][1:idx]
      h_i <- analogue_sim_data$h[i]
      k_i <- analogue_sim_data$k[i]
      method_i <- analogue_sim_data$method[i]

      result <- if (method_i == "distance") {
        do.call(moa_fn, c(list(y = y_sub, h = h_i, k = k_i), moa_params))
      } else if (method_i == "seasonal") {
        do.call(seas_fn, c(list(y = y_sub, h = h_i, k = k_i), seas_params))
      } else if (method_i == "marginal") {
        do.call(marginal_fn, c(list(y = y_sub, h = h_i, k = k_i), marginal_params))
      }

      result$pred
    }

  # Stop the cluster after work is done
  stopCluster(cl)

  # Build analogue results into long-format data.frame
  analogue_sim_data$pred <- preds
  analogue_sim_data$target_date_idx <- analogue_sim_data$pred_date_idx +
    analogue_sim_data$h

  model_labels <- c(distance = "moa_distance", seasonal = "moa_seasonal",
                    marginal = "marginal")

  analogue_results <- data.frame(
    model = model_labels[analogue_sim_data$method],
    observed = data[[outcome_col]][analogue_sim_data$target_date_idx],
    predicted = analogue_sim_data$pred,
    horizon = analogue_sim_data$h,
    forecast_date = data$date[analogue_sim_data$pred_date_idx],
    target_end_date = data$date[analogue_sim_data$target_date_idx],
    season = data$season[analogue_sim_data$pred_date_idx],
    season_week = data$season_week[analogue_sim_data$pred_date_idx],
    k = analogue_sim_data$k,
    stringsAsFactors = FALSE
  )

  # Compute hindcast using the provided hindcast function
  message("computing hindcasts...")
  hindcast_fitted <- hindcast_fn(data[[outcome_col]])

  hindcast_results <- data.frame(
    model = "hindcast",
    observed = data[[outcome_col]],
    predicted = hindcast_fitted,
    horizon = 0L,
    forecast_date = data$date,
    target_end_date = data$date,
    season = data$season,
    season_week = data$season_week,
    k = NA_integer_,
    stringsAsFactors = FALSE
  )

  rbind(analogue_results, hindcast_results)
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
