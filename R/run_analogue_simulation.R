
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
#'
#' @returns named list
#' @import dplyr foreach doParallel parallel
#' @export
#'
run_analogue_simulation <- function(
    data,
    outcome_col,
    h_vals,
    start_idx,
    k_vals_dist,
    k_val_seas
){

  ## TODO: add check that things are sorted on date
  ## TODO: add other checks?

  maxh <- max(h_vals)

  dist_data <- expand.grid(
    pred_date_idx = start_idx:(nrow(data)-maxh),
    h = h_vals,
    k = k_vals_dist,
    method = c("distance"),
    stringsAsFactors = FALSE
  ) |>
    mutate(
      pred = NA
    )

  seas_data <- expand.grid(
    pred_date_idx = start_idx:(nrow(data)-maxh),
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


  # Set up parallel backend
  num_cores <- detectCores() - 1  # Use all but one core
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  # Parallel loop using foreach
  i <- NULL ## needed to define the global variable to avoid check warnings
  preds <- foreach(i = 1:nrow(analogue_sim_data), .combine = 'c') %dopar% {
    idx <- analogue_sim_data$pred_date_idx[i]
    if (is.na(idx) || idx <= 1) return(NA)  # safety check

    result <- predictability::return_analogue_preds(
      y = data[[outcome_col]][1:idx],
      h = analogue_sim_data$h[i],
      k = analogue_sim_data$k[i],
      method = analogue_sim_data$method[i],
      p = 4,
      rho = pi / 52,
      eta = 1 / 10
    )
    result$pred
  }

  # Stop the cluster after work is done
  stopCluster(cl)

  # Assign predictions back to analogue_sim_data
  analogue_sim_data$pred <- preds
  analogue_sim_data$target_date_idx <- analogue_sim_data$pred_date_idx + analogue_sim_data$h

  analogue_sim_data$target <- data[[outcome_col]][analogue_sim_data$target_date_idx]
  analogue_sim_data$sq_error <- (analogue_sim_data$pred - analogue_sim_data$target)^2
  analogue_sim_data$pred_date_season <- data$season[analogue_sim_data$pred_date_idx]
  analogue_sim_data$target_date_season <- data$season[analogue_sim_data$target_date_idx]

  analogue_data_summary <- analogue_sim_data |>
    group_by(.data$pred_date_season, .data$method, .data$k, .data$h) |>
    summarise(mse = mean(.data$sq_error, na.rm = TRUE),
              sstot = mean((.data$target - mean(.data$target))^2)
    ) |>
    group_by(.data$pred_date_season, .data$k, .data$h) |>
    ## scale mse by mse value
    mutate(
      rsq = 1 - .data$mse/.data$sstot
    )

  ## clumsily join k=30 seasonal with each separate distance
  analogue_data_summary_dist <- analogue_data_summary |>
    filter(.data$method == "distance") |>
    select(.data$pred_date_season, .data$k, .data$h, .data$rsq) |>
    rename(rsq_dist = .data$rsq,
           k_dist = .data$k)

  analogue_data_summary_seas <- analogue_data_summary |>
    filter(.data$method == "seasonal") |>
    select(.data$pred_date_season, .data$k, .data$h, .data$rsq) |>
    rename(rsq_seas = .data$rsq,
           k_seas = .data$k)

  rsq_data <- left_join(
    analogue_data_summary_dist,
    analogue_data_summary_seas,
    by = c("pred_date_season", "h")
    )

  return(list(
    analogue_sim_data = analogue_sim_data,
    analogue_data_summary = analogue_data_summary,
    rsq_data = rsq_data
  ))
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
plot_analogue_sim <- function(
    analogue_sim_data,
    k_val_dist,
    squish = FALSE
) {
  p <- analogue_sim_data[["rsq_data"]] |>
    dplyr::filter(.data$k_dist==k_val_dist) |>
    ggplot() +
    geom_point(aes(x=.data$rsq_seas, y=.data$rsq_dist, color=.data$pred_date_season)) +
    geom_abline(slope=1, intercept=0) +
    facet_wrap(.~h)

  if(squish){
    ## limit plot to (0,1) on both axes and squish oob points
    p <- p +
      scale_x_continuous(limits = c(0, 1), oob = scales::squish) +
      scale_y_continuous(limits = c(0, 1), oob = scales::squish)
  }
  print(p)
}
