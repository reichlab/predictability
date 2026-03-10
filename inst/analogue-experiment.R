library(cdcfluview)
library(ggplot2)
library(foreach)
library(doParallel)

source("R/knn-utils.R")

ma_dat <- cdcfluview::ilinet(region = "state", years = 2010:2019) |>
  filter(region == "Massachusetts") |>
  mutate(
    season = ifelse(
      week < 40,
      paste0(year - 1, "/", year),
      paste0(year, "/", year + 1)
    )
  )


ggplot(ma_dat) +
  geom_path(aes(x = week_start, y = unweighted_ili))

## loop through each week in the dataset, starting with the second season and return the kNN predictions

## end result is a dataframe with columns for
##  method: seasonal, distance and uniform
##  k: values from 1 to 10 by 1, then 15 to 100 by 5
##  h: 1 to 5

start_idx <- 53 ## drop first season
maxh <- 5
maxk <- 5

knn_data <- expand.grid(
  data_idx = start_idx:(nrow(ma_dat) - maxh),
  h = 1:maxh,
  k = seq(1, maxk, by = 2),
  method = c("seasonal", "distance", "uniform"),
  stringsAsFactors = FALSE
) |>
  mutate(
    pred = NA
  )


# Set up parallel backend
num_cores <- detectCores() - 1 # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Parallel loop using foreach
preds <- foreach(i = 1:nrow(knn_data), .combine = 'c') %dopar%
  {
    idx <- knn_data$data_idx[i]
    if (is.na(idx) || idx <= 1) {
      return(NA)
    } # safety check

    result <- return_knn_preds(
      y = ma_dat$unweighted_ili[1:(idx - 1)],
      h = knn_data$h[i],
      k = knn_data$k[i],
      method = knn_data$method[i],
      p = 4,
      rho = pi / 52,
      eta = 1 / 10
    )
    result$pred
  }

# Stop the cluster after work is done
stopCluster(cl)

# Assign predictions back to knn_data
knn_data$pred <- preds

knn_data$unweighted_ili <- ma_dat$unweighted_ili[knn_data$data_idx]
knn_data$sq_error <- (knn_data$pred - knn_data$unweighted_ili)^2
knn_data$season <- ma_dat$season[knn_data$data_idx]

knn_data_summary <- knn_data |>
  group_by(season, method, k, h) |>
  summarise(
    mse = mean(sq_error, na.rm = TRUE),
    sstot = mean((unweighted_ili - mean(unweighted_ili))^2)
  ) |>
  group_by(season, k, h) |>
  ## scale mse by mse value
  mutate(
    scaled_mse = mse / mse[method == "seasonal"],
    rsq = 1 - mse / sstot
  )

rsq_data <- knn_data_summary |>
  select(season, method, rsq, k, h) |>
  tidyr::pivot_wider(
    names_from = method,
    values_from = rsq
  )

rsq_data |>
  filter(k == 5) |>
  ggplot() +
  geom_point(aes(x = seasonal, y = distance, color = season)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(. ~ h)

## plotting scaled seasonal summaries faceted by k
ggplot(knn_data_summary) +
  geom_point(aes(x = season, y = scaled_mse, color = method)) +
  scale_y_log10() +
  facet_wrap(~k)

## plotting scaled seasonal summaries by k, faceted by method and h
knn_data_summary |>
  filter(method != "seasonal") |>
  ggplot(aes(x = k, y = scaled_mse, color = season)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_color_brewer(type = "seq") +
  facet_grid(h ~ method)

## plotting scaled summaries by k, faceted by method and h
knn_data_summary |>
  filter(method == "distance") |>
  ggplot(aes(x = k, y = scaled_mse)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_color_brewer(type = "seq") +
  facet_grid(h ~ method)


## plotting UNscaled summary trends by k, faceted by method and h
knn_data_summary |>
  filter(method == "distance") |>
  ggplot(aes(x = k, y = mse, color = h, group = h)) +
  geom_smooth(se = FALSE)
## --> k ~= 5-10 is enough neighbors

## fix k = 9 and plot by horizon: distance and seasonal mses converge as h increases
knn_data_summary |>
  filter(
    method %in% c("distance", "seasonal"),
    k == 9
  ) |>
  ggplot(aes(x = h, y = mse, color = method)) +
  scale_y_sqrt() +
  geom_point() +
  geom_smooth()


knn_data_summary |>
  filter(method == "distance") |>
  ggplot(aes(x = k, y = scaled_mse, color = h, group = h)) +
  geom_smooth(se = FALSE)

knn_data_summary |>
  filter(method != "uniform") |>
  ggplot(aes(x = k, y = mse, color = h, group = h)) +
  geom_smooth(se = FALSE) +
  facet_grid(~method)


## plotting UNscaled seasonal summaries by k, faceted by method
ggplot(knn_data_summary, aes(x = k, y = mse, color = season)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_log10() +
  scale_color_brewer(type = "seq") +
  facet_grid(~method)


ggplot(knn_data_summary) +
  geom_point(aes(x = season, y = mse, color = k)) +
  scale_y_log10() +
  scale_color_gradient() +
  facet_wrap(~method)

####
## experiment with better seasonal calculations
###

start_idx <- 105 ## drop first two seasons
maxh <- 6
maxk <- 15

dist_data <- expand.grid(
  pred_date_idx = start_idx:(nrow(ma_dat) - maxh),
  h = 1:maxh,
  k = seq(1, maxk, by = 2),
  method = c("distance"),
  stringsAsFactors = FALSE
) |>
  mutate(
    pred = NA
  )

seas_data <- expand.grid(
  pred_date_idx = start_idx:(nrow(ma_dat) - maxh),
  h = 1:maxh,
  k = 30,
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
num_cores <- detectCores() - 1 # Use all but one core
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Parallel loop using foreach
preds <- foreach(i = 1:nrow(analogue_sim_data), .combine = 'c') %dopar%
  {
    idx <- analogue_sim_data$pred_date_idx[i]
    if (is.na(idx) || idx <= 1) {
      return(NA)
    } # safety check

    result <- return_analogue_preds(
      y = ma_dat$unweighted_ili[1:idx],
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
analogue_sim_data$target_date_idx <- analogue_sim_data$pred_date_idx +
  analogue_sim_data$h
analogue_sim_data$season_week <- ma_dat$season_week[
  analogue_sim_data$pred_date_idx
]

analogue_sim_data$target_unweighted_ili <- ma_dat$unweighted_ili[
  analogue_sim_data$target_date_idx
]
analogue_sim_data$sq_error <- (analogue_sim_data$pred -
  analogue_sim_data$target_unweighted_ili)^2
analogue_sim_data$pred_date_season <- ma_dat$season[
  analogue_sim_data$pred_date_idx
]
analogue_sim_data$target_date_season <- ma_dat$season[
  analogue_sim_data$target_date_idx
]

analogue_data_summary <- analogue_sim_data |>
  group_by(pred_date_season, method, k, h) |>
  summarise(
    mse = mean(sq_error, na.rm = TRUE),
    sstot = mean((unweighted_ili - mean(unweighted_ili))^2),
    total_weeks = n()
  ) |>
  group_by(pred_date_season, k, h) |>
  ## scale mse by mse value
  mutate(
    rsq = 1 - mse / sstot
  )

## clumsily join k=30 seasonal with each separate distance
rsq_data <- analogue_data_summary |>
  select(pred_date_season, method, rsq, k, h) |>
  filter(method == "distance") |>
  left_join(
    filter(analogue_data_summary, method == "seasonal"),
    by = c("pred_date_season", "h")
  )

rsq_data |>
  filter(k.x == 9) |>
  ggplot() +
  geom_point(aes(x = rsq.y, y = rsq.x, color = pred_date_season)) +
  #coord_cartesian(xlim = c(0,1), ylim=c(0,1)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(. ~ h)

analogue_sim_data |>
  filter(method == "seasonal") |>
  ggplot() +
  geom_line(aes(x = season_week, y = sq_error, color = factor(h))) +
  facet_wrap(. ~ pred_date_season)

tmp <- run_analogue_simulation(
  data = ma_dat,
  start_idx = 105,
  outcome_col = "unweighted_ili",
  h_vals = 1:6,
  k_val_seas = 30,
  k_vals_dist = seq(1, 15, by = 2)
)
