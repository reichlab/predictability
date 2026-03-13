## Helper: create test dataset with multiple seasons
make_test_data <- function(n_seasons = 4, weeks_per_season = 15) {
  n <- n_seasons * weeks_per_season
  data.frame(
    date = seq(as.Date("2019-01-01"), by = "week", length.out = n),
    value = sin(seq(0, n_seasons * 2 * pi, length.out = n)) + seq_len(n) * 0.05,
    season = rep(paste0("S", seq_len(n_seasons)), each = weeks_per_season),
    season_week = rep(seq_len(weeks_per_season), n_seasons)
  )
}

## Helper: mock hindcast
mock_hindcast_fn <- function(y) rep(mean(y), length(y))

## Helper: mock seasonal fn that records the length of y it receives
make_tracking_seas_fn <- function() {
  env <- new.env(parent = emptyenv())
  env$y_lengths <- integer(0)
  fn <- function(y, h, k, ...) {
    env$y_lengths <- c(env$y_lengths, length(y))
    list(
      pred = mean(y),
      analogue_indices = data.frame(
        topk_indices = seq_len(k),
        topk_values = rep(1, k),
        topk_weights = rep(1 / k, k)
      )
    )
  }
  list(fn = fn, env = env)
}

## --- Seasonal baseline tests ---

test_that("seasonal baseline has one row per target_end_date (not per origin)", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1:3, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  seas_rows <- result |> dplyr::filter(model == "moa_seasonal")
  ## Each target_end_date should appear exactly once for seasonal
  target_counts <- table(seas_rows$target_end_date)
  expect_true(all(target_counts == 1))
})

test_that("seasonal baseline forecast_date is the first date of the season", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1:3, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  seas_rows <- result |> dplyr::filter(model == "moa_seasonal")
  ## All seasonal rows should be in season S4 (rows 46-60)
  ## forecast_date should be the first date of S4
  season_start <- d$date[d$season == "S4"][1]
  expect_true(all(seas_rows$forecast_date == season_start))
})

test_that("seasonal baseline has horizon = NA", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1:3, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  seas_rows <- result |> dplyr::filter(model == "moa_seasonal")
  expect_true(all(is.na(seas_rows$horizon)))
})

test_that("seasonal baseline uses only pre-season data", {
  d <- make_test_data()
  tracker <- make_tracking_seas_fn()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      seas_fn = tracker$fn,
      seas_params = list(),
      hindcast_fn = mock_hindcast_fn
    )
  )
  ## Season S4 starts at row 46, so pre-season data is rows 1-45
  ## All calls to seas_fn should receive y of length 45
  expect_true(all(tracker$env$y_lengths == 45))
})

test_that("seasonal baseline covers all weeks in evaluated seasons", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1:3, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  seas_rows <- result |> dplyr::filter(model == "moa_seasonal")
  ## Season S4 has 15 weeks (rows 46-60), all should have predictions
  s4_dates <- d$date[d$season == "S4"]
  expect_true(all(s4_dates %in% seas_rows$target_end_date))
})

## --- Marginal baseline tests ---

test_that("marginal baseline has same structure as seasonal", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1:3, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      marginal_fn = return_analogue_preds,
      marginal_params = list(method = "uniform"),
      k_val_marginal = 10,
      hindcast_fn = mock_hindcast_fn
    )
  )
  marg_rows <- result |> dplyr::filter(model == "marginal")
  ## One row per target_end_date
  target_counts <- table(marg_rows$target_end_date)
  expect_true(all(target_counts == 1))
  ## horizon = NA
  expect_true(all(is.na(marg_rows$horizon)))
  ## forecast_date = season start
  season_start <- d$date[d$season == "S4"][1]
  expect_true(all(marg_rows$forecast_date == season_start))
})

## --- Hindcast tests ---

test_that("hindcast has forecast_date = NA and horizon = NA", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  hc_rows <- result |> dplyr::filter(model == "hindcast")
  expect_true(all(is.na(hc_rows$forecast_date)))
  expect_true(all(is.na(hc_rows$horizon)))
})

test_that("hindcast has one row per date in the full dataset", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  hc_rows <- result |> dplyr::filter(model == "hindcast")
  expect_equal(nrow(hc_rows), nrow(d))
})

## --- MOA distance tests ---

test_that("MOA distance rows have populated forecast_date and horizon", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1:2, start_idx = 50,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  dist_rows <- result |> dplyr::filter(model == "moa_distance")
  expect_false(any(is.na(dist_rows$forecast_date)))
  expect_false(any(is.na(dist_rows$horizon)))
})
