## Helper: create a small test dataset
make_test_data <- function(n = 30) {
  data.frame(
    date = seq(as.Date("2020-01-01"), by = "week", length.out = n),
    value = sin(seq(0, 4 * pi, length.out = n)) + seq_len(n) * 0.1,
    season = rep(c("2020", "2021"), each = n / 2),
    season_week = rep(seq_len(n / 2), 2)
  )
}

## Helper: a mock MOA function that always predicts 999
mock_moa_fn <- function(y, h, k, ...) {
  list(
    pred = 999,
    analogue_indices = data.frame(
      topk_indices = seq_len(k),
      topk_values = rep(1, k),
      topk_weights = rep(1 / k, k)
    )
  )
}

## Helper: a mock hindcast function that returns the mean
mock_hindcast_fn <- function(y) {
  rep(mean(y), length(y))
}

test_that("run_analogue_simulation accepts new function arguments", {
  d <- make_test_data()
  expect_no_error(
    suppressMessages(
      run_analogue_simulation(
        data = d, outcome_col = "value",
        h_vals = 1, start_idx = 10,
        k_vals_dist = 3, k_val_seas = 3,
        moa_fn = return_analogue_preds,
        moa_params = list(method = "distance", p = 4),
        seas_fn = return_analogue_preds,
        seas_params = list(method = "seasonal", rho = pi / 52, eta = 1 / 10),
        hindcast_fn = hindcast_trendfilter
      )
    )
  )
})

test_that("custom moa_fn is dispatched for distance method", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      moa_fn = mock_moa_fn,
      moa_params = list(),
      seas_fn = return_analogue_preds,
      seas_params = list(method = "seasonal", rho = pi / 52, eta = 1 / 10),
      hindcast_fn = mock_hindcast_fn
    )
  )
  dist_preds <- result |>
    dplyr::filter(model == "moa_distance")
  ## All distance predictions should be 999 from mock
  expect_true(all(dist_preds$predicted == 999))
})

test_that("custom seas_fn is dispatched for seasonal method", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      moa_fn = return_analogue_preds,
      moa_params = list(method = "distance", p = 4),
      seas_fn = mock_moa_fn,
      seas_params = list(),
      hindcast_fn = mock_hindcast_fn
    )
  )
  seas_preds <- result |>
    dplyr::filter(model == "moa_seasonal")
  expect_true(all(seas_preds$predicted == 999))
})

test_that("custom hindcast_fn is called instead of hardcoded trendfilter", {
  d <- make_test_data()
  hindcast_called <- FALSE
  tracking_hindcast <- function(y) {
    hindcast_called <<- TRUE
    rep(mean(y), length(y))
  }
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = tracking_hindcast
    )
  )
  expect_true(hindcast_called)
})

test_that("transform_fn is applied to outcome column before computation", {
  d <- make_test_data()
  ## Use a transform that doubles all values
  ## If applied, predictions from mock should still be 999,
  ## but the target values should be doubled compared to raw data
  result_raw <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      moa_fn = return_analogue_preds,
      moa_params = list(method = "distance", p = 4),
      hindcast_fn = mock_hindcast_fn,
      transform_fn = identity
    )
  )
  result_doubled <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      moa_fn = return_analogue_preds,
      moa_params = list(method = "distance", p = 4),
      hindcast_fn = mock_hindcast_fn,
      transform_fn = function(y) y * 2
    )
  )
  ## Observed values in the doubled run should be 2x the raw observed
  raw_obs <- result_raw |>
    dplyr::filter(model != "hindcast") |>
    dplyr::pull(observed)
  doubled_obs <- result_doubled |>
    dplyr::filter(model != "hindcast") |>
    dplyr::pull(observed)
  expect_equal(doubled_obs, raw_obs * 2)
})

test_that("default args reproduce original behavior", {
  ## With default arguments, the function should run the same as the old version
  d <- make_test_data()
  expect_no_error(
    suppressMessages(
      run_analogue_simulation(
        data = d, outcome_col = "value",
        h_vals = 1, start_idx = 10,
        k_vals_dist = 3, k_val_seas = 3
      )
    )
  )
})
