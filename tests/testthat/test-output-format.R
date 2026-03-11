## Helper: create a small test dataset
make_test_data <- function(n = 30) {
  data.frame(
    date = seq(as.Date("2020-01-01"), by = "week", length.out = n),
    value = sin(seq(0, 4 * pi, length.out = n)) + seq_len(n) * 0.1,
    season = rep(c("2020", "2021"), each = n / 2),
    season_week = rep(seq_len(n / 2), 2)
  )
}

## Helper: mock hindcast that avoids slow trendfilter
mock_hindcast_fn <- function(y) rep(mean(y), length(y))

test_that("run_analogue_simulation returns a data.frame", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  expect_s3_class(result, "data.frame")
})

test_that("output has expected columns", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  expected_cols <- c("model", "observed", "predicted", "horizon",
                     "forecast_date", "target_end_date",
                     "season", "season_week", "k")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("model values are from expected set", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  expected_models <- c("moa_distance", "moa_seasonal", "hindcast")
  expect_true(all(result$model %in% expected_models))
})

test_that("marginal model appears when marginal_fn is provided", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      marginal_fn = return_analogue_preds,
      marginal_params = list(method = "uniform"),
      k_val_marginal = 10,
      hindcast_fn = mock_hindcast_fn
    )
  )
  expect_true("marginal" %in% result$model)
})

test_that("row count matches expected for analogue models", {
  d <- make_test_data()
  h_vals <- c(1, 2)
  start_idx <- 25
  k_vals_dist <- c(3, 5)
  k_val_seas <- 3
  maxh <- max(h_vals)
  n_origins <- nrow(d) - maxh - start_idx + 1

  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = h_vals, start_idx = start_idx,
      k_vals_dist = k_vals_dist, k_val_seas = k_val_seas,
      hindcast_fn = mock_hindcast_fn
    )
  )

  dist_rows <- sum(result$model == "moa_distance")
  seas_rows <- sum(result$model == "moa_seasonal")
  hindcast_rows <- sum(result$model == "hindcast")

  expect_equal(dist_rows, n_origins * length(h_vals) * length(k_vals_dist))
  expect_equal(seas_rows, n_origins * length(h_vals) * 1) # single k_val_seas
  expect_equal(hindcast_rows, nrow(d))
})

test_that("no inline scoring columns in output", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  scoring_cols <- c("mse", "sq_error", "rsq", "sstot",
                    "rsq_dist", "rsq_seas", "rsq_hindcast")
  expect_false(any(scoring_cols %in% names(result)))
})

test_that("forecast_date and target_end_date are Date class", {
  d <- make_test_data()
  result <- suppressMessages(
    run_analogue_simulation(
      data = d, outcome_col = "value",
      h_vals = 1, start_idx = 25,
      k_vals_dist = 3, k_val_seas = 3,
      hindcast_fn = mock_hindcast_fn
    )
  )
  expect_s3_class(result$forecast_date, "Date")
  expect_s3_class(result$target_end_date, "Date")
})
