test_that("hindcast_trendfilter returns numeric vector same length as input", {
  set.seed(42)
  y <- cumsum(rnorm(50))
  result <- hindcast_trendfilter(y)
  expect_type(result, "double")
  expect_length(result, length(y))
})

test_that("hindcast_trendfilter works with default args", {
  set.seed(42)
  y <- cumsum(rnorm(50))
  ## should not error with defaults (ord = 2, k_cv = 10)
  expect_no_error(hindcast_trendfilter(y))
})

test_that("hindcast_trendfilter accepts custom ord and k_cv", {
  set.seed(42)
  y <- cumsum(rnorm(50))
  result <- hindcast_trendfilter(y, ord = 1, k_cv = 5)
  expect_type(result, "double")
  expect_length(result, length(y))
})

test_that("hindcast_trendfilter fitted values are smoother than input", {
  ## trend filter should smooth out noise
  set.seed(42)
  y <- sin(seq(0, 4 * pi, length.out = 100)) + rnorm(100, sd = 0.5)
  fit <- hindcast_trendfilter(y)
  ## fitted values should have smaller variance of successive differences
  ## (i.e., be smoother) than the raw data
  raw_roughness <- var(diff(y))
  fit_roughness <- var(diff(fit))
  expect_lt(fit_roughness, raw_roughness)
})
