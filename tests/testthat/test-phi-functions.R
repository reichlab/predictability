## Tests for phi_uniform

test_that("phi_uniform returns vector of all 1s with length t-1", {
  y <- c(10, 20, 30, 40, 50)
  result <- phi_uniform(y)
  expect_equal(result, rep(1, 4))
  expect_length(result, length(y) - 1)
})

test_that("phi_uniform works for length-2 input", {
  expect_equal(phi_uniform(c(1, 2)), 1)
})

## Tests for phi_distance

test_that("phi_distance returns vector of length t-p", {
  y <- c(1, 2, 3, 4, 5)
  result <- phi_distance(y, p = 2)
  expect_length(result, length(y) - 2)

  result3 <- phi_distance(y, p = 3)
  expect_length(result3, length(y) - 3)
})

test_that("phi_distance values are in (0, 1]", {
  y <- c(1, 2, 3, 4, 5)
  result <- phi_distance(y, p = 2)
  expect_true(all(result > 0))
  expect_true(all(result <= 1))
})

test_that("phi_distance gives similarity 1 for identical trailing windows", {
  ## y = c(3, 4, 3, 4), p = 2: xt = c(3,4), window at tprime=2 is c(3,4)
  ## distance = 0, so exp(0) = 1
  y <- c(3, 4, 3, 4)
  result <- phi_distance(y, p = 2)
  expect_equal(result[1], 1)
})

test_that("phi_distance errors when p > t or p < 1", {
  y <- c(1, 2, 3)
  expect_error(phi_distance(y, p = 0))
  expect_error(phi_distance(y, p = 4))
})

test_that("phi_distance accuracy: hand-computed values", {
  ## y = c(1,2,3,4,5), p = 2, sigma = 1
  ## xt = c(4, 5)
  ## tprime=2: xtprime = c(1,2), dist = sqrt((4-1)^2 + (5-2)^2) = sqrt(18)
  ## tprime=3: xtprime = c(2,3), dist = sqrt((4-2)^2 + (5-3)^2) = sqrt(8)
  ## tprime=4: xtprime = c(3,4), dist = sqrt((4-3)^2 + (5-4)^2) = sqrt(2)
  ## result = exp(-c(sqrt(18), sqrt(8), sqrt(2)) / (2 * 1^2))
  y <- c(1, 2, 3, 4, 5)
  result <- phi_distance(y, p = 2, sigma = 1)
  expected <- exp(-c(sqrt(18), sqrt(8), sqrt(2)) / 2)
  expect_equal(result, expected)
})

## Tests for phi_seasonal

test_that("phi_seasonal returns vector of length t-1", {
  y <- 1:10
  result <- phi_seasonal(y, h = 1, rho = pi / 52, eta = 1 / 10)
  expect_length(result, length(y) - 1)
})

test_that("phi_seasonal values are in (0, 1]", {
  y <- 1:20
  result <- phi_seasonal(y, h = 1, rho = pi / 52, eta = 1 / 10)
  expect_true(all(result > 0))
  expect_true(all(result <= 1))
})

test_that("phi_seasonal peaks at seasonal multiples", {
  ## With rho = pi/4 (period = 8), similarities should peak where
  ## (t+h - i) is a multiple of 4 (since sin(pi/4 * 4n) = sin(n*pi) = 0)
  ## For t=9, h=1: t+h = 10, so peaks where (10 - i) mod 4 == 0, i.e. i = 2, 6
  y <- 1:9
  result <- phi_seasonal(y, h = 1, rho = pi / 4, eta = 1)
  ## i=2 and i=6 should have similarity 1 (sin = 0 → exp(0) = 1)
  expect_equal(result[2], 1)
  expect_equal(result[6], 1)
})

test_that("phi_seasonal accuracy: hand-computed values", {
  ## y = 1:5, h = 1, rho = pi/4, eta = 1
  ## t = 5, computes for i = 1:(t-1) = 1:4
  ## exp(-sin(rho * (t + h - i))^2 / (2 * eta^2))
  ## i=1: sin(pi/4 * 5) = sin(5*pi/4) = -sqrt(2)/2, sq = 0.5, exp(-0.5/2) = exp(-0.25)
  ## i=2: sin(pi/4 * 4) = sin(pi) = 0, sq = 0, exp(0) = 1
  ## i=3: sin(pi/4 * 3) = sin(3*pi/4) = sqrt(2)/2, sq = 0.5, exp(-0.25)
  ## i=4: sin(pi/4 * 2) = sin(pi/2) = 1, sq = 1, exp(-1/2) = exp(-0.5)
  y <- 1:5
  result <- phi_seasonal(y, h = 1, rho = pi / 4, eta = 1)
  expected <- c(exp(-0.25), 1, exp(-0.25), exp(-0.5))
  expect_equal(result, expected)
})
