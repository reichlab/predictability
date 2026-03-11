test_that("return_analogue_preds dispatches to uniform method", {
  y <- c(10, 20, 30, 40, 50)
  result <- suppressWarnings(
    return_analogue_preds(y, h = 1, k = 4, method = "uniform")
  )
  expect_type(result, "list")
  expect_named(result, c("pred", "analogue_indices"))
})

test_that("return_analogue_preds dispatches to distance method", {
  y <- c(10, 20, 30, 40, 50)
  result <- return_analogue_preds(y, h = 1, k = 2, method = "distance", p = 2)
  expect_type(result, "list")
  expect_named(result, c("pred", "analogue_indices"))
})

test_that("return_analogue_preds dispatches to seasonal method", {
  y <- 1:20
  result <- suppressWarnings(
    return_analogue_preds(y, h = 1, k = 3, method = "seasonal",
                          rho = pi / 4, eta = 1)
  )
  expect_type(result, "list")
  expect_named(result, c("pred", "analogue_indices"))
})

test_that("uniform end-to-end accuracy", {
  ## y = c(10, 20, 30, 40, 50), h = 1, k = 4 (all past obs)
  ## phi_uniform(y) = c(1, 1, 1, 1) (length 4 = t-1)
  ## get_analogues(phi, k=4, h=1): trim to phi[1:4] = c(1,1,1,1)
  ##   order(decreasing=TRUE) with ties: c(1, 2, 3, 4)
  ##   weights: all 0.25
  ## No index adjustment for uniform
  ## pred = y[1+1]*0.25 + y[2+1]*0.25 + y[3+1]*0.25 + y[4+1]*0.25
  ##      = (20 + 30 + 40 + 50) / 4 = 35
  y <- c(10, 20, 30, 40, 50)
  result <- suppressWarnings(
    return_analogue_preds(y, h = 1, k = 4, method = "uniform")
  )
  expect_equal(result$pred, 35)
  expect_equal(result$analogue_indices$topk_indices, c(1, 2, 3, 4))
})

test_that("distance end-to-end accuracy", {
  ## y = c(1, 2, 3, 4, 5), h = 1, k = 2, p = 2, sigma = 1
  ##
  ## phi_distance: xt = c(4,5)
  ##   tprime=2: c(1,2), dist = sqrt(18), phi = exp(-sqrt(18)/2)
  ##   tprime=3: c(2,3), dist = sqrt(8),  phi = exp(-sqrt(8)/2)
  ##   tprime=4: c(3,4), dist = sqrt(2),  phi = exp(-sqrt(2)/2)
  ## similarities = c(exp(-sqrt(18)/2), exp(-sqrt(8)/2), exp(-sqrt(2)/2))
  ##
  ## get_analogues(phi, k=2, h=1): trim to phi[1:3] (h=1 so no trim)
  ##   top 2 by value: index 3 (exp(-sqrt(2)/2)), index 2 (exp(-sqrt(8)/2))
  ##   w1 = exp(-sqrt(2)/2) / (exp(-sqrt(2)/2) + exp(-sqrt(8)/2))
  ##   w2 = exp(-sqrt(8)/2) / (exp(-sqrt(2)/2) + exp(-sqrt(8)/2))
  ##
  ## Index adjustment: topk_indices + p - 1 = c(3, 2) + 1 = c(4, 3)
  ##
  ## pred = y[4 + 1] * w1 + y[3 + 1] * w2 = 5 * w1 + 4 * w2
  y <- c(1, 2, 3, 4, 5)
  result <- return_analogue_preds(y, h = 1, k = 2, method = "distance",
                                  p = 2, sigma = 1)

  phi3 <- exp(-sqrt(2) / 2)
  phi2 <- exp(-sqrt(8) / 2)
  w1 <- phi3 / (phi3 + phi2)
  w2 <- phi2 / (phi3 + phi2)
  expected_pred <- 5 * w1 + 4 * w2

  expect_equal(result$pred, expected_pred)
  expect_equal(result$analogue_indices$topk_indices, c(4, 3))
})

test_that("seasonal end-to-end accuracy", {
  ## y = 1:9, h = 1, k = 2, rho = pi/4, eta = 1
  ## t = 9
  ##
  ## phi_seasonal: for i = 1:8
  ##   exp(-sin(pi/4 * (10 - i))^2 / 2)
  ##   i=1: sin(pi/4*9) = sin(9*pi/4) = sin(pi/4) = sqrt(2)/2, sq=0.5 → exp(-0.25)
  ##   i=2: sin(pi/4*8) = sin(2*pi) = 0  → 1
  ##   i=3: sin(pi/4*7) = sin(7*pi/4) = -sqrt(2)/2, sq=0.5 → exp(-0.25)
  ##   i=4: sin(pi/4*6) = sin(3*pi/2) = -1, sq=1 → exp(-0.5)
  ##   i=5: sin(pi/4*5) = sin(5*pi/4) = -sqrt(2)/2, sq=0.5 → exp(-0.25)
  ##   i=6: sin(pi/4*4) = sin(pi) = 0  → 1
  ##   i=7: sin(pi/4*3) = sin(3*pi/4) = sqrt(2)/2, sq=0.5 → exp(-0.25)
  ##   i=8: sin(pi/4*2) = sin(pi/2) = 1, sq=1 → exp(-0.5)
  ##
  ## get_analogues(phi, k=2, h=1): trim to phi[1:8] (no trim for h=1)
  ##   top 2: indices 2 and 6 (both have value 1)
  ##   weights: 0.5, 0.5
  ##
  ## Seasonal index adjustment: topk_indices - h = c(2, 6) - 1 = c(1, 5)
  ##
  ## pred = y[1 + 1] * 0.5 + y[5 + 1] * 0.5 = y[2]*0.5 + y[6]*0.5 = 2*0.5 + 6*0.5 = 4
  y <- 1:9
  result <- suppressWarnings(
    return_analogue_preds(y, h = 1, k = 2, method = "seasonal",
                          rho = pi / 4, eta = 1)
  )
  expect_equal(result$pred, 4)
  expect_equal(result$analogue_indices$topk_indices, c(1, 5))
})

test_that("distance index adjustment is correct (offset by p-1)", {
  ## With p = 3, the phi vector starts at tprime = p = 3
  ## so raw index 1 in phi corresponds to original index p = 3 in y
  ## adjustment: topk_indices + p - 1
  y <- c(1, 1, 1, 1, 1, 1, 2, 3, 4)
  result <- return_analogue_preds(y, h = 1, k = 1, method = "distance",
                                  p = 3, sigma = 1)
  ## All adjusted indices should be >= p
  expect_true(all(result$analogue_indices$topk_indices >= 3))
})

test_that("seasonal index adjustment can produce invalid indices (known issue)", {
  ## The seasonal method adjusts topk_indices by -h so that the prediction line
  ## y[topk_indices + h] retrieves the correct values. The prediction is correct
  ## (the -h and +h cancel), but the stored topk_indices can be 0 or negative,
  ## which is invalid as R indices. This test documents the issue.
  y <- 1:20
  result <- suppressWarnings(
    return_analogue_preds(y, h = 3, k = 2, method = "seasonal",
                          rho = pi / 4, eta = 1)
  )
  idx <- result$analogue_indices$topk_indices
  ## Prediction is still correct: y[idx + h] gives the right seasonal analogues
  expect_equal(result$pred, mean(y[idx + 3]))
  ## But stored indices can be 0 (not valid R indices)
  expect_true(any(idx < 1))
})
