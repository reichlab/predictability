test_that("get_analogues returns data.frame with expected columns", {
  phi <- c(0.1, 0.5, 0.3, 0.9)
  result <- get_analogues(phi, k = 2, h = 1)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("topk_indices", "topk_values", "topk_weights"))
})

test_that("get_analogues weights sum to 1", {
  phi <- c(0.1, 0.5, 0.3, 0.9)
  result <- get_analogues(phi, k = 3, h = 1)
  expect_equal(sum(result$topk_weights), 1)
})

test_that("get_analogues returns the k largest phi values", {
  phi <- c(0.1, 0.5, 0.3, 0.9)
  result <- get_analogues(phi, k = 2, h = 1)
  ## After trimming last h-1 = 0 entries: phi stays c(0.1, 0.5, 0.3, 0.9)
  ## Top 2: indices 4 (0.9) and 2 (0.5)
  expect_equal(sort(result$topk_indices), c(2, 4))
  expect_equal(sort(result$topk_values, decreasing = TRUE), c(0.9, 0.5))
})

test_that("get_analogues trims entries for h > 1", {
  ## phi of length 4, h = 2 → trim to phi[1:3]
  phi <- c(0.1, 0.5, 0.3, 0.9)
  result <- get_analogues(phi, k = 2, h = 2)
  ## After trimming: phi = c(0.1, 0.5, 0.3), top 2: indices 2 (0.5) and 3 (0.3)
  expect_equal(sort(result$topk_indices), c(2, 3))
})

test_that("get_analogues errors when k exceeds available entries", {
  phi <- c(0.1, 0.5, 0.3)
  ## h = 2 trims to length 2, but k = 3
  expect_error(get_analogues(phi, k = 3, h = 2))
})

test_that("get_analogues warns on all-zero similarity", {
  phi <- c(0, 0, 0, 0)
  expect_warning(get_analogues(phi, k = 2, h = 1), "all selected similarity values are zero")
})

test_that("get_analogues warns on all-one similarity", {
  phi <- c(1, 1, 1, 1)
  expect_warning(get_analogues(phi, k = 2, h = 1), "all selected similarity values are one")
})

test_that("get_analogues accuracy: hand-computed values", {
  phi <- c(0.1, 0.5, 0.3, 0.9)
  result <- get_analogues(phi, k = 2, h = 1)
  ## Trimmed phi (h=1): c(0.1, 0.5, 0.3, 0.9)
  ## Top 2 by value: index 4 (0.9), index 2 (0.5)
  ## Weights: 0.9/1.4, 0.5/1.4
  expect_equal(result$topk_indices, c(4, 2))
  expect_equal(result$topk_values, c(0.9, 0.5))
  expect_equal(result$topk_weights, c(0.9 / 1.4, 0.5 / 1.4))
})

test_that("get_analogues with k = NULL uses all available entries", {
  phi <- c(0.1, 0.5, 0.3, 0.9)
  result <- get_analogues(phi, k = NULL, h = 1)
  ## h = 1 → no trimming, all 4 entries used
  expect_equal(nrow(result), 4)
  expect_equal(sum(result$topk_weights), 1)
  ## All indices present
  expect_equal(sort(result$topk_indices), 1:4)
})

test_that("get_analogues with k = NULL and h > 1 trims correctly", {
  phi <- c(0.1, 0.5, 0.3, 0.9)
  result <- get_analogues(phi, k = NULL, h = 2)
  ## h = 2 → trim to phi[1:3], use all 3
  expect_equal(nrow(result), 3)
  expect_equal(sort(result$topk_indices), 1:3)
})

test_that("get_analogues with k = NULL and uniform weights gives equal weights", {
  phi <- rep(1, 10)
  result <- get_analogues(phi, k = NULL, h = 1)
  expect_equal(nrow(result), 10)
  expect_equal(result$topk_weights, rep(0.1, 10))
})
