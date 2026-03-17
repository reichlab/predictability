test_that("week 40 start: correct season labels and season_week", {
  dat <- data.frame(
    epiyear = c(2010, 2010, 2010, 2011, 2011),
    epiweek = c(40, 41, 52, 1, 10)
  )
  res <- assign_seasons(dat, start_week = 40)

  expect_equal(res$season, rep("2010/2011", 5))
  expect_equal(res$season_week, 1:5)
})

test_that("week 13 start: correct labels for polio-style seasons", {
  dat <- data.frame(
    epiyear = c(1930, 1930, 1931, 1931),
    epiweek = c(13, 30, 1, 12)
  )
  res <- assign_seasons(dat, start_week = 13)

  expect_equal(res$season, rep("1930/1931", 4))
  expect_equal(res$season_week, 1:4)
})

test_that("week 1 start: season = epiyear, season_week = epiweek", {
  dat <- data.frame(
    epiyear = c(2020, 2020, 2021),
    epiweek = c(1, 52, 5)
  )
  res <- assign_seasons(dat, start_week = 1)

  expect_equal(res$season, c("2020", "2020", "2021"))
  expect_equal(res$season_week, c(1, 52, 5))
})

test_that("53-week year handling", {
  ## 2014-2015 season: 2014 has 53 epiweeks
  dat <- data.frame(
    epiyear = c(2014, 2014, 2014, 2015, 2015),
    epiweek = c(40, 52, 53, 1, 5)
  )
  res <- assign_seasons(dat, start_week = 40)

  expect_equal(res$season, rep("2014/2015", 5))
  ## should be sequential 1-5 even with week 53

  expect_equal(res$season_week, 1:5)
})

test_that("error on missing epiweek column", {
  dat <- data.frame(epiyear = 2020)
  expect_error(assign_seasons(dat), "epiweek")
})

test_that("error on missing epiyear column", {
  dat <- data.frame(epiweek = 1)
  expect_error(assign_seasons(dat), "epiyear")
})

test_that("error on invalid start_week", {
  dat <- data.frame(epiyear = 2020, epiweek = 1)
  expect_error(assign_seasons(dat, start_week = 0), "start_week")
  expect_error(assign_seasons(dat, start_week = 54), "start_week")
  expect_error(assign_seasons(dat, start_week = NA), "start_week")
  expect_error(
    suppressWarnings(assign_seasons(dat, start_week = "foo")),
    "start_week"
  )
})

test_that("preserves existing columns", {
  dat <- data.frame(
    epiyear = c(2010, 2011),
    epiweek = c(40, 1),
    location = c("MA", "MA"),
    value = c(10.5, 20.3)
  )
  res <- assign_seasons(dat, start_week = 40)

  expect_true(all(c("location", "value", "season", "season_week") %in% names(res)))
  expect_equal(res$location, c("MA", "MA"))
  expect_equal(res$value, c(10.5, 20.3))
})

test_that("multi-year data spanning several seasons", {
  ## Build 3 seasons of data: 2010/2011, 2011/2012, 2012/2013
  dat <- data.frame(
    epiyear = c(rep(2010, 13), rep(2011, 52), rep(2012, 52), rep(2013, 39)),
    epiweek = c(40:52, 1:52, 1:52, 1:39)
  )
  res <- assign_seasons(dat, start_week = 40)

  expect_equal(sort(unique(res$season)),
               c("2010/2011", "2011/2012", "2012/2013"))

  ## Check season_week resets for each season
  s1 <- res[res$season == "2010/2011", ]
  expect_equal(s1$season_week, seq_len(nrow(s1)))

  s2 <- res[res$season == "2011/2012", ]
  expect_equal(s2$season_week, seq_len(nrow(s2)))
})
