# This is a test file for the initial input for bunchr, to be used with the
# testthat package and devtools

context("test_input")

# create earning vector
ability_vec <- 4000 * rbeta(100000, 2, 5)
earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0, 0.1, 0, 1000)

test_that("main parameters are correct", {
  # not specifying zstar
  expect_error(bunch(earning_vec, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 2, exclude_after = 2, binw = 50 , draw = F))
  # no earnings vector
  expect_error(bunch(zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 2, exclude_after = 2, binw = 50, draw = F))
  # positive notch
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = -100,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 2, exclude_after = 2, binw = 50, draw = F))
})

test_that("counter-factual and excluded areas are well defined", {
  # excluded area and cf area problems
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 10, exclude_after = 10, binw = 50 , draw = F),
               "Excluded range must be a strict subset of analysis area")
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 12, exclude_after = 10, binw = 50 , draw = F),
               "cf_start and cf_end must be within the excluded range")
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 8,
                      exclude_before = 8, exclude_after = 10, binw = 50 , draw = F),
               "cf_start and cf_end must be within the excluded range")
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                        cf_start = 10, cf_end = 10,
                        exclude_before = 6, exclude_after = 6, binw = 50 , draw = F),
                 "Too few bins outside excluded area for polynomial size.")
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = -5, cf_end = 10,
                      exclude_before = -10, exclude_after = 5, binw = 50 , draw = F),
               "cf_start and cf_end must be positive integers")
})

test_that("Other variables are set correctly", {
  # negative bin
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 5, exclude_after = 5, binw = -2 , draw = F),
               "Bin width needs to be positive")
  # fraction or negative polynomial degree
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 5, exclude_after = 5, binw = 10,
                      poly_size = 4.5 , draw = F),
               "poly_size must be a positive integer")
  # bad convergence rate
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 5, exclude_after = 5, binw = 10,
                      convergence = -0.2 , draw = F),
               "Convergence threshold must be positive")
  # low convergence rate
  expect_warning(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 5, exclude_after = 5, binw = 10,
                      convergence = 0.2, draw = F),
               "Convergence threshold is low: 20%")
  # bad maximum iterations
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 5, exclude_after = 5, binw = 10,
                      max_iter = -50 , draw = F),
               "max_iter has to be positive")
  # low maximum iterations
  expect_warning(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 5, exclude_after = 5, binw = 10,
                      max_iter = 60.3, draw = F),
               "max_iter was rounded down to 60")
  # negative nboots
  expect_error(bunch(earning_vec, zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 5, exclude_after = 5, binw = 10,
                      nboots = -20 , draw = F),
               "nboots cannot be negative")
  # non-integer nboots
  expect_warning(bunch(earning_vec[1:1000], zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 1, exclude_after = 1, binw = 50,
                      correct = F, select = F, draw = F,
                      nboots = 50.5),
               "nboots was rounded down to 50")
  # too few nboots
  expect_warning(bunch(earning_vec[1:1000], zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                        cf_start = 10, cf_end = 10,
                        exclude_before = 1, exclude_after = 1, binw = 50,
                        correct = F, select = F, draw = F,
                        nboots = 1),
                "Such few bootstraps?")
  expect_error(bunch(earning_vec[1:1000], zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                     cf_start = 10, cf_end = 10,
                     exclude_before = 1, exclude_after = 1, force_after = 3,
                     binw = 50, correct = F, select = F, draw = F),
               "force_after must be TRUE or FALSE")
  expect_warning(bunch(earning_vec[1:1000], zstar = 1000, t1 = 0, t2 = 0.1, Tax = 0,
                     cf_start = 10, cf_end = 10,
                     exclude_before = 1, exclude_after = 1, force_after = T,
                     binw = 50, correct = F, select = F, draw = F, progress = 5),
                 "Wrong input for progress bar option, not showing it")


})

# clean up
remove(ability_vec, earning_vec)
