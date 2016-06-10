# This is a test file for the initial results for bunchr, to be used with the
# testthat package and devtools

# when the function actually runs, I trim the vector and add options for it to
# run faster
context("test_kink")

set.seed(1982)
ability_vec <- 4000 * rbeta(100000, 2, 5)
earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0, 0.2, 0, 1000)

test_that("cf and excluded area", {
  #cf and excluded areas - should not happen when using bunchr
  expect_warning(kink_estimator(earning_vec[1:1000], zstar = 1000,  t1 = 0, t2 = 0.1,
                                cf_end =  10,
                                exclude_before = 2, exclude_after = 2, binw = 10,
                                poly_size = 7, convergence = 0.01, max_iter = 100,
                                correct = T, select = T,draw = F),
                 "cf_start not specified, using 20 bins as default")
  expect_warning(kink_estimator(earning_vec[1:1000], zstar = 1000,  t1 = 0, t2 = 0.1,
                                cf_start = 10,
                                exclude_before = 2, exclude_after = 2, binw = 10,
                                poly_size = 7, convergence = 0.01, max_iter = 100,
                                correct = T, select = T,draw = F),
                 "cf_end not specified, using 20 bins as default")
})

test_that("equal tax rates", {
  expect_error(bunchr(earning_vec, t1 = 0, t2 = 0, Tax = 0,
                      cf_start = 10, cf_end = 10,
                      exclude_before = 2, exclude_after = 2, binw = 50),
               "No change in marginal tax rate - can't calculate elasticity!")

  expect_error(bunchr(earning_vec, zstar = 1000, t1 = 0.2, t2 = 0.1, Tax = 0,
                        cf_start = 10, cf_end = 10,
                        exclude_before = 1, exclude_after = 1, binw = 50,
                        correct = T, select = F, draw=F),
               "This function only analyzes convex kinks. Set t1 < t2.")

})

# testing if it seems to work
estim <- bunchr(earning_vec, zstar = 1000, t1 = 0, t2 = 0.2, Tax = 0,
                         cf_start = 50, cf_end = 50,
                         exclude_before = 0, exclude_after = 0, binw = 5,
                         correct = T, select = T, draw=F,
                         nboots = 100, seed = 2016)
median_e <- median(estim$booted_e)

test_that("it actually works", {
  expect_lt(median_e, 0.22)
  expect_gt(median_e, 0.18)
  expect_is(estim, "list")
  expect_length(estim, 2)
  expect_length(estim$results, 3)
  expect_length(estim$booted_e, 100)
})

# clean up
remove(ability_vec, earning_vec, estim, median_e)
