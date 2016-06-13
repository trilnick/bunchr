# This is a test file for the initial results for bunchr, to be used with the
# testthat package and devtools

# when the function actually runs, I trim the vector and add options for it to
# run faster
context("test_notch")

set.seed(1982)
ability_vec <- 4000 * rbeta(100000, 2, 5)
earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0.1, 0.1, 200, 1000)

test_that("cf and excluded area", {
  # Tax = 0, should not happen when using with bunchr
  expect_warning(notch_estimator(earning_vec[1:1000], zstar = 1000,  t1 = 0.1,
                                t2 = 0.1, Tax = 0,
                                cf_start =  50, cf_end = 80,
                                exclude_before = 2, exclude_after = 60, binw = 10,
                                poly_size = 7, convergence = 0.01, max_iter = 100,
                                select = F,draw = F),
                 "Input for notch is zero")
  # no cf_start
  expect_warning(notch_estimator(earning_vec[1:1000], zstar = 1000,  t1 = 0.1,
                                 t2 = 0.1, Tax = 200,
                                 cf_end = 80,
                                 exclude_before = 2, exclude_after = 60, binw = 10,
                                 poly_size = 7, convergence = 0.01, max_iter = 100,
                                 select = F,draw = F),
                 "cf_start not specified, using 20 bins as default")
  # no cf_end
  expect_warning(notch_estimator(earning_vec[1:1000], zstar = 1000,  t1 = 0.1,
                                 t2 = 0.1, Tax = 200,
                                 cf_start = 20,
                                 exclude_before = 2, exclude_after = 60, binw = 10,
                                 poly_size = 7, convergence = 0.01, max_iter = 100,
                                 select = F,draw = F),
                 "cf_end not specified, using 100 bins as default")
  # no exclude_before
  expect_warning(notch_estimator(earning_vec[1:1000], zstar = 1000,  t1 = 0.1,
                                 t2 = 0.1, Tax = 200,
                                 cf_start = 20, cf_end = 80,
                                 exclude_after = 60, binw = 10,
                                 poly_size = 7, convergence = 0.01, max_iter = 100,
                                 select = F,draw = F),
                 "exclude_before not specified, using 10 bins as default")
  # no exclude_after
  expect_warning(notch_estimator(earning_vec[1:1000], zstar = 1000,  t1 = 0.1,
                                 t2 = 0.1, Tax = 200,
                                 cf_start = 20, cf_end = 80,
                                 exclude_before = 2, binw = 10,
                                 poly_size = 7, convergence = 0.01, max_iter = 100,
                                 select = F,draw = F),
                 "exclude_after not specified, using 30% of cf_end")
})

# testing if it seems to work
estim <- bunchr(earning_vec, zstar = 1000, t1 = 0.1, t2 = 0.1, Tax = 200,
                         cf_start = 50, cf_end = 100,
                         exclude_before = 2, exclude_after = 50, binw = 10,
                         select = T, draw = F,
                         nboots = 100, seed = 2016)

median_e <- median(estim$booted_e)

test_that("it actually works", {
  expect_lt(median_e, 0.25)
  expect_gt(median_e, 0.15)
  expect_is(estim, "list")
  expect_length(estim, 2)
  expect_length(estim$results, 4)
  expect_length(estim$booted_e, 100)
})

# test #2 - complete inelastic (still expect bunching)
earning_vec <- sapply(ability_vec, earning_fun, 0, 0.1, 0.1, 200, 1000)
#bunch_viewer(earning_vec, zstar = 1000, cf_start = 50, cf_end = 100,
#             exclude_after=30, binw=10)
estim <- bunchr(earning_vec, zstar = 1000, t1 = 0.1, t2 = 0.1, Tax = 200,
                cf_start = 50, cf_end = 100,
                exclude_before = 2, exclude_after = 30, binw = 10,
                select = T, draw = F,
                nboots = 100, seed = 2016)
median_e <- median(estim$booted_e)

test_that("it actually works", {
  expect_lt(median_e, 0.05)
  expect_gt(median_e, 0)
})

# test #3 - very little cf on the left side
earning_vec <- sapply(ability_vec, earning_fun, 0.3, 0.1, 0.1, 200, 1000)
bunch_viewer(earning_vec, zstar = 1000, cf_start = 50, cf_end = 60,
            exclude_after = 55, binw=10)
estim <- bunchr(earning_vec, zstar = 1000, t1 = 0.1, t2 = 0.1, Tax = 200,
                cf_start = 50, cf_end = 60,
                exclude_before = 2, exclude_after = 55, binw = 10,
                select = T, draw = F,
                nboots = 100, seed = 2016)
median_e <- median(estim$booted_e)

test_that("it actually works", {
  expect_lt(median_e, 0.35)
  expect_gt(median_e, 0.25)
})

# test #3 - different distribution, t2 > t1
ability_vec <- 1500*runif(100000,0,2)
earning_vec <- sapply(ability_vec, earning_fun, elas = 0.3, t1 = 0.1, t2 = 0.2,
                      Tax = 100, zstar = 1200)
bunch_viewer(earning_vec, zstar = 1200, cf_start = 25, cf_end = 40,
             exclude_after = 22, binw = 20)
estim <- bunchr(earning_vec, zstar = 1200, t1 = 0.1, t2 = 0.2, Tax = 100,
                cf_start = 25, cf_end = 40,
                exclude_before = 2, exclude_after = 22, binw = 20,
                select = T, draw = F,
                nboots = 100, seed = 2016)
median_e <- median(estim$booted_e)
quantile(estim$booted_e, probs = c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1))
test_that("it actually works", {
  expect_lt(median_e, 0.35)
  expect_gt(median_e, 0.25)
})

# test #4 - adding random elasticity
ability_vec <- 1500*runif(100000,0,2)
elas_vec <- 0.2 + rnorm(100000, mean = 0, sd = 0.05)
# need to drop negative elasticities...
ability_vec <- ability_vec[elas_vec >=0 ]
elas_vec <- elas_vec[elas_vec >=0 ]
num <- length(ability_vec)
earning_vec <- rep(NA, num)
# apply earning function separately for each pair of ability and elasticity.
for ( i in 1:num) {
  earning_vec[i] <- earning_fun(ability_vec[i], elas_vec[i], 0.2, 0.2, 200, 1000)
}
bunch_viewer(earning_vec, zstar = 1000, cf_start = 30, cf_end = 70,
             exclude_after = 30, binw = 20)
estim <- bunchr(earning_vec, zstar = 1000, t1 = 0.2, t2 = 0.2, Tax = 200,
                cf_start = 30, cf_end = 70,
                exclude_before = 2, exclude_after = 30, binw = 20,
                select = T, draw = T,
                nboots = 100, seed = 2016)
quantile(estim$booted_e, probs = c(0,0.05,0.1,0.25,0.5,0.75,0.9,0.95,1))
median_e <- median(estim$booted_e)
test_that("it actually works", {
  expect_lt(median_e, 0.25)
  expect_gt(median_e, 0.15)
})


# clean up
remove(ability_vec, earning_vec, estim, median_e, elas_vec, num)

