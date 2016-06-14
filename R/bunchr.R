#' Bunching Analysis
#'
#' Given a kinked or notched budget set, this function gets a vector of earnings
#' and analyzes bunching.
#'
#' @param earnings Vector of earnings, hopefully a very large one.
#' @param zstar Place of kink (critical earning point).
#' @param t1 Marginal tax rate before kink.
#' @param t2 Marginal tax rate after kink.
#' @param Tax "Penalty" tax for crossing zstar.
#' @param cf_start Number of bins before the kink bin where counter-factual
#' histogram should start.
#' @param cf_end Number of bins after the kink bin where counter-factual
#' histogram should start.
#' @param exclude_before Number of excluded bins before the kink bin.
#' @param exclude_after Number of excluded bins after the kink bin.
#' @param binw Bin width.
#' @param poly_size Order of polynomial used to calculate counter-factual
#'  histogram.
#' @param convergence Minimal rate of change of bunching estimate to stop
#' iterations.
#' @param max_iter Maximum number of iterations for bunching estimates.
#' @param correct Should the counter-factual histogram be corrected to compensate
#' for shifting left because of the notch? See details.
#' @param select Should model selection be used to find counter-factual
#'  histogram? See details.
#' @param draw Should a graph be drawn?
#' @param nboots how many bootstraps should be run?
#' @param seed specify seed for bootstraps (earnings sampling).
#'
#' @details \code{bunchr} checks if the specification has a kink, i.e. if the Tax
#' parameter is greater than zero. If so, it applies \code{notch_estimator}.
#' Otherwise, it applies \code{kink_estimator}.  Additionaly, \code{bunchr}
#' can bootstrap by sampling the earnings vector, returning a vector with
#' the estimated elasticities.
#'
#' @return \code{bunchr} returns the parameters returned by
#' \code{kink_estimator} and \code{notch_estimator}. If bootstraps were asked for,
#' it returns a list with the function results as the first item, and a vector
#' of estimated elasticities as the second item. Drawing of histograms is
#' suppressed when running the bootsraps.
#'
#'
#' @seealso \code{\link{kink_estimator}}, \code{\link{notch_estimator}}
#'
#' @examples
#' # analyzing a kink
#' ability_vec <- 4000 * rbeta(100000, 2, 5)
#' earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0, 0.2, 0, 1000)
#' # bunch_viewer(earning_vec, 1000, 40, 40, 1, 1, binw = 10)
#' elast_est <- bunchr(earning_vec, 1000, 0, 0.2, Tax = 0, 40, 40, 1, 1,
#' binw = 10, draw=TRUE, nboots = 10, seed = 16)
#'elast_est$results$e
#' quantile(elast_est$booted_e, probs=c(0.05, 0.1, 0.5, 0.9, 0.95))
#'
#' # analyzing a notch
#' earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0.2, 0.2, 500, 1000)
#' # bunch_viewer(earning_vec, 1000, 60, 150, 2, 100, binw = 10)
#' elast_est <- bunchr(earning_vec, 1000, 0.2, 0.2, Tax = 500, 60, 150, 2, 100,
#' binw = 10, draw=TRUE, nboots = 10, seed = 16)
#' elast_est$results$e
#' quantile(elast_est$booted_e, probs=c(0.05, 0.1, 0.5, 0.9, 0.95))
#'
#' @export
#'

bunchr <- function(earnings, zstar, t1, t2, Tax = 0,
                   cf_start = NA, cf_end = NA,
                   exclude_before = NA, exclude_after = NA,
                   binw = 10, poly_size = 7,
                   convergence = 0.01, max_iter = 100,
                   correct = TRUE, select = TRUE, draw = TRUE, nboots = 0, seed = NA) {
  ## ---------------------------------------------------------------------------
  ## Error handling - this deals with all sorts of user errors in the input of
  #  parameters. These issues are NOT dealt with in the specific functions.
  #
  if (!is.numeric(earnings)) {
    stop("Earning ector must be numeric")
  }
  # if notch positive
  if (Tax < 0) {
    stop("This function does not analysze positive notches")
  }
  # excluded area not in cf range:
  if (exclude_before > cf_start | exclude_after > cf_end) {
    stop("cf_start and cf_end must be within the excluded range")
  }
  # none or very few bins to calculate counter-factual
  if (exclude_before == cf_start & exclude_after == cf_end) {
    stop("Excluded range must be a strict subset of analysis area")
  } else if ( (cf_start - exclude_before) +
             (cf_end - exclude_after) <= poly_size + 1) {
    stop("Too few bins outside excluded area for polynomial size.")
  }
  # non-positive input of analysis area in terms of bins from bunching bin
  if (!is.na(cf_start) & cf_start <= 0 |
      !is.na(cf_end) & cf_end <= 0 ) {
    stop("cf_start and cf_end must be positive integers")
  }
  # negative input of excluded area in terms of bins from bunching bin
  if (!is.na(exclude_before) & exclude_before < 0 |
      !is.na(exclude_after) & exclude_after < 0 ) {
    stop("exclude_before and exclude_after must be non-negative integers")
  }
  # non-positive bin width
  if (binw <= 0) {
    stop("Bin width needs to be positive")
  }
  # non-integer polynomial size for counter-factual estimation
  if (!poly_size%%(floor(poly_size)) == 0 & poly_size > 0) {
    stop("poly_size must be a positive integer")
  }
  # non-positive convergence threshold ( I assume it won't be zero)
  if (convergence <= 0) {
    stop("Convergence threshold must be positive")
  }
  # too high convergence threshold
  if (convergence > 0.1) {
    warning(paste0("Convergence threshold is low: ", convergence*100,"%"))
  }
  # negative or non integer number of maximum iterations
  if (max_iter <= 0) {
    stop("max_iter has to be positive")
  } else if (!max_iter%%(floor(max_iter)) == 0) {
    max_iter <- floor(max_iter)
    warning(paste0("max_iter was rounded down to ", max_iter))
  }
  if (max_iter < 50) {
    warning("max_iter is set below recommended level of 50")
  }
  if (nboots < 0 ) {
    stop("nboots cannot be negative")
  }
  if ( nboots > 0 & !nboots%%(floor(nboots)) == 0 ) {
    nboots <- floor(nboots)
    warning(paste0("nboots was rounded down to ", nboots))
  }
  if (nboots > 0 & nboots < 50) {
    warning("Such few bootstraps?")
  }

  ## ---------------------------------------------------------------------------

  population <- length(earnings)
  if (population < 1000) {
    warning("Earning vector smaller than 1000. Not sure you want to run an
            bunching analysis on this sample size")
  }



  # if notch is zero (kink)
  if (Tax == 0) {

    result1 <- kink_estimator(earnings, zstar, t1, t2, cf_start, cf_end,
                              exclude_before, exclude_after, binw, poly_size,
                              convergence, max_iter, correct, select, draw)
    if (nboots > 0) {
      boot_e <- rep(NA, nboots)
      boot_Bn <- rep(NA, nboots)
      boot_b <- rep(NA, nboots)
      if (!is.na(seed)) {
        set.seed(seed)
      }
      for (i in 1:nboots) {
        temp_pop <- sample(earnings,population,replace=TRUE)
        temp_result <- kink_estimator(temp_pop, zstar, t1, t2, cf_start, cf_end,
                                     exclude_before, exclude_after, binw, poly_size,
                                     convergence, max_iter, correct, select,
                                     draw=FALSE)
        boot_e[i] <- temp_result$e
        boot_Bn[i] <- temp_result$Bn
        boot_b[i] <- temp_result$b
        if (i%%10 == 0) {
          print(paste0("Done with ", i, " bootstraps ", Sys.time()))
        }
      }
      results <- list("e" = result1$e,
                      "Bn" = results1$Bn,
                      "b" = results1$b,
                      "data" = results1$data,
                      "booted_e" = boot_e,
                      "booted_Bn" = boot_Bn,
                      "booted_b" = boot_b )
      return(results)
    } else {
      return(result1)
    }
  }

  # if notch is greater than zero (notch)
  if (Tax > 0) {
    result1 <- notch_estimator(earnings, zstar, t1, t2, Tax,
                               cf_start, cf_end,
                               exclude_before, exclude_after,
                               binw, poly_size,
                               convergence, max_iter, select, draw)
    if (nboots > 0) {
      boot_e <- rep(NA, nboots)
      boot_Bn <- rep(NA, nboots)
      boot_dz <- rep(NA, nboots)
      if (!is.na(seed)) {
        set.seed(seed)
      }
      for (i in 1:nboots) {
        temp_pop <- sample(earnings,population,replace = TRUE)
        temp_result <- notch_estimator(temp_pop, zstar, t1, t2, Tax,
                                      cf_start, cf_end,
                                      exclude_before, exclude_after, binw,
                                      poly_size, convergence, max_iter, select,
                                      draw = FALSE)
        boot_e[i] <- temp_result$e
        boot_Bn[i] <- temp_result$Bn
        boot_dz[i] <- temp_result$notch_size
        if (i%%10 == 0) {
          print(paste0("Done with ", i, " bootstraps ", Sys.time()))
        }
      }
      results <- list("e" = result1$e,
                      "Bn" = result1$Bn,
                      "notch_size" = result1$notch_size,
                      "booted_e" = boot_e,
                      "booted_Bn" = boot_Bn,
                      "booted_notch_size" = boot_dz )
      return(results)
    } else {
      return(result1)
    }
  }
}
