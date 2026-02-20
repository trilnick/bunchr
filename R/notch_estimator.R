#' Analyzing Bunching at a Notch
#'
#' Given a notched budget set, this function gets a vector of earnings and
#' analyzes bunching. This function could be run independently, but best used
#' through the \code{bunch} function.
#'
#' @param earnings Vector of earnings, hopefully a very large one
#' @param zstar Place of notch (critical earning point)
#' @param t1 Tax rate before notch
#' @param t2 Tax rate after notch
#' @param Tax Lump sum penalty for crossing zstar.
#' @param cf_start Number of bins before the notch bin where counter-factual
#'   histogram should start.
#' @param cf_end Number of bins after the notch bin where counter-factual
#'   histogram should start.
#' @param exclude_before Number of excluded bins before the notch bin.
#' @param exclude_after Number of excluded bins after the notch bin.
#' @param force_after Should \code{bunch} be forced to use of the provided
#'   \emph{exclude_after} for the end of the bunching, rather than trying to
#'   find the bin where the sum of the integral is zero? See details.
#' @param binw Bin width.
#' @param poly_size Order of polynomial used to calculate counter-factual
#'   histogram.
#' @param convergence Minimal rate of change of bunching estimate to stop
#'   iterations.
#' @param max_iter Maximum number of iterations for bunching estimates.
#' @param select Should model selection be used to find counter-factual
#'   histogram? See details.
#' @param draw Should a graph be drawn?
#' @param title Title for plot output
#' @param varname Name for running variable, to be displayed in the plot
#'
#' @details
#'
#'   By default, \code{notch_estimator} will try to find the end of the notch,
#'   i.e. a histogram bin where the incentive to bunch wears off. To do this,
#'   a counter factual distribution is interpolated using the bins inside the
#'   counter-factual area but outside of the excluded area. This is assumed to be
#'   the histogram we would have without the tax at the notch point. At the
#'   observed bunching area, the bins should be much greater than their
#'   corresponding counter-factual bins. Later on, the bunching should create
#'   the notch area where the observed bins are lower than the counter-factual.
#'   factual bin  However, both observed and counter-factual histograms should
#'   have the same total mass. The end of the notch is determined as the bin
#'   where the running sum of differences between observed and counter-factual
#'   bins reaches zero. \code{notch_estimator} goes through an iterative
#'   process, setting the notch end bin and re-interpolating the counter-factual
#'   in the notch area using all the bins outside of the notch, trying to find
#'   a stable right-side boundary.
#'
#'   A user might want to force a visibly detectable end of notch, rather than
#'   let \code{notch_estimator} calculate one. Use \code{force_after=TRUE} with
#'   caution: this sets the notch size as \code{exclude_after} minus
#'    \code{exclude_before}
#'   in terms of bins. The notch size is used to calculate elasticity, and
#'   forcing the wrong notch size might bias the intensive margin elasticity
#'   estimate. In other settings, e.g. a labor market with extensive margins
#'   (entry and exit from labor force), forcing the notch size might be helpful.
#'
#'   For "impure" notches, where the marginal tax rate after the notch is
#'   different than the one before it, this function disregards the shifting of
#'   post-notch distribution to the right, as suggested by Kleven (2016).
#'   Assumption is that the notch effect is much stronger anyway.
#'
#'   The \code{select} option implements a \code{step} model selection. It runs
#'   backwards from the full polynomial model (specified by \code{poly_size}),
#'   trying to find the best explanatory polynomial model for counter-factual
#'   histogram.
#'

#'
#'
#'
#' @return \code{notch_estimator } returns a list of the following variables:
#' \describe{
#'   \item{\code{e}}{Estimated elasticity}
#'   \item{\code{Bn}}{The sum of total estimated extra bunching in the area starting
#'   at cf_start and through the notch bin (zstar) }
#'   \item{\code{notch_size}}{Distance between notch bin and bin where the estimated
#'   influence of the notch ends, delta_zed}
#'   \item{\code{data}}{A data frame with bin mids, counts, counter-factual
#'   counts, and excluded dummy}
#' }
#'
#'
#' @references Kleven, H J (2016). \emph{Bunching}, Annual Review of Economics,
#'   8(1).
#'
#' @seealso \code{\link{bunch}}, \code{\link{kink_estimator}}
#'
#' @examples
#' ability_vec <- 4000 * rbeta(100000, 2, 5)
#' earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0.2, 0.2, 500, 1000)
#' bunch_viewer(earning_vec, 1000, 15, 30, 2, 21, binw = 50)
#' notch_estimator(earning_vec, 1000, 0.2, 0.2, 500, 15, 30, 2, 21, binw = 50,
#' draw = FALSE)$e
#'
#' @export
#'



notch_estimator <- function(earnings, zstar, t1, t2, Tax = 0,
                            cf_start = NA, cf_end = NA,
                            exclude_before = NA, exclude_after = NA, force_after = FALSE,
                            binw = 10, poly_size = 7, convergence = 0.01,
                            max_iter = 100, select = TRUE, draw = TRUE,
                            title = "Bunching Visualization", varname = "Earnings") {
  ## ---------------------------------------------------------------------------
  ## 1. Error handling
  if (Tax == 0) {                      # this shouldn't happen when using bunch
    warning("Input for notch is zero")
  }
  if (is.na(cf_start)) {
    cf_start <- 20
    warning("cf_start not specified, using 20 bins as default")
  }
  if (is.na(cf_end)) {
    cf_end <- 100
    warning("cf_end not specified, using 100 bins as default")
  }
  if (is.na(exclude_before)) {
    exclude_before <- 10
    warning("exclude_before not specified, using 10 bins as default")
  }
  if (!is.logical(force_after)) {
    stop("force_after must be TRUE or FALSE")
  }

  ## 2. Setting up data frames

  # Create a histogram for this distribution - make zstar center of a bin.
  useful_calc <- ceiling( (zstar - min(earnings)) / binw )
  bunch_hist <- graphics::hist(earnings,
                               breaks = seq(
                                 from = floor((zstar - useful_calc * binw)) - binw/2,
                                 to=(ceiling(max(earnings) + binw)), by = binw),
                          plot=FALSE)

  if(!zstar %in% bunch_hist$mids) {
    stop("Problem with histogram")
  }
  remove(useful_calc)

  # Create a data-frame for the analysis
  reg_data <- as.data.frame(bunch_hist$mids)
  reg_data$counts <- bunch_hist$counts
  reg_data$cf_counts <- bunch_hist$counts   # cf_counts to be changed later
  colnames(reg_data) <- c("mid","counts","cf_counts")

  # Save the bins outside the CF area:
  saved_data <- reg_data[reg_data$mid < zstar - cf_start * binw |
                           reg_data$mid > zstar + cf_end * binw,]
  saved_data$cf_counts <- NA
  saved_data$excluded <- NA

  # Keep only the analysis part, between cf_start and cf_end
  reg_data <- reg_data[reg_data$mid >= zstar - cf_start * binw &
                         reg_data$mid <= zstar + cf_end * binw,]

  # Set the end of excluded area if not provided by user
  if (is.na(exclude_after)) {
    warning("exclude_after not specified, using 30% of cf_end")
    # calculate the top 30% area before cf_end
    useful_calc <- ceiling (0.3 * cf_end)
    exclude_after <- cf_end - useful_calc
  }

  # Create a list of excluded bins (list of bin mids in excluded area)
  excluded_list <- reg_data$mid[(reg_data$mid >= zstar - exclude_before * binw) &
                                  (reg_data$mid <= zstar + exclude_after * binw)]
  # Add a factor variable to reg_data, indicating excluded bins
  # All non-excluded bins will be "1".
  # Each excluded bin has its own factor, starting with "2"
  reg_data$excluded <- as.numeric(!(reg_data$mid %in% excluded_list))
  for (i in excluded_list) {
    reg_data$excluded[reg_data$mid == i] <- 1 + which(excluded_list == i)
  }
  reg_data$excluded <- as.factor(reg_data$excluded)
  # A column, factored all as not excluded, will help with counter-factuals
  cheat_excluded <- data.frame(as.factor(c(rep("1",dim(reg_data)[1]))))
  colnames(cheat_excluded) <- "excluded"
  # Add another column for temporary calculations
  cheat_excluded$temp_excluded <- cheat_excluded$excluded

  ## 3. "Naive" analysis - a starting point using all the excluded bins.
  # Create a variable list of polynomials for regression
  vars <- ""
  for (i in 1:poly_size) {
    vars <- paste0(vars," + I(mid^",i,")" )
  }
  # Run the first regression. Each bin in the excluded area has a
  # dummy variable. The "full" model includes the polynomial bin mids.
  null <- stats::lm(counts ~ excluded, data=reg_data)
  full <- stats::lm(stats::as.formula(paste0("counts ~ excluded",vars)), data=reg_data)
  if (select == TRUE) {
    reg_naive <- stats::step(full, scope = list(lower = null, upper = full),
                      direction = "backward",trace = 0)

  } else {reg_naive <- full}
  remove(null,full)
  # Forecast the first regression without the excluded area bin dummies: all the
  # bins have a "cheat excluded" factor value of 1 (not excluded). This
  # creates a forecast of counter-factuals: each bin forecast is the regression
  # intercept plus the coefficients for the polynomial value of the bin mid.
  # There are no individual bin effects in the forecast.
  reg_data$cf_counts <- stats::predict(reg_naive,cbind(reg_data[,c(1,3)],cheat_excluded))

  # Sum up all the coefficients for excluded bins (as defined by user)
  naive_B <- sum(reg_naive$coefficients[2:(length(excluded_list) + 1)])

  # Initialize the variable for total sum of bunching. The initial value is the
  # sum of coefficients for excluded bins before zstar, zstar itself, and one
  # after. Here we verify that this sum is positive.
  bunch_sum <- sum(reg_naive$coefficients[2:(2 + exclude_before + 1)])
  if (bunch_sum < 0) {
    stop("Something is wrong: first bin of the hole seems larger than the extra
        bunching at the notch point!
        \n
        First, Check your parameters. If they are correct,
        try one or more of the follwing:
        1. Using a smaller bin width.
        2. Extending CF area left and/or right.
        3. Shortening the excluded area to the left of the notch")
  }

  ## 4. Adjust notch size (excluded bin list) until total bunching equals zero.

  # Initial guess for delta zed (notch size in bins): exclude_after
  delta_zed <- exclude_after
  new_delta_zed <- 0
  counter <- 1

  # Iterating until delta_zed converges or maximum iterations
  while (abs(new_delta_zed - delta_zed) / delta_zed > convergence &
         counter < max_iter & force_after == FALSE) {

    # No need to update on first iteration. On later iterations,
    # update delta_zed with the value obtained in the previous run.
    if (counter > 1) {
      delta_zed <- new_delta_zed
      }

    # Create tentative list of excluded bins. Add 10 more at the end just to
    # make sure we're not in the hole: the user should have set exclude_after
    # so we're not in a visible hole.
    temp_excluded_v <- excluded_list[1:(exclude_before + 1 + delta_zed + 10)]
    # Create a column of temp_excluded bins in reg_data (and make it factors)
    reg_data$temp_excluded <- as.numeric(!reg_data$mid %in% temp_excluded_v)
    for (i in temp_excluded_v[!is.na(temp_excluded_v)]) {
      reg_data$temp_excluded[reg_data$mid == i] <- 1 + which(temp_excluded_v == i)
    }
    reg_data$temp_excluded <- as.factor(reg_data$temp_excluded)

    # Run another regression, with factors for the temporary excluded bins
    null <- stats::lm(counts ~ temp_excluded, data=reg_data)
    full <- stats::lm(stats::as.formula(paste0("counts ~ temp_excluded ",vars)), data = reg_data)
    # This time we want to start the selection with all the polynomial variables.
    if (select==TRUE) {
      temp_reg <- stats::step(full, scope=list(lower=null, upper=full),
                       direction="backward",trace=0)

    } else {temp_reg <- full}

    # Update counter-factual counts in reg data, using the latest regression
    reg_data$cf_counts <- stats::predict(temp_reg,cbind(reg_data[,c(1,3)],cheat_excluded))

    # The bunching displaces bin counts, but the total counts should be equal
    # to the counter factual. Start summing up the differences between
    # actual and counter-factual counts for each bin, starting at the beginning
    # of the excluded area (before zstar). After passing zstar, once the sum
    # reaches zero, that's delta zed: the end of the notch.

    # create vector of differences:
    diff_vec <-
      reg_data$counts[(reg_data$mid >= zstar - exclude_before * binw) &
                        (reg_data$mid <= zstar + exclude_after * binw)] -
      reg_data$cf_counts[(reg_data$mid >= zstar - exclude_before * binw) &
                           (reg_data$mid <= zstar + exclude_after * binw)]
    # Sum these differences until you reach zero on the right side of zstar
    bunch_sum <- diff_vec[1]
    i <- 2
    while( (bunch_sum[i-1] > 0 | i < (exclude_before + 2) ) &
           ( i < (exclude_after + 1) ) ) {
      bunch_sum[i] <- bunch_sum[i-1] +  diff_vec[i]
      i <- i+1
    }

    # Get a new delta zed as a number of bins
    new_delta_zed <- i - 1 - exclude_before - 1
    counter <- counter + 1
  }

  # If the user didn't force an excluded area end, and the notch size (in bins)
  # still exceeds the end of counter-factual range, raise issue.
  if (force_after==FALSE) {
    if (i >= cf_end) {
      stop("Something is wrong, try extending CF range to the right.")
    }
  }

  # Update the total bunching effect sum (on zstar and potentially excluded
  # before bins)
  ifelse(force_after == FALSE,
         new_B <- sum(sum(temp_reg$coefficients[2:(2 + exclude_before + 1)])),
         new_B <- naive_B)

  # Estimated elasticity using the equalizer.
  est_e <- stats::optimize(elas_equalizer, c(0, 5), t1, t2, Tax, zstar,
                    delta_zed, binw)$minimum

  # Plot the histogram with counter-factual.
  if (draw == TRUE) {
    graphics::plot(bunch_hist, freq=TRUE,ylim=c(0,1.1 *
                            stats::quantile(bunch_hist$counts, probs = c(0.99))),
         main = paste(title),
             xlab = paste(varname), ylab="Counts (bunch not to scale)")
    graphics::lines(x = reg_data$mid, y = reg_data$cf_counts,col = "purple",lwd=2)
    graphics::abline(v=c(zstar - binw / 2 - exclude_before * binw,
               zstar + binw / 2 + exclude_after * binw),
               col="green4", lty=2, lwd=2)
    graphics::abline(v=c(zstar - binw / 2 - cf_start * binw,
               zstar + binw / 2 + cf_end * binw), col="red",
           lty=2, lwd=2)
    graphics::abline(v= zstar + delta_zed * binw, col="purple",lty=2, lwd=2)
    graphics::legend("topright",col=c("red","green4","purple","purple"),
                 lty=c(2,2,1,2), legend=c("CF calc range", "Excluded bins",
                                        "CF distribution",
                                        expression(paste("Z* + ", Delta, "Z*"))
                                        ))
  }

  # Create the complete histogram data frame to return. Add the saved data
  # outside the analysis area.
  return_data <- saved_data[saved_data$mid < zstar - cf_start * binw, ]
  return_data <- rbind(return_data, reg_data[,c(1:4)])
  return_data <- rbind(return_data,
                       saved_data[saved_data$mid > zstar + cf_start * binw, ])

  results=list("e" = est_e,             # Estimate for elasticity
               "Bn" = new_B,            # Estimate of sum extra bunching
               "notch_size" = new_delta_zed * binw,
               "data" = return_data)

  return(results)

}
