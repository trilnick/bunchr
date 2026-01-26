#' Analyzing Bunching at an upward Notch
#'
#' See the \code{notch_estimator} function for details in the "classic" case of downward notch (e.g. losing benefits after an earning threshold). This
#' function does calculations for an upward notch (e.g. bonus after reaching an
#' earning target). The mechanics are the same but the signs
#' are opposite and the notch is to the left of zstar.
#'
#' @param earnings Vector of earnings, hopefully a very large one
#' @param zstar Place of kink (critical earning point)
#' @param t1 Tax rate before kink
#' @param t2 Tax rate after kink
#' @param Tax "Penalty" tax for crossing zstar.
#' @param cf_start Number of bins before the kink bin where counter-factual
#' histogram should start.
#' @param cf_end Number of bins after the kink bin where counter-factual
#' histogram should start.
#' @param exclude_before Number of excluded bins before the kink bin.
#' @param exclude_after Number of excluded bins after the kink bin.
#' @param force_after Should \code{bunch} be forced to use of the provided
#' \emph{exclude_after} for the end of the bunching, rather than trying to find
#' the bin where the sum of the integral is zero? See details.
#' @param binw Bin width.
#' @param poly_size Order of polynomial used to calculate counter-factual
#'  histogram.
#' @param convergence Minimal rate of change of bunching estimate to stop
#' iterations.
#' @param max_iter Maximum number of iterations for bunching estimates.
#' @param select Should model selection be used to find counter-factual
#'  histogram? See details.
#' @param draw Should a graph be drawn?
#' @param title Title for plot output
#' @param varname Name for running variable, to be desplayed in the plot
#'
#' @details A histogram is created from the earnings vector, with the kink
#' point zstar as the center of one of the bins.
#'
#' For "impure" notches, where the marginal tax rate after the notch is different
#' than the one before it, this function disregards the shifting of post-notch
#' distribution to the right, as suggested by Kleven (2016). Assumption is that
#' the notch effect is much stronger anyway.
#'
#' Model selection works using the \code{step} function from the stats package.
#' It runs backwards from the full polynomial model, trying to find the best
#' explanatory model using the Akaike Information Criterion.
#'
#' By default, \code{notch_estimator} will try to find the end of the notch, i.e.
#' a histogram bin defining a right-side boundary for a range of an excluded area.
#' An interpolation of the counts inside this range renders an equality between
#' the sum of the ``excess'' counts, from the left side to the notch point, and
#' the sum of ``missing'' counts from the notch point to the notch size.
#' \code{notch_estimator} goes through an iterative process to find a stable
#' right-side boundary, labels it \emph{notch_size} and returns it. However, the
#' user might want to force a visibly detectable end of notch, rather than let
#' \code{notch_estimator} calculate one. Use this option with caution: the notch
#' size is then used to calculate elasticity. For calculating intensive margin
#' elasticities, excess bunching must all come from other bins. Thus, total sums
#' must be equal and forcing the notch size might not be appropriate. In other
#' settings, e.g. a labor market with extensive margins (entry and exit from
#' labor force), forcing the notch size might be helpful.
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
#' 8(1).
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



.upward_notch_estimator <- function(earnings, zstar, t1, t2, Tax = 0,
                            cf_start = NA, cf_end = NA,
                            exclude_before = NA, exclude_after = NA, force_after = FALSE,
                            binw = 10, poly_size = 7, convergence = 0.01,
                            max_iter = 100, select = TRUE, draw = TRUE,
                            title = "Bunching Visualization", varname = "Earnings") {
  ## ---------------------------------------------------------------------------
  ## Error handling
  if (Tax == 0) {                      # this shouldn't happen when using bunch
    warning("Input for notch is zero")
  }
  if (is.na(cf_start)) {
    cf_start <- 100
    warning("cf_start not specified, using 100 bins as default")
  }
  if (is.na(cf_end)) {
    cf_end <- 20
    warning("cf_end not specified, using 20 bins as default")
  }
  if (is.na(exclude_before)) {
    exclude_after <- 10
    warning("exclude_after not specified, using 10 bins as default")
  }
  if (!is.logical(force_after)) {
    stop("force_after must be TRUE or FALSE")
  }


  # create a histogram for this distribution - make zstar center of a bin
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


  # create a data-frame for the analysis
  reg_data <- as.data.frame(bunch_hist$mids)
  reg_data$counts <- bunch_hist$counts
  reg_data$cf_counts <- bunch_hist$counts   # cf_counts to be changed late
  colnames(reg_data) <- c("mid","counts","cf_counts")

  # save the bins outside the CF area:
  saved_data <- reg_data[reg_data$mid < zstar - cf_start * binw |
                           reg_data$mid > zstar + cf_end * binw,]
  saved_data$cf_counts <- NA
  saved_data$excluded <- NA

  # keep only the analysis part, between cf_start and cf_end bins
  reg_data <- reg_data[reg_data$mid >= zstar - cf_start * binw &
                         reg_data$mid <= zstar + cf_end * binw,]

  # need to figure counter factual in the hole.
  # allow for user input
  if (is.na(exclude_before)) {
    warning("exclude_before not specified, using 30% of cf_before")
    # calculate the top 30% area before cf_end
    useful_calc <- ceiling (0.3 * cf_start)
    exclude_before <- cf_start - useful_calc
  }

  ## define the bins to exclude (the bunching bins)
  # create a list of excluded bins (list of bin mids)
  excluded_list <- reg_data$mid[(reg_data$mid >= zstar - exclude_before * binw) &
                                  (reg_data$mid <= zstar + exclude_after * binw)]
  # add a factor variable to reg_data, indicating excluded bins
  # all non-excluded bins will be "1".
  # the excluded bins will start at "2".
  # Note for upward kink, we iterate backwards, so we need to reverse
  # the order of these excluded bins. That's what the rev does.
  reg_data$excluded <- as.numeric(!(reg_data$mid %in% excluded_list))
  for (i in excluded_list) {
    reg_data$excluded[reg_data$mid == i] <- 1 + which(rev(excluded_list) == i)
  }
  # make the "excluded" variable a factor
  # DANGER: THE PREVIOUS VERSION HAS LISTED FACTORS
  reg_data$excluded <- as.factor(reg_data$excluded)
  # We need a column of all non-excluded for predicting counter-factual
  cheat_excluded <- data.frame(as.factor(c(rep("1",dim(reg_data)[1]))))
  colnames(cheat_excluded) <- "excluded"
  # and another one for temporary calculations
  cheat_excluded$temp_excluded <- cheat_excluded$excluded
  ###

  #### the "naive" regression - using actual earning counts
  # Create a variable list for regression with polynomial
  vars <- ""
  for (i in 1:poly_size) {
    vars <- paste0(vars," + I(mid^",i,")" )
  }
  # Run regression for counter-factual
  null <- stats::lm(counts ~ excluded, data=reg_data)
  full <- stats::lm(stats::as.formula(paste0("counts ~ excluded",vars)), data=reg_data)
  if (select == TRUE) {
    reg_naive <- stats::step(full, scope = list(lower = null, upper = full),
                      direction = "backward",trace = 0)

  } else {reg_naive <- full}
  remove(null,full)

  reg_data$cf_counts <- stats::predict(reg_naive,cbind(reg_data[,c(1,3)],cheat_excluded))

  # start variables to run analysis
  # naive bunching is the sum of coefficients of dummies for bins in excluded
  # area. Note: eventual sum of total bunching should be zero
  # until (including) zstar
  naive_B <- sum(reg_naive$coefficients[2:(length(excluded_list) + 1)])

  # initial "guess" for sum bunching uses coefficients from reg_naive, but sums
  # only until one before zstar. Should be positive
  bunch_sum <- sum(reg_naive$coefficients[2:(2 + exclude_after + 1)])
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

  # Iterate, updating delta zed until it converges
  # in the itaration:
  # 1) summing until the bunching sum is zero, get delta-zed
  # 2) updating excluded after as midway between delta-zed and former excluded after.

  # start with initial guess: delta zed
  delta_zed <- exclude_before
  new_delta_zed <- 0
  counter <- 1
  while (abs(new_delta_zed - delta_zed) / delta_zed > convergence &
         counter < max_iter & force_after == FALSE) {

    if (counter > 1) {
      delta_zed <- new_delta_zed
      }

    # make tentative excluded bin list.
    # plus "change" of 10 bins just in case we fall in the hole
    temp_excluded_v <- excluded_list[1:(exclude_before + 1 + delta_zed + 10)]
    temp_excluded_v <- temp_excluded_v[!is.na(temp_excluded_v)]
    # make it a factor variable again
    reg_data$temp_excluded <- as.numeric(!(reg_data$mid %in% temp_excluded_v))
    for (i in temp_excluded_v) {
      reg_data$temp_excluded[reg_data$mid == i] <- 1 + which(rev(temp_excluded_v) == i)
    }
    reg_data$temp_excluded <- as.factor(reg_data$temp_excluded)

    # run the regression again, with limited excluded bins
    null <- stats::lm(counts ~ temp_excluded, data=reg_data)
    full <- stats::lm(stats::as.formula(paste0("counts ~ temp_excluded ",vars)), data = reg_data)
    # choose to use the full max polynomial or select better
    if (select==TRUE) {
      temp_reg <- stats::step(full, scope=list(lower=null, upper=full),
                       direction="backward",trace=0)

    } else {temp_reg <- full}
    # summary(temp_reg)

    # Update counter-factual histogram
    reg_data$cf_counts <- stats::predict(temp_reg,cbind(reg_data[,c(1,3)],cheat_excluded))
    # Start summing up the differences between CF and actual counts. When you get
    # to zero, that's delta zed. You could just sum the coefficients for dummies
    # of excluded bins, but this way delta zed can extend further the excluded bin
    # limit.

    # create vector of differences:
    diff_vec <-
      reg_data$counts[(reg_data$mid >= zstar - exclude_before * binw) &
                        (reg_data$mid <= zstar + exclude_after * binw)] -
      reg_data$cf_counts[(reg_data$mid >= zstar - exclude_before * binw) &
                           (reg_data$mid <= zstar + exclude_after * binw)]
    # Sum these differences until you reach zero on the left side of notch
    # this means reversing the difference vector in an upward notch!
    diff_vec <- rev(diff_vec)
    bunch_sum <- diff_vec[1]
    i <- 2
    while( (bunch_sum[i-1] > 0 | i < (exclude_after + 2) ) &
           ( i < (exclude_before + 1) ) ) {
      bunch_sum[i] <- bunch_sum[i-1] +  diff_vec[i]
      i <- i+1
    }

    # setting the new delta zed:
    new_delta_zed <- i - 1 - exclude_after - 1
    counter <- counter+1
  }

  if (force_after==FALSE) {
    if (i >= cf_start) {
      stop("Something is wrong, try extending CF range to the left")
    }
  }

  # calculating the bunching estimate - for the bunch
  ifelse(force_after == FALSE,
         new_B <- sum(sum(temp_reg$coefficients[2:(2 + exclude_before + 1)])),
         new_B <- naive_B)

  # estimated elasticity
  est_e <- stats::optimize(elas_equalizer, c(0, 5), t1, t2, Tax, zstar,
                    delta_zed, binw)$minimum

  iterations <- counter
  #
  # drawing
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

  # creating the complete histogram data to be returned
  return_data <- saved_data[saved_data$mid < zstar - cf_start * binw, ]
  return_data <- rbind(return_data, reg_data[,c(1:4)])
  return_data <- rbind(return_data,
                       saved_data[saved_data$mid > zstar + cf_start * binw, ])

  results=list("e" = est_e,                           # estimate for elasticity
               "Bn" = new_B,               # estimate of sum extra bunching from
                                          # start of excluding area to notch bin
               "notch_size" = new_delta_zed * binw,
               "data" = return_data)

  return(results)

}
