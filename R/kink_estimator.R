#' Analyzing Bunching at a Kink
#'
#' Given a kinked budget set, this function gets a vector of earnings and
#' analyzes bunching. This function could be run independently, but best used
#' throught the \code{bunchr} function.
#'
#' @param earnings Vector of earnings, hopefully a very large one.
#' @param zstar Place of kink (critical earning point).
#' @param t1 Marginal tax rate before kink.
#' @param t2 MArginal tax rate after kink.
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
#'
#' @details A histogram is created from the earnings vector, with the kink
#' point zstar as the center of one of the bins.
#'
#' Correction of the counter-factual is required, as the kink-induced bunching
#' will shift the whole distribution on the right side of the kink to the left.
#' This option follows Chetty \emph{et al} (2009) in correcting for this.
#'
#' Model selection works using the \code{step} function from the stats package.
#' It runs backwards from the full polynomial model, trying to find the best
#' explanatory model using the Akaike information criterion.
#'
#'
#'
#' @return \code{kink_estimator} returns a list of the following variables:
#' \describe{
#'   \item{\code{e}}{Estimated elasticity}
#'   \item{\code{iterations}}{Number of iterations in the correction process}
#'   \item{\code{data}}{A data frame with buching histogram.}
#' }
#'
#'
#' @references Chetty, R., Friedman, J., Olsen, T., Pistaferri, L. (2009)
#' \emph{Adjustment Costs, Firm Responses, and Micro vs. Macro Labor Supply
#' Elasticities: Evidence from Danish Tax Records}, Quarterly Journal of
#'  Economics, 126(2).
#'
#' @seealso \code{\link{bunchr}}, \code{\link{notch_estimator}}
#'
#' @examples
#' ability_vec <- 4000 * rbeta(100000, 2, 5)
#' earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0, 0.2, 0, 1000)
#' # bunch_viewer(earning_vec, 1000, 40, 40, 1, 1, binw = 10)
#' kink_estimator(earning_vec, 1000, 0, 0.2, 40, 40, 1, 1, 10, draw = FALSE)
#'
#' @export
#'

kink_estimator <- function(earnings, zstar,  t1, t2,
                           cf_start = NA, cf_end = NA,
                           exclude_before = 2, exclude_after = 2, binw = 10,
                           poly_size = 7, convergence = 0.01, max_iter = 100,
                           correct = TRUE, select = TRUE,draw = TRUE) {
  ## ---------------------------------------------------------------------------
  ## Error handling
  # these two should not happen if run through bunchr
  if (is.na(cf_start)) {
    cf_start <- 20
    warning("cf_start not specified, using 20 bins as default")
  }
  if (is.na(cf_end)) {
    cf_end <- 20
    warning("cf_end not specified, using 20 bins as default")
  }
  #
  if (t1 > t2) {
    stop("This function only analyzes convex kinks. Set t1 < t2.")
  }
  if (t1 == t2) {
    stop("No change in marginal tax rate - can't calculate elasticity!")
  }
  ## ---------------------------------------------------------------------------
  # create a histogram for this distribution - make zstar center of a bin
  useful_calc <- ceiling( (zstar - min(earnings)) / binw )
  bunch_hist <- graphics::hist(earnings,
                  breaks=seq(from=floor((zstar - useful_calc * binw)) - binw / 2,
                             to=(ceiling(max(earnings) + binw)), by = binw),
                  plot=FALSE)

  if(!zstar %in% bunch_hist$mids) {
    stop("Problem with histogram")
  }
  remove(useful_calc)

  # create a data-frame for the analysis
  reg_data <- as.data.frame(bunch_hist$mids)
  reg_data$counts <- bunch_hist$counts
  reg_data$cf_counts <- bunch_hist$counts   # cf_counts to be changed later
  colnames(reg_data) <- c("mid","counts","cf_counts")

  # save the bins outside the CF area:
  saved_data <- reg_data[reg_data$mid < zstar - cf_start * binw |
                           reg_data$mid > zstar + cf_end * binw,]
  saved_data$cf_counts <- NA
  saved_data$excluded <- NA

  # keep only the analysis part, between cf_start and cf_end bins
  reg_data <- reg_data[reg_data$mid >= zstar - cf_start * binw &
                       reg_data$mid <= zstar + cf_end * binw,]

  ## define the bins to exclude (the bunching bins)
  # create a list of excluded bins (list of bin mids)
  excluded_list <- reg_data$mid[(reg_data$mid >= zstar - exclude_before * binw) &
                                (reg_data$mid <= zstar + exclude_after * binw)]
  # add a factor variable to reg_data, indicating excluded bins
  # all non-excluded bins will be "1".
  # the excluded bins will start at "2".
  reg_data$excluded <- as.numeric(!reg_data$mid %in% excluded_list)
  for (i in excluded_list) {
    reg_data$excluded[reg_data$mid == i] <- 1 + which(excluded_list == i)
  }
  # make the "excluded" variable a factor
  reg_data$excluded <- as.factor(reg_data$excluded)

  # We need a column of all non-excluded for predicting counter-factual
  cheat_excluded <- data.frame(as.factor(c(rep("1",dim(reg_data)[1]))))
  colnames(cheat_excluded) <- "excluded"

  #### the "naive" regression - using actual earning counts
  # Create a variable list for regression with polynomial
  vars <- "~ excluded"
  for (i in 1:poly_size) {
    vars <- paste0(vars," + I(mid^",i,")" )
  }

  ## Run regression for counter-factual
  null <- stats::lm(counts ~ excluded, data=reg_data)
  full <- stats::lm(stats::as.formula(paste0("counts ",vars)), data=reg_data)
  if (select == TRUE) {
    reg_naive <- stats::step(full, scope = list(lower = null, upper = full),
                       direction = "backward",trace = 0)

  } else {reg_naive <- full}

  remove(null,full)

  # The current bunching estimate is the sum of all dummies for excluded bins.
  naive_B <- sum(reg_naive$coefficients[2:(length(excluded_list) + 1)])

  # generate counterfactual using the "none-excluded" column
  reg_data$cf_counts <- stats::predict(reg_naive,cbind(reg_data[,c(1,2)],cheat_excluded))

  ### counter-factual adapting a-la chetty
  counter <- 1
  if (correct == TRUE) {
    # setting variables
    old_B <- naive_B
    new_B <- 0
    # generate the "extra" counts for bunching purposes
    reg_data$extra <- reg_data$counts *
      as.numeric(reg_data$mid > zstar + exclude_after * binw) *
      old_B /
      sum(bunch_hist$counts[bunch_hist$mids > zstar + exclude_after * binw])
    #
    # Iterative process
    while( (abs(new_B - old_B)) / old_B > convergence &
           counter < max_iter)  {

      # if this not the first run, uptade the bunching estimator and extra density
      if (!counter == 1) {
        old_B <- new_B
        # For the first round, this is done in the setup before the loop
        reg_data$extra <- reg_data$counts *
          as.numeric(reg_data$mid > zstar + exclude_after * binw) *
          old_B /
          sum(bunch_hist$counts[bunch_hist$mids > zstar + exclude_after * binw])
      }
      # run regression
      null <- stats::lm(counts + extra ~ excluded, data = reg_data)
      full <- stats::lm(stats::as.formula(paste0("counts + extra ",vars)), data = reg_data)
      #
      if (select==TRUE) {
        temp_reg <- stats::step(full, scope=list(lower=null, upper=full),
                         direction="backward",trace=0)

      } else {temp_reg <- full}
      # update bunching estimator
      new_B <- sum((temp_reg)$coefficients[2:(length(excluded_list) + 1)])
      # adding this now: updating cf_counts
      reg_data$cf_counts <- stats::predict(temp_reg,cbind(reg_data[,c(1,2)],cheat_excluded))
      # update counter
      counter <- counter + 1
    }
  }
  #
  # Drawing the histogram
  if (draw==TRUE) {
    graphics::plot(bunch_hist, freq=TRUE,ylim=c(0,1.1 *
                                        stats::quantile(bunch_hist$counts,
                                                                probs=c(0.99))),
         main=paste0("Bunching Visualization"),
         xlab="Earnings",ylab="Counts (bunch not to scale)")
    graphics::lines(x=reg_data$mid, y=reg_data$cf_counts,col="purple",lwd=2)
    graphics::abline(v=c(zstar - binw / 2 - exclude_before * binw,
               zstar + binw / 2 + exclude_after * binw),
           col="green4", lty=2, lwd=2)
    graphics::abline(v=c(zstar - binw / 2 - cf_start * binw,
               zstar + binw / 2 + cf_end * binw), col="red",
           lty=2, lwd=2)
    graphics::legend("topright",col=c("red","green","purple"),lty=c(2,2,1),
           legend=c("CF calc range","Excluded bins","CF distribution"))
  }

  # creating the complete histogram data to be returned
  return_data <- saved_data[saved_data$mid < zstar - cf_start * binw, ]
  return_data <- rbind(return_data, reg_data[,c(1:4)])
  return_data <- rbind(return_data,
                       saved_data[saved_data$mid > zstar + cf_start * binw, ])

  #calculating elasticity and returning results
  if (correct == TRUE) {
    est_b <- new_B/(sum(reg_data$cf_counts[!reg_data$excluded == 1] /
                         (length(excluded_list) * binw)))
    est_e <- est_b/(zstar * log( (1 - t1) / (1 - t2) ))
    #
    results <- list("e" = est_e,      #estimate for elasticity of earnings
                    "iterations" = counter - 1,
                  "data" = return_data )        #the histograms
    return(results)
  } else {
    est_b <- naive_B/(sum(reg_data$cf_counts[!reg_data$excluded == 1] /
                       (length(excluded_list) * binw)))
    est_e <- est_b/(zstar * log( (1 - t1)/(1 - t2) ))
    results <- list("e" = est_e, #estimate for elasticity of earnings
                    "iterations" = counter - 1,
                    "data" = return_data )  #the histograms
    return(results)
  }
}
