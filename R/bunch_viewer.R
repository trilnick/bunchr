#' Visualizing a histogram and potential excluded areas
#'
#' This function is meant to aid find excluded bins and analysis area for a
#' bunching study. It displays a histogram with borders. Optionally, you can get
#' the actual histogram back. This is convenient, as the kink/notch point is set
#' as the center of a bin.
#'
#' @param earnings Vector of earnings, hopefully a very large one
#' @param zstar Place of notch/kink (critical earning point)
#' @param cf_start Number of bins before the kink bin where counter-factual
#' histogram should start.
#' @param cf_end Number of bins after the kink bin where counter-factual
#' histogram should start.
#' @param exclude_before Number of excluded bins before the kink bin.
#' @param exclude_after Number of excluded bins after the kink bin.
#' @param binw Bin width.
#' @param trimy Logical. Should the y-axis be trimmed to better show off-bunching
#' histogram?
#' @param report Should the function return the actual histogram?
#' @param title Title for Plot output
#' @param varname Name for running variable, to be desplayed in the plot
#'
#' @return A plot, the actual histogram if report is set to TRUE.
#'
#' @seealso \code{\link{bunch}}
#'
#' @examples
#' ability_vec <- 4000 * rbeta(100000, 2, 5)
#' earning_vec <- sapply(ability_vec, earning_fun, 0.2, 0.1, 0.2, 0, 1000)
#' bunch_viewer(earning_vec, 1000, 20, 40, 2, 2, 20, trimy = TRUE, report = FALSE)
#'
#' @export
#'
bunch_viewer <- function(earnings, zstar = NA, cf_start = 10, cf_end = 50,
                         exclude_before = 2, exclude_after = 20,
                         binw = NA, trimy = TRUE, report = FALSE,
                         title = "Count Histogram", varname = "Running Variable") {

  ## ---------------------------------------------------------------------------
  ## Error handling
  # no bunch point provided
  if (is.na(zstar)) {
    stop("No zstar provided")
  }
  # No binw specified
  if (is.na(binw) | binw < 0) {
    binw <- round( (max(earnings)-min(earnings))/100 ,0)
    warning(paste0("Binw unspecified or negative, using binw = ",binw))
  }
  #other parameters unspecified
  if (is.na(cf_start) | is.na(cf_end) | is.na(exclude_before) |
      is.na(exclude_after) ) {
    warning("One or more parameters unspecified - plotting only what was given")
  }
  # negative parameters unspecified
  if (cf_start <= 0 | cf_end <= 0 | exclude_before < 0 | exclude_after < 0) {
    stop("cf_start and cf_end must be positive, exclude_before and exclude_after
         must be non-negative")
  }
  ## ---------------------------------------------------------------------------

  # create a histogram for this distribution - make zstar center of a bin
  useful_calc <- ceiling( (zstar - min(earnings)) / binw )
  bunch_hist <- graphics::hist(earnings,
              breaks = seq(from = floor((zstar - useful_calc * binw)) - binw/2,
                           to=(ceiling(max(earnings) + binw)), by = binw),
              plot=FALSE)
  # dealing with preference for y axis trim
  ifelse(trimy == TRUE,
         graphy <- c(min(bunch_hist$counts), 1.3 * as.numeric(
                         stats::quantile(bunch_hist$counts, probs = c(0.95)))),
         graphy <- c(min(bunch_hist$counts),1.1 * max(bunch_hist$counts))
         )
  # plotting the thing
  graphics::plot(bunch_hist, freq = TRUE, ylim = graphy,
       main = paste(title),
       xlab = varname, ylab="Counts (bunch not to scale)")
  graphics::abline(v = zstar, lty = 2,lwd = 2)
  graphics::abline(v=c(zstar - exclude_before * binw, zstar + exclude_after * binw),
         col="green4", lty = 2, lwd = 2)
  graphics::abline(v=c(zstar - binw / 2 - cf_start * binw,
             zstar + binw / 2 + cf_end * binw), col = "red", lty = 2, lwd = 2)
  graphics::legend("topright",col=c("black","red","green4"),lty=c(2,2,2),
         legend=c("Kink/Notch point","Analysis Area","Excluded Area"))
  #
  # return the histogram
  if (report == TRUE) {
    return(bunch_hist)
  }
}
