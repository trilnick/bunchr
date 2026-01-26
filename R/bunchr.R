#' bunchr: A Package for Bunching Analysis
#'
#' The \code{bunchr} package is meant to help analyze bunching. Given a vector
#' of earnings (or any other numeric vector), it creates a counter-factual
#' count histogram and calculates the compensated elasticity of earnings w.r.t.
#' the net-of-tax rate.
#'
#' @section Main functions:
#'
#' \code{bunchr} has three main functions:
#' \describe{
#'   \item{\code{\link{bunch}}}{is the main function running the actual analysis.}
#'   \item{\code{\link{bunch_viewer}}}{serves as an aid to the second by visualizing
#'  some of the user-specified options without running an analysis. Use it to see
#'  what the histogram of your earnings vector looks like when setting
#'  specific bin size, where the counter-factual analysis should be done, and
#'  the bounds of the excluded area. You can also save the histogram bins and
#'  counts.}
#'  \item{\code{\link{bunchApp}}}{is an interactive simulator. Use it to
#'  explore bunching simulation and estimation of earning elasticity.}
#' }
#'
#' @seealso \code{\link{bunch}}, \code{\link{bunch_viewer}}
#'
#' @name bunchr
#'
"_PACKAGE"
