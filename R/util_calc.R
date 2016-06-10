#' Calculating quasi-linear iso-elastic utility
#'
#' \deqn{u(z,n,elas,t1,t2,Tax,zstar) =}
#' \deqn{z * (1 - t1) + [z > zstar] * ((z - zstar)
#'  * (t2 - t1) - Tax) - n / (1 + (1 / elas)) * (z / n)^(1 + (1 / elas))}
#'
#' @param z Earnings
#' @param n Ability of person (earnings with zero tax)
#' @param elas elasticity of earnings w.r.t.  net-of-tax rate
#' @param t1 Tax rate before notch/kink
#' @param t2 Tax rate after notch/kink
#' @param Tax height of notch (zero for pure kink)
#' @param zstar place of notch/kink (critical earning point)
#'
#' @return The utility of earning sum z given other parameters.
#'
#' @examples
#' util_calc(900, 950, 0.2, 0.1, 0.2, 100, 1000)
#'
#' @export
#'


util_calc <- function(z, n, elas, t1, t2, Tax, zstar) {
  ## ---------------------------------------------------------------------------
  ## Error handling
  if (z < 0 | n < 0 | zstar < 0) {
    stop("Earnings, ability and zstar need to be non-negative")
  }
  ## ---------------------------------------------------------------------------
  # Utility for earnings below notch/kink threshold
  if (z <= zstar) {
    return(z - (t1 * z) - ((n/(1 + (1/elas))) * ((z/n)^(1 + (1/elas)))))
  }
  # Utility for earnings above notch/kink threshold
  return(z - t1 * zstar - (t2 * (z - zstar)) - Tax - ((n/(1 + (1/elas))) *
    ((z/n)^(1 + (1/elas)))))
}
