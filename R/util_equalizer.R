#' Calculating distance between utility at tangency and at notch/kink point
#'
#' Ability (n) and elasticity (e) determine an agent's earnings and utility. This
#' function determines the tangency point of the agent's utility with the budget
#' line and returns the distance between the utility of earning at that point
#' and the utility of earning at the notch/kink point. This function is mostly
#' used to find the marginal buncher.
#'
#' @param n Ability of person (earnings with zero tax)
#' @param elas elasticity of earnings w.r.t.  net-of-tax rate
#' @param t1 Tax rate before notch/kink
#' @param t2 Tax rate after notch/kink
#' @param Tax height of notch (zero for pure kink)
#' @param zstar place of notch/kink (critical earning point)
#'
#' @return Absolute value of utility at tangency minus utility at kink/notch
#' point.
#'
#' @seealso \code{\link{util_calc}}
#'
#' @examples
#' util_equalizer(1200,0.2,0.1,0.3,100,1000)
#'
#' @export
#'

util_equalizer <- function(n,elas,t1,t2,Tax,zstar) {
  ## ---------------------------------------------------------------------------
  ## Error handling
  if (n < 0 | zstar < 0) {
    stop("Ability and zstar need to be non-negative")
  }
  ## ---------------------------------------------------------------------------
  # if earnings with t1 exceed zstar, tangency would be after the kink
  z <- n*((1-t1)^elas)
  if (z > zstar) {
    z <- n*((1-t2)^elas)
  }
  # utility at that earning:
  U_I <- util_calc(z,n,elas,t1,t2,Tax,zstar)
  #utility at notch
  U_N <- util_calc(zstar,n,elas,t1,t2,Tax,zstar)
  return(abs(U_I-U_N))
}
