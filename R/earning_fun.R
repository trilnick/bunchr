#' Finding optimal earning under kinked/notched budget set
#'
#' For an agent with quasi-linear iso-elastic utility, find the utility
#' maximizing earning level.
#'
#' @param n Ability of person (earnings with zero tax)
#' @param elas elasticity of earnings w.r.t.  net-of-tax rate
#' @param t1 Tax rate before notch/kink
#' @param t2 Tax rate after notch/kink
#' @param Tax height of notch (zero for pure kink)
#' @param zstar place of notch/kink (critical earning point)
#'
#' @details \code{earn_funciton} is intended to simulate earnings of agents
#' under a kink or notch.
#' @return Optimal earning level.
#'
#' @seealso \code{\link{util_calc}}, \code{\link{bunch}}
#'
#' @examples
#' earning_fun(1200,0.2,0.1,0.3,100,1000)
#'
#' @export
#'
earning_fun <- function(n, elas, t1, t2, Tax, zstar) {
  ## ---------------------------------------------------------------------------
  ## Error handling
  if (n < 0 | zstar < 0) {
    stop("Ability and zstar need to be non-negative")
  }
  ## ---------------------------------------------------------------------------
  # Case one: positive or zero notch
  if (Tax >=0){
    # calculate counter-factual earnings
    z <- n * ((1 - t1) ^ (elas))
    # people whose counter-factual is below the kink/notch anyway
    if (z <= zstar) {
      return(z)
    }
    # people whose counter-factual is above the kink/notch
    z <- n * ((1 - t2) ^ (elas))
    #
    # return the earnings that maximize utility: tangent or kink/notch point
    # if agent is indifferent, they bunch.
    ifelse(util_calc(zstar,n,elas,t1,t2,Tax,zstar) >=
             util_calc(z,n,elas,t1,t2,Tax,zstar),
           return(zstar),
           return(n*((1-t2)^(elas))))
  } else {
    # Case 2: negative tax (upward notch)
    # default counter factual earning based on FOC
    z <- n * ((1 - t2) ^ (elas))
    if (z > zstar) {
      return(z)
    }
    # people whose counter-factual is below or at the kink/notch
    z <- n * ((1 - t1) ^ (elas))
    # return the earnings that maximize utility: tangent or kink/notch point
    # if agent is indifferent, they bunch.
    ifelse(util_calc(zstar,n,elas,t1,t2,Tax,zstar) >=
             util_calc(z,n,elas,t1,t2,Tax,zstar),
           return(zstar),
           return(n*((1-t1)^(elas))))
  }
}
