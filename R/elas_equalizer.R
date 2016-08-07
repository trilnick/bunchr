#' Using elasticity to calculating distance between utility at tangency and at
#' notch point
#'
#'
#' Given an elasticity, a budget set, and the earnings of the marginal buncher,
#' calculate the utility at notch point and at marginal buncher's earning, and
#' return the absolute difference. Equating these two utilities helps find the
#' elasticity of the marginal buncher. See equations (3) and (4) at Kelven and
#' Waseem (2013)
#'
#' @param elas elasticity of earnings w.r.t.  net-of-tax rate
#' @param t1 Tax rate before notch/kink
#' @param t2 Tax rate after notch/kink
#' @param Tax Height of notch (zero for pure kink)
#' @param zstar Place of notch/kink (critical earning point)
#' @param delta_zed The notch size
#' @param binw Bin width
#'
#' @return Absolute value of utility at \eqn{zstar+delta_zed} minus utility at
#' kink/notch point.
#'
#' @references Kleven, H. and Waseem, Mazhar (2013) \emph{Using notches to
#' uncover optimization frictions and structural elasticities: Theory and
#' evidence from Pakistan}, The Quarterly Journal of Economics 128(2)
#'
#' @examples
#' elas_equalizer(0.2, 0.1, 0.2, 100, 1000, 200, 20)
#'
#' @export
#'

elas_equalizer <- function(elas, t1, t2, Tax, zstar, delta_zed, binw) {
  ## ---------------------------------------------------------------------------
  ## Error handling
  if (zstar < 0 | delta_zed < 0 ) {
    stop("Zstar, delta_zed and binw need to be non-negative")
  }
  if (binw <= 0) {
    stop("Bin width must be positive")
  }
  if (elas < 0) {
    warning("Negative elasticity input")
  }
  ## ---------------------------------------------------------------------------
  # Utility at notch point
  U_N <- zstar * (1 - t1) -
    ( 1 / (1 + (1 / elas) ) ) *
    ( (1 - t2) / ( (zstar + delta_zed * binw) ^ ( 1 / elas) ) ) *
    ( zstar ^ (1 + (1 / elas) ) )

  # Utility at tangency point
  U_I <- zstar * (1 - t1) +
    (delta_zed * binw) *  (1 - t2) -
    Tax -
    ( (1 / (1 + (1 / elas))) *  (1 - t2) *  (zstar + delta_zed * binw)   )

  return( (U_I - U_N)^2 )
}
