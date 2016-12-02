#' Run bunchApp: an Interactive Bunching Simulation
#'
#' \code{bunchApp} is an interactive simulator for bunching analysis. It is meant
#' to serve as a tool for understanding bunching analysis in general, and the use of
#' \code{bunchr} for data analysis. This app is opened on a separate window.
#'
#' This function merely runs the app. It accepts no parameters.
#'
#' @seealso
#' The machinery behind the simulation: \code{\link{bunch}}
#'
#' This simulator is also offered online at \url{https://itrilnick.shinyapps.io/bunchrapp/}.
#'
bunchApp <- function() {
  app.path = file.path(system.file("bunchApp", "bunchApp.R",
                                   package = "bunchr"))
  shiny::runApp(app.path)
}
