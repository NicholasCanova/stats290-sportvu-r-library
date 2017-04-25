#' Run the packages shot chart / motion tracking app",
#'
#' @description Runs the shot chart / motion tracking appplication
#' @export
#' @importFrom shiny runApp


runShotChart <- function() {
    shiny::runApp(system.file('shinyApp', package = 'sportVU'))
}