#' @title Run WPROACM
#'
#' @description Runs the WPROACM shiny application, a GUI for the
#' WPRO online-calculator for excess deaths in countries
#'
#' @keywords graphs datagen models
#' @concept demography GUI shiny epidemiology
#' @export
#' @examples
#' \dontrun{
#' run()
#' }
#'
#'
run <- function() {
  shiny::runApp(system.file("shiny/WPROACM", package = "WPROACM"), port=7990)
}
