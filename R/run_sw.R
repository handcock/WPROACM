#' @title Run WPROACM
#'
#' @description Runs the WPROACM shiny application, a GUI for the statnet
#' suite of network analysis packages.
#'
#' @keywords graphs datagen models
#' @concept networks GUI shiny ergm
#' @export
#' @examples
#' \dontrun{
#' run_sw()
#' }
#'
#'
run_sw <- function() {
  shiny::runApp(system.file("shiny/WPROACM", package = "WPROACM"))
}
