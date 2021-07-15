#' Spin up an instance of the shiny application
#'
#' @param launch.browser Launch in browser mode
#' @param dev Refresh server upon code changes
#' 
#' @importFrom shiny runApp
#'
#' @export
run.shiny <- function(launch.browser=TRUE, dev=TRUE) {
    app.dir <- system.file("shiny", package="bieulergy")
    if (dev) options(shiny.autoreload=TRUE)
    shiny::runApp(app.dir, launch.browser=launch.browser, display.mode="normal")
}
