#' Spin up an instance of the shiny application
#'
#' @param launch.browser Launch in browser mode
#' @param dev Refresh server upon code changes
#' 
#' @importFrom shiny runApp
#'
#' @export
run.shiny <- function(launch.browser=TRUE, dev=TRUE, host='0.0.0.0', port=3838) {
    app.dir <- system.file("shiny", package="bieulergy")
    if (dev) options(shiny.autoreload=TRUE)
    shiny::runApp(app.dir, launch.browser=launch.browser, display.mode="normal", host=host, port=port)
}
