#'Launch treetop application
#
#'@description This function launch the treetop application
#'
#'@usage launchApp(...)
#'
#'@param ... additional parameters from the \code{\link[shiny:runApp]{runApp}} function.
#'
#'@examples
#'\dontrun{
#'
#'# Launch treetop application
#'treetop::launchApp(launch.browser = TRUE)
#'
#'}
#'@export
launchApp<-function(...){
  appDir <- file.path(path.package("treetop", quiet=TRUE),"app")
  shiny::runApp(appDir, ...)
}
