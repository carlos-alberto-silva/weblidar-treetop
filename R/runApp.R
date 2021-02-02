#'Launch treetop application
#
#'@description This function launch the treetop application
#'
#'@usage runApp(...)
#'
#'@param ... additional parameters from the \code{\link[shiny:runApp]{runApp}} function.
#'
#'@examples
#'\dontrun{
#'
#'# Launch treetop application
#'treetop::runApp(launch.browser = TRUE)
#'
#'}
#'@export
runApp<-function(...){
  appDir <- file.path(path.package("treetop", quiet=TRUE),"app")
  shiny::runApp(appDir, ...)
}
