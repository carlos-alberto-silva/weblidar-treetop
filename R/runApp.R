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
  #invisible(lapply(c("shiny","RColorBrewer","spatstat","raster","rasterVis","sp","geometry","maptools","rgdal","rgl","lidR","pryr","sf","stars","rglwidget"), require, character.only = TRUE))
  appDir <- file.path(path.package("treetop", quiet=TRUE),"app")
  shiny::runApp(appDir, ...)
}
