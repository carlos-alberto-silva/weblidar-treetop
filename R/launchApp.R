#'Launch treetop application
#
#'@description This function launch the treetop application.
#'
#'@usage launchApp(...)
#'
#'@param ... additional parameters from the \code{\link[shiny:runApp]{runApp}} function.
#'@return This function does not return.
#'@details The treetop GUI will pop up for LiDAR data visualization, processing and analysis. Import a LiDAR-derived Canopy Height Model (CHM) for custom data processing. Interrupt R to stop the application (usually by pressing Ctrl+C or Esc).
#'
#'@examples
#'\dontrun{
#'
#'# Launch treetop application
#'treetop::launchApp(launch.browser = TRUE)
#'
#'}
#'@export
#'@importFrom shiny runApp
#'@importFrom RColorBrewer brewer.pal
#'@importFrom spatstat envelope as.ppp
#'@importFrom raster raster projection res aggregate plot crop area intersect ncell cv
#'@importFrom sp coordinates SpatialPointsDataFrame SpatialPolygonsDataFrame merge
#'@importFrom geometry convhulln
#'@importFrom rgdal writeOGR
#'@importFrom rgl rgl.triangles lines3d axes3d title3d rgl.cur rgl.close
#'@importFrom lidR tree_detection silva2016
#'@import pryr
#'@importFrom stars st_as_stars
#'@importFrom rglwidget rglwidget
#'@importFrom rasterVis plot3D
#'@importFrom sf st_as_sf as_Spatial
launchApp<-function(...){
  appDir <- file.path(path.package("treetop", quiet=TRUE),"app")
  shiny::runApp(appDir, ...)
}
