

runApp<-function(...){
  appDir <- file.path(path.package("treetop", quiet=TRUE),"app")
  shiny::runApp(appDir, ...)
}
