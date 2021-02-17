.onAttach <- function(lib, pkg){
  info <- packageDescription("treetop")
  if (is.null(info$Date)){info$Date= "2021-02-10 10:11:17 UTC"}
  packageStartupMessage(
    paste('\n##----------------------------------------------------------------##\n',
          'treetop package, version ', info$Version, ', Released ', info$Date, '\n',
          'This package is based upon work supported by the Department of Defense ',
          'Strategic Environmental Research and Development Program (SERDP) under ',
          'grants No. RC20-136 and RC-2243.\n',
          '##----------------------------------------------------------------##',
          sep="")
  )
}
