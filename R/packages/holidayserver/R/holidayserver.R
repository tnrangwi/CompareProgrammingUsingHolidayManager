loadNamespace("basetools")

start <- function(configFile=NULL,configPath=NULL,workDir=NULL) {
  if(is.null(configPath)) configPath <- "."
  if(is.null(configFile)) configFile <- "holiday.conf"
  if(is.null(workDir)) workDir <- "."
  message("Config path:",configPath,",config file:",configFile,",workdir:'",workDir,"'")
  filename <- file.path(configPath, configFile)
  if (file.exists(filename)) {
    message("Reading config file ",filename)
    conf <- basetools::readConfigFile(filename)
    if(is.null(conf)) stop("Error reading configuration file -- exiting")
  } else {
    message("Config file does not exist, start using defaults")
    conf <- list()
  }
  print("Holiday Server not yet implemented")
}
