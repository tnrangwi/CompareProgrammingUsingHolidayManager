#!/usr/bin/Rscript
loadNamespace("basetools")
loadNamespace("holidayserver")
arg <- commandArgs(trailingOnly=TRUE)
argv <- basetools::getopt(list(c=TRUE,f=TRUE,w=TRUE), arg)
if(is.null(argv)) stop("Error in command line parsing -- exiting")
holidayserver::start(configFile=argv[["f"]],configPath=argv[["c"]],workDir=argv[["w"]])
