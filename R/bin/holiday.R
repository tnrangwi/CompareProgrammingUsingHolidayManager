#!/usr/bin/Rscript
loadNamespace("basetools")
#loadNamespace("holidayserver")
arg <- commandArgs(trailingOnly=TRUE)
print(arg)
argv <- basetools::getopt(list(c=TRUE,f=TRUE,w=TRUE), arg)
#Name of config file (default holiday.conf), path to read cofg file from (default cwd), users directory (default cwd)
#holidayserver::start configFile configDir workDir
