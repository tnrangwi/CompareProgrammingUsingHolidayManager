#Very simple getopt implementation. For demonstration purposes only.
#TODO: Accept options with or without arguments when neithe TRUE / FALSE.
getopt <- function(opt, argv) {

  typecheck <- function(v) typeof(v) == "locical" && length(v) == 1
  
  if (typeof(opt) == "list" && all(sapply(opt, typecheck)) && typeof(argv) == "character") {
    warning("Invalid data type for getopt")
    return(NULL)
  }
  result <- list(character(0))
  if (length(argv) == 0) return(result)

  for (c in 1:length(argv)) {
    v <- argv[c]
    if (v == "--") {
      result[[1]] <- c(result[[1]],argv[-1:-c])
      break
    } else if (nchar(v) >= 2 && substr(v, 1, 1) == "-") {
      o = substr(v, 2, 2)
      if (is.null(opt[[o]])) {
        warning(paste("Unsupported option in getopt:",v))
        return(NULL)
      }
      vlen <- nchar(v)
      if(opt[[o]] == TRUE) {
        if(vlen >= 3) {
          result[[o]] <- c(result[[o]],substring(v,3))
        } else {
          warning(paste("Option requires an argument:",v))
          return(NULL)
        }
      } else {
        if(vlen == 2) {
          result[[o]] <- c(result[[o]],"")
        } else {
          warning(paste("Option does not support an argument:",v))
          return(NULL)
        }
      }
    } else {
      result[[1]] <- c(result[[1]], v)
    }
  }
  return(result)
}

strip <- function(s,left=NA) {

  strip_ <- function (s,left) {
    l <- sub("\r?\n?$", "", s)    #strip newline / carriage return at end
    if(is.na(left) || left==TRUE)
      l <- sub("[\t ]*", "", s)   #strip leading white space
    if (is.na(left) || left==FALSE)
      l <- sub("[\t ]*$", "", l)  #skip trailing white spaces
    return(l)
  }

  if(typeof(s) == "character") {
      return(strip_(s,left=left))
  } else if(typeof(s) == "list") {
    return(lapply(s, strip, left=left))
  }

}

readConfigFile <- function(filename, fromMemory=FALSE) {

  readVar <- function(line) {
    nl = nchar(line)
    splitAt <- regexpr("=",line,fixed=TRUE)[1]
    if (splitAt <= 1) {
      warning(paste("Invalid line:",line))
      return(NULL)
    }
    key <- substr(line,1,splitAt - 1)
    if(splitAt == nl) {
      return(list(key=key,val=TRUE))
    } else if(substr(line,splitAt + 1,splitAt + 1) == '"') {
      if(substr(line, nl, nl) != '"') {
        warning(paste("Malformed string in line:",line))
        return(NULL)
      }
      return(list(key=key,val=substr(line,splitAt + 2, nl - 1)))
    } else if(length(grep(".",substr(line,splitAt + 1,nl),fixed=TRUE))) {
      n <- as.double(substr(line,splitAt + 1,nl))
      if(is.na(n)) {
        warning(paste("Malformed line, no double:",line))
        return(NULL)
      }
      return(list(key=key,val=n))
    } else {
      n <- as.integer(substr(line,splitAt + 1,nl))
      if(is.na(n)) {
        warning(paste("Malformed line, no integer:", line))
        return(NULL)
      }
      return(list(key=key,val=n))
    }
  }
  
  readSection <- function(lines, cfg) {
    if(length(lines) == 0) {
      return(list(dict=cfg, lines=lines))
    } else {
      l <- strip(lines[1])
      nl <- nchar(l)
      if(nl == 0 || substr(l, 1, 1) == '#') {
        return(readSection(lines[-1:0], cfg))
      } else if(substr(l, 1, 1) == '[') {
        return(list(dict=cfg,lines=lines))
      } else {
        r = readVar(l)
        if(is.null(r)) return(r)
        k <- r[["key"]]
        cfg[[k]] <- c(cfg[[k]], r[["val"]])
        return(readSection(lines[-1:0], cfg))
      }
    }
  }
    
  readLines <- function(lines, cfg) {
    if(length(lines) == 0) { #finished
      return(cfg)
    } else {
      l <- strip(lines[1])
      nl = nchar(l)
      if (nl == 0 || substr(l, 1, 1) == '#') { #comment line
        return(readLines(lines[-1:0], cfg))
      } else if (substr(l, 1, 1) == '[') { #section
        name = substr(l,2,nl-1)
        if (name == "" || substr(l, nl, nl) != ']') {
          warning(paste("Invalid section:",l))
          return(NULL)
        }
        sl <- readSection(lines[-1:0], list())
        if(is.null(sl)) return(sl)
        cfg[[name]] <- c(cfg[[name]], list(sl[["dict"]])) #enlist again against flattening
        return(readLines(sl[["lines"]], cfg))
      } else { #single value
        r <- readVar(l)
        if(is.null(r)) return(r)
        k <- r[["key"]]
        cfg[[k]] <- c(cfg[[k]], r[["val"]])
        return(readLines(lines[-1:0], cfg))
      }
    }
  }

  if(typeof(filename) != "character") {
    warning("Invalid datatype for filename in readConfigFile")
    return(NULL)
  }

  if(!fromMemory) {
    if(length(filename) != 1) {
      warning("Filename not given or given multiple times")
      return(NULL)
    }
    fd <- try(base::file(filename, "rt"))
    if(is(fd,"try-error")) {
      return(NULL)
    }
    lines <- base::readLines(fd, warn=FALSE) #Yes, this may crash if it fails during reading
    close(fd)
  } else {
    lines <- filename
  }
  return(readLines(lines, list()))
}
